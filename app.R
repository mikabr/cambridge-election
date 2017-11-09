library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(httr)
library(xml2)
library(glue)
library(forcats)
library(feather)
library(stringr)
library(lubridate)
library(langcog)
theme_set(theme_mikabr())

n_rounds <- 19

get_constants <- function() {
  constant_url <- "http://www.cambridgema.gov/election2017/Council%20Round.htm"
  constant_data <- GET(constant_url) %>% content(as = "text")
  list(
    date = constant_data %>% str_extract(".+(?= -- Cambridge, MA)"),
    seats = constant_data %>%
      str_extract("(?<=Electing )[,\\d]+(?= candidates)") %>% as.numeric(),
    quota = constant_data %>% str_extract("(?<=Quota is )[,\\d]+(?= votes)") %>%
      as.numeric(),
    valid_ballots = constant_data %>%
      str_extract("[,\\d]+(?= valid ballots)") %>% str_replace(",", "") %>%
      as.numeric(),
    invalid_ballots = constant_data %>%
      str_extract("[,\\d]+(?= invalid ballots)") %>% as.numeric()
  )
}

get_counts <- function() {

  round_counts <- function(r) {
    round_url <- glue("http://www.cambridgema.gov/election2017/Council%20Round{r}.htm")
    round_data <- GET(round_url) %>% content()

    results <- round_data %>%
      xml_find_all(".//table") %>%
      .[[2]] %>%
      xml_contents() %>%
      map(~.x %>% xml_children %>% map_chr(xml_text))

    results[2:(length(results) - 2)] %>%
      map(~set_names(.x, results[[1]])) %>%
      transpose() %>%
      map(unlist) %>%
      as_data_frame() %>%
      mutate(round = r)
  }

  all_counts <- map_df(1:n_rounds, round_counts)

  all_counts %>%
    bind_rows(all_counts %>% filter(round == 1) %>%
                mutate(round = 0, STATUS = "CONTINUING")) %>%
    filter(CANDIDATE != "TOTALS:", CANDIDATE != "EXHAUSTED PILE:") %>%
    transmute(candidate = as_factor(CANDIDATE),
              this_round = as.numeric(`THIS ROUND`),
              total = as.numeric(TOTAL),
              status = STATUS,
              round = round,
              pre_round = if_else(this_round <= 0 | round %in% 0:1,
                                  total, total - this_round),
              post_round = if_else(this_round < 0, total - this_round, total)
    ) %>%
    arrange(round, candidate) %>%
    mutate(candidate = fct_relevel(candidate, rev(levels(candidate)))) %>%
    group_by(round) %>%
    mutate(elected = str_detect(status, "ELECTED"),
           defeated = str_detect(status, "DEFEATED"),
           elected_round = round ==
             str_replace(str_trim(status), "ELECTED -- (.*)(st|nd|rd|th) count", "\\1"),
           defeated_round = round ==
             str_replace(str_trim(status), "DEFEATED -- (.*)(st|nd|rd|th) count", "\\1")
    )

}

load_counts <- function(cache = TRUE) {
  if (cache & file.exists("counts_cleaned.feather")) {
    counts_cleaned <- read_feather("counts_cleaned.feather")
  } else {
    counts_cleaned <- get_counts()
    write_feather(counts_cleaned, "counts_cleaned.feather")
  }
  return(counts_cleaned)
}

counts_cleaned <- load_counts()

constants <- get_constants()
constants$elimination = 50

n_candidates <- n_distinct(counts_cleaned$candidate)

x_breaks <- c(constants$elimination, constants$quota,
              seq(0, max(counts_cleaned$total), 400))
x_face <- c("bold", "bold", rep("plain", length(x_breaks) - 2))


ui <- fluidPage(
  theme = shinytheme("lumen"),
  shinyjs::useShinyjs(),

  fluidRow(column(
    12, align = "center",
    titlePanel("Cambridge City Council Election 2017"),
    p(em(glue("Preliminary results as of {constants$date}"))),
    p(glue("{format(constants$valid_ballots, big.mark = ',')} valid ballots were counted to elect {constants$seats} candidates out of {n_candidates} ({constants$invalid_ballots} invalid ballots)"))
  )),

  fluidRow(column(
    12, align = "center",
    div(style = "display:inline-block",
        actionButton("down", label = "", icon = icon("step-backward"),
                     class = "btn btn-sm")),
    div(style = "display:inline-block;margin-right:15px;margin-left:15px;padding:0px",
        h3(textOutput("round_label"), style = "margin:0px")),
    div(style = "display:inline-block",
        actionButton("up", label = "", icon = icon("step-forward"),
                     class = "btn btn-sm"))

  )),
  fluidRow(column(12, align = "center", plotOutput("round_plot"))),
  fluidRow(column(12, align = "center",
                  h4(htmlOutput("help")),
                  h5(textOutput("round_elected")),
                  h5(textOutput("round_defeated")))),

  br(),
  fluidRow(column(1, offset = 11,
                  a(href = "https://github.com/mikabr/cambridge-election",
                    icon("github", class = "fa-3x"))))
)


server <- function(input, output) {

  k <- reactive({
    min(max(0, input$up - input$down), n_rounds)
  })

  round_data <- reactive({
    counts_cleaned %>% filter(round == k())
  })

  observeEvent(input$up | input$down, {
    if (k() >= max(counts_cleaned$round)) disable("up") else enable("up")
    if (k() <= 0) disable("down") else enable("down")
  }, ignoreNULL = FALSE)

  output$round_label <- renderPrint({cat(paste("Round", k()))})

  output$help <- renderUI({
    case_when(
      k() == 0 ~ "To start off, count the number of #1 votes that each candidate received.",
      k() == 1 ~ sprintf("Any candidate whose count is above the quota of %s is declared elected.", constants$quota),
      k() == 2 ~ "Then, redistribute the surplus ballots of elected candidate(s) to the next ranked candidate on the ballot<br> (which ballots are selected for redistribution is random).",
      k() == 3 ~ sprintf("Next, eliminate any candidate whose count is below %s and redistribute their ballots.", constants$elimination),
      k() == n_rounds ~ sprintf("When there are %s candidates left, those are the winners!", constants$seats),
      TRUE ~ "In every next round, eliminate the lowest ranked candidate and redistribute their ballots.<br> If any candidate's count reaches quota, declare them elected and redistribute their surplus ballots."
    ) %>% HTML()
  })

  output$round_elected <- renderText({
    elected <- round_data() %>% filter(elected_round)
    if (nrow(elected))
      glue("Elected: {paste(elected$candidate, collapse = ' & ')}")
    else ""
  })

  output$round_defeated <- renderText({
    defeated <- round_data() %>% filter(defeated_round)
    if (nrow(defeated))
      glue("Eliminated: {paste(defeated$candidate, collapse = ' & ')}")
    else ""
  })

  output$round_plot <- renderPlot({

    y_colour <- rev(if_else(round_data()$defeated, "darkgrey", "black"))
    y_face <- rev(if_else(round_data()$elected, "bold", "plain"))

    ggplot(round_data(), aes(x = total, y = candidate)) +
      geom_vline(xintercept = constants$quota, colour = "dimgrey",
                 linetype = "dotted") +
      geom_vline(xintercept = constants$elimination, colour = "darkgrey",
                 linetype = "dotted") +
      geom_segment(aes(x = 0, xend = pre_round, yend = candidate),
                   size = 1, colour = "dimgrey") +
      geom_segment(aes(x = pre_round, xend = total, yend = candidate),
                   size = 1, colour = "#268bd2") +
      geom_segment(aes(x = total, xend = post_round, yend = candidate),
                   size = 1, colour = "#cb4b16") +
      geom_point() +
      labs(x = "", y = "") +
      scale_x_continuous(breaks = x_breaks,
                         limits = c(0, max(counts_cleaned$total)),
                         expand = c(0.005, 0)) +
      theme(axis.text.y = element_text(colour = y_colour, face = y_face),
            axis.text.x = element_text(face = x_face),
            axis.ticks.y = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line(size = 1))

  }, width = 800)
}

shinyApp(ui = ui, server = server)

