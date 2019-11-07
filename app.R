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
theme_set(theme_mikabr(base_family = "Source Sans Pro"))
extrafont::loadfonts()

get_constants <- function(election, year) {
  constant_url <- glue("http://www.cambridgema.gov/election{year}/{election}%20Round.htm")
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
      str_extract("[,\\d]+(?= invalid ballots)") %>% str_replace(",", "") %>%
      as.numeric()
  )
}

get_counts <- function(election, year, n_rounds) {

  round_counts <- function(r) {
    round_url <- glue("http://www.cambridgema.gov/election{year}/{election}%20Round{r}.htm")
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

load_counts <- function(election, year, n_rounds, cache = TRUE) {
  counts_file <- glue("counts_{year}_{election}.feather")
  if (cache & file.exists(counts_file)) {
    counts <- read_feather(counts_file)
  } else {
    counts <- get_counts(election, year, n_rounds)
    write_feather(counts, counts_file)
  }
  return(counts)
}

# rounds_council <- 19
rounds_council <- 15
counts_council <- load_counts("Council", "2019", rounds_council)
constants_council <- get_constants("Council", "2019")
constants_council$elimination <- 50
constants_council$n_candidates <- n_distinct(counts_council$candidate)
constants_council$max_x <- max(counts_council$total)

rounds_school <- 9
counts_school <- load_counts("School", "2019", rounds_school)
constants_school <- get_constants("School", "2019")
constants_school$elimination <- 50
constants_school$n_candidates <- n_distinct(counts_school$candidate)
constants_school$max_x <- max(counts_school$total)

round_plot <- function(round_data, constants) {

  y_colour <- rev(if_else(round_data$defeated, "darkgrey", "black"))
  y_face <- rev(if_else(round_data$elected, "bold", "plain"))

  x_breaks <- c(constants$elimination, constants$quota,
                seq(0, constants$max_x, 400))
  x_face <- c("bold", "bold", rep("plain", length(x_breaks) - 2))

  ggplot(round_data, aes(x = total, y = candidate)) +
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
    geom_point(aes(x = pre_round), colour = "#268bd2", shape = "|", size = 2) +
    geom_point() +
    labs(x = "", y = "") +
    scale_x_continuous(breaks = x_breaks,
                       limits = c(0, constants$max_x),
                       expand = c(0.005, 0)) +
    theme(axis.text.y = element_text(colour = y_colour, face = y_face),
          axis.text.x = element_text(face = x_face),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(size = 1))
}

electionUI <- function(constants) {
  fluidRow(column(
    12,
    br(),
    p(style = "font-size:120%",
      glue("Preliminary results as of {constants$date}"), br(),
      glue("{format(constants$valid_ballots, big.mark = ',')} valid ballots were
           counted to elect {constants$seats} candidates out of
           {constants$n_candidates}")
           # ({format(constants$invalid_ballots, big.mark = ',')} invalid
           # ballots)")
    ),
    br()
  ))
}

github <- function() {
  fluidRow(column(
    1, offset = 11,
    br(), br(),
    a(href = "https://github.com/mikabr/cambridge-election",
      icon("github", class = "fa-3x")),
    br(), br()))
}

ui <- fluidPage(
  theme = shinytheme("lumen"),
  shinyjs::useShinyjs(),

  titlePanel("Cambridge Municipal Election 2019"), br(),

  mainPanel(
    width = 12,

    tabsetPanel(
      type = "pills",

      tabPanel(
        "City Council",

        electionUI(constants_council),

        div(
          style = "width:800px",

          fluidRow(column(
            12, align = "center",
            div(style = "display:inline-block",
                actionButton("down_council", label = "",
                             icon = icon("step-backward"),
                             class = "btn btn-sm")),
            div(style = "display:inline-block;margin-right:15px;margin-left:15px;padding:0px",
                h3(textOutput("round_label_council"), style = "margin:0px")),
            div(style = "display:inline-block",
                actionButton("up_council", label = "", icon = icon("step-forward"),
                             class = "btn btn-sm")),
            plotOutput("round_plot_council", height = "400px"),
            h4(htmlOutput("help_council")),
            h5(textOutput("round_elected_council")),
            h5(textOutput("round_defeated_council")),
            github()))
        )
      ),

      tabPanel(
        "School Committee",

        electionUI(constants_school),

        div(
          style = "width:800px",

          fluidRow(column(
            12, align = "center",
            div(style = "display:inline-block",
                actionButton("down_school", label = "",
                             icon = icon("step-backward"),
                             class = "btn btn-sm")),
            div(style = "display:inline-block;margin-right:15px;margin-left:15px;padding:0px",
                h3(textOutput("round_label_school"), style = "margin:0px")),
            div(style = "display:inline-block",
                actionButton("up_school", label = "", icon = icon("step-forward"),
                             class = "btn btn-sm")),
            plotOutput("round_plot_school", height = "300px"),
            h4(htmlOutput("help_school")),
            h5(textOutput("round_elected_school")),
            h5(textOutput("round_defeated_school")),
            github()))
        )
      )
    )
  )
)


server <- function(input, output) {

  k_council <- reactive({
    min(max(0, input$up_council - input$down_council), rounds_council)
  })

  k_school <- reactive({
    min(max(0, input$up_school - input$down_school), rounds_school)
  })

  round_data_council <- reactive({
    counts_council %>% filter(round == k_council())
  })

  round_data_school <- reactive({
    counts_school %>% filter(round == k_school())
  })

  observeEvent(input$up_council | input$down_council, {
    if (k_council() >= max(counts_council$round)) disable("up_council") else enable("up_council")
    if (k_council() <= 0) disable("down_council") else enable("down_council")
  }, ignoreNULL = FALSE)

  observeEvent(input$up_school | input$down_school, {
    if (k_school() >= max(counts_school$round)) disable("up_school") else enable("up_school")
    if (k_school() <= 0) disable("down_school") else enable("down_school")
  }, ignoreNULL = FALSE)

  output$round_label_council <- renderPrint({cat(paste("Round", k_council()))})
  output$round_label_school <- renderPrint({cat(paste("Round", k_school()))})

  output$help_council <- renderUI({
    case_when(
      k_council() == 0 ~ "To start off, count the number of #1 votes that each candidate received.",
      k_council() == 1 ~ sprintf("Any candidate whose count is above the quota of %s is declared elected.", constants_council$quota),
      k_council() == 2 ~ sprintf("Then, redistribute the surplus ballots of elected candidate(s) to the next ranked candidate on the ballot<br> (which ballots are selected for redistribution is random).<br><br>Also, eliminate any candidate whose count is below %s and redistribute their ballots.", constants_council$elimination),
      # k_council() == 3 ~ sprintf("Next, eliminate any candidate whose count is below %s and redistribute their ballots.", constants_council$elimination),
      k_council() == rounds_council ~ sprintf("When there are %s candidates left, those are the winners!", constants_council$seats),
      TRUE ~ "In every next round, eliminate the lowest ranked candidate and redistribute their ballots.<br> If any candidate's count reaches quota, declare them elected and redistribute their surplus ballots."
    ) %>% HTML()
  })

  output$help_school <- renderUI({
    case_when(
      k_school() == 0 ~ "To start off, count the number of #1 votes that each candidate received.",
      k_school() == 1 ~ sprintf("Any candidate whose count is above the quota of %s is declared elected.", constants_school$quota),
      k_school() == 2 ~ "Then, redistribute the surplus ballots of elected candidate(s) to the next ranked candidate on the ballot<br> (which ballots are selected for redistribution is random).",
      k_school() == 3 ~ sprintf("Next, eliminate any candidate whose count is below %s and redistribute their ballots.", constants_school$elimination),
      k_school() == rounds_school ~ sprintf("When there are %s candidates left, those are the winners!", constants_school$seats),
      TRUE ~ "In every next round, eliminate the lowest ranked candidate and redistribute their ballots.<br> If any candidate's count reaches quota, declare them elected and redistribute their surplus ballots."
    ) %>% HTML()
  })

  output$round_elected_council <- renderText({
    elected <- round_data_council() %>% filter(elected_round)
    if (nrow(elected))
      glue("Elected: {paste(elected$candidate, collapse = ' & ')}")
    else ""
  })

  output$round_elected_school <- renderText({
    elected <- round_data_school() %>% filter(elected_round)
    if (nrow(elected))
      glue("Elected: {paste(elected$candidate, collapse = ' & ')}")
    else ""
  })

  output$round_defeated_council <- renderText({
    defeated <- round_data_council() %>% filter(defeated_round)
    if (nrow(defeated))
      glue("Eliminated: {paste(defeated$candidate, collapse = ' & ')}")
    else ""
  })

  output$round_defeated_school <- renderText({
    defeated <- round_data_school() %>% filter(defeated_round)
    if (nrow(defeated))
      glue("Eliminated: {paste(defeated$candidate, collapse = ' & ')}")
    else ""
  })

  output$round_plot_council <- renderPlot({
    round_plot(round_data_council(), constants_council)
  })

  output$round_plot_school <- renderPlot({
    round_plot(round_data_school(), constants_school)
  })

}

shinyApp(ui = ui, server = server)

