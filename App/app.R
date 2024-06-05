# Task Name: Streuung_VERA_graph ######################

library(shiny)
library(miniUI)
library(shinyjs)
library(tidyverse)
library(googlesheets4)
library(shinycssloaders)
library(hrbrthemes)
library(viridis)
set.seed(25051982)

## Googlesheets Connection Setup ###############################################
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets/"
)

gs4_auth()


## UI #########################################################################
ui <- miniPage(
  useShinyjs(),
  miniContentPanel(
    wellPanel(
      h4("Aufgabe: Heterogene Klassen VERA"),
      htmlOutput("prompt_task")
    ),
    wellPanel(
      plotOutput("plot")
    ),
    shinyjs::hidden(wellPanel(id = "feedbackpanel_task",
                              withSpinner(
                                htmlOutput("feedback_task"),
                                proxy.height = "50px",
                                color = "#8cd000"))
    ),
    wellPanel(
      uiOutput("ui_answers_task"),
      actionButton("show_feedback_task", 
                   "Prüfe meine Lösung!",
                   icon = icon("send")),
      actionButton("reshuffle_task", 
                   "Diese Aufgabe wiederholen",
                   icon = icon("repeat")),
      actionButton("new_task", 
                   "Neue Aufgabe derselben Art",
                   icon = icon("plus"))
     # , verbatimTextOutput("debug")
    )      
  )  
)



server <- function(input, output, session) {
  
  # Global functions ###########################################################
  ## round2 rounds .5 upwards
  round2 = function(x, n) {
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5 + sqrt(.Machine$double.eps)
    z = trunc(z)
    z = z/10^n
    z*posneg
  }

  ##############################################################################
  # Backend for task  ##########################################################
  ##############################################################################
  
  distribution_beta <- 
      function (n, shape1, shape2, ncp = 0, random = FALSE, ...) 
    {
      if (random) {
        stats::rbeta(n, shape1, shape2, ncp = ncp)
      }
      else {
        stats::qbeta(stats::ppoints(n), shape1, shape2, ncp = ncp, 
                     ...)
      }
    }
  
  ## Data for task  ####
  classsizes_random <- reactive({
    input$new_task
    sample(17:32, 8, replace = T)
  })
  classnames_random <- reactive({
    input$new_task
    sample(LETTERS[1:8], 8)
  })
  
  
  classdata <- reactive({
    input$new_task
    
    tibble(
      Performance = c(distribution_beta(classsizes_random()[1], 0004, 0004), # normal
                      distribution_beta(classsizes_random()[2], 00.7, 0002), # right
                      distribution_beta(classsizes_random()[3], 0001.5, 00.8), # left
                      distribution_beta(classsizes_random()[4], 1.05, 0.95), # unif
                      distribution_beta(classsizes_random()[5], 0.95, 1.05), # unif
                      distribution_beta(classsizes_random()[6], 005, 5), # lepto
                      distribution_beta(classsizes_random()[7], 20, 5), # lepto right
                      distribution_beta(classsizes_random()[8], sample(c(.2, .3), 1), sample(c(.2, .3), 1))), # u
      Klasse = c(rep(paste("Klasse ", classnames_random()[1]), classsizes_random()[1]),
                 rep(paste("Klasse ", classnames_random()[2]), classsizes_random()[2]),
                 rep(paste("Klasse ", classnames_random()[3]), classsizes_random()[3]),
                 rep(paste("Klasse ", classnames_random()[4]), classsizes_random()[4]),
                 rep(paste("Klasse ", classnames_random()[5]), classsizes_random()[5]),
                 rep(paste("Klasse ", classnames_random()[6]), classsizes_random()[6]),
                 rep(paste("Klasse ", classnames_random()[7]), classsizes_random()[7]),
                 rep(paste("Klasse ", classnames_random()[8]), classsizes_random()[8])
      )
    ) %>% 
    mutate(
      Kompetenzstufe = case_when(Performance < .2 ~ "1 = niedrigste Stufe",
                                 Performance < .4 ~ "2",
                                 Performance < .6 ~ "3",
                                 Performance < .8 ~ "4",
                                 T ~ "5 = höchste Stufe"),
      Kompetenzstufe = factor(Kompetenzstufe, 
                              levels = c("5 = höchste Stufe",
                                         "4",
                                         "3",
                                         "2",
                                         "1 = niedrigste Stufe")
      )
    )
    
  })
  
  output$plot <- renderPlot({
    ggplot(classdata(), aes(Klasse, fill = Kompetenzstufe)) +
      geom_bar(color = "#00000000") +
      scale_fill_viridis_d() +
      theme_ipsum() + 
      ylab("Anzahl Schüler*innen") + 
      coord_flip() +
      ggtitle("Kompetenzstufenbelegung", "in acht Klassen")
  })
  
 # output$debug <- renderPrint({
 #   classdata()
 # })
  
  
  
  ## Render UI for Answers ###
  output$ui_answers_task <- renderUI({
    input$reshuffle_task
    input$new_task
    radioButtons(
      "answers_task",
      "Bitte ankreuzen",
      paste("Klasse ", LETTERS[1:8]),
      selected = character(0)
    )
  })
  
  ## Prompt task 
  output$prompt_task <- renderText({
    "<b>Ein Schulleiter einer großen Grundschule hat folgende Ergebnisse der Vergleichsarbeiten in Klasse 3 (VERA 3) vorliegen. Er fragt sich: Welche der Klassen weist die größte Heterogenität (Streuung) auf? </b><br>"
  }) 
  
  ## Correct answers ###
  correct_answers_task <- reactive({
      paste("Klasse ", classnames_random()[8])
  })
  
  ## Feedback task  ####
  output$feedback_task <- renderText({   
    
    if(is.null(input$answers_task)){
      HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                 paste(correct_answers_task(), collapse = ", <br>✓ "),
                 "<br><i>",
                 learnr::random_encouragement(),
                 "</i>"))
    }else{
      if(setequal(correct_answers_task(), input$answers_task)){
        paste("Richtig!", learnr::random_praise())}else{
          HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                     paste(correct_answers_task(), collapse = ", <br>✓ "),
                     "<br><i>",
                     learnr::random_encouragement(),
                     "</i>"))
        }
    }
  })
  
  ## Show and Hide Feedback ####################################################
  
  ## Show feedback on button click 
  observeEvent(input$show_feedback_task, {
    showElement(id = "feedbackpanel_task")
  })
  
  ## Hide feedback on solution change or new plot type
  observeEvent(c(input$answers_task, input$new_task), {
    hideElement(id = "feedbackpanel_task")
  })
  
  ## Reset answer on new plot type
  observeEvent(c(input$reshuffle_task, input$new_task), {
    reset(id = "answers_task")
  })

  
  ## URL Variable fetching #####################################################
  url_vars <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  ## Usage Logging #############################################################
  observeEvent(input$show_feedback_task, {
    if(!is.null(input$answers_task)){
      sheet_append("14yNT-c11reOPS-Gt2cHnNoa9HsnkhYvkUYgytu8DAhg",
                   tibble(PID = ifelse(is.null(url_vars()$PID), 
                                       "PID is missing", #to keep ncol constant
                                       url_vars()$PID), # Person identifier from URL
                          task_name = "Streuung_VERA_graph",
                          task_version = "repeatable_and_parametrized",
                          time = Sys.time(),
                          timezone = Sys.timezone(),
                          new_task = as.numeric(input$new_task),
                          reshuffle_task = as.numeric(input$reshuffle_task),
                          result = # correct or wrong sol. provided by student
                            case_when(is.null(input$answers_task) ~ 
                                        "false_solution",
                                      setequal(correct_answers_task(), 
                                               input$answers_task) ~ 
                                        "correct_solution",
                                      TRUE ~  "false_solution")
                   ),
                   sheet = 1)
    }
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)
