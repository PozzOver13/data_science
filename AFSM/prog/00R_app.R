
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)

load("~/Google Drive/Tempo_Libero/AFSM/Stats/00R_df_html.rdata")

source("~/Documents/GitHub/data_science/AFSM/prog/functions/funzioni_per_html.R")

ui <- fluidPage(
  
  titlePanel("ASFM help", windowTitle = "asfm_help"), 
  div("Info per asta", 
      style = "color: black;"),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(radioButtons("typeInput", 
                              "Seleziona ruolo",
                              choices = c("P", "D", "C", "A"),
                              selected = "P"),
                 
                 radioButtons("varInput", 
                              "Variabile per cui ordinare",
                              choices = c("qt_i", 
                                          "gs_tot",
                                          "clsh_tot",
                                          "gf_tot",
                                          "mean_mark_tot",
                                          "dbl_tot",
                                          "rf_tot",
                                          "as_tot"),
                              selected = "qt_i"),
                 
                 # textInput("varInput", 
                 #           "Variabile per cui ordinare", 
                 #           value = "qt_i"),
                 
                 sliderInput("limitInput", 
                             "Numero giocatori da considerare", 
                             min = 1, max = 100,
                             value = c(1, 10)),
                 checkboxGroupInput("players",
                                    "Giocatori disponibili:")),
    
    mainPanel("Distribuzione score", 
              plotOutput("coolplot"),
              br(),
              br(),
              dataTableOutput("TopPlayers"))
  ),
  br()
  
)
server <- function(input, output, session) {
  
  output$coolplot <- renderPlot({
    
    ruoli <- c("P", "D", "C", "A")
    colori <- c("dodgerblue3", 
                "orange3", 
                "mediumpurple3", 
                "lightslategrey")
    
    colore_1 <- colori[str_detect(ruoli,  input$typeInput)]
    
    show_me_best_score(df_in = df_in_n %>% filter(playername %in% input$players), 
                       ruolo_char = input$typeInput, 
                       var_target = input$varInput, 
                       input$limitInput[1]:input$limitInput[2]) %>%
      rename_("variabile_target" = input$varInput) %>%
      ggplot(aes(reorder(playername, -variabile_target), variabile_target)) +
      geom_bar(stat= "identity", fill = colore_1) +
      labs(x = "", y = input$varInput) +
      coord_flip() +
      theme_light()
  })
  
  observe({
    x <- input$inCheckboxGroup
    
    choices=c()
    if (input$typeInput == "P") 
      choices = df_in_n$playername[df_in_n$role == "P"]
    if (input$typeInput == "D") 
      choices = df_in_n$playername[df_in_n$role == "D"]
    if (input$typeInput == "C") 
      choices = df_in_n$playername[df_in_n$role == "C"]
    if (input$typeInput == "A") 
      choices = df_in_n$playername[df_in_n$role == "A"]
    
    updateCheckboxGroupInput(session, "players",
                             choices = choices,
                             selected = choices
    )
  })
  
  output$TopPlayers <- renderDataTable({
    show_me_best_score(df_in = df_in_n %>% filter(playername %in% input$players), 
                       ruolo_char = input$typeInput, 
                       var_target = input$varInput, 
                       input$limitInput[1]:input$limitInput[2])
  })
  
  
}
shinyApp(ui = ui, server = server)