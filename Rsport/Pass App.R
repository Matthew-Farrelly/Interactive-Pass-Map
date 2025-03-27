#Shiny Football Passing Map
#@author Matt Farrelly
#Data made public by Metrica Sports -  https://github.com/metrica-sports/sample-datas
#'events-definitions' file in 'documentation' folder for explanation of Event Types and Subtypes variables.

setwd("C:/Users/mazza/OneDrive/Documents/Rsport")
#Packages
#install.packages("shiny")
#install.packages("dplyr")
#install.packages("ggsoccer")
#install.packages("ggplot2")
#install.packages("readr")
library(shiny)
library(dplyr)
library(ggsoccer)
library(ggplot2)
library(readr)

server <- function(input, output, session) {
  # Loading Game1 data 
  Game_Data1 <- read_csv("sample-data-master/data/Sample_Game_1/Sample_Game_1_RawEventsData.csv")
  Game_Data1 <- filter(Game_Data1, Type == "PASS")                                                          # Selecting Pass data
  Game_Data1$From <- gsub("Player", "", Game_Data1$From) %>%                                                # Ordering and Changing Player's names for better data visualisation and user interface.
    as.numeric(Game_Data1$From) 
  Game_Data1 <- Game_Data1[order(Game_Data1$From),]
  Game_Data1$From <- paste(Game_Data1$From, Game_Data1$Team, sep = "-")
  Game_Data1 <- Game_Data1 %>% mutate(x1 = `Start X` * 100,                                                 #ggsoccer Metrica Sports use a similar (0,0)-(1,1) coordinate system similar to stats bomb
                                      y1 = `Start Y` * 100,  
                                      x2 = `End X` * 100,
                                      y2 = `End Y` * 100, 
                                      
                                      x1 = ifelse(Period == 2, 100 - x1, x1),
                                      y1 = ifelse(Period == 2, 100 - y1, y1),
                                      x2 = ifelse(Period == 2, 100 - x2, x2),
                                      y2 = ifelse(Period == 2, 100 - y2, y2))                                 # Inverting passes, first half and second half passes displayed in the same direction.
  
  Game_Data1 <- Game_Data1 %>% mutate(Pass_Direction = ifelse(Team == "Home", `x2`- `x1`, `x1`- `x2`),         #Creating a new variable tracking wether the ball goes forward or back ward
                                      Pass_Direction = ifelse(Pass_Direction < 0, "Backwards", "Forwards"))
  
  # Loading Game 2 data 
  Game_Data2 <- read_csv("sample-data-master/data/Sample_Game_2/Sample_Game_2_RawEventsData.csv")
  Game_Data2 <- filter(Game_Data2, Type == "PASS")                                                          # Selecting Pass data
  Game_Data2$From <- gsub("Player", "", Game_Data2$From) %>%                                                # Ordering and Changing Player's names for better data visualisation and user interface.
    as.numeric(Game_Data2$From) 
  Game_Data2 <- Game_Data2[order(Game_Data2$From),]
  Game_Data2$From <- paste(Game_Data2$From, Game_Data2$Team, sep = "-")
  
  Game_Data2 <- Game_Data2 %>% mutate(x1 = `Start X` * 100,                                                 #ggsoccer Metrica Sports use a similar (0,0)-(1,1) coordinate system similar to stats bomb
                                      y1 = `Start Y` * 100,  
                                      x2 = `End X` * 100,
                                      y2 = `End Y` * 100, 
                                      x1 = ifelse(Period == 1, 100 - x1, x1),
                                      y1 = ifelse(Period == 1, 100 - y1, y1),
                                      x2 = ifelse(Period == 1, 100 - x2, x2),
                                      y2 = ifelse(Period == 1, 100 - y2, y2))                                 # Inverting passes, first half and second half passes displayed in the same direction.
  
  Game_Data2 <- Game_Data2 %>% mutate(Pass_Direction = ifelse(Team == "Home", `x2`- `x1`, `x1`- `x2`),         #Creating a new variable tracking wether the ball goes forward or back ward
                                      Pass_Direction = ifelse(Pass_Direction < 0, "Backwards", "Forwards"))
  
  #summarise data and plot
  data <- reactive({
    req(input$sel_Game)
    req(input$sel_Player)
    ifelse(input$sel_Game == "Game 1", Game <- Game_Data1, Game <- Game_Data2)
    Game %>% filter(From %in% input$sel_Player)
  })
  
  #update SelectInput Dynamically
  observe({
    updateSelectInput(session,"sel_Game", choices = c("Game 1", "Game 2"))
    updateSelectInput(session,"sel_Player", choices = Game_Data1$From)
  })
  
  #plot
  output$plot <- renderPlot({
    ggplot(data()) +
      annotate_pitch(dimensions = pitch_wyscout,
                     colour = "white",
                     fill   = "springgreen4",
                     limits = FALSE) +
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = Pass_Direction),
                   arrow = arrow(length = unit(0.15, "cm"),
                                 type = "closed")) +
      theme_pitch() +
      theme(panel.background = element_rect(fill = "springgreen4")) +
      
      geom_label(
        label= "<----- Away team    Home Team ----->", 
        x=51,
        y=-3,
        label.padding = unit(0.11, "lines"), # Rectangle size around label
        label.size = 0.25,
        color = "black") +
      
      
      scale_color_manual(values = c("Forwards" = "purple", "Backwards" = "red")) +
      
      ggtitle("Pass Map" , subtitle = "NOTE: throwins count as passes in this dataset") 
  }, height = 500, width = 800) 
  
  #text
  output$selected_var <- renderText({
    paste("Viewing", input$sel_Game, "- Player", input$sel_Player)
  })
}
ui <- fluidPage(
  
  titlePanel("Interactive Pass map"),
  
  sidebarPanel( width = 12,
                selectInput(inputId = "sel_Game",
                            label = "Choose Game",
                            choices = c("Game 1", "Game 2")),
                selectInput(inputId = "sel_Player",
                            label = "Choose Player",
                            "names")
                
  ),
  
  mainPanel(
    tabsetPanel(type= "tab",
                tabPanel("Pass Map",
                         align = "centre",h1(textOutput("selected_var")),
                         plotOutput("plot"), width = "100%", height = "100%"),
                tabPanel("Player Profile WIP", tags$image(src = "Default.jpg", width = "170px", height= "200px", align = "right"),
                         p("Player name:"),
                         p("Age:"),
                         p("Nationality:"),
                         p("Current club:"),
                         p("Position:"),
                         p("Preffered foot:"),
                         p("Height"),
                         p("Weight:"),
                         p("Spoken Languages:"),
                         strong("Work In Progress, would need access to Identified player data, not provided in data set."))
    )
    
  )
  
)

shinyApp(ui = ui, server = server)
