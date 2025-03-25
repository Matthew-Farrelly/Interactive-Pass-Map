#Shiny Football Passing Map
#@author Matt Farrelly
#Data made public by Metrica Sports -  https://github.com/metrica-sports/sample-datas
#'events-definitions' file in 'documentation' folder for explanation of Event Types and Subtypes variables.

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
  # Loading Pass data 
  Game_Data <- read_csv("Rsport/sample-data-master/data/Sample_Game_1/Sample_Game_1_RawEventsData.csv")
  
  # Selecting Pass data
  Game_Data <- filter(Game_Data, Type == "PASS")
  
  # Ordering and Changing Player's names for better data visualisation and user interface.
  Game_Data$From <- gsub("Player", "", Game_Data$From) %>%
                    as.numeric(Game_Data$From) 
  Game_Data <- Game_Data[order(Game_Data$From),]
  Game_Data$From <- paste(Game_Data$From, Game_Data$Team, sep = "- ")
  
  #ggsoccer Metrica Sports use a similar (0,0)-(1,1) coordinate system similar to stats bomb
  Game_Data <- Game_Data %>% mutate(x1 = `Start X` * 100,
                                    y1 = `Start Y` * 100,  
                                    x2 = `End X` * 100,
                                    y2 = `End Y` * 100, 
                                    x1 = ifelse(Period == 2, 100 - x1, x1),
                                    y1 = ifelse(Period == 2, 100 - y1, y1),
                                    x2 = ifelse(Period == 2, 100 - x2, x2),
                                    y2 = ifelse(Period == 2, 100 - y2, y2)) # Inverting passes, first half and second half passes displayed in the same direction.
          
  #summarise data and plot
  data <- reactive({
    req(input$sel_Player)
    df <- Game_Data %>% filter(From %in% input$sel_Player)
  })
 
  #update SelectInput Dynamically
  observe({
  updateSelectInput(session,"sel_Player", choices = Game_Data$From)
  })

  #plot
  output$plot <- renderPlot({
    ggplot(data()) +
    annotate_pitch(dimensions = pitch_wyscout,
                   colour = "white",
                   fill   = "springgreen4",
                   limits = FALSE) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 arrow = arrow(length = unit(0.15, "cm"),
                               type = "closed")) +
    theme_pitch() +
    theme(panel.background = element_rect(fill = "springgreen4")) +
    
    geom_label(
        label= "<----- Away team    Home Team ----->", 
        x=51,
        y=-3,
        label.padding = unit(0.15, "lines"), # Rectangle size around label
        label.size = 0.25,
        color = "black") +
    
      ggtitle("Pass Map by individual Players", subtitle = "NOTE: throwins count as passes in this dataset") 
  })
  
}
ui <- fluidPage(
  
  titlePanel("Interactive Pass map"),
    
    sidebarPanel(
      selectInput(inputId = "sel_Player",
                  label = "Choose Player",
                  "names")
      ),
    
    mainPanel(
      plotOutput("plot"),
    )
  )

shinyApp(ui = ui, server = server)