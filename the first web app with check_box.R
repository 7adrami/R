library(shiny)
library(tidyverse)
library(readxl)
data = read.csv("C:/Users/dell/Desktop/Hadrami/Desktop/serie temporelles/treating the TS/JoGvy_QWS-WBr8v0Fsvllg_389057e650da406fbc1d22caf632bfaf_publish_practice.csv")
view(data)
dat=data %>% ggplot(aes(x=varX, y=varY, color=Group))+
  geom_point()
dat
##Defining th ui
ui = fluidPage(

    sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "checked_group",
        label = "which of the groups do you like to display?",
        choices = c("a", "b", "c"),
        selected = c("a", "b", "c")
      )
    ),
    mainPanel(
      plotOutput("scatter")
    )
  )
)


##Define a server
server = function(input, output){
  output$scatter = renderPlot({
    
    plotdata=filter(data, Group %in% input$checked_group)
        
    ggplot(plotdata, aes(x=varX, y=varY, color=Group))+geom_point()
  })
}
shinyApp(ui=ui, server = server)

