library(shiny)


#Difine the user interface
ui = fluidPage(
  titlePanel("The name of the app"),
  textInput(inputId = "10", label = "Enter some text"),
  textOutput(outputId = "print_text")
)


#server
server = function(input, output){
  output$print_text=renderText(input$"10")
}

shinyApp(ui=ui, server = server)
