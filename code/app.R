#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a dropdown menu
server = function(input,output, session){
  output$selected_var <- renderText({
    theUser <<- input$region
    paste("You have selected", input$region,".Please select 
          'Close window' to move to the next annotation.")
  })
  observe({
    if (input$close > 0) stopApp()                             # stop shiny
  })
}
ui <- basicPage(
  h1("Summer 2022 Hollings ML Checker"),
  selectInput("region", label = "Sea creature",
              choices = c("Astropecten","Anemone","Asterias forbesi",
                          "Asterias vulgaris","Brittle Stars","Bryozoan",
                          "Buried Astropecten","Convict worm", "Comb Jelly",
                          "Hermit Crab", "Henricia","Leptasterias","Little Skate", "Mussel",
                          "Sand Dollar", "seastar",
                          "Scallop","Sclerasterias","Sea pen","Sea Mouse",
                          "Sponge","Whelk","Unknown"),
              selected = "Unknown"),
  
  textOutput("selected_var"),
  tags$button(
    id = 'close',
    type = "button",
    class = "btn action-button",
    onclick = "setTimeout(function(){window.close();},500);",  # close browser
    "Close window"
  )
)
shinyApp(ui = ui, server = server)

