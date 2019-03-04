#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Additional Bootstrap themes at [Bootswatch](http://bootswatch.com/)

library(shiny)
library(tidyverse)

list_choices <-  unique(msleep$vore)[!is.na(unique(msleep$vore))]
names(list_choices) <- paste(unique(msleep$vore)[!is.na(unique(msleep$vore))],"vore",sep="")

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",

  tabPanel("msleep",
    fluidPage( 
     sidebarLayout(# position = "right",
       sidebarPanel(
         selectInput("select", label = h3("Plot by type of alimentation"), 
                     choices = character(0),
                     selected = 1)
       ),
       mainPanel(
         plotOutput(outputId = "plot")
       )
     )
  ) # fluidPage
  ), #  titlePanel
  tabPanel("References",
           includeMarkdown("references.md")
  ) #  titlePanel
) # navbarPage

col_scale <- scale_colour_discrete(limits = unique(msleep$vore))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Can also set the label and select items
  updateSelectInput(session, "select",
                    choices = list_choices,
                    selected = tail(list_choices, 1)
  )
  
  output$plot <- renderPlot({
    # cat(file=stderr(), "input$select:", input$select == "", "\n")
    if(input$select != ""){
       ggplot(msleep %>% filter(vore == input$select), aes(bodywt, sleep_total, colour = vore)) +
        scale_x_log10() +
        col_scale +
        geom_point()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

