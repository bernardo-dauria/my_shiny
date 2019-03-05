#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Additional Bootstrap themes at [Bootswatch](http://bootswatch.com/)

require(shiny)
require(tidyverse)
require("shinyjs")

list_choices <-  unique(msleep$vore)[!is.na(unique(msleep$vore))]
names(list_choices) <- paste(unique(msleep$vore)[!is.na(unique(msleep$vore))],"vore",sep="")

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
  tabPanel("msleep",
    fluidPage( 
       sidebarLayout(
         sidebarPanel(
           selectInput("select", label = h3("Plot by type of alimentation"), 
                       choices = character(0),
                       selected = 1)
         ), # sidebarPanel
         mainPanel(
           plotOutput(outputId = "plot", click = "plot_click"),
           verbatimTextOutput("info")
         ) # mainPanel
       ) # sidebarLayout
    ) # fluidPage
  ), #  tabPanel
  tabPanel("Random generator",
       useShinyjs(),
       sidebarLayout(position = "right",
         sidebarPanel(
           selectInput("dist", label = h3("Select the distribution"), 
                       choices = list(Normal="rnorm", Uniform="runif", Exponential="rexp"),
                       selected = 1),
           sliderInput("n_sample", label = h3("Number of samples"), min = 10, 
                       max = 100, value = 50),
           fluidRow(
             h3(style = "margin-left: 20px; margin-bottom: 0px;", "Number of bins"),
             column(2,
                div(style = "margin-top: 37px", checkboxInput("auto_bins", label = "auto", value = TRUE))
             ),
             column(10,
              sliderInput("n_bins", label="", min = 1, max = 50, value = 30)
             )
           )
         ), # sidebarPanel
         mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Plot", plotOutput("histPlot")),
                       tabPanel("Summary", verbatimTextOutput("histSummary")),
                       tabPanel("Table", tableOutput("histTable"))
           )
         ) # mainPanel
       ) # sidebarLayout
  ), #  tabPanel
  tabPanel("References",
           includeMarkdown("references.md")
  ) #  tabPanel
) # navbarPage

col_scale <- scale_colour_discrete(limits = unique(msleep$vore))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Can also set the label and select items
  updateSelectInput(session, "select",
                    choices = list_choices,
                    selected = tail(list_choices, 1)
  );
  
  output$plot <- renderPlot({
    # cat(file=stderr(), "input$select:", input$select == "", "\n")
    if(input$select != ""){
       ggplot(msleep %>% filter(vore == input$select), aes(bodywt, sleep_total, colour = vore)) +
        scale_x_log10() +
        col_scale +
        geom_point()
    }
  });
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  samples <- reactive({
      dist <- eval(parse(text=paste(input$dist)))
      dist(input$n_sample)
    })
  
  observe(if(input$auto_bins) disable("n_bins") else enable("n_bins"))
  
  output$histPlot <- renderPlot(
      hist(samples(), main="Random Generation", 
           breaks = if(!input$auto_bins) {input$n_bins} else {"Sturges"})
  )
  output$histSummary <- renderPrint(summary(samples()))
  output$histTable <- renderTable(samples())
  
}

# Run the application 
shinyApp(ui = ui, server = server)

