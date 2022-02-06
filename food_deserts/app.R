#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)

read.csv(here("data", "food_access_subset"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Food Deserts in America"),
    navbarPage("My Application",
               tabPanel(
                 "Widget 1",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("select", label = h3("Select State:"), 
                                  choices = list("Alabama" = "Alabama", "Alaska" = "Alaska", "Arizona" = "Arizona",
                                                 "Arkansas" = "Arkansas", "California" = "California", "Colorado" = "Colorado",
                                                 "Connecticut" = "Connecticut", "Delaware" = "Delaware", "Florida" = "Florida",
                                                 "Georgia" = "Georgia", "Hawaii" = "Hawaii", "Idaho" = "Idaho",
                                                 "Illinois" = "Illinois", "Indiana" = "Indiana", "Iowa" = "Iowa",
                                                 "Kansas" = "Kansas", "Kentucky" = "Kentucky", "Louisiana" = "Louisiana",
                                                 "Maine" = "Maine", "Maryland" = "Maryland", "Massechussetts" = "Massechussetts",
                                                 "Michigan" = "Michigan", "Minnesota" = "Minnesota", "Mississippi" = "Mississippi",
                                                 "Missouri" = "Missouri", "Montana" = "Montana", "Nebraska" = "Nebraska",
                                                 "Nevada" = "Nevada", "New Hampshire" = "New Hampshire", "New Jersey" = "New Jersey",
                                                 "New Mexico" = "New Mexico", "New York" = "New York", "North Carolina" = "North Carolina",
                                                 "North Dakota" = "North Dakota", "Ohio" = "Ohio", "Oklahoma" = "Oklahoma",
                                                 "Oregon" = "Oregon", "Pennsylvania" = "Pennsylvania", "Rhode Island" = "Rhode Island",
                                                 "South Carolina" = "South Carolina", "South Dakota" = "South Dakota", "Tennessee" = "Tennessee",
                                                 "Texas" = "Texas", "Utah" = "Utah", "Vermont" = "Vermont",
                                                 "Virginia" = "Virginia", "Washington" = "Washington", "West Virginia" = "West Virginia",
                                                 "Wisconsin" = "Wisconsin", "Wyoming" = "Wyoming"), 
                                  selected = "Alabama"),
                      
                      hr(),
                      fluidRow(column(12, verbatimTextOutput("state")))
                    ) # end sidebarPanel
                  ) #End sidebarLayout   
                        ) # End tabPanel
               ), # End navbarPage

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
