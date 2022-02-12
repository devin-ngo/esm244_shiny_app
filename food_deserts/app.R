library(shiny)
library(here)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(leaflet.extras)

# Reading in data
food_access <- read_csv(here("data", "food_access_subset.csv")) %>% 
  mutate(median_family_income = as.character(median_family_income))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"), #Will probably customize own theme later
                titlePanel("Food Deserts in America "), # Application title 
                navbarPage("Food Access Tools",
                           tabPanel("W1 - Rural/Urban Breakdown",
                                    sidebarLayout(
                                      sidebarPanel("Breakdown of rural and urban areas by state"), #end sidebar panel
                                      selectInput("select", label = h3("Select State"), 
                                                  choices = list("Alabama" = "Alabama", "Alaska" = "Alaska", "Arizona" = "Arizona", "Arkansas" = "Arkansas", "California" = "California", 
                                                                 "Colorado" = "Colorado", "Connecticut" = "Connecticut", "Delaware" = "Delaware", "Florida" = "Florida",
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
                                                  selected = "Alabama") # end select input
                                    ), # end sidebarPanel 1
                                    mainPanel(
                                      plotOutput("distPlot")
                                    ) #End sidebarLayout 1 
                           ), #End Tab 1
                           
                           tabPanel("W2 - Income Range",
                                    sidebarLayout(
                                      sidebarPanel("Tracking access by median family income",
                                                   sliderInput("slider2", label = h3("Income Range (in thousands USD)"), min = 0, 
                                                               max = 250, value = c(50, 100)),
                                                   selectInput("select", label = h3("Select State"), 
                                                               choices = list("Alabama" = "Alabama", "Alaska" = "Alaska", "Arizona" = "Arizona", "Arkansas" = "Arkansas", "California" = "California", 
                                                                              "Colorado" = "Colorado", "Connecticut" = "Connecticut", "Delaware" = "Delaware", "Florida" = "Florida",
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
                                                               selected = "Alabama")
                                      ), #end sidebarPanel 2
                                      mainPanel(
                                        plotOutput("distPlot")) #end mainPanel
                                    ) # end sidebarLayout 2
                           ), #End tabPanel 2
                           
                           tabPanel("W3 - Ethnicity Checkbox", 
                                    sidebarLayout(
                                      sidebarPanel("Distance from a supermarket based on ethnicity group",
                                                   checkboxGroupInput("checkGroup", label = h3("Ethnicity"), 
                                                                      choices = list("American Indian and Alaska Native" = 1, "Asian" = 2, "Black or African American" = 3, 
                                                                                     "Hispanic or Latino" = 4, "Native Hawaiian and Other Pacific Islander" = 5, 
                                                                                     "White" = 6, "Other/Multiple Race Population" = 7),
                                                                      selected = 1)
                                      ), #end sidebarPanel 3
                                      mainPanel(
                                        plotOutput("distPlot")) #end mainPanel
                                    ) #end sidebar Layout
                           ), #end tabPanel 3
                           
                           tabPanel("W4 - Access Tracts",
                                    sidebarLayout(
                                      sidebarPanel("Low access tracts based on miles from supermarket",
                                                   radioButtons("radio", label = h3("Distance"),
                                                                choices = list("1/2 Mile" = 1, "1 Mile" = 2, "10 Miles" = 3, "20 Miles" = 4), 
                                                                selected = 1)
                                      ), #end sidebarPanel 4
                                      mainPanel(
                                        plotOutput("distPlot")) #end mainPanel
                                    ) #end sidebar Layout
                           ), #end tabPanel 4
                           
                           tabPanel("About Page",
                                    sidebarLayout(
                                      sidebarPanel("About this app:"
                                      ), # end sidebarPanel 5
                                      mainPanel(h6("This app is focused on examining food deserts in the US and 
                                                   how factors such as income and ethnicity play a role in the distance of individuals 
                                                   are located from supermarkets. We hope to shed a light on the issue of food insecurity
                                                   and how changes need to be made to improve access to food for disadvantaged communities")) #end mainPanel
                                    ) # end sidebarLayout
                                    ) #end tabPanel 5
                           
                ) #End Navbar Page
) # End UI

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  map <- reactive({
    food_access %>% 
      filter(state = input$state)
  })
  output$state <- renderLeaflet({ input$state })
  
  print_income <- reactive({
    food_access %>% 
      filter(income = input$median_family_income)
  })
  output$income <- renderPrint({ income })
  
  print_state <- reactive({
    food_access %>% 
      filter(state = input$state)
  })
  output$state <- renderPrint({ state })
}

# Run the application 
shinyApp(ui = ui, server = server)
