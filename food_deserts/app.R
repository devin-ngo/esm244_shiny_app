library(shiny)
library(here)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(tmap)
library(tmaptools)

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
                                      selectInput(inputId = "state", label = h3("Select State"),
                                                  choices = list("Alabama", "Alaska", "Arizona",
                                                                 "Arkansas", "California", "Colorado",
                                                                 "Connecticut", "Delaware", "Florida",
                                                                 "Georgia", "Hawaii", "Idaho",
                                                                 "Illinois", "Indiana", "Iowa",
                                                                 "Kansas", "Kentucky", "Louisiana",
                                                                 "Maine", "Maryland", "Massechussetts",
                                                                 "Michigan", "Minnesota", "Mississippi",
                                                                 "Missouri", "Montana", "Nebraska",
                                                                 "Nevada", "New Hampshire", "New Jersey",
                                                                 "New Mexico", "New York", "North Carolina",
                                                                 "North Dakota", "Ohio", "Oklahoma",
                                                                 "Oregon", "Pennsylvania", "Rhode Island",
                                                                 "South Carolina", "South Dakota", "Tennessee",
                                                                 "Texas", "Utah", "Vermont",
                                                                 "Virginia", "Washington", "West Virginia",
                                                                 "Wisconsin", "Wyoming"),
                                                  selected = "Alabama") # end select input
                                    ), # end sidebarPanel 1
                                    mainPanel(
                                      textOutput("selected_state")
                                    ) #End sidebarLayout 1
                           ), #End Tab 1
                           
                           tabPanel("W2 - Income Range",
                                    sidebarLayout(
                                      sidebarPanel("Tracking access by median family income",
                                                   sliderInput(inputId = "median_family_income", label = h3("Income Range (in thousands USD)"), min = 0, 
                                                               max = 250, value = c(50, 100)),
                                                   selectInput(inputId = "state2", label = h3("Select State"), 
                                                               choices = list("Alabama", "Alaska", "Arizona", 
                                                                              "Arkansas", "California", "Colorado",
                                                                              "Connecticut", "Delaware", "Florida",
                                                                              "Georgia", "Hawaii", "Idaho",
                                                                              "Illinois", "Indiana", "Iowa",
                                                                              "Kansas", "Kentucky", "Louisiana",
                                                                              "Maine", "Maryland", "Massechussetts",
                                                                              "Michigan", "Minnesota", "Mississippi",
                                                                              "Missouri", "Montana", "Nebraska",
                                                                              "Nevada", "New Hampshire", "New Jersey",
                                                                              "New Mexico", "New York", "North Carolina",
                                                                              "North Dakota", "Ohio", "Oklahoma",
                                                                              "Oregon", "Pennsylvania", "Rhode Island",
                                                                              "South Carolina", "South Dakota", "Tennessee",
                                                                              "Texas", "Utah", "Vermont",
                                                                              "Virginia", "Washington", "West Virginia",
                                                                              "Wisconsin", "Wyoming"),
                                                               selected = "Alabama")
                                      ), #end sidebarPanel 2
                                      mainPanel(
                                        tableOutput(outputId = "state_pop_table"),
                                        # tableOutput(outputId = "income_snap_table")
                                        ) #end mainPanel
                                    ) # end sidebarLayout 2
                           ), #End tabPanel 2
                           
                           tabPanel("W3 - Ethnicity Checkbox",
                                    sidebarLayout(
                                      sidebarPanel("Distance from a supermarket based on ethnicity group",
                                                   checkboxGroupInput(inputId = "ethnicity_check", label = h3("Ethnicity"),
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
                                                   radioButtons(inputId = "distance_radio", label = h3("Distance"),
                                                                choices = list("1/2 Mile" = 1, "1 Mile" = 2, "10 Miles" = 3, "20 Miles" = 4),
                                                                selected = 1)
                                      ), #end sidebarPanel 4
                                      mainPanel(
                                        ) #end mainPanel
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
  
  output$selected_state <- renderText({
    paste("You have selected", input$state)
  })

  map <- reactive({
    food_access %>%
      filter(state == input$state)
  })
  output$state <- renderLeaflet({ input$state })
  
  state_pop_table <- reactive({
    message("Input$state2 = ", input$state2)
    state_fa <- food_access %>% 
    filter(state == input$state2) %>% 
      summarize(total_pop = sum(pop2010),
                total_snap = sum(tract_snap))
    print(state_fa)
    return(state_fa)
  })
  output$state_pop_table <- renderTable({
    message("message 2")
    state_pop_table()
  })
  
  income_snap_table <- reactive({
    message("Income-snap table reactive")
    food_access %>% 
      filter(income == input$median_family_income, state == input$state2) %>% 
      mutate(median_family_income = case_when(
        input$median_family_income >= "0" & input$median_family_income < "25000" ~ "1",
        input$median_family_income >= "25000" & input$median_family_income < "50000" ~ "2",
        input$median_family_income >= "50000" & input$median_family_income < "75000" ~ "3",
        input$median_family_income >= "75000" & input$median_family_income < "100000" ~ "4",
        input$median_family_income >= "100000" & input$median_family_income < "125000" ~ "5",
        input$median_family_income >= "125000" & input$median_family_income < "150000" ~ "6",
        input$median_family_income >= "150000" & input$median_family_income < "175000" ~ "7",
        input$median_family_income >= "175000" & input$median_family_income < "200000" ~ "8",
        input$median_family_income >= "200000" & input$median_family_income < "225000" ~ "9",
        input$median_family_income >= "225000" & input$median_family_income <= "250000" ~ "10"
      )) %>% 
      group_by(input$state, input$median_family_income) %>% 
      summarize(mean_SNAP = mean(tract_snap))
  })
  output$income_snap_table <- renderTable({ 
    message("render table")
    income_snap_table() })
}

# Run the application 
shinyApp(ui = ui, server = server)

# # white
# eth_white_sub <- food_access %>% 
#   select(state, urban, lawhitehalf, lawhite1, lawhite10, lawhite20) %>% 
#   group_by(state) %>% 
#   summarize(sum_white_half = sum(lawhitehalf),
#             sum_white_1 = sum(lawhite1),
#             sum_white_10 = sum(lawhite10),
#             sum_white_20 = sum(lawhite20))
# 
# # black
# eth_black_sub <- food_access %>% 
#   select(state, urban, lablackhalf, lablack1, lablack10, lablack20) %>% 
#   group_by(state) %>% 
#   summarize(sum_black_half = sum(lablackhalf),
#             sum_black_1 = sum(lablack1),
#             sum_black_10 = sum(lablack10),
#             sum_black_20 = sum(lablack20))
# 
# #asian
# eth_asian_sub <- food_access %>% 
#   select(state, urban, laasianhalf, laasian1, laasian10, laasian20) %>% 
#   group_by(state) %>% 
#   summarize(sum_asian_half = sum(laasianhalf),
#             sum_asian_1 = sum(laasian1),
#             sum_asian_10 = sum(laasian10),
#             sum_asian_20 = sum(laasian20))
# 
# # Native Hawaiian or other Pacific Islander
# eth_nhopi_sub <- food_access %>% 
#   select(state, urban, lanhopihalf, lanhopi1, lanhopi10, lanhopi20) %>% 
#   group_by(state) %>% 
#   summarize(sum_nhopi_half = sum(lanhopihalf),
#             sum_nhopi_1 = sum(lanhopi1),
#             sum_nhopi_10 = sum(lanhopi10),
#             sum_nhopi_20 = sum(lanhopi20))
# 
# # American Indian or Alaska Native
# eth_aian_sub <- food_access %>% 
#   select(state, urban, laaianhalf, laaian1, laaian10, laaian20) %>% 
#   group_by(state) %>% 
#   summarize(sum_aian_half = sum(laaianhalf),
#             sum_aian_1 = sum(laaian1),
#             sum_aian_10 = sum(laaian10),
#             sum_aian_20 = sum(laaian20))
# 
# # Hispanic
# eth_hisp_sub <- food_access %>% 
#   select(state, urban, lahisphalf, lahisp1, lahisp10, lahisp20) %>% 
#   group_by(state) %>% 
#   summarize(sum_hisp_half = sum(lahisphalf),
#             sum_hisp_1 = sum(lahisp1),
#             sum_hisp_10 = sum(lahisp10),
#             sum_hisp_20 = sum(lahisp20))
# 
# eth_omultir_sub <- food_access %>% 
#   select(state, urban, laomultirhalf, laomultir1, laomultir10, laomultir20) %>% 
#   group_by(state) %>% 
#   summarize(sum_omultir_half = sum(laomultirhalf),
#             sum_omultir_1 = sum(laomultir1),
#             sum_omultir_10 = sum(laomultir10),
#             sum_omultir_20 = sum(laomultir20))
