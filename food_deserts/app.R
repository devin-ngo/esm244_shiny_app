library(shiny)
library(here)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(tmap)
library(tmaptools)
library(sf)
library(janitor)
library(rasterize)

# Reading in data
food_access <- read_csv(here("data", "food_access_subset.csv"))

county_sf <- read_sf(here("data/US_County_Boundaries/US_County_Boundaries.shp")) %>% 
  clean_names()

county_subset_sf <- county_sf %>% 
  select(state, county, shape_area)

state_subset_sf <- county_sf %>% 
  select(state, shape_area)

rur_urb_geom_sf <- read_sf(here("rur_urb_geom_sf/rur_urb_geom_sf.shp"))

vehicle_food <- read_csv(here("data", "vehicle_food_subset.csv"))

pivot_longer_vehicle <- vehicle_food %>% 
  pivot_longer(vehicle_half:vehicle20)

ethnicity <- read_csv(here("data","ethnicity_subset.csv"))

ethnicity_sub <- ethnicity %>% 
  mutate(dist_eth = case_when(
    eth_dist == "sum_nhopi_half" ~ "half",
    eth_dist == "sum_asian_half" ~ "half",
    eth_dist == "sum_black_half" ~ "half",
    eth_dist == "sum_hisp_half" ~ "half",
    eth_dist == "sum_aian_half" ~ "half",
    eth_dist == "sum_white_half" ~ "half",
    eth_dist == "sum_omultir_half" ~ "half",
    eth_dist == "sum_nhopi_1" ~ "1",
    eth_dist == "sum_asian_1" ~ "1",
    eth_dist == "sum_black_1" ~ "1",
    eth_dist == "sum_hisp_1" ~ "1",
    eth_dist == "sum_aian_1" ~ "1",
    eth_dist == "sum_white_1" ~ "1",
    eth_dist == "sum_omultir_1" ~ "1",
    eth_dist == "sum_nhopi_10" ~ "10",
    eth_dist == "sum_asian_10" ~ "10",
    eth_dist == "sum_black_10" ~ "10",
    eth_dist == "sum_hisp_10" ~ "10",
    eth_dist == "sum_aian_10" ~ "10",
    eth_dist == "sum_white_10" ~ "10",
    eth_dist == "sum_omultir_10" ~ "10",
    eth_dist == "sum_nhopi_20" ~ "20",
    eth_dist == "sum_asian_20" ~ "20",
    eth_dist == "sum_black_20" ~ "20",
    eth_dist == "sum_hisp_20" ~ "20",
    eth_dist == "sum_aian_20" ~ "20",
    eth_dist == "sum_white_20" ~ "20",
    eth_dist == "sum_omultir_20" ~ "20"
  )) %>% 
  mutate(ethnicity = case_when(
    eth_dist == "sum_nhopi_half" ~ "nhopi",
    eth_dist == "sum_asian_half" ~ "asian",
    eth_dist == "sum_black_half" ~ "black",
    eth_dist == "sum_hisp_half" ~ "hisp",
    eth_dist == "sum_aian_half" ~ "aian",
    eth_dist == "sum_white_half" ~ "white",
    eth_dist == "sum_omultir_half" ~ "omultir",
    eth_dist == "sum_nhopi_1" ~ "nhopi",
    eth_dist == "sum_asian_1" ~ "asian",
    eth_dist == "sum_black_1" ~ "black",
    eth_dist == "sum_hisp_1" ~ "hisp",
    eth_dist == "sum_aian_1" ~ "aian",
    eth_dist == "sum_white_1" ~ "white",
    eth_dist == "sum_omultir_1" ~ "omultir",
    eth_dist == "sum_nhopi_10" ~ "nhopi",
    eth_dist == "sum_asian_10" ~ "asian",
    eth_dist == "sum_black_10" ~ "black",
    eth_dist == "sum_hisp_10" ~ "hisp",
    eth_dist == "sum_aian_10" ~ "aian",
    eth_dist == "sum_white_10" ~ "white",
    eth_dist == "sum_omultir_10" ~ "omultir",
    eth_dist == "sum_nhopi_20" ~ "nhopi",
    eth_dist == "sum_asian_20" ~ "asian",
    eth_dist == "sum_black_20" ~ "black",
    eth_dist == "sum_hisp_20" ~ "hisp",
    eth_dist == "sum_aian_20" ~ "aian",
    eth_dist == "sum_white_20" ~ "white",
    eth_dist == "sum_omultir_20" ~ "omultir"
  ))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"), # Will probably customize own theme later
                titlePanel("Food Deserts in America "), # Application title 
                navbarPage("Food Access Tools",
                           tabPanel("W1 - Rural/Urban Breakdown",
                                    sidebarLayout(
                                      sidebarPanel("Breakdown of rural and urban areas by state", #end sidebar panel
                                                   selectInput(inputId = "state", label = h3("Select State"),
                                                               choices = unique(food_access$state), selected = "Alabama") # end select input
                                    ), # end sidebarPanel 1
                                    mainPanel(
                                      tmapOutput("county_map"))
                                    ) #End sidebarLayout 1
                           ), #End Tab 1
                           
                           tabPanel("W2 - Income Range",
                                    sidebarLayout(
                                      sidebarPanel("Tracking access by median family income",
                                                   sliderInput(inputId = "income_slider", label = h3("Income Range"), min = 0, 
                                                               max = 250000, value = c(0, 250000)),
                                                   selectInput(inputId = "state2", label = h3("Select State"),
                                                               choices = unique(food_access$state), selected = "Alabama")  
                                      ), #end sidebarPanel 2
                                      mainPanel(
                                        tableOutput(outputId = "state_pop_table"),
                                        tableOutput(outputId = "income_snap_table")
                                        ) #end mainPanel
                                    ) # end sidebarLayout 2
                           ), #End tabPanel 2
                           
                           tabPanel("W3 - Ethnicity Checkbox",
                                    sidebarLayout(
                                      sidebarPanel("Distance from a supermarket based on ethnicity group",
                                                   selectInput(inputId = "state3", label = h3("Select State"),
                                                               choices = unique(food_access$state), selected = "Alabama"),  
                                                   checkboxGroupInput(inputId = "ethnicity_check", label = h3("Ethnicity"),
                                                                      choices = c("White" = "white","Black or African American" = "black", 
                                                                                  "Asian" = "asian", "Native Hawaiian or Other Pacific Islander" = "nhopi", 
                                                                                  "American Indian or Alaska Native" = "aian", "Hispanic or Latino"= "hisp",
                                                                                  "Other or Multiple Race" = "omultir"),
                                                                      selected = c("white", "black", "asian"))
                                      ), #end sidebarPanel 3
                                      mainPanel(
                                        plotOutput("eth_plot")) #end mainPanel
                                    ) #end sidebar Layout
                           ), #end tabPanel 3

                           tabPanel("W4 - Access Tracts",
                                    sidebarLayout(
                                      sidebarPanel("Low access tracts based on miles from supermarket",
                                                   selectInput(inputId = "state4", label = h3("Select State"),
                                                               choices = unique(pivot_longer_vehicle$state), selected = "Alabama"),  
                                                   radioButtons(inputId = "vehicle_radio", label = h3("County Classification:"),
                                                                choiceNames = list("1/2 Mile", "1 Mile", "10 Miles", "20 Miles"),
                                                                choiceValues = list("vehicle_half", "vehicle1", "vehicle10", "vehicle20"),
                                                                selected = "vehicle_half")
                                      ), #end sidebarPanel 4
                                      mainPanel(
                                        plotOutput("vehicle_access")) #end mainPanel
                                    ) #end sidebar Layout
                           ), #end tabPanel 4
                           
                           tabPanel("About Page",
                                    sidebarLayout(
                                      sidebarPanel("About this app:"
                                      ), # end sidebarPanel 5
                                      mainPanel(h6("This app is focused on examining food deserts in the US and 
                                                   how factors such as income and ethnicity play a role in the distance of individuals 
                                                   are located from supermarkets. We hope to shed a light on the issue of food insecurity
                                                   and how changes need to be made to improve access to food for disadvantaged communities"),
                                                plotOutput("state_map")) #end mainPanel
                                    ) # end sidebarLayout
                                    ) #end tabPanel 5
                           
                ) #End Navbar Page
) # End UI

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  county_map <- reactive({
    state_county_sf <- rur_urb_geom_sf %>%
      filter(state == input$state) %>% 
      st_as_sf(state_county_sf)

    tmap_mode(mode = "view")
    tmap_options(check.and.fix = TRUE,
                 max.categories = 80)

    county_tmap <- tm_shape(state_county_sf) +
      tm_fill("type", legend.show = FALSE, popup.vars = c("County" = "county", "Total Population (2010)" = "total_pop", "County Classification" = "type")) +
      tm_borders(col = "black")

    print(county_tmap)
    return(county_tmap)
  })

  output$county_map <- renderTmap({
    county_map()
  })
  
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
    food_access %>%
      filter(state == input$state2) %>%
      filter(
        median_family_income >= input$income_slider[1], 
        median_family_income <= input$income_slider[2]) %>%
      group_by(county) %>% 
      summarize(mean_SNAP = mean(tract_snap))
  })
  
  output$income_snap_table <- renderTable({
    income_snap_table()
  })

  # Widget 3 output
  
  eth_plot <- reactive({
    eth_table <- ethnicity_sub %>% 
      filter(state == input$state3) %>% 
      filter(ethnicity == input$ethnicity_check)
    
      ggplot(data = eth_table,
             aes(x = ethnicity, y = count)) +
      geom_jitter()
  })
  
  output$eth_plot <- renderPlot({
    eth_plot()
  })
  
  # ethn_plot <- reactive({
  #   eth_10 <- ethnicity %>% 
  #     filter(state == input$state3) %>% 
  #     filter(eth_dist == input$ethnicity_check)
  #   
  #   ggplot(data = eth_10,
  #          aes(x = eth_dist, y = count)) +
  #     geom_col() +
  #     coord_flip() + 
  #     scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  # })
  # 
  # output$ethn_plot <- renderPlot({
  #   ethn_plot()
  # })
  # 
  
  # widget 4 output
  
  vehicle_access <- reactive({
    vehicle_access_data <- pivot_longer_vehicle %>% 
      filter(state == input$state4,
             name == input$vehicle_radio)
    
      ggplot(data = vehicle_access_data, 
             aes(x = value, y = tot_pop)) +
      geom_point(aes(color = county,shape = urban)) +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
        theme(legend.position = "none")
  })
  
  output$vehicle_access <- renderPlot({
    vehicle_access()
    })
  
  # About Page
  state_map <- reactive({
    tmap_mode(mode = "view")
        state_tmap <- tm_shape(state_subset_sf) +
      tm_fill("state") +
      tm_borders(col = "black")
    print(state_map)
    return(state_map)
  })
  
  output$state_map <- renderPlot({
    state_map()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
