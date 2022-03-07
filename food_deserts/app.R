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
library(plotly)

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
    eth_dist == "sum_nhopi_half" ~ "0.5",
    eth_dist == "sum_asian_half" ~ "0.5",
    eth_dist == "sum_black_half" ~ "0.5",
    eth_dist == "sum_hisp_half" ~ "0.5",
    eth_dist == "sum_aian_half" ~ "0.5",
    eth_dist == "sum_white_half" ~ "0.5",
    eth_dist == "sum_omultir_half" ~ "0.5",
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

# us_map <- img(src = "US_map.png")
# 
# desert_stat <- img(src = "food_deserts_stat.png")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"), # Will probably customize own theme later
                titlePanel("Food Deserts in America "), # Application title 
                navbarPage("Food Access Tools",
                           tabPanel("Introduction",
                                      mainPanel(
                                        textOutput("introduction_text1"),
                                        img(src = "food_deserts_stats.png"),
                                        img(src = "US_map.png"),
                                        imageOutput("us_pic"),
                                        textOutput("introduction_text2"),
                                        textOutput("widget1_text"),
                                        textOutput("widget2_text"),
                                        textOutput("widget3_text"),
                                        textOutput("widget4_text"),
                                        textOutput("introduction_text3")
                                    ) # end mainPanel 1
                           ), #end tabPanel 1
                           tabPanel("W1 - Rural/Urban Breakdown",
                                    sidebarLayout(
                                      sidebarPanel("Interactive map visualization of rural and urban areas by state and county", #end sidebar panel
                                                   selectInput(inputId = "state", label = h3("Select State"),
                                                               choices = unique(food_access$state), selected = "Alabama") # end select input
                                    ), # end sidebarPanel 2
                                    mainPanel(
                                      tmapOutput("county_map"))
                                    ) #End sidebarLayout 2
                           ), #End Tab 2
                           
                           tabPanel("W2 - Income Range",
                                    sidebarLayout(
                                      sidebarPanel("Interactive tables detailing state population, SNAP benefits, and median family income",
                                                   sliderInput(inputId = "income_slider", label = h3("Income Range"), min = 0, 
                                                               max = 250000, value = c(0, 250000)),
                                                   selectInput(inputId = "state2", label = h3("Select State"),
                                                               choices = unique(food_access$state), selected = "Alabama")  
                                      ), #end sidebarPanel 3
                                      mainPanel(
                                        tableOutput(outputId = "state_pop_table"),
                                        tableOutput(outputId = "income_snap_table")
                                        ) #end mainPanel
                                    ) # end sidebarLayout 3
                           ), #End tabPanel 3
                           
                           tabPanel("W3 - Ethnicity Checkbox",
                                    sidebarLayout(
                                      sidebarPanel("Interactive plot visualizing ethnicty groups and population counts 
                                                   that are certain distances from the nearest supermarket",
                                                   selectInput(inputId = "state3", label = h3("Select State"),
                                                               choices = unique(food_access$state), selected = "Alabama"),  
                                                   checkboxGroupInput(inputId = "ethnicity_check", label = h3("Ethnicity"),
                                                                      choices = c("White" = "white","Black or African American" = "black", 
                                                                                  "Asian" = "asian", "Native Hawaiian or Other Pacific Islander" = "nhopi", 
                                                                                  "American Indian or Alaska Native" = "aian", "Hispanic or Latino"= "hisp",
                                                                                  "Other or Multiple Race" = "omultir"),
                                                                      selected = c("white", "black", "asian"))
                                      ), #end sidebarPanel 4
                                      mainPanel(
                                        plotOutput("eth_plot")) #end mainPanel
                                    ) #end sidebar Layout
                           ), #end tabPanel 4

                           tabPanel("W4 - Access Tracts",
                                    sidebarLayout(
                                      sidebarPanel("Interactive scatterplot visualizing the total population count
                                                   vs the number of households without a vehicle at certain distances
                                                   from the nearest supermarket",
                                                   selectInput(inputId = "state4", label = h3("Select State"),
                                                               choices = unique(pivot_longer_vehicle$state), selected = "Alabama"),  
                                                   radioButtons(inputId = "vehicle_radio", label = h3("County Classification:"),
                                                                choiceNames = list("1/2 Mile", "1 Mile", "10 Miles", "20 Miles"),
                                                                choiceValues = list("vehicle_half", "vehicle1", "vehicle10", "vehicle20"),
                                                                selected = "vehicle_half")
                                      ), #end sidebarPanel 5
                                      mainPanel(
                                        plotlyOutput("vehicle_access")) #end mainPanel
                                    ) #end sidebar Layout
                           ) #end tabPanel 5
                           
                ) #End Navbar Page
) # End UI

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  introduction_text1 <- reactive({
    print("Throughout the United States, food deserts are a major issue. Food deserts are defined as
           regions where people have limited access to food that is both nutritious and helpful (Jessica
           Caporuscia, 2020). Limitations can arise due to low-income or distance to the nearest supermarket. 
           In 2012, the United States Department of Agriculture (USDA) reported that based on the 2000 and 20006
           census data, there were more than 6,500 food deserts within the U.S. (USDA). Based on the finding of the 
           report created by the USDA in 2012, they found the most significant factor relating to food deserts
           is poverty and ethnicity (USDA). Other significant factors are included in this figure produced by the USDA.
           A map of the United States is included below for reference.")
  })
  
  # food_deserts_fig <- reactive({
  #   print(desert_stat)
  # })
  # 
  # us_pic <- reactive({
  #   print(us_map)
  # })
  
  introduction_text2 <- reactive({                                    
    print("\nThis app is focused on examining food deserts in the US and 
           how factors such as income and ethnicity play a role in the distance of individuals 
           from supermarkets. We hope to shed a light on the issue of food insecurity
           and how changes need to be made to improve access to food for disadvantaged communities.")
  })
    
  widget1_text <- reactive({
    print("The first widget takes a look at the breakdown of counties by state and rural/urban designations.")
  })
  
  widget2_text <- reactive({
    print("The second examines median family income and SNAP benefits by state and county.")
  })
  
  widget3_text <- reactive({
    print("The third widget breaks down access tracts to the nearest supermarket by ethnicity and mileage.")
  })
  
  widget4_text <- reactive({
    print("The final widget looks at how many housing units do not own a vehicle within a certain 
           distance from the nearest supermarket.")
  })
  
  introduction_text3 <- reactive({                                  
    print("\nShiny app created by Kiera Matiska and Devin Ngo, Master's Candidates at the Bren School
           of Environmental Science and Management. Data is from the Food Access Research Atlas 
           and compiled by user @Tim Crammond on Kaggle. 
           The data can be retrieved from: https://www.kaggle.com/tcrammond/food-access-and-food-deserts)")
  })
  
  output$introduction_text1 <- renderText({
    introduction_text1()
  })
  
  # output$food_deserts_fig <- renderImage({
  #   food_deserts_fig()
  # })
  # 
  # output$us_pic <- renderImage({
  #   us_pic()
  # })
  # 
  output$introduction_text2 <- renderText({
    introduction_text2()
  })
  
  output$introduction_text3 <- renderText({
    introduction_text3()
  })
  
  output$widget1_text <- renderText({
    widget1_text()
  })
  
  output$widget2_text <- renderText({
    widget2_text()
  })
  
  output$widget3_text <- renderText({
    widget3_text()
  })
  
  output$widget4_text <- renderText({
    widget4_text()
  })
  
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
    state_pop_table() %>% 
      rename("Total US Population" = "total_pop",
             "Total US Population with SNAP Benefits" = "total_snap")
  })
   
  income_snap_table <- reactive({
    food_access %>%
      filter(state == input$state2) %>%
      filter(
        median_family_income >= input$income_slider[1], 
        median_family_income <= input$income_slider[2]) %>%
      rename(County = county) %>% 
      group_by(County) %>% 
      summarize(sum_SNAP = sum(tract_snap))
  })
  
  output$income_snap_table <- renderTable({
    income_snap_table() %>% 
      rename("Population with SNAP Benefits" = "sum_SNAP")
      
  })

  # Widget 3 output
  
  eth_plot <- reactive({
    eth_table <- ethnicity_sub %>% 
      filter(state == input$state3) %>% 
      filter(ethnicity == input$ethnicity_check)
    
      ggplot(data = eth_table,
             aes(x = ethnicity, y = count)) +
      geom_jitter() +
        labs(x = "Ethnicity", y = "Count")
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
             name == input$vehicle_radio) %>% 
      group_by(county) %>% 
      rename(Classification = urban,
             County = county,
             HUNV = value,
             Population = tot_pop)
    
      x <- ggplot(data = vehicle_access_data, 
             aes(x = HUNV, y = Population)) +
      geom_point(aes(color = County,shape = Classification)) +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
        theme(legend.position = "none") +
        labs(x = "Housing Units Without a Vehicle (HUNV)",
             y = "Total Population Count")
      
      ggplotly(x)
  })
  
  output$vehicle_access <- renderPlotly({
    vehicle_access()
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
