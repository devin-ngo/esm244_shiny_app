library(shiny)
library(shinydashboard)
library(here)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(janitor)
library(rasterize)
library(plotly)
library(bslib)
library(DT)

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
  separate(eth_dist,
           sep = "_",
           into = c("sum", "ethnicity", "distance"))

my_theme <- bs_theme(bootswatch = "flatly",
                     primary = "#1b6535",
                     success = "#a8c66c")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = my_theme, # Will probably customize own theme later
                titlePanel("Food Deserts in America"), # Application title 
                navbarPage("Food Access Tools",
                           tabPanel("Introduction",
                                    sidebarLayout(
                                      sidebarPanel(
                                        textOutput("sp_citation"),
                                        p(h4(strong(textOutput("sp_authors")))),
                                        p(h5(strong(textOutput("sp_author_km")))),
                                        textOutput("sp_km_bio"),
                                        p(h5(strong(textOutput("sp_author_dn")))),
                                        textOutput("sp_dn_bio")
                                      ), # end sidebarPanel1
                                      mainPanel(style = "border-style: solid; border-color: black",
                                        h3(textOutput("introduction")),
                                        p(textOutput("introduction_text1")),
                                        p(img(src = "food_deserts_stats.png")),
                                        p(img(src = "US_map.png", width = "800px", height = "500px")),
                                        p(textOutput("introduction_text2")),
                                        p(textOutput("widget1_text")),
                                        p(textOutput("widget2_text")),
                                        p(textOutput("widget3_text")),
                                        p(textOutput("widget4_text"))
                                    ) # end mainPanel 1
                                    ) # end sidebarLayout 1
                           ), #end tabPanel 1
                           tabPanel("Rural and Urban County Breakdown",
                                    sidebarLayout(
                                      sidebarPanel("Interactive map visualization of rural and urban areas by state and county", #end sidebar panel
                                                   selectInput(inputId = "state", label = h3("Select State"),
                                                               choices = unique(food_access$state), selected = "Alabama") # end select input
                                    ), # end sidebarPanel 2
                                    mainPanel(
                                      tmapOutput("county_map"),
                                      p(h5(strong(textOutput("county_map_figure")))),
                                      p(textOutput("county_map_text")))
                                    ) #End sidebarLayout 2
                           ), #End Tab 2
                           
                           tabPanel("Income and SNAP Program",
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
                           
                           tabPanel("Ethnicity and Food Access",
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
                                        plotlyOutput("eth_plot")) #end mainPanel
                                    ) #end sidebar Layout
                           ), #end tabPanel 4

                           tabPanel("Vehicle Access and Food Access",
                                    sidebarLayout(
                                      sidebarPanel("Interactive scatterplot visualizing the total population count
                                                   vs the number of households without a vehicle at certain distances
                                                   from the nearest supermarket",
                                                   selectInput(inputId = "state4", label = h3("Select State"),
                                                               choices = unique(pivot_longer_vehicle$state), selected = "Alabama"),  
                                                   radioButtons(inputId = "vehicle_radio", label = h3("Distance from nearest supermarket"),
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
 
  # Sidebar panel text for introduction tab 
  sp_citation <- reactive({
    print("Data is from the Food Access Research Atlas 
           and compiled by user @Tim Crammond on Kaggle. 
           The data can be retrieved from: https://www.kaggle.com/tcrammond/food-access-and-food-deserts")
  })
  
  sp_authors <- reactive({
    print("Authors:")
  })
  
  sp_author_km <- reactive({
    print("Kiera Matiska")
  })
  
  sp_km_bio <- reactive({
    print("I am a Master's Student at the Bren School of Environmental Science and Management at the University of California - 
          Santa Barbara, specializing in Conservation Planning and Energy and Climate. I am interested in wildlife and natural
          resource conservation and energy disparities within the United States. I hope to be able to use the skills I learn at
          Bren to work at the intersection of how energy systems in the United States and worldwide impact natural ecosystems
          in nearby areas.")
  })
  
  sp_author_dn <- reactive({
    print("Devin Ngo")
  })
  
  sp_dn_bio <- reactive({
    print("I am a Master's Student at the Bren School of Environmental Science and Management at the University of California, Santa
          Barbara, planning to specialize in Energy and Climate. I am looking to better understand energy systems and look at ways 
          technology and policy impacts energy production and consumption. I also want to incorporate environmental justice in my work by 
          looking at solutions to address the disparities disavantaged communities face from climate impacts.")
  })
  
  output$sp_citation <- renderText({
    sp_citation()
  })
  
  output$sp_authors <- renderText({
    sp_authors()
  })
  
  output$sp_author_km <- renderText({
    sp_author_km()
  })
  
  output$sp_km_bio <- renderText({
    sp_km_bio()
  })
  
  output$sp_author_dn <- renderText({
    sp_author_dn()
  })
  
  output$sp_dn_bio <- renderText({
    sp_dn_bio()
  })
  
  # main panel outputs for the introduction tab
  introduction <- reactive({
    print("Introduction")
  })
  
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
  
  introduction_text2 <- reactive({                                    
    print("This app is focused on examining food deserts in the US and 
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
  
  output$introduction <- renderText({
    introduction()
  })
  
  output$introduction_text1 <- renderText({
    introduction_text1()
  })

  output$introduction_text2 <- renderText({
    introduction_text2()
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
  
  # Main panel output for map tab
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
  
  county_map_figure <- reactive({
    print("Figure 1:")
  })

  county_map_text <- reactive({
    print("An interactive map that will switch displays depending on which state is chosen. Counties are colored based
          on their county classification: yellow for counties that are more urban and blue for counties that are more 
          rural. When a county is chosen, you can view the county name, the total population in 2010, and the county 
          classification.") # try renderText
  })
  
  output$county_map <- renderTmap({
    county_map()
  })
  
  output$county_map_figure <- renderText({
    county_map_figure()
  })
  
  output$county_map_text <- renderText({
    county_map_text()
  })
  
  state_pop_table <- reactive({
    state_fa <- food_access %>%
    filter(state == input$state2) %>%
      summarize(total_pop = sum(pop2010),
                total_snap = sum(tract_snap))
    print(state_fa)
    return(state_fa)
  })
  
  output$state_pop_table <- renderTable({
    state_pop_table() %>% 
      rename("State Population" = "total_pop",
             "Population with SNAP Benefits" = "total_snap")
  })
   
  income_snap_table <- reactive({
    table <- food_access %>%
      filter(state == input$state2) %>%
      filter(
        median_family_income >= input$income_slider[1], 
        median_family_income <= input$income_slider[2]) %>%
      rename(County = county) %>% 
      group_by(County) %>% 
      summarize(sum_SNAP = sum(tract_snap))
    
    # datatable(table)
  })
  
  output$income_snap_table <- renderTable({
    income_snap_table() 
      # rename("Population with SNAP Benefits" = "sum_SNAP")
  })

  # Widget 3 output
  
  eth_plot <- reactive({
    eth_table <- ethnicity_sub %>% 
      filter(state == input$state3) %>% 
      filter(ethnicity %in% input$ethnicity_check)
    
    y <-  ggplot(data = eth_table,
             aes(x = ethnicity, y = count)) +
      geom_jitter(aes(color = distance)) +
        labs(x = "Ethnicity", y = "Count") +
        theme(legend.position = "bottom")
    
    ggplotly(y)
  })
  
  output$eth_plot <- renderPlotly({
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
