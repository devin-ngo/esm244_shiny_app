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
                                      sidebarPanel(style = "background-color: #f8f4e4",
                                        textOutput("sp_citation"),
                                        p(h4(strong(textOutput("sp_authors")))),
                                        p(h5(strong(textOutput("sp_author_km")))),
                                        textOutput("sp_km_bio"),
                                        p(h5(strong(textOutput("sp_author_dn")))),
                                        textOutput("sp_dn_bio")
                                      ), # end sidebarPanel1
                                      mainPanel(
                                        h3(textOutput("introduction")),
                                        p(textOutput("introduction_text1")),
                                        p(img(src = "food_deserts_stats.png")),
                                        p(img(src = "US_map.png", width = "600px", height = "375px")),
                                        p(textOutput("introduction_text2")),
                                        p(textOutput("introduction_text3")),
                                        p(textOutput("widget1_text")),
                                        p(textOutput("widget2_text")),
                                        p(textOutput("widget3_text")),
                                        p(textOutput("widget4_text"))
                                    ) # end mainPanel 1
                                    ) # end sidebarLayout 1
                           ), #end tabPanel 1
                           tabPanel("Rural and Urban County Breakdown",
                                    sidebarLayout(
                                      sidebarPanel(style = "background-color: #f8f4e4",
                                                   selectInput(inputId = "state", label = h4("Select State:"),
                                                               choices = unique(food_access$state), selected = "Alabama") # end select input
                                    ), # end sidebarPanel 2
                                    mainPanel(h3("Rural and Urban County Breakdown"),
                                      tmapOutput("county_map"),
                                      p(h5(strong(textOutput("county_map_figure")))),
                                      p(textOutput("county_map_text")))
                                    ) #End sidebarLayout 2
                           ), #End Tab 2
                           
                           tabPanel("Income and SNAP Program",
                                    sidebarLayout(
                                      sidebarPanel(style = "background-color: #f8f4e4",
                                                   selectInput(inputId = "state2", label = h4("Select State:"),
                                                               choices = unique(food_access$state), selected = "Alabama"),
                                                   sliderInput(inputId = "income_slider", label = h4("Select Income Range:"), min = 0, 
                                                               max = 250000, value = c(50000, 100000))
                                      ), #end sidebarPanel 3
                                      mainPanel(h3("Income and SNAP Program"),
                                        dataTableOutput(outputId = "state_pop_table"),
                                        p(h5(strong(textOutput("figure2")))),
                                        p(textOutput("figure2_text")),
                                        dataTableOutput(outputId = "income_snap_table"),
                                        p(h5(strong(textOutput("figure3")))),
                                        p(textOutput("figure3_text"))
                                        ) #end mainPanel
                                    ) # end sidebarLayout 3
                           ), #End tabPanel 3
                           
                           tabPanel("Ethnicity and Food Access",
                                    sidebarLayout(
                                      sidebarPanel(style = "background-color: #f8f4e4", 
                                          selectInput(inputId = "state3", label = h4("Select State:"),
                                                               choices = unique(food_access$state), selected = "Alabama"),  
                                                   checkboxGroupInput(inputId = "ethnicity_check", label = h4("Select Ethnicity:"),
                                                                      choices = c("White" = "white","Black or African American" = "black", 
                                                                                  "Asian" = "asian", "Native Hawaiian or Other Pacific Islander (NHOPI)" = "nhopi", 
                                                                                  "American Indian or Alaska Native (AIAN)" = "aian", "Hispanic or Latino"= "hisp",
                                                                                  "Other or Multiple Race" = "omultir"),
                                                                      selected = c("white", "black", "asian", "nhopi", "aian", "hisp", "omultir")),
                                                   radioButtons(inputId = "ethnicity_radio", label = h4("Distance from nearest supermarket:"),
                                                                choiceNames = list("1/2 Mile", "1 Mile", "10 Miles", "20 Miles"),
                                                                choiceValues = list("half", "1", "10", "20"),
                                                                selected = "half")
                                      ), #end sidebarPanel 4
                                      mainPanel(h3("Ethnicity and Food Access"),
                                        plotOutput("eth_plot"),
                                        p(h5(strong(textOutput("eth_header")))),
                                        p(textOutput("eth_text"))) #end mainPanel
                                    ) #end sidebar Layout
                           ), #end tabPanel 4

                           tabPanel("Vehicle Access and Food Access",
                                    sidebarLayout(
                                      sidebarPanel(style = "background-color: #f8f4e4",
                                                   selectInput(inputId = "state4", label = h4("Select State:"),
                                                               choices = unique(pivot_longer_vehicle$state), selected = "Alabama"),  
                                                   radioButtons(inputId = "vehicle_radio", label = h4("Select Distance from Nearest Supermarket:"),
                                                                choiceNames = list("1/2 Mile", "1 Mile", "10 Miles", "20 Miles"),
                                                                choiceValues = list("vehicle_half", "vehicle1", "vehicle10", "vehicle20"),
                                                                selected = "vehicle_half")
                                      ), #end sidebarPanel 5
                                      mainPanel(h3("Vehicle Access and Food Access"),
                                        plotlyOutput("vehicle_access"),
                                        p(h5(strong(textOutput("vehicle_header")))),
                                        p(textOutput("vehicle_text"))) #end mainPanel
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
           In 2012, the United States Department of Agriculture (USDA) reported that based on the 2000 and 2006
           census data, there were more than 6,500 food deserts within the U.S. (USDA). Based on the finding of the 
           report created by the USDA in 2012, they found the most significant factor relating to food deserts
           is poverty and ethnicity (USDA). Other significant factors are included in this figure produced by the USDA.
           A map of the United States is included below for reference.")
  })
  
  introduction_text2 <- reactive({
    print("Items found in lower income neighborhoods are often the less healthier options. This includes
          more sugar, fewer whole grains, and less produce (Khan, 2018). These low-income neighborhoods also were more  
          likely to have convenience stores rather than supermarkets, which provide less nutritious options (Khan, 2018).
          A study examining neighborhoods in Austin, Texas found that the mean percentage of healthy food available in food
          oases, areas where residents had abundant access to healthful foods, was signficantly higher than that 
          of food deserts (Jin and Lu, 2021). These findings indicate an urgent need to provide impoverished communities
          equal access to healthy foods (Jin and Lu, 2021).")
    
  })
  
  introduction_text3 <- reactive({                                    
    print("This app is focused on examining food deserts in the US and 
           how factors such as income and ethnicity play a role in the distance individuals are 
           from the nearest supermarket. We hope to shed a light on the issue of food insecurity
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
    
    state_fa_df <- data.frame(state_fa)
    
    datatable(state_fa_df,
              colnames = c("State Population" = "total_pop",
                           "Population with SNAP Benefits" = "total_snap"),
              style = "bootstrap4")
  })
  
  figure2 <- reactive({
    print("Figure 2:")
  })
  
  figure2_text <- reactive({
    print("Population of chosen state with number of households that receive SNAP benefits.")
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
    
    table_df <- data.frame(table)
  
    datatable(table_df,
              colnames = c("County" = "County", 
                           "Population with SNAP Benefits" = "sum_SNAP"),
              style = "bootstrap4")
  })
  
  figure3 <- reactive({
    print("Figure 3:")
  })
  
  figure3_text <- reactive({
    print("Interactive table showing the number of household in each county that receive SNAP benefits. Counties are
          listed based on the state chosen.")
  })
  
  output$state_pop_table <- renderDataTable({
    state_pop_table()
  })
  
  output$figure2 <- renderText({
    figure2()
  })
  
  ouput$figure2_text <- renderText({
    figure2_text()
  })
  
  output$income_snap_table <- renderDataTable({
    income_snap_table()
  })
  
  output$figure3 <- renderText({
    figure3()
  })
  
  output$figure3_text <- renderText({
    figure3_text()
  })

  # Widget 3 output
  
  eth_plot <- reactive({
    eth_table <- ethnicity_sub %>% 
      filter(state == input$state3) %>% 
      filter(ethnicity %in% input$ethnicity_check) %>% 
      filter(distance == input$ethnicity_radio) %>% 
      mutate(ethnicity = case_when(
        ethnicity == "asian" ~ "Asian",
        ethnicity == "hisp" ~ "Hispanic or Latino",
        ethnicity == "black" ~ "Black or African American",
        ethnicity == "white" ~ "White",
        ethnicity == "nhopi" ~ "NHOPI",
        ethnicity == "aian" ~ "AIAN", 
        ethnicity == "omultir" ~ "Other or Multiple Race"))
    
    ggplot(data = eth_table,
             aes(x = ethnicity, y = count)) +
      geom_col(aes(fill = ethnicity)) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      scale_fill_manual(values = c("maroon", "lightskyblue1", "darkgreen", "mediumaquamarine",
                        "darkmagenta", "salmon1", "dodgerblue4")) +
      labs(x = "Ethnicity", y = "Population Count", fill = "Ethnicity") +
      theme(legend.position = "bottom")

  })
  
  output$eth_plot <- renderPlot({
    eth_plot() 
  })
  
  eth_header <- reactive({
    print("Figure 3:")
  })
  
  eth_text <- reactive({
    print("An interactive column graph that displays the population count of different ethnicity groups 
          that are 1/2 mile, 1 mile, 10 miles, or 20 miles away from the nearest supermarket for each state. The 
          user can select which ethnicity groups they would like to look at, as well as the state and distance.")
    
  })
  
  output$eth_header <- renderText({
    eth_header()
  })
  
  output$eth_text <- renderText({
    eth_text()
  })
  
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
        labs(x = "Housing Units with No Vehicle (HUNV)",
             y = "Total Population Count")
      
      ggplotly(x)
  })
  
  output$vehicle_access <- renderPlotly({
    vehicle_access()
    })
  
  vehicle_header <- reactive({
    print("Figure 4:")
  })
  
  vehicle_text <- reactive({
    print("An interactive scatterplot that displays population count versus how many housing units have 
    no vehicle access at a specific distance from the nearest supermarket. The user can select which distances to look at:
    1/2 mile, 1 mile, 10 miles, and 20 miles from the nearest supermarket. Circles represent rural county classification 
    while triangles represent urban county classification. The user can hover over each point to see the exact number
          of housing units with no vehicle access (HUNV) for each county.")

  })
  
  output$vehicle_header <- renderText({
    vehicle_header()
  })
  
  output$vehicle_text <- renderText({
    vehicle_text()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
