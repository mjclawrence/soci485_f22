# Load packages

library(tidyverse)
library(weights)
library(DT)

#install.packages("shiny") # hashtag after installing
library(shiny)

#install.packages("shinythemes") # hashtag after installing
library(shinythemes)

# Load the data

## For the final app we will want local data files
## For now we can use the web versions

han <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/han_data_complete_pop.csv")

han <- han |> 
  relocate(xsite, .after = "NAME") |> 
  relocate(evict_pct, .after = "xsite") |> 
  relocate(population, .after = "xsite") |> 
  relocate(majority_race, .after = population) |> 
  pivot_longer(names_to = "characteristic", values_to = "value", 7:22)

# The tab with iframe embeds will need urls.
## Most of these don't work as embeds but this is where
## you will add the maps of each city in xsite order

websites <- c("https://www.datawrapper.de",
              "https://www.cnn.com",
              "https://middlebury.instructure.com",
              "https://www.nytimes.com",
              "https://www.bbc.com",
              "https://www.asanet.org",
              "https://www.rstudio.com",
              "https://www.espn.com",
              "https://www.amazon.com")

citysites <- bind_cols(unique(han$xsite), websites) |> 
  rename(xsite = 1,
         website = 2)


# First part of the app is setting up the user interface

ui <- fluidPage(

  ## The theme selector lets you experiment with different themes
  shinythemes::themeSelector(),
  
  ### Once you decide on a theme, hashtag the previous line and edit the next line to use your theme
  #shinytheme(theme = "flatly"),

  ## Application title
    titlePanel("Eviction and Race"),

  ## Sidebar has two inputs right now.
    ### The first is where users choose a city. We'll use that selection as a variable called "city"
    ### The second is where users choose one or more characteristics. We'll use that selection as a variable called "characteristic"

        sidebarPanel(
          selectizeInput(inputId = "city", #name of input
                      label = "Choose a city:", #label displayed in ui
                      choices = as.character(unique(han$xsite)),
                      selected = "PHILADELPHIA"),
          selectizeInput(inputId = "characteristic", #name of input
                        label = "Select characteristic(s):", #label displayed in ui
                        #choices = as.character(unique(han$characteristic)),
                        selected = "nonwhite_pct",
                        multiple = TRUE,
                        options = list(plugins= list('remove_button')),
                        list(
                          `Demographic Measures` = list(
                            "Pct Non White Residents" = "nonwhite_pct",
                            "Pct Asian Residents" = "asian_pct",
                            "Pct Black Residents" = "black_pct",
                            "Pct Hispanic Residents" = "hisp_pct",
                            "Pct Other Residents" = "other_pct",
                            "Pct White Residents" = "white_pct"
                          ),
                          `Zip Code Economic Indicators` = list(
                          "Gini Coefficient" = "gini",
                          "Unemployment Rate" = "unemploymentrate",
                          "Pct of Residents With College Degree or More" = "college_plus",
                          "Pct of Residents With Commutes < 15 Minutes" = "Less_than_15_minutes"),
                             `Household Economic Indicators` = list(
                               "Median Household Income" = "median_hh_income",
                               "Median Pct of Income Spent on Rent" = "rent_pct_incomeE",
                               "Pct of Households With Single Parent" = "singlerate",
                               "Pct of Households Receiving SNAP Benefits" = "snap_pct",
                               "Pct of Housing Units Occupied By Renters" = "prop_renter_occ")))
                        ), # close the sidebar panel
        
  ## Main panel has three tabs right now.
    ### We express the outputs here by name, but we create them in the server section of this file
        mainPanel(
          navbarPage(
            "Select Tab For Output",
            tabPanel("Correlation(s) With Eviction", 
                    plotOutput("eviction_scatterplot")),
            tabPanel("Descriptives By Majority Race", 
                    dataTableOutput("race_descriptives")),
            tabPanel("iFrame Test",
                      htmlOutput("frame"))
        ) # close the navbarPage
    ) # close the main panel
) # close the ui code

# Define server logic
server <- function(input, output) {
  
  ## We have two separate datasets that will react to values from the sidebar selections.
  
  ### The first filters the big dataset for the inputted city and the inputted characteristic(s)
  dataInput <- reactive({
    han |> 
      filter(xsite == input$city &
               characteristic %in% input$characteristic)
  })
  
  ### The second filter the websites for a city
  citysiteInput <- reactive({
    citysites |> 
      filter(xsite == input$city)
  })
  
## Here I am renaming each dataset after filtering
### The debounce function adds a small amount of time (500ms) between filtering the datasets and creating the output.
### Adding that time tends to make the output more responsive to the filters 

  han_filter <- dataInput |>  debounce(500)
  
  site_filter <- citysiteInput |> debounce(500)
  

  ## This section creates the scatterplot.
  ### The name we give to the object - eviction_scatterplot - is the name we will use in the output for the tab in the ui
  output$eviction_scatterplot <- renderPlot({
        plot1 <- han_filter() |> 
          group_by(characteristic) |> 
          ggplot(aes(x = value,
                     y = evict_pct,
                     color = majority_race)) +
          geom_point(aes(size = 3)) + 
          facet_wrap(~characteristic, scales = "free") +
          theme(legend.position = "bottom") + guides(size = FALSE)
    
          plot1
        })

  ## This section creates the descriptives table  
  output$race_descriptives <- renderDataTable({    
      table1 <- han_filter() |> 
        group_by(characteristic, majority_race) |> 
        summarise(mean = round(wtd.mean(value, population, na.rm = TRUE),2)) |> 
        pivot_wider(names_from = "majority_race", values_from = "mean") |> 
        datatable(rownames = FALSE,
        options = list(
          dom = 'Btip', # see explanation below
          buttons = list(list(extend = 'copy', title = NULL))))

        table1
      })
    

## This section creates the iframe embeds of the websites
  output$frame <- renderUI({ 
    
    city_embed <- site_filter()$website
    
    tags$iframe(#src = 'https://datawrapper.de', 
                src = city_embed,
                width = "100%", height = "600",
                seamless = TRUE)
  }) 
  
} # This curly bracket closes the server code



 # After the ui code and server code are set, run the application 
shinyApp(ui = ui, server = server)