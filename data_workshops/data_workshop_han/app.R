# Load packages

library(tidyverse)
library(weights)
library(DT)
library(scales)

#install.packages("shiny") # hashtag after installing
library(shiny)

#install.packages("shinythemes") # hashtag after installing
library(shinythemes)

# Load the data

## For the final app we will want local data files
## For now we can use the web versions

han <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/han_data_complete_pop.csv")
eviction_lab <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/eviction_socius.csv")

eviction_lab <- eviction_lab |> 
  select(zip, eviction_count, renter_hh_2019) |> 
  rename(GEOID = zip)

han <- left_join(han, eviction_lab)

han <- han |> 
  relocate(xsite, .after = "NAME") |> 
  relocate(evict_pct, .after = "xsite") |> 
  relocate(population, .after = "xsite") |> 
  relocate(majority_race, .after = population) |> 
  relocate(c(eviction_count, renter_hh_2019), .after = evict_pct) |> 
  pivot_longer(names_to = "characteristic", values_to = "value", 9:24) |> 
  mutate(xsite = str_to_title(xsite)) |> 
  mutate(xsite = ifelse(xsite == "Fortworth", "Fort Worth", xsite)) |> 
  mutate(label_format = ifelse(characteristic == "median_hh_income", dollar_format()(value),
                               round(value,2)))

han <- han|>
  mutate(variable_name = ifelse(characteristic == "median_hh_income", "Median Household Income",
                                ifelse(characteristic == "gini", "Gini Coefficient",
                                       ifelse(characteristic == "prop_owner_occ", "Proportion Owner-Occupied Residences",
                                              ifelse(characteristic == "prop_renter_occ", "Proportion Renter-Occupied Residences",
                                                     ifelse(characteristic == "unemploymentrate", "Unemployment Rate",
                                                            ifelse(characteristic == "singlerate", "Proportion Households Headed by Single Parent",
                                                                   ifelse(characteristic == "rent_pct_incomeE", "Percent Monthly Income Spent on Rent",
                                                                          ifelse(characteristic == "snap_pct", "Percent Households Receiving SNAP Benefits",
                                                                                 ifelse(characteristic == "nonwhite_pct", "Percent Population Nonwhite",
                                                                                        ifelse(characteristic == "white_pct", "Percent Population White",
                                                                                               ifelse(characteristic == "black_pct", "Percent Population Black",
                                                                                                      ifelse(characteristic == "hisp_pct", "Percent Population Hispanic",
                                                                                                             ifelse(characteristic == "asian_pct", "Percent Population Asian",
                                                                                                                    ifelse(characteristic == "other_pct", "Percent Population Other Race",
                                                                                                                           ifelse(characteristic == "Less_than_15_minutes", "Proportion Population with Commute Less than 15 minutes",
                                                                                                                                  "Proportion Population with College Degree or Higher"))))))))))))))))



# The tab with iframe embeds will need urls.
## Most of these don't work as embeds but this is where
## you will add the maps of each city in xsite order

websites <- c("https://datawrapper.dwcdn.net/WxsP3/2/", #philadelphia
              "https://datawrapper.dwcdn.net/JoA5X/2/", #South Bend
              "https://datawrapper.dwcdn.net/ZBvdY/2/", #Indianapolis
              "https://datawrapper.dwcdn.net/23uKF/1/", #Phoenix
              "https://datawrapper.dwcdn.net/JoA5X/2/", #Dallas
              "https://datawrapper.dwcdn.net/99I6W/1/", #Houston
              "https://datawrapper.dwcdn.net/2UH3v/1/", #Fortworth
              "https://datawrapper.dwcdn.net/qEQ69/2/", #Austin
              "https://datawrapper.dwcdn.net/yU9XH/1/") #New York

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
                   #choices = as.character(unique(han$xsite)),
                   selected = "Philadelphia",
                   list(
                     "New York",
                     "Philadelphia",
                     "Phoenix",
                     "Indiana" = list(
                       "Indianapolis",
                       "South Bend"
                     ),
                     "Texas" = list(
                       "Austin",
                       "Dallas",
                       "Fort Worth",
                       "Houston"
                     )
                   )),
    selectizeInput(inputId = "characteristic", #name of input
                   label = "Select characteristic(s):", #label displayed in ui
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
               DT::dataTableOutput("race_descriptives")),
      tabPanel("Share of Evictions and Population",
               DT::dataTableOutput("eviction_shares")),
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
  
  ### The second filters the websites for a city
  citysiteInput <- reactive({
    citysites |> 
      filter(xsite == input$city)
  })
  
  
  
  ## Here I am renaming each dataset after filtering
  ### The debounce function adds a small amount of time (500ms) between filtering the datasets and creating the output.
  ### Adding that time tends to make the output more responsive to the filters 
  
  han_filter <- dataInput |>  debounce(500)
  
  site_filter <- citysiteInput |> debounce(500)
  
  corr_coefficient <- reactive({
    han_filter() |> 
      group_by(characteristic) |> 
      summarise(coefficient = wtd.cor(value, evict_pct, population)[[1]])
  })
  
  han_filter2 <- reactive({
    left_join(han_filter(), corr_coefficient()) |> 
      mutate(label = paste0(variable_name, "\nCorrelation With Eviction = ", round(coefficient, 3)))
  })
  
  
  ## This section creates the scatterplot.
  ### The name we give to the object - eviction_scatterplot - is the name we will use in the output for the tab in the ui
  
  output$eviction_scatterplot <- renderPlot({
  
    
    
    plot1 <- han_filter2() |> 
      group_by(characteristic) |>
      ggplot(aes(x = value,
                 y = evict_pct,
                 color = majority_race)) +
      geom_point(aes(size = 3)) + 
      geom_smooth(aes(color = NULL), method = "lm", se = FALSE) +
      facet_wrap(~label, scales = "free",
                 labeller = label_wrap_gen(width=30)) + 
      theme(legend.position = "bottom") + guides(size = "none") +
      labs(x = "Value of Selected Characteristic", 
           y = "Percentage of Renter-Occupied Housing Units\nExperiencing Eviction",
           color = "Majority Race\nIn Zip Code")
    
    plot1
  })


  table1 <- reactive({
    han_filter() |> 
      mutate(total_pop = sum(population),
           total_eviction = sum(eviction_count)) |> 
    group_by(majority_race) |> 
    summarise("Eviction Rate" = round(wtd.mean(evict_pct, population, na.rm = TRUE),3),
              "Proportion of All Evictions" = round((sum(eviction_count)/total_eviction),3),
              "Proportion of Total Population" = round((sum(population)/total_pop),3)) |> 
    distinct() |> 
    pivot_longer(names_to = "variable_name", values_to = "mean", 2:4) |> 
    pivot_wider(names_from = "majority_race", values_from = "mean") 
  })
  
  
  table2 <- reactive({
    han_filter() |> 
    group_by(variable_name, majority_race) |> 
    summarise(mean = round(wtd.mean(value, population, na.rm = TRUE),2)) |> 
    pivot_wider(names_from = "majority_race", values_from = "mean")
  })
    
  ## This section creates the descriptives table  
  output$race_descriptives <- DT::renderDataTable({    
    table1 <- han_filter() |> 
      group_by(variable_name, majority_race) |> 
      summarise(mean = round(wtd.mean(value, population, na.rm = TRUE),2)) |> 
      pivot_wider(names_from = "majority_race", values_from = "mean") |> 
      datatable(rownames = FALSE,
                guides(scale = "none"),
                options = list(
                  dom = 'Btip', # see explanation below
                  buttons = list(list(extend = 'copy', title = NULL))))
    
    table1
  })
  
  ## This section creates the share of eviction and share of population table
  table2 <- reactive({
    han_filter() |> 
      mutate(total_pop = sum(population),
             total_eviction = sum(eviction_count)) |> 
      group_by(majority_race) |> 
      summarise("Eviction Rate" = round(wtd.mean(evict_pct, population, na.rm = TRUE),3),
                "Proportion of All Evictions" = round((sum(eviction_count)/total_eviction),3),
                "Proportion of Total Population" = round((sum(population)/total_pop),3)) |> 
      distinct()|>
      pivot_longer(names_to = "variable_name", values_to = "mean", 2:4) |> 
      pivot_wider(names_from = "majority_race", values_from = "mean") 
  })
  
  output$eviction_shares<- DT::renderDataTable({    
    
    table2<-table2()|> # CHANGE THE DATASET YOU ARE PULLING FROM HERE TO table2() WHICH YOU CREATED ABOVE NOT han_filter()
      #select("Eviction Rate", "Proportion of All Evictions", "Proportion of Total Population")|>
      # DON'T USE SELECT IN LINE ABOVE; THAT IS FOR CHOOSING COLUMNS BUT THESE ARE ROWS
      # AND THEY ARE THE ONLY ROWS IN THIS TABLE SO YOU WOULDN'T NEED TO FILTER FOR THEM EITHER
      datatable(rownames = FALSE,
                options = list(
                  dom = 'Btip', # see explanation below
                  buttons = list(list(extend = 'copy', title = NULL))))
    table2
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
