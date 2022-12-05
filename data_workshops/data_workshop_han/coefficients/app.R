# Load packages

library(tidyverse)
library(weights)
library(DT)
library(broom)
library(plotly)
library(tidytext)
library(scales)

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
  pivot_longer(names_to = "characteristic", values_to = "value", 9:24)

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


test <- han |> 
  select(-variable_name) |> 
  pivot_wider(names_from = "characteristic", values_from = "value") |> 
  relocate(nonwhite_pct, .after = evict_pct) |> 
  pivot_longer(names_to = "characteristic", values_to = "value", median_hh_income:college_plus) |> 
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

no_controls <- test |>
  select(xsite, evict_pct, nonwhite_pct, population) |> 
  distinct() |> 
  nest_by(xsite) |> 
  mutate(nonwhite_coefficient_nocontrol = list(coef(lm(scale(evict_pct) ~ scale(nonwhite_pct), 
                                                       data = data, weight = population))[[2]])) |> 
  dplyr::summarize(tidy(nonwhite_coefficient_nocontrol)) |> 
  rename(nonwhite_coefficient_nocontrol = x)


controls <- test |> 
  nest_by(xsite, variable_name) |> 
  mutate(nonwhite_coefficient_control = list(coef(lm(scale(evict_pct) ~ scale(nonwhite_pct) + scale(value), 
                                                     data = data, weight = population))[[2]])) |> 
  dplyr::summarize(tidy(nonwhite_coefficient_control)) |> 
  rename(nonwhite_coefficient_control = x)

controls_merge <- left_join(no_controls, controls) |> 
  mutate(nonwhite_coefficient_prop_diff = (nonwhite_coefficient_control - nonwhite_coefficient_nocontrol) / (nonwhite_coefficient_nocontrol))

variable_names <- han |> 
  select(characteristic, variable_name) |> 
  distinct()

controls_merge <- left_join(controls_merge, variable_names) |> 
  select(xsite, nonwhite_coefficient_nocontrol, characteristic, variable_name, nonwhite_coefficient_control,
         nonwhite_coefficient_prop_diff)



library(shiny)

# First part of the app is setting up the user interface

ui <- fluidPage(
  
  
  ## Application title
  titlePanel("Eviction and Race"),
  
  ## Sidebar has two inputs right now.
  ### The first is where users choose a city. We'll use that selection as a variable called "city"
  ### The second is where users choose one or more characteristics. We'll use that selection as a variable called "characteristic"
  
  sidebarPanel(
    selectizeInput(inputId = "city", #name of input
                   label = "Choose a city:", #label displayed in ui
                   choices = as.character(unique(han$xsite)),
                   selected = "PHILADELPHIA")
  ), # close the sidebar panel
  
  ## Main panel has three tabs right now.
  ### We express the outputs here by name, but we create them in the server section of this file
  mainPanel(
    navbarPage(
      "Select Tab For Output",
      tabPanel("Coefficients", 
               plotlyOutput("coefficient_plot"))
    ) # close the navbarPage
  ) # close the main panel
) # close the ui code

# Define server logic
server <- function(input, output) {
  
  ## We have two separate datasets that will react to values from the sidebar selections.
  
  ### The first filters the big dataset for the inputted city and the inputted characteristic(s)
  dataInput <- reactive({
    controls_merge |> 
      filter(xsite == input$city)
  })
  
  coefficient_filter <- dataInput |>  debounce(500)
  
  plot1 <- reactive({
    coefficient_filter() |> 
      mutate(diff_sign = ifelse(nonwhite_coefficient_control<nonwhite_coefficient_nocontrol, "negative", "positive")) |> 
      ggplot(aes(y = reorder_within(variable_name, nonwhite_coefficient_control, xsite), 
                 x = nonwhite_coefficient_control, color = diff_sign,
                 text = str_wrap(paste0("In ", xsite, ", an increase of 1 standard deviation
                               in the percent of nonwhite residents in a zipcode
                               is associated with an increase in the eviction percent of ",
                               round(nonwhite_coefficient_nocontrol, 3),
                               ".<br>
                               After controlling for ", variable_name, 
                               " that association changes to ",
                               round(nonwhite_coefficient_control, 3), "."),30))) + 
      scale_y_reordered() +
      geom_point() +
      guides(color = "none") +
      labs(y = "",
           x = "Coefficient for Percent Of Zipcode Residents Who Are Not White",
           caption = "Positive values mean the control variable is positively correlated with ") 
  })
  
  ## This section creates the plot.
  output$coefficient_plot <- renderPlotly({
    
    ggplotly(plot1(),
             tooltip = "text") |> 
      layout(hoverlabel = list(align = "left"))
  })


  
} # This curly bracket closes the server code



# After the ui code and server code are set, run the application 
shinyApp(ui = ui, server = server)