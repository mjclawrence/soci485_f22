# Load packages

library(tidyverse)
library(weights)
library(DT)
library(pollster)

#nstall.packages("shiny") # hashtag after installing
library(shiny)

#install.packages("shinythemes") # hashtag after installing
library(shinythemes)

# Load the data

## For the final app we will want local data files
## For now we can use the web versions

mccallum <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/cces_08_20_threeparty_states.csv")
mccallum_all <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/cces_08_20_threeparty_all.csv")

mccallum_all <- mccallum_all |> 
  mutate(state = "All")

mccallum <- bind_rows(mccallum_all, mccallum) |> 
  pivot_longer(names_to = "party", values_to = "pct",
               ends_with("proportion")) |> 
  mutate(pct = pct * 100,
         party = factor(party,
                        levels = c("dem_proportion",
                                   "rep_proportion",
                                   "other_proportion"))) 


# First part of the app is setting up the user interface
ui <- fluidPage(
  
  ## The theme selector lets you experiment with different themes
  shinythemes::themeSelector(),
  
  ### Once you decide on a theme, hashtag the previous line and edit the next line to use your theme
  #shinytheme(theme = "flatly"),

    ## Application title
    titlePanel("Presidential Vote By Union Household Status"),

    ## Sidebar has two inputs right now.
      ### The first is where users choose a state. We'll use that selection as a variable called "state"
      ### The second is where users choose one or more years. We'll use that selection as a variable called "year"

        sidebarPanel(
          selectInput(inputId = "state", #name of input
                             label = "Choose a State:", #label displayed in ui
                             choices = as.character(unique(mccallum$state)),
                             selected = "All"),
    checkboxGroupInput(inputId = "year",
                       label = "Select Year(s)",
                       choices = unique(mccallum$year),
                       selected = c("2008", "2020"))
    ), # close the sidebar panel

    ## Main panel has one tab right now.
      ### We express the output here by name, but we create it in the server section of this file
        mainPanel(
          tabsetPanel(
            tabPanel("Year Plot", 
                    plotOutput("year_plot")),

        ) # close the tabset panel
    ) # close the main panel
) # close the ui code

# Define server logic
server <- function(input, output) {
  
## We have one dataset that will react to values from the sidebar selections.
  
  ### Here we filter the dataset for the inputted state and the inputted year
  dataInput <- reactive({
    mccallum |> 
      filter(state == input$state &
               year %in% input$year)
  })
  
## Here I am renaming the dataset after filtering
## The debounce function adds a small amount of time (500ms) between filtering the datasets and creating the output.
## Adding that time tends to make the output more responsive to the filters 

  mccallum_filter <- dataInput |>  debounce(500)
  

  ## This section creates the plot.
    ## The name we give to the object - year_plot - is the name we will use in the output for the tab in the ui
  output$year_plot <- renderPlot({
        plot1 <- mccallum_filter() |> 
          ggplot(aes(x = as.factor(year), y = pct, color = party, group = party)) +
          geom_point() + geom_line() + facet_wrap(~union_any) +
          theme(legend.position = "bottom") + scale_color_manual(values = c("blue", "red", "darkgreen"))
          
          plot1
        })
  
} # This curly bracket closes the server code

# After the ui code and server code are set, run the application 

shinyApp(ui = ui, server = server)


