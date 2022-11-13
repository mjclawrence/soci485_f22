library(shiny)
library(tidyverse)
library(weights)
library(DT)
library(pollster)
library(shinythemes)

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


# Define UI for application that pulls a city and characteristics
ui <- fluidPage(
  
  shinythemes::themeSelector(),
  #shinytheme(theme = "flatly"),

    # Application title
    titlePanel("Presidential Vote By Union Household Status"),

    # Sidebar with a slider input for number of bins 
        sidebarPanel(
          selectInput(inputId = "state", #name of input
                             label = "Choose a State:", #label displayed in ui
                             choices = as.character(unique(mccallum$state)),
                             selected = "All"),
    checkboxGroupInput(inputId = "year",
                       label = "Select Year(s)",
                       choices = unique(mccallum$year),
                       selected = c("2008", "2020"))
    ),
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Year Plot", 
                    plotOutput("year_plot")),

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  dataInput <- reactive({
    mccallum |> 
      filter(state == input$state &
               year %in% input$year)
  })
  

  mccallum_filter <- dataInput |>  debounce(500)
  
  output$year_plot <- renderPlot({
        plot1 <- mccallum_filter() |> 
          ggplot(aes(x = as.factor(year), y = pct, color = party, group = party)) +
          geom_point() + geom_line() + facet_wrap(~union_any) +
          theme(legend.position = "bottom") + scale_color_manual(values = c("blue", "red", "darkgreen"))
          
          plot1
        })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


