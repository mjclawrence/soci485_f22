library(shiny)
library(tidyverse)
library(weights)
library(DT)
library(pollster)

mccallum <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/cces_08_20_rev.csv")

mccallum <- mccallum |> 
  pivot_longer(names_to = "union", values_to = "percent", starts_with("union")) |> 
  mutate(percent = percent * 100)

# Define UI for application that pulls a city and characteristics
ui <- fluidPage(

    # Application title
    titlePanel("Democratic Vote By Union Household Status"),

    # Sidebar with a slider input for number of bins 
        sidebarPanel(
          selectInput(inputId = "state", #name of input
                             label = "Choose a State:", #label displayed in ui
                             choices = as.character(unique(mccallum$state)),
                             selected = "Alabama"),
    checkboxGroupInput(inputId = "year",
                       label = "Select Year(s)",
                       choices = unique(mccallum$year),
                       selected = "2020")
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
          ggplot(aes(x = as.factor(year), y = percent, fill = union)) +
          geom_col(position = position_dodge()) +
          theme(legend.position = "bottom")
          
          plot1
        })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


