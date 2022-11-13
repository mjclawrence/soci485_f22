library(shiny)
library(tidyverse)
library(weights)
library(DT)
library(pollster)

tiger <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/tiger_w74_92.csv")

tiger <- tiger |> 
  relocate(c("cancul1", "crimsent", "weight_w92"), .after = "qkey") |> 
  pivot_longer(names_to = "characteristic", values_to = "value", 5:9) |> 
  filter(!is.na(cancul1)) |> 
  filter(!is.na(crimsent)) |> 
  filter(!is.na(value))


# Define UI for application that pulls a city and characteristics
ui <- fluidPage(

    # Application title
    titlePanel("Formal and Informal Punishment"),

    # Sidebar with a slider input for number of bins 
        sidebarPanel(
          selectInput(inputId = "characteristic", #name of input
                             label = "Select characteristic(s):", #label displayed in ui
                             choices = as.character(unique(tiger$characteristic)),
                             selected = "education")
                        ),
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Descriptive Plot", 
                    plotOutput("descriptive_plot")),
            tabPanel("Hypothesis Test", 
                    helpText("Highlighted groups indicate a significant association between beliefs about informal and formal punishment"),
                    dataTableOutput("hypothesis_table")),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  dataInput <- reactive({
    tiger |> 
      filter(characteristic %in% input$characteristic)
  })
  
  
  tiger_filter <- dataInput |>  debounce(500)
  
  output$descriptive_plot <- renderPlot({
        plot1 <- tiger_filter() |> 
          crosstab_3way(cancul1, crimsent, value, weight_w92,
                        format = "long") |> 
          ggplot(aes(x = cancul1, y = pct,
                     fill = crimsent)) +
          geom_col() + facet_wrap(~value) +
          theme(legend.position = "bottom")
          
          plot1
        })
  
  output$hypothesis_table <- renderDataTable({
    table1 <- tiger_filter() |> 
      group_by(value) |> 
      summarise(chisq_p = wtd.chi.sq(cancul1, crimsent, 
                                         weight = weight_w92)[[3]]) |> 
      mutate(chisq_p = sprintf("%1.3f",chisq_p)) |> 
      datatable(rownames = FALSE,
                caption = "A Title Here?",
                options = list(
                  dom = 'Btip', # see explanation below
                  buttons = list(list(extend = 'copy', 
                                      title = NULL)))) |> 
      formatStyle(
        "chisq_p",
        target = 'row',
        backgroundColor = styleInterval(0.05, c('cornflowerblue', 'white'))) |> 
      formatStyle(
        "chisq_p",
        target = "row",
        fontWeight = styleInterval(0.05, c('bold', 'normal'))
      )
    
    table1
        })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


