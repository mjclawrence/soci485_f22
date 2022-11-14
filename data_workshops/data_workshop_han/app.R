library(shiny)
library(tidyverse)
library(weights)
library(DT)
library(shinythemes)

han <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/han_data_complete_pop.csv")

han <- han |> 
  relocate(xsite, .after = "NAME") |> 
  relocate(evict_pct, .after = "xsite") |> 
  relocate(population, .after = "xsite") |> 
  relocate(majority_race, .after = population) |> 
  pivot_longer(names_to = "characteristic", values_to = "value", 7:22)

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


# Define UI for application that pulls a city and characteristics
ui <- fluidPage(
  
  shinythemes::themeSelector(),
  #shinytheme(theme = "flatly"),

    # Application title
    titlePanel("Eviction and Race"),

    # Sidebar with a slider input for number of bins 
        sidebarPanel(
          selectizeInput(inputId = "city", #name of input
                      label = "Choose a city:", #label displayed in ui
                      choices = as.character(unique(han$xsite)),
                      selected = "PHILADELPHIA"),
          selectizeInput(inputId = "characteristic", #name of input
                        label = "Select characteristic(s):", #label displayed in ui
                        choices = as.character(unique(han$characteristic)),
                        selected = "nonwhite_pct",
                        multiple = TRUE,
                        options = list(plugins= list('remove_button')))
                        ),
        # Show a plot of the generated distribution
        mainPanel(
          navbarPage(
            "Select Tab For Output",
            tabPanel("Correlation(s) With Eviction", 
                    plotOutput("eviction_scatterplot")),
            tabPanel("Descriptives By Majority Race", 
                    dataTableOutput("race_descriptives")),
            tabPanel("iFrame Test",
                      htmlOutput("frame"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  dataInput <- reactive({
    han |> 
      filter(xsite == input$city &
               characteristic %in% input$characteristic)
  })
  
  citysiteInput <- reactive({
    citysites |> 
      filter(xsite == input$city)
  })
  
  han_filter <- dataInput |>  debounce(500)
  
  site_filter <- citysiteInput |> debounce(500)
  
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
    

  output$frame <- renderUI({ 
    
    city_embed <- site_filter()$website
    
    tags$iframe(#src = 'https://datawrapper.de', 
                src = city_embed,
                width = "100%", height = "600",
                seamless = TRUE)
  }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

