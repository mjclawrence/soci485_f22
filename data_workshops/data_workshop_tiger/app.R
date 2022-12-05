# Load packages

library(tidyverse)
library(weights)
library(DT)
library(pollster)


library(shiny)


library(shinythemes)

# Load the data

## For the final app we will want local data files
## For now we can use the web versions

tiger <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/tiger_w74_92.csv")


tiger <- tiger |> 
  relocate(c("cancul1", "crimsent", "weight_w92"), .after = "qkey") |> 
  pivot_longer(names_to = "characteristic", values_to = "value", 5:9)|> 
  mutate(
    value = factor(value, 
                                  levels= c("Less Than High School", "High School Graduate","Some College",
                                            "Associate's Degree","College graduate","Postgraduate",
                                            "Woman", "Man","Refused to Answer",
                                            "Very Conservative", "Conservative", "Moderate", "Liberal", "Very Liberal",
                                            "Pacific","Mountain", "West South Central", "East South Central", "South Atlantic", "West North Central", "East North Central", "Middle Atlantic", "New England",
                                            "Asian", "Black", "Hispanic", "White", "Other","Refused")), 
         ) |>
  filter(!is.na(cancul1)) |> 
  filter(!is.na(crimsent)) |> 
  filter(!is.na(value)) |> 
  mutate(crimsent = factor(crimsent,
                           levels = c("Too little time",
                                      "About the right amount",
                                      "Too much time")))


# First part of the app is setting up the user interface
ui <- fluidPage(
  
  ## The theme selector lets you experiment with different themes
  shinythemes::themeSelector(),
  
  ### Once you decide on a theme, hashtag the previous line and edit the next line to use your theme
  #shinytheme(theme = "flatly"),
  
  ## Application title
  titlePanel("Formal and Informal Punishment"),
  
  ## Sidebar has one input right now.
  ### This is where users choose a characteristic. We'll use that selection as a variable called "characteristic"
  
  sidebarPanel(
    selectizeInput(inputId = "characteristic", #name of input
                   label = "Select characteristic(s):", #label displayed in ui
                   choices = as.character(unique(tiger$characteristic)),
                   selected = "education")
  ), # close the sidebar panel
  
  ## Main panel has two tabs right now.
  ### We express the output here by name, but we create it in the server section of this file        
  mainPanel(
    tabsetPanel(
      tabPanel("Descriptive Plot", 
               plotOutput("descriptive_plot")),
      tabPanel("Hypothesis Test", 
               helpText("Highlighted groups indicate a significant association between beliefs about informal and formal punishment"),
               dataTableOutput("hypothesis_table")),
      tabPanel("Descriptive Tables",
               helpText("The table below shows the distribution of responses to the following question about cancel culture: In general, when people publicly call out others on social media for posting content that might be considered offensive, are they more likely to…"),
               tableOutput("cancul_descriptives"),
               helpText("The table below shows the distribution of responses to the following question about criminal sentencing: Would you say people in this country spend X amount in prison?"),
               tableOutput("crimsent_descriptives")),
    ) # close the tabset panel
  ) # close the main panel
) # close the ui code

# Define server logic
server <- function(input, output) {
  
  ## We have one dataset that will react to values from the sidebar selections.
  
  ### Here we filter the dataset for the inputted characteristic
  dataInput <- reactive({
    tiger |> 
      filter(characteristic %in% input$characteristic)
  })
  
  ## Here I am renaming the dataset after filtering
  ## The debounce function adds a small amount of time (500ms) between filtering the datasets and creating the output.
  ## Adding that time tends to make the output more responsive to the filters 
  
  
  tiger_filter <- dataInput |>  debounce(500)
  
  
  ## This section creates the plot.
  ### The name we give to the object - descriptive_plot - is the name we will use in the output for the tab in the ui
  
  output$descriptive_plot <- renderPlot({
    plot1 <- tiger_filter() |> 
      crosstab_3way(cancul1, crimsent, value, weight_w92,
                    format = "long") |> 
      ggplot(aes(x = cancul1, y = pct,
                 fill = crimsent)) +
      geom_col() + facet_wrap(~value) +
      theme(legend.position = "bottom") +
      geom_text(aes(label = round(pct, 0)), position = position_stack(vjust = 0.5))
    plot1
  })
  
   
  
  ## This section creates the table with the chi square test results 
  
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
  
  
  
  ## This section creates the descriptive table
  
  
  output$cancul_descriptives <- renderTable({
    tiger_filter() |> 
      crosstab(value, cancul1, weight_w92, n = FALSE)
  })
  
  
  output$crimsent_descriptives <- renderTable({
    tiger_filter() |> 
      crosstab(value, crimsent, weight_w92, n = FALSE)
  })
  
} # This curly bracket closes the server code

# After the ui code and server code are set, run the application 

shinyApp(ui = ui, server = server)

