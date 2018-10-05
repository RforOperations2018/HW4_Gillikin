#Homework 4

library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

departments <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "department_name")$department_name)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Pittsburgh Revenues and Expenses"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates",
                     "Dates",
                     start = Sys.Date()-30,
                     end = Sys.Date()),
      selectInput("department_select",
                  "Department",
                  choices = departments,
                  selected = "Potholes")
    ),
    
    # Tabset Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Line Plot",
                 plotlyOutput("linePlot")
        ),
        tabPanel("Open/Closed",
                 plotlyOutput("barChart"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  loadAccount <- reactive({
    # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20%22general_ledger_date%22%20%3E=%20%27", input$dates[1], "%27%20AND%20%22general_ledger_date%22%20%3C=%20%27", input$dates[2], "%27%20AND%20%22department_name%22%20=%20%27", input$department_select, "%27")
    
    # Load and clean data
    dataAccount <- ckanSQL(url) %>%
      mutate(date = as.Date(general_ledger_date),
             STATUS = ifelse(STATUS == 1, "Closed", "Open"))
    
    return(dataAccount)
  })
  output$linePlot <- renderPlotly({
    dataAccount <- loadAcount()
    
    # shape the data for chart
    table <- dataAccount %>%
      group_by(date) %>%
      summarise(count = n())
    
    # draw plot
    ggplot(table, aes(x = date, y = count)) +
      geom_point(colour = "#d95f02") +
      geom_line(colour = "#d95f02") +
      geom_smooth()
  })
  output$barChart <- renderPlotly({
    dataAccount <- loadAccount()
    
    # shape the data for chart
    table <- dataAccount %>%
      group_by(STATUS) %>%
      summarise(count = n())
    
    # draw plot
    ggplot(table, aes(x = STATUS, y = count, fill = STATUS)) +
      geom_bar(stat = "identity")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

