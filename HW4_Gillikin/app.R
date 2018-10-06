#Homework 4

library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)

options(scipen = 999)

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
  titlePanel("Pittsburgh City Department Expenses"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      #date input
      dateRangeInput("dates",
                     "Dates",
                     start = Sys.Date()-60,
                     end = Sys.Date()),
      #deparment input
      #only some workl many throw errors for unknown reason
      selectInput("department_select",
                  "Department",
                  choices = departments,
                  selected = "DPW-Operations"),
      #Turning on multiple = TRUE threw an error, so I wanted to make it compare to one other department
      #I wasn't able to figure out how to have this second option
      selectInput("department_select2",
                  "Comparison Department",
                  choices = departments,
                  selected = "DPS-Police"), #doesn't populate
      #action button to reset Department
      actionButton("reset", "Reset Selection", icon = icon("refresh"))
    ),
    
    # Tabset Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Instances",
                 plotlyOutput("linePlot")),
        tabPanel("Type & Fund",
                 plotlyOutput("barChart"),
                 plotlyOutput("barChart2")),
        tabPanel("Amount",
                 plotlyOutput("hexPlot")),
        tabPanel("Table",
                 inputPanel(
                   downloadButton("downloadData","Download Revenue/Expense Data")
                 ),
                 fluidPage(DT::dataTableOutput("table"))
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  loadAccount <- reactive({
    # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20%22general_ledger_date%22%20%3E=%20%27", input$dates[1], "%27%20AND%20%22general_ledger_date%22%20%3C=%20%27", input$dates[2], "%27%20AND%20%22department_name%22%20=%20%27", input$department_select, "%27") #%20AND20%22amount%22%20%3E=%20%27", input$amount_select[1], "%27%20AND%20%22amount%22%20%3C=%20%27", input$amount_select[2], "%27")
    
    # Load and clean data
    dataAccount <- ckanSQL(url) %>%
      mutate(date = as.Date(general_ledger_date),
             object_account_number = ifelse(object_account_number == 54201, "Maintenance", (ifelse(object_account_number == 56151, "Operational Supplies", (ifelse(object_account_number == 56101, "Office Supplies", (ifelse(object_account_number == 54601, "Electric", (ifelse(object_account_number == 56351, "Tools", "Other"))))))))),
             fund_number = ifelse(fund_number == 11101, "General Fund", "Other"),
             amount = amount)

    return(dataAccount)
  })
  output$linePlot <- renderPlotly({
    dataAccount <- loadAccount()
    
    # shape the data for chart
    dat <- dataAccount %>%
      group_by(date) %>%
      summarise(count = n())
    
    # draw plot
    ggplot(dat, aes(x = date, y = count)) +
      geom_point(colour = "#ca0020") +
      geom_line(colour = "#0571b0") 
  })
  output$barChart <- renderPlotly({
    dataAccount <- loadAccount()
    
    # shape the data for chart
    dat <- dataAccount %>%
      group_by(object_account_number) %>%
      summarise(count = n())
    
    # draw plot
    ggplot(dat, aes(x = object_account_number , y = count, fill = object_account_number)) +
      geom_bar(stat = "identity")
  })
  output$barChart2 <- renderPlotly({
    dataAccount <- loadAccount()
    
    # shape the data for chart
    dat <- dataAccount %>%
      group_by(fund_number) %>%
      summarise(count = n())
    
    # draw plot
    ggplot(dat, aes(x = fund_number , y = count, fill = fund_number)) +
      geom_bar(stat = "identity")
  })
  output$hexPlot <- renderPlotly({
    dataAccount <- loadAccount()
    # draw plot
    ggplot(dataAccount, aes(x = date, y = abs(amount))) +
      geom_point(colour = "#ca0020")
  })
  # Datatable
  output$table <- DT::renderDataTable({
    subset(loadAccount(), select = c(department_name, general_ledger_date, object_account_description, amount))
  })
  # Reset Selection of Data
  observeEvent(input$reset, {
    updateSelectInput(session, "department_select", selected = c("DPW-Operations"))
    showNotification("Loading...", type = "message")
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("revenue.expenses-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(loadAccount(), file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

