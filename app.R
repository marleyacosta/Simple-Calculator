library(shiny)
library(plotly)
library(shinythemes)

" Define UI for an application that calculates the interest 
 and loan payment and displays the result in an interactive pie chart."
ui <- shinyUI(fluidPage(theme = shinytheme('readable'),
                        
                        
                        # Application title
                        titlePanel("Loan Payment Calculator", windowTitle = "Loan Payment Calculator" ),
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            numericInput("loan_amount" , label = h4("Loan Amount ($)"), 10000, min = 1, step = 500),
                            sliderInput("number_of_years", label = h4("Number of Year(s)"),
                                        min = 1, max = 20, value = 10, step = .1),
                            numericInput("interest_rate", label = h4("Interest Rate (APR)"), min = 1, max = 30, value = 6.5, step = .5)
                          ),
                          # Show text and a pie chart.
                          mainPanel( 
                            textOutput("fixPay"),
                            plotlyOutput("pieChart")
                          )
                        )
))


# Define server logic required to draw a piechart
server <- shinyServer(function(input, output) {
  
  lb <- c("Principal", "Interest") # The pie labels.
  months <- reactive({input$number_of_years * 12}) # Convert years to months.
  monthPay <- reactive({ round(monthlyPayment(input$loan_amount, months(), input$interest_rate), digits = 2)}) # Calculate the monthly pay.
  interest <- reactive({round(interestPaymemt(input$loan_amount, months(), monthPay()), digits = 2)}) # Calculate how much was paid for interest.
  
  
  output$fixPay <- renderText({
    paste("You will need to pay $", monthPay(), " every month for ", 
          input$number_of_years, " years to payoff the debt with a total interest of $", interest() )})
  
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  
  # Display simple Pie Chart
  output$pieChart <- renderPlotly({
    val <- c(input$loan_amount, interest())
    plot_ly(type="pie",values = val, labels = lb,textinfo="label+value+percent") %>%
      layout(xaxis = ax, yaxis = ax)
  })
  
})

# This function calculates the monthly payment.
monthlyPayment <- function(loanAmount, months, interestRate){
  interest <- (interestRate / 100) / 12 # Convert interest percentage to a decimal.
  monthlyPay <- (interest * loanAmount) / (1 - ((1 + interest)^-months)) # Calculate monthly pay.
  return(monthlyPay)                                  
}

# This function calculates the interest payment.
interestPaymemt <- function(loanAmount, months, monthlyPayment){
  interestPay <- (monthlyPayment * months ) - loanAmount # Calculate interest payment.
  return(interestPay)
}


# Run the application 
shinyApp(ui = ui, server = server)
