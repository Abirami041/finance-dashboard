library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Smart Personal Finance Manager"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Income"),
      numericInput("salary", "Monthly Salary", value = 0),
      numericInput("extra_income", "Additional Income", value = 0),
      
      h3("Savings Entry"),
      numericInput("manual_savings", "Add Saved Amount", value = 0),
      
      h3("Manage Commitments"),
      textInput("commit_name", "Commitment Name"),
      numericInput("commit_amount", "Amount", value = 0),
      selectInput("commit_type", "Type", choices = c("Expense", "Savings")),
      actionButton("add_commit", "Add Commitment"),
      
      br(), br(),
      
      selectInput("edit_commit", "Select Commitment", choices = NULL),
      numericInput("edit_amount", "Update Amount", value = 0),
      actionButton("update_commit", "Update Commitment"),
      
      br(),
      actionButton("delete_commit", "Delete Commitment"),
      
      br(), br(),
      
      selectInput("pay_commit", "Mark as Paid", choices = NULL),
      actionButton("pay_btn", "Pay Commitment"),
      
      hr(),
      
      h3("Add Expense"),
      selectInput("category", "Category",
                  choices = c("1. Food" = "Food",
                              "2. Transport" = "Transport",
                              "3. Shopping" = "Shopping",
                              "4. Bills" = "Bills",
                              "5. Other" = "Other")),
      conditionalPanel(
        condition = "input.category == 'Other'",
        textInput("other_category", "Specify Category")
      ),
      numericInput("amount", "Amount", value = 0),
      textInput("desc", "Description"),
      actionButton("add", "Add Expense")
    ),
    
    mainPanel(
      h3("Dashboard"),
      verbatimTextOutput("summary"),
      plotOutput("expensePlot"),
      plotOutput("pieChart"),
      
      h3("Commitments Status"),
      tableOutput("commitTable"),
      
      h3("Expenses"),
      tableOutput("expenseTable"),
      
      h3("Alerts & Insights"),
      verbatimTextOutput("alerts"),
      verbatimTextOutput("insights")
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # EXPENSES
  expenses <- reactiveVal(data.frame(
    Category = character(),
    Amount = numeric(),
    Description = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$add, {
    cat <- if(input$category == "Other") input$other_category else input$category
    new_data <- data.frame(Category = cat, Amount = input$amount, Description = input$desc)
    expenses(rbind(expenses(), new_data))
  })
  
  # COMMITMENTS
  commitments <- reactiveVal(data.frame(
    Name = character(),
    Amount = numeric(),
    Type = character(),
    Paid = logical(),
    stringsAsFactors = FALSE
  ))
  
  # ADD
  observeEvent(input$add_commit, {
    new_commit <- data.frame(
      Name = input$commit_name,
      Amount = input$commit_amount,
      Type = input$commit_type,
      Paid = FALSE
    )
    commitments(rbind(commitments(), new_commit))
  })
  
  # UPDATE SELECT INPUTS
  observe({
    df <- commitments()
    updateSelectInput(session, "edit_commit", choices = df$Name)
    
    unpaid <- df %>% filter(Paid == FALSE)
    updateSelectInput(session, "pay_commit", choices = unpaid$Name)
  })
  
  # UPDATE AMOUNT
  observeEvent(input$update_commit, {
    df <- commitments()
    idx <- which(df$Name == input$edit_commit)
    if(length(idx) > 0) {
      df$Amount[idx] <- input$edit_amount
      commitments(df)
    }
  })
  
  # DELETE
  observeEvent(input$delete_commit, {
    df <- commitments()
    df <- df[df$Name != input$edit_commit, ]
    commitments(df)
  })
  
  # PAY
  observeEvent(input$pay_btn, {
    df <- commitments()
    idx <- which(df$Name == input$pay_commit & df$Paid == FALSE)
    
    if(length(idx) > 0) {
      df$Paid[idx] <- TRUE
      
      if(df$Type[idx] == "Expense") {
        new_exp <- data.frame(
          Category = df$Name[idx],
          Amount = df$Amount[idx],
          Description = "Commitment Payment"
        )
        expenses(rbind(expenses(), new_exp))
      }
      
      commitments(df)
    }
  })
  
  # CALCULATIONS
  total_income <- reactive({ input$salary + input$extra_income })
  total_expense <- reactive({ sum(expenses()$Amount) })
  
  total_savings <- reactive({
    df <- commitments()
    commit_savings <- sum(df$Amount[df$Type == "Savings" & df$Paid == TRUE])
    commit_savings + input$manual_savings
  })
  
  remaining_balance <- reactive({ total_income() - total_expense() - total_savings() })
  
  # SUMMARY
  output$summary <- renderPrint({
    cat("Total Income:", total_income(), "\n")
    cat("Expenses:", total_expense(), "\n")
    cat("Savings:", total_savings(), "\n")
    cat("Remaining Balance:", remaining_balance(), "\n")
    
    if(total_income() > 0) {
      cat("Savings %:", round((total_savings()/total_income())*100,2), "%\n")
    }
  })
  
  # VISUALS
  output$expensePlot <- renderPlot({
    df <- expenses()
    if(nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = Category, y = Amount, fill = Category)) +
      geom_bar(stat = "identity") + theme_minimal()
  })
  
  output$pieChart <- renderPlot({
    df <- expenses()
    if(nrow(df) == 0) return(NULL)
    
    df_sum <- df %>% group_by(Category) %>% summarise(Total = sum(Amount))
    
    ggplot(df_sum, aes(x = "", y = Total, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") + theme_void()
  })
  
  output$expenseTable <- renderTable({ expenses() })
  output$commitTable <- renderTable({ commitments() })
  
  # ALERTS
  output$alerts <- renderPrint({
    if(total_income() == 0) cat("⚠ Add your salary\n")
    
    if(total_expense() > total_income()) cat("⚠ Expenses exceed income!\n")
    
    if(remaining_balance() < 0) cat("⚠ Negative balance!\n")
    
    if(total_income() > 0 && total_savings()/total_income() < 0.2) {
      cat("⚠ Savings less than 20%\n")
    }
  })
  
  # INSIGHTS
  output$insights <- renderPrint({
    df <- expenses()
    
    if(nrow(df) == 0) {
      cat("No data for insights\n")
      return()
    }
    
    df_sum <- df %>% group_by(Category) %>% summarise(Total = sum(Amount))
    top_cat <- df_sum[which.max(df_sum$Total), ]
    
    cat(paste("Highest spending category:", top_cat$Category, "\n"))
    
    if(total_income() > 0) {
      save_per <- round((total_savings()/total_income())*100,2)
      cat(paste("Savings percentage:", save_per, "%\n"))
    }
  })
}


library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Smart Personal Finance Manager"),

  sidebarLayout(
    sidebarPanel(
      h3("Income"),
      numericInput("salary", "Monthly Salary", value = 0),
      numericInput("extra_income", "Additional Income", value = 0),

      h3("Savings Entry"),
      numericInput("manual_savings", "Add Saved Amount", value = 0),

      h3("Manage Commitments"),
      textInput("commit_name", "Commitment Name"),
      numericInput("commit_amount", "Amount", value = 0),
      selectInput("commit_type", "Type", choices = c("Expense", "Savings")),
      actionButton("add_commit", "Add Commitment"),

      br(), br(),

      selectInput("edit_commit", "Select Commitment", choices = NULL),
      numericInput("edit_amount", "Update Amount", value = 0),
      actionButton("update_commit", "Update Commitment"),

      br(),
      actionButton("delete_commit", "Delete Commitment"),

      br(), br(),

      selectInput("pay_commit", "Mark as Paid", choices = NULL),
      actionButton("pay_btn", "Pay Commitment"),

      hr(),

      h3("Add Expense"),
      selectInput("category", "Category",
                  choices = c("1. Food" = "Food",
                              "2. Transport" = "Transport",
                              "3. Shopping" = "Shopping",
                              "4. Bills" = "Bills",
                              "5. Other" = "Other")),
      conditionalPanel(
        condition = "input.category == 'Other'",
        textInput("other_category", "Specify Category")
      ),
      numericInput("amount", "Amount", value = 0),
      textInput("desc", "Description"),
      actionButton("add", "Add Expense")
    ),

    mainPanel(
      h3("Dashboard"),
      verbatimTextOutput("summary"),
      plotOutput("expensePlot"),

      h3("Commitments Status"),
      tableOutput("commitTable"),

      h3("Expenses"),
      tableOutput("expenseTable"),

      h3("Alerts & Insights"),
      verbatimTextOutput("alerts"),
      verbatimTextOutput("insights")
    )
  )
)

server <- function(input, output, session) {

  expenses <- reactiveVal(data.frame(
    Category = character(),
    Amount = numeric(),
    Description = character(),
    stringsAsFactors = FALSE
  ))

  observeEvent(input$add, {
    cat <- if(input$category == "Other") input$other_category else input$category
    new_data <- data.frame(Category = cat, Amount = input$amount, Description = input$desc)
    expenses(rbind(expenses(), new_data))
  })

  commitments <- reactiveVal(data.frame(
    Name = character(),
    Amount = numeric(),
    Type = character(),
    Paid = logical(),
    stringsAsFactors = FALSE
  ))

  observeEvent(input$add_commit, {
    new_commit <- data.frame(
      Name = input$commit_name,
      Amount = input$commit_amount,
      Type = input$commit_type,
      Paid = FALSE
    )
    commitments(rbind(commitments(), new_commit))
  })

  observe({
    df <- commitments()
    updateSelectInput(session, "edit_commit", choices = df$Name)

    unpaid <- df %>% filter(Paid == FALSE)
    updateSelectInput(session, "pay_commit", choices = unpaid$Name)
  })

  observeEvent(input$update_commit, {
    df <- commitments()
    idx <- which(df$Name == input$edit_commit)
    if(length(idx) > 0) {
      df$Amount[idx] <- input$edit_amount
      commitments(df)
    }
  })

  observeEvent(input$delete_commit, {
    df <- commitments()
    df <- df[df$Name != input$edit_commit, ]
    commitments(df)
  })

  observeEvent(input$pay_btn, {
    df <- commitments()
    idx <- which(df$Name == input$pay_commit & df$Paid == FALSE)

    if(length(idx) > 0) {
      df$Paid[idx] <- TRUE

      if(df$Type[idx] == "Expense") {
        new_exp <- data.frame(
          Category = df$Name[idx],
          Amount = df$Amount[idx],
          Description = "Commitment Payment"
        )
        expenses(rbind(expenses(), new_exp))
      }

      commitments(df)
    }
  })

  total_income <- reactive({ input$salary + input$extra_income })
  total_expense <- reactive({ sum(expenses()$Amount) })

  total_savings <- reactive({
    df <- commitments()
    commit_savings <- sum(df$Amount[df$Type == "Savings" & df$Paid == TRUE])
    commit_savings + input$manual_savings
  })

  remaining_balance <- reactive({ total_income() - total_expense() - total_savings() })

  output$summary <- renderPrint({
    cat("Total Income:", total_income(), "\n")
    cat("Expenses:", total_expense(), "\n")
    cat("Savings:", total_savings(), "\n")
    cat("Remaining Balance:", remaining_balance(), "\n")

    if(total_income() > 0) {
      cat("Savings %:", round((total_savings()/total_income())*100,2), "%\n")
    }
  })

  output$expensePlot <- renderPlot({
    df <- expenses()
    if(nrow(df) == 0) return(NULL)

    ggplot(df, aes(x = Category, y = Amount, fill = Category)) +
      geom_bar(stat = "identity") + theme_minimal()
  })

  output$expenseTable <- renderTable({ expenses() })
  output$commitTable <- renderTable({ commitments() })

  # ALERTS (ONLY BASED ON EXPENSES)
  output$alerts <- renderPrint({
    if(total_income() == 0) cat("⚠ Add your salary\n")

    if(total_expense() > total_income()) cat("⚠ Expenses exceed income!\n")

    if(remaining_balance() < 0) cat("⚠ Negative balance!\n")

    df <- expenses()
    if(nrow(df) > 0) {
      df_sum <- df %>% group_by(Category) %>% summarise(Total = sum(Amount))
      high <- df_sum %>% filter(Total == max(Total))

      if(nrow(high) > 0 && total_income() > 0 && (high$Total / total_income()) > 0.4) {
        cat(paste("⚠", high$Category, "expenses are too high\n"))
      }
    }
  })

  # INSIGHTS (ONLY EXPENSE BASED)
  output$insights <- renderPrint({
    df <- expenses()

    if(nrow(df) == 0) {
      cat("No data for insights\n")
      return()
    }

    df_sum <- df %>% group_by(Category) %>% summarise(Total = sum(Amount))
    top_cat <- df_sum[which.max(df_sum$Total), ]

    cat(paste("Highest spending category:", top_cat$Category, "\n"))

    if(total_income() > 0) {
      save_per <- round((total_savings()/total_income())*100,2)
      cat(paste("Savings percentage:", save_per, "%\n"))
    }
  })
}

shinyApp(ui = ui, server = server)

# RUN
shinyApp(ui = ui, server = server)
