#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


ui <- fluidPage(
  titlePanel("Class Balance of Loans"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset_choice", "Select Dataset:",
                  choices = c("Before SMOTE", "After SMOTE"))
    ),
    mainPanel(
      plotOutput("barplot")
    )
  )
)

server <- function(input, output, session) {
  
  output$barplot <- renderPlot({
    colors <- c("lightblue", "salmon")
    
    if (input$dataset_choice == "Before SMOTE") {
      counts <- table(df_loans$unpaid_loan)
      main_title <- "Class Balance of Response Variable"
    } else {
      counts <- table(balanced_df$data$class)
      main_title <- "Class Balance of Response Variable - After SMOTE"
    }
    
    par(mar = c(5, 4, 4, 8), xpd = TRUE)
    
    bp <- barplot(counts,
                  main = main_title,
                  xlab = "Unpaid Loan Status",
                  ylab = "Count",
                  col = colors,
                  names.arg = c("Paid - 0", "Unpaid - 1"))
    
    text(x = bp, 
         y = counts, 
         label = counts, 
         pos = 3, 
         cex = 0.8, 
         col = "black")
    
    legend("topright",
           inset = c(-0.15, 0),
           legend = c("Paid - 0", "Unpaid - 1"),
           fill = colors,
           title = "Loan Status",
           bty = "n")
  })
  
}

shinyApp(ui, server)

