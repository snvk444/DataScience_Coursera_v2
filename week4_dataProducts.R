setwd("C:/Users/Venkata/Desktop/Personal/Coursera/Data_Science_Specialization/9_Data_Products/Week1/prediction_app")

library(shiny)
ui <- fluidPage(
  titlePanel("Predict Wage per Age"),
  sidebarLayout(
    sidebarPanel(
      h3("This is the side-bar"),
      sliderInput(inputId = "sliderMPG", "What is the predicted Wage for the selected age?", 18, 80, value = 40),
      checkboxInput(inputId = "showModel1", "Show/hide model1", value = TRUE),
      submitButton("Submit")
      
    ),
    mainPanel(
      h4("This is a sample presentation of how the algorithm works. Use the sidebar on the
         left to modify the input and there will be a change in the plot on the right. This
         is achieved using reactive function in shiny."),
      plotOutput("plot1"),
      h3("predicted output from model:"),
      textOutput("pred1")
      )
    )
)

server <- function(input, output) {
  model1 <- lm(wage~age,data=Wage)  

  model1pred <- reactive({
    WageInput <- input$sliderMPG
    predict(model1, newdata = data.frame(wage = WageInput))
  })
  
  output$plot1 <- renderPlot({
    wageInput <- input$sliderMPG
    
    plot(Wage$wage, Wage$age, xlab = "Age",
         ylab = "Wage", bty = "n", pch = 16, xlim = c(10,35), ylim = c(50, 350))
    if(input$showModel1)
    {
      abline(model1, col="red",lwd =2)
    }
        
    legend(25, 250, c("Model1 Prediction"), pch = 16,
           col = c("red"), bty = "n", cex = 2)
    points(wageInput, model1pred(), col = "red", pch = 16, cex = 2)
    })
  
  output$pred1 <- renderText({
    model1pred()
  })
}

shinyApp(ui = ui, server = server)