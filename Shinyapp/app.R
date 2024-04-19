# app.R

# Load required libraries
library(shiny)

# Source the R code file
source("functions.R", local = TRUE)

nhanes_comb_feat <- read.csv('nhanes_comb_feat.csv')

nhane_xgb <- read.csv("nhane_xgb.csv") # this data is used to train cox reg model (nhanesdesignmodel), but it's still needed for survfit function

# re-train functions to avoid survfit error
nhanesdesignmodel <- coxph(Surv(ftime+0.1, mortstat) ~ c_Age+c_Gender+c_SMQ + Prediction_center,
                           nhane_xgb, weights = wt,ties="breslow")


# Define UI
ui <- fluidPage(
  titlePanel("Cox Proportional Hazards Model Predictor"),
  sidebarLayout(
    sidebarPanel(

      sliderInput("input_age", "Input Age:",
                  min = min(25), max = max(85), value = 60),
      selectInput("input_gender", "Select Gender:", choices = c("Male", "Female")),
      
      sliderInput("input_bmi", "Input BMI:",
                  min = min(10), max = max(50), value = 20),
      
      selectInput("input_BP", "High Blood Pressure:", choices = c("Yes", "No")),
      selectInput("input_smoke", "Smoke Status:", choices = c("Smoke", "No Smoke", "Former Smoker")),
      selectInput("input_work", "Work Type:", choices = c("Work", "No Work", "Look Work"))

    ),
    mainPanel(
      plotOutput("survival_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Plot survival curve
  output$survival_plot <- renderPlot({
    
    new_data <- nhanes_comb_feat[100, ]
    
    new_data$c_Age <- input$input_age

    new_data$c_Gender <- ifelse(input$input_gender == "Male", 1, 0)

    new_data$c_BMI <- input$input_bmi
  
    new_data$c_HighBP <- ifelse(input$input_BP == "Yes", 1, 0)
    
    new_data$c_SMQ <- ifelse(input$input_smoke == "No Smoke", 0, ifelse(input$input_smoke == "Former Smoker", 1, 2))
    
    new_data$c_WorkType <- ifelse(input$input_work=='Work', 0, ifelse(input$input_work=='LookWork', 1, 2))
    
    newdataxgb <- xgboost_test(new_data)
    
      fit <- survfit(nhanesdesignmodel, newdata = newdataxgb)
      ylower =  ifelse( min(fit$surv)>=0.9, 0.9, ifelse( min(fit$surv)>=0.8, 0.8,  ifelse( min(fit$surv)>=0.6, 0.5, 0  )    )   )
      ggsurvplot(fit, 
                data = newdataxgb, 
                 palette = "Dark2", 
                 conf.int = TRUE,
                ylim = c(ylower, 1), 
                size = 0.2,
                  xlab='Time in year', ylab='Survival probabilty',
                ) +
      ggtitle("Survival Curve with Confidence Interval") 
  })
}

# Run the application
shinyApp(ui = ui, server = server)