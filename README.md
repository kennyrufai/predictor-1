# predictor-1
pred
---
title: "Untitled"
author: "Atieno"
date: "2023-07-25"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(shiny)
library(rpart)
```
```{r}
data <- diabetes_prediction_dataset
```

```{r}
ui <- fluidPage(
  titlePanel("Diabetes Diagnosis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("age", "Select age:",
                  choices = unique(data$age)),
      selectInput("bmi", "Select bmi:",
                  choices = unique(data$bmi)),
      selectInput("hypertension", "Select hypertension:",
                  choices = unique(data$hypertension)),
      selectInput("heart_disease", "Select heart_disease:",
                  choices = unique(data$heart_disease)),
      selectInput("smoking_history", "Select smoking_history:",
                  choices = unique(data$smoking_history)),
      selectInput("gender", "Select gender:",
                  choices = unique(data$gender)),
      numericInput("glucose", "Enter Blood Glucose Level:",
                  value = 100, min = 50, max = 200),
       numericInput("Hba1c", "Enter Hba1c Level:",
                  value = 5.7, min = 5.0, max = 9.0),
       actionButton("show_prediction", "predict")
    ),
    mainPanel(
      h2("Diabetes Diagnosis Prediction"),
      verbatimTextOutput("Diagnosis")
    )
  )
)
```
```{r}
server <- function(input, output){
  
model <- reactive({
  
  rpart(data$data ~ data$age + data$bmi + data$hypertension + data$heart_disease + data$blood_glucose_level + data$HbA1c_level + data$smoking_history + data$gender)
  result <- eventReactive(input$show_prediction,{
    new_info <- info.frame(
      age = input$age,
      bmi = input$bmi,
      glucose = input$blood_glucose_level,
      hypertension = input$hypertension,
      heart_disease = input$heart_disease,
      smoking_history = input$smoking_history,
      gender = input$gender,
      Hba1c = input$Hba1c
    )
    predict(model(), new_info, type = "class")
  }) 
  
  output$Diagnosis <- renderText({
    prediction <- result()
    if(prediction == 0) {
      "Not Diabetic"
    } else {
      "Diabetic"
    }
    
  })
})
}
shinyApp(ui = ui, server = server)
```


