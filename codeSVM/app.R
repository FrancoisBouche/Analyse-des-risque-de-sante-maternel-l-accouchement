library(shiny)
library(kernlab)  # Pour utiliser ksvm

# Interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Prédiction du niveau de risque"),
  
  sidebarLayout(
    sidebarPanel(
      # Champs de saisie pour les variables explicatives
      numericInput("Age", "Âge :", value = 25, min = 0),
      numericInput("SystolicBP", "Pression artérielle systolique :", value = 120, min = 0),
      numericInput("DiastolicBP", "Pression artérielle diastolique :", value = 80, min = 0),
      numericInput("BS", "Taux de sucre dans le sang (BS) :", value = 100, min = 0),
      numericInput("BodyTemp", "Température corporelle (°C) :", value = 36.5, min = 0),
      numericInput("HeartRate", "Fréquence cardiaque (bpm) :", value = 75, min = 0),
      
      actionButton("predict", "Faire la prédiction")
    ),
    
    mainPanel(
      # Afficher les résultats de la prédiction
      textOutput("prediction"),
      textOutput("probabilities")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Charger le modèle SVM enregistré
  svm_model_low <- readRDS("svm_model_low.rds")
  
  # Fonction de mise à l'échelle (par exemple, centrage-réduction)
  scale_new_data <- function(data) {
    # Si vous avez utilisé une mise à l'échelle différente, ajustez cette fonction
    scaled_data <- scale(data, center = TRUE, scale = TRUE)  # Ajuster selon votre méthode
    return(scaled_data)
  }
  
  # Prédiction lorsque l'utilisateur clique sur "predict"
  observeEvent(input$predict, {
    # Créer un DataFrame avec les entrées de l'utilisateur
    new_data <- data.frame(
      Age = input$Age,
      SystolicBP = input$SystolicBP,
      DiastolicBP = input$DiastolicBP,
      BS = input$BS,
      BodyTemp = input$BodyTemp,
      HeartRate = input$HeartRate
    )
    
    # Mettre à l'échelle les données
    new_data_scaled <- scale_new_data(new_data)
    
    # Faire la prédiction
    pred <- predict(svm_model_low, newdata = new_data_scaled, type = "probabilities")
    
    # Récupérer les probabilités
    prob <- pred[, "low risk"]
    
    # Afficher les résultats
    output$prediction <- renderText({
      paste("La classe prédite est :", as.character(pred))
    })
    
    output$probabilities <- renderText({
      paste("Probabilités : Low Risk =" (prob[1], 2), 
            ", Mid Risk =" (prob[2], 2),
            ", High Risk =" (prob[3], 2))
    })
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
