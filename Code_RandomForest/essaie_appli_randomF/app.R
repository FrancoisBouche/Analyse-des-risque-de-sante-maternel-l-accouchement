library(shiny)
library(randomForest)
library(DT)

# Charger le modèle
model_rf <- readRDS("model_rf_Final.rds")

# Chargement de ma dataset (remplacez "votre_dataset.csv" par le chemin vers votre fichier CSV)
dataset <- read.csv("Maternal Health Risk Data Set.csv")

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Application de Prédiction du Risque Maternel"),
  
  tabsetPanel(
    # Première page : Tableau des Variables
    tabPanel("Tableau des Variables",
             fluidRow(
               column(12,
                      tags$h3("Tableau des Variables"),
                      tags$table(
                        class = "table",
                        tags$thead(
                          tags$tr(
                            tags$th("Nom de la Variable"),
                            tags$th("Rôle"),
                            tags$th("Type"),
                            tags$th("Démographique"),
                            tags$th("Description"),
                            tags$th("Unités"),
                            tags$th("Valeurs Manquantes")
                          )
                        ),
                        tags$tbody(
                          tags$tr(
                            tags$td("Âge"),
                            tags$td("Caractéristique"),
                            tags$td("Entier"),
                            tags$td("Âge"),
                            tags$td("Âge en années lorsqu'une femme est enceinte."),
                            tags$td(""),
                            tags$td("non")
                          ),
                          tags$tr(
                            tags$td("PressionSystolique"),
                            tags$td("Caractéristique"),
                            tags$td("Entier"),
                            tags$td(""),
                            tags$td("Valeur supérieure de la pression artérielle en mmHg, un autre attribut significatif pendant la grossesse."),
                            tags$td("mmHg"),
                            tags$td("non")
                          ),
                          tags$tr(
                            tags$td("PressionDiastolique"),
                            tags$td("Caractéristique"),
                            tags$td("Entier"),
                            tags$td(""),
                            tags$td("Valeur inférieure de la pression artérielle en mmHg, un autre attribut significatif pendant la grossesse."),
                            tags$td("mmHg"),
                            tags$td("non")
                          ),
                          tags$tr(
                            tags$td("Glycémie"),
                            tags$td("Caractéristique"),
                            tags$td("Entier"),
                            tags$td(""),
                            tags$td("Niveaux de glucose sanguin exprimés en concentration molaire."),
                            tags$td("mmol/L"),
                            tags$td("non")
                          ),
                          tags$tr(
                            tags$td("TempératureCorporelle"),
                            tags$td("Caractéristique"),
                            tags$td("Entier"),
                            tags$td(""),
                            tags$td("Température corporelle au repos."),
                            tags$td("°C"),
                            tags$td("non")
                          ),
                          tags$tr(
                            tags$td("FréquenceCardiaque"),
                            tags$td("Caractéristique"),
                            tags$td("Entier"),
                            tags$td(""),
                            tags$td("Fréquence cardiaque au repos."),
                            tags$td("bpm"),
                            tags$td("non")
                          ),
                          tags$tr(
                            tags$td("NiveauDeRisque"),
                            tags$td("Cible"),
                            tags$td("Catégoriel"),
                            tags$td(""),
                            tags$td("Niveau d'intensité du risque prédit pendant la grossesse en fonction des attributs précédents."),
                            tags$td(""),
                            tags$td("non")
                          )
                        )
                      ),
                      
                      # Afficher le dataset en dessous de la table de description
                      tags$h3("Aperçu du Dataset"),
                      DT::dataTableOutput("datasetTable")
               )
             )
    ),
    
    # Deuxième page : Prédiction du Risque
    tabPanel("Prédiction du Niveau de Risque",
             sidebarLayout(
               sidebarPanel(
                 numericInput("Age", "Âge :", value = 30, min = 15, max = 60),
                 numericInput("SystolicBP", "Pression artérielle systolique (mmHg) :", value = 120),
                 numericInput("DiastolicBP", "Pression artérielle diastolique (mmHg) :", value = 80),
                 numericInput("BS", "Sucre dans le sang (mg/dL) :", value = 100),
                 numericInput("BodyTemp", "Température corporelle (°C) :", value = 37),
                 numericInput("HeartRate", "Fréquence cardiaque (bpm) :", value = 70),
                 actionButton("predict", "Prédire le risque")
               ),
               mainPanel(
                 h3("Niveau de risque prédit :"),
                 textOutput("prediction"),
                 uiOutput("recommendations")
               )
             )
    )
  )
)

# Serveur
server <- function(input, output) {
  
  # Afficher le dataset
  output$datasetTable <- DT::renderDataTable({
    datatable(dataset, options = list(pageLength = 5))
  })
  
  observeEvent(input$predict, {
    # Créer une nouvelle observation avec les valeurs de saisie utilisateur
    new_data <- data.frame(
      Age = input$Age,
      SystolicBP = input$SystolicBP,
      DiastolicBP = input$DiastolicBP,
      BS = input$BS,
      BodyTemp = input$BodyTemp,
      HeartRate = input$HeartRate
    )
    
    # Faire la prédiction avec le modèle
    risk_prediction <- predict(model_rf, newdata = new_data)
    
    # Afficher le niveau de risque prédit
    output$prediction <- renderText({
      paste("Le niveau de risque est :", risk_prediction)
    })
    
    # Afficher les recommandations en fonction du risque
    output$recommendations <- renderUI({
      if (risk_prediction == "high risk") {
        tagList(
          tags$h4("Recommandations pour un risque élevé :"),
          tags$ul(
            tags$li("Effectuez une évaluation complète et augmentez la fréquence de surveillance."),
            tags$li("Élaborer un plan de soins personnalisé en collaboration avec une équipe multidisciplinaire."),
            tags$li("Augmentez la fréquence des consultations prénatales et des examens de surveillance."),
            tags$li("Informez clairement la patiente sur les risques potentiels et les signes d'alerte à surveiller."),
            tags$li("Planifiez l'accouchement dans un établissement disposant des ressources nécessaires pour gérer les complications potentielles."),
            tags$li("Assurez une surveillance étroite pendant la période post-partum immédiate."),
            tags$li("Organisez un suivi à long terme pour gérer les éventuelles séquelles ou complications chroniques."),
            tags$p("En suivant ces préconisations, vous contribuerez à optimiser la prise en charge de votre patiente à haut risque et à réduire le risque de mortalité maternelle."),
            tags$p("Sources :"),
            tags$p("1. American College of Obstetricians and Gynecologists: Levels of maternal care: Obstetric care consensus No. 9. Obstet Gynecol 134(2):428-434, 2019."),
            tags$a(href = "https://www.msdmanuals.com/fr/professional/gyn%C3%A9cologie-et-obst%C3%A9trique/complications-pr%C3%A9natales/revue-g%C3%A9n%C3%A9rale-des-grossesses-%C3%A0-haut-risque", "2. MSD Manuals")
          )
        )
      } else if (risk_prediction == "mid risk") {
        tagList(
          tags$h4("Recommandations pour un risque moyen :"),
          tags$ul(
            tags$li("Élaborez un plan de soins adapté en fonction des risques identifiés."),
            tags$li("Augmentez la fréquence des consultations prénatales et des examens de surveillance selon les besoins."),
            tags$li("Envisagez des tests spécialisés supplémentaires tels que des échographies plus fréquentes."),
            tags$li("Optimisez la gestion des pathologies préexistantes connues pour avoir un impact sur la grossesse."),
            tags$li("Réévaluez les médicaments et suppléments pour s'assurer de leur innocuité."),
            tags$li("Conseillez un régime alimentaire équilibré et encouragez l'arrêt des substances nocives."),
            tags$li("Préparez la patiente aux situations d'urgence potentielles en lui expliquant les signes à surveiller."),
            tags$p("En suivant ces préconisations, vous contribuerez à optimiser la prise en charge de votre patiente à risque moyen et à réduire le risque de complications maternelles."),
            tags$p("Sources :"),
            tags$a(href = "https://pulsations.hug.ch/article-pro/les-grossesses-haut-risque-sont-toujours-plus-nombreuses", "1. HUG Pulsations")
          )
        )
      }
    })
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
