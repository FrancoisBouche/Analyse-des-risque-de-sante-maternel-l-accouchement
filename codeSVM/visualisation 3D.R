# Installer les bibliothèques nécessaires
install.packages("plotly")
library(plotly)
library(dplyr)

# Charger le jeu de données
data <- read.csv("Maternal Health Risk Data Set.csv")

# Visualisation 3D : deux variables explicatives avec la variable cible comme couleur
fig <- plot_ly(data, 
               x = ~Age,         # Remplacer 'Age' par la première variable souhaitée
               y = ~SystolicBP,  # Remplacer 'SystolicBP' par la seconde variable souhaitée
               z = ~DiastolicBP, # Remplacer 'DiastolicBP' par une autre variable d'intérêt si souhaité
               color = ~RiskLevel, 
               colors = c("red", "orange", "green")) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'Age'),       # Mettre à jour le titre de l'axe x
    yaxis = list(title = 'SystolicBP'), # Mettre à jour le titre de l'axe y
    zaxis = list(title = 'DiastolicBP') # Mettre à jour le titre de l'axe z
  ))

fig
data$BS

# names(data)
# [1] "Age"         "SystolicBP"  "DiastolicBP" "BS"          "BodyTemp"   
# [6] "HeartRate"   "RiskLevel"  

# changeons de modalités pour observer à nouveau

# Installer les bibliothèques nécessaires

library(plotly)

# Charger le jeu de données
data <- read.csv("Maternal Health Risk Data Set.csv")

# Visualisation 3D : Age, BS et DiastolicBP avec la variable cible RiskLevel
fig <- plot_ly(data, 
               x = ~Age, 
               y = ~BS, 
               z = ~DiastolicBP, 
               color = ~RiskLevel, 
               colors = c("red", "orange", "green")) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'Age'),
    yaxis = list(title = 'BS'),
    zaxis = list(title = 'DiastolicBP')
  ))

fig
