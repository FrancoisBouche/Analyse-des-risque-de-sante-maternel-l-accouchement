#Installer et charger les packages nécessaires :
#install.packages("randomForest")
library(randomForest)

# Lire les données
data <- read.csv("Maternal Health Risk Data Set.csv")

data$RiskLevel <- as.factor(data$RiskLevel)

# Fixer la graine pour assurer la reproductibilité
set.seed(123)

# Définir la taille de l'ensemble d'entraînement (63 %)
train_index2 <- sample(seq_len(nrow(data)), size = 0.63 * nrow(data))

# Diviser les données en ensemble d'entraînement et de test
train_data2 <- data[train_index2, ]
test_data2 <- data[-train_index2, ]

model_rf_final2 <- randomForest(RiskLevel ~ ., data = train_data2, ntree = 156, mtry = 2, importance = TRUE)

# Enregistrer le modèle en fichier .rds
saveRDS(model_rf_final2, "model_rf_Final.rds")