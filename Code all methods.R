#install.packages("randomForest")
library(randomForest)
library(pROC) # Courbes de ROC
library(ggplot2)
library(caret)
# Lire les données
data <- read.csv("Maternal Health Risk Data Set.csv", stringsAsFactors = T)
data$RiskLevel <- factor(data$RiskLevel, 
                                levels = c("high risk", "mid risk", "low risk"), 
                                labels = c("high", "mid", "low"))
str(data)

# Fixer la graine pour assurer la reproductibilité
set.seed(123)

# Définir la taille de l'ensemble d'entraînement (63 %)
train_index <- sample(seq_len(nrow(data)), size = 0.63 * nrow(data))

# Diviser les données en ensemble d'entraînement et de test
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

str(train_data)
str(test_data)


### Modèle final RandomForest
model_rf_final <- randomForest(RiskLevel ~ ., data = train_data, ntree = 156, mtry = 2, importance = TRUE)
print(model_rf_final)
plot(model_rf_final)
plot(model_rf_final$err.rate, pch=16)


### Test Model RF
predictions <- predict(model_rf_final, newdata = test_data)

# Calcul de la matrice de confusion
conf_matrix <- confusionMatrix(predictions, test_data$RiskLevel)

# Afficher la matrice de confusion et les métriques de performance
print(conf_matrix)
# Prédire les probabilités sur l'ensemble de test
prob_predictions <- predict(model_rf_final, newdata = test_data, type = "prob")

# Créer les courbes ROC pour chaque classe
# 1. Pour la classe 'high risk'
roc_high <- roc(response = as.numeric(test_data$RiskLevel == "high risk"), 
                predictor = prob_predictions[, "high risk"])

# 2. Pour la classe 'low risk'
roc_low <- roc(response = as.numeric(test_data$RiskLevel == "low risk"), 
               predictor = prob_predictions[, "low risk"])

# 3. Pour la classe 'mid risk'
roc_mid <- roc(response = as.numeric(test_data$RiskLevel == "mid risk"), 
               predictor = prob_predictions[, "mid risk"])

# Tracer les courbes ROC
plot(roc_high, col = "red", main = "Courbes ROC pour chaque classe", lwd = 2)
plot(roc_low, col = "blue", add = TRUE, lwd = 2)
plot(roc_mid, col = "green", add = TRUE, lwd = 2)
legend(0.3,0.4, legend = c("High", "Low", "Mid"), col = c("red", "blue", "green"), lwd = 2, cex=0.5)


# Afficher l'AUC pour chaque classe
auc_high <- auc(roc_high)
auc_low <- auc(roc_low)
auc_mid <- auc(roc_mid)
auc <- cat("AUC High Risk:", auc_high, "\nAUC Low Risk:", auc_low, "\nAUC Mid Risk:", auc_mid, "\n")




