# Chargement les bibliothèques nécessaires
library(randomForest)

# Lire les données
data <- read.csv("Maternal Health Risk Data Set.csv")

# Afficher les premières lignes et structure des données
head(data)
str(data)
data$RiskLevel <- as.factor(data$RiskLevel)


############################### Division de la data en train et en test :
# Fixer la graine pour assurer la reproductibilité
set.seed(133)

# Définir la taille de l'ensemble d'entraînement (70 %)
train_index <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))

# Diviser les données en ensemble d'entraînement et de test
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Vérifier la taille des ensembles
cat("Taille de l'ensemble d'entraînement :", nrow(train_data), "\n")
cat("Taille de l'ensemble de test :", nrow(test_data), "\n")


##################### implémentation du modéle de base 
# Création du modèle randomForest avec RiskLevel comme variable cible
model_rf1 <- randomForest(RiskLevel ~ ., data = train_data, ntree = 500, mtry = 3, importance = TRUE)

# Afficher le modèle
print(model_rf1)
# Call:
#   randomForest(formula = RiskLevel ~ ., data = train_data, ntree = 500,      mtry = 3, importance = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# OOB estimate of  error rate: 20.59% # à améliorer
# Confusion matrix:
#   high risk low risk mid risk class.error
# high risk       172        7       13   0.1041667
# low risk          8      230       55   0.2150171
# mid risk         19       44      161   0.2812500
plot(model_rf1)
########## regardons un peu notre data, est qu'il y du déséquilibre entre les modalités de notre variable cible
table(data$RiskLevel)
# high risk  low risk  mid risk 
# 272       406       336

# test de umbalanced vs balanced
is.pbalanced(data) # [1] FALSE # donc donnée déséquilibré
######################################## boucle sur plusieurs arbre pour essayer d'améliorer mon estimation:
# Définir un vecteur pour stocker les résultats
results2 <- data.frame(ntree = integer(), OOB_Error = numeric())

# Boucle pour tester plusieurs valeurs de ntree
for (n in seq(1, 500, by = 1)) {
  # Créer le modèle randomForest avec la valeur de ntree actuelle
  model_rf2 <- randomForest(RiskLevel ~ ., data = train_data, ntree = n, mtry = 3, importance = TRUE)
  
  # Enregistrer la valeur de ntree et l'erreur OOB
  results2 <- rbind(results2, data.frame(ntree = n, OOB_Error = model_rf2$err.rate[nrow(model_rf2$err.rate), "OOB"]))
}

# Afficher les résultats
print(results2)
plot(model_rf2)
# Identifier la meilleure valeur de ntree (celle avec la plus faible erreur OOB)
best_ntree2 <- results2[which.min(results2$OOB_Error), "ntree"]
cat("La meilleure valeur de ntree est :", best_ntree2, "avec une erreur OOB de :", min(results2$OOB_Error), "\n")

#La meilleure valeur de ntree est : 156 avec une erreur OOB de : 0.1833568 
############################## tester de différents mtry, ie nombre de variable pris dans les noeuds des arbres
# Fixer la graine pour la reproductibilité
set.seed(123)

# Définir un vecteur pour stocker les résultats
results3 <- data.frame(mtry = integer(), OOB_Error = numeric())

# Boucle pour tester plusieurs valeurs de mtry
for (m in 1:(ncol(train_data) - 1)) {
  # Créer le modèle randomForest avec la valeur de mtry actuelle
  model_rf3 <- randomForest(RiskLevel ~ ., data = train_data, ntree = 156, mtry = m, importance = TRUE)
  
  # Enregistrer la valeur de mtry et l'erreur OOB (Out-Of-Bag) pour évaluation
  results3 <- rbind(results3, data.frame(mtry = m, OOB_Error = model_rf3$err.rate[nrow(model_rf3$err.rate), "OOB"]))
}

# Afficher les résultats
print(results3)

# Identifier la meilleure valeur de mtry (celle avec la plus faible erreur OOB)
best_mtry3 <- results3[which.min(results3$OOB_Error), "mtry"]
cat("La meilleure valeur de mtry est :", best_mtry3, "avec une erreur OOB de :", min(results3$OOB_Error), "\n")
#La meilleure valeur de mtry est : 2 avec une erreur OOB de : 0.1946403 

####### combinaaison de mes 2 bests param pour voir si j'améliore mon modèle de foret.
# Créer le modèle randomForest avec RiskLevel comme variable cible
set.seed(133)
model_rf_final <- randomForest(RiskLevel ~ ., data = train_data, ntree = 156, mtry = 2, importance = TRUE)

# Afficher le modèle
print(model_rf_final)
plot(model_rf_final) 

# Call:
#   randomForest(formula = RiskLevel ~ ., data = train_data, ntree = 156,      mtry = 2, importance = TRUE) 
# Type of random forest: classification
# Number of trees: 156
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 18.9%
# Confusion matrix:
#   high risk low risk mid risk class.error
# high risk       172        7       11  0.09473684
# low risk          8      232       50  0.20000000
# mid risk         14       44      171  0.25327511

###############test en changeant le % de data train
# Fixer la graine pour assurer la reproductibilité
set.seed(123)

# Définir la taille de l'ensemble d'entraînement (63 %)
train_index2 <- sample(seq_len(nrow(data)), size = 0.63 * nrow(data))

# Diviser les données en ensemble d'entraînement et de test
train_data2 <- data[train_index2, ]
test_data2 <- data[-train_index2, ]

# Vérifier la taille des ensembles
cat("Taille de l'ensemble d'entraînement :", nrow(train_data2), "\n")
cat("Taille de l'ensemble de test :", nrow(test_data2), "\n")

model_rf_final2 <- randomForest(RiskLevel ~ ., data = train_data2, ntree = 156, mtry = 2, importance = TRUE)

# Afficher le modèle
print(model_rf_final2)
plot(model_rf_final2)
importance(model_rf_final2) #ntree = 156, mtry = 2
var.imp <- model_rf_final2$importance
ord <- order(var.imp, decreasing = TRUE)
barplot(sort(var.imp, decreasing = TRUE),
        names.arg = row.names(var.imp)[ord], cex.names=0.6)

# pas mieux
# Call:
#   randomForest(formula = RiskLevel ~ ., data = train_data2, ntree = 156,      mtry = 2, importance = TRUE) 
# Type of random forest: classification
# Number of trees: 156
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 20.06%
# Confusion matrix:
#   high risk low risk mid risk class.error
# high risk       146        7       17   0.1411765
# low risk          6      213       44   0.1901141
# mid risk         17       37      151   0.2634146

################### selection param du mtry avec la bibliothèque caret
# Charger les bibliothèques nécessaires
library(caret)

# Définir une grille de valeurs pour mtry
tune_grid <- expand.grid(mtry = 1:(ncol(train_data) - 1))  # Tester toutes les valeurs possibles de mtry

# Entraîner le modèle randomForest avec caret en utilisant OOB
model_caret <- train(RiskLevel ~ ., 
                     data = train_data, 
                     method = "rf", 
                     trControl = trainControl(method = "oob"),  # Utiliser OOB
                     tuneGrid = tune_grid)

# Afficher les résultats
print(model_caret)

# Visualiser les résultats
plot(model_caret)

# test 2ieme façon de procéder (coe dans le livre du prof husson):
library(caret)
tune_grid <- expand.grid(mtry = 1:(ncol(train_data) - 1))
ctrl <- trainControl(method = "oob")
library(doParallel) # pour paralléliser
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
set.seed(12345)
sel.mtry <- train(RiskLevel ~ .,
                  data = train_data, 
                  method = "rf",
                  trControl =ctrl,
                  tuneGrid =tune_grid)

stopCluster(cl)
sel.mtry

# Random Forest 
# 
# 709 samples
# 6 predictor
# 3 classes: 'high risk', 'low risk', 'mid risk' 
# 
# No pre-processing
# Resampling results across tuning parameters: 
#   
#   mtry  Accuracy   Kappa    
# 1     0.7771509  0.6577485
# 2     0.8194640  0.7252587
# 3     0.8279267  0.7389086
# 4     0.8208745  0.7287535
# 5     0.8208745  0.7286897
# 6     0.8208745  0.7287036
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 3.

# en utilisant ce nouveau resuslt et le ntree 156 obtenu précedemment est -ce que j'arrive à améliorer mon modele
set.seed(123)
model_rf_final4 <- randomForest(RiskLevel ~ ., data = train_data, ntree = 156, mtry = 3, importance = TRUE)

# Afficher le modèle
print(model_rf_final4)
plot(model_rf_final4) 
importance(model_rf_final4) #ntree = 156, mtry = 3

# > importance(model_rf_final2) #ntree = 156, mtry = 2
# high risk  low risk mid risk MeanDecreaseAccuracy MeanDecreaseGini
# Age          6.702115 10.392250 26.58113             22.50974         53.60410
# SystolicBP  19.238080 23.973344 25.37017             35.70543         63.87534
# DiastolicBP 11.294879  7.748201 21.81537             23.28778         41.82709
# BS          38.983787 35.701764 38.57914             57.56666        124.17808
# BodyTemp    10.450038 15.044878 17.08269             23.11040         23.53108
# HeartRate    6.556232  4.632051 23.12116             21.24578         37.24985
# > importance(model_rf_final4) #ntree = 156, mtry = 3
# high risk  low risk mid risk MeanDecreaseAccuracy MeanDecreaseGini
# Age          6.409808 12.874081 32.93074             27.94008         62.29304
# SystolicBP  24.974198 29.875406 31.05894             46.96642         81.27927
# DiastolicBP 10.640318 10.374676 24.11840             25.92491         45.42211
# BS          39.873161 39.990780 52.58974             71.92234        147.54516
# BodyTemp    17.458146 16.801369 20.29793             29.77565         27.52238
# HeartRate    9.607146  2.924792 25.98515             25.92925         38.72488

####################################################################################
# prédictions avec le modéle de #ntree = 156, mtry = 3
# Prédictions sur l'ensemble de test
predictions <- predict(model_rf_final4, newdata = test_data)

# Calcul de la matrice de confusion
conf_matrix <- confusionMatrix(predictions, test_data$RiskLevel)

# Afficher la matrice de confusion et les métriques de performance
print(conf_matrix)

# prédictions sur modele de #ntree = 156, mtry = 2 avec 1 train = 63%
# Prédictions sur l'ensemble de test
predictions2 <- predict(model_rf_final2, newdata = test_data)

# Calcul de la matrice de confusion
conf_matrix2 <- confusionMatrix(predictions2, test_data$RiskLevel)

# Afficher la matrice de confusion et les métriques de performance
print(conf_matrix2)


# prédictions avec le modéle de #ntree = 156, mtry = 2 avec 1 train = 70%
# Prédictions sur l'ensemble de test
predictions3 <- predict(model_rf_final, newdata = test_data)

# Calcul de la matrice de confusion
conf_matrix3 <- confusionMatrix(predictions3, test_data$RiskLevel)

# Afficher la matrice de confusion et les métriques de performance
print(conf_matrix3)


# accurancy sur le train
#mtry  Accuracy   Kappa
# 3     0.8279267  0.7389086 

# accurancy sur le test en utilisant le modele_finale2 #ntree = 156, mtry = 2 avec train 63%
#Accuracy : 0.8918 
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  high risk low risk mid risk
# high risk        78        0        7
# low risk          2      103        9
# mid risk          2       13       91


# accurancy sur le test en utilisant le modele_finale4 #ntree = 156, mtry = 3
# Accuracy : 0.8295 

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  high risk low risk mid risk
# high risk        77        1       15
# low risk          1       97       13
# mid risk          4       18       79

# accurancy sur le test en utilisant le modele_finale #ntree = 156, mtry = 2 avec un train de 70% mm chose que avec train = 63%
# Accuracy : 0.8918
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  high risk low risk mid risk
# high risk        78        0        7
# low risk          2      103        9
# mid risk          2       13       91
###################################""" tracer de la courbe ROC pour le modele modele_finale4 #ntree = 156, mtry = 3
# Installation de pROC 
#install.packages("pROC")
# Chargement du package nécessaire
library(pROC)

# Prédire les probabilités sur l'ensemble de test
prob_predictions <- predict(model_rf_final4, newdata = test_data, type = "prob")

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

# # Ajouter une légende pour identifier chaque courbe
# legend("bottomright", legend = c("High Risk", "Low Risk", "Mid Risk"), col = c("red", "blue", "green"), lwd = 2)
# Ajouter une légende pour identifier chaque courbe
legend("bottomright", 
       legend = c(paste("High Risk (AUC =", round(auc_high, 2), ")"), 
                  paste("Low Risk (AUC =", round(auc_low, 2), ")"), 
                  paste("Mid Risk (AUC =", round(auc_mid, 2), ")")), 
       col = c("red", "blue", "green"), lwd = 2)


# Afficher l'AUC pour chaque classe
auc_high <- auc(roc_high)
auc_low <- auc(roc_low)
auc_mid <- auc(roc_mid)
cat("AUC High Risk:", auc_high, "\nAUC Low Risk:", auc_low, "\nAUC Mid Risk:", auc_mid, "\n")

########################""" tracer de la courbe ROC pour le modele modele_finale2 #ntree = 156, mtry = 2
# Prédire les probabilités sur l'ensemble de test
prob_predictions2 <- predict(model_rf_final2, newdata = test_data, type = "prob")

# Créer les courbes ROC pour chaque classe
# 1. Pour la classe 'high risk'
roc_high <- roc(response = as.numeric(test_data$RiskLevel == "high risk"), 
                predictor = prob_predictions2[, "high risk"])

# 2. Pour la classe 'low risk'
roc_low <- roc(response = as.numeric(test_data$RiskLevel == "low risk"), 
               predictor = prob_predictions2[, "low risk"])

# 3. Pour la classe 'mid risk'
roc_mid <- roc(response = as.numeric(test_data$RiskLevel == "mid risk"), 
               predictor = prob_predictions2[, "mid risk"])

# Tracer les courbes ROC
plot(roc_high, col = "red", main = "Courbes ROC2 pour chaque classe", lwd = 2)
plot(roc_low, col = "blue", add = TRUE, lwd = 2)
plot(roc_mid, col = "green", add = TRUE, lwd = 2)

# # Ajouter une légende pour identifier chaque courbe
# legend("bottomright", legend = c("High Risk", "Low Risk", "Mid Risk"), col = c("red", "blue", "green"), lwd = 2)
# Ajouter une légende pour identifier chaque courbe
legend("bottomright", 
       legend = c(paste("High Risk (AUC =", round(auc_high, 2), ")"), 
                  paste("Low Risk (AUC =", round(auc_low, 2), ")"), 
                  paste("Mid Risk (AUC =", round(auc_mid, 2), ")")), 
       col = c("red", "blue", "green"), lwd = 2)


# Afficher l'AUC pour chaque classe
auc_high <- auc(roc_high)
auc_low <- auc(roc_low)
auc_mid <- auc(roc_mid)
cat("AUC High Risk:", auc_high, "\nAUC Low Risk:", auc_low, "\nAUC Mid Risk:", auc_mid, "\n")

