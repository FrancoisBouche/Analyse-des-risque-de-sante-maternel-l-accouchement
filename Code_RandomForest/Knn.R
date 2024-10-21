# Charger les bibliothèques nécessaires
library(caret)
library(ggplot2)

# Charger le jeu de données Health_risk
Health_risk <- read.csv("Code_RandomForest/Maternal Health Risk Data Set.csv", stringsAsFactors = TRUE)

# Renommer les niveaux de la colonne RiskLevel pour les rendre compatibles avec R
Health_risk$RiskLevel <- factor(Health_risk$RiskLevel, 
                                levels = c("high risk", "mid risk", "low risk"), 
                                labels = c("high", "mid", "low"))

# Vérifier les nouveaux niveaux
print(levels(Health_risk$RiskLevel))

# Nombre d'initialisations aléatoires (seed) à tester
num_seeds <- 25
set.seed(123)  # Fixer la seed pour la reproductibilité initiale
seeds <- sample(1:10000, num_seeds)  # Générer 25 seeds aléatoires

# Créer une structure pour stocker les résultats
results_all_seeds <- data.frame(Seed = integer(), Best_k = integer(), Accuracy = double())

# Boucle sur les différentes seeds
for (s in seeds) {
  
  set.seed(s)
  
  # Division des données en ensemble d'entraînement (60%), validation (20%) et test (20%) avec la seed actuelle
  trainIndex <- createDataPartition(Health_risk$RiskLevel, p = 0.6, list = FALSE)
  trainData <- Health_risk[trainIndex, ]
  
  remainingData <- Health_risk[-trainIndex, ]
  valIndex <- createDataPartition(remainingData$RiskLevel, p = 0.5, list = FALSE)
  valData <- remainingData[valIndex, ]
  testData <- remainingData[-valIndex, ]
  
  # Contrôle pour validation croisée (10-fold cross-validation) sur l'ensemble de validation
  fitControl.TwoClass <- trainControl(method = "cv", 
                                      number = 10, 
                                      classProbs = TRUE, 
                                      summaryFunction = multiClassSummary)
  
  # Définir la grille pour k de 1 à 30
  knn_grid <- data.frame(k = 1:30)
  
  # Entraîner le modèle avec validation croisée en optimisant l'accuracy sur l'ensemble de validation
  knn_model_cv <- caret::train(RiskLevel ~ ., 
                               data = trainData, 
                               method = "knn", 
                               tuneGrid = knn_grid,  # Grille pour k
                               trControl = fitControl.TwoClass,
                               metric = "Accuracy")  # Optimiser l'accuracy sur validation
  
  # Meilleur k trouvé
  best_k <- knn_model_cv$bestTune$k
  
  # Entraîner le modèle final sur train + validation avec le meilleur k
  final_model <- knn3(RiskLevel ~ ., data = rbind(trainData, valData), k = best_k)
  
  # Prédictions sur le jeu de données test
  knn_predictions <- predict(final_model, newdata = testData, type = "class")
  
  # Calcul de l'accuracy sur l'ensemble de test
  test_accuracy <- sum(knn_predictions == testData$RiskLevel) / nrow(testData)
  
  # Stocker les résultats pour cette seed
  results_all_seeds <- rbind(results_all_seeds, data.frame(Seed = s, Best_k = best_k, Accuracy = test_accuracy))
  
  # Afficher les résultats intermédiaires
  print(paste("Seed:", s, "Meilleur k:", best_k, "Accuracy (Test):", round(test_accuracy, 4)))
}

# Afficher les résultats pour toutes les seeds
print("Résumé des meilleurs résultats pour chaque seed :")
print(results_all_seeds)

# Calcul de l'intervalle de confiance à 95 % pour les accuracies
mean_accuracy <- mean(results_all_seeds$Accuracy)
std_dev_accuracy <- sd(results_all_seeds$Accuracy)
error_margin <- qt(0.975, df = num_seeds - 1) * std_dev_accuracy / sqrt(num_seeds)

lower_bound <- mean_accuracy - error_margin
upper_bound <- mean_accuracy + error_margin

# Afficher l'intervalle de confiance à 95%
print(paste("Intervalle de confiance à 95% pour l'accuracy : [", round(lower_bound, 4), ", ", round(upper_bound, 4), "]"))

# Graphique de l'évolution de l'accuracy en fonction de k pour la dernière seed testée
ggplot(knn_model_cv$results, aes(x = k, y = Accuracy)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Evolution de l'Accuracy pour KNN avec validation croisée", 
       x = "Nombre de voisins (k)", 
       y = "Accuracy")

# Graphique de la distribution des meilleurs k pour chaque seed
ggplot(results_all_seeds, aes(x = Best_k)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution des meilleurs k pour différentes seeds", 
       x = "Meilleur k", 
       y = "Fréquence")
