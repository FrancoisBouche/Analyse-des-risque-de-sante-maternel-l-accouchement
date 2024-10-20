# les librairys
library(e1071)
library(caret)
library(ggplot2)
library(reshape2)
library(kernlab)


# Sauvegarder le modèle dans un fichier .rds
saveRDS(svm_model_low, file = "svm_model_low.rds")

# Sauvegarder le modèle dans un fichier .rds
saveRDS(svm_model_mid, file = "svm_model_mid.rds")

# Sauvegarder le modèle dans un fichier .rds
saveRDS(svm_model_high, file = "svm_model_high.rds")

