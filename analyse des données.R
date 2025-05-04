# Installer et charger les packages nécessaires
install.packages("FactorMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)

# Importer les données
data_with_anglais <- data.frame(
  Nom = c("Marième", "Fatou", "Sidi"),
  ADD = c(15, 16, 2),
  SI = c(17, 16, 9),
  Anglais = c(8, 10, 14)
)
data_with_anglais
# Réaliser l'ACP
acp_result <- PCA(data_with_anglais[, -1], scale.unit = TRUE, ncp = 5, graph = FALSE)

# Afficher le résumé des résultats
print(acp_result)

# Visualiser les résultats
fviz_pca_var(acp_result, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
fviz_pca_ind(acp_result, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
