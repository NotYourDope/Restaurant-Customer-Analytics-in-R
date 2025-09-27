sabor <- read.csv("") # indicate correct path to the dataset csv file

install.packages("performance")
library(performance)
install.packages("psych")
install.packages("parameters")
library(psych)
library(parameters)


# 1.a-e
model <- lm(MntVeganVegetarian ~ Income + Age + Recency + MntMeatFish, data = sabor)
summary(model)

# 1.f, g
plot(
  sabor$Income, sabor$MntVeganVegetarian, 
  main="Income vs MntVegan.Vegetarian", 
  xlab="Income", 
  ylab=" MntVegan.Vegetarian", 
  col="darkblue",  
  pch=19)       
abline(lm(sabor$MntVeganVegetarian ~ sabor$Income), col="red")    

shapiro.test(residuals(model))  

mean(residuals(model))

check_collinearity(model)

# 1.h
model <- lm(MntVeganVegetarian ~ Income + Age + Recency + MntMeatFish + Gender + Response_Cmp4, data = sabor)
summary(model)

# 2.1.a
cor_matrix <- cor(sabor[, c("MntMeatFish", "MntEntries", "MntVeganVegetarian",
                            "MntDrinks", "MntDesserts", "MntAdditionalRequests")])
kmo_result <- KMO(cor_matrix)
print(kmo_result)

bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(sabor))
print(bartlett_result)

# 2.1.b

# PCA
pca_result <- principal(sabor[, c("MntMeatFish", "MntEntries", "MntVeganVegetarian",
                                  "MntDrinks", "MntDesserts", "MntAdditionalRequests")],
                        nfactors = 6, rotate = "none")
# Table with eigenvalues and variance
eigenvalues <- pca_result$values
variances <- eigenvalues / sum(eigenvalues) * 100
cum_variances <- cumsum(variances)
# Create table with results
pca_table <- data.frame(
  Componente = 1:length(eigenvalues),
  Valor_Proprio = eigenvalues,
  Variância_Explicada = variances,
  Variância_Acumulada = cum_variances)
print(pca_table)

# Scree Plot
plot(pca_table$Componente, pca_table$Valor_Proprio,
     type = "b", # Lines and points
     main = "Scree Plot",
     xlab = "Componente Principal",
     ylab = "Valor Próprio (Eigenvalue)",
     pch = 19) # Color of the line and points


# 2.2
num_components <- 3

# Factor loadings of the retained components
loadings_retidos <- pca_result$loadings[, 1:num_components]

# Communalities: Sum of squared loadings of the retained components
comunalidades <- rowSums(loadings_retidos^2)

# Create table of communalities
comunalidades_table <- data.frame(
  Variável = rownames(pca_result$loadings),
  Comunalidade = round(comunalidades, 2))
print(comunalidades_table)

# Varimax rotation

# Loadings before rotation (retained components only)
loadings_antes <- pca_result$loadings[, 1:num_components]

# Show rounded loadings
loadings_antes_table <- data.frame(round(loadings_antes, 2))

# Apply Varimax rotation
rotated_result <- principal(sabor[, c("MntMeatFish", "MntEntries", "MntVeganVegetarian",
                                      "MntDrinks", "MntDesserts", "MntAdditionalRequests")],
                            nfactors = num_components, rotate = "varimax", cor = TRUE)

# Loadings after rotation
loadings_rotacao <- rotated_result$loadings[, 1:num_components]

# Show rounded loadings
loadings_rotacao_table <- data.frame(round(loadings_rotacao, 2))

# 2.2.c

# Save the scores in the original dataset
sabor <- cbind(sabor, as.data.frame(rotated_result$scores))

# Manually created indices
vars_PC1 <- c("MntEntries", "MntDrinks", "MntDesserts")
vars_PC2 <- c("MntAdditionalRequests")
vars_PC3 <- c("MntMeatFish", "MntVeganVegetarian")
sabor$Indice_PC1 <- rowMeans(sabor[, vars_PC1], na.rm = TRUE)
sabor$Indice_PC2 <- sabor$MntAdditionalRequests
sabor$Indice_PC3 <- rowMeans(sabor[, vars_PC3], na.rm = TRUE)

# Check consistency using correlation
correlacao_PC1 <- cor(sabor$Indice_PC1, sabor$RC1)
correlacao_PC2 <- cor(sabor$Indice_PC2, sabor$RC2)
correlacao_PC3 <- cor(sabor$Indice_PC3, sabor$RC3)

# Show results
print(correlacao_PC1)
print(correlacao_PC2)
print(correlacao_PC3)





# 3
# Select the indices as variables
dados_clustering <- sabor[, c("Indice_PC1", "Indice_PC2", "Indice_PC3", "Income", "Age")]

# Dissimilarity matrix (squared Euclidean distance)
distances <- dist(dados_clustering, method =
                    "euclidean")^2
dissimilarity_matrix <- as.matrix(round(distances, 2))

# Apply Ward's method
cluster_hierarchical <- hclust(distances, method = "ward.D2")

# Create the agglomerative process table
agglomeration <- data.frame(
  Step = 1:(nrow(dados_clustering) - 1),
  Cluster1 = cluster_hierarchical$merge[, 1],
  Cluster2 = cluster_hierarchical$merge[, 2],
  Distance = round(cluster_hierarchical$height, 4))

# Determine the 'Next Stage' for each cluster
next_stage <- rep(NA, nrow(agglomeration))
for (i in 1:nrow(agglomeration)) {next_stage[i] <- which(i == agglomeration$Cluster1 | i == agglomeration$Cluster2)[1]}
agglomeration$Next_Stage <- ifelse(is.na(next_stage), "-", next_stage)

# Adjust cluster names
agglomeration$Cluster1 <- ifelse(agglomeration$Cluster1 < 0, -agglomeration$Cluster1, paste0("Cluster_", agglomeration$Cluster1))
agglomeration$Cluster2 <- ifelse(agglomeration$Cluster2 < 0, -agglomeration$Cluster2, paste0("Cluster_", agglomeration$Cluster2))
print(agglomeration)

library(ggplot2)
library(ggdendro)
# Apply Ward's method
cluster_hierarchical <- hclust(distances, method = "ward.D2")
dendro_data <- as.dendrogram(cluster_hierarchical)
ggdendrogram(dendro_data, theme_dendro = FALSE) + labs(y = "Distância Euclidiana ao Quadrado")




library(dplyr)
# Create 3 clusters using the K-means method
set.seed(123) # Ensure reproducibility
kmeans_result <- kmeans(dados_clustering, centers = 4,
                        nstart = 25)
# Table of the final cluster centers
final_cluster_centers <-
  data.frame(kmeans_result$centers)
# Number of cases in each cluster
cluster_sizes <-
  as.data.frame(table(kmeans_result$cluster))
colnames(cluster_sizes) <- c("Cluster",
                             "Número_de_Casos")
# Distance between the Final Cluster Centers
dist_matrix <- as.matrix(dist(kmeans_result$centers))
# ANOVA
# Treat clusters as a factor
clusters <- as.factor(kmeans_result$cluster)
variaveis_clustering <- names(dados_clustering)
# Iterate over variables to compute ANOVA
resultados_anova <- lapply(variaveis_clustering, function(var) {
  anova_result <- summary(aov(dados_clustering[[var]] ~
                                clusters))[[1]]
  f_valor <- anova_result[["F value"]][1]
  p_valor <- anova_result[["Pr(>F)"]][1]
  mean_square_cluster <- anova_result[["Mean Sq"]][1]
  mean_square_error <- anova_result[["Mean Sq"]][2]
  data.frame(Variável = var, Mean_Square_Cluster =
               mean_square_cluster, Mean_Square_Error = mean_square_error, F_Valor =
               f_valor, P_Valor = p_valor)})
# Combine results into a single data frame
tabela_anova <- do.call(rbind, resultados_anova)


library(tidyr)
# Convert data to long format
data_long <- final_cluster_centers %>%
  pivot_longer(cols = c("Indice_PC1", "Indice_PC2", "Indice_PC3", "Income", "Age"),
               names_to = "Variable", values_to = "Value")

# Build bar charts with variable partitioning
ggplot(data_long, aes(x = factor(Cluster), y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Variable, scales = "free_y") +  # Separate plots by variables
  labs(title = "Bar Charts for Each Variable by Cluster",
       x = "Cluster", y = "Value", fill = "Variable") +
  theme_minimal()
