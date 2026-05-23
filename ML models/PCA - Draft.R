# =========================================================
# PRINCIPAL COMPONENT ANALYSIS (PCA)
# Concrete Strength Dataset
# =========================================================

# =========================================================
# 1. INSTALL REQUIRED PACKAGES
# =========================================================

install.packages("factoextra")
install.packages("FactoMineR")
install.packages("corrplot")

# =========================================================
# 2. LOAD LIBRARIES
# =========================================================

library(factoextra)
library(FactoMineR)
library(corrplot)

# =========================================================
# 3. LOAD DATASET
# =========================================================

setwd("G:/R Programming/ML/Predicting Concrete Strength/")

data <- read.csv(
  "G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv"
)

head(data)
str(data)

# =========================================================
# 4. SELECT NUMERIC VARIABLES
# =========================================================

pca_data <- data[, c(
  "cement",
  "slag",
  "flyash",
  "water",
  "superplasticizer",
  "coarseaggregate",
  "fineaggregate",
  "age",
  "csMPa"
)]

# =========================================================
# 5. STANDARDIZE DATA
# =========================================================

pca_scaled <- scale(pca_data)

# =========================================================
# 6. RUN PCA
# =========================================================

pca_model <- prcomp(
  pca_scaled,
  
  center = TRUE,
  scale. = TRUE
)

# =========================================================
# 7. PCA SUMMARY
# =========================================================

summary(pca_model)

# =========================================================
# 8. EXPLAINED VARIANCE
# =========================================================

explained_variance <-
  pca_model$sdev^2 /
  sum(pca_model$sdev^2)

explained_variance

# =========================================================
# 9. CUMULATIVE VARIANCE
# =========================================================

cumulative_variance <- cumsum(explained_variance)

cumulative_variance

# =========================================================
# 10. CREATE VARIANCE TABLE
# =========================================================

variance_table <- data.frame(
  
  Principal_Component =
    paste0("PC",1:length(explained_variance)),
  
  Explained_Variance =
    explained_variance,
  
  Cumulative_Variance =
    cumulative_variance
)

print(variance_table)

# =========================================================
# 11. SCREE PLOT
# =========================================================

fviz_eig(
  pca_model,
  
  addlabels = TRUE,
  
  ylim = c(0,50),
  
  barfill = "steelblue",
  
  barcolor = "black",
  
  linecolor = "red",
  
  main = "Scree Plot of Principal Components"
)


fviz_eig(
  pca_model,
  
  addlabels = TRUE,
  
  ylim = c(0,50),
  
  barfill = "steelblue",
  
  barcolor = "black",
  
  linecolor = "red"
) +
  ggtitle("Scree Plot of Principal Components")

# =========================================================
# 12. PCA VARIABLE CONTRIBUTION PLOT
# =========================================================
fviz_pca_var(
  pca_model,
  
  col.var = "contrib",
  
  gradient.cols = c(
    "blue",
    "yellow",
    "red"
  ),
  
  repel = TRUE
) +
  ggtitle("PCA Variable Contribution Plot")
# =========================================================
# 13. PCA INDIVIDUAL SCORE PLOT
# =========================================================

fviz_pca_ind(
  pca_model,
  
  geom.ind = "point",
  
  col.ind = "blue",
  
  pointshape = 19,
  
  pointsize = 2,
  
  repel = TRUE,
)+ggtitle("PCA Score Plot")

# =========================================================
# 14. PCA BIPLOT
# =========================================================

fviz_pca_biplot(
  pca_model,
  repel = TRUE,
  col.var = "red",
  col.ind = "blue",
  pointshape = 19,
  pointsize = 2,
  title = "PCA Biplot of Concrete Variables"
)

# =========================================================
# 15. VARIABLE LOADINGS
# =========================================================

loadings <- pca_model$rotation

print(loadings)

# =========================================================
# 16. LOADINGS TABLE
# =========================================================

loading_table <- as.data.frame(loadings)

print(loading_table)

# =========================================================
# 17. CORRELATION MATRIX
# =========================================================

cor_matrix <- cor(pca_data)

corrplot(
  cor_matrix,
  
  method = "color",
  
  type = "upper",
  
  addCoef.col = "black",
  
  tl.col = "black",
  
  number.cex = 0.7
)

# =========================================================
# 18. SAVE PCA SCORES
# =========================================================

pca_scores <- as.data.frame(
  pca_model$x
)

write.csv(
  pca_scores,
  "PCA_Scores.csv",
  row.names = FALSE
)

# =========================================================
# 19. SAVE LOADINGS
# =========================================================

write.csv(
  loading_table,
  "PCA_Loadings.csv",
  row.names = TRUE
)

# =========================================================
# 20. INTERPRETATION HELP
# =========================================================

cat("\n============================\n")
cat("PCA INTERPRETATION GUIDE\n")
cat("============================\n")

cat("\nPC1 explains the largest variance.\n")

cat("\nVariables close together are positively correlated.\n")

cat("\nVariables opposite each other are negatively correlated.\n")

cat("\nLong arrows indicate strong contribution.\n")

cat("\nFirst 2 PCs are usually used for visualization.\n")

# =========================================================
# END OF PCA ANALYSIS
# =========================================================