#-------------------------------------------------------
# PCA - Math 585 - Project - Economic & Emissions Data 
# Nate Islip 
#-------------------------------------------------------------------------------------------------------------
# About - This file takes data from the EIA, and FRED institutions regarding economic indicators and their, 
# relationships pertaining to energy data obtained through the EIA. 
# More specifically, renewable and fossil fuel energy sources. 
#-------------------------------------------------------------------------------------------------------------

### Loading Packages =================================

{
library(psych)
library(dplyr)
library(matlib)
library(stargazer)
library(ggplot2)
library(latex2exp)
library(pastecs)
library(ggfortify)
library(patchwork)
library(MVT)
}

### Data Analysis & Import ===========================
 

# Upon initial inspection, the correlation matrix of the original data set 
# contained highly correlated variables (Vars > 80% correlation). These 
# variables were removed, and PCA was performed again, on the reduced data set.

# Reduced Data set (from correlation matrix)
{
df2 <- Data_Set_reduced # Reduced dataset from correlation matrix
s.df2 <- as.data.frame(scale(df2))
s.C2 <- cor(s.df2)
CorPlot1_2 <- pairs.panels(s.df2, cor = TRUE, ci = TRUE) 
CorPlot2_2 <- Alternative <- corPlot(df2, cex = 1.2)
}

# Original data set (before correlation matrix)
{
df <- Monthly_Data_TestSet # rename data 
# df$`Total Renewable Energy Production Monthly` <- NULL # REmove Production due to Multicolinearity
s.df <- as.data.frame(scale(df)) # standardize the data N(0,1)
s.df <- as_tibble(s.df)
s.C <- cor(s.df) # Correlation matrix 
CorPlot1 <- pairs.panels(s.df, cor = TRUE, ci = TRUE) 
CorPlot2 <- Alternative <- corPlot(df, cex = 1.2)
}

### Performing PCA  ========================================

# PCA (Original Data Set)
{
pca <- prcomp(s.df, scale=T)# PCA Analysis, Scaled for unit variance
loadings_pca <- data.frame(pca[["rotation"]])
PCs <- data.frame(PC = paste0("PC", 1:9), # Matrix of components and var
                  VAR = (pca$sdev)^2/sum((pca$sdev)^2))
} 


# PCA (Reduced Data Set)
{
  pca2 <- prcomp(s.df2, scale=T)# PCA Analysis, Scaled for unit variance
  loadings_pca2 <- data.frame(pca2[["rotation"]])
  PCs2 <- data.frame(PC = paste0("PC", 1:7), # Matrix of components and var
                    VAR = (pca2$sdev)^2/sum((pca2$sdev)^2))
} 

### Eigen Vectors and Eigen Values =======================

# (Original Data Set) Eigen values, and Eigen vectors 
{
EEs <- Eigen(cor(s.df)) # Eigens for Correlation matrix of Standardized Obs
EEva <- as.data.frame(EEs[["values"]])
EEve <- as.data.frame(EEs[["vectors"]])
}


# (Reduced Data Set) Eigen values, and Eigen vectors 
{
  EEs2 <- Eigen(cor(s.df2)) # Eigens for Correlation matrix of Standardized Obs
  EEva2 <- as.data.frame(EEs2[["values"]])
  EEve2 <- as.data.frame(EEs2[["vectors"]])
}

### PCA Plots (Biplots, Scree Plot, and Control Ellipse =====

# Reduced Data set 

# Scree Plot for determining Optimal # of PCs 
{
  plot(pca2)
  screeplot(pca2, type="line", main = "Scree plot of PCs versus Variances")
}
# Biplot for principal components
{
  PC12_2 <- autoplot(pca2, loadings = TRUE, loadings.label = T, x = 1, y = 2)
  PC13_2 <- autoplot(pca2, loadings = TRUE, loadings.label = T, x = 1, y = 3)
  PC14_2 <- autoplot(pca2, loadings = TRUE, loadings.label = T, x = 1, y = 4)
  PC15_2 <- autoplot(pca2, loadings = TRUE, loadings.label = T, x = 1, y = 5)
  PC16_2 <- autoplot(pca2, loadings = TRUE, loadings.label = T, x = 2, y = 3)
  PC17_2 <- autoplot(pca2, loadings = TRUE, loadings.label = T, x = 3, y = 4)
  
  PC12_2 + PC13_2 + PC14_2 + PC15_2 + 
    plot_layout(ncol = 2, nrow = 2)
}
# Plotting principal Components 
{
  PC_Scores_2 <- as.data.frame(pca2[['x']])
  PC12_3 <- ggplot(PC_Scores_2, aes(x = PC1, y = PC2)) + geom_point() + xlab(TeX('$\\hat{y}_{2}')) + ylab(TeX('$\\hat{y}_{1}')) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle(TeX('Plotting the scores of $\\hat{y}_{1}$ and $\\hat{y}_{2}')) + stat_ellipse()
  PC13_3 <- ggplot(PC_Scores_2, aes(x = PC1, y = PC3)) + geom_point() + xlab(TeX('$\\hat{y}_{3}')) + ylab(TeX('$\\hat{y}_{1}')) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle(TeX('Plotting the scores of $\\hat{y}_{1}$ and $\\hat{y}_{3}')) + stat_ellipse()
  PC14_3 <- ggplot(PC_Scores_2, aes(x = PC1, y = PC4)) + geom_point() + xlab(TeX('$\\hat{y}_{4}')) + ylab(TeX('$\\hat{y}_{1}')) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle(TeX('Plotting the scores of $\\hat{y}_{1}$ and $\\hat{y}_{4}')) + stat_ellipse()
  PC15_3 <- ggplot(PC_Scores_2, aes(x = PC1, y = PC5)) + geom_point() + xlab(TeX('$\\hat{y}_{5}')) + ylab(TeX('$\\hat{y}_{1}')) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle(TeX('Plotting the scores of $\\hat{y}_{1}$ and $\\hat{y}_{5}')) + stat_ellipse()
  
  PC12_3 + PC13_3 + PC14_3 + PC15_3 + 
    plot_layout(ncol = 2, nrow = 2)
}

# Original Data set

# Scree Plot for determining Optimal # of PCs 
{
plot(pca)
screeplot(pca, type="line", main = "Scree plot of PCs versus Variances")
}
# Biplot for principal components
{
PC12 <- autoplot(pca, loadings = TRUE, loadings.label = T, x = 1, y = 2)
PC13 <- autoplot(pca, loadings = TRUE, loadings.label = T, x = 1, y = 3)
PC14 <- autoplot(pca, loadings = TRUE, loadings.label = T, x = 1, y = 4)
PC15 <- autoplot(pca, loadings = TRUE, loadings.label = T, x = 1, y = 5)
PC16 <- autoplot(pca, loadings = TRUE, loadings.label = T, x = 2, y = 3)
PC17 <- autoplot(pca, loadings = TRUE, loadings.label = T, x = 3, y = 4)

PC12 + PC13 + PC14 + PC15 + 
  plot_layout(ncol = 2, nrow = 2)
}
# Plotting principal Components 
{
PC_Scores <- as.data.frame(pca[['x']])
PC12 <- ggplot(PC_Scores, aes(x = PC1, y = PC2)) + geom_point() + xlab(TeX('$\\hat{y}_{2}')) + ylab(TeX('$\\hat{y}_{1}')) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle(TeX('Plotting the scores of $\\hat{y}_{1}$ and $\\hat{y}_{2}')) + stat_ellipse()
PC13 <- ggplot(PC_Scores, aes(x = PC1, y = PC3)) + geom_point() + xlab(TeX('$\\hat{y}_{3}')) + ylab(TeX('$\\hat{y}_{1}')) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle(TeX('Plotting the scores of $\\hat{y}_{1}$ and $\\hat{y}_{3}')) + stat_ellipse()
PC14 <- ggplot(PC_Scores, aes(x = PC1, y = PC4)) + geom_point() + xlab(TeX('$\\hat{y}_{4}')) + ylab(TeX('$\\hat{y}_{1}')) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle(TeX('Plotting the scores of $\\hat{y}_{1}$ and $\\hat{y}_{4}')) + stat_ellipse()
PC15 <- ggplot(PC_Scores, aes(x = PC1, y = PC5)) + geom_point() + xlab(TeX('$\\hat{y}_{5}')) + ylab(TeX('$\\hat{y}_{1}')) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle(TeX('Plotting the scores of $\\hat{y}_{1}$ and $\\hat{y}_{5}')) + stat_ellipse()

PC12 + PC13 + PC14 + PC15 + 
  plot_layout(ncol = 2, nrow = 2)
}



#=========================END================================

