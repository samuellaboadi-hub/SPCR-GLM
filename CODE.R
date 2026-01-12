library(spcr)
library(ggplot2)
library(Matrix)
library(MASS)
library(elasticnet)
library(gridExtra)
library(mice)
library(nnet)
library(AER)
library(kableExtra)
# Data generation
set.seed(123)
p<-18
a1 <- c(3, 3);
a2 <- c(-3, 3); 
a3 <- c(-3, -3); 
a4 <- c(3, -3); 
a5 <- c(0, 0)
centers <- list(a1, a2, a3, a4, a5)
n <- 1000


u_list <- lapply(1:n, function(i) {
  j <- sample(1:5, 1)
  mvrnorm(1, mu = centers[[j]], Sigma = diag(0.5^2, 2))
})

u <- do.call(rbind, u_list)
Sigma_v <- outer(1:p, 1:p, function(i, j) 0.1^abs(i - j))
v <- mvrnorm(n, mu = rep(0, p), Sigma = Sigma_v)
z <- cbind(1,u, v)
beta<-c(1,1,-1,rep(0, p))
# Informative directions and response
logit_p <- as.vector(z%*%beta)
p <- 1 / (1 + exp(-logit_p))
y <- rbinom(n, size = 1, prob = p)

pairs(u,pch = 10, label=c("X1","X2"), main = "Original")


# Apply SPCR-GLM
result <- spcrglm(x = z, y = y, family = "binomial", k = 2,w = 0.01, xi = 0.001, lambda.B = 2, lambda.gamma = 0.1)
# PC scores
scores <- z %*% result$loadings.B
pairs(scores[, 1:2], labels=c("PC1","PC2","PC3"), pch = 19,
      main = "SPC-GLM")
pca_result <- prcomp(x, scale. = TRUE)
pca_scores <- pca_result$x[, 1:2]
pairs(pca_scores[, 1:2], labels=c("PC1","PC2","PC3"), pch = 19,
      main = "PCA")



# Application Doctor data set
data("DoctorVisits")
x <- model.matrix(~.,data=DoctorVisits)[,-c(1,2)]
y <- DoctorVisits$visits
model <- spcrglm(x = x, y = y, k = 5, family = "poisson",
                 lambda.B = 10, lambda.gamma = 0, w = 0.1, xi = 0.001, center=TRUE,scale = TRUE)
xs<-scale(x, center = TRUE, scale = TRUE)
loadings <- model$loadings.B
kbl(round(loadings,3),format="latex")
scores <- xs%*%loadings
pairs(scores,labels=c("PC1","PC2","PC3","PC4","PC5"))
pca_result <- prcomp(x, center = TRUE, scale. = TRUE)
pca_scores <- pca_result$x[, 1:5]
pairs(pca_scores)