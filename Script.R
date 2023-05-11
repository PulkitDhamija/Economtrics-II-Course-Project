# Loading Dataset
library(readxl)
dataset <- read_excel("C:/Users/Pulkit/OneDrive - IIT Kanpur/Acads & Work/ECO342/Final Paper/FinalDataset.xlsx")
View(dataset)

#Lasso regression
library(glmnet)

# Defining variables
Y <- dataset$ElectricConsumptionScaled
X <- data.matrix(dataset[,c("LnPOP","LnGDP","LnGNE","Urbanpopulationgrowth","Averageprecipitationindepth","GDPpercapitagrowth","Consumerpriceindex","Populationgrowth")])

# Perform k-fold cross-validation to find optimal lambda value
cv_model = cv.glmnet(X, Y, alpha = 1)

# Find optimal lambda value that minimizes test MSE
best_lambda = cv_model$lambda.min
best_lambda

# Produce plot of test MSE by lambda value
#plot(cv_model)

# Find coefficients of best model
best_model <- glmnet(X, Y, alpha = 1, lambda = best_lambda)
coef(best_model)

# Quantile Regression
library(SparseM)
library(quantreg)
attach(dataset)
datanew = dataset[order(ElectricConsumptionScaled),]
attach(datanew)
Y <- cbind(ElectricConsumptionScaled)
X <- cbind(LnPOP, LnGNE, Averageprecipitationindepth, Consumerpriceindex, Populationgrowth)

# OLS regression
olsreg <- lm(Y ~ X, data=datanew)
summary(olsreg)

# Quantile regression
quantreg25 <- rq(Y ~ X, data=datanew, tau=0.25)
summary(quantreg25)

quantreg50 <- rq(Y ~ X, data=datanew, tau=0.5)
summary(quantreg50)

quantreg75 <- rq(Y ~ X, data=datanew, tau=0.75)
summary(quantreg75)

# ANOVA test for coefficient differences
anova(quantreg25, quantreg75)


