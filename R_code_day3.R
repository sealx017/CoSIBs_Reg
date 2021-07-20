
setwd("/Users/seals/Desktop/CSPH/CoSIBS/CoSIBs_Reg/Datasets")

## First Logistic regression Example--------------------
leukemia <- read.delim("leukemia_remission.txt", fileEncoding="UCS-2LE")
head(leukemia)

# Fitting the logistic regression model 
model.1 <- glm(REMISS ~ LI, family="binomial", data = leukemia)
summary(model.1)

# Interval Estimate of log odds
confint.default(model.1) # based on asymptotic normality

# Interval Estimate of odds
exp(confint.default(model.1)) 

# Adding more predictors
model.2 <- glm(REMISS ~ CELL + SMEAR + INFIL + LI + BLAST + TEMP, family="binomial", data = leukemia)
require(lmtest)
lrtest(model.2 , model.1)

# logistic diagnostics
require(ResourceSelection)
hoslem.test(model.2$y, fitted(model.2), g=9) # Hoslem Test

1-model.2$deviance/model.2$null.deviance # "R-squared"

summary(influence.measures(model.2)) # examining influence



## Second Logistic regression Example--------------------
smoke_cancer_dataset = (matrix(c(281, 228, 210, 279),nrow = 2,ncol = 2))
colnames(smoke_cancer_dataset) = c(1, 0)
rownames(smoke_cancer_dataset) = c(1, 0)
smoke_cancer_dataframe = as.data.frame(as.table(smoke_cancer_dataset))
colnames(smoke_cancer_dataframe)[1:2] = c("Smoke", "Cancer")
smoke_cancer_dataframe$Cancer = ifelse(as.numeric(smoke_cancer_dataframe$Cancer)==1,1,0)


# Fitting the logistic regression model 
fit <- glm(Cancer ~ Smoke, weights = Freq, data = smoke_cancer_dataframe, family = binomial(logit))
summary(fit)

# Interpreting the estimates
beta0 = fit$coefficient[1]
beta1 = fit$coefficient[2]
pi0 <- exp(beta0) / (1 + exp(beta0))
pi1 <- exp(beta0 + beta1) / (1 + exp(beta0+beta1))

odds0 <- pi0 / (1 - pi0)
odds1 <- pi1 / (1 - pi1)
OR <- odds1 / odds0

#the same result with:
OR <- exp(beta1)

