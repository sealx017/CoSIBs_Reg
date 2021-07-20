head(mtcars); dim(mtcars);
n = dim(mtcars)[1]

# Fitting a full model with 3 predictors 
k = 3 # no. of predictors
Full_model <- lm(mpg~disp+hp+drat, data = mtcars)
summary(Full_model)

# Fitting a reduced model with first 2 predictors 
Reduced_model <- lm(mpg~disp+hp, data = mtcars)
summary(Reduced_model)

# Checking residual sum of squares of both the models
r = 1 #no. of predictors forced to be 0 in the reduced model
Full_model_SSError = anova(Full_model)$'Sum Sq'[4]
Reduced_model_SSError = anova(Reduced_model)$'Sum Sq'[3]

# Computing partial F statistic from scratch
F_statistic = (Reduced_model_SSError-Full_model_SSError)/r/(Full_model_SSError/(n-k-1))
pf(F_statistic, r, (n-k-1), lower.tail = FALSE)

# Computing partial F statistic directly using a R function
anova(Full_model, Reduced_model)

# Checking variance inflation factor (VIF) of the predictors in the full model
car::vif(Full_model)
