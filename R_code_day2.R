## WLS example-------------------------------
setwd("/Users/seals/Desktop/CSPH/CoSIBS/")
galton_data = read.table("CoSIBs_Reg/Datasets/Galton.txt", header = T)

# OLS
model.1 <- lm(Progeny ~ Parent, data = galton_data)
summary(model.1)

# WLS
model.2 <- lm(Progeny ~ Parent, weights=1/SD^2, data = galton_data)
summary(model.2)



## Dummy variable example--------------------
Birth_data = read.table("CoSIBs_Reg/Datasets/birthsmokers.txt", header = T)

# Scatter-plot of the data
plot(Birth_data)


require(ggplot2)
ggplot(Birth_data, 
  aes(x = Gest, y = Wgt, col = Smoke)) + 
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# Creating a dummy variable corresponding to Smoking
Birth_data$Smoke_binary = as.factor(ifelse(Birth_data$Smoke=='no',1,0))

# Regression lequation with the dummy variable
LR_with_dummy = lm(Wgt~Gest+Smoke_binary, data = Birth_data)
summary(LR_with_dummy)


# Creating a second dummy variable corresponding to Smoking
Birth_data$Smoke_binary_second = as.factor(ifelse(Birth_data$Smoke=='yes',1,0))

# Regression lequation with the dummy variable
LR_with_two_dummies = lm(Wgt~Gest+Smoke_binary+Smoke_binary_second, data = Birth_data)
summary(LR_with_two_dummies)


## Chi square test of independence-----------

# Import the data
file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)


# Fancy Plot of the table
require(gplots)

# 1. convert the data as a table
dt <- as.table(as.matrix(housetasks))

# 2. Graph
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Chi square test of independence
chisq <- chisq.test(housetasks)
chisq

