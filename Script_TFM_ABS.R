#############################################################
# SCRIPT CREATED TO CORRELATE GROSS INCOME VS OTHER VARIABLES
# By Eduard J Alvarez Palau & Aleix Batlle Sureda
#############################################################

# DEFINE WORKING FOLDER
setwd("C:\\Users\\Aleix\\Documents\\R\\DADES\\TFM")

# READ ATTRIBUTES TABLE

library(foreign)
table <- read.dbf("Completmodel1.dbf", as.is = FALSE)
names(table)
head(table)

# PREPARE VARIABLES FOR THE MODEL
# CONVERT VARIABLES TO NUMERIC
table$CUSEC <- as.numeric(table$CUSEC)
table$CUMUN <- as.numeric(table$CUMUN)
table$POBTOTAL <- as.numeric(table$POBTOTAL)
table$E_MITJANA <- as.numeric(table$E_MITJANA)
table$RBP2019 <- as.numeric(table$RBP2019)
table$RNP2019 <- as.numeric(table$RNP2019)
table$DISTAV_1 <- as.numeric(table$DISTAV_1)
table$DISTAP <- as.numeric(table$DISTAP)
table$DISTCTR_12 <- as.numeric(table$DISTCTR_12)
table$DISTCBD_13 <- as.numeric(table$DISTCBD_13)
table$DISTF_14 <- as.numeric(table$DISTF_14)
table$DISTMTR_15 <- as.numeric(table$DISTMTR_15)
table$DISTBUS_16 <- as.numeric(table$DISTBUS_16)
table$A_INDUS_M2 <- as.numeric(table$A_INDUS_M2)
table$A_CMPCT_M2 <- as.numeric(table$A_CMPCT_M2)
table$A_SPRWL_M2 <- as.numeric(table$A_SPRWL_M2)
table$PERCENTARI <- as.numeric(table$PERCENTARI)
table$PERCENTARC <- as.numeric(table$PERCENTARC)
table$PERCENTARS <- as.numeric(table$PERCENTARS)
table$E15_64PERC <- as.numeric(table$E15_64PERC)
table$E65_100PER <- as.numeric(table$E65_100PER)
table$NV_1PERC <- as.numeric(table$NV_1PERC)
table$NV_5PERC <- as.numeric(table$NV_5PERC)
table$DISTF <- as.numeric(table$DISTF)
table$DIST_APIAV <- as.numeric(table$DIST_APIAV)
table$DENSITATPO <- as.numeric(table$DENSITATPO)
table$DIST_CAPME <- as.numeric(table$DIST_CAPME)
table$DIST_DROGO <- as.numeric(table$DIST_DROGO)
table$DIST_UNI <- as.numeric(table$DIST_UNI)
table$DIST_AJUNT <- as.numeric(table$DIST_AJUNT)
table$DIST_CEPRI <- as.numeric(table$DIST_CEPRI)
table$DIST_CEPUB <- as.numeric(table$DIST_CEPUB)
table$DIST_PQPI <- as.numeric(table$DIST_PQPI)
table$NV_1I2_PER <- as.numeric(table$NV_1I2_PER)
table$GINI_2019 <- as.numeric(table$GINI_2019)
table$P80P20_19 <- as.numeric(table$P80P20_19)

# CONVERT TYPE OF VARIABLE TO FACTOR  (SI NO HO CONVERTEIXES A FACTOR FUNCIONA LA MATRIU DE CORRELACIÓ)
table$CAPCOM <- factor(table$CAPCOM)
table$LITORAL <- factor(table$LITORAL) 
table$AMB <- factor(table$AMB) 

# CORRELATION MATRIX
colsCM <- (colnames(table) %in% c("RBP2019", 
                                  "DIST_APIAV","DISTCTR_12",
                                  "DISTCBD_13","DISTF_14",
                                  "DISTMTR_15","DISTBUS_16",
                                  "PERCENTARI","E0_14PERC",
                                  "E15_64PERC","NV_5PERC",
                                  "E65_100PER","DENSITATPO",
                                  "PERCENTARC","PERCENTARS",
                                  "DIST_CAPME",
                                  "DIST_UNI","DIST_CEPUB",
                                  "DIST_PQPI", "NV_1I2_PER", 
                                  "GINI_2019", "P80P20_19"
))
tableaCM <- subset(table,,colsCM)
CM <- cor(tableaCM, method = c("pearson", "kendall", "spearman"))
round(CM,2)

# PLOT CORRELATION MATRIX
#install.packages("corrplot")
library(corrplot)
corrplot(CM, type = "upper", order = "hclust", tl.cex=0.5,
         tl.col = "black", tl.srt = 45)

## VISUALISE BASIC STATISTICS MODEL COMPLET
# PLOT SUMMARY TABLE
summary(table)
# # PLOT HISTOGRAMS
hist(table$RBP2019,main= "Distribució de la RBP submodel AMB", ylab = "Nº Seccions censals",xlab = "RBP en €" , 
     xlim = c(0, 50000), breaks = 10 , col = "darkgreen" , freq = TRUE)
      # Extract breaks
my_breaks <- hist(table$RBP2019)$breaks 
      # Print breaks
my_breaks      
      # Specify colors corresponding to breaks
my_colors <- rep("#1b98e0", length(my_breaks))       
my_colors[my_breaks > 0] <- "#ffffff"
my_colors[my_breaks > 5000] <- "#f0fceb"
my_colors[my_breaks > 10000] <- "#caeac3"
my_colors[my_breaks > 15000] <- "#7bc87c"
my_colors[my_breaks > 20000] <- "#2a924a"
my_colors[my_breaks > 25000] <- "#0f702c"
my_colors[my_breaks > 30000] <- "#00441b"
my_colors[my_breaks > 35000] <- "#00441b"
my_colors[my_breaks > 40000] <- "#00441b"
my_colors[my_breaks > 45000] <- "#7bc87c"
my_colors
      # Base R histogram with colors
hist(table$RBP2019,main= "Distribució de la RBP", ylab = "Nº Seccions censals",xlab = "RBP en €" , 
     xlim = c(0, 50000), breaks = my_breaks, col = my_colors)

hist(table$DENSITATPO, main= "Densitat de població submodel AMB", ylab = "Nº Seccions censals",xlab = "hab/km2" , 
     xlim = c(0, 150000), breaks = 20 , col = "lightgrey", freq = TRUE)
hist(table$E_MITJANA)
hist(table$CAPCOM)
hist(table$LITORAL)
hist(table$DIST_APIAV)
hist(table$DISTF_14)
hist(table$DISTCBD_13)
hist(table$DISTMTR_15)
hist(table$DISTBUS_16)
hist(table$DISTCTR_12)
hist(table$PERCENTARI)
hist(table$PERCENTARS)
# Extract breaks
my_breaks <- hist(table$PERCENTARS)$breaks 
# Print breaks
my_breaks      
# Specify colors corresponding to breaks
my_colors <- rep("#1b98e0", length(my_breaks))       
my_colors[my_breaks > 0] <- "#ffffff"
my_colors[my_breaks > 5000] <- "#f0fceb"
my_colors[my_breaks > 10000] <- "#caeac3"
my_colors[my_breaks > 15000] <- "#7bc87c"
my_colors[my_breaks > 20000] <- "#2a924a"
my_colors[my_breaks > 25000] <- "#0f702c"
my_colors[my_breaks > 30000] <- "#00441b"
my_colors[my_breaks > 35000] <- "#00441b"
my_colors[my_breaks > 40000] <- "#00441b"
my_colors[my_breaks > 45000] <- "#7bc87c"
my_colors
# Base R histogram with colors
hist(table$PERCENTARS,main= "Distribució de la RBP", ylab = "Nº Seccions censals",xlab = "RBP en €" , 
     xlim = c(0, 50000), breaks = my_breaks, col = my_colors)
hist(table$PERCENTARC, ylab = Nº)
# Extract breaks
my_breaks <- hist(table$PERCENTARC)$breaks 
# Print breaks
my_breaks      
# Specify colors corresponding to breaks
my_colors <- rep("#1b98e0", length(my_breaks))       
my_colors[my_breaks > 0] <- "#ffffff"
my_colors[my_breaks > 0,1] <- "#f0fceb"
my_colors[my_breaks > 0,2] <- "#caeac3"
my_colors[my_breaks > 0,3] <- "#7bc87c"
my_colors[my_breaks > 0,4] <- "#2a924a"
my_colors[my_breaks > 0,5] <- "#0f702c"
my_colors[my_breaks > 0,6] <- "#00441b"
my_colors[my_breaks > 0,7] <- "#00441b"
my_colors[my_breaks > 0,8] <- "#00441b"
my_colors[my_breaks > 0,9] <- "#7bc87c"
my_colors
# Base R histogram with colors
hist(table$PERCENTARC,main= "Distribució de la RBP", ylab = "Nº Seccions censals",xlab = "RBP en €" , 
     xlim = c(0, 1), breaks = my_breaks, col = my_colors)
hist(table$NV_1PERC)
hist(table$NV_5PERC)
hist(table$DIST_CAPME)
#hist(table$DIST_DROGO)
hist(table$DIST_UNI)
#hist(table$DIST_AJUNT)
#hist(table$DIST_CEPRI)
hist(table$DIST_CEPUB)
hist(table$DIST_PQPI)
hist(table$NV_1I2_PER)
hist(table$GINI_2019)
hist(table$P80P20_19)

# PLOT SUMMARY TABLE
summary(table)
# BASIC STATISTICS
sd(table$RBP2019)
sd(table$DENSITATPO)
sd(table$E_MITJANA)
sd(table$CAPCOM)
sd(table$LITORAL)
sd(table$DIST_APIAV)
sd(table$DISTF_14)
sd(table$DISTCBD_13)
sd(table$DISTMTR_15)
sd(table$DISTBUS_16)
sd(table$DISTCTR_12)
sd(table$PERCENTARI)
sd(table$PERCENTARS)
sd(table$PERCENTARC)
#sd(table$NV_1PERC)
sd(table$NV_5PERC)
sd(table$DIST_CAPME)
#sd(table$DIST_DROGO)
sd(table$DIST_UNI)
#sd(table$DIST_AJUNT)
#sd(table$DIST_CEPRI)
sd(table$DIST_CEPUB)
sd(table$DIST_PQPI)
sd(table$NV_1I2_PER)
sd(table$GINI_2019)
sd(table$P80P20_19)

# PLOT SUMMARY TABLE
summary(table)
#SCATTERPLOT  
library(ggplot2)
data("table")
#Distf
ggplot(data = table , aes(x = DISTF_14, y = RBP2019)) + geom_point(size = 0.5 ) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Distbus
ggplot(data = table , aes(x = DISTBUS_16, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Distmetro
ggplot(data = table , aes(x = DISTMTR_15, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist autopistes i autovies 
ggplot(data = table , aes(x = DIST_APIAV, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CTR
ggplot(data = table , aes(x = DISTCTR_12, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CBD
ggplot(data = table , aes(x = DISTCBD_13, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist UNI
ggplot(data = table , aes(x = DIST_UNI, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CEPUB
ggplot(data = table , aes(x = DIST_CEPUB, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist PQPI
ggplot(data = table , aes(x = DIST_PQPI, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CAP
ggplot(data = table , aes(x = DIST_CAPME, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dens POB
ggplot(data = table , aes(x = DENSITATPO, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Mitjana EDAT
ggplot(data = table , aes(x = E_MITJANA, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#NV1_2 EST
ggplot(data = table , aes(x = NV_1I2_PER, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#NV5 EST
ggplot(data = table , aes(x = NV_5PERC, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#COMPACTPERC
ggplot(data = table , aes(x = PERCENTARC, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#SPRAWLPERC
ggplot(data = table , aes(x = PERCENTARS, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#INDUSPERC
ggplot(data = table , aes(x = PERCENTARI, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#GINI_2019
ggplot(data = table , aes(x = GINI_2019, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#P80P20
ggplot(data = table , aes(x = P80P20_19, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())


# TABLE SUBDIVISION table2 AMB/ table3 NO AMB (! EXCLOU VALORS)
table2 = table[!table$AMB==0,] 
table3 = table[!table$AMB==1,]

# OLS REGRESSION MODEL COMPLET
#install.packages("tidyverse")
library(tidyverse) 
model1a <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
                DISTMTR_15 + DISTBUS_16 + A_SPRWL_M2 + A_INDUS_M2 + A_CMPCT_M2 + NV_1PERC + NV_5PERC + AMB +
                DIST_CAPME + DIST_DROGO + DIST_UNI + DIST_AJUNT + DIST_CEPRI + DIST_CEPUB + DIST_PQPI + GINI_2019  , data = table)
summary(model1a)

# OLS REGRESSION WITH % CLC MODEL COMPLET
library(tidyverse) 
model1b <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
                DISTMTR_15 + DISTBUS_16 + DISTCTR_12 + PERCENTARI + PERCENTARS + PERCENTARC + NV_5PERC + AMB + 
                DIST_CAPME + DIST_UNI + DIST_PQPI + DIST_CEPUB + NV_1I2_PER + GINI_2019, data = table)
summary(model1b)

# OLS REGRESSION MODEL NO AMB
#install.packages("tidyverse")
library(tidyverse) 
model2a <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
                DISTMTR_15 + DISTBUS_16 + A_SPRWL_M2 + A_INDUS_M2 + A_CMPCT_M2 + NV_1PERC + NV_5PERC  + 
                DIST_CAPME + DIST_DROGO + DIST_UNI + DIST_AJUNT + DIST_CEPRI + DIST_CEPUB + DIST_PQPI  , data = table3)
summary(model2a)

# OLS REGRESSION WITH % CLC MODEL NO AMB
library(tidyverse) 
model2b <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
                DISTMTR_15 + DISTBUS_16 + DISTCTR_12 + PERCENTARI + PERCENTARS + PERCENTARC + NV_5PERC  + 
                DIST_CAPME + DIST_UNI + DIST_CEPUB + DIST_PQPI + NV_1I2_PER + GINI_2019, data = table3)
summary(model2b)

# OLS REGRESSION MODEL AMB
#install.packages("tidyverse")
library(tidyverse) 
model3a <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
                DISTMTR_15 + DISTBUS_16 + A_SPRWL_M2 + A_INDUS_M2 + A_CMPCT_M2 + NV_1PERC + NV_5PERC  + 
                DIST_CAPME + DIST_DROGO + DIST_UNI + DIST_AJUNT + DIST_CEPRI + DIST_CEPUB + DIST_PQPI , data = table2)
summary(model3a)

# OLS REGRESSION WITH % CLC MODEL AMB
library(tidyverse) 
model3b <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
                DISTMTR_15 + DISTBUS_16 + DISTCTR_12 + PERCENTARI + PERCENTARS + PERCENTARC + NV_5PERC  + 
                DIST_CAPME + DIST_UNI + DIST_CEPUB + DIST_PQPI + NV_1I2_PER + GINI_2019 , data = table2)
summary(model3b)

#SCATTERPLOT (proves link prof)
library(seaborn)
sns.lmplot(x="FlyAsh", y="Strength", hue="AirEntrain", data=con);
sns.lmplot(x="FlyAsh", y="Strength", data=con);

#SCATTERPLOT (proves YT) 1 tipus sense equació 
library(ggplot2)
data("cars")
ggplot(data = cars , aes(x = speed, y = dist)) + geom_point() + geom_abline(aes(intercept = 0, slope = 3), colour = "red", linetype = 1)

#SCATTERPLOT (proves YT + TIDYVERSEweb senyalitzacions) 1 tipus amb equació  
library(ggplot2)
data("table")
ggplot(data = table2 , aes(x = DIST_UNI, y = RBP2019)) + geom_point(mapping = aes(x = DIST_UNI, y = RBP2019)) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "grey", linetype = 1)



## VISUALISE BASIC STATISTICS MODEL AMB
# PLOT SUMMARY TABLE
summary(table2)
# # PLOT HISTOGRAMS
HV = hist(table2$RBP2019, main= "Distribució de la RBP submodel AMB", ylab = "Nº Seccions censals",xlab = "RBP en €" , 
     xlim = c(0, 50000), ylim = c(0,1000), breaks = 10 , col = "lightgrey", freq = TRUE)
print(HV)

hist(table2$RBP2019,main= "Distribució de la RBP", ylab = "Nº Seccions censals", ylim = c(0,1000), xlab = "RBP en €" , 
     xlim = c(0, 50000), breaks = my_breaks, col = my_colors)
# Extract breaks
my_breaks <- hist(table2$RBP2019)$breaks 
# Print breaks
my_breaks      
# Specify colors corresponding to breaks
my_colors <- rep("#1b98e0", length(my_breaks))       
my_colors[my_breaks > 0] <- "#ffffff"
my_colors[my_breaks > 5000] <- "#f0fceb"
my_colors[my_breaks > 10000] <- "#caeac3"
my_colors[my_breaks > 15000] <- "#7bc87c"
my_colors[my_breaks > 20000] <- "#2a924a"
my_colors[my_breaks > 25000] <- "#0f702c"
my_colors[my_breaks > 30000] <- "#00441b"
my_colors[my_breaks > 35000] <- "#00441b"
my_colors[my_breaks > 40000] <- "#00441b"
my_colors[my_breaks > 45000] <- "#7bc87c"
my_colors
# Base R histogram with colors
hist(table2$RBP2019,main= "Distribució de la RBP", ylab = "Nº Seccions censals", ylim = c(0,1000), xlab = "RBP en €" , 
     xlim = c(0, 50000), breaks = my_breaks, col = my_colors)

hist(table2$DENSITATPO, main= "Densitat de població submodel AMB", ylab = "Nº Seccions censals",xlab = "hab/km2" , 
     xlim = c(0, 150000), breaks = 200 , col = "lightgrey", freq = TRUE)
hist(table2$E_MITJANA, main= "Edat Mitjana submodel AMB", ylab = "Nº Seccions censals",xlab = "Edat(anys)" , 
     xlim = c(30, 60), breaks = 100 , col = "lightgrey", freq = TRUE)
#hist(table2$CAPCOM)
#hist(table2$LITORAL)
hist(table2$DIST_APIAV, main= "Distància a vies d'alta velocitat", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
hist(table2$DISTF_14, main= "Distància a parades d'autobus", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
hist(table2$DISTCBD_13, main= "Distància al centre municipal", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
hist(table2$DISTMTR_15, main= "Distància a estacions de metro", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1690), breaks = 20 , col = "lightgrey", freq = TRUE)
hist(table2$DISTBUS_16, main= "Distància a estacions de ferrocarril", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
hist(table2$DISTCTR_12, main= "Distància a carreteres principals", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
#hist(table2$PERCENTARI)
hist(table2$PERCENTARS, main= "Percentatge de sòl discontinu", ylab = "Nº Seccions censals",xlab = "Total de sòl discontinu " , 
     xlim = c(0, 1), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
hist(table2$PERCENTARC, main= "Percentatge sòl compacte", ylab = "Nº Seccions censals",xlab = "Total de sòl compacte" , 
     xlim = c(0, 1), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
#hist(table2$NV_1PERC)
hist(table2$NV_5PERC, main= "Percentatge de població amb alt nivell d'estudis", ylab = "Nº Seccions censals",xlab = "Percentatge" , 
     xlim = c(0, 1), ylim=c(0,1000), breaks = 10 , col = "lightgrey", freq = TRUE)
hist(table2$NV_1I2_PER, main= "Percentatge de població sense estudis", ylab = "Nº Seccions censals",xlab = "Total de població sense estudis" , 
     xlim = c(0, 1), ylim=c(0,1000), breaks = 10 , col = "lightgrey", freq = TRUE)
hist(table2$DIST_CAPME, main= "Distància a centres de salut", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
#hist(table$DIST_DROGO)
hist(table2$DIST_UNI, main= "Distància a centres universitàris", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
#hist(table$DIST_AJUNT)
#hist(table$DIST_CEPRI)
hist(table2$DIST_CEPUB, main= "Distància a centres educatius", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
hist(table2$DIST_PQPI, main= "Distància a centres de formació i inserció laboral per joves", ylab = "Nº Seccions censals",xlab = "Km" , 
     xlim = c(0, 40), ylim=c(0,1500), breaks = 10 , col = "lightgrey", freq = TRUE)
hist(table2$GINI_2019)
hist(table2$P80P20_19)

# PLOT SUMMARY TABLE 2 (MODEL AMB 2)
summary(table2)
# BASIC STATISTICS
sd(table2$RBP2019)
sd(table2$DENSITATPO)
sd(table2$E_MITJANA)
sd(table2$CAPCOM)
sd(table2$LITORAL)
sd(table2$DIST_APIAV)
sd(table2$DISTF_14)
sd(table2$DISTCBD_13)
sd(table2$DISTMTR_15)
sd(table2$DISTBUS_16)
sd(table2$DISTCTR_12)
sd(table2$PERCENTARI)
sd(table2$PERCENTARS)
sd(table2$PERCENTARC)
#sd(table$NV_1PERC)
sd(table2$NV_5PERC)
sd(table2$DIST_CAPME)
#sd(table$DIST_DROGO)
sd(table2$DIST_UNI)
#sd(table$DIST_AJUNT)
#sd(table$DIST_CEPRI)
sd(table2$DIST_CEPUB)
sd(table2$DIST_PQPI)
sd(table2$NV_1I2_PER)
sd(table2$GINI_2019)
sd(table2$P80P20_19)

# TABLE SUBDIVISION table2 AMB/ table3 NO AMB (! EXCLOU VALORS)
table2 = table[!table$AMB==0,] 
table3 = table[!table$AMB==1,]

# PLOT SUMMARY TABLE 2 (model AMB)
summary(table2)
#SCATTERPLOT  
library(ggplot2)
data("table")
#Distf
ggplot(data = table2 , aes(x = DISTF_14, y = RBP2019)) + geom_point(size = 0.5 ) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Distbus
ggplot(data = table2 , aes(x = DISTBUS_16, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Distmetro
ggplot(data = table2 , aes(x = DISTMTR_15, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist autopistes i autovies 
ggplot(data = table2 , aes(x = DIST_APIAV, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CTR
ggplot(data = table2 , aes(x = DISTCTR_12, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CBD
ggplot(data = table2 , aes(x = DISTCBD_13, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist UNI
ggplot(data = table2 , aes(x = DIST_UNI, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CEPUB
ggplot(data = table2 , aes(x = DIST_CEPUB, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist PQPI
ggplot(data = table2 , aes(x = DIST_PQPI, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CAP
ggplot(data = table2 , aes(x = DIST_CAPME, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dens POB
ggplot(data = table2 , aes(x = DENSITATPO, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Mitjana EDAT
ggplot(data = table2 , aes(x = E_MITJANA, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#NV1_2 EST
ggplot(data = table2 , aes(x = NV_1I2_PER, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#NV5 EST
ggplot(data = table2 , aes(x = NV_5PERC, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#COMPACTPERC
ggplot(data = table2 , aes(x = PERCENTARC, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#SPRAWLPERC
ggplot(data = table2 , aes(x = PERCENTARS, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#INDUSPERC
ggplot(data = table2 , aes(x = PERCENTARI, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#GINI_2019
ggplot(data = table2 , aes(x = GINI_2019, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#P80P20
ggplot(data = table2 , aes(x = P80P20_19, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())

# CORRELATION MATRIX MODEL AMB
colsCM <- (colnames(table2) %in% c("RBP2019", 
                                   "DIST_APIAV","DISTCTR_12",
                                   "DISTCBD_13","DISTF_14",
                                   "DISTMTR_15","DISTBUS_16",
                                   "PERCENTARI","E0_14PERC",
                                   "E15_64PERC","NV_5PERC",
                                   "E65_100PER","DENSITATPO",
                                   "PERCENTARC","PERCENTARS",
                                   "DIST_CAPME",
                                   "DIST_UNI","DIST_CEPUB",
                                   "DIST_PQPI", "NV_1I2_PER", 
                                   "GINI_2019", "P80P20_19"
))
tableaCM2 <- subset(table2,,colsCM)
CM <- cor(tableaCM2, method = c("pearson", "kendall", "spearman"))
round(CM,2)

# PLOT CORRELATION MATRIX AMB
#install.packages("corrplot")
library(corrplot)
corrplot(CM, type = "upper", order = "hclust", tl.cex=0.5,
         tl.col = "black", tl.srt = 45)

## VISUALISE BASIC STATISTICS MODEL NO AMB
# PLOT SUMMARY TABLE 3
summary(table3)
# # PLOT HISTOGRAMS
hist(table3$RBP2019, 
     main= "Distribució de la RBP al model NO AMB", ylab = "Nº Seccions censals",xlab = "RBP en €" , 
     xlim = c(0, 50000), breaks = 10 , col = "lightblue", freq = TRUE)
# Extract breaks
my_breaks <- hist(table2$RBP2019)$breaks 
# Print breaks
my_breaks      
# Specify colors corresponding to breaks
my_colors <- rep("#1b98e0", length(my_breaks))       
my_colors[my_breaks > 0] <- "#ffffff"
my_colors[my_breaks > 5000] <- "#f0fceb"
my_colors[my_breaks > 10000] <- "#caeac3"
my_colors[my_breaks > 15000] <- "#7bc87c"
my_colors[my_breaks > 20000] <- "#2a924a"
my_colors[my_breaks > 25000] <- "#0f702c"
my_colors[my_breaks > 30000] <- "#00441b"
my_colors[my_breaks > 35000] <- "#00441b"
my_colors[my_breaks > 40000] <- "#00441b"
my_colors[my_breaks > 45000] <- "#7bc87c"
my_colors
# Base R histogram with colors
hist(table3$RBP2019,main= "Distribució de la RBP", ylab = "Nº Seccions censals", ylim = c(0,1000), xlab = "RBP en €" , 
     xlim = c(0, 50000), breaks = my_breaks, col = my_colors)

#Analisis dels breaks per colorejar-los cadascun d'un color diferent o per crear gradients
hist(table3$DENSITATPO, 
     main= "Distribució de la RBP al model NO AMB", ylab = "Nº Seccions censals",xlab = "RBP en €" , 
     xlim = c(0, 100000), breaks = 10 , col = "lightblue", freq = TRUE)
# Extract breaks
my_breaks <- hist(table3$DENSITATPO, breaks = 10, xlim = c(0,100000))$breaks 
# Print breaks
my_breaks      
# Specify colors corresponding to breaks
my_colors <- rep("#1b98e0", length(my_breaks))       
my_colors[my_breaks > 0] <- "#ffffff"
my_colors[my_breaks > 5000] <- "#f0fceb"
my_colors[my_breaks > 10000] <- "#caeac3"
my_colors[my_breaks > 15000] <- "#7bc87c"
my_colors[my_breaks > 20000] <- "#2a924a"
my_colors[my_breaks > 25000] <- "#0f702c"
my_colors[my_breaks > 30000] <- "#00441b"
my_colors[my_breaks > 35000] <- "#00441b"
my_colors[my_breaks > 40000] <- "#00441b"
my_colors[my_breaks > 45000] <- "#7bc87c"
my_colors
# Base R histogram with colors
hist(table3$DENSITATPO,main= "Distribució de la RBP", ylab = "Nº Seccions censals", ylim = c(0,1000), xlab = "RBP en €" , 
     xlim = c(0, 100000), breaks = my_breaks, col = my_colors)
#Fer els breaks personalitzats 
table3$DENSITATPO <- rnorm(n = 851)
hist(table3$DENSITATPO , breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,150000))


hist(table3$E_MITJANA)
hist(table3$CAPCOM)
hist(table3$LITORAL)
hist(table3$DIST_APIAV)
hist(table3$DISTF_14)
hist(table3$DISTCBD_13)
hist(table3$DISTMTR_15)
hist(table3$DISTBUS_16)
hist(table3$DISTCTR_12)
hist(table3$PERCENTARI)
hist(table3$PERCENTARS)
hist(table3$PERCENTARC)
#hist(table2$NV_1PERC)
hist(table3$NV_5PERC)
hist(table3$DIST_CAPME)
#hist(table$DIST_DROGO)
hist(table3$DIST_UNI)
#hist(table$DIST_AJUNT)
#hist(table$DIST_CEPRI)
hist(table3$DIST_CEPUB)
hist(table3$DIST_PQPI)
hist(table3$NV_1I2_PER)
hist(table3$GINI_2019)
hist(table3$P80P20_19)

# PLOT SUMMARY TABLE 3 (MODEL NO AMB )
summary(table3)
# BASIC STATISTICS
sd(table3$RBP2019)
sd(table3$DENSITATPO)
sd(table3$E_MITJANA)
sd(table3$CAPCOM)
sd(table3$LITORAL)
sd(table3$DIST_APIAV)
sd(table3$DISTF_14)
sd(table3$DISTCBD_13)
sd(table3$DISTMTR_15)
sd(table3$DISTBUS_16)
sd(table3$DISTCTR_12)
sd(table3$PERCENTARI)
sd(table3$PERCENTARS)
sd(table3$PERCENTARC)
#sd(table$NV_1PERC)
sd(table3$NV_5PERC)
sd(table3$DIST_CAPME)
#sd(table$DIST_DROGO)
sd(table3$DIST_UNI)
#sd(table$DIST_AJUNT)
#sd(table$DIST_CEPRI)
sd(table3$DIST_CEPUB)
sd(table3$DIST_PQPI)
sd(table3$NV_1I2_PER)
sd(table3$GINI_2019)
sd(table3$P80P20_19)

# PLOT SUMMARY TABLE 3 (model NO AMB)
summary(table3)
#SCATTERPLOT  
library(ggplot2)
data("table")
#Distf
ggplot(data = table3 , aes(x = DISTF_14, y = RBP2019)) + geom_point(size = 0.5 ) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Distbus
ggplot(data = table3 , aes(x = DISTBUS_16, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Distmetro
ggplot(data = table3 , aes(x = DISTMTR_15, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist autopistes i autovies 
ggplot(data = table3 , aes(x = DIST_APIAV, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CTR
ggplot(data = table3 , aes(x = DISTCTR_12, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CBD
ggplot(data = table3 , aes(x = DISTCBD_13, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist UNI
ggplot(data = table3 , aes(x = DIST_UNI, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CEPUB
ggplot(data = table3 , aes(x = DIST_CEPUB, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
rlang::last_error()
rlang::last_trace()
#Dist PQPI
ggplot(data = table3 , aes(x = DIST_PQPI, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dist CAP
ggplot(data = table3 , aes(x = DIST_CAPME, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Dens POB
ggplot(data = tableaCM3 , aes(x = DENSITATPO, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#Mitjana EDAT
ggplot(data = table3 , aes(x = E_MITJANA, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#NV1_2 EST
ggplot(data = table3 , aes(x = NV_1I2_PER, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#NV5 EST
ggplot(data = table3 , aes(x = NV_5PERC, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#COMPACTPERC
ggplot(data = table3 , aes(x = PERCENTARC, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#SPRAWLPERC
ggplot(data = table3 , aes(x = PERCENTARS, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#INDUSPERC
ggplot(data = table3 , aes(x = PERCENTARI, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#GINI_2019
ggplot(data = table3 , aes(x = GINI_2019, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())
#P80P20
ggplot(data = table3 , aes(x = P80P20_19, y = RBP2019)) + geom_point(size = 0.5) + 
  geom_smooth(formula = y ~ x, method = "loess", colour = "#808080", linetype = 1) +  
  theme_set(theme_test())

# CORRELATION MATRIX MODEL NO AMB
colsCM <- (colnames(table3) %in% c("RBP2019", 
                                  "DIST_APIAV","DISTCTR_12",
                                  "DISTCBD_13","DISTF_14",
                                  "DISTMTR_15","DISTBUS_16",
                                  "PERCENTARI","E0_14PERC",
                                  "E15_64PERC","NV_5PERC",
                                  "E65_100PER","DENSITATPO",
                                  "PERCENTARC","PERCENTARS",
                                  "DIST_CAPME", "E_MITJANA",
                                  "DIST_UNI","DIST_CEPUB",
                                  "DIST_PQPI", "NV_1I2_PER",
                                  "GINI_2019", "P80P20_19"
))
tableaCM3 <- subset(table3,,colsCM)
CM <- cor(tableaCM3, method = c("pearson", "kendall", "spearman"))
round(CM,2)

# PLOT CORRELATION MATRIX NO AMB
#install.packages("corrplot")
library(corrplot)
corrplot(CM, type = "upper", order = "hclust", tl.cex=0.5,
         tl.col = "black", tl.srt = 45)


