#############################################################
# SCRIPT CREATED TO CORRELATE GROSS INCOME VS OTHER VARIABLES
# By Eduard J Alvarez Palau
#############################################################

# DEFINE WORKING FOLDER
setwd("C:\\Users\\Aleix\\Documents\\R\\DADES\\TFM")

# READ ATTRIBUTES TABLE

library(foreign)
table <- read.dbf("Modelcomplet.dbf", as.is = FALSE)
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
# CONVERT TYPE OF VARIABLE TO FACTOR  (SI NO HO CONVERTEIXES A FACTOR FUNCIONA LA MATRIU DE CORRELACIÓ)
table$CAPCOM <- factor(table$CAPCOM)
table$LITORAL <- factor(table$LITORAL) 
table$AMB <- factor(table$AMB) 

# CORRELATION MATRIX
colsCM <- (colnames(table) %in% c("RBP2019", "DISTAP",
                                    "DISTAV_1","DISTCTR_12",
                                    "DISTCBD_13","DISTF_14",
                                    "DISTMTR_15","DISTBUS_16",
                                    "PERCENTARI","E0_14PERC",
                                    "E15_64PERC",
                                    "E65_100PER","DENSITATPO",
                                    "NV_1PERC","NV_5PERC",
                                    "PERCENTARC","PERCENTARS",
                                    "DIST_CAPME","DIST_DROGO", 
                                    "DIST_UNI","DIST_AJUNT",
                                    "DIST_CEPRI","DIST_CEPUB",
                                    "DIST_PQPI"))
tableaCM <- subset(table,colsCM)
CM <- cor(tableaCM, method = c("pearson", "kendall", "spearman"))
round(CM,2)

# PLOT CORRELATION MATRIX
#install.packages("corrplot")
library(corrplot)
corrplot(CM, type = "upper", order = "hclust", tl.cex=0.5,
         tl.col = "black", tl.srt = 45)

## VISUALISE BASIC STATISTICS
# PLOT SUMMARY TABLE
summary(table)
# # PLOT HISTOGRAMS
hist(table$RBP2019)
hist(table$DENSITATPO)
hist(table$E_MITJANA)
hist(table$CAPCOM)
hist(table$LITORAL)
hist(table$DIST_APIAV)
hist(table$DISTF_14)
hist(table$DISTCBD_13)
hist(table$DISTMTR_15)
hist(table$DISTBUS_16)
hist(table$PERCENTARI)
hist(table$PERCENTARS)
hist(table$PERCENTARC)
hist(table$NV_1PERC)
hist(table$NV_5PERC)

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
sd(table$PERCENTARI)
sd(table$PERCENTARS)
sd(table$PERCENTARC)
sd(table$NV_1PERC)
sd(table$NV_5PERC)

# TABLE SUBDIVISION table 2 AMB/ table3 NO AMB (! EXCLOU VALORS)
table2 = table[!table$AMB==0,] 
table3 = table[!table$AMB==1,]

# OLS REGRESSION MODEL COMPLET
#install.packages("tidyverse")
library(tidyverse) 
model1a <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
              DISTMTR_15 + DISTBUS_16 + A_SPRWL_M2 + A_INDUS_M2 + A_CMPCT_M2 + NV_1PERC + NV_5PERC + AMB , data = table)
summary(model1a)

# OLS REGRESSION WITH % CLC MODEL COMPLET
library(tidyverse) 
model1b <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
              DISTMTR_15 + DISTBUS_16 + PERCENTARI + PERCENTARS + PERCENTARC + NV_1PERC + NV_5PERC + AMB , data = table)
summary(model1b)

# OLS REGRESSION MODEL NO AMB
#install.packages("tidyverse")
library(tidyverse) 
model2a <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
              DISTMTR_15 + DISTBUS_16 + A_SPRWL_M2 + A_INDUS_M2 + A_CMPCT_M2 + NV_1PERC + NV_5PERC , data = table3)
summary(model2a)

# OLS REGRESSION WITH % CLC MODEL NO AMB
library(tidyverse) 
model2b <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
              DISTMTR_15 + DISTBUS_16 + PERCENTARI + PERCENTARS + PERCENTARC + NV_1PERC + NV_5PERC, data = table3)
summary(model2b)

# OLS REGRESSION MODEL AMB
#install.packages("tidyverse")
library(tidyverse) 
model3a <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
              DISTMTR_15 + DISTBUS_16 + A_SPRWL_M2 + A_INDUS_M2 + A_CMPCT_M2 + NV_1PERC + NV_5PERC , data = table2)
summary(model3a)

# OLS REGRESSION WITH % CLC MODEL AMB
library(tidyverse) 
model3b <- lm(RBP2019 ~ DENSITATPO + E_MITJANA + CAPCOM + LITORAL + DIST_APIAV +  DISTF_14 + DISTCBD_13 +
              DISTMTR_15 + DISTBUS_16 + PERCENTARI + PERCENTARS + PERCENTARC + NV_1PERC + NV_5PERC, data = table2)
summary(model3b)

#SCATTERPLOT (proves)
sns.lmplot(x="FlyAsh", y="Strength", hue="AirEntrain", data=con);
sns.lmplot(x="FlyAsh", y="Strength", data=con);

