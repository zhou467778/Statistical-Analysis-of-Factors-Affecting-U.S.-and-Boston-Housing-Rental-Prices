install.packages("readxl")
library("readxl")
data <- read_excel("/Users/apple/Desktop/MATH7343 Applied Statistics/final project/related files/housing clean.xlsx")

## calculate mean and standard deviation for all prices
meanPriceAll <- mean(data$price) #1192.198 point estimator
meanPriceAll
sdPriceAll <- sd(data$price) #592.9392
sdPriceAll 
alpha <- 0.05
meanPriceAll + c(-1,1) * qt(1-alpha/2, df = length(data$price) - 1) * sdPriceAll/sqrt(length(data$price)) 
#-------------------------------------------------------------------------------------------------------
## calculate mean and standard deviation for each type of housing, and 95% C.I(t-interval)
apartment <- data[data$type == 'apartment',]
meanApart <- mean(apartment$price)
meanApart #1160.792
sdApart <- sd(apartment$price)
sdApart #536.9006
meanApart + c(-1,1) * qt(1-alpha/2,df = nrow(apartment)-1) * sdApart/sqrt(nrow(apartment)) #(1158.877, 1162.707)

assisted_living <- data[data$type == 'assisted living',]
meanAssi <- mean(assisted_living$price)
meanAssi #1787.5
sdAssi <- sd(assisted_living$price)
sdAssi #2280.419
meanAssi + c(-1,1) * qt(1-alpha/2,df=nrow(assisted_living)-1) * sdAssi/sqrt(nrow(assisted_living)) #(-18701.26,22276.26)

condo <- data[data$type == 'condo',]
meanCon<-mean(condo$price) #1595.137
meanCon
sdCon<-sd(condo$price) 
sdCon #854.7913
meanCon + c(-1,1) * qt(1-alpha/2,df=nrow(condo)-1) * sdCon/sqrt(nrow(condo)) #(1573.513, 1616.762)

cottage <- data[data$type == 'cottage/cabin',]
meanCot <- mean(cottage$price)
meanCot #1279.6
sdCot <-sd(cottage$price) 
sdCot #707.5114
meanCot+ c(-1,1) * qt(1-alpha/2,df=nrow(cottage)-1) * sdCot/sqrt(nrow(cottage)) #(1225.441, 1333.759)

duplex <- data[data$type == 'duplex',]
meanDu<-mean(duplex $price)
meanDu #1230.995
sdDu<-sd(duplex$price) 
sdDu # 603.004
meanDu+ c(-1,1) * qt(1-alpha/2,df=nrow(duplex)-1) * sdDu/sqrt(nrow(duplex)) #(1214.172 ,1247.818)

flat <- data[data$type == 'flat',]
meanFlat<-mean(flat$price) #1597.846
meanFlat
sdFlat<-sd(flat$price) #860.9716
sdFlat
meanFlat+ c(-1,1) * qt(1-alpha/2,df=nrow(flat)-1) * sdFlat/sqrt(nrow(flat)) #(1522.648, 1673.043)

house <- data[data$type == 'house',]
meanHouse<-mean(house$price) #1380.349
meanHouse
sdHouse<-sd(house$price)
sdHouse #887.0839
meanHouse+ c(-1,1) * qt(1-alpha/2,df=nrow(house)-1) * sdHouse/sqrt(nrow(house)) #(1370.718 ,1389.980)

inLaw <- data[data$type == 'in-law',]
meanLaw<-mean(inLaw$price) #1330.08
sdLaw<-sd(inLaw$price) #497.3621
alpha <- 0.05
meanLaw+ c(-1,1) * qt(1-alpha/2,df=nrow(inLaw)-1) * sdLaw/sqrt(nrow(inLaw)) #(1246.049, 1414.112)

land <- data[data$type == 'land',]
meanLand<-mean(land$price) #530
sdLand<-sd(land$price) #144.0486
meanLand+ c(-1,1) * qt(1-alpha/2,df=nrow(land)-1) * sdLand/sqrt(nrow(land)) #(351.14, 708.86)

loft <- data[data$type == 'loft',]
meanLoft<-mean(loft$price) #1376.787
sdLoft<-sd(loft$price) #700.3527
meanLoft+ c(-1,1) * qt(1-alpha/2,df=nrow(loft)-1) * sdLoft/sqrt(nrow(loft)) #(1321.506 ,1432.067)

manu <- data[data$type == 'manufactured',]
meanManu<-mean(manu$price) #917.2199
sdManu<-sd(manu$price) #345.4665
meanManu+ c(-1,1) * qt(1-alpha/2,df=nrow(manu)-1) * sdManu/sqrt(nrow(manu)) #(906.6564, 927.7834)

townhouse <- data[data$type == 'townhouse',]
meanTown<-mean(townhouse$price) #1286.374
sdTown<-sd(townhouse$price) #602.8269
meanTown+ c(-1,1) * qt(1-alpha/2,nrow(townhouse)-1) * sdTown/sqrt(nrow(townhouse)) #(1276.940, 1295.808)

#--------------------------------------------------------------------
# Compare the true mean between different housing types

# condo and flat:
t.test(condo$price, flat$price)

# cottage and townhouse
t.test(cottage$price, townhouse$price)

# house and loft
t.test(house$price, loft$price)
# --------------------------------------------------------------------------
# Compare the true mean between different housing types

# cottage, duplex, townhouse:
df1 <- rbind(cottage, duplex, townhouse)
df1$type <- as.factor(df1$type)
df1.fit <- aov(price~type, data = df1)
summary(df1.fit)
TukeyHSD(df1.fit, conf.level = 0.95)

# house, inLaw, loft:
df2 <- rbind(house, inLaw, loft)
df2$type <- as.factor(df2$type)
df2.fit <- aov(price~type, data = df2)
summary(df2.fit)
#--------------------------------------------------------------------
## Calculate correlation between all numerical variables and price

#sqfeet and prices
cor.test(data$sqfeet, data$price, method = 'spearman') 

#beds and prices
cor.test(data$beds, data$price, method = 'spearman') 

# baths and prices
cor.test(data$baths, data$price, method = 'spearman') 

#cats_allowed and price
cor.test(data$cats_allowed, data$price, method = 'spearman')

#dogs_allowed and price
cor.test(data$dogs_allowed, data$price,method = 'spearman')

#smoking allowed and price
cor.test(data$smoking_allowed, data$price,method = 'spearman')

# wheelchair_access and price
cor.test(data$wheelchair_access, data$price,method = 'spearman')

# electric_vehicle_charge and price
cor.test(data$electric_vehicle_charge, data$price,method = 'spearman')

#comes_furnished and price
cor.test(data$comes_furnished, data$price,method = 'spearman')
#-----------------------------------------------------------------------------
## Multiple Linear Regression
data$cats_allowed <- as.factor(data$cats_allowed)
data$dogs_allowed <- as.factor(data$dogs_allowed)
data$smoking_allowed <- as.factor(data$smoking_allowed)
data$wheelchair_access <- as.factor(data$wheelchair_access)
data$electric_vehicle_charge <- as.factor(data$electric_vehicle_charge)
data$comes_furnished <- as.factor(data$comes_furnished)

full_weight_model <- lm(price ~ sqfeet + beds + baths + cats_allowed
                        + dogs_allowed + smoking_allowed + wheelchair_access + electric_vehicle_charge
                        + comes_furnished, data = data)

summary(full_weight_model)

## Depending on full_weight_model, remove cats_allowed1 since its p value 0.81 is biggest, adjusted R^2 remains unchanged
step.model1 <- lm(price ~ sqfeet + beds + baths + dogs_allowed + smoking_allowed + wheelchair_access + electric_vehicle_charge
                  + comes_furnished, data = data)
summary(step.model1)
plot(step.model1, which=1)
plot(step.model1, which=2)

