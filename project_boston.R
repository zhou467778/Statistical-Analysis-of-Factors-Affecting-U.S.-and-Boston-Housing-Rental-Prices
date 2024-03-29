## Data for Boston only
data <- read.csv("/Users/apple/Desktop/MATH7343 Applied Statistics/final project/related files/housing boston.csv")

## calculate mean and standard deviation for all prices
meanPriceAll <- mean(data$price) #2411.648, point estimator
sdPriceAll <- sd(data$price) #878.4851
alpha <- 0.05
meanPriceAll + c(-1,1) * qt(1-alpha/2, df = length(data$price) - 1) * sdPriceAll/sqrt(length(data$price)) 
#-------------------------------------------------------------------------------------------------------
## calculate mean and standard deviation for each type of housing, and 95% C.I(t-interval)
apartment <- data[data$type == 'apartment',]
meanApart <- mean(apartment$price) #2369.043
sdApart <- sd(apartment$price) #837.2677
meanApart + c(-1,1) * qt(1-alpha/2,df = nrow(apartment)-1) * sdApart/sqrt(nrow(apartment)) #(2323.221 2414.864)

condo <- data[data$type == 'condo',]
meanCon<-mean(condo$price) #2556.846
sdCon<-sd(condo$price) # 835.7147
meanCon + c(-1,1) * qt(1-alpha/2,df=nrow(condo)-1) * sdCon/sqrt(nrow(condo)) #(2368.422 2745.271)

duplex <- data[data$type == 'duplex',]
meanDu<-mean(duplex $price) #3037.5
sdDu<-sd(duplex$price) #1809.449
meanDu+ c(-1,1) * qt(1-alpha/2,df=nrow(duplex)-1) * sdDu/sqrt(nrow(duplex)) #(1524.762 4550.238)

flat <- data[data$type == 'flat',]
meanFlat<-mean(flat$price) #2376.429
sdFlat<-sd(flat$price) #364.4026
meanFlat+ c(-1,1) * qt(1-alpha/2,df=nrow(flat)-1) * sdFlat/sqrt(nrow(flat)) #(2039.412 2713.445)

house <- data[data$type == 'house',]
meanHouse<-mean(house$price) #2843.434
meanHouse
sdHouse<-sd(house$price)
sdHouse #1258.461
meanHouse+ c(-1,1) * qt(1-alpha/2,df=nrow(house)-1) * sdHouse/sqrt(nrow(house)) #(2568.641, 3118.226)

loft <- data[data$type == 'loft',]
meanLoft<-mean(loft$price) 
meanLoft #3200
sdLoft<-sd(loft$price)
sdLoft #0
meanLoft+ c(-1,1) * qt(1-alpha/2,df=nrow(loft)-1) * sdLoft/sqrt(nrow(loft)) #(3200, 3200)

townhouse <- data[data$type == 'townhouse',]
meanTown<-mean(townhouse$price)
meanTown #2500.353
sdTown<-sd(townhouse$price)
sdTown #816.4448
meanTown+ c(-1,1) * qt(1-alpha/2,nrow(townhouse)-1) * sdTown/sqrt(nrow(townhouse)) #(2080.576 ,2920.130)

#--------------------------------------------------------------------------
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
#---------------------------------------------------------------------------------------
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

## Depending on full_weight_model, remove electric_vehicle_charge1 since its p value 0.62 is biggest, adjusted R^2 increases to 0.3465
step.model1 <- lm(price ~ sqfeet + beds + baths + cats_allowed + dogs_allowed + smoking_allowed + wheelchair_access + comes_furnished, data = data)
summary(step.model1)

## Depending on step.model1, remove dogs_allowed1 since its p value 0.19 is biggest, adjusted R^2 decreased to 0.3462
step.model2 <- lm(price ~ sqfeet + beds + baths + cats_allowed + smoking_allowed + wheelchair_access + comes_furnished, data = data)
summary(step.model2)
plot(step.model2, which=1)
plot(step.model2, which=2)
