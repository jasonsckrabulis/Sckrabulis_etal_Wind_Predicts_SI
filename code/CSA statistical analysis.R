# CSA lifeguard statisical analysis

# Libraries
library(car)	# Anova() function
library(lme4)	# glmer() function
library(PropCIs)	# exactci() function
library(suncalc)	# getSunlightTimes() function
library(chron)	# times() function

# Datasets

# Datasets limited by temporal data

# No time of day effect dataset, 1 datapoint per day (use 'CSA data - among day variation.csv')
Lifeguard <- read.csv(file.choose(), header=TRUE)

# Remove days with zero swimmers & datapoints with missing temporal predictor values
NoZero <- subset(Lifeguard, SwimTotal!=0 & WindDir!="" & WaterTempC!="NA" & AvgPrevTemp7!="NA" & PrevSi7!="NA")

# Make SI and Swimmer counts equal when total SI count is > swimmer count
for(i in 1:length(NoZero$SwimTotal)){
	if (NoZero$SiTotal[i] > NoZero$SwimTotal[i]){
		NoZero$SwimTotal[i] <- NoZero$SiTotal[i]
	}	
}

# Time of day effect dataset, 2 datapoints per day (use 'CSA data - within day variation.csv')
LifeguardTod <- read.csv(file.choose(), header=TRUE)

#Remove days with zero swimmers & datapoints with missing temporal predictor values
NoZeroTime <- subset(LifeguardTod, SwimNum!=0 & WindDir!="" & WaterTempC!="NA" & AvgPrevTemp7!="NA" & PrevSi7!="NA")

# Make SI and Swimmer counts equal when total SI count is > swimmer count
for(i in 1:length(NoZeroTime$SwimTotal)){
	if (NoZeroTime$SiTotal[i] > NoZeroTime$SwimTotal[i]){
		NoZeroTime$SwimTotal[i] <- NoZeroTime$SiTotal[i]
	}	
}

# Datasets not limited by temporal data

# No time of day effect dataset, 1 datapoint per day (a different subset of the full 'Lifeguard' data frame)
# Remove days with swimmers & datapoints with missing relevant values
LifeguardFull <- subset(Lifeguard, SwimTotal!=0 & WindDir!="")

# Make SI and Swimmer counts equal when total SI count is > swimmer count
for(i in 1:length(LifeguardFull$SwimTotal)){
	if (LifeguardFull$SiTotal[i] > LifeguardFull$SwimTotal[i]){
		LifeguardFull$SwimTotal[i] <- LifeguardFull$SiTotal[i]
	}
}

# Paired wind directions for multiple comparisons
NoZeroNNE <- subset(LifeguardFull, WindDir=="N" | WindDir=="NE")
NoZeroNEE <- subset(LifeguardFull, WindDir=="NE" | WindDir=="E")
NoZeroESE <- subset(LifeguardFull, WindDir=="E" | WindDir=="SE")
NoZeroSES <- subset(LifeguardFull, WindDir=="SE" | WindDir=="S")
NoZeroSSW <- subset(LifeguardFull, WindDir=="S" | WindDir=="SW")
NoZeroSWW <- subset(LifeguardFull, WindDir=="SW" | WindDir=="W")
NoZeroWNW <- subset(LifeguardFull, WindDir=="W" | WindDir=="NW")
NoZeroNWN <- subset(LifeguardFull, WindDir=="NW" | WindDir=="N")

# Wind speed effects by each direction
NoZeroN <- subset(LifeguardFull, WindDir=="N")
NoZeroNE <- subset(LifeguardFull, WindDir=="NE")
NoZeroE <- subset(LifeguardFull, WindDir=="E")
NoZeroSE <- subset(LifeguardFull, WindDir=="SE")
NoZeroS <- subset(LifeguardFull, WindDir=="S")
NoZeroSW <- subset(LifeguardFull, WindDir=="SW")
NoZeroW <- subset(LifeguardFull, WindDir=="W")
NoZeroNW <- subset(LifeguardFull, WindDir=="NW")

# Time of day effect dataset, 2 datapoints per day (a different subset of the full 'LifeguardTod' data frame)
LifeguardTodFull <- subset(LifeguardTod, SwimTotal!=0 & WindDir!="")

# Make SI and Swimmer counts equal when total SI count is > swimmer count
for(i in 1:length(LifeguardTodFull$SwimTotal)){
	if (LifeguardTodFull$SiTotal[i] > LifeguardTodFull$SwimTotal[i]){
		LifeguardTodFull$SwimTotal[i] <- LifeguardTodFull$SiTotal[i]
	}	
}

# Data for confidence interval calculation
conf <- subset(LifeguardFull, select=c("Date", "SiTotal", "WindDir", "WindVel", "SwimTotal"))

#_
# Models
# No time of day effect, 1 datapoint per day
# Forward selection using glmer()

#S tep 1 (Best model: model2 w/ WindDirGroup1)
model1 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WaterTempC+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model1)
model2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model2)
model3 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup2+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model3)
model4 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~PrevSi1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model4)
model5 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~PrevSi3+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model5)
model6 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~PrevSi5+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model6)
model7 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~PrevSi7+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model7)
model8 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~AvgPrevTemp1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model8)
model9 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~AvgPrevTemp3+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model9)
model10 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~AvgPrevTemp5+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model10)
model11 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~AvgPrevTemp7+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model11)
model12 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model12)
model13 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model13)
model14 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~NWSprecip1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model14)
model15 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~NWSprecip2+(1|CalDay), family="binomial",data=NoZero, na.action=na.omit)
Anova(model15)

# Step 2 (Best model: model10 w/ WindDirGroup1+WindVel)
model1 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WaterTempC+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model1)
model2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+PrevSi1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model2)
model3 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+PrevSi3+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model3)
model4 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+PrevSi5+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model4)
model5 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+PrevSi7+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model5)
model6 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+AvgPrevTemp1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model6)
model7 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+AvgPrevTemp3+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model7)
model8 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+AvgPrevTemp5+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model8) # warnings
# Check AvgPrevTemp5 effect with residuals and asin+sqrt transform lm (not significant)
model8a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model8a)
plot(NoZero$AvgPrevTemp5, residuals(model8))
model8b <- lm(residuals(model8a)~NoZero$AvgPrevTemp5)
Anova(model8b)
model8asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+AvgPrevTemp5, data=NoZero, na.action=na.omit)
Anova(model8asin)
model9 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+AvgPrevTemp7+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model9)
model10 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model10)
model11 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model11)
model12 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+NWSprecip1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model12)
model13 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+NWSprecip2+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model13)

# Step 3 (Best model: model 11 w/ WindDirGroup1+WindVel+SwimTotal)
model1 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+WaterTempC+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model1)
model2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+PrevSi1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model2)
model3 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+PrevSi3+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model3)
model4 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+PrevSi5+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model4)
model5 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+PrevSi7+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model5)
model6 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+AvgPrevTemp1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model6)
model7 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+AvgPrevTemp3+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model7) # warnings
# Check AvgPrevTemp3 effect with residuals and asin+sqrt transform lm (not significant)
model7a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+(1|CalDay), family="binomial", data=NoZero)
Anova(model7a)
plot(NoZero$AvgPrevTemp3, residuals(model7a))
model7b <- lm(residuals(model7a)~NoZero$AvgPrevTemp3)
Anova(model7b)
model7asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+AvgPrevTemp3, data=NoZero, na.action=na.omit)
Anova(model7asin)
model8 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+AvgPrevTemp5+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model8) # warnings
# Check AvgPrevTemp5 effect with residuals and asin+sqrt transform lm (not significant)
model8a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+(1|CalDay), family="binomial", data=NoZero)
Anova(model8a)
plot(NoZero$AvgPrevTemp5, residuals(model8a))
model8b <- lm(residuals(model8a)~NoZero$AvgPrevTemp5)
Anova(model8b)
model8asin <-lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+AvgPrevTemp5, data=NoZero, na.action=na.omit)
Anova(model8asin)
model9 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+AvgPrevTemp7+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model9) # warnings
# Check AvgPrevTemp7 effect with residuals and asin+sqrt transform lm (not significant)
model9a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+(1|CalDay), family="binomial", data=NoZero)
Anova(model9a)
plot(NoZero$AvgPrevTemp7, residuals(model9a))
model9b <- lm(residuals(model9a)~NoZero$AvgPrevTemp7)
Anova(model9b)
model9asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+AvgPrevTemp7, data=NoZero, na.action=na.omit)
Anova(model9asin)
model10 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1*WindVel+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model10) # warnings
# Check WindDirGroup1 x WindVel interaction effect with asin+sqrt transform lm (significant)
model10asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1*WindVel, data=NoZero, na.action=na.omit)
Anova(model10asin)
model11 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+(1|CalDay),family="binomial",data=NoZero,na.action=na.omit)
Anova(model11)
model12 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+NWSprecip1+(1|CalDay),family="binomial",data=NoZero,na.action=na.omit)
Anova(model12)
model13 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+NWSprecip2+(1|CalDay),family="binomial",data=NoZero,na.action=na.omit)
Anova(model13)

# Step 4 (no significant predictors added)
model1 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+WaterTempC+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model1) # warnings
# Check WaterTempC effect with residuals and asin+sqrt transform lm (not significant)
model1a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model1a)
plot(NoZero$WaterTempC, residuals(model1a))
model1b <- lm(residuals(model1a)~NoZero$WaterTempC)
model1asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+SwimTotal+WaterTempC, data=NoZero, na.action=na.omit)
Anova(model1asin)
model2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+PrevSi1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model2)
model3 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+PrevSi3+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model3)
model4 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+PrevSi5+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model4)
model5 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+PrevSi7+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model5)
model6 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+AvgPrevTemp1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model6) # warnings
# Check AvgPrevTemp1 with residuals and asin+sqrt transform lm (not significant)
model6a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model6a)
plot(NoZero$AvgPrevTemp1, residuals(model6a))
model6b <- lm(residuals(model6a)~NoZero$AvgPrevTemp1)
model6asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+SwimTotal+AvgPrevTemp1, data=NoZero, na.action=na.omit)
Anova(model6asin)
model7 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+AvgPrevTemp3+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model7) # warnings
# Check AvgPrevTemp3 effect with residuals and asin+sqrt transform lm (not significant)
model7a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model7a)
plot(NoZero$AvgPrevTemp3, residuals(model7a))
model7b <- lm(residuals(model7a)~NoZero$AvgPrevTemp3)
model7asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+SwimTotal+AvgPrevTemp3, data=NoZero, na.action=na.omit)
Anova(model7asin)
model8 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+AvgPrevTemp5+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model8) # warnings
# Check AvgPrevTemp5 effect with residuals and asin+sqrt transform lm (not significant)
model8a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model8a)
plot(NoZero$AvgPrevTemp5, residuals(model8a))
model8b <- lm(residuals(model8a)~NoZero$AvgPrevTemp5)
modell8asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+SwimTotal+AvgPrevTemp5, data=NoZero, na.action=na.omit)
Anova(model8asin)
model9 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+AvgPrevTemp7+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model9) # warnings
# Check AvgPrevTemp7 effect with residuals and asin+sqrt transform lm (not significant)
model9a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model9a)
plot(NoZero$AvgPrevTemp7, residuals(model9a))
model9b <- lm(residuals(model9a)~NoZero$AvgPrevTemp7)
model9asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+SwimTotal+AvgPrevTemp7, data=NoZero, na.action=na.omit)
Anova(model9asin)
model10 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1*WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model10) # warnings
#C heck WindDirGroup1*WindVel interaction effect with asin+sqrt transform lm (significant)
model10asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1*WindVel+SwimTotal, data=NoZero, na.action=na.omit)
Anova(model10asin)
model11 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+NWSprecip1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model11) # warnings
# Check NWSprecip1 effect with residuals and asin+sqrt transform lm (not significant)
model11a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model11a)
model11b <- lm(residuals(model11a)~NoZero$NWSprecip1)
model11asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+SwimTotal+NWSprecip1, data=NoZero, na.action=na.omit)
Anova(model11asin)
model12 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+NWSprecip2+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model12)

#_
# Final model for daily SI risk (1 datapoint per day)
modelFull <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(modelFull)
summary(modelFull)
modelDir <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(modelDir)
summary(modelDir)
modelVel <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZero,na.action=na.omit)
Anova(modelVel)
summary(modelVel)
modelSwim <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(modelSwim)
summary(modelSwim)
modelNull <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(modelNull)
summary(modelNull)

# R2 from Naglekerkle 1991 using log-likelihood ratios
totalCount <- sum(NoZero$SiTotal)+sum(NoZero$SwimTotal-NoZero$SiTotal)
r2Full <- as.numeric((1-exp(-2/totalCount*(logLik(modelFull)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Dir <- as.numeric((1-exp(-2/totalCount*(logLik(modelDir)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Vel <- as.numeric((1-exp(-2/totalCount*(logLik(modelVel)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Swim <- as.numeric((1-exp(-2/totalCount*(logLik(modelSwim)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))

# Full model with WindDirGroup2 instead of WindDirGroup1
model10g2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup2+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model10g2)
summary(model10g2)

# Final model with all wind directions included in WindDir variable
model10all <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZero, na.action=na.omit)
Anova(model10all) # warnings
model10all <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir+WindVel+SwimTotal, data=NoZero, na.action=na.omit)
Anova(model10all)

#_
# Final model with all days included (1 datapoint per day)
modelFull <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+WindVel+SwimTotal+(1|CalDay), family="binomial", data=LifeguardFull, na.action=na.omit)
Anova(modelFull)
summary(modelFull)
modelFullasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1+WindVel+SwimTotal, data=LifeguardFull, na.action=na.omit)
Anova(modelFullasin)
modelDir <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+(1|CalDay), family="binomial", data=LifeguardFull, na.action=na.omit)
Anova(modelDir)
summary(modelDir)
modelDirasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1, data=LifeguardFull, na.action=na.omit)
Anova(modelDirasin)
modelVel <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=LifeguardFull, na.action=na.omit)
Anova(modelVel)
summary(modelVel)
modelVelasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=LifeguardFull, na.action=na.omit)
Anova(modelVelasin)
modelSwim <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~SwimTotal+(1|CalDay), family="binomial", data=LifeguardFull, na.action=na.omit)
Anova(modelSwim)
summary(modelSwim)
modelSwimasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~SwimTotal, data=LifeguardFull, na.action=na.omit)
Anova(modelSwimasin)
modelNull <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~(1|CalDay), family="binomial", data=LifeguardFull, na.action=na.omit)
Anova(modelNull)
summary(modelNull)

#_
# R2 from Naglekerkle 1991 using log-likelihood ratios
totalCount <- sum(LifeguardFull$SiTotal)+sum(LifeguardFull$SwimTotal-LifeguardFull$SiTotal)
r2Full <- as.numeric((1-exp(-2/totalCount*(logLik(modelFull)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Dir <- as.numeric((1-exp(-2/totalCount*(logLik(modelDir)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Vel <- as.numeric((1-exp(-2/totalCount*(logLik(modelVel)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Swim <- as.numeric((1-exp(-2/totalCount*(logLik(modelSwim)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))


# R2 for asinsqrt models
r2Fullasin <- summary(modelFullasin)$r.squared
r2Dirasin <- summary(modelDirasin)$r.squared
r2Velasin <- summary(modelVelasin)$r.squared
r2Swimasin <- summary(modelSwimasin)$r.squared

# Full model with WindDirGroup2 instead of WindDirGroup1
model10g2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup2+WindVel+SwimTotal+(1|CalDay), family="binomial", data=LifeguardFull, na.action=na.omit)
Anova(model10g2)
summary(model10g2)

# Final model with all wind directions included in WindDir variable
model10all <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+WindVel+SwimTotal+(1|CalDay), family="binomial", data=LifeguardFull, na.action=na.omit)
Anova(model10all) # warnings
model10all <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir+WindVel+SwimTotal, data=LifeguardFull, na.action=na.omit)
Anova(model10all)

#_
# Multiple comparisons (one datapoint per day)
# Subsets of paired directions to test for effects of direction

NNE <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+(1|CalDay), family="binomial", data=NoZeroNNE, na.action=na.omit) # significant
Anova(NNE)
NNEasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir, data=NoZeroNNE, na.action=na.omit) # significant
Anova(NNEasin)

NEE <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+(1|CalDay), family="binomial", data=NoZeroNEE, na.action=na.omit) # significant
Anova(NEE)
NEEasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir, data=NoZeroNEE, na.action=na.omit) # significant
Anova(NEE)

ESE <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+(1|CalDay), family="binomial", data=NoZeroESE, na.action=na.omit) # not sigificant
Anova(ESE)
ESEasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir, data=NoZeroESE, na.action=na.omit) # not significant
Anova(ESE)

SES <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+(1|CalDay), family="binomial", data=NoZeroSES, na.action=na.omit) # warnings
Anova(SES)
SESasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir, data=NoZeroSES, na.action=na.omit) # not significant
Anova(SES)

SSW <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+(1|CalDay), family="binomial", data=NoZeroSSW, na.action=na.omit) # warnings
Anova(SSW)
SSWasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir, data=NoZeroSSW, na.action=na.omit) # not significant
Anova(SSW)

SWW <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+(1|CalDay), family="binomial", data=NoZeroSWW, na.action=na.omit) # not significant
Anova(SWW)
SWWasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir, data=NoZeroSWW, na.action=na.omit) # not significant
Anova(SWW)

WNW <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+(1|CalDay), family="binomial", data=NoZeroWNW, na.action=na.omit) # significant
Anova(WNW)
WNWasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir, data=NoZeroWNW, na.action=na.omit) # significant
Anova(WNW)

NWN <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDir+(1|CalDay), family="binomial", data=NoZeroNWN, na.action=na.omit) # not significant
Anova(NWN)
NWNasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDir, data=NoZeroNWN, na.action=na.omit) # not significant
Anova(NWN)

# P values of each wind direction effect
# Binomial models
pvalues_binomial <- c((summary(NNE)$coef[2,4]),(summary(NEE)$coef[2,4]),(summary(ESE)$coef[2,4]),
	(summary(SES)$coef[2,4]),(summary(SSW)$coef[2,4]),(summary(SWW)$coef[2,4]),(summary(WNW)$coef[2,4]),(summary(NWN)$coef[2,4]))
# [1] 1.097701e-04 1.735662e-07 5.596078e-01 9.756343e-01 9.057330e-01 8.782898e-01 1.246812e-02 6.935914e-01
pvalues_binomial_adjust<-p.adjust(pvalues_binomial,method="BH")
# [1] 0.0105472080 0.0000226189 0.8674859200 0.9999842000 0.9999842000 0.9999842000 0.1234272000 0.8674859200

# Asin+sqrt models
pvalues_asinsqrt <- c((summary(NNEasin)$coef[2,4]),(summary(NEEasin)$coef[2,4]),(summary(ESEasin)$coef[2,4]),
	(summary(SESasin)$coef[2,4]),(summary(SSWasin)$coef[2,4]),(summary(SWWasin)$coef[2,4]),(summary(WNWasin)$coef[2,4]),(summary(NWNasin)$coef[2,4]))
# [1] 3.945784e-04 3.125614e-05 1.991537e-01 1.458891e-01 1.991537e-01 6.095318e-01 2.913011e-02 8.359805e-01
pvalues_asinsqrt_adjust<-p.adjust(pvalues_asinsqrt,method="BH")
# [1] 0.0015783134 0.0002500491 0.2655382014 0.2655382014 0.2655382014 0.6966077830 0.0776802977 0.8359805237

#_
# Wind speed effects with each direction, not limited by temporal data

N <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroN, na.action=na.omit) # warnings
Anova(N)
Nasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=NoZeroN, na.action=na.omit) # significant
Anova(Nasin)

NE <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroNE, na.action=na.omit) # warnings
Anova(NE)
NEasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=NoZeroNE, na.action=na.omit) # not significant
Anova(NEasin)

E <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroE, na.action=na.omit)
Anova(E)
Easin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=NoZeroE, na.action=na.omit) # not significant
Anova(Easin)

SE <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroSE, na.action=na.omit)
Anova(SE)
SEasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=NoZeroSE, na.action=na.omit) # not significant
Anova(SEasin)

S <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroS, na.action=na.omit) # converge failure
Anova(S)
Sasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=NoZeroS, na.action=na.omit)
Anova(Sasin) # residual SS = 0

SW <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroSW, na.action=na.omit)
Anova(SW)
SWasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=NoZeroSW, na.action=na.omit) # not significant
Anova(SWasin)

W <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroW, na.action=na.omit)
Anova(W)
Wasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=NoZeroW, na.action=na.omit) # not significant
Anova(Wasin)

NW <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroNW, na.action=na.omit)
Anova(NW)
NWasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=NoZeroNW, na.action=na.omit) # not significant
Anova(NWasin)

#_
# SI cases vs wind speed: 1 panel, N, NE, E with restricted lines to available data

#lm models
lmE <- lm(I(SiTotal/SwimTotal)~WindVel, data=NoZeroE)
lmN <- lm(I(SiTotal/SwimTotal)~WindVel, data=NoZeroN)
lmNE <- lm(I(SiTotal/SwimTotal)~WindVel, data=NoZeroNE)
lmNW <- lm(I(SiTotal/SwimTotal)~WindVel, data=NoZeroNW)
lmS <- lm(I(SiTotal/SwimTotal)~WindVel, data=NoZeroS)
lmSE <- lm(I(SiTotal/SwimTotal)~WindVel, data=NoZeroSE)
lmSW <- lm(I(SiTotal/SwimTotal)~WindVel, data=NoZeroSW)
lmW <- lm(I(SiTotal/SwimTotal)~WindVel, data=NoZeroW)

# lm plot
# NE solid circle
plot(NoZeroNE$WindVel, NoZeroNE$SiTotal/NoZeroNE$SwimTotal, pch=16, xlim=c(0,20), ylim=c(-.1,1), ylab="Proportion SI cases", xlab="Wind speed (mph)")
segments(min(NoZeroNE$WindVel),as.numeric(coef(lmNE)[1])+(as.numeric(coef(lmNE)[2])*min(NoZeroNE$WindVel)),max(NoZeroNE$WindVel),as.numeric(coef(lmNE)[1])+(as.numeric(coef(lmNE)[2])*max(NoZeroNE$WindVel)))

# N open circle dashed
points(NoZeroN$WindVel, NoZeroN$SiTotal/NoZeroN$SwimTotal,pch=1)
segments(min(NoZeroN$WindVel),as.numeric(coef(lmN)[1])+(as.numeric(coef(lmN)[2])*min(NoZeroN$WindVel)),max(NoZeroN$WindVel),as.numeric(coef(lmN)[1])+(as.numeric(coef(lmN)[2])*max(NoZeroN$WindVel)),lty=2)

# E open square dotted
points(NoZeroE$WindVel, NoZeroE$SiTotal/NoZeroE$SwimTotal,pch=4)
segments(min(NoZeroE$WindVel),as.numeric(coef(lmE)[1])+(as.numeric(coef(lmE)[2])*min(NoZeroE$WindVel)),max(NoZeroE$WindVel),as.numeric(coef(lmE)[1])+(as.numeric(coef(lmE)[2])*max(NoZeroE$WindVel)),lty=3)

#_
# Models
# Time of day effect, 2 datapoints per day
# Forward selection using glmer()

# Step 1 (Best model: model13 w/ TimeOfDay)
model1 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WaterTempC+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model1)
model2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model2)
model3 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup2+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model3)
model4 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~PrevSi1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model4) # warnings
# Check PrevSi1 effect with residuals and asin+sqrt transform lm (not significant)
model4a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~(1|CalDay), family="binomial", data=NoZeroTime)
plot(NoZeroTime$PrevSi1, residuals(model4a))
model4b <- lm(residuals(model4a)~NoZeroTime$PrevSi1)
Anova(model4b)
model4asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~PrevSi1, data=NoZeroTime, na.action=na.omit)
Anova(model4asin)
model5 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~PrevSi3+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model5)
model6 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~PrevSi5+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model6)
model7 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~PrevSi7+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model7)
model8 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~AvgPrevTemp1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model8)
model9 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~AvgPrevTemp3+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model9)
model10 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~AvgPrevTemp5+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model10)
model11 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~AvgPrevTemp7+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model11)
model12 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model12)
model13 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model13)
model14 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~SwimTotal+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model14)
model15 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~NWSprecip1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model15)
model16 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~NWSprecip2+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model16)

# Step 2 (Best model: model2 w/ TimeOfDay+WindDirGroup1)
model1 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WaterTempC+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model1)
model2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model2)
model3 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup2+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model3)
model4 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+PrevSi1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model4)
model5 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+PrevSi3+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model5)
model6 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+PrevSi5+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model6)
model7 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+PrevSi7+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model7)
model8 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+AvgPrevTemp1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model8)
model9 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+AvgPrevTemp3+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model9)
model10 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+AvgPrevTemp5+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model10)
model11 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+AvgPrevTemp7+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model11)
model12 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindVel+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model12) # warnings
# Check WindVel effect with residuals and asin+sqrt transform lm (significant)
model12a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+(1|CalDay), family="binomial", data=NoZeroTime)
plot(NoZeroTime$WindVel, residuals(model12a))
model12b <- lm(residuals(model12a)~NoZeroTime$WindVel)
Anova(model12b)
model12asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindVel, data=NoZeroTime, na.action=na.omit)
Anova(model12asin)
model13 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+SwimTotal+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model13) # warnings
# Check SwimTotal effect with residuals and asin+sqrt transform lm (not significant)
model13a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+(1|CalDay), family="binomial", data=NoZeroTime)
plot(NoZeroTime$SwimTotal, residuals(model13a))
model13b <- lm(residuals(model13a)~NoZeroTime$SwimTotal)
model13asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+SwimTotal, data=NoZeroTime, na.action=na.omit)
Anova(model13asin)
model14 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+NWSprecip1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model14)
model15 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+NWSprecip2+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model15)

# Step 3 (Best model: model10 w/ TimeOfDay+WindDirGroup1+WindVel)
model1 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WaterTempC+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model1)
model2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+PrevSi1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model2)
model3 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+PrevSi3+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model3)
model4 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+PrevSi5+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model4)
model5 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+PrevSi7+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model5)
model6 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+AvgPrevTemp1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model6)
model7 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+AvgPrevTemp3+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model7)
model8 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+AvgPrevTemp5+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model8) # warnings
# Check WindVel effect with residuals and asin+sqrt transform lm (significant)
model8a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+(1|CalDay), family="binomial", data=NoZeroTime)
plot(NoZeroTime$AvgPrevTemp5, residuals(model10a))
model8b <- lm(residuals(model10a)~NoZeroTime$AvgPrevTemp5)
Anova(model8b)
model8asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1+AvgPrevTemp5, data=NoZeroTime, na.action=na.omit)
Anova(model8asin)
model9 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+AvgPrevTemp7+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model9)
model10 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model10) # warnings
# Check WindVel effect with residuals and asin+sqrt transform lm (significant)
model10a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+(1|CalDay), family="binomial",data=NoZeroTime)
plot(NoZeroTime$WindVel, residuals(model10a))
model10b <- lm(residuals(model10a)~NoZeroTime$WindVel)
Anova(model10b)
model10asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1+WindVel, data=NoZeroTime, na.action=na.omit)
Anova(model10asin)
model11 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+SwimTotal+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model11) # warnings
# Check SwimTotal effect with residuals and asin+sqrt transform lm (not significant)
model11a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+(1|CalDay), family="binomial",data=NoZeroTime)
plot(NoZeroTime$SwimTotal, residuals(model11a))
model11b <- lm(residuals(model11a)~NoZeroTime$SwimTotal)
model11asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1+SwimTotal, data=NoZeroTime, na.action=na.omit)
Anova(model11asin)
model12 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+NWSprecip1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model12)
model13 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+NWSprecip2+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model13)

# Step 4 (No additional predictors significant)
model1 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+WaterTempC+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model1) # warnings
# Check WaterTempC effect with residuals and asin+sqrt transform lm (not significant)
model1a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+(1|CalDay), family="binomial", data=NoZeroTime)
plot(NoZeroTime$WaterTempC, residuals(model1a))
model1b <- lm(residuals(model1a)~NoZeroTime$WaterTempC)
Anova(model1b)
model1asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1+WindVel+WaterTempC, data=NoZeroTime, na.action=na.omit)
Anova(model1asin)
model2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+PrevSi1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model2) # warnings
# Check PrevSi1 effect with residuals and asin+sqrt transform lm (not significant)
model2a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+(1|CalDay), family="binomial", data=NoZeroTime)
plot(NoZeroTime$PrevSi1, residuals(model2a))
model2b <- lm(residuals(model1a)~NoZeroTime$PrevSi1)
Anova(model2b)
model2asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1+WindVel+PrevSi1, data=NoZeroTime, na.action=na.omit)
Anova(model2asin)
model3 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+PrevSi3+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model3)
model4 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+PrevSi5+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model4)
model5 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+PrevSi7+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model5)
model6 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+AvgPrevTemp1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model6)
model7 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+AvgPrevTemp3+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model7) # warnings
# Check AvgPrevTemp3 effect with residuals and asin+sqrt transform lm (not significant)
model7a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+(1|CalDay), family="binomial",data=NoZeroTime)
plot(NoZeroTime$AvgPrevTemp3, residuals(model7a))
model7b <- lm(residuals(model7a)~NoZeroTime$AvgPrevTemp3)
Anova(model7b)
model7asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1+WindVel+AvgPrevTemp3, data=NoZeroTime, na.action=na.omit)
Anova(model7asin)
model8 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+AvgPrevTemp5+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model8)
model9 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+AvgPrevTemp7+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model9)
model10 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1*WindVel+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model10) # warnings
# Check WindDirGroup1 x WindVel interaction effect with asin+sqrt transformed lm (significant)
model10asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1*WindVel, data=NoZeroTime, na.action=na.omit)
Anova(model10asin)
model11 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+SwimTotal+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model11) # warnings
# Check SwimTotal effect with residuals and asin+sqrt transform lm (not significant)
model11a <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+(1|CalDay), family="binomial",data=NoZeroTime)
plot(NoZeroTime$SwimTotal, residuals(model11a))
model11b <- lm(residuals(model11a)~NoZeroTime$SwimTotal)
model11asin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1+WindVel+SwimTotal, data=NoZeroTime, na.action=na.omit)
Anova(model11asin)
model12 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+NWSprecip1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model12)
model13 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+NWSprecip2+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model13)


#_
#F inal model for time of day SI risk
modelFull <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+(1|CalDay),family="binomial",data=NoZeroTime,na.action=na.omit)
Anova(modelFull) # warnings
summary(modelFull)
modelFullasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1+WindVel, data=NoZeroTime, na.action=na.omit)
Anova(modelFullasin)
modelTod <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(modelTod)
summary(modelTod)
modelTodasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay, data=NoZeroTime, na.action=na.omit)
Anova(modelTodasin)
modelDir <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(modelDir)
summary(modelDir)
modelDirasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1, data=NoZeroTime, na.action=na.omit)
Anova(modelDirasin)
modelVel <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(modelVel)
summary(modelVel)
modelVelasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=NoZeroTime, na.action=na.omit)
Anova(modelVelasin)
modelNull <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(modelNull)
summary(modelNull)
modelNullasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~0, data=NoZeroTime, na.action=na.omit)

# R2 from Naglekerke 1991 using log-likelihood ratios for binomial
totalCount <- sum(NoZeroTime$SiTotal)+sum(NoZeroTime$SwimTotal-NoZeroTime$SiTotal)
r2Full <- as.numeric((1-exp(-2/totalCount*(logLik(modelFull)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Tod <- as.numeric((1-exp(-2/totalCount*(logLik(modelTod)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Dir <- as.numeric((1-exp(-2/totalCount*(logLik(modelDir)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Vel <- as.numeric((1-exp(-2/totalCount*(logLik(modelVel)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))

# R2 for asinsqrt models
r2Fullasin <- summary(modelFullasin)$r.squared
r2Todasin <- summary(modelTodasin)$r.squared
r2Dirasin <- summary(modelDirasin)$r.squared
r2Velasin <- summary(modelVelasin)$r.squared

# Full model with WindDirGroup2 instead of WindDirGroup1
model10g2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup2+WindVel+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model10g2)
summary(model10g2)

# Final model with all wind directions included in WindDir variable
model10all <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDir+WindVel+(1|CalDay), family="binomial", data=NoZeroTime, na.action=na.omit)
Anova(model10all) # warnings
model10allasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDir+WindVel, data=NoZeroTime, na.action=na.omit)
Anova(model10allasin)

#_________________________________
# Final model with all datapoints
modelFull <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup1+WindVel+(1|CalDay), family="binomial", data=LifeguardTodFull, na.action=na.omit)
Anova(modelFull)
summary(modelFull)
modelFullasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDirGroup1+WindVel, data=LifeguardTodFull, na.action=na.omit)
Anova(modelFullasin)
modelTod <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+(1|CalDay), family="binomial", data=LifeguardTodFull, na.action=na.omit)
Anova(modelTod)
summary(modelTod)
modelTodasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay, data=LifeguardTodFull, na.action=na.omit)
Anova(modelTodasin)
modelDir <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindDirGroup1+(1|CalDay), family="binomial", data=LifeguardTodFull, na.action=na.omit)
Anova(modelDir)
summary(modelDir)
modelDirasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindDirGroup1, data=LifeguardTodFull, na.action=na.omit)
Anova(modelDirasin)
modelVel <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~WindVel+(1|CalDay), family="binomial", data=LifeguardTodFull, na.action=na.omit)
Anova(modelVel)
summary(modelVel)
modelVelasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~WindVel, data=LifeguardTodFull, na.action=na.omit)
Anova(modelVelasin)
modelNull <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~(1|CalDay), family="binomial", data=LifeguardTodFull, na.action=na.omit)
Anova(modelNull)
summary(modelNull)
modelNullasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~0, data=LifeguardTodFull, na.action=na.omit)

# R2 from Naglekerke 1991 using log-likelihood ratios for binomial
totalCount <- sum(LifeguardTodFull$SiTotal)+sum(LifeguardTodFull$SwimTotal-LifeguardTodFull$SiTotal)
r2Full <- as.numeric((1-exp(-2/totalCount*(logLik(modelFull)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Tod <- as.numeric((1-exp(-2/totalCount*(logLik(modelTod)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Dir <- as.numeric((1-exp(-2/totalCount*(logLik(modelDir)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))
r2Vel <- as.numeric((1-exp(-2/totalCount*(logLik(modelVel)-logLik(modelNull))))/(1-exp(2/totalCount*logLik(modelNull))))

# R2 for asinsqrt models
r2Fullasin <- summary(modelFullasin)$r.squared
r2Todasin <- summary(modelTodasin)$r.squared
r2Dirasin <- summary(modelDirasin)$r.squared
r2Velasin <- summary(modelVelasin)$r.squared

# Full model with WindDirGroup2 instead of WindDirGroup1
model10g2 <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDirGroup2+WindVel+(1|CalDay), family="binomial", data=LifeguardTodFull, na.action=na.omit)
Anova(model10g2)
summary(model10g2)

# Final model with all wind directions included in WindDir variable
model10all <- glmer(cbind(SiTotal,SwimTotal-SiTotal)~TimeOfDay+WindDir+WindVel+(1|CalDay), family="binomial", data=LifeguardTodFull, na.action=na.omit)
Anova(model10all) # warnings
model10allasin <- lm(I(asin(sqrt(SiTotal/SwimTotal)))~TimeOfDay+WindDir+WindVel, data=LifeguardTodFull, na.action=na.omit)
Anova(model10allasin)

#_
# Generate Clopper-Pearson "exact" confidence intervals for binomial proportion data

SIcases <- conf$SiTotal
Swimmers <- conf$SwimTotal

lower <- numeric(length(SIcases))
upper <- numeric(length(SIcases))
diff <- numeric(length(SIcases))

# Calculate confidence boundaries and the difference to plot stacked area in MS Excel
for(i in 1:length(SIcases)){
	lower[i] <- exactci(SIcases[i], Swimmers[i],0.95)$conf.int[1]
	upper[i] <- exactci(SIcases[i], Swimmers[i],0.95)$conf.int[2]
	diff[i]<-upper[i]-lower[i]
}

df <- data.frame(cbind(conf, numSI=SIcases, numSwim=Swimmers, lower=lower, upper=upper, diff=diff))

# Write data frame to a .csv for outside use
# be sure to specify your file path
write.csv(df, "your_path_here", row.names=FALSE)