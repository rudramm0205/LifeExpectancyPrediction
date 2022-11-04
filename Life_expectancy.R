# Load Life Expectancy Dataset
LifeExpectancy<- read.csv("/Users/rudramm0205/Downloads/Life_Expectancy_Data1.csv",row.names=1)
#Change Column
colnames(LifeExpectancy) <- c (  
  "Country.Code",
  "Continent",
  "Life.expectancy.at.birth",
  "Access.to.electricity",
  "Adjusted.net.national.income",
  "Adjusted.net.national.income.per.capita",
  "Children.newly.infected.HIV",
  "Children.out.of.school",
  "Educational.attainment.primary",
  "Educational.attainment.Bachelors",
  "Mortality.rate.infant",
  "Primary.completion.rate.total",
  "Literacy.rate.adult.total",
  "Real.interest.rate",
  "Population.growth",
  "Population.density",
  "Population.total",
  "Current.health.expenditure.PPP",
  "Current.health.expenditure.GDP",
  "Unemployment.total",
  "GDP.growth",
  "GDP.per.capita.PPP",
  "Birth.rate.crude",
  "Renewable.energy.consumption",
  "Adults.newly.infected.HIV",
  "safely.managed.drinking.water.services",
  "Poverty.headcount.ratio",
  "Compulsory.education.duration"
)

#Q.1
attach  ( LifeExpectancy)
summary ( LifeExpectancy)
stat.desc( LifeExpectancy[,c("Access.to.electricity","Adjusted.net.national.income","Mortality.rate.infant","Population.density","Current.health.expenditure.PPP", "Adjusted.net.national.income.per.capita", "Birth.rate.crude","Adults.newly.infected.HIV","safely.managed.drinking.water.services" )])

# Summary statistics 
#viewing missing data for sample dataset.
library (mice)
library (VIM)
badata <- LifeExpectancy[,4:ncol(LifeExpectancy)]
misbadata <- apply( badata, 2, function(col)sum(is.na(col))/length(col)*100)
aggr_plot <- aggr ( badata , col = c('navyblue','grey'), numbers= TRUE, sortVars=TRUE, labels=names(badata),cex.axis=.9,gap=3,
                    ylab=c("Histogram-Missing Data","Pattern"), dev.new (width=100, height=50))

# checking the retationship b/w the predictor and response variables.
plot   ( Life.expectancy.at.birth ~ Access.to.electricity ,pch = 20, cex = 1.4 )
plot   ( Life.expectancy.at.birth ~ safely.managed.drinking.water.services, pch = 20, cex = 1.4 )
plot   ( Life.expectancy.at.birth ~ Mortality.rate.infant ,pch = 20, cex = 1.4 )
plot   ( Life.expectancy.at.birth ~ Birth.rate.crude ,pch = 20, cex = 1.4 )
plot   ( Life.expectancy.at.birth ~ Population.density    ,pch = 20, cex = 1.4 )
plot   ( Life.expectancy.at.birth ~ Adults.newly.infected.HIV , pch = 20, cex = 1.4 )
plot   ( Life.expectancy.at.birth ~ Current.health.expenditure.PPP ,pch = 20, cex = 1.4 )
plot   ( Life.expectancy.at.birth ~ Adjusted.net.national.income.per.capita ,pch = 20, cex = 1.4 )
# checking the life expectance base on the recorded response in dataset
hist ( Life.expectancy.at.birth ,xlab = "Life.expectancy" )
qplot ( x=Mortality.rate.infant, y=Birth.rate.crude , geom="boxplot"  )

#Q.2
#Identifying Percentage of NA Values in Columns
library(tidyverse)
apply(LifeExpectancy, 2, function(col)sum(is.na(col))/length(col))

#Removing Variables with more than 70% NA Values, Country Code and Continent Column 
LifeExpectancy2 = LifeExpectancy[,c(-1:-2,-9,-10,-13,-24,-27)]
view(LifeExpectancy2)
apply(LifeExpectancy2, 2, function(col)sum(is.na(col))/length(col))

#Imputed Median for all NA Values
for(i in 1:ncol(LifeExpectancy2)) {
  LifeExpectancy2[ , i][is.na(LifeExpectancy2[ , i])] <- median(LifeExpectancy2[ , i], na.rm = TRUE)
}
view(LifeExpectancy2)

# Imputed Mean of all the Variables
for(i in 1:ncol(LifeExpectancy2)) {
  LifeExpectancy2[ , i][is.na(LifeExpectancy2[ , i])] <- mean(LifeExpectancy2[ , i], na.rm = TRUE)
}
#Running linear model with the data having least significant variables just for comparison with the Data in which we have removed least significant variables 


lm_1 <- lm(LifeExpectancy2$Life.expectancy.at.birth ~ ., data = LifeExpectancy2)
summary(lm_1)
hist(lm_1$residuals, breaks = 5)
plot(lm_1,which= 2)
#Q.3 
# Collinearity Matrix
library(corrplot)
ColMat <- cor(LifeExpectancy2)
print(ColMat)
ColMat.corr <- cor(LifeExpectancy2)
# Heatmap of Correlation matrix
corrplot(ColMat.corr)
# Removing Least Significant Variables after checking Heatmap 
names(LifeExpectancy2)
droping<- c("Adjusted.net.national.income", "Adjusted.net.national.income.per.capita", "Children.newly.infected.HIV", "Children.out.of.school", "Mortality.rate.infant","Primary.completion.rate.total", "Real.interest.rate", "Population.growth", "Population.density", "Population.total", "Current.health.expenditure.GDP", "Unemployment.total", "GDP.growth", "Birth.rate.crude", "Adults.newly.infected.HIV", "Compulsory.education.duration")
LifeExpectancy2 <- LifeExpectancy2[, !(names(LifeExpectancy2) %in% drop)]
view(LifeExpectancy2)
#Applying VIF on the data
vif(LifeExpectancy2)


#Running the Linear Model again after removing the least significant variables 

lm_2 <- lm(LifeExpectancy2$Life.expectancy.at.birth ~ ., data = LifeExpectancy2)
summary(lm_2)
plot(lm_2,which= 2)
hist(lm_2$residuals, breaks = 20)
# Compute the analysis of variance
life.aov <- aov(Life.expectancy.at.birth ~ ., data = LifeExpectancy2)
# Summary of the analysis
summary(life.aov)
# Summary Function 
summary(LifeExpectancy2)
