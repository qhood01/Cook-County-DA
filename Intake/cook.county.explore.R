setwd("~/Documents/Cook County DA/")
intake <- readRDS("intake.rds")
##initiation <- readRDS("initiation.rds")

## intake <- read.csv("Intake.csv")
## saveRDS(intake,"./intake.rds")
## initiation <- read.csv("Initiation.csv")
## saveRDS(initiation,"./initiation.rds")
## disposition <- read.csv("Dispositions.csv")
## sentencing <- read.csv("Sentencing.csv")

## Viz
## Breakdown of charges by percent
## Click on charge and see flow chart of outcomes
## Break this down by race
## Predict outcomes

## New Model DF
modelDF <- data.frame(matrix(nrow=nrow(intake),ncol=0))

## Intake
## Dependent Var - PARTICIPANT_STATUS (Only approved vs. rejected)
modelDF$result <- NA
modelDF$result[which(intake$PARTICIPANT_STATUS == "Approved")] <- 1
modelDF$result[which(intake$PARTICIPANT_STATUS == "Rejected")] <- 0

## Independent Vars
#### Time var - year?
modelDF$year <- NA
modelDF$year <- substr(intake$RECEIVED_DATE,7,10)
modelDF$year <- factor(modelDF$year)

#### Offense Cat - aggregate from 87 levels
modelDF$charge.cat <- "Other"
###### Drugs
modelDF$charge.cat[which(intake$Offense_Category == "Narcotics")] <- "Drugs"
###### Weapons
modelDF$charge.cat[which(intake$Offense_Category == "UUW - Unlawful Use of Weapon")] <- "Weapons"
modelDF$charge.cat[which(intake$Offense_Category == "Gun - Non UUW")] <- "Weapons"
modelDF$charge.cat[which(intake$Offense_Category == "Reckless Discharge of Firearm")] <- "Weapons"
###### Theft/Retail Theft
modelDF$charge.cat[which(intake$Offense_Category == "Theft")] <- "Theft"
modelDF$charge.cat[which(intake$Offense_Category == "Retail Theft")] <- "Theft"
###### DUI
modelDF$charge.cat[which(intake$Offense_Category == "DUI")] <- "DUI"
modelDF$charge.cat[which(intake$Offense_Category == "Aggravated DUI")] <- "DUI"
###### Burglary/Residential Burg
modelDF$charge.cat[which(intake$Offense_Category == "Burglary")] <- "Burglary"
modelDF$charge.cat[which(intake$Offense_Category == "Residential Burglary")] <- "Burglary"
modelDF$charge.cat[which(intake$Offense_Category == "Possession Of Burglary Tools")] <- "Burglary"
###### Suspended License
modelDF$charge.cat[which(intake$Offense_Category == "Driving With Suspended Or Revoked License")] <- "Suspended License"
###### Sex
modelDF$charge.cat[which(intake$Offense_Category == "Sex Crimes")] <- "Sex"
modelDF$charge.cat[which(intake$Offense_Category == "Attempt Sex Crimes")] <- "Sex"
###### Robbery/Armed Robbery
modelDF$charge.cat[which(grepl("Robbery",intake$Offense_Category))] <- "Robbery"
modelDF$charge.cat[which(grepl("Home Invasion",intake$Offense_Category))] <- "Robbery"
modelDF$charge.cat[which(grepl("Vehicular Hijacking",intake$Offense_Category))] <- "Robbery"
###### Stolen Car
modelDF$charge.cat[which(intake$Offense_Category == "Possession of Stolen Motor Vehicle")] <- "Stolen Car"
###### Battery
modelDF$charge.cat[which(grepl("Assault",intake$Offense_Category))] <- "Battery/Assault"
modelDF$charge.cat[which(grepl("Battery",intake$Offense_Category))] <- "Battery/Assault"
###### Homicide
modelDF$charge.cat[which(grepl("Homicide",intake$Offense_Category))] <- "Homicide"
modelDF$charge.cat <- factor(modelDF$charge.cat)

#### Age
modelDF$age <- NA
modelDF$age[which(intake$AGE_AT_INCIDENT >= 17 & intake$AGE_AT_INCIDENT <= 20)] <- "17-20"
modelDF$age[which(intake$AGE_AT_INCIDENT >= 21 & intake$AGE_AT_INCIDENT <= 25)] <- "21-25"
modelDF$age[which(intake$AGE_AT_INCIDENT >= 26 & intake$AGE_AT_INCIDENT <= 30)] <- "26-30"
modelDF$age[which(intake$AGE_AT_INCIDENT >= 31 & intake$AGE_AT_INCIDENT <= 35)] <- "31-35"
modelDF$age[which(intake$AGE_AT_INCIDENT >= 36 & intake$AGE_AT_INCIDENT <= 40)] <- "36-40"
modelDF$age[which(intake$AGE_AT_INCIDENT >= 41 & intake$AGE_AT_INCIDENT <= 45)] <- "41-45"
modelDF$age[which(intake$AGE_AT_INCIDENT >= 46 & intake$AGE_AT_INCIDENT <= 50)] <- "46-50"
modelDF$age[which(intake$AGE_AT_INCIDENT >= 51 & intake$AGE_AT_INCIDENT <= 55)] <- "51-55"
modelDF$age[which(intake$AGE_AT_INCIDENT > 55)] <- "55+"
modelDF$age <- factor(modelDF$age)

#### Gender
modelDF$gender <- NA
###### Male
modelDF$gender[which(intake$GENDER == "Male")] <- "Male"
###### Female
modelDF$gender[which(intake$GENDER == "Female")] <- "Female"
modelDF$gender <- factor(modelDF$gender)

#### Race
modelDF$race <- NA
###### White
modelDF$race[which(intake$RACE == "White")] <- "White"
###### Black
modelDF$race[which(intake$RACE == "Black")] <- "Black"
###### Hispanic
modelDF$race[which(grepl("Hispanic",intake$RACE,ignore.case = TRUE))] <- "Hispanic"
modelDF$race <- factor(modelDF$race)

#### Law enforcement
modelDF$chicagoPD <- ifelse(intake$LAW_ENFORCEMENT_AGENCY == "CHICAGO PD","Chicago PD","Other PD")
modelDF$chicagoPD <- factor(modelDF$chicagoPD)

#### Number of defendants on case- more than 1?
case.id.counts <- plyr::count(intake,vars=c("CASE_ID"))
names(case.id.counts)[1] <- c("CASE_ID")
case.id.counts$multiple.defendants <- ifelse(case.id.counts$freq > 1, "Multiple", "One")
case.id.counts$multiple.defendants <- factor(case.id.counts$multiple.defendants)
case.ids.merge <- plyr::join(x=intake,y=case.id.counts)

modelDF$multiple.defendents <- case.ids.merge$multiple.defendants
modelDF$multiple.defendents <- factor(modelDF$multiple.defendents, c("One","Multiple"))

fit <- glm(result ~ ., family="binomial", data=modelDF)

summary_glm <- summary(fit)

list( summary_glm$coefficient,
      round( 1 - ( summary_glm$deviance / summary_glm$null.deviance ), 2 ) )

prop.table(table(modelDF$charge.cat,modelDF$result),1)*100
prop.table(table(modelDF$race,modelDF$result),1)*100
prop.table(table(modelDF$chicagoPD,modelDF$result),1)*100

## Initiation is a charge level file
## Can roll up to top charge in order to merge with intake

probabilities <- predict(fit, type="response")

logit = log(probabilities/(1-probabilities))

for (c in levels(modelDF$charge.cat)) {
    print(c)
    df <- modelDF[which(modelDF$charge.cat == c),]
    print(prop.table(table(df$race,df$result),1)*100)
}
