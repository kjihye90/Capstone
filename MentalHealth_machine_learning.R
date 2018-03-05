library(tidyr)
library(dplyr)
library(ggplot2)

# catagorize the size of the company
MentalHealth_raw$no_employees<- replace(MentalHealth_raw$no_employees, MentalHealth_raw$no_employees=="1-5", "1")
MentalHealth_raw$no_employees<- replace(MentalHealth_raw$no_employees, MentalHealth_raw$no_employees=="6-25", "2")
MentalHealth_raw$no_employees<- replace(MentalHealth_raw$no_employees, MentalHealth_raw$no_employees=="26-100", "3")
MentalHealth_raw$no_employees<- replace(MentalHealth_raw$no_employees, MentalHealth_raw$no_employees=="100-500", "4")
MentalHealth_raw$no_employees<- replace(MentalHealth_raw$no_employees, MentalHealth_raw$no_employees=="500-1000", "5")
MentalHealth_raw$no_employees<- replace(MentalHealth_raw$no_employees, MentalHealth_raw$no_employees=="More than 1000", "6")

# NA values
  # Since no_employees is not normally distributed, median was used for missing value
MentalHealth_raw$no_employees[is.na(MentalHealth_raw$no_employees)] <- "4"
MentalHealth_raw$no_employees <- as.numeric(as.character(MentalHealth_raw$no_employees))

#################################################################################

# categorical values to numeric values  
# variables which have three answers
MentalHealth_raw$benefits[MentalHealth_raw$benefits=="yes"] <- 3
MentalHealth_raw$benefits[MentalHealth_raw$benefits=="Don't know"] <- 2
MentalHealth_raw$benefits[MentalHealth_raw$benefits=="No"] <- 1
MentalHealth_raw$care_options[MentalHealth_raw$care_options=="Yes"] <- 3
MentalHealth_raw$care_options[MentalHealth_raw$care_options=="Not sure"] <- 2
MentalHealth_raw$care_options[MentalHealth_raw$care_options=="No"] <- 1
MentalHealth_raw$wellness_program[MentalHealth_raw$wellness_program=="Yes"] <- 3
MentalHealth_raw$wellness_program[MentalHealth_raw$wellness_program=="Don't know"] <- 2
MentalHealth_raw$wellness_program[MentalHealth_raw$wellness_program=="No"] <- 1
MentalHealth_raw$seek_help[MentalHealth_raw$seek_help=="Yes"] <- 3
MentalHealth_raw$seek_help[MentalHealth_raw$seek_help=="Don't know"] <- 2
MentalHealth_raw$seek_help[MentalHealth_raw$seek_help=="No"] <- 1
MentalHealth_raw$anonymity[MentalHealth_raw$anonymity=="Yes"] <- 3
MentalHealth_raw$anonymity[MentalHealth_raw$anonymity=="Don't know"] <- 2
MentalHealth_raw$anonymity[MentalHealth_raw$anonymity=="No"] <- 1
MentalHealth_raw$mental_health_consequence[MentalHealth_raw$mental_health_consequence=="Yes"] <- 3
MentalHealth_raw$mental_health_consequence[MentalHealth_raw$mental_health_consequence=="Maybe"] <- 2
MentalHealth_raw$mental_health_consequence[MentalHealth_raw$mental_health_consequence=="No"] <- 1
MentalHealth_raw$phys_health_consequence[MentalHealth_raw$phys_health_consequence=="Yes"] <- 3
MentalHealth_raw$phys_health_consequence[MentalHealth_raw$phys_health_consequence=="Maybe"] <- 2
MentalHealth_raw$phys_health_consequence[MentalHealth_raw$phys_health_consequence=="No"] <- 1
MentalHealth_raw$coworkers[MentalHealth_raw$coworkers=="Yes"] <- 3
MentalHealth_raw$coworkers[MentalHealth_raw$coworkers=="Some of them"] <- 2
MentalHealth_raw$coworkers[MentalHealth_raw$coworkers=="No"] <- 1
MentalHealth_raw$supervisor[MentalHealth_raw$supervisor=="Yes"] <- 3
MentalHealth_raw$supervisor[MentalHealth_raw$supervisor=="Some of them"] <- 2
MentalHealth_raw$supervisor[MentalHealth_raw$supervisor=="No"] <- 1
MentalHealth_raw$mental_health_interview[MentalHealth_raw$mental_health_interview=="Yes"] <- 3
MentalHealth_raw$mental_health_interview[MentalHealth_raw$mental_health_interview=="Maybe"] <- 2
MentalHealth_raw$mental_health_interview[MentalHealth_raw$mental_health_interview=="No"] <- 1
MentalHealth_raw$phys_health_interview[MentalHealth_raw$phys_health_interview=="Yes"] <- 3
MentalHealth_raw$phys_health_interview[MentalHealth_raw$phys_health_interview=="Maybe"] <- 2
MentalHealth_raw$phys_health_interview[MentalHealth_raw$phys_health_interview=="No"] <- 1
MentalHealth_raw$mental_vs_physical[MentalHealth_raw$mental_vs_physical=="Yes"] <- 3
MentalHealth_raw$mental_vs_physical[MentalHealth_raw$mental_vs_physical=="Don't know"] <- 2
MentalHealth_raw$mental_vs_physical[MentalHealth_raw$mental_vs_physical=="No"] <- 1

MentalHealth_raw$benefits <- as.numeric(as.character(MentalHealth_raw$benefits))
MentalHealth_raw$care_options <- as.numeric(as.character(MentalHealth_raw$care_options))
MentalHealth_raw$wellness_program <- as.numeric(as.character(MentalHealth_raw$wellness_program))
MentalHealth_raw$seek_help <- as.numeric(as.character(MentalHealth_raw$seek_help))
MentalHealth_raw$anonymity <- as.numeric(as.character(MentalHealth_raw$anonymity))
MentalHealth_raw$mental_health_consequence <- as.numeric(as.character(MentalHealth_raw$mental_health_consequence))
MentalHealth_raw$phys_health_consequence <- as.numeric(as.character(MentalHealth_raw$phys_health_consequence))
MentalHealth_raw$coworkers <- as.numeric(as.character(MentalHealth_raw$coworkers))
MentalHealth_raw$supervisor <- as.numeric(as.character(MentalHealth_raw$supervisor))
MentalHealth_raw$mental_health_interview <- as.numeric(as.character(MentalHealth_raw$mental_health_interview))
MentalHealth_raw$phys_health_interview <- as.numeric(as.character(MentalHealth_raw$phys_health_interview))
MentalHealth_raw$mental_vs_physical <- as.numeric(as.character(MentalHealth_raw$mental_vs_physical))

# variables which have two answers
MentalHealth_raw$family_history[MentalHealth_raw$family_history=="Yes"] <- 1
MentalHealth_raw$family_history[MentalHealth_raw$family_history=="No"] <- 0
MentalHealth_raw$treatment[MentalHealth_raw$treatment=="Yes"] <- 1
MentalHealth_raw$treatment[MentalHealth_raw$treatment=="No"] <- 0
MentalHealth_raw$tech_company[MentalHealth_raw$tech_company=="Yes"] <- 1
MentalHealth_raw$tech_company[MentalHealth_raw$tech_company=="No"] <- 0
MentalHealth_raw$remote_work[MentalHealth_raw$remote_work=="Yes"] <- 1
MentalHealth_raw$remote_work[MentalHealth_raw$remote_work=="No"] <- 0
MentalHealth_raw$obs_consequence[MentalHealth_raw$obs_consequence=="Yes"] <- 1
MentalHealth_raw$obs_consequence[MentalHealth_raw$obs_consequence=="No"] <- 0

MentalHealth_raw$family_history <- as.numeric(as.character(MentalHealth_raw$family_history))
MentalHealth_raw$treatment <- as.numeric(as.character(MentalHealth_raw$treatment))
MentalHealth_raw$tech_company <- as.numeric(as.character(MentalHealth_raw$tech_company))
MentalHealth_raw$remote_work <- as.numeric(as.character(MentalHealth_raw$remote_work))
MentalHealth_raw$obs_consequence <- as.numeric(as.character(MentalHealth_raw$obs_consequence))

# corrPlot to examine the relationships between variables
install.packages("corrplot")
library(corrplot)
  
  # variables which have three answers (Yes/No/Don't Know)
MentalHealth_Data_1 <- subset(MentalHealth_raw, select = c(benefits, care_options, wellness_program, seek_help, anonymity, mental_health_consequence, phys_health_consequence, coworkers,supervisor, mental_health_interview, phys_health_interview, mental_vs_physical))
M <- cor(MentalHealth_Data_1)
corrplot(M, method = "number")
  # From here, I found a quite strong correlation between 'seek_help' & 'wellness_program' (correlation coefficient of 0.62)

  # variables which have two answers (Yes/No)
MentalHealth_Data_2 <- subset(MentalHealth_raw, select = c(family_history, treatment, tech_company, remote_work, obs_consequence))
N <- cor(MentalHealth_Data_2)
corrplot(N, method = "number")

# plot 'seek_help', 'wellness_program', 'no_employees'
ggplot(MentalHealth_raw, aes(x=seek_help, y=wellness_program, color=no_employees)) + geom_point()

###################################################################################
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(plyr)

# Using the complete data to predict seek_help a categorical variable. Using method="class"
fit <- rpart(seek_help ~ no_employees + wellness_program, method = "class", data = raw)
fit1 <- rpart(seek_help ~ no_employees + wellness_program + mental_vs_physical, method = "class", data = raw)
fit2 <- rpart(seek_help ~ no_employees + wellness_program + care_options + anonymity + mental_vs_physical, method = "class", data = raw)
fit3 <- rpart(seek_help ~ no_employees + wellness_program + care_options + anonymity + mental_vs_physical + mental_health_consequence + mental_health_interview, method = "class", data = raw)
fit4 <- rpart(seek_help ~ no_employees + wellness_program + care_options, method = "class", data = raw)
fit5 <- rpart(seek_help ~ no_employees + wellness_program + Country, method = "class", data = raw)
fit6 <- rpart(seek_help ~ no_employees + wellness_program + care_options + anonymity, method = "class", data = raw)

# Tuning variable cp with best value of 0.01.
printcp(fit)
plotcp(fit)

printcp(fit1)
plotcp(fit1)

printcp(fit2)
plotcp(fit2)

printcp(fit3)
plotcp(fit3)

printcp(fit4)
plotcp(fit4)

printcp(fit5)
printcp(fit5)

printcp(fit6)
printcp(fit6)

# Understanding the summary of fit
summary(fit)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

# Plotting the model tree
fancyRpartPlot(fit)
fancyRpartPlot(fit1)
fancyRpartPlot(fit2)

############################################################################################

# This is not the ideal way. We are using the same inputdata for testing accuracy.
# Just for demonstration purpose. Ideally we should spit into training , testing and validation data sets

# Calculating accuracy of the model
t_pred = predict(fit4,raw,type="class")
t = raw$seek_help
accuracy = sum(t_pred == t)/length(t)
print(accuracy)
