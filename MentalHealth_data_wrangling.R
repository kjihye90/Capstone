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

