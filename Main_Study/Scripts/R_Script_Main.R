
# set working directory
setwd("C:/Users/bened/OneDrive/Desktop/TUC/Wintersemester 202223/Masterarbeit/R_Analysen/STA-master/STA-master/STACMR-R")
# The folder "STACMR-R" needs to be the working directory. This folder is part
# of the STACMR-R package by Dunn and Kalish (2020), that needs to be
# installed properly following a tutorial.
# This tutorial can be accessed under: https://github.com/michaelkalish/STA/tree/master
# File Path: STACMR-R --> STACMR-R.pdf

#### 0. Packages
#install if necessary
library(car)
library(readr)
library(dplyr)
library(tidyr)
library(effectsize)
library(psych)
library(lsr)

install.packages(devtools)
library(devtools)

devtools::install_github("monotonicity/stacmr", force = TRUE)

library(stacmr)
install.packages("tidyverse")
library(tidyverse)




################################################################################
################################################################################


#### 1. Read Data


library(readr)
raw_data <- read_csv("C:/Users/bened/OneDrive/Desktop/TUC/Wintersemester 202223/Masterarbeit/Daten_MA/Data_Main.csv")

View(raw_data)

# Experimental Conditions:
#C10O1: Condition 1
#C11O0: Condition 2
#C10OM: Condition 3
#C11OM: Condition 4
#C11O1: Condition 5
#C10O0: Condition 6

#prefix 'ws': within-subject condition, i.e. not the first condition that
#subjects went through

#Liberalism items:
#SQ002
#SQ004
#SQ006
#SQ008

#Intellect items:
#SQ001
#SQ003
#SQ005
#SQ007

#Item O98
#SQ010

#Item C05
#SQ009


################################################################################
################################################################################



#### 2. Data Tidying


### 2.2 exclude Subjects 15, 149 and 115 

raw_data_clean <- raw_data[!(raw_data$id %in% c(15, 149, 115)), ]


### 2.3 Turn Responses into numerical values

## Replace responses with number strings

table(raw_data_clean$`C10O1[SQ001]`)

raw_data_clean <- replace(raw_data_clean, raw_data_clean == "trifft nicht zu", "1") %>%
  replace(raw_data_clean == "trifft eher nicht zu", "2") %>%
  replace(raw_data_clean == "weder noch", "3") %>%
  replace(raw_data_clean == "trifft eher zu", "4") %>%
  replace(raw_data_clean == "trifft zu", "5")


#Select all Response Items from the survey's main part

library(dplyr)
cols_likert <- dplyr::select(raw_data_clean, matches("SQ"))

#Turn columns numeric

for (i in c(1:ncol(raw_data_clean))) {
  if (colnames(raw_data_clean)[[i]] %in% colnames(cols_likert)) {
    raw_data_clean[[i]] <- as.numeric(raw_data_clean[[i]])
  }
}


### 2.4 invert negative Coded Items


# Data Frame for all columns with negative coded items


cols_negCod <- dplyr::select(raw_data_clean, matches(c("SQ003","SQ005","SQ006",
                                                  "SQ007","SQ008", "SQ010")))

# Index for all columns with negative coded items in raw_data_clean
Index_negCod <- which(raw_data_clean %in% cols_negCod)



raw_data_clean[,Index_negCod] <- 6 - raw_data_clean[,Index_negCod]



################################################################################
################################################################################


#### 3. Bring Data in a more general Format for between- and within-analyses




### 3.1 Set names for columns

prefix <- c("C10O1", "C11O0", "C10OM", "C11OM", "C11O1", "C10O0")
suffix <- c("SQ001", "SQ002", "SQ003", "SQ004", "SQ010", "SQ005", "SQ006", "SQ009", "SQ007", "SQ008")

names_cols_Time <- sprintf("%s%s", rep(prefix, each = 1),
                                       rep("Time", times = length(prefix)))

names_cols_norm_within <- sprintf("%s[%s]", rep(prefix, each = length(suffix)),
                           rep(suffix, times = length(prefix))) %>%
  c(., names_cols_Time)


names_cols_norm_between <- (c("SQ001", "SQ002", "SQ003", "SQ004", "SQ010", 
                              "SQ005", "SQ006", "SQ009", "SQ007", "SQ008",
                              "TimeFallbsp"))




### 3.2 Create data.frames with STA-relevant data per participant

tidy_data <- function(data, between) {
  
#Initialize List
liste_dfs <- list()

if (between == FALSE) {

for (i in 1:nrow(data)) {
  
  # All Variables, except for unanswered Vignettes 
  selected_columns <- as.data.frame(data[i,]) %>%
    dplyr::select(-(contains("C1") & where(anyNA))) %>%
    dplyr::select(-contains("SQ011"))
  
  # Create Indices for Vignettes and Timings
  index <- grep("C1", colnames(selected_columns))
  index_C1 <- index[!index %in% grep("Time", colnames(selected_columns))]
  index_Time <- index[index %in% grep("Time", colnames(selected_columns))]
  
  # Norm sequential Order of Vignettes 
cols_ordered <- as.data.frame(bind_cols(dplyr::select(selected_columns[index_C1], contains("C10O1"),
                                  contains("C11O0"),
                                  contains("C10OM"),
                                     contains("C11OM"),
                                     contains("C11O1"),
                                     contains("C10O0"),
                                     contains("C10O1"),
                                     contains("C11O0")),
                                    
                                  dplyr::select(selected_columns[index_Time], contains("C10O1"),
                                         contains("C11O0"),
                                         contains("C10OM"),
                                         contains("C11OM"),
                                         contains("C11O1"),
                                         contains("C10O0"),
                                         contains("C10O1"),
                                         contains("C11O0"))))

  # Apply sequential order
  selected_columns[,index] <- cols_ordered
  #Uniform Column names
  colnames(selected_columns)[index] <- names_cols_norm_within

  liste_dfs[[i]] <- selected_columns 
}
}
  else {
    
    # All Variables, except for unanswered Vignettes 
    for (i in 1:nrow(data)) {
      
      selected_columns <- data[i,] %>%
        dplyr::select(-(contains("C1") & where(anyNA))) %>%
        dplyr::select(-(contains("wsC1") | contains("wC1")))
      
      # Uniform Column names
      names(selected_columns)[grep("C1", 
                                  colnames(selected_columns))] <- names_cols_norm_between
      
      liste_dfs[[i]] <- selected_columns 
    }
  }

# Merge lists of single participants to one data frame
STA_matrix <- do.call(rbind, lapply(liste_dfs, as.matrix))
data_STA_format <- as.data.frame(STA_matrix)

return(data_STA_format)

}





#between data frame
between_STA_data <- tidy_data(data = raw_data_clean, between = TRUE)

#within data frame

within_STA_data <- tidy_data(data = raw_data_clean, between = FALSE) 

# General Format
  
general_STA_data <- within_STA_data %>%
# bring data into long format
  pivot_longer(
  cols = contains("C1") & -contains("Time"),
  names_to = "Condition",
  values_to = "values"
) %>%
  
# Split Condition- and Item-code
separate(Condition, into = c("Bedingung","Item"),
           sep = "SQ") %>%

# make Data wider
pivot_wider(names_from = Item, values_from = values)

# as data.frame
general_STA_data <- as.data.frame(general_STA_data)



  

### 3.3 Make Columns numeric
make_numeric <- function(data) {
  ind_num <- grep("SQ|G01Q10|interviewtime|001|002|003|004|010|005|006|009|007|008",
                  colnames(data))

  for (i in ind_num) {
    data[,i] <- as.numeric(data[,i])
  }
  return(data)
}

within_STA_data <- make_numeric(within_STA_data)
between_STA_data <- make_numeric(between_STA_data)
general_STA_data <- make_numeric(general_STA_data)


### 3.4 Determine Sum Scores

Lib_items <- c("002", "004", "006", "008")
Int_items <- c("001", "003", "005", "007")

add_sums <- function(data) {
  
data$LibSum <- rowSums(dplyr::select(data, contains(Lib_items)
                                         & -contains("SCALI")))
data$IntSum <- rowSums(dplyr::select(data, contains(Int_items)
                              & -contains(c("SCALI", "Check"))))

return(data)
}

between_STA_data <- add_sums(between_STA_data)
general_STA_data <- add_sums(general_STA_data)


#general_STA_data: For every participant, there are six rows, corresponding
# to the six experimental conditions

# between_STA_data: Every row represents a participant. Only the responses
# to the first vignette presented are included

# within_STA_data: Every row represents a participant: All responses to the six vignettes
# are included

################################################################################
################################################################################




#### 4. Descriptive Stats - overall sample (Thesis Section 5.1.1)




################################################################################
##
#  Important: 4.1.1 - 4.1.5 cannot be executed because demographic data has been omitted
#             in Data_Pilot.csv for data privacy reasons
#             - Hence, 4.1.4 - 4.1.5 was commented out in this script
##
################################################################################



### 4.1 Sample Characteristics - overall sample

## 4.1.1 Age Distribution (Thesis Section 5.1.1.2)

#table(raw_data_clean$G01Q10)
#mean(raw_data_clean$G01Q10)
#sd(raw_data_clean$G01Q10)


## 4.1.2 Gender distribution - identity (Thesis Section 5.1.1.3)

#table(raw_data_clean$G01Q05)
#table(raw_data_clean$G01Q05)/113 #percentages


#table(raw_data_clean$`G01Q05[other]`)

## 4.1.3 Gender distribution - biological

#table(raw_data_clean$gender)
#table(raw_data_clean$gender)/113 #percentages


#table(raw_data_clean$`gender[other]`)


## 4.1.3 Occupation (Thesis Section 5.1.1.4)

#table(raw_data_clean$BerufPos1)
#table(raw_data_clean$BerufPos1)/113

#table(raw_data_clean$`BerufPos1[other]`)

# Study Subject  (Thesis Section 5.1.1.4)
#table(raw_data_clean$Studienfach)


## 4.1.4 Occupational Area (Thesis Section 5.1.1.4)

#table(raw_data_clean$Berufsfeld)
#table(raw_data_clean$Berufsfeld) / 113

#table(raw_data_clean$`Berufsfeld[other]`)


## Occupational Areas of Employees 

#table(raw_data_clean$Berufsfeld[raw_data_clean$BerufPos1 == 
#                                 "Arbeiter/in bzw. Angestellte/r"])

#table(raw_data_clean$Berufsfeld[raw_data_clean$BerufPos1 == 
#                                  "Arbeiter/in bzw. Angestellte/r"]) / 22

#table(raw_data_clean[raw_data_clean$BerufPos1 == "Arbeiter/in bzw. Angestellte/r"
#                     , 184])


## Occupational Areas of non students 

#table(raw_data_clean$Berufsfeld[raw_data_clean$BerufPos1 != "Studierende/r"])

#table(raw_data_clean$Berufsfeld[raw_data_clean$BerufPos1 != "Studierende/r"]) / 
#  (113- 79)


#table(raw_data_clean[raw_data_clean$BerufPos1 != "Studierende/r"
#                       , 184])


#table(raw_data_clean$Berufsfeld[raw_data_clean$BerufPos1 == 
 #                                 "Arbeiter/in bzw. Angestellte/r"]) /22



## 4.1.5 Education (Thesis Section 5.1.1.4)


#table(raw_data_clean$Edu1)
#table(raw_data_clean$Edu1) / 113

#table(raw_data_clean$`Edu1[other]`)




################################################################################


## 4.1.6 Self- Assessments (Thesis Section 5.1.1.5; Table 2)
library(psych)

# Create Data.frame with all self-assessment data
data_SCALI <- dplyr::select(raw_data_clean, matches(c("SCALI", "Gleichung")))

# Indices Scali-Scales

# Lib: SQ002, SQ004, SQ006, SQ008
# int: SQ001, SQ003, SQ005, SQ007
# C1: SQ009, SQ012, SQ013, SQ016
# Aes: SQ92900, SQ015, SQ43802, SQ010


# Internal Consistencies (Thesis Table 2, Alpha column)


#Lib
data_SCALI_Lib <- dplyr::select(data_SCALI, matches(c("002", "004",
                                               "006", "008", "Gleichung")))
data_SCALI_Lib$Sum <- rowSums(data_SCALI_Lib[, 1:4])

psych::alpha(data_SCALI_Lib[,1:4])

#Int
data_SCALI_Int <- dplyr::select(data_SCALI, matches(c("001", "003",
                                               "005", "007", "Gleichung")))
data_SCALI_Int$Sum <- rowSums(data_SCALI_Int[, 1:4])

psych::alpha(data_SCALI_Int[,1:4])


#Self-Efficacy
data_SCALI_C1 <- dplyr::select(data_SCALI, matches(c("009", "012",
                                               "013", "016", "Gleichung")))
data_SCALI_C1$Sum <- rowSums(data_SCALI_C1[, 1:4])

psych::alpha(data_SCALI_C1[,1:4])

#Artistic Interests
data_SCALI_Aes <- dplyr::select(data_SCALI, matches(c("929", "015",
                                               "438", "010", "Gleichung")))
data_SCALI_Aes$Sum <- rowSums(data_SCALI_Aes[, 1:4])

psych::alpha(data_SCALI_Aes[,1:4])


# MVs und SDs (Thesis Table 2, M and SD columns)

mean(data_SCALI_Lib$Sum)
sd(data_SCALI_Lib$Sum)

mean(data_SCALI_Int$Sum)
sd(data_SCALI_Int$Sum)


mean(data_SCALI_C1$Sum)
sd(data_SCALI_C1$Sum)


mean(data_SCALI_Aes$Sum)
sd(data_SCALI_Aes$Sum)



################################################################################



# 4.2 Sample Characteristics - subgroup comparisons (Thesis Section 5.1.3; Appendix D)



################################################################################
##
#  Important: 4.2.1 - 4.2.5 cannot be executed because demographic data has been omitted
#             in Data_Pilot.csv for data privacy reasons
#             - Hence, 4.2.4 - 4.2.5 was commented out in this script
##
################################################################################


## 4.2.1 Age Distribution

#tapply(raw_data_clean$G01Q10, raw_data_clean$Gleichung, mean)
#tapply(raw_data_clean$G01Q10, raw_data_clean$Gleichung, sd)


# ANOVA
# Check Assumptions

# Normal_Distribution: Boxplots
#boxplot(raw_data_clean$G01Q10 ~ raw_data_clean$Gleichung,
#        data = raw_data_clean)

# Data are not normally distributed

#kruskal_test_age <- kruskal.test(raw_data_clean$G01Q10 ~ raw_data_clean$Gleichung,
#           data = raw_data_clean)

#H <- kruskal_test_age$statistic
#k <- length(unique(raw_data_clean$Gleichung))
#n <- length(raw_data_clean$G01Q10)

# Calculate epsilon-squared
#epsilon_squared <- (H - k + 1) / (n - k)


## 4.2.2 gender distribution - social

#tapply(raw_data_clean$G01Q05, raw_data_clean$Gleichung, table)

# Fisher test

#Vec_gender_1 <- c(5, 1, 8) 
#Vec_gender_2 <- c(8, 0, 7)
#Vec_gender_3 <- c(8, 0, 15)
#Vec_gender_4 <- c(6,2,8)
#Vec_gender_5 <- c(4, 0, 13)
#Vec_gender_6 <- c(8, 1, 16)

#matrix_gender <- rbind(Vec_gender_1, Vec_gender_2, Vec_gender_3, Vec_gender_4,
#                    Vec_gender_5, Vec_gender_6)

#Fisher_test_gender <- fisher.test(matrix_gender)


# package lsr for Cramer's V
#if (!require(lsr)) install.packages("lsr")
#library(lsr)

# Calculate Cramér's V
#cramers_v_gender <- cramersV(matrix_gender)

# Print Cramér's V
#print(cramers_v_gender)







## 4.2.3 gender distribution - biological

#tapply(raw_data_clean$gender, raw_data_clean$Gleichung, table)


# Fisher test

# Create Nominal Distribution Matrix
#Vec_sex_1 <- c(1, 5, 8) 
#Vec_sex_2 <- c(0, 8, 7)
#Vec_sex_3 <- c(0, 8, 15)
#Vec_sex_4 <- c(0,8,10)
#Vec_sex_5 <- c(0, 4, 13)
#Vec_sex_6 <- c(1, 8, 16)

#matrix_sex <- rbind(Vec_sex_1, Vec_sex_2, Vec_sex_3, Vec_sex_4,
#                       Vec_sex_5, Vec_sex_6)

#Fisher_test_Job <- fisher.test(matrix_sex)
#cramers_v_gender <- cramersV(matrix_sex)

## 4.2.3 Occupation

#tapply(raw_data_clean$BerufPos1, raw_data_clean$Gleichung, table)


# Fisher test

#Vec_job_1 <- c(4, 2, 0, 7,1,0,0) 
#Vec_job_2 <- c(5, 1, 1, 8,0,0,0)
#Vec_job_3 <- c(3, 0, 1, 19,0,0,0)
#Vec_job_4 <- c(3, 0, 1, 14,0,1,0)
#Vec_job_5 <- c(2, 1, 0, 14,0,0,0)
#Vec_job_6 <- c(5, 0, 1, 17,0,1,1)

#matrix_job <- rbind(Vec_job_1, Vec_job_2, Vec_job_3, Vec_job_4,
#                    Vec_job_5, Vec_job_6)

#Fisher_test_Job <- fisher.test(matrix_job, workspace = 2e7, simulate.p.value = TRUE)
#chisq.test(matrix_job)

#cramers_v_job <- cramersV(matrix_job)


# Study subjects
#tapply(raw_data_clean$Studienfach, raw_data_clean$Gleichung, table)


## 4.2.4 Occupational Field

#tapply(raw_data_clean$Berufsfeld, raw_data_clean$Gleichung, table)


## 4.2.5 Education

#tapply(raw_data_clean$Edu1, raw_data_clean$Gleichung, table)

# Fisher test

#Vec_edu_1 <- c(7,7,0,0,0) 
#Vec_edu_2 <- c(7,7,1,0,0)
#Vec_edu_3 <- c(18,5,0,0,0)
#Vec_edu_4 <- c(13,5,0,0,1)
#Vec_edu_5 <- c(10,5,0,2,0)
#Vec_edu_6 <- c(12,12,1,0,0)

#matrix_edu <- rbind(Vec_edu_1, Vec_edu_2, Vec_edu_3, Vec_edu_4,
#                    Vec_edu_5, Vec_edu_6)

#Fisher_test_edu <- fisher.test(matrix_edu, workspace = 2e7)
#chisq.test(matrix_edu)

#cramers_v_edu <- cramersV(matrix_edu)


## 4.2.6 Self-Assessments


# MVs und SDs for different subgroups

tapply(data_SCALI_Lib$Sum, data_SCALI_Lib$Gleichung, mean)
tapply(data_SCALI_Lib$Sum, data_SCALI_Lib$Gleichung, sd)

tapply(data_SCALI_Int$Sum, data_SCALI_Int$Gleichung, mean)
tapply(data_SCALI_Int$Sum, data_SCALI_Int$Gleichung, sd)



tapply(data_SCALI_C1$Sum, data_SCALI_C1$Gleichung, mean)
tapply(data_SCALI_C1$Sum, data_SCALI_C1$Gleichung, sd)


tapply(data_SCALI_Aes$Sum, data_SCALI_Aes$Gleichung, mean)
tapply(data_SCALI_Aes$Sum, data_SCALI_Lib$Gleichung, sd)




# ANOVAs to test for subgroup differences

# Lib 

# Normal Distribution: Boxplots
boxplot(data_SCALI_Lib$Sum ~ data_SCALI_Lib$Gleichung,
        data = data_SCALI_Lib)

#Normal distribution: Shapiro Tests
shapiro_results_Lib <- tapply(data_SCALI_Lib$Sum,
                              as.factor(data_SCALI_Lib$Gleichung), 
                              shapiro.test)

# Homogenity of Variances: Leveene Test
leveneTest(data_SCALI_Lib$Sum ~ as.factor(data_SCALI_Lib$Gleichung),
           data = data_SCALI_Lib)

# Assumptions are met
# ANOVA

aov_lib <- aov(data_SCALI_Lib$Sum ~ as.factor(data_SCALI_Lib$Gleichung))
summary(aov_lib)

# Eta-Squared berechnen
etaSquared(aov_lib)




# Int

# Normal Distribution: Boxplots
boxplot(data_SCALI_Int$Sum ~ data_SCALI_Int$Gleichung,
        data = data_SCALI_Int)

#Normal distribution: Shapiro Tests
shapiro_results_Int <- tapply(data_SCALI_Int$Sum,
                              as.factor(data_SCALI_Int$Gleichung), 
                              shapiro.test)

# Homogenity of Variances: Leveene Test
leveneTest(data_SCALI_Int$Sum ~ as.factor(data_SCALI_Int$Gleichung),
           data = data_SCALI_Int)

# Assumptions are met
# ANOVA

aov_int <- aov(data_SCALI_Int$Sum ~ as.factor(data_SCALI_Int$Gleichung))
summary(aov_int)

# Eta-Squared berechnen
etaSquared(aov_int)







# Self-Efficacy

# Normal Distribution: Boxplots
boxplot(data_SCALI_C1$Sum ~ data_SCALI_C1$Gleichung,
        data = data_SCALI_C1)

#Normal distribution: Shapiro Tests
shapiro_results_C1 <- tapply(data_SCALI_C1$Sum,
                             as.factor(data_SCALI_C1$Gleichung), 
                             shapiro.test)

# Homogenity of Variances: Leveene Test
leveneTest(data_SCALI_C1$Sum ~ as.factor(data_SCALI_C1$Gleichung),
           data = data_SCALI_C1)

# Assumptions are met
# ANOVA

aov_C1 <- aov(data_SCALI_C1$Sum ~ as.factor(data_SCALI_C1$Gleichung))
summary(aov_C1)

# Eta-Squared berechnen
etaSquared(aov_C1)



# Artistic interests

# Normal Distribution: Boxplots
boxplot(data_SCALI_Aes$Sum ~ data_SCALI_Aes$Gleichung,
        data = data_SCALI_Aes)

#Normal distribution: Shapiro Tests
shapiro_results_Aes <- tapply(data_SCALI_Aes$Sum,
                              as.factor(data_SCALI_Aes$Gleichung), 
                              shapiro.test)

# Homogenity of Variances: Leveene Test
leveneTest(data_SCALI_Aes$Sum ~ as.factor(data_SCALI_Aes$Gleichung),
           data = data_SCALI_Aes)

# Assumptions are met
# ANOVA

aov_Aes <- aov(data_SCALI_Aes$Sum ~ as.factor(data_SCALI_Aes$Gleichung))
summary(aov_Aes)

# Eta-Squared berechnen
etaSquared(aov_Aes)




# Internal Consistency (Thesis Section 5.3) 

# overall, correlative


# Define Dataset with all item columns from the between_subject data
items_stats_b <- between_STA_data[, c(8:18, 79,80)]

#Calculate Cronbach's Alphas for Liberalism and Intellect
alphas_Lib_agg_between <- psych::alpha(dplyr::select(items_stats_b, matches(c("SQ002","SQ004","SQ006",
                                                              "SQ008"))))
alphas_Int_agg_between <- psych::alpha(dplyr::select(items_stats_b, matches(c("SQ001","SQ003","SQ005","SQ007"))))





# Manipulation Check

#C10: Item C05 Answers for Conditions with low self-efficacy

C10_vec_b <- items_stats_b$SQ009[items_stats_b$Gleichung %in% c(1,3,6)]
table(C10_vec_b)


hist(C10_vec_b, main = "Antworten Kompetenz: Bedingung wenig Kompetenz", 
     xlab = "Zustimmung", xlim = c(0,5))



#C11: Item C05 Answers for Conditions with high self-efficacy

C11_vec_b <- items_stats_b$SQ009[items_stats_b$Gleichung %in% c(2,4,5)]

table(C11_vec_b)


hist(C11_vec_b, main = "Antworten Kompetenz: Bedingung viel Kompetenz", 
     xlab = "Zustimmung", xlim = c(0,5))




#O0: Item O98 Answers for Conditions with low artistic interests
O0_vec_b <- items_stats_b$SQ010[items_stats_b$Gleichung %in% c(2,6)]

table(O0_vec_b)


hist(O0_vec_b, main = "Antworten Kompetenz: Bedingung wenig Ästhetik", 
     xlab = "Zustimmung", xlim = c(0,5))



#O1: Item O98 Answers for Conditions with high artistic interests

O1_vec_b <- items_stats_b$SQ010[items_stats_b$Gleichung %in% c(1,5)]

table(O1_vec_b)


hist(O1_vec_b, main = "Antworten Kompetenz: Bedingung viel Ästhetik", 
     xlab = "Zustimmung", xlim = c(0,5))

# OM:  Item O98 Answers for Conditions with moderate artistic interests

OM_vec_b <- items_stats_b$SQ010[items_stats_b$Gleichung %in% c(3,4)]

table(OM_vec_b)


hist(OM_vec_b, main = "Antworten Kompetenz: Bedingung mittel Ästhetik", 
     xlab = "Zustimmung", xlim = c(0,5))




# Correlations of Items c05 and their respective with Dummy-Variables


# Dummy-Variable for Self-Efficacy

# add Condition Variable 

items_stats_b_manicheck <- items_stats_b

D_C11 <- items_stats_b$Gleichung

for (i in 1:length(D_C11)) {
  if (D_C11[i] %in% c(2,4,5)) {
    D_C11[i] <- 1 }
  else {
    D_C11[i] <- 0
    
  }
}


D_O1 <- items_stats_b$Gleichung

for (i in 1:length(D_O1)) {
  if (D_O1[i] %in% c(1,5)) {
    D_O1[i] <- 1 }
  else {
    D_O1[i] <- 0
    
  }
}


# Dummy-Variable for Artistic Interests

D_OM <- items_stats_b$Gleichung

for (i in 1:length(D_OM)) {
  if (D_OM[i] %in% c(3,4)) {
    D_OM[i] <- 1 }
  else {
    D_OM[i] <- 0
    
  }
}


D_O0 <- items_stats_b$Gleichung

for (i in 1:length(D_O0)) {
  if (D_O0[i] %in% c(2,6)) {
    D_O0[i] <- 1 }
  else {
    D_O0[i] <- 0
    
  }
}


items_stats_b_manicheck$D_C11 <- as.numeric(D_C11)
items_stats_b_manicheck$D_O1 <- as.numeric(D_O1)
items_stats_b_manicheck$D_OM <- as.numeric(D_OM)
items_stats_b_manicheck$D_O0 <- as.numeric(D_O0)


# Manipulation check C11 (Thesis Section 5.6.1)

plot(items_stats_b_manicheck$SQ009, items_stats_b_manicheck$D_C11)
cor_C11_SQ09 <- cor.test(items_stats_b_manicheck$SQ009, items_stats_b_manicheck$D_C11)

# Manipulation check O1
cor_O1_SQ010 <- cor.test(items_stats_b_manicheck$SQ010, items_stats_b_manicheck$D_O1)
cor_O0_SQ010 <- cor.test(items_stats_b_manicheck$SQ010, items_stats_b_manicheck$D_O0)



#MVs and SDs for different Dummy Variable Levels (Thesis Section 5.6.1)

#Self-Efficacy and C05

#C10
mean(C10_vec_b)
sd(C10_vec_b)

#C11
mean(C11_vec_b)
sd(C11_vec_b)


#Artistic Interests and C89

#O0

mean(O0_vec_b)
sd(O0_vec_b)

# O1

mean(O1_vec_b)
sd(O1_vec_b)

# OM 

mean(OM_vec_b)
sd(OM_vec_b)






################################################################################
################################################################################



#### 5. Between Analysis


## 5.1 Tidying

# Exctract relevant columns from between_STA_data

between_STA_data2 <- data.frame(id = as.numeric(between_STA_data$id),
                                Gleichung = as.factor(between_STA_data$Gleichung),
                                LibSum = between_STA_data$LibSum, 
                                IntSum = between_STA_data$IntSum) %>%
# Open DV column

pivot_longer(
  cols = c("LibSum", "IntSum"),
  names_to = "AV",
  values_to = "Sum"
) %>%
  mutate(AV = ifelse(AV == "LibSum", 1, 
                     ifelse(AV == "IntSum", 2, AV)))

# Make DV column numeric

between_STA_data2$AV <- as.numeric(between_STA_data2$AV)

#as data.frame

between_STA_data2 <- as.data.frame(between_STA_data2)






## 5.2 STA plot and Stats (Thesis Sections 5.6.1 - 5.6.2)



###############################################################################
##
#
# For this section, The STACMR-R package by Dunn and Kalish (2020) needs to be
# installed properly following a tutorial.
# This tutorial can be accessed under: https://github.com/michaelkalish/STA/tree/master
# File Path: STACMR-R --> STACMR-R.pdf
#
##
################################################################################



source("staCMRsetup.R")


#partial order


E <- list(c(6,3,1), c(2,4,5))


# Thesis Table 3: Mean Values


STATS <- sta_stats(data = between_STA_data2, col_value = "Sum", col_participant = "id",
                   col_dv = "AV", col_between = "Gleichung")

# V1 = Liberalism, V2 = Intellect





# SEs (Thesis Table 3 SE columns) 

#Lib

SE_lib <- sqrt(1 / diag(STATS[[1]]$weights))

# Int
SE_int <- sqrt(1 / diag(STATS[[2]]$weights))


# SDs

# Liberalism
tapply(between_STA_data2$Sum[between_STA_data2$AV == 1], 
       data_SCALI_Int$Gleichung, sd)

# Intellect
tapply(between_STA_data2$Sum[between_STA_data2$AV == 2], 
       data_SCALI_Int$Gleichung, sd)




MR <- staMR(data = between_STA_data2, partial = E)


# CMR model predicted values and GoF value (Thesis Section 5.6.2) 
CMR <- staCMR(between_STA_data2, partial = E)


# CMR predicted means
#Lib
CMR$x[[1]]

#Int
CMR$x[[2]]


#CMR GoF value
CMR$fval


# Create STA Plot
sta_plot<- staPLOT(data = between_STA_data2, ylim = c(4,20), xlim = c(4,20),
                   groups = list(c(1,3,6), c(2,4,5), c(6,2), c(3,4), c(1,5)),  
                   grouplabels = list("Low Self-Efficacy", "High Self-Efficacy",
                                      "Low Aesthetics","Medium Aesthetics",
                                      "High Aesthetics"),
                   axislabels = list("Liberalität", "Intellektualität"),
                   pred = CMR[[1]])


# Custom STA PLot with shapes and colors to clarify IVs (Thesis Figure 8)

# ggplot 

library(ggplot2)

# Data for the Plot
x_means_main <- STATS[[1]]$means
y_means_main <- STATS[[2]]$means
errors_lib_main <- SE_lib
errors_int_main <- SE_int
col_main <- c("low", "high", "low",
              "high", "high", "low")
pch_main <- c("high", "low", "moderate"
              , "moderate", "high", "low")

x_iso_main <- CMR$x[[1]]
y_iso_main <- CMR$x[[2]]



# Dataframe for main data
data_plot_main <- data.frame(
  x_main = x_means_main,
  y_main = y_means_main,
  Self_Efficacy = factor(col_main,
                         levels = c("low",
                                    "high")),  #Define Factor Levels for Self-Efficacy
  Artistic_interests = factor(pch_main,
                              levels = c("low", 
                                         "moderate",
                                         "high")),   #Define Factor Levels for Artistic Interests
  
  x_min = x_means_main - SE_lib,  # Define lower and upper limits for error bars
  x_max = x_means_main + SE_lib,  
  y_min = y_means_main - SE_int,  
  y_max = y_means_main + SE_int,   
  x_iso = x_iso_main,
  y_iso = y_iso_main
)


# Create Dta Frame for regression model

data_iso_main <- data.frame(
  x_iso = x_iso_main,
  y_iso = y_iso_main
)


# ggplot
ggplot(data_plot_main, aes(x = x_main, y = y_main)) +
  geom_point(data = data_iso_main, aes(x = x_iso, y = y_iso), color = "grey",
             shape = 18, size = 3) +  # Add predicted Points (Regression)
  geom_line(data = data_iso_main, aes(x = x_iso, y = y_iso), color = "grey", linetype = "dashed") +  # grey curve for Regression model
  geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.2) +  # y-Axis Error bars
  geom_errorbarh(aes(xmin = x_min, xmax = x_max), height = 0.2) +  # Add x-Axis Error Bars
  geom_point(aes(shape = Artistic_interests, color = Self_Efficacy), size = 4) +  # Add mean values for Conditions; size = 4 for cex = 1.5
  scale_shape_manual(values = c(19, 17, 15)) +  # Shapes
  scale_color_manual(values = c("blue", "green")) +  # Colors
  
  
  labs(
    title = "STA-plot",
    x = "Liberalism",
    y = "Intellect",
    shape = "Artistic Interests",   # Legend Title for Artistic Interests
    color = "Self-Efficacy"          # Legend Title for Self-Efficacy
  ) +
  
  
  guides(
    color = guide_legend(
      override.aes = list(shape = c(35), color = c("blue", "green")),  # Customize Shapes for Legend
      order = 1  # Specify Order of IVs in Legend
    ),
    shape = guide_legend(
      order = 2  
    )
  ) +
  xlim(4, 20) +
  ylim(4, 20) +
  
  
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 16),  # Make Labels bigger
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        legend.title = element_text(size = 16),  
        legend.text = element_text(size = 14)  
  )






## 5.3 Bootstrap Significance Test:


# 5.3.1 Store Differences between original data MVs and CMR-Points

Diffs_Lib <-  CMR$x[[1]] - STATS[[1]]$means


Diffs_Int <-   CMR$x[[2]] - STATS[[2]]$means


Diffs <- data.frame(Diffs_Lib, Diffs_Int)
colnames(Diffs) <- c("Lib", "Int")



# 5.3.2 Shift Data Points (Equation: CMR - STATS = Diff)

between_Sim <- between_STA_data2

for (i in 1:nrow(between_STA_data2)) {
  if (between_Sim$Gleichung[i] == 1) {
    if (between_Sim$AV[i] == 1) {
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Lib[1] 
    }
    else {
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Int[1]
    }
  }
  else if (between_Sim$Gleichung[i] == 2) {
    if (between_Sim$AV[i] == 1) {
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Lib[2] 
    }
    else {
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Int[2]
    }
  }
  else if (between_Sim$Gleichung[i] == 3) {
    if (between_Sim$AV[i] == 1) {
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Lib[3] 
    }
    else { 
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Int[3]
    }
  }
  else if (between_Sim$Gleichung[i] == 4) {
    if (between_Sim$AV[i] == 1) {
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Lib[4] 
    }
    else { 
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Int[4]
    }
  }
  else if (between_Sim$Gleichung[i] == 5) {
    if (between_Sim$AV[i] == 1) {
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Lib[5] 
    }
    else { 
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Int[5]
    }
  }
  else {
    if (between_Sim$AV[i] == 1) {
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Lib[6] 
    }
    else {
      between_Sim$Sum[i] <- between_STA_data2$Sum[i] + Diffs$Int[6]
    }
  }
}
# Check:

sta_stats(between_Sim, col_value = "Sum", col_participant = "id",
          col_dv = "AV", col_between = "Gleichung")


# Check for unrealistic values

table(between_Sim$Sum >= 20)
table(between_Sim$Sum <= 4)


# 5.3.3 Create a bootstrap simulation function

sim_CMRfit <- function(data, n_sample, n_factor = 1, p_krit) {
  
  ## initialize List
  
  n <- c(14,15,23,19,17,25)*n_factor
  
  # List with Structure of Simulation Data of a single Simulation run
  # Elements 1 and 2 correspond to AVs
  # Individual Lists correspond to Conditions
  
  List_sim <- list(element1 = list(list1 = 1:n[1], list2 = 1:n[2], list3 = 1:n[3], 
                                   list4 = 1:n[4],list5 = 1:n[5], list6 = 1:n[6]),
                   element2 = list(list1 = 1:n[1], list2 = 1:n[2], list3 = 1:n[3],
                                   list4 = 1:n[4], list5 = 1:n[5], list6 = 1:n[6]))
  
  # Create Return List
  result_list <- list()
  result_list$List_fits <- list(1:n_sample)
  List_dfs <- list()             # Initialize List with bootstrap samples
  for (k in 1:n_sample) {
    
    dfs <- data.frame(I = 1:(2*sum(n)), G = 1:(2*sum(n)),
                      A = 1:(2*sum(n)), S = 1:(2*sum(n)))
    List_dfs[[k]] <- dfs
    
  }
  result_list$List_dfs <- List_dfs
  result_list$percentile <- 1 
  
# Draw Bootstrap values and list them
  
  for (i in 1:n_sample) {
    
    # Create List with Bootstrap values
    
    for (AV in 1:2) {
      for (Cond in 1: length(unique(data$Gleichung))) {

        List_sim[[AV]][[Cond]] <- sample(data[data$Gleichung == Cond & data$AV == AV,4], 
                                         size = n[Cond], replace = TRUE)
      }
    }
    
    
    # Now assemble Simulation Data data frame
    
    Id_sim <- rep(1:sum(n), times = 2)
    Sum_sim <- unlist(List_sim)  
    Gleichung_sim <- rep(rep(1:6, times = c(n[1], n[2], n[3], n[4], n[5], n[6])), times = 2)
    AV_sim <- rep(1:2, times = c(sum(n), sum(n)))
    
    Df_sim <- data.frame(Id_sim, Gleichung_sim, AV_sim, Sum_sim)
    
    STATS_sim <- sta_stats(Df_sim, col_value = "Sum_sim", col_participant = "Id_sim",
                           col_dv = "AV_sim", col_between = "Gleichung_sim")
    
    CMR_sim <- staCMR(Df_sim)
    
    #debugging
    
    print(i)  
    print(Df_sim)  
    print(STATS_sim)  
    print(CMR_sim)  
    
    
    
    result_list$List_dfs[[i]] <- Df_sim
    result_list$List_fits[i] <- CMR_sim$fval
    
    
  }
  
  
  # Add Percentile
  
  cdf <- ecdf(unlist(result_list$List_fits))
  value_p <- quantile(unlist(result_list$List_fits), p_krit)
  
  result_list$percentile <-  value_p
  
  cat("Percentage of data below the 95th percentile:", 100 * cdf(value_p), "%\n")
  
  return(result_list)
  
}



# 5.3.4 Excecute Simulation Function and calculate p-value (Thesis Section 5.6.2)

# Set Seed for reproducability

set.seed(9)


List_fits_result_1 <- sim_CMRfit(data = between_Sim, n_sample = 10000,
                                 n_factor = 1, p_krit = 0.95)

# 95th percentile of the bootstrap GoF value distribution (critical GoF-value)
p95 <- List_fits_result_1$percentile


# Visualize Bootstrap GoF value distribution



hist(unlist(List_fits_result_1$List_fits), xlab = "CMR model fit", main = "Bootstrapped sample distribution of fit values",
     breaks = 40)
abline(v = CMR$fval, lwd = 2, col = "blue") # empirical Fit-Value
abline(v = p95 , lwd = 2, col = "green")    # critical Fit-Value

# note: Result is significant, if blue empirical GoF exceeds or equals green critical GoF


# Determine p-value 

p_Wert_b <- mean(unlist(List_fits_result_1$List_fits) >= CMR$fval)
p_Wert_b

################################################################################
################################################################################

#### 6. within Analysis (Thesis Section 5.7)

### 6.1 Lib and Int Sums for within format

reg_exps_within_sums <- c("C10O1\\[", "C11O0\\[", "C10OM\\[",
                          "C11OM\\[", "C11O1\\[", "C10O0\\[")
ws_sums <- list()


for (i in reg_exps_within_sums) {
  ws_sums[[i]] <- list()
  ws_sums[[i]]$Lib <- apply(dplyr::select(within_STA_data, matches(i) & 
                                     contains(Lib_items)), MARGIN = 1, FUN = sum)
  ws_sums[[i]]$Int <- apply(dplyr::select(within_STA_data, matches(i) & 
                                     contains(Int_items)), MARGIN = 1, FUN = sum)
  }

within_STA_data$C10O1_LibSum <- ws_sums[[1]]$Lib
within_STA_data$C10O1_IntSum <- ws_sums[[1]]$Int

within_STA_data$C11O0_LibSum <- ws_sums[[2]]$Lib
within_STA_data$C11O0_IntSum <- ws_sums[[2]]$Int

within_STA_data$C10OM_LibSum <- ws_sums[[3]]$Lib
within_STA_data$C10OM_IntSum <- ws_sums[[3]]$Int

within_STA_data$C11OM_LibSum <- ws_sums[[4]]$Lib
within_STA_data$C11OM_IntSum <- ws_sums[[4]]$Int

within_STA_data$C11O1_LibSum <- ws_sums[[5]]$Lib
within_STA_data$C11O1_IntSum <- ws_sums[[5]]$Int

within_STA_data$C10O0_LibSum <- ws_sums[[6]]$Lib
within_STA_data$C10O0_IntSum <- ws_sums[[6]]$Int




### 6.2 tyding

# Exclude Subject VP 89

within_STA_data <- within_STA_data[-(within_STA_data$id == "89"),]
  
# Extract Relevant Columns from within_STA_data

within_STA_data2 <- data.frame(id = as.numeric(within_STA_data$id),
                                Gleichung = as.factor(within_STA_data$Gleichung),
                                within_STA_data[, grep("Sum",colnames(within_STA_data))]) %>%
  

# Bring Data into long format
pivot_longer(
  cols = contains("Sum"),
  names_to = "Condition",
  values_to = "values"
) %>%
  
  # Split Condition- and Itemcode
  separate(Condition, into = c("Bedingung","Scale"),
           sep = "_") %>%
  
  # Make Data wider again
  pivot_wider(names_from = Bedingung, values_from = values)

# as data.frame
within_STA_data2 <- as.data.frame(within_STA_data2)



# Set between Condition to 1

within_STA_data2$Gleichung <- 1


# Numerize Libsum und Intsum : Lib = 1; Int = 2

within_STA_data2$Scale[within_STA_data2$Scale == "LibSum"] <- 1
within_STA_data2$Scale[within_STA_data2$Scale == "IntSum"] <- 2







## 6.3 STA plot, Stats and CMR model (Thesis Section 5.7) 



###############################################################################
##
#
# For this section, The STACMR-R package by Dunn and Kalish (2020) needs to be
# installed properly following a tutorial.
# This tutorial can be accessed under: https://github.com/michaelkalish/STA/tree/master
# File Path: STACMR-R --> STACMR-R.pdf
#
##
################################################################################


source("staCMRsetup.R")


#partial order


E <- list(c(6,3,1), c(2,4,5))



# Thesis Section 5.7.1, Table 4: Mean value column
STATS_w1 <- staSTATS(data=within_STA_data2, varnames=list("C10O1", "C11O0", "C10OM",
                                            "C11OM", "C11O1", "C10O0"),shrink= 0)


# Lib
STATS_w1[[1]]$means
# Int
STATS_w1[[2]]$means



MR_w1 <- staMR(data = within_STA_data2, partial = E)


# Thesis Section 5.7.2, CMR model predictions and GoF value

CMR_w1 <- staCMR(within_STA_data2, partial = E, shrink = 0)

# predicted mean values (CMR)
CMR_w1$x[[1]] # Lib
CMR_w1$x[[2]] #Int


# GoF value
CMR_w1$fval



# Create Sta plot
sta_plot_w1<- staPLOT(data=within_STA_data2, groups= list(c(6,3,1), c(2,4,5)), 
grouplabels=list("Kompetenz gering","Kompetenz hoch"),
                   axislabels = list("Liberalität", "Intellektualität"),
                   xlim= c(4,20), ylim= c(4,20), 
                   pred=CMR_w1[[1]], palette="Set1")



# SEs (Thesis Section 5.7.1, Table 4; SE columns)

SE_Lib_w <- list()
SE_Int_w <- list()

## Cond 1

# Lib
SE_Lib_w[[1]] <- sd(within_STA_data2$C10O1[within_STA_data2$Scale ==1]) / sqrt(0.5*nrow(within_STA_data2))

# Int
SE_Int_w[[1]] <- sd(within_STA_data2$C10O1[within_STA_data2$Scale ==2]) / sqrt(0.5*nrow(within_STA_data2))


## Cond 2

# Lib
SE_Lib_w[[2]] <- sd(within_STA_data2$C11O0[within_STA_data2$Scale ==1]) / sqrt(0.5*nrow(within_STA_data2))

# Int
SE_Int_w[[2]] <- sd(within_STA_data2$C11O0[within_STA_data2$Scale ==2]) / sqrt(0.5*nrow(within_STA_data2))


## Cond 3

# Lib
SE_Lib_w[[3]] <- sd(within_STA_data2$C10OM[within_STA_data2$Scale ==1]) / sqrt(0.5*nrow(within_STA_data2))

# Int
SE_Int_w[[3]] <- sd(within_STA_data2$C10OM[within_STA_data2$Scale ==2]) / sqrt(0.5*nrow(within_STA_data2))


## Cond 4

# Lib
SE_Lib_w[[4]] <- sd(within_STA_data2$C11OM[within_STA_data2$Scale ==1]) / sqrt(0.5*nrow(within_STA_data2))

# Int
SE_Int_w[[4]] <- sd(within_STA_data2$C11OM[within_STA_data2$Scale ==2]) / sqrt(0.5*nrow(within_STA_data2))


## Cond 5

# Lib
SE_Lib_w[[5]] <- sd(within_STA_data2$C11O1[within_STA_data2$Scale ==1]) / sqrt(0.5*nrow(within_STA_data2))

# Int
SE_Int_w[[5]] <- sd(within_STA_data2$C11O1[within_STA_data2$Scale ==2]) / sqrt(0.5*nrow(within_STA_data2))


## Cond 6

# Lib
SE_Lib_w[[6]] <- sd(within_STA_data2$C10O0[within_STA_data2$Scale ==1]) / sqrt(0.5*nrow(within_STA_data2))

# Int
SE_Int_w[[6]] <- sd(within_STA_data2$C10O0[within_STA_data2$Scale ==2]) / sqrt(0.5*nrow(within_STA_data2))






## cutom ggplot STA_plot (Thesis Figure 9)


# ggplot 

library(ggplot2)

# Datn für STA Plot
x_means_main <- STATS_w1[[1]]$means
y_means_main <- STATS_w1[[2]]$means
errors_lib_main <- SE_Lib_w
errors_int_main <- SE_Int_w
col_main <- c("low", "high", "low",       # Defie Factor Levels for IVs
              "high", "high", "low")
pch_main <- c("high", "low", "moderate"
              , "moderate", "high", "low")

x_iso_main <- CMR_w1$x[[1]]
y_iso_main <- CMR_w1$x[[2]]



# Data frame für Mittelwerte
data_plot_main <- data.frame(
  x_main = x_means_main,
  y_main = y_means_main,
  Self_Efficacy = factor(col_main,
                         levels = c("low", "high")),  # Define Factor levels
  Artistic_interests = factor(pch_main,
                          levels = c("low", "moderate", "high")),   
  x_min = x_means_main - SE_lib,   # Set Limits for error bars
  x_max = x_means_main + SE_lib,   
  y_min = y_means_main - SE_int,   
  y_max = y_means_main + SE_int,   
  x_iso = x_iso_main,
  y_iso = y_iso_main
)


# Data Frame for CMR regression model 

data_iso_main <- data.frame(
  x_iso = x_iso_main,
  y_iso = y_iso_main
)


# ggplot
ggplot(data_plot_main, aes(x = x_main, y = y_main)) +
  geom_point(data = data_iso_main, aes(x = x_iso, y = y_iso), color = "grey",
             shape = 18, size = 3) +  # Add Regression points
  geom_line(data = data_iso_main, aes(x = x_iso, y = y_iso), color = "grey", linetype = "dashed") +  # grey regression curve
  geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.2) +     # y-Axis Error bars
  geom_errorbarh(aes(xmin = x_min, xmax = x_max), height = 0.2) +   # x-Axis error bars
  geom_point(aes(shape = Artistic_interests, color = Self_Efficacy), size = 4) +   # MVs; size = 4 for cex = 1.5
  scale_color_manual(values = c("blue", "green")) +    # Colors
  scale_shape_manual(values = c(19, 17, 15)) +         # Shapes
  labs(
    title = "STA-plot",
    x = "Liberalism",
    y = "Intellect",
    color = "Self-Efficacy",      # Legend: Self-efficacy
    shape = "Artistic Interests"  # Legend: Artistic Intereste
  ) +
  guides(
    color = guide_legend(
      override.aes = list(shape = c(35), color = c("blue", "green")),  # customize Legend 
    order = 1),
    shape = guide_legend(
      order = 2
    )
  ) +
  xlim(4, 20) +
  ylim(4, 20) +
  theme_minimal() + 
#  guides(
#    shape = guide_legend(order = 2),   
#    color = guide_legend(order = 1)) +
  theme(axis.title.x = element_text(size = 16),  # Enlarge Labels
        axis.title.y = element_text(size = 16),  
        axis.text.x = element_text(size = 12),  
        legend.title = element_text(size = 16),  
        legend.text = element_text(size = 14) 
  )









################################################################################




## 6.4 Bootstrap Significance test:


# 6.4.1 Differences between original Data MVs and CMR-Points

Diffs_Lib_w1 <-  CMR_w1$x[[1]] - STATS_w1[[1]]$means


Diffs_Int_w1 <-   CMR_w1$x[[2]] - STATS_w1[[2]]$means


Diffs_w1 <- data.frame(Diffs_Lib_w1, Diffs_Int_w1)
colnames(Diffs_w1) <- c("Lib", "Int")



# 6.4.2 Shift Data Poits (Gleichung: CMR - STATS = Diff)

within_Sim <- within_STA_data2

within_Sim$Scale <- as.numeric(within_Sim$Scale)

for (i in 1:nrow(within_STA_data2)) {
    if (within_Sim$Scale[i] == 1) {
      within_Sim$C10O1[i] <- within_STA_data2$C10O1[i] + Diffs_w1$Lib[1]
      within_Sim$C11O0[i] <- within_STA_data2$C11O0[i] + Diffs_w1$Lib[2]
      within_Sim$C10OM[i] <- within_STA_data2$C10OM[i] + Diffs_w1$Lib[3]
      within_Sim$C11OM[i] <- within_STA_data2$C11OM[i] + Diffs_w1$Lib[4]
      within_Sim$C11O1[i] <- within_STA_data2$C11O1[i] + Diffs_w1$Lib[5]
      within_Sim$C10O0[i] <- within_STA_data2$C10O0[i] + Diffs_w1$Lib[6]
    }
    else {
      within_Sim$C10O1[i] <- within_STA_data2$C10O1[i] + Diffs_w1$Int[1]
      within_Sim$C11O0[i] <- within_STA_data2$C11O0[i] + Diffs_w1$Int[2]
      within_Sim$C10OM[i] <- within_STA_data2$C10OM[i] + Diffs_w1$Int[3]
      within_Sim$C11OM[i] <- within_STA_data2$C11OM[i] + Diffs_w1$Int[4]
      within_Sim$C11O1[i] <- within_STA_data2$C11O1[i] + Diffs_w1$Int[5]
      within_Sim$C10O0[i] <- within_STA_data2$C10O0[i] + Diffs_w1$Int[6]
    }
  }
 
# Check:

staSTATS(data=within_Sim, varnames=list("C10O1", "C11O0", "C10OM",
                                              "C11OM", "C11O1", "C10O0"),
                                               shrink= 0)

staCMR(within_Sim, partial = E, shrink = 0)



# Check for unrealistic values

table(within_Sim$Sum >= 20)
table(within_Sim$Sum <= 4)


# 6.4.3 Create Simulation Function

sim_CMRfit_w <- function(data, n_sample, n_factor = 1, p_krit) {
  
  ## Initialize List
  
  n <- 112*n_factor
  result_list <- list()
  
    
    
List_fits <- list()             # Initialize List with Bootstrap Data frames
    
    
for (i  in 1:n_sample) {
  
# ssample Rows    
    
sampled_rows <- sample(data$id, size = n, replace = TRUE)
  
# Bootstrap sample 

# Sample Data for Scales

# Lib
data_bootstrap_Lib <- data[data$Scale =="1", ]

for (j in 1:n) {
data_bootstrap_Lib[j,] <- data[data$id == sampled_rows[[j]] &  data$Scale == "1",]
}

# Int
data_bootstrap_Int <- data[data$Scale =="2", ]

for (j in 1:n) {
  data_bootstrap_Int[j,] <- data[data$id == sampled_rows[[j]] &  data$Scale == "2",]
}

# Merge Data samples

df_sim <- rbind(data_bootstrap_Lib, data_bootstrap_Int)
List_fits[[i]] <- staCMR(df_sim, partial = E, shrink = 0)$fval


}
    
  
  # Add percentile
  
  value_p <- quantile(unlist(List_fits), p_krit)
  
  result_list$percentile <-  value_p
  
  result_list$p_table <- table(unlist(List_fits) > CMR_w1$fval)
  
  # Add p-value
  
  result_list$p_val <- mean(unlist(List_fits) > CMR_w1$fval)
  
  return(result_list)
  
}





# Apply function: Calculate p-Value (Thesis Section 5.7.2)

set.seed(10)

List_fits_result_w <- sim_CMRfit_w(data = within_Sim, n_sample = 10000,
                                   n_factor = 1, p_krit = .95)

# P-Val

List_fits_result_w$p_val
