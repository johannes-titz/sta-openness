# set working directory
setwd("C:/Users/bened/OneDrive/Desktop/TUC/Wintersemester 202223/Masterarbeit/R_Analysen/STA-master/STA-master/STACMR-R")

# The folder "STACMR-R" needs to be the working directory. This folder is part
# of the STACMR-R package by Dunn and Kalish (2020), that needs to be
# installed properly following a tutorial.
# This tutorial can be accessed under: https://github.com/michaelkalish/STA/tree/master
# File Path: STACMR-R --> STACMR-R.pdf

#### 0. Pakete
# install if necessary
library(readr)
library(dplyr)
library(tidyr)
library(psych)  

install.packages(devtools)
library(devtools)

devtools::install_github("monotonicity/stacmr", force = TRUE)

library(stacmr)
install.packages("tidyverse")
library(tidyverse)




#### 1. Read in Data

library(readr)
survey_Data_Pilot_raw <- read_csv("C:/Users/bened/OneDrive/Desktop/TUC/Wintersemester 202223/Masterarbeit/Daten_MA/Data_Pilot.csv")
View(survey_Data_Pilot_raw)

# Experimental Conditions:
#C10O1: Condition 1
#C11O0: Condition 2
#C11O1: Condition 3
#C10O0: Condition 4


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

#Item O89
#SQ010

#Item C05
#SQ009

#### 2. Tidy Data


# 2.1 Delete unnecessary columns

colnames(survey_Data_Pilot_raw) 

del_cols_rawData  <- c("submitdate", "lastpage", "startlanguage", "seed",
                       "gender", "Gleichunggendercase", "GleichunggendercaseTime",
                       "GleichungTime") 


survey_data_tidy <- survey_Data_Pilot_raw[,
                    !names(survey_Data_Pilot_raw) %in% del_cols_rawData]


# 2.1 Convert response codes (A*) to numeric values from 1-5

library(dplyr)

survey_data_tidy <- replace(survey_data_tidy, survey_data_tidy == "A1", "1") %>%
replace(survey_data_tidy == "A2", "2") %>%
  replace(survey_data_tidy == "A3", "3") %>%
  replace(survey_data_tidy == "A4", "4") %>%
  replace(survey_data_tidy == "A5", "5")


# Define data columns as numeric

colnames(survey_data_tidy)

#convert NAs to 0s:

survey_data_tidy[is.na(survey_data_tidy)] <- "0"

# Loop

#Select all question Items from the survey's main part...

library(dplyr)

cols_likert <- dplyr::select(survey_data_tidy, matches("SQ"))

#... and make them all numeric
for (i in c(1:ncol(survey_data_tidy))) {
  if (colnames(survey_data_tidy)[[i]] %in% colnames(cols_likert)) {
    survey_data_tidy[[i]] <- as.numeric(survey_data_tidy[[i]])
  }
}


# 2.3 Reverse negative coded items

#SQ003
#SQ005
#SQ006
#SQ007
#SQ008

# Create Data.Frame for all columns with negative coded items


cols_negCod <- dplyr::select(survey_data_tidy, matches(c("SQ003","SQ005","SQ006",
                                                  "SQ007","SQ008")))

# Create Index for all columns with negative coded items in survey data tidy
Index_negCod <- which(survey_data_tidy %in% cols_negCod)


# Invert items: response value - 6 
survey_data_tidy[,Index_negCod] <- 6 - survey_data_tidy[,Index_negCod]

# 6 now signifies NA

# repllace 6 with 0 again


for (i in Index_negCod) {
  for (j in 1:length(survey_data_tidy$`C10O1[SQ001]`)) {
    if (survey_data_tidy[j,i] == 6) {
      survey_data_tidy[j,i] <- 0 
    }
    
  }
  }
  



# 2.3 Bring Data into STA-Plot Format


# 2.3.1 Add Sum Scores for Int and Lib


#Lib Items:

#002
#004
#006
#008

#Int Items:
#001
#003
#005
#007

#C1
#009

#Aest
#010


# Define Function to add Lib and Int Scores to Conditions

sumscoreSTA <- function(dat, condition) {
  
  sel_cols <- dplyr::select(dat, matches(condition)) %>%
    dplyr::select(-matches("Time")) %>%
    dplyr::select(-matches("sum"))
  
  
# Calculate Sum Score
#First, create data frames with all liberalism and intellect item columns  
Lib_cols <- dplyr::select(sel_cols, matches(c("SQ002", "SQ004", "SQ006", "SQ008")))
Int_cols <- dplyr::select(sel_cols, matches(c("SQ001", "SQ003", "SQ005", "SQ007")))
# Calculate sum
sumLib <- apply(Lib_cols, MARGIN = 1, FUN = sum)
sumInt <- apply(Int_cols, MARGIN = 1, FUN = sum)

# Create List of Variables
List_sum <- list(sumLib, sumInt)

# Name List
List_name <- paste(condition,"sums")
assign(List_name, List_sum)

rm(List_sum)

return(get(List_name))
}

# Apply sum function to variables for all conditions each
#   1   C10O1
Liste_C10O1 <- sumscoreSTA(dat = survey_data_tidy, condition = "C10O1")

#   2   C11O0
Liste_C11O0 <- sumscoreSTA(dat = survey_data_tidy, condition = "C11O0")

#   3   C11O1
Liste_C11O1 <- sumscoreSTA(dat = survey_data_tidy, condition = "C11O1")

#   4   C10O0
Liste_C10O0 <- sumscoreSTA(dat = survey_data_tidy, condition = "C10O0")



# Add Liberalism sum columns to survey data tidy 

survey_data_tidy$sumLib_C10O1 <- unlist(Liste_C10O1[1])
survey_data_tidy$sumLib_C11O0 <- unlist(Liste_C11O0[1])
survey_data_tidy$sumLib_C11O1 <- unlist(Liste_C11O1[1])
survey_data_tidy$sumLib_C10O0 <- unlist(Liste_C10O0[1])

# Add Intellect sum columns to survey data tidy 

survey_data_tidy$sumInt_C10O1 <- unlist(Liste_C10O1[2])
survey_data_tidy$sumInt_C11O0 <- unlist(Liste_C11O0[2])
survey_data_tidy$sumInt_C11O1 <- unlist(Liste_C11O1[2])
survey_data_tidy$sumInt_C10O0 <- unlist(Liste_C10O0[2])



# Add Liberalsim and Intellect sum scores

survey_data_tidy$sumLib <- apply(dplyr::select(survey_data_tidy, 
                                matches("sumLib")), MARGIN = 1, FUN = sum)

survey_data_tidy$sumInt <- apply(dplyr::select(survey_data_tidy, 
                                matches("sumInt")), MARGIN = 1, FUN = sum)



# Create Data subset for STA-Analysis relevant Data

data_STA <- dplyr::select(survey_data_tidy, id, Gleichung, sumInt, sumLib)




# Convert from wide to long format

library(tidyr)

long_data_STA <- gather(data_STA, key = "AV", value = "Sum", -id, -Gleichung)

long_data_STA$AV <- as.numeric(as.factor(long_data_STA$AV))



# convert to integer



# convert long data to integer

long_data_STA$id <- as.integer(unlist(long_data_STA$id))
long_data_STA$Gleichung <- as.integer(unlist(long_data_STA$Gleichung))
long_data_STA$AV <- as.integer(unlist(long_data_STA$AV))
long_data_STA$Sum <- as.integer(unlist(long_data_STA$Sum))


# Convert "Gleichung" and "AV" to factors

long_data_STA$Gleichung <- as.factor(unlist(long_data_STA$Gleichung))
long_data_STA$AV <- as.factor(unlist(long_data_STA$AV))
long_data_STA$id <- as.factor(unlist(long_data_STA$id))



# longdataSTA as data.frame

long_data_STA <- as.data.frame(long_data_STA)



# normalize id column

long_data_STA$id <- rep(1:42, times = 2)


# Adapt AV Index to staPLOT() norms 

#Lib = AV1
#Int = AV2


AV_inv <- ifelse(long_data_STA$AV == 1, 2, 
                 ifelse(long_data_STA$AV == 2, 1, long_data_STA$AV))

long_data_STA$AV <- AV_inv

#### 3. Eliminate outliers

boxplot(survey_data_tidy$groupTime101782)
mean(survey_data_tidy$groupTime101782)
sd(survey_data_tidy$groupTime101782)
range(survey_data_tidy$groupTime101782)

# No elimination needed



### 4. Descriptive Analysis

# 4.1 Sample CHaracteristics (Thesis Section 4.1)



################################################################################
##
#  Important: 4.1 cannot be excecuted because demographic data has been omitted
#             in Data_Pilot.csv for data privacy reasons
#             - Hence, 4.1 was commented out in this script
##
################################################################################


#4.1.1 Overall Sample

# Gender distribution
#table(survey_data_tidy$G01Q05)


# Mean Age and SD
#mean(survey_data_tidy$G01Q10)
#sd(survey_data_tidy$G01Q10)



#4.1.2 Subsample sizes (see Thesis Section 4.3)

#Number of Subjects per Experimental Condition
table(survey_data_tidy$Gleichung)



# 4.2 Internal Consistencies of DV scales (Thesis Section 4.3)

# Cronbachs alphas 

#install.packages("psych")  # Install the psych package
library(psych)  
library(dplyr)

# first: Create subsets for individual Conditions 1-4

survey_data_tidy_Bed_1 <-  survey_data_tidy[survey_data_tidy$Gleichung == 1,] %>%
  dplyr::select(matches(c("C10O1", "Gleichung"))) %>%
  dplyr::select(-matches("time")) 


survey_data_tidy_Bed_2 <-  survey_data_tidy[survey_data_tidy$Gleichung == 2,]%>%
  dplyr::select(matches(c("C11O0", "Gleichung"))) %>%
  dplyr::select(-matches("time")) 


survey_data_tidy_Bed_3 <-  survey_data_tidy[survey_data_tidy$Gleichung == 3,]%>%
  dplyr::select(matches(c("C11O1", "Gleichung"))) %>%
  dplyr::select(-matches("time")) 


survey_data_tidy_Bed_4 <-  survey_data_tidy[survey_data_tidy$Gleichung == 4,]%>%
  dplyr::select(matches(c("C10O0", "Gleichung"))) %>%
  dplyr::select(-matches("time")) 




# Crobach's Alphas


# Norm Column names first

Col_names <- c("SQ001", "SQ002", "SQ003", "SQ004", "SQ010", "SQ005",
               "SQ006", "SQ009", "SQ007", "SQ008", "Sum_Lib", "Sum_Int", "Gleichung")

# Create data.frames with same column names for merging 
merger_Bed1 <- survey_data_tidy_Bed_1
colnames(merger_Bed1) <- Col_names

merger_Bed2 <- survey_data_tidy_Bed_2
colnames(merger_Bed2) <- Col_names

merger_Bed3 <- survey_data_tidy_Bed_3
colnames(merger_Bed3) <- Col_names

merger_Bed4 <- survey_data_tidy_Bed_4
colnames(merger_Bed4) <- Col_names


# merge data.frames

merged_items <- rbind(merger_Bed1, merger_Bed2, merger_Bed3, merger_Bed4)


# Alphas for aggregated data

alphas_Lib_agg <- psych::alpha(dplyr::select(merged_items, matches(c("SQ002","SQ004","SQ006",
                                                              "SQ008"))))
alphas_Int_agg <- psych::alpha(dplyr::select(merged_items, matches(c("SQ001","SQ003","SQ005",
                                                              "SQ007"))))



# 4.3 Manipulation Check (Thesis Section 4.6.1)


#C10: Check Item C05 responses for Conditions with low self-efficacy

#get all C05 item answers for low self-efficacy conditions 
C10_vec <- c(survey_data_tidy$`C10O0prox[SQ009]`, survey_data_tidy$`C10O1[SQ009]`)

table(C10_vec)

# omit 0 values from high-self-efficacy subjects
new_C10_vec <- C10_vec[C10_vec != 0]

hist(new_C10_vec, main = "Antworten Kompetenz: Bedingung wenig Kompetenz", 
     xlab = "Zustimmung", xlim = c(0,5))



#C11: Check Item C05 responses for Conditions with high self-efficacy

#get all C05 item answers for high self-efficacy conditions 
C11_vec <- c(survey_data_tidy$`C11O0prox[SQ009]`, survey_data_tidy$`C11O1prox[SQ009]`)

table(C11_vec)

# omit 0 values from low self-efficacy subjects
new_C11_vec <- C11_vec[C11_vec != 0]


hist(new_C11_vec, main = "Antworten Kompetenz: Bedingung viel Kompetenz",
     xlab = "Zustimmung", xlim = c(0,5))




#O0 Check Item O89 responses for Conditions with low artistic interests
O0_vec <- c(survey_data_tidy$`C11O0prox[SQ010]`, survey_data_tidy$`C10O0prox[SQ010]`)

table(O0_vec)

new_O0_vec <- O0_vec[O0_vec != 0]


hist(new_O0_vec, main = "Antworten Ästhetik: Bedingung wenig Ästhetik",
     xlab = "Zustimmung", xlim = c(0,5))


#O0 Check Item O89 responses for Conditions with high artistic interests

O1_vec <- c(survey_data_tidy$`C10O1[SQ010]`, survey_data_tidy$`C11O1prox[SQ010]`)

new_O1_vec <- O1_vec[O1_vec != 0]


hist(new_O1_vec, main = "Antworten Ästhetik: Bedingung viel Ästhetik",
     xlab = "Zustimmung", xlim = c(0,5))


# Correlation with Dummy-Variable


# Create Dummy-Variable for Self-Efficacy


# Add Condition Variable to merged_items data frame

merged_items_manicheck <- merged_items

D_C11 <- merged_items_manicheck$Gleichung

for (i in 1:length(D_C11)) {
  if (D_C11[i] %in% c(2,3)) {
    D_C11[i] <- 1 }
  else {
    D_C11[i] <- 0
    
  }
}


D_O1 <- merged_items_manicheck$Gleichung

for (i in 1:length(D_O1)) {
  if (D_O1[i] %in% c(1,3)) {
    D_O1[i] <- 1 }
  else {
    D_O1[i] <- 0
    
  }
}

#Add Dummy Variable to Data
merged_items_manicheck$D_C11 <- D_C11 # self-efficacy 
merged_items_manicheck$D_O1 <- D_O1 # artistic interests


# Manipulation check Self-efficacy and O5 (Thesis Section 4.6.1, p.36)
cor_C11_SQ09 <- cor.test(merged_items_manicheck$SQ009, merged_items_manicheck$D_C11)

# Manipulation check O1 (Thesis Section 4.6.1, p.36)
cor_C11_SQ10 <- cor.test(merged_items_manicheck$SQ010, merged_items_manicheck$D_O1)



#MVs and SDs for Manipulation Check Items for different Dummy Variable Levels (Thesis Section 4.6.1, p.36)

#C10
mean(new_C10_vec)
sd(new_C10_vec)

#C11
mean(new_C11_vec)
sd(new_C11_vec)

#O0

mean(new_O0_vec)
sd(new_O0_vec)

# O1

mean(new_O1_vec)
sd(new_O1_vec)




##### 5. STA_Analyses (Thesis Section 4.6)


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


#set_wd

setwd("C:/Users/bened/OneDrive/Desktop/TUC/Wintersemester 202223/Masterarbeit/R_Analysen/STA-master/STA-master/STACMR-R")
source("staCMRsetup.R")
# The folder "STACMR-R" needs to be the working directory. 



# partial order

E <- list(c(4,1), c(2,3))




# 5.1 State Trace and CMR model

# get STATS for the state trace (Thesis Table 1, mean values)
STATS <- sta_stats(data = long_data_STA, col_value = "Sum", col_participant = "id",
          col_dv = "AV", col_between = "Gleichung")

#Liberalism = V1, Intelllect = V2


#calculate CMR regression model fit and GoF value Thesis Section 4.6.2)
CMR <- staCMR(long_data_STA, partial = E)

#Liberalism
CMR$x[[1]]

#Intellect
CMR$x[[2]]


#Create STA-Plot
sta_plot<- staPLOT(long_data_STA, ylim = c(4,20), xlim = c(4,20),
                   groups = list(c(1,4), c(2,3)), 
                   grouplabels = list("Kompetenz gering","Kompetenz hoch"),
                   axislabels = list("Liberalität", "Intellektualität"),
                   pred = CMR[[1]])

#GoF value
CMR$fval

# Standard Errors (Thesis Table 1, Standard errors)

#Lib
SE_lib <- sqrt(1 / diag(STATS[[1]]$weights))

# Int
SE_int <- sqrt(1 / diag(STATS[[2]]$weights))



# Create Custom STA-Plot using ggplot (Thesis Figure 6)


# ggplot 

library(ggplot2)

# Data for plot
x_means_main <- STATS[[1]]$means
y_means_main <- STATS[[2]]$means
errors_lib_main <- SE_lib
errors_int_main <- SE_int
col_main <- c("low", "high", "high",
              "low")
pch_main <- c("high", "low", 
              "high", "low")

x_iso_main <- CMR$x[[1]]
y_iso_main <- CMR$x[[2]]



# Create a data frame for main plot
data_plot_main <- data.frame(
  x_main = x_means_main,
  y_main = y_means_main,
  Self_Efficacy = factor(col_main,
                         levels = c("low", "high")),  # Convert to factor for ggplot aesthetics
  Artistic_interests = factor(pch_main,
                          levels = c("low", "high")),   # Convert to factor for ggplot aesthetics
  x_min = x_means_main - SE_lib,  # Set Limits for error bars
  x_max = x_means_main + SE_lib,  
  y_min = y_means_main - SE_int,  
  y_max = y_means_main + SE_int,   
  x_iso = x_iso_main,
  y_iso = y_iso_main
)


# Create a data frame for isotonic regression

data_iso_main <- data.frame(
  x_iso = x_iso_main,
  y_iso = y_iso_main
)


# ggplot
ggplot(data_plot_main, aes(x = x_main, y = y_main)) +
  geom_point(data = data_iso_main, aes(x = x_iso, y = y_iso), color = "grey",
             shape = 18, size = 3) +  # Add isotonic regression points
  geom_line(data = data_iso_main, aes(x = x_iso, y = y_iso), color = "grey", linetype = "dashed") +  # Add dashed line connecting the points
  geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.1) +  # Add y-axis error bars of condition means
  geom_errorbar(aes(xmin = x_min, xmax = x_max), orientation = "y",
                width = 0.1) +  # Add x-axis error bars of condition means
  geom_point(aes(shape = Artistic_interests, color = Self_Efficacy), size = 4) +  # set condition means; size = 4 for cex = 1.5
  scale_shape_manual(values = c(19, 15)) +  # Custom shapes
  scale_color_manual(values = c("blue", "green")) +  # Custom colors
  labs(
    title = "STA-plot",
    x = "Liberalism",
    y = "Intellect",
    shape = "Artistic Interests",  # Customize legend title
    color = "Self-Efficacy"        
  ) +
  guides(
    color = guide_legend(
      override.aes = list(shape = 35, color = c("blue", "green"))  # Override the shapes in the legend
    )
  ) +
  xlim(4, 20) +
  ylim(4, 20) +
  theme_minimal() + # Use a minimal theme
  theme(axis.title.x = element_text(size = 16),  # Enlarge Labels
axis.title.y = element_text(size = 16),  
axis.text.x = element_text(size = 12),  
axis.text.y = element_text(size = 12),  
legend.title = element_text(size = 16), 
legend.text = element_text(size = 14)  
 )









################################################################################
################################################################################




# Important:


# long_data_STA: Original Data in the correct format for STA analyses

# CMR: Object containig the CMR-Model's fitted mean values, as well as the Fit value (GoF) 



################################################################################
################################################################################





####### 6. Power-Analysis


### 6.1 Shift original data mean values to CMR predictions


# Index 1 = Liberalism

# Index 2 = Intellect


# Get Differences between Origial Data and CMR predictions

Diffs_Lib <-  CMR$x[[1]] - STATS[[1]]$means


Diffs_Int <-   CMR$x[[2]] - STATS[[2]]$means


Diffs <- data.frame(Diffs_Lib, Diffs_Int)
colnames(Diffs) <- c("Lib", "Int")


# Shift empirical Data points to predicted CMR values
# Equation: CMR - STATS = Diff

long_data_SimSTA <- long_data_STA

for (i in 1:nrow(long_data_STA)) {
    if (long_data_SimSTA$Gleichung[i] == 1) {
      if (long_data_SimSTA$AV[i] == 1) {
        long_data_SimSTA$Sum[i] <- long_data_STA$Sum[i] + Diffs$Lib[1] 
      }
      else {
        long_data_SimSTA$Sum[i] <- long_data_STA$Sum[i] + Diffs$Int[1]
      }
    }
  else if (long_data_SimSTA$Gleichung[i] == 2) {
    if (long_data_SimSTA$AV[i] == 1) {
      long_data_SimSTA$Sum[i] <- long_data_STA$Sum[i] + Diffs$Lib[2] 
    }
    else {
      long_data_SimSTA$Sum[i] <- long_data_STA$Sum[i] + Diffs$Int[2]
    }
  }
  else if (long_data_SimSTA$Gleichung[i] == 3) {
    if (long_data_SimSTA$AV[i] == 1) {
      long_data_SimSTA$Sum[i] <- long_data_STA$Sum[i] + Diffs$Lib[3] 
    }
    else { 
      long_data_SimSTA$Sum[i] <- long_data_STA$Sum[i] + Diffs$Int[3]
    }
  }
  else {
    if (long_data_SimSTA$AV[i] == 1) {
      long_data_SimSTA$Sum[i] <- long_data_STA$Sum[i] + Diffs$Lib[4] 
    }
    else {
      long_data_SimSTA$Sum[i] <- long_data_STA$Sum[i] + Diffs$Int[4]
  }
  }
}
# Check:

sta_stats(long_data_SimSTA, col_value = "Sum", col_participant = "id",
         col_dv = "AV", col_between = "Gleichung")



# long_data_SimSTA = Data point with shifted mean values, so that the mean values
# correspond to the CMR model predictions




### 6.2 Build Bootstrap Simulation Function

# Now draw 12, 10, 12 and 8 values from Conditions 1 - 4 
# draw from long_data_SimSTA for Lib as well as Int; 



# Define Function

#This function determines a separate CMR model for each bootstrap data set 
# and calculates the fit value relative to the "own" CMR model of the sample


sim_CMRfit <- function(data, 
                       n_sample,     # Number of bootstrap samples
                       n_factor = 1, # Factor to multiply the number of samples per group
                       perc = .95    # Percentile of the fit values distribution to analyze
) {
  
  # Define sample sizes
  n <- c(12,10,12,8) * n_factor
  
  # Predefine lists
  List_sim <- list(
    element1 = list(list1 = 1:n[1], list2 = 1:n[2], list3 = 1:n[3], list4 = 1:n[4]), # Lib
    element2 = list(list1 = 1:n[1], list2 = 1:n[2], list3 = 1:n[3], list4 = 1:n[4])  # Int
  )
  
  # Initialize result lists
  result_list <- list()                                    
  result_list$List_fits <- vector("list", n_sample)      # List to store fit values
  result_list$List_dfs <- vector("list", n_sample)       # List to store bootstrap datasets
  result_list$percentile <- NA                           # Initialize percentile
  
  # Initialize error log
  error_log <- list()
  
  # Fill lists with simulation values
  for (i in 1:n_sample) {
    tryCatch({
      # Create simulation lists
      for (AV in 1:2) {                                       # For Int and Lib
        for (Cond in 1:length(unique(data$Gleichung))) {      # For all 4 conditions
          List_sim[[AV]][[Cond]] <- sample(
            data[data$Gleichung == Cond & data$AV == AV, 4], 
            size = n[Cond], replace = TRUE
          )                  # Sample data with replacement
        }
      }
      
      # Define bootstrap dataset
      Id_sim <- rep(1:sum(n), times = 2)              # Column indicating participant number
      Sum_sim <- unlist(List_sim)                     # Column with sum score
      Gleichung_sim <- rep(rep(1:4, times = c(n[1], n[2], n[3], n[4])), times = 2)  # Condition column (1-4)
      AV_sim <- rep(1:2, times = c(sum(n), sum(n)))   # AV index column (1 = Lib, 2 = Int)
      
      Df_sim <- data.frame(Id_sim, Gleichung_sim, AV_sim, Sum_sim)
      
      # Bootstrap dataset in STA analysis format
      STATS_sim <- sta_stats(Df_sim, col_value = "Sum_sim", col_participant = "Id_sim",
                             col_dv = "AV_sim", col_between = "Gleichung_sim")
      CMR_sim <- staCMR(Df_sim)
      
      # Debugging
      print(i)  # Check the value of i
      print(Df_sim)  # Check the value of Df_sim
      print(STATS_sim)  # Check the value of STATS_sim
      print(CMR_sim)  # Check the value of CMR_sim
      
      # Store results
      result_list$List_dfs[[i]] <- Df_sim         # Store bootstrap dataset
      result_list$List_fits[[i]] <- CMR_sim$fval  # Store fit value
      
    }, error = function(e) {
      # Log error
      error_log[[length(error_log) + 1]] <- list(iteration = i, message = e$message)
      result_list$List_dfs[[i]] <- NA          # Store NA for this iteration
      result_list$List_fits[[i]] <- NA         # Store NA for this iteration
    })
  }
  
  # Calculate and store percentile
  valid_fits <- unlist(result_list$List_fits)
  valid_fits <- valid_fits[!is.na(valid_fits)]  # Remove NAs
  cdf <- ecdf(valid_fits)                       # Calculate cumulative distribution
  quant <- quantile(valid_fits, perc)           # Calculate quantile
  result_list$percentile <- quant
  
  cat("Percentage of data below the percentile:", 100 * cdf(quant), "%\n")
  
  # Return results and error log
  return(list(results = result_list, errors = error_log))
}



################################################################################
################################################################################

# Thesis Section 4.6.3; Appendix C


# Significance test und Power-Analysis for n_factor = 1

set.seed(1)


# Apply Function
List_fits_result_null_1 <- sim_CMRfit(data = long_data_SimSTA, n_sample = 10000,
                                      n_factor = 1, perc = .95)

# 95th percentile of the null hypothesis bootstrap GoF value distribution

p95_null_1 <- List_fits_result_null_1$results$percentile



# Visualisation of the Null-Hypothesis Distribution

hist(unlist(List_fits_result_null_1$results$List_fits), xlab = "CMR model fit", main = "Bootstrapped sample distribution of fit values",
     breaks = 40)
abline(v = CMR$fval, lwd = 2, col = "blue") # empirical GoF-value
abline(v = p95_null_1, lwd = 2, col = "green")    # critical GoF-value

# note: Result is significant, if blue empirical GoF exceeds or equals green critical GoF


# Determine p-value for Significance test

p_Wert_1 <- mean(unlist(List_fits_result_null_1$results$List_fits) >= CMR$fval)
p_Wert_1


################################################################################
################################################################################


# Alternative_hypothesis distribution

set.seed(2)

List_fits_result_alt_1 <- sim_CMRfit(data = long_data_STA, n_sample = 10000,
                                     n_factor = 1, perc = .20)

# 20th percentile of the alt hypothesis bootstrap GoF value distribution
p20_alt_1 <- List_fits_result_alt_1$results$percentile


# Visualisation of Alternative_hypothesis distribution


hist(unlist(List_fits_result_alt_1$results$List_fits), xlab = "CMR model fit",
     main = "Bootstrapped sample distribution of fit values",
     breaks = 40)
abline(v = CMR$fval, lwd = 2, col = "blue") # empirical Fit-value
abline(v = p95_null_1, lwd = 2, col = "green")    # critical Fit-value
abline(v = p20_alt_1, lwd = 2, col = "purple")


# Beta-error likelihood

Beta_1 <-  sum(unlist(List_fits_result_alt_1$results$List_fits) < 
      p95_null_1)/length(unlist(List_fits_result_alt_1$results$List_fits))

# Power
1 - Beta_1


################################################################################
################################################################################





# Significance test und Power-Analysis for n_factor = 1.5

set.seed(3)


# Apply Function
List_fits_result_null_15 <- sim_CMRfit(data = long_data_SimSTA, n_sample = 10000,
                                      n_factor = 1.5, perc = .95)

p95_null_15 <- List_fits_result_null_15$results$percentile



# Visualisation of the Null-Hypothesis Distribution

hist(unlist(List_fits_result_null_15$results$List_fits), xlab = "CMR model fit", main = "Bootstrapped sample distribution of fit values",
     breaks = 40)
abline(v = CMR$fval, lwd = 2, col = "blue") # empirischer Fit-Wert
abline(v = p95_null_15, lwd = 2, col = "green")    # kritischer Fit-Wert


# Determine p-value

p_Wert_1 <- mean(unlist(List_fits_result_null_15$results$List_fits) >= CMR$fval)
p_Wert_1


################################################################################


# Alternative_hypothesis distribution

set.seed(4)

List_fits_result_alt_15 <- sim_CMRfit(data = long_data_STA, n_sample = 10000,
                                     n_factor = 1.5, perc = .20)

p20_alt_15 <- List_fits_result_alt_15$results$percentile


# Visualisation of Alternative_hypothesis distribution

hist(unlist(List_fits_result_alt_15$results$List_fits), xlab = "CMR model fit",
     main = "Bootstrapped sample distribution of fit values",
     breaks = 40)
abline(v = CMR$fval, lwd = 2, col = "blue") # empirischer Fit-Wert
abline(v = p95_null_15, lwd = 2, col = "green")    # kritischer Fit-Wert
abline(v = p20_alt_15, lwd = 2, col = "purple")


# Beta-error likelihood

Beta_15 <-  sum(unlist(List_fits_result_alt_15$results$List_fits) < 
                 p95_null_15)/length(unlist(List_fits_result_alt_15$results$List_fits))

# Power
1 - Beta_15



################################################################################



# Significance test und Power-Analysis for n_factor = 2

set.seed(5)


# Apply Function
List_fits_result_null_2 <- sim_CMRfit(data = long_data_SimSTA, n_sample = 10000,
                                       n_factor = 2, perc = .95)

p95_null_2 <- List_fits_result_null_2$results$percentile



# Visualisation of the Null-Hypothesis Distribution

hist(unlist(List_fits_result_null_2$results$List_fits), xlab = "CMR model fit", main = "Bootstrapped sample distribution of fit values",
     breaks = 40)
abline(v = CMR$fval, lwd = 2, col = "blue") # empirischer Fit-Wert
abline(v = p95_null_2, lwd = 2, col = "green")    # kritischer Fit-Wert


# Determine p-value

p_Wert_2 <- mean(unlist(List_fits_result_null_2$results$List_fits) >= CMR$fval)
p_Wert_2


################################################################################


# Alternative_hypothesis distribution

set.seed(6)

List_fits_result_alt_2 <- sim_CMRfit(data = long_data_STA, n_sample = 10000,
                                      n_factor = 2, perc = .20)

p20_alt_2 <- List_fits_result_alt_2$results$percentile


# Visualisation of Alternative_hypothesis distribution

hist(unlist(List_fits_result_alt_2$results$List_fits), xlab = "CMR model fit",
     main = "Bootstrapped sample distribution of fit values",
     breaks = 40)
abline(v = CMR$fval, lwd = 2, col = "blue") # empirischer Fit-Wert
abline(v = p95_null_2, lwd = 2, col = "green")    # kritischer Fit-Wert
abline(v = p20_alt_2, lwd = 2, col = "purple")


# Beta-error likelihood

Beta_2 <-  sum(unlist(List_fits_result_alt_2$results$List_fits) < 
                  p95_null_2)/length(unlist(List_fits_result_alt_2$results$List_fits))

# Power
1 - Beta_2






################################################################################
################################################################################




# Significance test und Power-Analysis for n_factor = 2.5

set.seed(7)


# Apply Function
List_fits_result_null_25 <- sim_CMRfit(data = long_data_SimSTA, n_sample = 10000,
                                      n_factor = 2.5, perc = .95)

p95_null_25 <- List_fits_result_null_25$results$percentile



# Visualisation of the Null-Hypothesis Distribution

hist(unlist(List_fits_result_null_25$results$List_fits), 
     xlab = "CMR model fit", main = "Bootstrapped sample distribution of fit values",
     breaks = 40)
abline(v = CMR$fval, lwd = 2, col = "blue") # empirischer Fit-Wert
abline(v = p95_null_25, lwd = 2, col = "green")    # kritischer Fit-Wert


# Determine p-value

p_Wert_25 <- mean(unlist(List_fits_result_null_25$results$List_fits) >= CMR$fval)
p_Wert_25


################################################################################


# Alternative_hypothesis distribution

set.seed(8)

List_fits_result_alt_25 <- sim_CMRfit(data = long_data_STA, n_sample = 10000,
                                     n_factor = 2.5, perc = .20)

p20_alt_25 <- List_fits_result_alt_25$results$percentile


# Visualisation of Alternative_hypothesis distribution

hist(unlist(List_fits_result_alt_25$results$List_fits), xlab = "CMR model fit",
     main = "Bootstrapped sample distribution of fit values",
     breaks = 40)
abline(v = CMR$fval, lwd = 2, col = "blue") # empirischer Fit-Wert
abline(v = p95_null_25, lwd = 2, col = "green")    # kritischer Fit-Wert
abline(v = p20_alt_25, lwd = 2, col = "purple")


# Beta-error likelihood

Beta_25 <-  sum(unlist(List_fits_result_alt_25$results$List_fits) < 
                 p95_null_25)/length(unlist(List_fits_result_alt_25$results$List_fits))

# Power
1 - Beta_25





