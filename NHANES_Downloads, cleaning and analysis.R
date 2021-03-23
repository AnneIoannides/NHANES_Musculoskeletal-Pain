#Anne Elizabeth Ioannides
#For the qualification of: PhD - Physiology, University of the Witwatersrand, South Africa

#National Health and Nutrition Examination Survey (NHANES) (Continuous)

#Please note that the the script contains large data files, and this code may need to be run in pieces, depending on your computer's ram and processing power


#load packages
library(haven)
library(tidyverse)
library(survey)
library(gdata)
library(ggplot2)


# == 2017-18 == #

#DOWNLOADS

##Demographics (and survey information)
NHANES_Demo_1718 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT"))

#Save as an RDS file
saveRDS(NHANES_Demo_1718,
        file = "NHANES_Demoraw_1718.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##BP & Cholesterol
NHANES_BPQ_1718 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BPQ_J.XPT"))
#Save as RDS File
saveRDS(NHANES_BPQ_1718,
        file = "NHANES_BPQraw_1718.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Medical Conditions Data
NHANES_MCQ_1718 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/MCQ_J.XPT"))
#Save as RDS file
saveRDS(NHANES_MCQ_1718,
        file = "NHANES_MCQraw_1718.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Occupation
NHANES_OCQ_1718 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/OCQ_J.XPT"))
#Save as RDS file
saveRDS(NHANES_OCQ_1718,
        file = "NHANES_OCQraw_1718.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Body examination (physical exam)
NHANES_BMX_1718 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT"))
saveRDS(NHANES_BMX_1718,
        file = "NHANES_BMXraw_1718.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#Select relevant variables in each dataset, and then combine
NHANES_Demo_1718 <- select(NHANES_Demo_1718, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHIN2")
NHANES_BPQ_1718 <- select(NHANES_BPQ_1718, "SEQN", "BPQ020")
NHANES_MCQ_1718 <- select(NHANES_MCQ_1718, "SEQN", "MCQ160A", "MCQ160O")
NHANES_OCQ_1718 <- select(NHANES_OCQ_1718, "SEQN", "OCD150")
NHANES_BMX_1718 <- select(NHANES_BMX_1718, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_1718a <- merge(NHANES_Demo_1718, NHANES_BPQ_1718)
NHANES_1718b <- merge(NHANES_1718a, NHANES_MCQ_1718)
NHANES_1718c <- merge(NHANES_1718b, NHANES_OCQ_1718)
NHANES1718 <- merge(NHANES_1718c, NHANES_BMX_1718)

#Select only age demographic asked about arthritis
NHANES1718 <- NHANES1718[NHANES1718$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES1718)
tail(NHANES1718)
glimpse(NHANES1718)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES1718))
which(colSums(is.na(NHANES1718)) == nrow(NHANES1718))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES1718$MCQ160A <- recode(NHANES1718$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES1718$MCQ160A <- unknownToNA(NHANES1718$MCQ160A, unknown = c("7", "9")))
table(NHANES1718$MCQ160A)

NHANES1718$MCQ160A <- as.factor(NHANES1718$MCQ160A)
class(NHANES1718$MCQ160A)

#Recode gender variable
NHANES1718$RIAGENDR <- recode(NHANES1718$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES1718$RIAGENDR)
NHANES1718$RIAGENDR <- as.factor(NHANES1718$RIAGENDR)

#Recode age into categories
NHANES1718$RIDAGEYR <- recode(NHANES1718$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above")
table(NHANES1718$RIDAGEYR)
NHANES1718$RIDAGEYR <- as.factor(NHANES1718$RIDAGEYR)

#Recode education variable
NHANES1718$DMDEDUC2 <- recode(NHANES1718$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES1718$DMDEDUC2 <- unknownToNA(NHANES1718$DMDEDUC2, unknown = c("7", "9")))
table(NHANES1718$DMDEDUC2)
NHANES1718$DMDEDUC2 <- as.factor(NHANES1718$DMDEDUC2)

#Recode annual household income
NHANES1718$INDHHIN2 <- recode(NHANES1718$INDHHIN2,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "14" = ">= $75 000",
                              "15" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES1718$INDHHIN2 <- unknownToNA(NHANES1718$INDHHIN2, unknown = c("77", "99")))
table(NHANES1718$INDHHIN2)
NHANES1718$INDHHIN2 <- as.factor(NHANES1718$INDHHIN2)


#Recode bpq020
NHANES1718$BPQ020 <- recode(NHANES1718$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES1718$BPQ020 <- unknownToNA(NHANES1718$BPQ020, unknown = c("7", "9")))
table(NHANES1718$BPQ020)


#Recode copd variable
NHANES1718$MCQ160O <- recode(NHANES1718$MCQ160O,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
(NHANES1718$MCQ160O <- unknownToNA(NHANES1718$MCQ160O, unknown = c("7", "9")))
table(NHANES1718$MCQ160O)

#Recode employment situaion
NHANES1718$OCD150 <- recode(NHANES1718$OCD150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES1718$OCD150 <- unknownToNA(NHANES1718$OCD150, unknown = c("7", "9"))
table(NHANES1718$OCD150)


#Check class on BMI numeric
class(NHANES1718$BMXBMI)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_1718_dataset <- subset(NHANES1718,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_1718_dataset$WTINT2YR)
#The sum of the weights is 225 441 502

#Check the number of unique PSUs
length(unique(NHANES_1718_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_1718_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Used to generate frequency tables (unweighted) Appendices
table(subset(NHANES_1718_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_1718_dataset, RIDAGEYR == "70 and above")$RIAGENDR)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_1718_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_1718_dataset)
#Observe the design oject
NHANES_1718_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_1718_DO,
        file = "NHANES1718_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#ANALYSIS


#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 2017-18 prevalence 
NA_1718 <- svymean(~factor(MCQ160A), 
                   NHANES_1718_DO, 
                   na.rm = TRUE)

NA1718.c <- NA_1718 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1718_ci <- confint(NA_1718) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA1718 <- bind_cols(NA1718.c, NA1718_ci)
#remove js = 0
NA1718 <- NA1718[-c(1), ] #final proportion, se & 95% CI
#save for backup
#write.csv(NA1718, "NA1718.csv")



#2. Spatial trends
    #no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

      #Demographic groups consisted of age, sex, employment status, education level, and annual household income
          # - prevalence of arthritis per demographic group
          # - logistic regression per demographic group

#Prevalence by age
NHA1718_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_1718_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1718_Arthritis_age <- NHA1718_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1718_arth_age_ci <- confint(NHA1718_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA1718_arth_age_ci <- NHA1718_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_1718_arthritis.age <- bind_cols(NHA1718_Arthritis_age, NHA1718_arth_age_ci) #NHA_1718_arthritis.age = final proportion, se & 95% ci
#save for backup
#write.csv(NHA_1718_arthritis.age, "NHA1718_Arthritis.age.csv")

#logistic regression by age
NHA1718_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1718_DO)
summary(NHA1718_age_glm)
exp(cbind(OR=coef(NHA1718_age_glm), confint(NHA1718_age_glm)))


#Prevalence by sex
NHA1718_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_1718_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1718_Arthritis_sex <- NHA1718_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1718_arth_sex_ci <- confint(NHA1718_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA1718_arth_sex_ci <- NHA1718_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_1718_arthritis.sex <- bind_cols(NHA1718_Arthritis_sex, NHA1718_arth_sex_ci) #NHA_1718_arthritis.sex = final proportion, se & 95% ci
#save for backup
#write.csv(NHA_1718_arthritis.sex, "NHA1718_Arthritis.sex.csv")


#logistic regression by sex
NHA1718_sex_glm <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                          family = quasibinomial,
                          design = NHANES_1718_DO)
summary(NHA1718_sex_glm)
exp(cbind(OR=coef(NHA1718_sex_glm), confint(NHA1718_sex_glm)))


#Prevalence by employment group
NHA1718_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCD150,
                          design = NHANES_1718_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1718_Arthritis_emp <- NHA1718_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1718_Arth_emp_ci <- confint(NHA1718_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA1718_Arth_emp_ci <- NHA1718_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA1718_Arthritis.employ <- bind_cols(NHA1718_Arthritis_emp, NHA1718_Arth_emp_ci) #NHA_1718_Arthritis.employ = final proportion, se & 95% ci
#save for backup
#write.csv(NHA1718_Arthritis.employ, "NHA1718_Arthritis.employment.csv")


#logistic regression by employment group
NHA1718_emp_glm <- svyglm(MCQ160A~relevel(factor(OCD150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1718_DO)
summary(NHA1718_emp_glm)
exp(cbind(OR=coef(NHA1718_emp_glm), confint(NHA1718_emp_glm)))

#logistic regression by education level
NHA1718_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_1718_DO)
summary(NHA1718_educ_glm)
exp(cbind(OR=coef(NHA1718_educ_glm), confint(NHA1718_educ_glm)))

#logistic regression by annual household income
NHA1718_inc_glm <- svyglm(MCQ160A~relevel(INDHHIN2, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1718_DO)
summary(NHA1718_inc_glm)
exp(cbind(OR=coef(NHA1718_inc_glm), confint(NHA1718_inc_glm)))



#4. Lifestyle trends of arthritis diagnoses

  #Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA1718.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_1718_DO)
summary(NA1718.BMI.glm)
exp(cbind(OR=coef(NA1718.BMI.glm), confint(NA1718.BMI.glm)))

#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________


# == 2015-16 == #

#DOWNLOADS

##Demographics Data (and survey information)
NHANES_Demo_1516 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT"))
#Save as an RDS file
saveRDS(NHANES_Demo_1516,
        file = "NHANES_Demoraw_1516.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##BP & Cholesterol
NHANES_BPQ_1516 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BPQ_I.XPT"))
#Save as RDS File
saveRDS(NHANES_BPQ_1516,
        file = "NHANES_BPQraw_1516.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Medical Conditions Data
NHANES_MCQ_1516 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.XPT"))
#Save as RDS file
saveRDS(NHANES_MCQ_1516,
        file = "NHANES_MCQraw_1516.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Occupation
NHANES_OCQ_1516 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/OCQ_I.XPT"))
#Save as RDS file
saveRDS(NHANES_OCQ_1516,
        file = "NHANES_OCQraw_1516.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Body examination (physical exam)
NHANES_BMX_1516 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT"))
#Save as RDS file
saveRDS(NHANES_BMX_1516,
        file = "NHANES_BMXraw_1516.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#Select relevant variables
NHANES_Demo_1516 <- select(NHANES_Demo_1516, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHIN2")
NHANES_BPQ_1516 <- select(NHANES_BPQ_1516, "SEQN", "BPQ020")
NHANES_MCQ_1516 <- select(NHANES_MCQ_1516, "SEQN", "MCQ160A", "MCQ160O")
NHANES_OCQ_1516 <- select(NHANES_OCQ_1516, "SEQN", "OCD150")
NHANES_BMX_1516 <- select(NHANES_BMX_1516, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_1516a <- merge(NHANES_Demo_1516, NHANES_BPQ_1516)
NHANES_1516b <- merge(NHANES_1516a, NHANES_MCQ_1516)
NHANES_1516c <- merge(NHANES_1516b, NHANES_OCQ_1516)
NHANES1516 <- merge(NHANES_1516c, NHANES_BMX_1516)

#Select only age demographic asked about arthritis
NHANES1516 <- NHANES1516[NHANES1516$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES1516)
tail(NHANES1516)
glimpse(NHANES1516)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES1516))
which(colSums(is.na(NHANES1516)) == nrow(NHANES1516))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES1516$MCQ160A <- recode(NHANES1516$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES1516$MCQ160A <- unknownToNA(NHANES1516$MCQ160A, unknown = c("7", "9")))
table(NHANES1516$MCQ160A)

NHANES1516$MCQ160A <- as.factor(NHANES1516$MCQ160A)
class(NHANES1516$MCQ160A)

#Recode gender variable
NHANES1516$RIAGENDR <- recode(NHANES1516$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES1516$RIAGENDR)
NHANES1516$RIAGENDR <- as.factor(NHANES1516$RIAGENDR)

#Recode age into categories
NHANES1516$RIDAGEYR <- recode(NHANES1516$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above")
table(NHANES1516$RIDAGEYR)
NHANES1516$RIDAGEYR <- as.factor(NHANES1516$RIDAGEYR)

#Recode education variable
NHANES1516$DMDEDUC2 <- recode(NHANES1516$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES1516$DMDEDUC2 <- unknownToNA(NHANES1516$DMDEDUC2, unknown = c("7", "9")))
table(NHANES1516$DMDEDUC2)
NHANES1516$DMDEDUC2 <- as.factor(NHANES1516$DMDEDUC2)

#Recode annual household income
NHANES1516$INDHHIN2 <- recode(NHANES1516$INDHHIN2,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "14" = ">= $75 000",
                              "15" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES1516$INDHHIN2 <- unknownToNA(NHANES1516$INDHHIN2, unknown = c("77", "99")))
table(NHANES1516$INDHHIN2)
NHANES1516$INDHHIN2 <- as.factor(NHANES1516$INDHHIN2)


#Recode bpq020
NHANES1516$BPQ020 <- recode(NHANES1516$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES1516$BPQ020 <- unknownToNA(NHANES1516$BPQ020, unknown = c("7", "9")))
table(NHANES1516$BPQ020)


#Recode copd variable
NHANES1516$MCQ160O <- recode(NHANES1516$MCQ160O,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
(NHANES1516$MCQ160O <- unknownToNA(NHANES1516$MCQ160O, unknown = c("7", "9")))
table(NHANES1516$MCQ160O)

#Recode employment situaion
NHANES1516$OCD150 <- recode(NHANES1516$OCD150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES1516$OCD150 <- unknownToNA(NHANES1516$OCD150, unknown = c("7", "9"))
table(NHANES1516$OCD150)


#Check class on BMI numeric
class(NHANES1516$BMXBMI)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_1516_dataset <- subset(NHANES1516,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_1516_dataset$WTINT2YR)
#The sum of the weights is 225 010 002

#Check the number of unique PSUs
length(unique(NHANES_1516_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_1516_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generating unweighted frequency tables
table(subset(NHANES_1516_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_1516_dataset, RIDAGEYR == "70 and above")$RIAGENDR)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_1516_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_1516_dataset)
#Observe the design oject
NHANES_1516_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_1516_DO,
        file = "NHANES1516_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#ANALYSIS


#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 2015-16 prevalence 
NA_1516 <- svymean(~factor(MCQ160A), 
                   NHANES_1516_DO, 
                   na.rm = TRUE)
NA1516.c <- NA_1516 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1516_ci <- confint(NA_1516) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA1516 <- bind_cols(NA1516.c, NA1516_ci)
#remove js = 0
NA1516 <- NA1516[-c(1), ] #final proportion, se & 95% CI
#save for backup
#write.csv(NA1516, "NA1516.csv")




#2. Spatial trends
  #no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

  #Demographic groups consisted of age, sex, employment status, education level, and annual household income
      # - prevalence of arthritis per demographic group
      # - logistic regression per demographic group

#Prevalence by age
NHA1516_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_1516_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1516_Arthritis_age <- NHA1516_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1516_arth_age_ci <- confint(NHA1516_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA1516_arth_age_ci <- NHA1516_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_1516_arthritis.age <- bind_cols(NHA1516_Arthritis_age, NHA1516_arth_age_ci) #final proportion, se & 95% CI
#save for backup
#write.csv(NHA_1516_arthritis.age, "NHA1516_Arthritis.age.csv")

#logistic regression by age
NHA1516_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1516_DO)
summary(NHA1516_age_glm)
exp(cbind(OR=coef(NHA1516_age_glm), confint(NHA1516_age_glm)))


#Prevalence by sex
NHA1516_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_1516_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1516_Arthritis_sex <- NHA1516_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1516_arth_sex_ci <- confint(NHA1516_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA1516_arth_sex_ci <- NHA1516_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_1516_arthritis.sex <- bind_cols(NHA1516_Arthritis_sex, NHA1516_arth_sex_ci) #final proportion, se & 95% ci
#save
write.csv(NHA_1516_arthritis.sex, "NHA1516_Arthritis.sex.csv")


#logistic regression by sex
NHA1516_sex_glm <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                          family = quasibinomial,
                          design = NHANES_1516_DO)
summary(NHA1516_sex_glm)
exp(cbind(OR=coef(NHA1516_sex_glm), confint(NHA1516_sex_glm)))



#Prevalence by employment group
NHA1516_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCD150,
                          design = NHANES_1516_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1516_Arthritis_emp <- NHA1516_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1516_Arth_emp_ci <- confint(NHA1516_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA1516_Arth_emp_ci <- NHA1516_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA1516_Arthritis.employ <- bind_cols(NHA1516_Arthritis_emp, NHA1516_Arth_emp_ci) #final proportion, se & 95% ci
#save for backup
write.csv(NHA1516_Arthritis.employ, "NHA1516_Arthritis.employment.csv")


#logistic regression by employment group
NHA1516_emp_glm <- svyglm(MCQ160A~relevel(factor(OCD150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1516_DO)
summary(NHA1516_emp_glm)
exp(cbind(OR=coef(NHA1516_emp_glm), confint(NHA1516_emp_glm)))

#logistic regression by education level
NHA1516_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_1516_DO)
summary(NHA1516_educ_glm)
exp(cbind(OR=coef(NHA1516_educ_glm), confint(NHA1516_educ_glm)))

#logistic regression by annual household income
NHA1516_inc_glm <- svyglm(MCQ160A~relevel(INDHHIN2, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1516_DO)
summary(NHA1516_inc_glm)
exp(cbind(OR=coef(NHA1516_inc_glm), confint(NHA1516_inc_glm)))



#4. Lifestyle trends of arthritis diagnoses

    #Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA1516.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_1516_DO)
summary(NA1516.BMI.glm)
exp(cbind(OR=coef(NA1516.BMI.glm), confint(NA1516.BMI.glm)))

#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________


# == 2013-14 == #

#DOWNLOADS

##Demographic Data (and survey information)
NHANES_Demo_1314 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT"))
#Save as RDS
saveRDS(NHANES_Demo_1314,
        file = "NHANES_Demoraw_1314.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##BP (confounder)
NHANES_BPQ_1314 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BPQ_H.XPT"))
#Save as RDS file
saveRDS(NHANES_BPQ_1314,
        file = "NHANES_BPQraw_1314.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Medical Conditions
NHANES_MCQ_1314 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MCQ_H.XPT"))
#Save as rds file
saveRDS(NHANES_MCQ_1314,
        file = "NHANES_MCQraw_1314.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

##Occupation
NHANES_OCQ_1314 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/OCQ_H.XPT"))
#Save as RDS file
saveRDS(NHANES_OCQ_1314,
        file = "NHANES_OCQraw_1314.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Body examination (physical exam)
NHANES_BMX_1314 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.XPT"))
#Save as RDS file
saveRDS(NHANES_BMX_1314,
        file = "NHANES_BMXraw_1314.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#Selecting relevant variables
NHANES_Demo_1314 <- select(NHANES_Demo_1314, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHIN2")
NHANES_BPQ_1314 <- select(NHANES_BPQ_1314, "SEQN", "BPQ020")
NHANES_MCQ_1314 <- select(NHANES_MCQ_1314, "SEQN", "MCQ160A", "MCQ160O")
NHANES_OCQ_1314 <- select(NHANES_OCQ_1314, "SEQN", "OCD150")
NHANES_BMX_1314 <- select(NHANES_BMX_1314, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_1314a <- merge(NHANES_Demo_1314, NHANES_BPQ_1314)
NHANES_1314b <- merge(NHANES_1314a, NHANES_MCQ_1314)
NHANES_1314c <- merge(NHANES_1314b, NHANES_OCQ_1314)
NHANES1314 <- merge(NHANES_1314c, NHANES_BMX_1314)

#Select only age demographic asked about arthritis
NHANES1314 <- NHANES1314[NHANES1314$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES1314)
tail(NHANES1314)
glimpse(NHANES1314)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES1314))
which(colSums(is.na(NHANES1314)) == nrow(NHANES1314))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES1314$MCQ160A <- recode(NHANES1314$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES1314$MCQ160A <- unknownToNA(NHANES1314$MCQ160A, unknown = c("7", "9")))
table(NHANES1314$MCQ160A)

NHANES1314$MCQ160A <- as.factor(NHANES1314$MCQ160A)
class(NHANES1314$MCQ160A)

#Recode gender variable
NHANES1314$RIAGENDR <- recode(NHANES1314$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES1314$RIAGENDR)
NHANES1314$RIAGENDR <- as.factor(NHANES1314$RIAGENDR)

#Recode age into categories
NHANES1314$RIDAGEYR <- recode(NHANES1314$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above")
table(NHANES1314$RIDAGEYR)
NHANES1314$RIDAGEYR <- as.factor(NHANES1314$RIDAGEYR)

#Recode education variable
NHANES1314$DMDEDUC2 <- recode(NHANES1314$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES1314$DMDEDUC2 <- unknownToNA(NHANES1314$DMDEDUC2, unknown = c("7", "9")))
table(NHANES1314$DMDEDUC2)
NHANES1314$DMDEDUC2 <- as.factor(NHANES1314$DMDEDUC2)

#Recode annual household income
NHANES1314$INDHHIN2 <- recode(NHANES1314$INDHHIN2,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "14" = ">= $75 000",
                              "15" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES1314$INDHHIN2 <- unknownToNA(NHANES1314$INDHHIN2, unknown = c("77", "99")))
table(NHANES1314$INDHHIN2)
NHANES1314$INDHHIN2 <- as.factor(NHANES1314$INDHHIN2)


#Recode bpq020
NHANES1314$BPQ020 <- recode(NHANES1314$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES1314$BPQ020 <- unknownToNA(NHANES1314$BPQ020, unknown = c("7", "9")))
table(NHANES1314$BPQ020)


#Recode copd variable
NHANES1314$MCQ160O <- recode(NHANES1314$MCQ160O,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
(NHANES1314$MCQ160O <- unknownToNA(NHANES1314$MCQ160O, unknown = c("7", "9")))
table(NHANES1314$MCQ160O)

#Recode employment situaion
NHANES1314$OCD150 <- recode(NHANES1314$OCD150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES1314$OCD150 <- unknownToNA(NHANES1314$OCD150, unknown = c("7", "9"))
table(NHANES1314$OCD150)


#Check class on BMI numeric
class(NHANES1314$BMXBMI)
#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_1314_dataset <- subset(NHANES1314,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_1314_dataset$WTINT2YR)
#The sum of the weights is 222 978 874, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_1314_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_1314_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Used to generate unweighted frequency tables (appendix)
table(subset(NHANES_1314_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_1314_dataset, RIDAGEYR == "70 and above")$RIAGENDR)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_1314_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_1314_dataset)
#Observe the design oject
NHANES_1314_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_1314_DO,
        file = "NHANES1314_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#ANALYSIS


#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 2013-14 prevalence 
NA_1314 <- svymean(~factor(MCQ160A), 
                   NHANES_1314_DO, 
                   na.rm = TRUE)
NA1314.c <- NA_1314 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1314_ci <- confint(NA_1314) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA1314 <- bind_cols(NA1314.c, NA1314_ci)
#remove js = 0
NA1314 <- NA1314[-c(1), ] #final proportion, se & 95% ci
#save for backup
#write.csv(NA1314, "NA1314.csv")




#2. Spatial trends
#no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

  #Demographic groups consisted of age, sex, employment status, education level, and annual household income
      # - prevalence of arthritis per demographic group
      # - logistic regression per demographic group

#Prevalence by age
NHA1314_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_1314_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1314_Arthritis_age <- NHA1314_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1314_arth_age_ci <- confint(NHA1314_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA1314_arth_age_ci <- NHA1314_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_1314_arthritis.age <- bind_cols(NHA1314_Arthritis_age, NHA1314_arth_age_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA_1314_arthritis.age, "NHA1314_Arthritis.age.csv")


#logistic regression by age
NHA1314_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1314_DO)
summary(NHA1314_age_glm)
exp(cbind(OR=coef(NHA1314_age_glm), confint(NHA1314_age_glm)))


#Prevalence by sex
NHA1314_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_1314_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1314_Arthritis_sex <- NHA1314_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1314_arth_sex_ci <- confint(NHA1314_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA1314_arth_sex_ci <- NHA1314_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_1314_arthritis.sex <- bind_cols(NHA1314_Arthritis_sex, NHA1314_arth_sex_ci) #final proportion, se & 95% ci
#save for backup
write.csv(NHA_1314_arthritis.sex, "NHA1314_Arthritis.sex.csv")



#logistic regression by sex
NHA1314_sex_glm_c <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                            family = quasibinomial,
                            design = NHANES_1314_DO)
summary(NHA1314_sex_glm_c)
exp(cbind(OR=coef(NHA1314_sex_glm_c), confint(NHA1314_sex_glm_c)))


#Prevalence by employment group
NHA1314_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCD150,
                          design = NHANES_1314_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1314_Arthritis_emp <- NHA1314_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1314_Arth_emp_ci <- confint(NHA1314_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA1314_Arth_emp_ci <- NHA1314_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA1314_Arthritis.employ <- bind_cols(NHA1314_Arthritis_emp, NHA1314_Arth_emp_ci) #final proportion, se & 95% ci
#save for backup
write.csv(NHA1314_Arthritis.employ, "NHA1314_Arthritis.employment.csv")

#logistic regression by employment group
NHA1314_emp_glm <- svyglm(MCQ160A~relevel(factor(OCD150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1314_DO)
summary(NHA1314_emp_glm)
exp(cbind(OR=coef(NHA1314_emp_glm), confint(NHA1314_emp_glm)))

#logistic regression by education level
NHA1314_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_1314_DO)
summary(NHA1314_educ_glm)
exp(cbind(OR=coef(NHA1314_educ_glm), confint(NHA1314_educ_glm)))

#logistic regression by annual household income
NHA1314_inc_glm <- svyglm(MCQ160A~relevel(INDHHIN2, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1314_DO)
summary(NHA1314_inc_glm)
exp(cbind(OR=coef(NHA1314_inc_glm), confint(NHA1314_inc_glm)))



#4. Lifestyle trends of arthritis diagnoses

  #Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA1314.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_1314_DO)
summary(NA1314.BMI.glm)
exp(cbind(OR=coef(NA1314.BMI.glm), confint(NA1314.BMI.glm)))

#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________

# == 2011-12 == #

#DOWNLOADS

##Demographic Data (and survey information)
NHANES_Demo_1112 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT"))
#Save as RDS file
saveRDS(NHANES_Demo_1112,
        file = "NHANES_Demoraw_1112.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#BP (for condfounder consideration)
NHANES_BPQ_1112 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BPQ_G.XPT"))
#Save as rds file
saveRDS(NHANES_BPQ_1112,
        file = "NHANES_BPQraw_1112.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Medical Conditions
NHANES_MCQ_1112 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/MCQ_G.XPT"))
#Save as RDS file
saveRDS(NHANES_MCQ_1112,
        file = "NHANES_MCQraw_1112.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Occupation
NHANES_OCQ_1112 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/OCQ_G.XPT"))
#Save as RDS file
saveRDS(NHANES_OCQ_1112,
        file = "NHANES_OCQraw_1112.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

##Body examination (physical exam)
NHANES_BMX_1112 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.XPT"))
#Save as RDS file
saveRDS(NHANES_BMX_1112,
        file = "NHANES_BMXraw_1112.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#Select relevant variables
NHANES_Demo_1112 <- select(NHANES_Demo_1112, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHIN2")
NHANES_BPQ_1112 <- select(NHANES_BPQ_1112, "SEQN", "BPQ020")
NHANES_MCQ_1112 <- select(NHANES_MCQ_1112, "SEQN", "MCQ160A")
NHANES_OCQ_1112 <- select(NHANES_OCQ_1112, "SEQN", "OCD150")
NHANES_BMX_1112 <- select(NHANES_BMX_1112, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_1112a <- merge(NHANES_Demo_1112, NHANES_BPQ_1112)
NHANES_1112b <- merge(NHANES_1112a, NHANES_MCQ_1112)
NHANES_1112c <- merge(NHANES_1112b, NHANES_OCQ_1112)
NHANES1112 <- merge(NHANES_1112c, NHANES_BMX_1112)

#Select only age demographic asked about arthritis
NHANES1112 <- NHANES1112[NHANES1112$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES1112)
tail(NHANES1112)
glimpse(NHANES1112)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES1112))
which(colSums(is.na(NHANES1112)) == nrow(NHANES1112))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES1112$MCQ160A <- recode(NHANES1112$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES1112$MCQ160A <- unknownToNA(NHANES1112$MCQ160A, unknown = c("7", "9")))
table(NHANES1112$MCQ160A)

NHANES1112$MCQ160A <- as.factor(NHANES1112$MCQ160A)
class(NHANES1112$MCQ160A)

#Recode gender variable
NHANES1112$RIAGENDR <- recode(NHANES1112$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES1112$RIAGENDR)
NHANES1112$RIAGENDR <- as.factor(NHANES1112$RIAGENDR)

#Recode age into categories
NHANES1112$RIDAGEYR <- recode(NHANES1112$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above")
table(NHANES1112$RIDAGEYR)
NHANES1112$RIDAGEYR <- as.factor(NHANES1112$RIDAGEYR)

#Recode education variable
NHANES1112$DMDEDUC2 <- recode(NHANES1112$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES1112$DMDEDUC2 <- unknownToNA(NHANES1112$DMDEDUC2, unknown = c("7", "9")))
table(NHANES1112$DMDEDUC2)
NHANES1112$DMDEDUC2 <- as.factor(NHANES1112$DMDEDUC2)

#Recode annual household income
NHANES1112$INDHHIN2 <- recode(NHANES1112$INDHHIN2,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "14" = ">= $75 000",
                              "15" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES1112$INDHHIN2 <- unknownToNA(NHANES1112$INDHHIN2, unknown = c("77", "99")))
table(NHANES1112$INDHHIN2)
NHANES1112$INDHHIN2 <- as.factor(NHANES1112$INDHHIN2)


#Recode bpq020
NHANES1112$BPQ020 <- recode(NHANES1112$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES1112$BPQ020 <- unknownToNA(NHANES1112$BPQ020, unknown = c("7", "9")))
table(NHANES1112$BPQ020)


#No more copd variable

#Recode employment situaion
NHANES1112$OCD150 <- recode(NHANES1112$OCD150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES1112$OCD150 <- unknownToNA(NHANES1112$OCD150, unknown = c("7", "9"))
table(NHANES1112$OCD150)


#Check class on BMI numeric
class(NHANES1112$BMXBMI)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_1112_dataset <- subset(NHANES1112,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_1112_dataset$WTINT2YR)
#The sum of the weights is 214 783 380, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_1112_dataset[["SDMVPSU"]]))
#3

#Check the number of unique strata
length(unique(NHANES_1112_dataset[["SDMVSTRA"]]))
#The number of unique strata is 14

#To generate unweighted frequency tables
table(subset(NHANES_1112_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_1112_dataset, RIDAGEYR == "70 and above")$RIAGENDR)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_1112_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_1112_dataset)
#Observe the design oject
NHANES_1112_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_1112_DO,
        file = "NHANES1112_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)



#ANALYSIS

#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 2011-12 prevalence 
NA_1112 <- svymean(~factor(MCQ160A), 
                   NHANES_1112_DO, 
                   na.rm = TRUE)
NA1112.c <- NA_1112 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1112_ci <- confint(NA_1112) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA1112 <- bind_cols(NA1112.c, NA1112_ci)
#remove js = 0
NA1112 <- NA1112[-c(1), ] #inal proportion, se & 95% ci
#save for backup
#write.csv(NA1112, "NA1112.csv")



#2. Spatial trends
#no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

#Demographic groups consisted of age, sex, employment status, education level, and annual household income
# - prevalence of arthritis per demographic group
# - logistic regression per demographic group

#Prevalence by age
NHA1112_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_1112_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1112_Arthritis_age <- NHA1112_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1112_arth_age_ci <- confint(NHA1112_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA1112_arth_age_ci <- NHA1112_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_1112_arthritis.age <- bind_cols(NHA1112_Arthritis_age, NHA1112_arth_age_ci) #final proportion, se & 95% ci
#save
#write.csv(NHA_1112_arthritis.age, "NHA1112_Arthritis.age.csv")


#logistic regression by age
NHA1112_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1112_DO)
summary(NHA1112_age_glm)
exp(cbind(OR=coef(NHA1112_age_glm), confint(NHA1112_age_glm)))


#Prevalence by sex
NHA1112_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_1112_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1112_Arthritis_sex <- NHA1112_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1112_arth_sex_ci <- confint(NHA1112_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA1112_arth_sex_ci <- NHA1112_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_1112_arthritis.sex <- bind_cols(NHA1112_Arthritis_sex, NHA1112_arth_sex_ci) #final proportion, se & 95% ci
#save
write.csv(NHA_1112_arthritis.sex, "NHA1112_Arthritis.sex.csv")


#logistic regression by sex
NHA1112_sex_glm <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                          family = quasibinomial,
                          design = NHANES_1112_DO)
summary(NHA1112_sex_glm)
exp(cbind(OR=coef(NHA1112_sex_glm), confint(NHA1112_sex_glm)))


#Prevalence by employment group
NHA1112_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCD150,
                          design = NHANES_1112_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA1112_Arthritis_emp <- NHA1112_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA1112_Arth_emp_ci <- confint(NHA1112_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA1112_Arth_emp_ci <- NHA1112_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA1112_Arthritis.employ <- bind_cols(NHA1112_Arthritis_emp, NHA1112_Arth_emp_ci) #final proportion, se & 95% ci
#save
write.csv(NHA1112_Arthritis.employ, "NHA1112_Arthritis.employment.csv")


#logistic regression by employment group
NHA1112_emp_glm <- svyglm(MCQ160A~relevel(factor(OCD150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1112_DO)
summary(NHA1112_emp_glm)
exp(cbind(OR=coef(NHA1112_emp_glm), confint(NHA1112_emp_glm)))

#logistic regression by education level
NHA1112_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_1112_DO)
summary(NHA1112_educ_glm)
exp(cbind(OR=coef(NHA1112_educ_glm), confint(NHA1112_educ_glm)))

#logistic regression by annual household income
NHA1112_inc_glm <- svyglm(MCQ160A~relevel(INDHHIN2, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_1112_DO)
summary(NHA1112_inc_glm)
exp(cbind(OR=coef(NHA1112_inc_glm), confint(NHA1112_inc_glm)))



#4. Lifestyle trends of arthritis diagnoses

#Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA1112.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_1112_DO)
summary(NA1112.BMI.glm)
exp(cbind(OR=coef(NA1112.BMI.glm), confint(NA1112.BMI.glm)))

#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________



# == 2009-10 == #

#DOWNLOADS

##Demographic Files (and survey information)
NHANES_Demo_0910 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT"))
#Save as RDS
saveRDS(NHANES_Demo_0910,
        file = "NHANES_Demoraw_0910.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##BP (for confounding variable consideration)
NHANES_BPQ_0910 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BPQ_F.XPT"))
#Save as RDS file
saveRDS(NHANES_BPQ_0910,
        file = "NHANES_BPQraw_0910.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Medical conditions
NHANES_MCQ_0910 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/MCQ_F.XPT"))
#Save as RDS file
saveRDS(NHANES_MCQ_0910,
        file = "NHANES_MCQraw_0910.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

##Occupation
NHANES_OCQ_0910 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/OCQ_F.XPT"))
#Save as RDS
saveRDS(NHANES_OCQ_0910,
        file = "NHANES_OCQraw_0910.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Body examination (physical exam)
NHANES_BMX_0910 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.XPT"))
#Save as RDS
saveRDS(NHANES_BMX_0910,
        file = "NHANES_BMXraw_0910.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#Select relevant variables
NHANES_Demo_0910 <- select(NHANES_Demo_0910, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHIN2")
NHANES_BPQ_0910 <- select(NHANES_BPQ_0910, "SEQN", "BPQ020")
NHANES_MCQ_0910 <- select(NHANES_MCQ_0910, "SEQN", "MCQ160A")
NHANES_OCQ_0910 <- select(NHANES_OCQ_0910, "SEQN", "OCD150")
NHANES_BMX_0910 <- select(NHANES_BMX_0910, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0910a <- merge(NHANES_Demo_0910, NHANES_BPQ_0910)
NHANES_0910b <- merge(NHANES_0910a, NHANES_MCQ_0910)
NHANES_0910c <- merge(NHANES_0910b, NHANES_OCQ_0910)
NHANES0910 <- merge(NHANES_0910c, NHANES_BMX_0910)

#Select only age demographic asked about arthritis
NHANES0910 <- NHANES0910[NHANES0910$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0910)
tail(NHANES0910)
glimpse(NHANES0910)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0910))
which(colSums(is.na(NHANES0910)) == nrow(NHANES0910))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0910$MCQ160A <- recode(NHANES0910$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0910$MCQ160A <- unknownToNA(NHANES0910$MCQ160A, unknown = c("7", "9")))
table(NHANES0910$MCQ160A)

NHANES0910$MCQ160A <- as.factor(NHANES0910$MCQ160A)
class(NHANES0910$MCQ160A)

#Recode gender variable
NHANES0910$RIAGENDR <- recode(NHANES0910$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0910$RIAGENDR)
NHANES0910$RIAGENDR <- as.factor(NHANES0910$RIAGENDR)

#Recode age into categories
NHANES0910$RIDAGEYR <- recode(NHANES0910$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above")
table(NHANES0910$RIDAGEYR)
NHANES0910$RIDAGEYR <- as.factor(NHANES0910$RIDAGEYR)

#Recode education variable
NHANES0910$DMDEDUC2 <- recode(NHANES0910$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES0910$DMDEDUC2 <- unknownToNA(NHANES0910$DMDEDUC2, unknown = c("7", "9")))
table(NHANES0910$DMDEDUC2)
NHANES0910$DMDEDUC2 <- as.factor(NHANES0910$DMDEDUC2)

#Recode annual household income
NHANES0910$INDHHIN2 <- recode(NHANES0910$INDHHIN2,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "14" = ">= $75 000",
                              "15" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES0910$INDHHIN2 <- unknownToNA(NHANES0910$INDHHIN2, unknown = c("77", "99")))
table(NHANES0910$INDHHIN2)
NHANES0910$INDHHIN2 <- as.factor(NHANES0910$INDHHIN2)


#Recode bpq020
NHANES0910$BPQ020 <- recode(NHANES0910$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES0910$BPQ020 <- unknownToNA(NHANES0910$BPQ020, unknown = c("7", "9")))
table(NHANES0910$BPQ020)


#No more copd variable

#Recode employment situaion
NHANES0910$OCD150 <- recode(NHANES0910$OCD150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES0910$OCD150 <- unknownToNA(NHANES0910$OCD150, unknown = c("7", "9"))
table(NHANES0910$OCD150)


#Check class on BMI numeric
class(NHANES0910$BMXBMI)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0910_dataset <- subset(NHANES0910,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_0910_dataset$WTINT2YR)
#The sum of the weights is 213 948 116, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0910_dataset[["SDMVPSU"]]))
#3

#For generating unweighted frequency tables (appendix)
table(subset(NHANES_0910_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_0910_dataset, RIDAGEYR == "70 and above")$RIAGENDR)

#Check the number of unique strata
length(unique(NHANES_0910_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0910_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0910_dataset)
#Observe the design oject
NHANES_0910_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_0910_DO,
        file = "NHANES0910_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#ANALYSIS

#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 2009-10 prevalence 
NA_0910 <- svymean(~factor(MCQ160A), 
                   NHANES_0910_DO, 
                   na.rm = TRUE)
NA0910.c <- NA_0910 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0910_ci <- confint(NA_0910) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0910 <- bind_cols(NA0910.c, NA0910_ci)
#remove js = 0
NA0910 <- NA0910[-c(1), ] #final proportion, se & 95% ci
#save for backup
#write.csv(NA0910, "NA0910.csv")


#2. Spatial trends
#no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

#Demographic groups consisted of age, sex, employment status, education level, and annual household income
# - prevalence of arthritis per demographic group
# - logistic regression per demographic group

#Prevalence by age
NHA0910_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_0910_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0910_Arthritis_age <- NHA0910_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0910_arth_age_ci <- confint(NHA0910_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0910_arth_age_ci <- NHA0910_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_0910_arthritis.age <- bind_cols(NHA0910_Arthritis_age, NHA0910_arth_age_ci) #final proportion, se & 95% ci
#save
#write.csv(NHA_0910_arthritis.age, "NHA0910_Arthritis.age.csv")

#logistic regression by age
NHA0910_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0910_DO)
summary(NHA0910_age_glm)
exp(cbind(OR=coef(NHA0910_age_glm), confint(NHA0910_age_glm)))


#Prevalence by sex
NHA0910_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_0910_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0910_Arthritis_sex <- NHA0910_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0910_arth_sex_ci <- confint(NHA0910_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0910_arth_sex_ci <- NHA0910_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_0910_arthritis.sex <- bind_cols(NHA0910_Arthritis_sex, NHA0910_arth_sex_ci)  #final proportion, se & 95% ci
#save for backup
write.csv(NHA_0910_arthritis.sex, "NHA0910_Arthritis.sex.csv")


#logistic regression by sex
NHA0910_sex_glm <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                          family = quasibinomial,
                          design = NHANES_0910_DO)
summary(NHA0910_sex_glm)
exp(cbind(OR=coef(NHA0910_sex_glm), confint(NHA0910_sex_glm)))

#Prevalence by employment group
NHA0910_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCD150,
                          design = NHANES_0910_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0910_Arthritis_emp <- NHA0910_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0910_Arth_emp_ci <- confint(NHA0910_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA0910_Arth_emp_ci <- NHA0910_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA0910_Arthritis.employ <- bind_cols(NHA0910_Arthritis_emp, NHA0910_Arth_emp_ci) #final proportion, se & 95% ci
#save
write.csv(NHA0910_Arthritis.employ, "NHA0910_Arthritis.employment.csv")



#logistic regression by employment group
NHA0910_emp_glm <- svyglm(MCQ160A~relevel(factor(OCD150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0910_DO)
summary(NHA0910_emp_glm)
exp(cbind(OR=coef(NHA0910_emp_glm), confint(NHA0910_emp_glm)))


#logistic regression by education level
NHA0910_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_0910_DO)
summary(NHA0910_educ_glm)
exp(cbind(OR=coef(NHA0910_educ_glm), confint(NHA0910_educ_glm)))

#logistic regression by annual household income
NHA0910_inc_glm <- svyglm(MCQ160A~relevel(INDHHIN2, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0910_DO)
summary(NHA0910_inc_glm)
exp(cbind(OR=coef(NHA0910_inc_glm), confint(NHA0910_inc_glm)))


#4. Lifestyle trends of arthritis diagnoses

#Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA0910.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0910_DO)
summary(NA0910.BMI.glm)
exp(cbind(OR=coef(NA0910.BMI.glm), confint(NA0910.BMI.glm)))


#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________

# == 2007-08 == #

#DOWNLOADS

##Demograhic files (and survey information)
NHANES_Demo_0708 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.XPT"))
#Save as RDS
saveRDS(NHANES_Demo_0708,
        file = "NHANES_Demoraw_0708.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)



##BP (for consideration of confounding variable)
NHANES_BPQ_0708 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BPQ_E.XPT"))
#Save as RDS file
saveRDS(NHANES_BPQ_0708,
        file = "NHANES_BPQraw_0708.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)



##Medical Conditions
NHANES_MCQ_0708 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/MCQ_E.XPT"))
#Save as RDS file
saveRDS(NHANES_MCQ_0708,
        file = "NHANES_MCQraw_0708.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Occupation 
NHANES_OCQ_0708 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/OCQ_E.XPT"))
#Save as RDS file
saveRDS(NHANES_OCQ_0708,
        file = "NHANES_OCQraw_0708.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Body examination (physical exam)
NHANES_BMX_0708 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.XPT"))
#Save as RDS file
saveRDS(NHANES_BMX_0708,
        file = "NHANES_BMXraw_0708.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#Selecting relevant variables
NHANES_Demo_0708 <- select(NHANES_Demo_0708, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHIN2")
NHANES_BPQ_0708 <- select(NHANES_BPQ_0708, "SEQN", "BPQ020")
NHANES_MCQ_0708 <- select(NHANES_MCQ_0708, "SEQN", "MCQ160A")
NHANES_OCQ_0708 <- select(NHANES_OCQ_0708, "SEQN", "OCD150")
NHANES_BMX_0708 <- select(NHANES_BMX_0708, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0708a <- merge(NHANES_Demo_0708, NHANES_BPQ_0708)
NHANES_0708b <- merge(NHANES_0708a, NHANES_MCQ_0708)
NHANES_0708c <- merge(NHANES_0708b, NHANES_OCQ_0708)
NHANES0708 <- merge(NHANES_0708c, NHANES_BMX_0708)

#Select only age demographic asked about arthritis
NHANES0708 <- NHANES0708[NHANES0708$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0708)
tail(NHANES0708)
glimpse(NHANES0708)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0708))
which(colSums(is.na(NHANES0708)) == nrow(NHANES0708))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0708$MCQ160A <- recode(NHANES0708$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0708$MCQ160A <- unknownToNA(NHANES0708$MCQ160A, unknown = c("7", "9")))
table(NHANES0708$MCQ160A)

NHANES0708$MCQ160A <- as.factor(NHANES0708$MCQ160A)
class(NHANES0708$MCQ160A)

#Recode gender variable
NHANES0708$RIAGENDR <- recode(NHANES0708$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0708$RIAGENDR)
NHANES0708$RIAGENDR <- as.factor(NHANES0708$RIAGENDR)

#Recode age into categories
NHANES0708$RIDAGEYR <- recode(NHANES0708$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above")
table(NHANES0708$RIDAGEYR)
NHANES0708$RIDAGEYR <- as.factor(NHANES0708$RIDAGEYR)

#Recode education variable
NHANES0708$DMDEDUC2 <- recode(NHANES0708$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES0708$DMDEDUC2 <- unknownToNA(NHANES0708$DMDEDUC2, unknown = c("7", "9")))
table(NHANES0708$DMDEDUC2)
NHANES0708$DMDEDUC2 <- as.factor(NHANES0708$DMDEDUC2)

#Recode annual household income
NHANES0708$INDHHIN2 <- recode(NHANES0708$INDHHIN2,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "14" = ">= $75 000",
                              "15" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES0708$INDHHIN2 <- unknownToNA(NHANES0708$INDHHIN2, unknown = c("77", "99")))
table(NHANES0708$INDHHIN2)
NHANES0708$INDHHIN2 <- as.factor(NHANES0708$INDHHIN2)


#Recode bpq020
NHANES0708$BPQ020 <- recode(NHANES0708$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES0708$BPQ020 <- unknownToNA(NHANES0708$BPQ020, unknown = c("7", "9")))
table(NHANES0708$BPQ020)


#No more copd variable

#Recode employment situaion
NHANES0708$OCD150 <- recode(NHANES0708$OCD150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES0708$OCD150 <- unknownToNA(NHANES0708$OCD150, unknown = c("7", "9"))
table(NHANES0708$OCD150)


#Check class on BMI numeric
class(NHANES0708$BMXBMI)

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0708_dataset <- subset(NHANES0708,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_0708_dataset$WTINT2YR)
#The sum of the weights is 207 936 631, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0708_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_0708_dataset[["SDMVSTRA"]]))
#The number of unique strata is 16

#Used to generate the unweighted frequency tables
table(subset(NHANES_0708_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_0708_dataset, RIDAGEYR == "70 and above")$RIAGENDR)


#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0708_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0708_dataset)
#Observe the design oject
NHANES_0708_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_0708_DO,
        file = "NHANES0708_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#ANALYSIS

#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 2007-08 prevalence 
NA_0708 <- svymean(~factor(MCQ160A), 
                   NHANES_0708_DO, 
                   na.rm = TRUE)
NA0708.c <- NA_0708 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0708_ci <- confint(NA_0708) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0708 <- bind_cols(NA0708.c, NA0708_ci)
#remove js = 0
NA0708 <- NA0708[-c(1), ] #final proportion, se & 95% ci
#save for backup
#write.csv(NA0708, "NA0708.csv")


#2. Spatial trends
#no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

#Demographic groups consisted of age, sex, employment status, education level, and annual household income
# - prevalence of arthritis per demographic group
# - logistic regression per demographic group

#Prevalence by age
NHA0708_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_0708_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0708_Arthritis_age <- NHA0708_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0708_arth_age_ci <- confint(NHA0708_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0708_arth_age_ci <- NHA0708_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_0708_arthritis.age <- bind_cols(NHA0708_Arthritis_age, NHA0708_arth_age_ci) #proportion, se & 95% ci
#save for backup
#write.csv(NHA_0708_arthritis.age, "NHA0708_Arthritis.age.csv")


#logistic regression by age
NHA0708_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0708_DO)
summary(NHA0708_age_glm)
exp(cbind(OR=coef(NHA0708_age_glm), confint(NHA0708_age_glm)))


#Prevalence by sex
NHA0708_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_0708_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0708_Arthritis_sex <- NHA0708_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0708_arth_sex_ci <- confint(NHA0708_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0708_arth_sex_ci <- NHA0708_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_0708_arthritis.sex <- bind_cols(NHA0708_Arthritis_sex, NHA0708_arth_sex_ci) #final proportion, se & 95% ci
#save
write.csv(NHA_0708_arthritis.sex, "NHA0708_Arthritis.sex.csv")


#logistic regression by sex
NHA0708_sex_glm <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                          family = quasibinomial,
                          design = NHANES_0708_DO)
summary(NHA0708_sex_glm)
exp(cbind(OR=coef(NHA0708_sex_glm), confint(NHA0708_sex_glm)))


#Prevalence by employment group
NHA0708_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCD150,
                          design = NHANES_0708_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0708_Arthritis_emp <- NHA0708_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0708_Arth_emp_ci <- confint(NHA0708_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA0708_Arth_emp_ci <- NHA0708_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA0708_Arthritis.employ <- bind_cols(NHA0708_Arthritis_emp, NHA0708_Arth_emp_ci) #final proportion, se & 95% ci
#save for backup
write.csv(NHA0708_Arthritis.employ, "NHA0708_Arthritis.employment.csv")


#logistic regression by employment group
NHA0708_emp_glm <- svyglm(MCQ160A~relevel(factor(OCD150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0708_DO)
summary(NHA0708_emp_glm)
exp(cbind(OR=coef(NHA0708_emp_glm), confint(NHA0708_emp_glm)))


#logistic regression by education level
NHA0708_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_0708_DO)
summary(NHA0708_educ_glm)
exp(cbind(OR=coef(NHA0708_educ_glm), confint(NHA0708_educ_glm)))

#logistic regression by annual household income
NHA0708_inc_glm <- svyglm(MCQ160A~relevel(INDHHIN2, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0708_DO)
summary(NHA0708_inc_glm)
exp(cbind(OR=coef(NHA0708_inc_glm), confint(NHA0708_inc_glm)))


#4. Lifestyle trends of arthritis diagnoses

#Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA0708.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0708_DO)
summary(NA0708.BMI.glm)
exp(cbind(OR=coef(NA0708.BMI.glm), confint(NA0708.BMI.glm)))

#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________


# == 2005-06 == #

#DOWNLOADS

##Demographic file (and survey information)
NHANES_Demo_0506 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT"))
#Save as RDS file
saveRDS(NHANES_Demo_0506,
        file = "NHANES_Demoraw_0506.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##BP (to consider confounding variables)
NHANES_BPQ_0506 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BPQ_D.XPT"))
#Save as RDS file
saveRDS(NHANES_BPQ_0506,
        file = "NHANES_BPQraw_0506.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Medical conditions
NHANES_MCQ_0506 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/MCQ_D.XPT"))
#Save as RDS file
saveRDS(NHANES_MCQ_0506,
        file = "NHANES_MCQraw_0506.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Occupation
NHANES_OCQ_0506 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OCQ_D.XPT"))
#Save as RDS file
saveRDS(NHANES_OCQ_0506,
        file = "NHANES_OCQraw_0506.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Body examination (physical exam)
NHANES_BMX_0506 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.XPT"))
#Save as RDS
saveRDS(NHANES_BMX_0506,
        file = "NHANES_BMXraw_0506.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#Select relevant variables
NHANES_Demo_0506 <- select(NHANES_Demo_0506, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHINC")
NHANES_BPQ_0506 <- select(NHANES_BPQ_0506, "SEQN", "BPQ020")
NHANES_MCQ_0506 <- select(NHANES_MCQ_0506, "SEQN", "MCQ160A")
NHANES_OCQ_0506 <- select(NHANES_OCQ_0506, "SEQN", "OCD150")
NHANES_BMX_0506 <- select(NHANES_BMX_0506, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0506a <- merge(NHANES_Demo_0506, NHANES_BPQ_0506)
NHANES_0506b <- merge(NHANES_0506a, NHANES_MCQ_0506)
NHANES_0506c <- merge(NHANES_0506b, NHANES_OCQ_0506)
NHANES0506 <- merge(NHANES_0506c, NHANES_BMX_0506)

#Select only age demographic asked about arthritis
NHANES0506 <- NHANES0506[NHANES0506$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0506)
tail(NHANES0506)
glimpse(NHANES0506)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0506))
which(colSums(is.na(NHANES0506)) == nrow(NHANES0506))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0506$MCQ160A <- recode(NHANES0506$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0506$MCQ160A <- unknownToNA(NHANES0506$MCQ160A, unknown = c("7", "9")))
table(NHANES0506$MCQ160A)

NHANES0506$MCQ160A <- as.factor(NHANES0506$MCQ160A)
class(NHANES0506$MCQ160A)

#Recode gender variable
NHANES0506$RIAGENDR <- recode(NHANES0506$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0506$RIAGENDR)
NHANES0506$RIAGENDR <- as.factor(NHANES0506$RIAGENDR)

#Recode age into categories
NHANES0506$RIDAGEYR <- recode(NHANES0506$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above",
                              "81" = "70 and above",
                              "82" = "70 and above",
                              "83" = "70 and above", 
                              "84" = "70 and above",
                              "85" = "70 and above")
table(NHANES0506$RIDAGEYR)
NHANES0506$RIDAGEYR <- as.factor(NHANES0506$RIDAGEYR)

#Recode education variable
NHANES0506$DMDEDUC2 <- recode(NHANES0506$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES0506$DMDEDUC2 <- unknownToNA(NHANES0506$DMDEDUC2, unknown = c("7", "9")))
table(NHANES0506$DMDEDUC2)
NHANES0506$DMDEDUC2 <- as.factor(NHANES0506$DMDEDUC2)

#Recode annual household income
NHANES0506$INDHHINC <- recode(NHANES0506$INDHHINC,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "11" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES0506$INDHHINC <- unknownToNA(NHANES0506$INDHHINC, unknown = c("77", "99")))
table(NHANES0506$INDHHINC)
NHANES0506$INDHHINC <- as.factor(NHANES0506$INDHHINC)


#Recode bpq020
NHANES0506$BPQ020 <- recode(NHANES0506$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES0506$BPQ020 <- unknownToNA(NHANES0506$BPQ020, unknown = c("7", "9")))
table(NHANES0506$BPQ020)


#No more copd variable

#Recode employment situaion
NHANES0506$OCD150 <- recode(NHANES0506$OCD150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES0506$OCD150 <- unknownToNA(NHANES0506$OCD150, unknown = c("7", "9"))
table(NHANES0506$OCD150)


#Check class on BMI numeric
class(NHANES0506$BMXBMI)
#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0506_dataset <- subset(NHANES0506,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))


#To generate unwieghted frequency tables (appendix)
table(subset(NHANES_0506_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_0506_dataset, RIDAGEYR == "70 and above")$RIAGENDR)



#Check that the sum of the weights is equal to the US population
sum(NHANES_0506_dataset$WTINT2YR)
#The sum of the weights is 201 916 632, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0506_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_0506_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0506_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0506_dataset)
#Observe the design oject
NHANES_0506_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_0506_DO,
        file = "NHANES0506_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)

#ANALYSIS

#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 2005-06 prevalence 
NA_0506 <- svymean(~factor(MCQ160A), 
                   NHANES_0506_DO, 
                   na.rm = TRUE)
NA0506.c <- NA_0506 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0506_ci <- confint(NA_0506) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0506 <- bind_cols(NA0506.c, NA0506_ci)
#remove js = 0
NA0506 <- NA0506[-c(1), ] #final proportion, se & 95% ci
#save for backup
#write.csv(NA0506, "NA0506.csv")


#2. Spatial trends
#no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

#Demographic groups consisted of age, sex, employment status, education level, and annual household income
# - prevalence of arthritis per demographic group
# - logistic regression per demographic group

#Prevalence by age
NHA0506_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_0506_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0506_Arthritis_age <- NHA0506_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0506_arth_age_ci <- confint(NHA0506_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0506_arth_age_ci <- NHA0506_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_0506_arthritis.age <- bind_cols(NHA0506_Arthritis_age, NHA0506_arth_age_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA_0506_arthritis.age, "NHA0506_Arthritis.age.csv")


#logistic regression by age
NHA0506_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0506_DO)
summary(NHA0506_age_glm)
exp(cbind(OR=coef(NHA0506_age_glm), confint(NHA0506_age_glm)))


#Prevalence by sex
NHA0506_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_0506_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0506_Arthritis_sex <- NHA0506_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0506_arth_sex_ci <- confint(NHA0506_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0506_arth_sex_ci <- NHA0506_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_0506_arthritis.sex <- bind_cols(NHA0506_Arthritis_sex, NHA0506_arth_sex_ci) #final proportion, se & 95% ci
#save for backup
write.csv(NHA_0506_arthritis.sex, "NHA0506_Arthritis.sex.csv")


#logistic regression by sex
NHA0506_sex_glm <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                          family = quasibinomial,
                          design = NHANES_0506_DO)
summary(NHA0506_sex_glm)
exp(cbind(OR=coef(NHA0506_sex_glm), confint(NHA0506_sex_glm)))


#Prevalence by employment group
NHA0506_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCD150,
                          design = NHANES_0506_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0506_Arthritis_emp <- NHA0506_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0506_Arth_emp_ci <- confint(NHA0506_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA0506_Arth_emp_ci <- NHA0506_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA0506_Arthritis.employ <- bind_cols(NHA0506_Arthritis_emp, NHA0506_Arth_emp_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA0506_Arthritis.employ, "NHA0506_Arthritis.employment.csv")

#logistic regression by employment group
NHA0506_emp_glm <- svyglm(MCQ160A~relevel(factor(OCD150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0506_DO)
summary(NHA0506_emp_glm)
exp(cbind(OR=coef(NHA0506_emp_glm), confint(NHA0506_emp_glm)))


#logistic regression by education level
NHA0506_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_0506_DO)
summary(NHA0506_educ_glm)
exp(cbind(OR=coef(NHA0506_educ_glm), confint(NHA0506_educ_glm)))

#logistic regression by annual household income
NHA0506_inc_glm <- svyglm(MCQ160A~relevel(INDHHINC, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0506_DO)
summary(NHA0506_inc_glm)
exp(cbind(OR=coef(NHA0506_inc_glm), confint(NHA0506_inc_glm)))


#4. Lifestyle trends of arthritis diagnoses

#Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA0506.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0506_DO)
summary(NA0506.BMI.glm)
exp(cbind(OR=coef(NA0506.BMI.glm), confint(NA0506.BMI.glm)))

#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________


# == 2003-04 == #

#DOWNLOADS

##Demographic data (and survey information)
NHANES_Demo_0304 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT"))
#Save as RDS file
saveRDS(NHANES_Demo_0304,
        file = "NHANES_Demoraw_0304.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##BP (for consideration of confounders)
NHANES_BPQ_0304 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BPQ_C.XPT"))
#Save as RDS file
saveRDS(NHANES_BPQ_0304,
        file = "NHANES_BPQraw_0304.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Medical conditions
NHANES_MCQ_0304 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/MCQ_C.XPT"))
#Save as RDS file
saveRDS(NHANES_MCQ_0304,
        file = "NHANES_MCQraw_0304.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Occupation
NHANES_OCQ_0304 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/OCQ_C.XPT"))
#Save as RDS file
saveRDS(NHANES_OCQ_0304,
        file = "NHANES_OCQraw_0304.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#Body examination (physical exam)
NHANES_BMX_0304 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.XPT"))
#Save as RDS
saveRDS(NHANES_BMX_0304,
        file = "NHANES_BMXraw_0304.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#Selecting relevant variables
NHANES_Demo_0304 <- select(NHANES_Demo_0304, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHINC")
NHANES_BPQ_0304 <- select(NHANES_BPQ_0304, "SEQN", "BPQ020")
NHANES_MCQ_0304 <- select(NHANES_MCQ_0304, "SEQN", "MCQ160A")
NHANES_OCQ_0304 <- select(NHANES_OCQ_0304, "SEQN", "OCD150")
NHANES_BMX_0304 <- select(NHANES_BMX_0304, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0304a <- merge(NHANES_Demo_0304, NHANES_BPQ_0304)
NHANES_0304b <- merge(NHANES_0304a, NHANES_MCQ_0304)
NHANES_0304c <- merge(NHANES_0304b, NHANES_OCQ_0304)
NHANES0304 <- merge(NHANES_0304c, NHANES_BMX_0304)

#Select only age demographic asked about arthritis
NHANES0304 <- NHANES0304[NHANES0304$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0304)
tail(NHANES0304)
glimpse(NHANES0304)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0304))
which(colSums(is.na(NHANES0304)) == nrow(NHANES0304))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0304$MCQ160A <- recode(NHANES0304$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0304$MCQ160A <- unknownToNA(NHANES0304$MCQ160A, unknown = c("7", "9")))
table(NHANES0304$MCQ160A)

NHANES0304$MCQ160A <- as.factor(NHANES0304$MCQ160A)
class(NHANES0304$MCQ160A)

#Recode gender variable
NHANES0304$RIAGENDR <- recode(NHANES0304$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0304$RIAGENDR)
NHANES0304$RIAGENDR <- as.factor(NHANES0304$RIAGENDR)

#Recode age into categories
NHANES0304$RIDAGEYR <- recode(NHANES0304$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above",
                              "81" = "70 and above",
                              "82" = "70 and above",
                              "83" = "70 and above", 
                              "84" = "70 and above",
                              "85" = "70 and above")
table(NHANES0304$RIDAGEYR)
NHANES0304$RIDAGEYR <- as.factor(NHANES0304$RIDAGEYR)

#Recode education variable
NHANES0304$DMDEDUC2 <- recode(NHANES0304$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES0304$DMDEDUC2 <- unknownToNA(NHANES0304$DMDEDUC2, unknown = c("7", "9")))
table(NHANES0304$DMDEDUC2)
NHANES0304$DMDEDUC2 <- as.factor(NHANES0304$DMDEDUC2)

#Recode annual household income
NHANES0304$INDHHINC <- recode(NHANES0304$INDHHINC,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "11" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES0304$INDHHINC <- unknownToNA(NHANES0304$INDHHINC, unknown = c("77", "99")))
table(NHANES0304$INDHHINC)
NHANES0304$INDHHINC <- as.factor(NHANES0304$INDHHINC)


#Recode bpq020
NHANES0304$BPQ020 <- recode(NHANES0304$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES0304$BPQ020 <- unknownToNA(NHANES0304$BPQ020, unknown = c("7", "9")))
table(NHANES0304$BPQ020)


#No more copd variable

#Recode employment situaion
NHANES0304$OCD150 <- recode(NHANES0304$OCD150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES0304$OCD150 <- unknownToNA(NHANES0304$OCD150, unknown = c("7", "9"))
table(NHANES0304$OCD150)


#Check class on BMI numeric
class(NHANES0304$BMXBMI)
#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0304_dataset <- subset(NHANES0304,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_0304_dataset$WTINT2YR)
#The sum of the weights is 193 472 727, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0304_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_0304_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Used to generate unweighted frequency tables (appendix)
table(subset(NHANES_0304_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_0304_dataset, RIDAGEYR == "70 and above")$RIAGENDR)


#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0304_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0304_dataset)
#Observe the design oject
NHANES_0304_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_0304_DO,
        file = "NHANES0304_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#ANALYSIS

#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 2003-04 prevalence 
NA_0304 <- svymean(~factor(MCQ160A), 
                   NHANES_0304_DO, 
                   na.rm = TRUE)
NA0304.c <- NA_0304 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0304_ci <- confint(NA_0304) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0304 <- bind_cols(NA0304.c, NA0304_ci)
#remove js = 0
NA0304 <- NA0304[-c(1), ] #final proportion, se & 95% ci
#save for backup
#write.csv(NA0304, "NA0304.csv")



#2. Spatial trends
#no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

#Demographic groups consisted of age, sex, employment status, education level, and annual household income
# - prevalence of arthritis per demographic group
# - logistic regression per demographic group

#Prevalence by age
#Prevalence by age
NHA0304_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_0304_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0304_Arthritis_age <- NHA0304_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0304_arth_age_ci <- confint(NHA0304_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0304_arth_age_ci <- NHA0304_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_0304_arthritis.age <- bind_cols(NHA0304_Arthritis_age, NHA0304_arth_age_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA_0304_arthritis.age, "NHA0304_Arthritis.age.csv")


#logistic regression by age
NHA0304_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0304_DO)
summary(NHA0304_age_glm)
exp(cbind(OR=coef(NHA0304_age_glm), confint(NHA0304_age_glm)))


#Prevalence by sex
NHA0304_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_0304_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0304_Arthritis_sex <- NHA0304_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0304_arth_sex_ci <- confint(NHA0304_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0304_arth_sex_ci <- NHA0304_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_0304_arthritis.sex <- bind_cols(NHA0304_Arthritis_sex, NHA0304_arth_sex_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA_0304_arthritis.sex, "NHA0304_Arthritis.sex.csv")


#logistic regression by sex
NHA0304_sex_glm <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                          family = quasibinomial,
                          design = NHANES_0304_DO)
summary(NHA0304_sex_glm)
exp(cbind(OR=coef(NHA0304_sex_glm), confint(NHA0304_sex_glm)))


#Prevalence by employment group
NHA0304_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCD150,
                          design = NHANES_0304_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0304_Arthritis_emp <- NHA0304_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0304_Arth_emp_ci <- confint(NHA0304_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA0304_Arth_emp_ci <- NHA0304_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA0304_Arthritis.employ <- bind_cols(NHA0304_Arthritis_emp, NHA0304_Arth_emp_ci) #final proprtion, se & 95% ci
#save for backup
#write.csv(NHA0304_Arthritis.employ, "NHA0304_Arthritis.employment.csv")


#logistic regression by employment group
NHA0304_emp_glm <- svyglm(MCQ160A~relevel(factor(OCD150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0304_DO)
summary(NHA0304_emp_glm)
exp(cbind(OR=coef(NHA0304_emp_glm), confint(NHA0304_emp_glm)))


#logistic regression by education level
NHA0304_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_0304_DO)
summary(NHA0304_educ_glm)
exp(cbind(OR=coef(NHA0304_educ_glm), confint(NHA0304_educ_glm)))

#logistic regression by annual household income
NHA0304_inc_glm <- svyglm(MCQ160A~relevel(INDHHINC, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0304_DO)
summary(NHA0304_inc_glm)

exp(cbind(OR=coef(NHA0304_inc_glm), confint(NHA0304_inc_glm)))


#4. Lifestyle trends of arthritis diagnoses

#Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA0304.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0304_DO)
summary(NA0304.BMI.glm)
exp(cbind(OR=coef(NA0304.BMI.glm), confint(NA0304.BMI.glm)))

#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________


# == 2001-02 == #

#DOWNLOADS

##Demographic data (and survey information)
NHANES_Demo_0102 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.XPT"))
#Save as RDS file
saveRDS(NHANES_Demo_0102,
        file = "NHANES_Demoraw_0102.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##BP (for consideration of confounding variables)
NHANES_BPQ_0102 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BPQ_B.XPT"))
#Save as RDS file 
saveRDS(NHANES_BPQ_0102,
        file = "NHANES_BPQraw_0102.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Medical conditions
NHANES_MCQ_0102 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/MCQ_B.XPT"))
#Save as RDS file
saveRDS(NHANES_MCQ_0102,
        file = "NHANES_MCQraw_0102.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Occupation
NHANES_OCQ_0102 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/OCQ_B.XPT"))
#Save as RDS file
saveRDS(NHANES_OCQ_0102,
        file = "NHANES_OCQraw_0102.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Body examination (physical exam)
NHANES_BMX_0102 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BMX_B.XPT"))
#Save as RDS 
saveRDS(NHANES_BMX_0102,
        file = "NHANES_BMXraw_0102.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

NHANES_Demo_0102 <- select(NHANES_Demo_0102, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHINC")
NHANES_BPQ_0102 <- select(NHANES_BPQ_0102, "SEQN", "BPQ020")
NHANES_MCQ_0102 <- select(NHANES_MCQ_0102, "SEQN", "MCQ160A")
NHANES_OCQ_0102 <- select(NHANES_OCQ_0102, "SEQN", "OCD150")
NHANES_BMX_0102 <- select(NHANES_BMX_0102, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0102a <- merge(NHANES_Demo_0102, NHANES_BPQ_0102)
NHANES_0102b <- merge(NHANES_0102a, NHANES_MCQ_0102)
NHANES_0102c <- merge(NHANES_0102b, NHANES_OCQ_0102)
NHANES0102 <- merge(NHANES_0102c, NHANES_BMX_0102)

#Select only age demographic asked about arthritis
NHANES0102 <- NHANES0102[NHANES0102$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0102)
tail(NHANES0102)
glimpse(NHANES0102)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0102))
which(colSums(is.na(NHANES0102)) == nrow(NHANES0102))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0102$MCQ160A <- recode(NHANES0102$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0102$MCQ160A <- unknownToNA(NHANES0102$MCQ160A, unknown = c("7", "9")))
table(NHANES0102$MCQ160A)

NHANES0102$MCQ160A <- as.factor(NHANES0102$MCQ160A)
class(NHANES0102$MCQ160A)

#Recode gender variable
NHANES0102$RIAGENDR <- recode(NHANES0102$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0102$RIAGENDR)
NHANES0102$RIAGENDR <- as.factor(NHANES0102$RIAGENDR)

#Recode age into categories
NHANES0102$RIDAGEYR <- recode(NHANES0102$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above",
                              "81" = "70 and above",
                              "82" = "70 and above",
                              "83" = "70 and above", 
                              "84" = "70 and above",
                              "85" = "70 and above")
table(NHANES0102$RIDAGEYR)
NHANES0102$RIDAGEYR <- as.factor(NHANES0102$RIDAGEYR)

#Recode education variable
NHANES0102$DMDEDUC2 <- recode(NHANES0102$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES0102$DMDEDUC2 <- unknownToNA(NHANES0102$DMDEDUC2, unknown = c("7", "9")))
table(NHANES0102$DMDEDUC2)
NHANES0102$DMDEDUC2 <- as.factor(NHANES0102$DMDEDUC2)

#Recode annual household income
NHANES0102$INDHHINC <- recode(NHANES0102$INDHHINC,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "11" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES0102$INDHHINC <- unknownToNA(NHANES0102$INDHHINC, unknown = c("77", "99")))
table(NHANES0102$INDHHINC)
NHANES0102$INDHHINC <- as.factor(NHANES0102$INDHHINC)


#Recode bpq020
NHANES0102$BPQ020 <- recode(NHANES0102$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES0102$BPQ020 <- unknownToNA(NHANES0102$BPQ020, unknown = c("7", "9")))
table(NHANES0102$BPQ020)


#No more copd variable

#Recode employment situaion
NHANES0102$OCD150 <- recode(NHANES0102$OCD150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES0102$OCD150 <- unknownToNA(NHANES0102$OCD150, unknown = c("7", "9"))
table(NHANES0102$OCD150)


#Check class on BMI numeric
class(NHANES0102$BMXBMI)
#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0102_dataset <- subset(NHANES0102,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))


#Check that the sum of the weights is equal to the US population
sum(NHANES_0102_dataset$WTINT2YR)
#The sum of the weights is 189 051 537, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0102_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_0102_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#To generate unweighted frequency tables (appendix)
table(subset(NHANES_0102_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_0102_dataset, RIDAGEYR == "70 and above")$RIAGENDR)

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0102_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0102_dataset)
#Observe the design oject
NHANES_0102_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_0102_DO,
        file = "NHANES0102_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)



#ANALYSIS

#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 2001-02 prevalence 
NA_0102 <- svymean(~factor(MCQ160A), 
                   NHANES_0102_DO, 
                   na.rm = TRUE)
NA0102.c <- NA_0102 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0102_ci <- confint(NA_0102) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0102 <- bind_cols(NA0102.c, NA0102_ci)
#remove js = 0
NA0102 <- NA0102[-c(1), ] #final proportion, se & 95% ci
#save
#write.csv(NA0102, "NA0102.csv")



#2. Spatial trends
#no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

#Demographic groups consisted of age, sex, employment status, education level, and annual household income
# - prevalence of arthritis per demographic group
# - logistic regression per demographic group

#Prevalence by age
NHA0102_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_0102_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0102_Arthritis_age <- NHA0102_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0102_arth_age_ci <- confint(NHA0102_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0102_arth_age_ci <- NHA0102_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_0102_arthritis.age <- bind_cols(NHA0102_Arthritis_age, NHA0102_arth_age_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA_0102_arthritis.age, "NHA0102_Arthritis.age.csv")


#logistic regression by age
NHA0102_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0102_DO)
summary(NHA0102_age_glm)
exp(cbind(OR=coef(NHA0102_age_glm), confint(NHA0102_age_glm)))


#Prevalence by sex
NHA0102_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_0102_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0102_Arthritis_sex <- NHA0102_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0102_arth_sex_ci <- confint(NHA0102_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA0102_arth_sex_ci <- NHA0102_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_0102_arthritis.sex <- bind_cols(NHA0102_Arthritis_sex, NHA0102_arth_sex_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA_0102_arthritis.sex, "NHA0102_Arthritis.sex.csv")



#logistic regression by sex
NHA0102_sex_glm <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                          family = quasibinomial,
                          design = NHANES_0102_DO)
summary(NHA0102_sex_glm)
exp(cbind(OR=coef(NHA0102_sex_glm), confint(NHA0102_sex_glm)))


#Prevalence by employment group
NHA0102_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCD150,
                          design = NHANES_0102_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA0102_Arthritis_emp <- NHA0102_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA0102_Arth_emp_ci <- confint(NHA0102_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA0102_Arth_emp_ci <- NHA0102_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA0102_Arthritis.employ <- bind_cols(NHA0102_Arthritis_emp, NHA0102_Arth_emp_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA0102_Arthritis.employ, "NHA0102_Arthritis.employment.csv")


#logistic regression by employment group
NHA0102_emp_glm <- svyglm(MCQ160A~relevel(factor(OCD150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0102_DO)
summary(NHA0102_emp_glm)
exp(cbind(OR=coef(NHA0102_emp_glm), confint(NHA0102_emp_glm)))


#logistic regression by education level
NHA0102_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_0102_DO)
summary(NHA0102_educ_glm)
exp(cbind(OR=coef(NHA0102_educ_glm), confint(NHA0102_educ_glm)))

#logistic regression by annual household income
NHA0102_inc_glm <- svyglm(MCQ160A~relevel(INDHHINC, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_0102_DO)
summary(NHA0102_inc_glm)
exp(cbind(OR=coef(NHA0102_inc_glm), confint(NHA0102_inc_glm)))


#4. Lifestyle trends of arthritis diagnoses

#Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA0102.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0102_DO)
summary(NA0102.BMI.glm)
exp(cbind(OR=coef(NA0102.BMI.glm), confint(NA0102.BMI.glm)))


#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________

# == 1999-2000 == #

#DOWNLOADS

##Demographic data (with sampling weights)
NHANES_Demo_9900 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.XPT"))
#Save as RDS file
saveRDS(NHANES_Demo_9900,
        file = "NHANES_Demoraw_9900.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##BP (to consider confounding variables)
NHANES_BPQ_9900 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BPQ.XPT"))
#Save as RDS file
saveRDS(NHANES_BPQ_9900,
        file = "NHANES_BPQraw_9900.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Medical conditions
NHANES_MCQ_9900 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/MCQ.XPT"))
#Save as RDS file
saveRDS(NHANES_MCQ_9900,
        file = "NHANES_MCQraw_9900.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Occupation
NHANES_OCQ_9900 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/OCQ.XPT"))
#Save as RDS file
saveRDS(NHANES_OCQ_9900,
        file = "NHANES_OCQraw_9900.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


##Body examination (physical exam)
NHANES_BMX_9900 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BMX.XPT"))
#Save as RDS file
saveRDS(NHANES_BMX_9900,
        file = "NHANES_BMXraw_9900.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#CLEANING

#select relevant variables
NHANES_Demo_9900 <- select(NHANES_Demo_9900, "SEQN", "RIAGENDR", "RIDAGEYR", "DMDEDUC2", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "INDHHINC")
NHANES_BPQ_9900 <- select(NHANES_BPQ_9900, "SEQN", "BPQ020")
NHANES_MCQ_9900 <- select(NHANES_MCQ_9900, "SEQN", "MCQ160A")
NHANES_OCQ_9900 <- select(NHANES_OCQ_9900, "SEQN", "OCQ150")
NHANES_BMX_9900 <- select(NHANES_BMX_9900, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_9900a <- merge(NHANES_Demo_9900, NHANES_BPQ_9900)
NHANES_9900b <- merge(NHANES_9900a, NHANES_MCQ_9900)
NHANES_9900c <- merge(NHANES_9900b, NHANES_OCQ_9900)
NHANES9900 <- merge(NHANES_9900c, NHANES_BMX_9900)

#Select only age demographic asked about arthritis
NHANES9900 <- NHANES9900[NHANES9900$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES9900)
tail(NHANES9900)
glimpse(NHANES9900)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES9900))
which(colSums(is.na(NHANES9900)) == nrow(NHANES9900))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES9900$MCQ160A <- recode(NHANES9900$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES9900$MCQ160A <- unknownToNA(NHANES9900$MCQ160A, unknown = c("7", "9")))
table(NHANES9900$MCQ160A)

NHANES9900$MCQ160A <- as.factor(NHANES9900$MCQ160A)
class(NHANES9900$MCQ160A)

#Recode gender variable
NHANES9900$RIAGENDR <- recode(NHANES9900$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES9900$RIAGENDR)
NHANES9900$RIAGENDR <- as.factor(NHANES9900$RIAGENDR)

#Recode age into categories
NHANES9900$RIDAGEYR <- recode(NHANES9900$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
                              "25" = "25 to 29",
                              "26" = "25 to 29",
                              "27" = "25 to 29",
                              "28" = "25 to 29",
                              "29" = "25 to 29",
                              "30" = "30 to 34",
                              "31" = "30 to 34",
                              "32" = "30 to 34",
                              "33" = "30 to 34",
                              "34" = "30 to 34",
                              "35" = "35 to 39",
                              "36" = "35 to 39",
                              "37" = "35 to 39",
                              "38" = "35 to 39",
                              "39" = "35 to 39",
                              "40" = "40 to 44",
                              "41" = "40 to 44",
                              "42" = "40 to 44",
                              "43" = "40 to 44",
                              "44" = "40 to 44",
                              "45" = "45 to 49",
                              "46" = "45 to 49",
                              "47" = "45 to 49",
                              "48" = "45 to 49",
                              "49" = "45 to 49",
                              "50" = "50 to 54",
                              "51" = "50 to 54",
                              "52" = "50 to 54",
                              "53" = "50 to 54",
                              "54" = "50 to 54",
                              "55" = "55 to 59",
                              "56" = "55 to 59",
                              "57" = "55 to 59",
                              "58" = "55 to 59",
                              "59" = "55 to 59",
                              "60" = "60 to 64",
                              "61" = "60 to 64",
                              "62" = "60 to 64",
                              "63" = "60 to 64",
                              "64" = "60 to 64",
                              "65" = "65 to 69",
                              "66" = "65 to 69",
                              "67" = "65 to 69",
                              "68" = "65 to 69",
                              "69" = "65 to 69",
                              "70" = "70 and above",
                              "71" = "70 and above",
                              "72" = "70 and above",
                              "73" = "70 and above",
                              "74" = "70 and above",
                              "75" = "70 and above",
                              "76" = "70 and above",
                              "77" = "70 and above",
                              "78" = "70 and above",
                              "79" = "70 and above",
                              "80" = "70 and above",
                              "81" = "70 and above",
                              "82" = "70 and above",
                              "83" = "70 and above", 
                              "84" = "70 and above",
                              "85" = "70 and above")
table(NHANES9900$RIDAGEYR)
NHANES9900$RIDAGEYR <- as.factor(NHANES9900$RIDAGEYR)

#Recode education variable
NHANES9900$DMDEDUC2 <- recode(NHANES9900$DMDEDUC2,
                              "1" = "< 9th grade",
                              "2" = "9th - 11th grade",
                              "3" = "High school graduate/equivalent",
                              "4" = "Some college/AA degree",
                              "5" = "College graduate or above",
                              "7" = "7",
                              "9" = "9")
#Change 7 and 9 values to NA
(NHANES9900$DMDEDUC2 <- unknownToNA(NHANES9900$DMDEDUC2, unknown = c("7", "9")))
table(NHANES9900$DMDEDUC2)
NHANES9900$DMDEDUC2 <- as.factor(NHANES9900$DMDEDUC2)

#Recode annual household income
NHANES9900$INDHHINC <- recode(NHANES9900$INDHHINC,
                              "1" = "$0 to $4 999",
                              "2" = "$5 000 to $9 999",
                              "3" = "$10 000 to $14 999",
                              "4" = "$15 000 to $19 999",
                              "5" = "$20 000 to $24 999",
                              "6" = "$25 000 to $34 999",
                              "7" = "$35 000 to $44 999",
                              "8" = "$45 000 to $54 999",
                              "9" = "$55 000 to $64 999",
                              "10" = "$65 000 to $74 999",
                              "11" = ">= $75 000",
                              "77" = "77",
                              "99" = "99")
(NHANES9900$INDHHINC <- unknownToNA(NHANES9900$INDHHINC, unknown = c("77", "99")))
table(NHANES9900$INDHHINC)
NHANES9900$INDHHINC <- as.factor(NHANES9900$INDHHINC)


#Recode bpq020
NHANES9900$BPQ020 <- recode(NHANES9900$BPQ020,
                            "1" = "1",
                            "2" = "0",
                            "7" = "7",
                            "9" = "9")
(NHANES9900$BPQ020 <- unknownToNA(NHANES9900$BPQ020, unknown = c("7", "9")))
table(NHANES9900$BPQ020)


#No more copd variable

#Recode employment situaion
NHANES9900$OCQ150 <- recode(NHANES9900$OCQ150,
                            "1" = "Working at a job/business",
                            "2" = "9",
                            "3" = "9",
                            "4" = "Not working at a job/business",
                            "7" = "7",
                            "9" = "9")
NHANES9900$OCQ150 <- unknownToNA(NHANES9900$OCQ150, unknown = c("7", "9"))
table(NHANES9900$OCQ150)


#Check class on BMI numeric
class(NHANES9900$BMXBMI)
#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_9900_dataset <- subset(NHANES9900,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_9900_dataset$WTINT2YR)
#The sum of the weights is 174 783 278, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_9900_dataset[["SDMVPSU"]]))
#3

#Check the number of unique strata
length(unique(NHANES_9900_dataset[["SDMVSTRA"]]))
#The number of unique strata is 13

#Used to generate unweighted frequency tables (appendix)
table(subset(NHANES_9900_dataset, RIDAGEYR == "20 to 24")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "25 to 29")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "30 to 34")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "35 to 39")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "40 to 44")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "45 to 49")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "50 to 54")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "55 to 59")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "60 to 64")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "65 to 69")$RIAGENDR)
table(subset(NHANES_9900_dataset, RIDAGEYR == "70 and above")$RIAGENDR)


#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_9900_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_9900_dataset)
#Observe the design oject
NHANES_9900_DO

#Save design object as RDS file for ease of retrieval for analysis, thereby avoiding running the data download and the design object creation code multiple times
saveRDS(NHANES_9900_DO,
        file = "NHANES9900_DO.rds",
        ascii = FALSE,
        compress = "gzip",
        refhook = NULL)


#ANALYSIS

#1. Prevalence of arthritis diagnoses for the survey cycle, in the overall population

#Overall 1999-2000 prevalence 
NA_9900 <- svymean(~factor(MCQ160A), 
                   NHANES_9900_DO, 
                   na.rm = TRUE)
NA9900.c <- NA_9900 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA9900_ci <- confint(NA_9900) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA9900 <- bind_cols(NA9900.c, NA9900_ci)
#remove js = 0
NA9900 <- NA9900[-c(1), ] #final proportion, se & 95% ci
#save for backup
#write.csv(NA9900, "NA9900.csv")


#2. Spatial trends
#no spatial data available for NHANES


#3. Demographic trends of arthritis diagnoses

#Demographic groups consisted of age, sex, employment status, education level, and annual household income
# - prevalence of arthritis per demographic group
# - logistic regression per demographic group

#Prevalence by age
NHA9900_Arth_age <- svyby(formula = ~MCQ160A,
                          by = ~ RIDAGEYR,
                          design = NHANES_9900_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA9900_Arthritis_age <- NHA9900_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA9900_arth_age_ci <- confint(NHA9900_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA9900_arth_age_ci <- NHA9900_arth_age_ci[-c(1:11), ]
#join proportions and ci
NHA_9900_arthritis.age <- bind_cols(NHA9900_Arthritis_age, NHA9900_arth_age_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA_9900_arthritis.age, "NHA9900_Arthritis.age.csv")



#logistic regression by age
NHA9900_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_9900_DO)
summary(NHA9900_age_glm)
exp(cbind(OR=coef(NHA9900_age_glm), confint(NHA9900_age_glm)))


#Prevalence by sex
NHA9900_Arth_sex <- svyby(formula = ~MCQ160A,
                          by = ~ RIAGENDR,
                          design = NHANES_9900_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA9900_Arthritis_sex <- NHA9900_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA9900_arth_sex_ci <- confint(NHA9900_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove ci for no arth
NHA9900_arth_sex_ci <- NHA9900_arth_sex_ci[-c(1:2), ]
#join proportions and ci
NHA_9900_arthritis.sex <- bind_cols(NHA9900_Arthritis_sex, NHA9900_arth_sex_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA_9900_arthritis.sex, "NHA9900_Arthritis.sex.csv")


#logistic regression by sex
NHA9900_sex_glm_c <- svyglm(MCQ160A~relevel(RIAGENDR, ref = "Male") + RIDAGEYR,
                            family = quasibinomial,
                            design = NHANES_9900_DO)
summary(NHA9900_sex_glm_c)
exp(cbind(OR=coef(NHA9900_sex_glm_c), confint(NHA9900_sex_glm_c)))


#Prevalence by employment group
NHA9900_Arth_emp <- svyby(formula = ~MCQ160A,
                          by = ~OCQ150,
                          design = NHANES_9900_DO,
                          FUN = svymean,
                          na.rm = TRUE)
NHA9900_Arthritis_emp <- NHA9900_Arth_emp %>%
  select(1, 3, 5) %>%
  setNames(c("Employment", "Proportion", "SE")) %>%
  mutate(Employment = as.factor(Employment)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NHA9900_Arth_emp_ci <- confint(NHA9900_Arth_emp) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#remove ci for arth = 0
NHA9900_Arth_emp_ci <- NHA9900_Arth_emp_ci[-c(1:2), ]
#join ci and proportion
NHA9900_Arthritis.employ <- bind_cols(NHA9900_Arthritis_emp, NHA9900_Arth_emp_ci) #final proportion, se & 95% ci
#save for backup
#write.csv(NHA9900_Arthritis.employ, "NHA9900_Arthritis.employment.csv")

#logistic regression by employment group
NHA9900_emp_glm <- svyglm(MCQ160A~relevel(factor(OCQ150, ordered = FALSE), ref = "Working at a job/business") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_9900_DO)
summary(NHA9900_emp_glm)
exp(cbind(OR=coef(NHA9900_emp_glm), confint(NHA9900_emp_glm)))


#logistic regression by education level
NHA9900_educ_glm <- svyglm(MCQ160A~relevel(DMDEDUC2, ref = "College graduate or above") + RIDAGEYR + RIAGENDR,
                           family = quasibinomial,
                           design = NHANES_9900_DO)
summary(NHA9900_educ_glm)
exp(cbind(OR=coef(NHA9900_educ_glm), confint(NHA9900_educ_glm)))


#logistic regression by annual household income
NHA9900_inc_glm <- svyglm(MCQ160A~relevel(INDHHINC, ref = ">= $75 000") + RIDAGEYR + RIAGENDR,
                          family = quasibinomial,
                          design = NHANES_9900_DO)
summary(NHA9900_inc_glm)
exp(cbind(OR=coef(NHA9900_inc_glm), confint(NHA9900_inc_glm)))


#4. Lifestyle trends of arthritis diagnoses

#Lifestyle group consisted only of BMI (no physical activity)

#logistic regression by BMI
NA9900.BMI.glm <- svyglm(MCQ160A ~ BMXBMI + RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_9900_DO)
summary(NA9900.BMI.glm)
exp(cbind(OR=coef(NA9900.BMI.glm), confint(NA9900.BMI.glm)))

#_____________________________________________________________________________________
#_____________________________________________________________________________________
#_____________________________________________________________________________________

#COMPOUND ANNUAL GROWTH RATE ACROSS TIME


#CAGR
CAGR_formula <- function(EndVal, StartVal, Yrs) {
  values <- (((EndVal/StartVal)^(1/(2*Yrs)))-1)
  return(values)
} #NHANES compounded every two years, therefore multiply number of years by 2



#Overall
NAend <- as.data.frame(NA1718)
NAstart <- as.data.frame(NA9900)

(names(NAend)[names(NAend) == "Proportion"] <- "PropNA1718")
(names(NAstart)[names(NAstart) == "Proportion"] <- "PropNA9900")
(names(NAend)[names(NAend) == "SE"] <- "SE2")
(names(NAend)[names(NAend) == "CI_Prop_low"] <- "CI_Prop_low2")
(names(NAend)[names(NAend) == "CI_Prop_upp"] <- "CI_Prop_upp2")

#Merge
NHA <- cbind(NAend, NAstart)

#CAGR formula
NHA %>%
  mutate(CAGR_formula(PropNA1718, PropNA9900, 20))

#Age
NAend_age <- as.data.frame(NHA_1718_arthritis.age)
NAstart_age <- as.data.frame(NHA_9900_arthritis.age)

(names(NAend_age)[names(NAend_age) == "Proportion"] <- "PropNA1718")
(names(NAstart_age)[names(NAstart_age) == "Proportion"] <- "PropNA9900")
(names(NAend_age)[names(NAend_age) == "SE"] <- "SE2")
(names(NAend_age)[names(NAend_age) == "CI_Prop_low"] <- "CI_Prop_low2")
(names(NAend_age)[names(NAend_age) == "CI_Prop_upp"] <- "CI_Prop_upp2")

#Merge
NA.Age <- merge(NAstart_age, NAend_age, by = "Age")
#CAGR function
NA.Age %>%
  mutate(CAGR_formula(PropNA1718, PropNA9900, 20))


#Sex
NAend_sex <- as.data.frame(NHA_1718_arthritis.sex)
NAstart_sex <- as.data.frame(NHA_9900_arthritis.sex)

(names(NAend_sex)[names(NAend_sex) == "Proportion"] <- "PropNA1718")
(names(NAstart_sex)[names(NAstart_sex) == "Proportion"] <- "PropNA9900")
(names(NAend_sex)[names(NAend_sex) == "SE"] <- "SE2")
(names(NAend_sex)[names(NAend_sex) == "CI_Prop_low"] <- "CI_Prop_low2")
(names(NAend_sex)[names(NAend_sex) == "CI_Prop_upp"] <- "CI_Prop_upp2")

#Merge
NA.Sex <- merge(NAstart_sex, NAend_sex, by = "Sex")
#CAGR function
NA.Sex %>%
  mutate(CAGR_formula(PropNA1718, PropNA9900, 20))



#Employment
NAend_emp <- as.data.frame(NHA1718_Arthritis.employ)
NAstart_emp <- as.data.frame(NHA9900_Arthritis.employ)

(names(NAend_emp)[names(NAend_emp) == "Proportion"] <- "PropNA1718")
(names(NAstart_emp)[names(NAstart_emp) == "Proportion"] <- "PropNA9900")
(names(NAend_emp)[names(NAend_emp) == "SE"] <- "SE2")
(names(NAend_emp)[names(NAend_emp) == "CI_Prop_low"] <- "CI_Prop_low2")
(names(NAend_emp)[names(NAend_emp) == "CI_Prop_upp"] <- "CI_Prop_upp2")

#Merge
NA.emp <- merge(NAstart_emp, NAend_emp, by = "Employment")
#CAGR function
NA.emp %>%
  mutate(CAGR_formula(PropNA1718, PropNA9900, 20))

#NHANES FIGURES:

#make quick function for every nth year shown on x-axis
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

# I. Temporal figures
#   i)   Overall prevalence per year

#Add years onto the dfs
NA1718$Year <- c("2017-18")
NA1516$Year <- c("2015-16")
NA1314$Year <- c("2013-14")
NA1112$Year <- c("2011-12")
NA0910$Year <- c("2009-10")
NA0708$Year <- c("2007-08")
NA0506$Year <- c("2005-06")
NA0304$Year <- c("2003-04")
NA0102$Year <- c("2001-02")
NA9900$Year <- c("1999-2000")

NA_arth <- Reduce(function(x, y) merge(x, y, all=TRUE), list(NA1718, NA1516, NA1314, NA1112, NA0910, NA0708, NA0506, NA0304, NA0102, NA9900))

#Temporal plot
NA_arth.fig <- ggplot(data = NA_arth,
                      aes(x = Year, y = Proportion, group = 1)) +
  geom_line(colour = "#009933") +
  geom_point(colour = "#009933") +
  geom_ribbon(aes(ymin = NA_arth$CI_Prop_low, ymax = NA_arth$CI_Prop_upp), alpha = 0.2, fill = "#009933") +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("NHANES survey cycles (years)")
ggsave("NHA.arth-time.png", width = 18, height = 11)

#   ii)  Age-sepcific prevalence per year

#Make convenient labels
NA1718_age <- NHA_1718_arthritis.age
NA1516_age <- NHA_1516_arthritis.age
NA1314_age <- NHA_1314_arthritis.age
NA1112_age <- NHA_1112_arthritis.age
NA0910_age <- NHA_0910_arthritis.age
NA0708_age <- NHA_0708_arthritis.age
NA0506_age <- NHA_0506_arthritis.age
NA0304_age <- NHA_0304_arthritis.age
NA0102_age <- NHA_0102_arthritis.age
NA9900_age <- NHA_9900_arthritis.age

#Add years
NA1718_age$Year <- c("2017-18", "2017-18", "2017-18", "2017-18", "2017-18", "2017-18", "2017-18", "2017-18", "2017-18", "2017-18", "2017-18")
NA1516_age$Year <- c("2015-16", "2015-16", "2015-16", "2015-16", "2015-16", "2015-16", "2015-16", "2015-16", "2015-16", "2015-16", "2015-16")
NA1314_age$Year <- c("2013-14", "2013-14", "2013-14", "2013-14", "2013-14", "2013-14", "2013-14", "2013-14", "2013-14", "2013-14", "2013-14")
NA1112_age$Year <- c("2011-12", "2011-12", "2011-12", "2011-12", "2011-12", "2011-12", "2011-12", "2011-12", "2011-12", "2011-12", "2011-12")
NA0910_age$Year <- c("2009-10", "2009-10", "2009-10", "2009-10", "2009-10", "2009-10", "2009-10", "2009-10", "2009-10", "2009-10", "2009-10")
NA0708_age$Year <- c("2007-08", "2007-08", "2007-08", "2007-08", "2007-08", "2007-08", "2007-08", "2007-08", "2007-08", "2007-08", "2007-08")
NA0506_age$Year <- c("2005-06", "2005-06", "2005-06", "2005-06", "2005-06", "2005-06", "2005-06", "2005-06", "2005-06", "2005-06", "2005-06")
NA0304_age$Year <- c("2003-04", "2003-04", "2003-04", "2003-04", "2003-04", "2003-04", "2003-04", "2003-04", "2003-04", "2003-04", "2003-04")
NA0102_age$Year <- c("2001-02", "2001-02", "2001-02", "2001-02", "2001-02", "2001-02", "2001-02", "2001-02", "2001-02", "2001-02", "2001-02")
NA9900_age$Year <- c("1999-2000", "1999-2000", "1999-2000", "1999-2000", "1999-2000", "1999-2000", "1999-2000", "1999-2000", "1999-2000", "1999-2000", "1999-2000")

NHA_age <- Reduce(function(x, y) merge(x, y, all=TRUE), list(NA1718_age, NA1516_age, NA1314_age, NA1112_age, NA0910_age, NA0708_age, NA0506_age, NA0304_age, NA0102_age, NA9900_age))

#Age temporal plot
NA.age.fig <- ggplot(data = NHA_age,
                     aes(x = Year, y = Proportion, group = Age)) +
  geom_line(colour = "#009933") +
  geom_point(colour = "#009933") +
  geom_ribbon(aes(ymin = NHA_age$CI_Prop_low, ymax = NHA_age$CI_Prop_upp), alpha = 0.2, fill = "#009933") +
  facet_wrap(facets = ~Age) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("NHANES survey cycles (years)") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16))
ggsave("NHA.arth-time-age.png", width = 18, height = 12)


#   iii) Sex-specific prevalence per year

#Make labels convenient
NA1718_sex <- NHA_1718_arthritis.sex
NA1516_sex <- NHA_1516_arthritis.sex
NA1314_sex <- NHA_1314_arthritis.sex
NA1112_sex <- NHA_1112_arthritis.sex
NA0910_sex <- NHA_0910_arthritis.sex
NA0708_sex <- NHA_0708_arthritis.sex
NA0506_sex <- NHA_0506_arthritis.sex
NA0304_sex <- NHA_0304_arthritis.sex
NA0102_sex <- NHA_0102_arthritis.sex
NA9900_sex <- NHA_9900_arthritis.sex

#Add years
NA1718_sex$Year <- c("2017-18", "2017-18")
NA1516_sex$Year <- c("2015-16", "2015-16")
NA1314_sex$Year <- c("2013-14", "2013-14")
NA1112_sex$Year <- c("2011-12", "2011-12")
NA0910_sex$Year <- c("2009-10", "2009-10")
NA0708_sex$Year <- c("2007-08", "2007-08")
NA0506_sex$Year <- c("2005-06", "2005-06")
NA0304_sex$Year <- c("2003-04", "2003-04")
NA0102_sex$Year <- c("2001-02", "2001-02")
NA9900_sex$Year <- c("1999-2000", "1999-2000")

#Merge
NHA_sex <- Reduce(function(x, y) merge(x, y, all=TRUE), list(NA1718_sex, NA1516_sex, NA1314_sex, NA1112_sex, NA0910_sex, NA0708_sex, NA0506_sex, NA0304_sex, NA0102_sex, NA9900_sex))

#Sex temporal plot
NA.sex.fig <- ggplot(data = NHA_sex,
                     aes(x = Year, y = Proportion, group = Sex)) +
  geom_line(colour = "#009933") +
  geom_point(colour = "#009933") +
  geom_ribbon(aes(ymin = NHA_sex$CI_Prop_low, ymax = NHA_sex$CI_Prop_upp), alpha = 0.2, fill = "#009933") +
  facet_wrap(facets = ~Sex) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("NHANES survey cycles (years)") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16))
ggsave("NHA.arth-time-sex.png", width = 18, height = 12)

#   iv)  Employment-specific prevalence per year


#Make convenient labels
NA1718_emp <- NHA1718_Arthritis.employ
NA1516_emp <- NHA1516_Arthritis.employ
NA1314_emp <- NHA1314_Arthritis.employ
NA1112_emp <- NHA1112_Arthritis.employ
NA0910_emp <- NHA0910_Arthritis.employ
NA0708_emp <- NHA0708_Arthritis.employ
NA0506_emp <- NHA0506_Arthritis.employ
NA0304_emp <- NHA0304_Arthritis.employ
NA0102_emp <- NHA0102_Arthritis.employ
NA9900_emp <- NHA9900_Arthritis.employ

#Add years
NA1718_emp$Year <- c("2017-18", "2017-18")
NA1516_emp$Year <- c("2015-16", "2015-16")
NA1314_emp$Year <- c("2013-14", "2013-14")
NA1112_emp$Year <- c("2011-12", "2011-12")
NA0910_emp$Year <- c("2009-10", "2009-10")
NA0708_emp$Year <- c("2007-08", "2007-08")
NA0506_emp$Year <- c("2005-06", "2005-06")
NA0304_emp$Year <- c("2003-04", "2003-04")
NA0102_emp$Year <- c("2001-02", "2001-02")
NA9900_emp$Year <- c("1999-2000", "1999-2000")

#Merge
NHA_emp <- Reduce(function(x, y) merge(x, y, all=TRUE), list(NA1718_emp, NA1516_emp, NA1314_emp, NA1112_emp, NA0910_emp, NA0708_emp, NA0506_emp, NA0304_emp, NA0102_emp, NA9900_emp))

#Employment temporal plot
NA.emp.fig <- ggplot(data = NHA_emp,
                     aes(x = Year, y = Proportion, group = Employment)) +
  geom_line(colour = "#009933") +
  geom_point(colour = "#009933") +
  geom_ribbon(aes(ymin = NHA_emp$CI_Prop_low, ymax = NHA_emp$CI_Prop_upp), alpha = 0.2, fill = "#009933") +
  facet_wrap(facets = ~Employment) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  scale_x_discrete(breaks = every_nth(n = 2)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 18, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  xlab("NHANES survey cycles (years)") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16))
ggsave("NHA.arth-time-employment.png", width = 18, height = 12)


# II. Demographic trends

#   i)  Age-specific trends

N_age_plot <- ggplot(data = NHA_age,
                     aes(x = Age, y = Proportion)) +
  geom_bar(stat = "identity", fill = "#009933") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE), colour = "black", size = 0.4, width = 0.3, position = position_dodge(0.9)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Age group (years)") +
  facet_wrap(facets = ~Year) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16)) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100))
ggsave("NHA_age.png", width = 18, height = 12)


#   ii) Sex-specific trends

N_sex_plot <- ggplot(data = NHA_sex,
                     aes(x = Sex, y = Proportion)) +
  geom_bar(stat = "identity", fill = "#009933") +
  geom_errorbar(aes(ymin = Proportion-SE, ymax = Proportion+SE), colour = "black", size = 0.4, width = 0.3, position = position_dodge(0.9)) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  xlab("Sex") +
  facet_wrap(facets = ~Year) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 16)) +
  scale_y_continuous(name = "Percentage (%)", breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100))
ggsave("NHA_sex.png", width = 18, height = 12)


# ------- END NHANES ARTHRITIS ANALYSIS HERE ------- #
