### Author: Farhad Panahov
### Purpose: Prepare Egypt LFS file



########################
### START ##############
########################
# clear all
rm(list = ls())
cat("\014")

# packages
if (!require(tidyverse)) {
  install.packages("tidyverse")
}
library(tidyverse)


if (!require(readxl)) {
  install.packages("readxl")
}
library(readxl)

if (!require(readr)) {
  install.packages("readr")
}
library(readr)

if (!require(haven)) {
  install.packages("haven")
}
library(haven)


if (!require(stringr)) {
  install.packages("stringr")
}
library(stringr)


#working directory for files
setwd("C:/Users/panah/OneDrive/Desktop/rdir/1 - WB Green skills")





#######################################
### SECTION 1: LOAD DATA ##############
#######################################

############
# LFS 2022 #
############

# load metadata
df_lfs_egypt <- read_dta("1 - input/3 - lfs/2 - egypt/Egypt 2022-LFS IND-V1.dta")


# isic --- industry at 2 digits
df_isic <- read.csv("1 - input/4 - isic rev4.csv")
df_isic$Code <- ifelse(nchar(df_isic$Code) == 1,
                                       str_c("0", df_isic$Code),
                       df_isic$Code) # add leading zero to groups starting with zero, since they miss it


# isco
df_isco <- read_excel("1 - input/5 - isco structure.xlsx")





############################################
### SECTION 2: LFS DATA EDITS ##############
############################################

####################################
### LABOUOR FORCE INDICATORS #######
####################################

# Indicator name: MAS
# 1 Employed 
# 2 Unemployed
# 3 Homemaker (Housewife) 
# 4 Student 
# 5 Pensioners/retired/disabled
# 6 Others 
# 99 Not stated


# 1 --- employed
df_lfs_egypt$emp <- 0
df_lfs_egypt$emp[df_lfs_egypt$mas == 1] <- 1


# 2 --- unemployed
df_lfs_egypt$unemp <- 0
df_lfs_egypt$unemp[df_lfs_egypt$mas == 2] <- 1


# 3 --- not in the labour force
df_lfs_egypt$nilf <- 0
df_lfs_egypt$nilf[df_lfs_egypt$lfs == 2] <- 1

  

########################
### EMPLOYMENT TYPE ####
########################

# divide employment by types
df_lfs_egypt$emp_salaried <- 0 # salaried employees
df_lfs_egypt$emp_salaried[df_lfs_egypt$emps == 1] <-1

df_lfs_egypt$emp_owner <- 0 # business owner / self-employed
df_lfs_egypt$emp_owner[df_lfs_egypt$emps == 2 | df_lfs_egypt$emps == 3] <- 1

df_lfs_egypt$emp_other <- 0 # all other
df_lfs_egypt$emp_other[(df_lfs_egypt$emps %in% c(4,5,6))] <- 1



###############
### INCOME ####
###############

df_lfs_egypt$income <- coalesce(df_lfs_egypt$totwag, 0) +
  coalesce(df_lfs_egypt$irrgwag, 0) +
  coalesce(df_lfs_egypt$empinc, 0) +
  coalesce(df_lfs_egypt$sempinc, 0)



############################
### STANDARDIZE EDUC #######
############################

df_lfs_egypt$educ_standard <- NA
df_lfs_egypt$educ_standard[df_lfs_egypt$educ == 1] <- "no formal schooling"
df_lfs_egypt$educ_standard[df_lfs_egypt$educ == 2] <- "primary" 
df_lfs_egypt$educ_standard[df_lfs_egypt$educ == 3] <- "secondary" 
df_lfs_egypt$educ_standard[df_lfs_egypt$educ == 4 | df_lfs_egypt$educ == 5 | df_lfs_egypt$educ == 6] <-  "post-secondary & university"  



####################
### SUBSET #########
####################

### SUBSET LFS TO CONTAION ONLY:
df_lfs_egypt <- df_lfs_egypt %>% select(c(sex, age,
                                          educ, educ_standard, emp, emp_salaried, totwag, pweight, occ_isco08_4, ind_isic4_4)) %>%  subset()

colnames(df_lfs_egypt) <- c("sex","age","educ", "educ_standard" , "emp", "salaried", "wage", "weight", "isco_4","isic_4")



############################
### ADD OCCUP & INDUSTRY ###
############################

### ADD OCCUPATION & INDUSTRY GROUP CODES
# get 1 & 3 digit isco
df_lfs_egypt$isco_3 <- substr(df_lfs_egypt$isco_4, 1,3)
df_lfs_egypt$isco_1 <- substr(df_lfs_egypt$isco_4, 1,1)


# get industry 2 digit --- first add missing leading zero
df_lfs_egypt$isic_4 <- sprintf("%04d", df_lfs_egypt$isic_4)
df_lfs_egypt$isic_2 <- substr(df_lfs_egypt$isic_4,1,2)


# occupation names
df_lfs_egypt$isco_1_name <- df_isco$`Title EN`[match(df_lfs_egypt$isco_1, df_isco$`ISCO 08 Code`)]
df_lfs_egypt$isco_3_name <- df_isco$`Title EN`[match(df_lfs_egypt$isco_3, df_isco$`ISCO 08 Code`)]


# add industry group
df_lfs_egypt$isic_2_name <- df_isic$ISIC.Rev..4.label[match(df_lfs_egypt$isic_2, df_isic$Code)]
df_lfs_egypt$isic_1_name <- df_isic$Section..1.digit.[match(df_lfs_egypt$isic_2, df_isic$Code)]


# remove
rm(df_isco, df_isic)



##################
### ADD SKILLS ###
##################

### ADD OCCUPATION SKILLS LEVELS
df_lfs_egypt$skill_level <- NA
df_lfs_egypt$skill_level[df_lfs_egypt$isco_1 == 9] <- "level 1"
df_lfs_egypt$skill_level[df_lfs_egypt$isco_1 %in% c(4,5,6,7,8)] <- "level 2"
df_lfs_egypt$skill_level[df_lfs_egypt$isco_1 %in% c(1,2,3)] <- "level 3/4"





##################################
### SAVE #########################
##################################

save.image("3 - env/3 - lfs - egypt.R.RData")

write.csv(df_lfs_egypt, "2 - output/3 - lfs egypt_raw.csv")


