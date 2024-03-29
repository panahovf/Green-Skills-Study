### Author: Farhad Panahov
### Purpose: Gets isco08/esco at 3 digit level and mapps skills and gets share of green skills



########################
### START ##############
########################
# clear all
rm(list = ls())
cat("\014")

# packages
if(!require(tidyverse))
  install.packages("tidyverse")
library(tidyverse)

if(!require(readxl))
  install.packages("readxl")
library(readxl)



#working directory for files
setwd("C:/Users/panah/OneDrive/Desktop/rdir/1 - WB Green skills")





#######################################
### SECTION 1: LOAD DATA ##############
#######################################

# occupations
df_esco_occup_hierarchy <- read.csv("1 - input/2 - esco/broaderRelationsOccPillar_en.csv")
df_esco_occup_unit <- read.csv("1 - input/2 - esco/occupations_en.csv")
df_esco_skills_occup <- read.csv("1 - input/2 - esco/occupationSkillRelations_en.csv")


# skills
df_esco_skills_green <- read.csv("1 - input/2 - esco/greenSkillsCollection_en.csv")
df_esco_skills_digital <- read.csv("1 - input/2 - esco/digitalSkillsCollection_en.csv")
df_esco_skills_research <- read.csv("1 - input/2 - esco/researchSkillsCollection_en.csv")
df_esco_skills_comp <- read.csv("1 - input/2 - esco/digCompSkillsCollection_en.csv")
df_esco_skills_transversal <- read.csv("1 - input/2 - esco/transversalSkillsCollection_en.csv")
df_esco_skills <- read.csv("1 - input/2 - esco/skills_en.csv")


# isco 08
df_isco08 <- read_excel("1 - input/5 - isco structure.xlsx")





##########################
### MAKE EDITS: SKILLS ###
##########################

# keep only essentail skills for each occupation
#df_esco_skills_occup <- df_esco_skills_occup %>% filter(.$relationType == "essential") %>% subset()


# name the skill
df_esco_skills_occup$name <- df_esco_skills$preferredLabel[match(df_esco_skills_occup$skillUri, df_esco_skills$conceptUri)]


# skill category
df_esco_skills_occup$category <- "general"


# add green
temp_indices <- which(df_esco_skills_occup$skillUri %in% df_esco_skills_green$conceptUri)
df_esco_skills_occup$category[temp_indices] <- "green"


# add digital
temp_indices <- which(df_esco_skills_occup$skillUri %in% df_esco_skills_digital$conceptUri)
df_esco_skills_occup$category[temp_indices] <- "stem"


# add digital comp
temp_indices <- which(df_esco_skills_occup$skillUri %in% df_esco_skills_comp$conceptUri)
df_esco_skills_occup$category[temp_indices] <- "stem"


# add research
temp_indices <- which(df_esco_skills_occup$skillUri %in% df_esco_skills_research$conceptUri)
df_esco_skills_occup$category[temp_indices] <- "stem"


# remove
rm(temp_indices)
rm(df_esco_skills_comp, df_esco_skills_research, df_esco_skills_green, df_esco_skills_digital, df_esco_skills_transversal)


# check skills count
length(unique(df_esco_skills_occup$skillUri[df_esco_skills_occup$category == "green"])) # --> 544
length(unique(df_esco_skills_occup$skillUri[df_esco_skills_occup$category == "stem"])) # --> 120
length(unique(df_esco_skills_occup$skillUri[df_esco_skills_occup$category == "general"])) # --> 1203



#######################################
### SECTION 2: MAKE EDIT ##############
#######################################

# identify occupational blocks (digits --- eg: 7543.10.2.3 --- 7543 = block 1, .10 = block 2, etc.)
df_esco_occup_unit$num_blocks <- str_count(df_esco_occup_unit$code, fixed(".")) + 1


# identify count of occupations/group --- this is for general knowledge --- not used for quantitative purposes
length(unique(df_esco_occup_unit$iscoGroup)) ### --> 426
sum(df_esco_occup_unit$num_blocks == 2) ### --> 1760
sum(df_esco_occup_unit$num_blocks == 3) ### --> 1072
sum(df_esco_occup_unit$num_blocks == 4) ### --> 135
sum(df_esco_occup_unit$num_blocks == 5) ### --> 40
sum(df_esco_occup_unit$num_blocks == 6) ### --> 0


### keep only lowest level occupations
# 1 remove isco groups in level down
df_esco_occup_hierarchy2 <- df_esco_occup_hierarchy %>% filter(df_esco_occup_hierarchy$conceptType != "ISCOGroup")


# 2 select only occupations that are level down, and dont show up as a roll up occupation
df_esco_occup_hierarchy2 <- df_esco_occup_hierarchy2 %>% filter(!(df_esco_occup_hierarchy2$conceptUri %in% 
                                                                    df_esco_occup_hierarchy2$broaderUri)) %>% subset()


# 3 filter occup units to lowest levels only
df_esco_occup_unit <- df_esco_occup_unit %>% filter(df_esco_occup_unit$conceptUri %in% df_esco_occup_hierarchy2$conceptUri) %>% subset()


# for skills add the occupation code
df_esco_skills_occup$code <- df_esco_occup_unit$code[match(df_esco_skills_occup$occupationUri, 
                                                           df_esco_occup_unit$conceptUri)]


# filter skills X occup --- keeps only occupations at lowest level
df_esco_skills_occup <- df_esco_skills_occup %>% filter(df_esco_skills_occup$occupationUri %in% df_esco_occup_unit$conceptUri)


# remove
rm(df_esco_occup_hierarchy, df_esco_occup_hierarchy2)





#########################################
### SECTION 3: GREEN SHARE ##############
#########################################

###############
### 3 digit ###
###############

# get 1 and 3 digit major and minor codes
df_esco_skills_occup$code_major <- substr(df_esco_skills_occup$code,1,1)
df_esco_skills_occup$code_minor <- substr(df_esco_skills_occup$code,1,3)


# summarize by occup group
df_green_minor <- df_esco_skills_occup %>% group_by(code_minor, occupationUri, category) %>% summarise(count = n_distinct(skillUri))


# lond to wide format
df_green_minor <- df_green_minor %>% pivot_wider(names_from = category, values_from = count)


# get skills count as numeric
# if NA replace with zero
df_green_minor$green[is.na(df_green_minor$green)] <- 0
df_green_minor$stem[is.na(df_green_minor$stem)] <- 0
df_green_minor$general[is.na(df_green_minor$general)] <- 0
df_green_minor$total_skills <- df_green_minor$general + df_green_minor$green + df_green_minor$stem


# find green share
df_green_minor$green_share <- df_green_minor$green / df_green_minor$total_skills


# summarize
df_green_minor <- df_green_minor %>% group_by(code_minor) %>% summarise(green_share = mean(green_share), 
                                                                        total_skills = sum(total_skills), # here total skills are not unique
                                                                        total_green_skills = sum(green), # here total skills are not unique
                                                                        total_occups = n_distinct(occupationUri))


# get average number of skills per occupation
df_green_minor$avg_skills <- df_green_minor$total_skills/df_green_minor$total_occups





#########################################
### SECTION 4: GREEN COUNT ##############
#########################################

# this gets unique skills --- previous count measures gets all skills count

###############
### 3 digit ###
###############

# Summarize by minor groups to get skills count & pivot wider
df_minor_skills_count <- df_esco_skills_occup %>% group_by(code_minor, category) %>% summarise(count = n_distinct(skillUri))
df_minor_skills_count <- df_minor_skills_count %>%pivot_wider(names_from = category, values_from = count)


# replace NA's with zeros, and get total
df_minor_skills_count <- df_minor_skills_count %>% mutate(across(everything(), ~replace(., is.na(.), 0))) # replace NAs with zeros
df_minor_skills_count$total <- df_minor_skills_count$general + df_minor_skills_count$green + df_minor_skills_count$stem


# Add counts to 'df_green_minor' master file
df_green_minor$count_total <- df_minor_skills_count$total[match(df_green_minor$code_minor, df_minor_skills_count$code_minor)]
df_green_minor$count_green <- df_minor_skills_count$green[match(df_green_minor$code_minor, df_minor_skills_count$code_minor)]
df_green_minor$count_stem <- df_minor_skills_count$stem[match(df_green_minor$code_minor, df_minor_skills_count$code_minor)]


# remove
rm(df_minor_skills_count)





#####################################################
### SECTION 5: ADD MINOR OCCUPATION GROUP NAMES #####
#####################################################

# get isco 3 digit names to master file
df_green_minor$name_minor <- NA
df_green_minor$name_minor <- df_isco08$`Title EN`[match(df_green_minor$code_minor, df_isco08$`ISCO 08 Code`)]


# remove 
rm(df_isco08)





###############################################
### SECTION 5: GREEN OCCUPATIONS ##############
###############################################

###################
# All occupations #
###################

# K-means Clustering
set.seed(444) # for reproducibility
k <- 3 # or another number based on your preference or domain knowledge
km_res <- kmeans(df_green_minor$green_share, centers = k)

centers <- km_res$centers
sorted_centers <- sort(centers)

cut_offs <- c()
for (i in 1:(length(sorted_centers) - 1)) {
  cut_offs[i] <- (sorted_centers[i] + sorted_centers[i + 1]) / 2
}

rm(i,k,sorted_centers, centers, km_res)

print(cut_offs)
# [1] 0.06368323 0.26932899   ----> 0, 0-5%, 5-25%, 25%+


# categorize
df_green_minor$type_all <- NA
df_green_minor$type_all[df_green_minor$green_share == 0] <- "no-greenness"
df_green_minor$type_all[df_green_minor$green_share >0 & df_green_minor$green_share <=0.06368323] <- "low-greenness"
df_green_minor$type_all[df_green_minor$green_share >0.06368323 & df_green_minor$green_share <=0.26932899] <- "mid-greenness"
df_green_minor$type_all[df_green_minor$green_share >0.26932899] <- "high-greenness"



##################
# EX-agriculture #
##################

# K-means Clustering
set.seed(444) # for reproducibility
k <- 3 # or another number based on your preference or domain knowledge
km_res <- kmeans(df_green_minor$green_share[!(df_green_minor$code_minor %in% c("611","612", "613", "621", "622"))], centers = k)

centers <- km_res$centers
sorted_centers <- sort(centers)

cut_offs <- c()
for (i in 1:(length(sorted_centers) - 1)) {
  cut_offs[i] <- (sorted_centers[i] + sorted_centers[i + 1]) / 2
}

rm(i,k,sorted_centers, centers, km_res)

print(cut_offs)
# [1] 0.04301286 0.15260256   ----> 0, 0-5%, 5-15%, 15%+


# categorize
df_green_minor$type_exag <- NA
df_green_minor$type_exag[df_green_minor$green_share == 0] <- "no-greenness"
df_green_minor$type_exag[df_green_minor$green_share >0 & df_green_minor$green_share <=0.05] <- "low-greenness"
df_green_minor$type_exag[df_green_minor$green_share >0.05 & df_green_minor$green_share <=0.15] <- "mid-greenness"
df_green_minor$type_exag[df_green_minor$green_share >0.15] <- "high-greenness"


# compare exag vs all
df_green_minor$exag_vs_all <- df_green_minor$type_exag == df_green_minor$type_all
table(df_green_minor$type_all)
table(df_green_minor$type_exag)





################################
### SECTION 6: ROBUSTNESS ######
################################

# this is to check robustness --- looks at number of lowest level occupation units that have/dont have green skills
df_robust <- df_esco_skills_occup %>% group_by(code, code_minor, code_major, category) %>% summarise(number = n_distinct(skillUri))
df_robust <- pivot_wider(df_robust, names_from = category, values_from = number)


# if no green skills make = 0
df_robust$green[is.na(df_robust$green)] <- 0
length(unique(df_robust$code[df_robust$green == 0])) # ---> 1504 occupations with zero green skills


# get share of uccupation units by minor groups with zero green skills
df_robust <- df_robust %>%
  group_by(code_minor) %>%
  summarise(
    total = n(), 
    green_zero_count = sum(green == 0, na.rm = TRUE),
    share_green_zero = green_zero_count / total
  )


# add green skills share
df_robust$green_skills_share <- NA
df_robust$green_skills_share <- df_green_minor$green_share[match(df_robust$code_minor, df_green_minor$code_minor)]


# add the category
df_robust$category <- NA
df_robust$category <- df_green_minor$type_exag[match(df_robust$code_minor, df_green_minor$code_minor)]





####################
### SAVE ###########
####################

save.image("3 - env/1 - esco - green skill share.R.RData")

write.csv(df_green_minor, "2 - output/1 - green skills share - minor.csv")
write.csv(df_esco_skills_occup, "2 - output/1 - skills X occup.csv")
write.csv(df_robust, "2 - output/1 - robustness check.csv")


