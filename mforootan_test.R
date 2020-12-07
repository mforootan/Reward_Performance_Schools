# startup settings
setwd("/Users/ghfmhf/Documents/DeptEdu/")
library("dplyr")
library("tidyverse")
library("tools")
library("readxl", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("gdata", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
options(stringsAsFaboaors=F)

# Reading the raw data
k8_base_raw <- read.csv('K8_base_file.csv')
k8_tvaas_raw <- read.csv('K8_TVAAS.csv')

# FROM THE INSTRUCTIONS:
  # Only subjects with 30 or more tests are included in a school’s success rate. 
  # Test count is verified at the year-school-subject-student group level.
  # Test records with missing grade are excluded from calculations. 
  # Since the “All Grades” records include missing grade, you should not use these records. 
  # Instead you should aggregate across grades after you reassign using the criteria outlined in step 1. 
k8_base_adj <- k8_base_raw %>% 
  filter(valid_tests >= 30) %>% 
  filter(grade != 'All Grades')

  # High school End of Course tests taken by students in grades less than 9 are reassigned in the following manner: 
  # Algebra I and II are reassigned to Math; English I, II, and III are reassigned to Reading; and Biology I is reassigned to Science.
k8_base_adj$subject_adj <- k8_base_adj$subject

for (i in 1:nrow(k8_base_adj)) {
  if (!(k8_base_adj[i,'grade'] %in% list('9','10','11'))) {
    k8_base_adj[i,'subject_adj'] <- case_when(
      k8_base_adj[i,'subject'] =='Algebra I' ~ 'Math',
      k8_base_adj[i,'subject'] =='Algebra II' ~ 'Math',
      k8_base_adj[i,'subject'] =='English I' ~ 'RLA',
      k8_base_adj[i,'subject'] =='English II' ~ 'RLA',
      k8_base_adj[i,'subject'] =='English III' ~ 'RLA',
      k8_base_adj[i,'subject'] =='Biology I' ~ 'Science'
    )
  } else {k8_base_adj$subject_adj <- k8_base_adj$subject}
}


# Required columns for calculating success - all years
k8_base_sum <- k8_base_adj %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 
 # Keep required values only
agg_success <- aggregate(k8_base_sum[,1:ncol(k8_base_sum)], by=list(k8_base_sum$school,k8_base_sum$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))
colnames(agg_success)[colnames(agg_success)=="Group.1"] <- "school"
colnames(agg_success)[colnames(agg_success)=="Group.2"] <- "system"

 # Success Rate formula
agg_success$success_rate <- (agg_success$n_adv + agg_success$n_prof) / agg_success$valid_tests

 # Calculate the z value for success rate, then flag those that z>1.281 (p(z=1.281)=0.9)
agg_success$norm <- (agg_success$success_rate - mean(agg_success$success_rate)) / sd(agg_success$success_rate)
agg_success$topten <- case_when(
  agg_success$norm > 1.281 ~ 'Yes',
  agg_success$norm <= 1.281 ~ 'No'
)

# Required columns for calculating success - 2014
k8_base_sum_14 <- k8_base_adj %>% 
  filter(year == '2014') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_14 <- aggregate(k8_base_sum_14[,1:ncol(k8_base_sum_14)], by=list(k8_base_sum_14$school,k8_base_sum_14$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))
colnames(agg_success_14)[colnames(agg_success_14)=="Group.1"] <- "school"
colnames(agg_success_14)[colnames(agg_success_14)=="Group.2"] <- "system"
agg_success_14$success_rate_14 <- (agg_success_14$n_adv + agg_success_14$n_prof) / agg_success_14$valid_tests
agg_success_14$norm_14 <- (agg_success_14$success_rate_14 - mean(agg_success_14$success_rate_14)) / sd(agg_success_14$success_rate_14)
agg_success_14$topten_14 <- case_when(
  agg_success_14$norm_14 > 1.281 ~ 'Yes',
  agg_success_14$norm_14 <= 1.281 ~ 'No'
)

# Required columns for calculating success - 2015
k8_base_sum_15 <- k8_base_adj %>% 
  filter(year == '2015') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_15 <- aggregate(k8_base_sum_15[,1:ncol(k8_base_sum_15)], by=list(k8_base_sum_15$school,k8_base_sum_15$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))
colnames(agg_success_15)[colnames(agg_success_15)=="Group.1"] <- "school"
colnames(agg_success_15)[colnames(agg_success_15)=="Group.2"] <- "system"
agg_success_15$success_rate_15 <- (agg_success_15$n_adv + agg_success_15$n_prof) / agg_success_15$valid_tests
agg_success_15$norm_15 <- (agg_success_15$success_rate_15 - mean(agg_success_15$success_rate_15)) / sd(agg_success_15$success_rate_15)

agg_success_15$topten_15 <- case_when(
  agg_success_15$norm_15 > 1.281 ~ 'Yes',
  agg_success_15$norm_15 <= 1.281 ~ 'No'
)


# Flag schools with large student group gaps (based on total years)
# A school is exempt from reward status if it meets both of the following conditions for any of its student group gaps:
#  •	Its three-year gap is larger than the median gap among schools with sufficient data to calculate a three-year gap.


# Required columns for calculating success - Black/Hispanic/Native American
k8_base_sum_BHN <- k8_base_adj %>% 
  filter(subgroup == 'Black/Hispanic/Native American') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_BHN <- aggregate(k8_base_sum_BHN[,1:ncol(k8_base_sum_BHN)], by=list(k8_base_sum_BHN$school,k8_base_sum_BHN$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_BHN)[colnames(agg_success_BHN)=="Group.1"] <- "school"
colnames(agg_success_BHN)[colnames(agg_success_BHN)=="Group.2"] <- "system"
agg_success_BHN$success_rate_BHN <- (agg_success_BHN$n_adv + agg_success_BHN$n_prof) / agg_success_BHN$valid_tests
agg_success_BHN$norm_BHN <- (agg_success_BHN$success_rate_BHN - mean(agg_success_BHN$success_rate_BHN)) / sd(agg_success_BHN$success_rate_BHN)

# Merge Black/Hispanic/Native American with All Students to compare
BHNvsALL <- merge(agg_success,agg_success_BHN, by= c('school','system'), all = TRUE) %>% 
  select(c('system','school','success_rate','success_rate_BHN'))

BHNvsALL$exempt_BHN <- case_when(
  BHNvsALL$success_rate_BHN > BHNvsALL$success_rate ~ 'Yes', 
  BHNvsALL$success_rate_BHN <= BHNvsALL$success_rate ~ 'No' 
  )


# Required columns for calculating success - Economically Disadvantaged
k8_base_sum_ECD <- k8_base_adj %>% 
  filter(subgroup == 'Economically Disadvantaged') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_ECD <- aggregate(k8_base_sum_ECD[,1:ncol(k8_base_sum_ECD)], by=list(k8_base_sum_ECD$school,k8_base_sum_ECD$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_ECD)[colnames(agg_success_ECD)=="Group.1"] <- "school"
colnames(agg_success_ECD)[colnames(agg_success_ECD)=="Group.2"] <- "system"
agg_success_ECD$success_rate_ECD <- (agg_success_ECD$n_adv + agg_success_ECD$n_prof) / agg_success_ECD$valid_tests

# Required columns for calculating success - Non-Economically Disadvantaged
k8_base_sum_nECD <- k8_base_adj %>% 
  filter(subgroup == 'Non-Economically Disadvantaged') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_nECD <- aggregate(k8_base_sum_nECD[,1:ncol(k8_base_sum_nECD)], by=list(k8_base_sum_nECD$school,k8_base_sum_nECD$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_nECD)[colnames(agg_success_nECD)=="Group.1"] <- "school"
colnames(agg_success_nECD)[colnames(agg_success_nECD)=="Group.2"] <- "system"
agg_success_nECD$success_rate_nECD <- (agg_success_nECD$n_adv + agg_success_nECD$n_prof) / agg_success_nECD$valid_tests

# Merge Disadvantaged groups to compare
ECDvsnECD <- merge(agg_success_nECD,agg_success_ECD, by= c('school','system'), all = TRUE) %>% 
  select(c('system','school','success_rate_nECD','success_rate_ECD'))

ECDvsnECD$exempt_ECD <- case_when(
  ECDvsnECD$success_rate_ECD > ECDvsnECD$success_rate_nECD ~ 'Yes', 
  ECDvsnECD$success_rate_ECD <= ECDvsnECD$success_rate_nECD ~ 'No' 
)

# Required columns for calculating success - Students with Disabilities
k8_base_sum_DIS <- k8_base_adj %>% 
  filter(subgroup == 'Students with Disabilities') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_DIS <- aggregate(k8_base_sum_DIS[,1:ncol(k8_base_sum_DIS)], by=list(k8_base_sum_DIS$school,k8_base_sum_DIS$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_DIS)[colnames(agg_success_DIS)=="Group.1"] <- "school"
colnames(agg_success_DIS)[colnames(agg_success_DIS)=="Group.2"] <- "system"
agg_success_DIS$success_rate_DIS <- (agg_success_DIS$n_adv + agg_success_DIS$n_prof) / agg_success_DIS$valid_tests

# Required columns for calculating success - Non-Students with Disabilities
k8_base_sum_nDIS <- k8_base_adj %>% 
  filter(subgroup == 'Non-Students with Disabilities') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_nDIS <- aggregate(k8_base_sum_nDIS[,1:ncol(k8_base_sum_nDIS)], by=list(k8_base_sum_nDIS$school,k8_base_sum_nDIS$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_nDIS)[colnames(agg_success_nDIS)=="Group.1"] <- "school"
colnames(agg_success_nDIS)[colnames(agg_success_nDIS)=="Group.2"] <- "system"
agg_success_nDIS$success_rate_nDIS <- (agg_success_nDIS$n_adv + agg_success_nDIS$n_prof) / agg_success_nDIS$valid_tests

# Merge Disability groups to compare
DISvsnDIS <- merge(agg_success_nDIS,agg_success_DIS, by= c('school','system'), all = TRUE) %>% 
  select(c('system','school','success_rate_nDIS','success_rate_DIS'))

DISvsnDIS$exempt_DIS <- case_when(
  DISvsnDIS$success_rate_DIS > DISvsnDIS$success_rate_nDIS ~ 'Yes', 
  DISvsnDIS$success_rate_DIS <= DISvsnDIS$success_rate_nDIS ~ 'No' 
)

# Required columns for calculating success - English Language Learners with T1/T2
k8_base_sum_ENG <- k8_base_adj %>% 
  filter(subgroup == 'English Language Learners with T1/T2') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_ENG <- aggregate(k8_base_sum_ENG[,1:ncol(k8_base_sum_ENG)], by=list(k8_base_sum_ENG$school,k8_base_sum_ENG$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_ENG)[colnames(agg_success_ENG)=="Group.1"] <- "school"
colnames(agg_success_ENG)[colnames(agg_success_ENG)=="Group.2"] <- "system"
agg_success_ENG$success_rate_ENG <- (agg_success_ENG$n_adv + agg_success_ENG$n_prof) / agg_success_ENG$valid_tests

# Required columns for calculating success - Non-English Language Learners with T1/T2
k8_base_sum_nENG <- k8_base_adj %>% 
  filter(subgroup == 'Non-English Language Learners/T1 or T2') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_nENG <- aggregate(k8_base_sum_nENG[,1:ncol(k8_base_sum_nENG)], by=list(k8_base_sum_nENG$school,k8_base_sum_nENG$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_nENG)[colnames(agg_success_nENG)=="Group.1"] <- "school"
colnames(agg_success_nENG)[colnames(agg_success_nENG)=="Group.2"] <- "system"
agg_success_nENG$success_rate_nENG <- (agg_success_nENG$n_adv + agg_success_nENG$n_prof) / agg_success_nENG$valid_tests

# Merge English learners groups to compare
ENGvsnENG <- merge(agg_success_nENG,agg_success_ENG, by= c('school','system'), all = TRUE) %>% 
  select(c('system','school','success_rate_nENG','success_rate_ENG'))

ENGvsnENG$exempt_ENG <- case_when(
  ENGvsnENG$success_rate_ENG > ENGvsnENG$success_rate_nENG ~ 'Yes', 
  ENGvsnENG$success_rate_ENG <= ENGvsnENG$success_rate_nENG ~ 'No' 
) 

# Flag schools with large student group gaps (based on current and last year)
# A school is exempt from reward status if it meets both of the following conditions for any of its student group gaps:
#  •	Its current year gap has widened (is larger) compared to its prior year gap.

# Required columns for calculating success - Black/Hispanic/Native American - 2015
k8_base_sum_BHN_15 <- k8_base_adj %>% 
  filter(subgroup == 'Black/Hispanic/Native American') %>% 
  filter(year == '2015') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_BHN_15 <- aggregate(k8_base_sum_BHN_15[,1:ncol(k8_base_sum_BHN_15)], by=list(k8_base_sum_BHN_15$school,k8_base_sum_BHN_15$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_BHN_15)[colnames(agg_success_BHN_15)=="Group.1"] <- "school"
colnames(agg_success_BHN_15)[colnames(agg_success_BHN_15)=="Group.2"] <- "system"
agg_success_BHN_15$success_rate_BHN_15 <- (agg_success_BHN_15$n_adv + agg_success_BHN_15$n_prof) / agg_success_BHN_15$valid_tests
agg_success_BHN_15$norm_BHN_15 <- (agg_success_BHN_15$success_rate_BHN_15 - mean(agg_success_BHN_15$success_rate_BHN_15)) / sd(agg_success_BHN_15$success_rate_BHN_15)


# Required columns for calculating success - Black/Hispanic/Native American - 2014 and 2015
k8_base_sum_BHN_14 <- k8_base_adj %>% 
  filter(subgroup == 'Black/Hispanic/Native American') %>% 
  filter(year == '2014') %>% 
select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_BHN_14 <- aggregate(k8_base_sum_BHN_14[,1:ncol(k8_base_sum_BHN_14)], by=list(k8_base_sum_BHN_14$school,k8_base_sum_BHN_14$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_BHN_14)[colnames(agg_success_BHN_14)=="Group.1"] <- "school"
colnames(agg_success_BHN_14)[colnames(agg_success_BHN_14)=="Group.2"] <- "system"
agg_success_BHN_14$success_rate_BHN_14 <- (agg_success_BHN_14$n_adv + agg_success_BHN_14$n_prof) / agg_success_BHN_14$valid_tests
agg_success_BHN_14$norm_BHN_14 <- (agg_success_BHN_14$success_rate_BHN_14 - mean(agg_success_BHN_14$success_rate_BHN_14)) / sd(agg_success_BHN_14$success_rate_BHN_14)


BHN14vsBHN15 <- merge(agg_success_BHN_14,agg_success_BHN_15, by= c('school','system'), all = TRUE) %>% 
  select(c('system','school','success_rate_BHN_14','success_rate_BHN_15'))

BHN14vsBHN15$exempt_BHN <- case_when(
  BHN14vsBHN15$success_rate_BHN_15 > BHN14vsBHN15$success_rate_BHN_14 ~ 'Yes', 
  BHN14vsBHN15$success_rate_BHN_15 <= BHN14vsBHN15$success_rate_BHN_14 ~ 'No' 
)

# Required columns for calculating success - Economically Disadvantaged 2014 and 2015
k8_base_sum_ECD_14 <- k8_base_adj %>% 
  filter(subgroup == 'Economically Disadvantaged') %>% 
  filter(year == '2014') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_ECD_14 <- aggregate(k8_base_sum_ECD_14[,1:ncol(k8_base_sum_ECD_14)], by=list(k8_base_sum_ECD_14$school,k8_base_sum_ECD_14$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_ECD_14)[colnames(agg_success_ECD_14)=="Group.1"] <- "school"
colnames(agg_success_ECD_14)[colnames(agg_success_ECD_14)=="Group.2"] <- "system"
agg_success_ECD_14$success_rate_ECD_14 <- (agg_success_ECD_14$n_adv + agg_success_ECD_14$n_prof) / agg_success_ECD_14$valid_tests

k8_base_sum_ECD_15 <- k8_base_adj %>% 
  filter(subgroup == 'Economically Disadvantaged') %>% 
  filter(year == '2015') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_ECD_15 <- aggregate(k8_base_sum_ECD_15[,1:ncol(k8_base_sum_ECD_15)], by=list(k8_base_sum_ECD_15$school,k8_base_sum_ECD_15$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_ECD_15)[colnames(agg_success_ECD_15)=="Group.1"] <- "school"
colnames(agg_success_ECD_15)[colnames(agg_success_ECD_15)=="Group.2"] <- "system"
agg_success_ECD_15$success_rate_ECD_15 <- (agg_success_ECD_15$n_adv + agg_success_ECD_15$n_prof) / agg_success_ECD_15$valid_tests

# Merge Economically Disadvantaged American for 2014 and 2015 to compare
ECD14vsECD15 <- merge(agg_success_ECD_14,agg_success_ECD_15, by= c('school','system'), all = TRUE) %>% 
  select(c('system','school','success_rate_ECD_14','success_rate_ECD_15'))

ECD14vsECD15$exempt_ECD <- case_when(
  ECD14vsECD15$success_rate_ECD_15 > ECD14vsECD15$success_rate_ECD_14 ~ 'Yes', 
  ECD14vsECD15$success_rate_ECD_15 <= ECD14vsECD15$success_rate_ECD_14 ~ 'No' 
)

# Required columns for calculating success - Students with Disabilities 2014 and 2015
k8_base_sum_DIS_14 <- k8_base_adj %>% 
  filter(subgroup == 'Students with Disabilities') %>%
  filter(year == '2014') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_DIS_14 <- aggregate(k8_base_sum_DIS_14[,1:ncol(k8_base_sum_DIS_14)], by=list(k8_base_sum_DIS_14$school,k8_base_sum_DIS_14$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_DIS_14)[colnames(agg_success_DIS_14)=="Group.1"] <- "school"
colnames(agg_success_DIS_14)[colnames(agg_success_DIS_14)=="Group.2"] <- "system"
agg_success_DIS_14$success_rate_DIS_14 <- (agg_success_DIS_14$n_adv + agg_success_DIS_14$n_prof) / agg_success_DIS_14$valid_tests

k8_base_sum_DIS_15 <- k8_base_adj %>% 
  filter(subgroup == 'Students with Disabilities') %>%
  filter(year == '2015') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_DIS_15 <- aggregate(k8_base_sum_DIS_15[,1:ncol(k8_base_sum_DIS_15)], by=list(k8_base_sum_DIS_15$school,k8_base_sum_DIS_15$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_DIS_15)[colnames(agg_success_DIS_15)=="Group.1"] <- "school"
colnames(agg_success_DIS_15)[colnames(agg_success_DIS_15)=="Group.2"] <- "system"
agg_success_DIS_15$success_rate_DIS_15 <- (agg_success_DIS_15$n_adv + agg_success_DIS_15$n_prof) / agg_success_DIS_15$valid_tests

# Merge Disability for 2014 and 2015 to compare
DIS14vsDIS15 <- merge(agg_success_DIS_14,agg_success_DIS_15, by= c('school','system'), all = TRUE) %>% 
  select(c('system','school','success_rate_DIS_14','success_rate_DIS_15'))

DIS14vsDIS15$exempt_DIS <- case_when(
  DIS14vsDIS15$success_rate_DIS_15 > DIS14vsDIS15$success_rate_DIS_14 ~ 'Yes', 
  DIS14vsDIS15$success_rate_DIS_15 <= DIS14vsDIS15$success_rate_DIS_14 ~ 'No' 
)

# Required columns for calculating success - English Language Learners with T1/T2 2014 and 2015
k8_base_sum_ENG_14 <- k8_base_adj %>% 
  filter(subgroup == 'English Language Learners with T1/T2') %>% 
  filter(year == '2014') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_ENG_14 <- aggregate(k8_base_sum_ENG_14[,1:ncol(k8_base_sum_ENG_14)], by=list(k8_base_sum_ENG_14$school,k8_base_sum_ENG_14$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_ENG_14)[colnames(agg_success_ENG_14)=="Group.1"] <- "school"
colnames(agg_success_ENG_14)[colnames(agg_success_ENG_14)=="Group.2"] <- "system"
agg_success_ENG_14$success_rate_ENG_14 <- (agg_success_ENG_14$n_adv + agg_success_ENG_14$n_prof) / agg_success_ENG_14$valid_tests

k8_base_sum_ENG_15 <- k8_base_adj %>% 
  filter(subgroup == 'English Language Learners with T1/T2') %>% 
  filter(year == '2015') %>% 
  select(c('system','school','valid_tests','n_prof','n_adv')) 

agg_success_ENG_15 <- aggregate(k8_base_sum_ENG_15[,1:ncol(k8_base_sum_ENG_15)], by=list(k8_base_sum_ENG_15$school,k8_base_sum_ENG_15$system), FUN=sum) %>% 
  select(c('Group.1','Group.2','valid_tests','n_adv','n_prof'))

colnames(agg_success_ENG_15)[colnames(agg_success_ENG_15)=="Group.1"] <- "school"
colnames(agg_success_ENG_15)[colnames(agg_success_ENG_15)=="Group.2"] <- "system"
agg_success_ENG_15$success_rate_ENG_15 <- (agg_success_ENG_15$n_adv + agg_success_ENG_15$n_prof) / agg_success_ENG_15$valid_tests

# Merge English Learner for 2014 and 2015 to compare
ENG14vsENG15 <- merge(agg_success_ENG_14,agg_success_ENG_15, by= c('school','system'), all = TRUE) %>% 
  select(c('system','school','success_rate_ENG_14','success_rate_ENG_15'))

ENG14vsENG15$exempt_ENG <- case_when(
  ENG14vsENG15$success_rate_ENG_15 > ENG14vsENG15$success_rate_ENG_14 ~ 'Yes', 
  ENG14vsENG15$success_rate_ENG_15 <= ENG14vsENG15$success_rate_ENG_14 ~ 'No' 
)

# Merge all the exempt tables

m1 <- merge(BHNvsALL,BHN14vsBHN15,by= c('school','system'), all = TRUE) %>% 
  select(c('school','system','exempt_BHN.x','exempt_BHN.y'))
m2 <- merge(m1,DIS14vsDIS15,by= c('school','system'), all = TRUE) %>% 
  select(c('school','system','exempt_BHN.x','exempt_BHN.y','exempt_DIS'))
m3 <- merge(m2,DISvsnDIS,by= c('school','system'), all = TRUE) %>% 
  select(c('school','system','exempt_BHN.x','exempt_BHN.y','exempt_DIS.x','exempt_DIS.y'))
m4 <- merge(m3,ECD14vsECD15,by= c('school','system'), all = TRUE) %>% 
  select(c('school','system','exempt_BHN.x','exempt_BHN.y','exempt_DIS.x','exempt_DIS.y','exempt_ECD'))
m5 <- merge(m4,ECDvsnECD,by= c('school','system'), all = TRUE) %>% 
  select(c('school','system','exempt_BHN.x','exempt_BHN.y','exempt_DIS.x','exempt_DIS.y','exempt_ECD.x','exempt_ECD.y'))
m6 <- merge(m5,ENG14vsENG15,by= c('school','system'), all = TRUE) %>% 
  select(c('school','system','exempt_BHN.x','exempt_BHN.y','exempt_DIS.x','exempt_DIS.y','exempt_ECD.x','exempt_ECD.y','exempt_ENG'))
m7 <- merge(m6,ENGvsnENG,by= c('school','system'), all = TRUE) %>% 
  select(c('school','system','exempt_BHN.x','exempt_BHN.y','exempt_DIS.x','exempt_DIS.y','exempt_ECD.x','exempt_ECD.y','exempt_ENG.x','exempt_ENG.y'))

# One exepmt will disqualify the school for reward status
m7$reward_exempt <- ''
m7[is.na(m7)] <- 0
for (i in 1:nrow(m7)){
  for (j in 3:ncol(m7)-1) {
    if (m7[i,j] == 'Yes'){m7[i,ncol(m7)] <- 'No'}
  }
}
m7 <- m7 %>% 
  select(c('school','system','reward_exempt'))

m7$reward_exempt[m7$reward_exempt !='No'] <- 'Yes'


# Next, identify 5 percent of non-exempt schools with the largest TVAAS index values as Reward Progress. 
# If there is overlap between Reward Performance and Reward Progress schools, you will need to further identify 
# Reward Progress schools such that there are 5 percent of schools identified uniquely for Reward Progress.


k8_tvaas_raw$reward_progress <- ''
k8_tvaas_raw$reward_progress[k8_tvaas_raw$Index > 1.644] <- 'Yes' # z(p=.95)
k8_tvaas_raw$reward_progress[k8_tvaas_raw$reward_progress !='Yes'] <- 'No'


# All the three results are merged together here.


final_merge1 <- merge(k8_tvaas_raw,m7,by= c('school','system'), all = TRUE) %>% 
  select(-c('Index'))



final_merge <- merge(final_merge1,agg_success,by= c('school','system'), all = TRUE) %>% 
  select(c('school','system','reward_progress','reward_exempt','topten'))
colnames(final_merge)[colnames(final_merge)=="topten"] <- "reward_performance"
save(final_merge, file = "mforootan.Rda")
