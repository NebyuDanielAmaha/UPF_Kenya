# 01_recode.R
# Purpose: Prepare and recode variables from 2022 Kenya DHS for children aged 6–23 months
# Author: Nebyu D. Amaha
# Date: 2025-06-11

library(haven)
library(here)
library(dplyr)
library(labelled)
library(naniar)
library(survey)
library(regclass)
library(car)
library(purrr)
library(tableone)
library(epiDisplay)
library(epitools)
library(ggplot2)

#import the data
df <- read_dta("KEKR8CFL.DTA")

#create the variable age
df <- df %>% mutate(age = b19)

#filter by age, living at home and the youngest child
df <- df %>%
  subset(age >=6 & age < 24 & b9==0) %>% # children under 24 months living at home
  arrange(caseid, bidx) %>% # make sure the df is sorted
  subset(is.na(lag(caseid)) | caseid!=lag(caseid)) # select just the youngest

#create survey weight
df <- df %>% mutate(wt = v005/1000000)

#Outcome variables
#Stunting
df <- df %>%
  mutate(nt_ch_stunt =
           case_when(
             hw70< -200  ~ 1 ,
             hw70>= -200 & hw70 <9996 ~ 0 ,
             hw70>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_stunt = c(99))) %>%
  set_value_labels(nt_ch_stunt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_stunt = "Stunted child under 5 years")

#Wasting
df <- df %>%
  mutate(nt_ch_wast =
           case_when(
             hw72< -200  ~ 1 ,
             hw72>= -200 & hw72 <9996 ~ 0 ,
             hw72>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_wast = c(99))) %>%
  set_value_labels(nt_ch_wast = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_wast = "Wasted child under 5 years")

#Underweight
df <- df %>%
  mutate(nt_ch_underwt =
           case_when(
             hw71< -200  ~ 1 ,
             hw71>= -200 & hw71 <9996 ~ 0 ,
             hw71>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_underwt = c(99))) %>%
  set_value_labels(nt_ch_underwt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_stunt = "Underweight child under 5 years")

#Overweight for age
df <- df %>%
  mutate(nt_ch_ovwt =
           case_when(
             hw71> 200 & hw71 <9996  ~ 1 ,
             hw71<= 200 & hw71 <9996 ~ 0 ,
             hw71>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_ovwt = c(99))) %>%
  set_value_labels(nt_ch_ovwt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_ovwt = "Underweight child under 5 years")


#*******Covariates****#
#Co-variates

df <- df %>% 
  # Child age group
  mutate(child_agegrp = factor(case_when(
    age >= 6 & age <= 11 ~ 1,
    age >= 12 & age <= 17 ~ 2,
    age >= 18 & age <= 23 ~ 3
  ),
  levels = c(1, 2, 3),
  labels = c("6–11 months", "12–17 months", "18–23 months"))) %>% 
  
  # Fever (binary)
  mutate(sex = factor(
    b4,
    levels = c(1, 2),
    labels = c("Male", "Female")
  )) %>% 
  
  # Mother's age group
  mutate(mom_agegrp = factor(case_when(
    v012 >= 15 & v012 <= 24 ~ 1,
    v012 >= 25 & v012 <= 34 ~ 2,
    v012 >= 35 & v012 <= 49 ~ 3
  ),
  levels = c(1, 2, 3),
  labels = c("15–24 years", "25–34 years", "35–49 years"))) %>%
  
  # Fever (binary)
  mutate(fever = factor(case_when(
    h22 == 1 ~ 1,
    h22 == 0 ~ 0
  ),
  levels = c(0, 1),
  labels = c("No", "Yes"))) %>%
  
  # Diarrhea (binary)
  mutate(diarrhea = factor(case_when(
    h11 == 2 ~ 1,
    h11 == 0 ~ 0
  ),
  levels = c(0, 1),
  labels = c("No", "Yes"))) %>%
  
  # Birth order binary
  mutate(birth = factor(case_when(
    bord <= 3 ~ 1,
    bord > 3 ~ 2
  ),
  levels = c(1, 2),
  labels = c("1st–3rd order", "4th or later"))) %>%
  
  # Working status (already categorical in v714)
  mutate(working = factor(v714,
                          levels = c(0, 1),
                          labels = c("Not working", "Working"))) %>%
  
  # ANC visits
  mutate(anc_cat2 = factor(case_when(
    !is.na(m14) & m14 < 4 ~ 0,
    m14 >= 4 ~ 1
  ),
  levels = c(0, 1),
  labels = c("<4 visits", "≥4 visits"))) %>%
  
  # Wealth
  mutate(wealth = factor(v190,
                         levels = 1:5,
                         labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))) %>%
  
  # Residence
  mutate(residence = factor(v025,
                            levels = c(1, 2),
                            labels = c("Urban", "Rural"))) %>%
  
  # Household size
  mutate(hhnum = factor(case_when(
    v136 < 6 ~ 1,
    v136 >= 6 ~ 2
  ),
  levels = c(1, 2),
  labels = c("Less than 6", "6 or more"))) %>%
  
  # Improved water source
  mutate(ph_wtr_improve = factor(case_when(
    v113 %in% c(11:15, 21, 31, 41, 51, 61:73) ~ 1,
    v113 %in% c(30, 32, 40, 42, 43, 96) ~ 0,
    v113 == 99 ~ 99
  ),
  levels = c(0, 1, 99),
  labels = c("Unimproved/surface water", "Improved water", "Missing")))

# Improved sanitation
df <- df %>% 
  mutate(
    ph_sani_improve_code = case_when(
      v116 %in% c(11:13, 15, 21, 22, 41, 51) ~ 1,
      v116 %in% c(14, 23, 24, 42, 43, 96) ~ 2,
      v116 == 31 ~ 3,
      v116 == 99 ~ NA_real_,
      TRUE ~ NA_real_  # Handles any unexpected values
    ),
    ph_sani_improve = factor(
      ph_sani_improve_code,
      levels = 1:3,
      labels = c("Improved sanitation", "Unimproved sanitation", "Open defecation"),
      exclude = NULL  # Keeps NA values as NA
    )
  )

#Currently breastfeeding status
#Dietary Diversity
#are below

# *** Breastfeeding and complemenatry feeding ***
# 
# //currently breastfed
df <- df %>%
  mutate(
    nt_bf_curr = factor(
      case_when(
        m4 == 95 ~ 1,
        m4 %in% c(93, 94, 98, 99) ~ 0,
        TRUE ~ NA_real_
      ),
    )
  ) %>%
  set_variable_labels(nt_bf_curr = "Currently breastfeeding - last-born under 2 years")
# *** Foods consumed ***
df <- df %>%
  mutate(food1  = case_when(v414a==1  ~ 1 , v414a!=1 ~ 0)) %>%
  mutate(food2  = case_when(v414b==1  ~ 1 , v414a!=1 ~ 0)) %>%
  mutate(food3  = case_when(v414c==1  ~ 1 , v414a!=1 ~ 0)) %>%
  mutate(food4  = case_when(v414d==1  ~ 1 , v414a!=1 ~ 0)) %>%
  mutate(nt_formula  = case_when(v411a==1  ~ 1 , v411a!=1~ 0)) %>% # Given formula
  mutate(nt_milk  = case_when(v411==1  ~ 1 , v411!=1~ 0)) %>% # Given other milk
  mutate(nt_liquids= case_when(v410==1 | v412c==1 | v413==1  ~ 1 , v410!=1 | v412c!=1 | v413!=1  ~ 0)) %>% # Given other liquids
  mutate(nt_bbyfood  = case_when(v412a==1  ~ 1 , v412a!=1~ 0)) %>% # Given fortified baby food
  mutate(nt_grains  = case_when(v412a==1 | v414e==1 ~ 1 , v412a!=1 | v414e!=1 ~ 0)) %>% # Given grains
  mutate(nt_vita = case_when(v414i==1 | v414j==1 | v414k==1 ~ 1 , v414i!=1 | v414j!=1 | v414k!=1 ~ 0)) %>% # Given Vit A rich foods
  mutate(nt_frtveg  = case_when(v414l==1  ~ 1 , v414l!=1~ 0)) %>% # Given other fruits or vegetables
  mutate(nt_root  = case_when(   # Given roots and tubers  
    (v000 == "KE8" & (v414f==1 | food1==1)) | (v000 != "KE8" & v414f==1) ~ 1, 
    (v000 == "KE8" & (v414f!=1 | food1!=1)) | (v000 != "KE8" & v414f!=1) ~ 0)) %>%
  mutate(nt_nuts  = case_when(v414o==1  ~ 1 , v414o!=1~ 0)) %>% # Given nuts or legumes
  mutate(nt_meatfish  = case_when(   # Given meat, fish, shellfish, or poultry  
    (v000 == "KE8" & (v414h==1 |v414m==1 |v414n==1| food2==1)) | (v000 != "KE8" & (v414h==1 | v414m==1 | v414n==1)) ~ 1, 
    (v000 == "KE8" & !(v414h==1 |v414m==1 |v414n==1| food2==1)) | (v000 != "KE8" & !(v414h==1 | v414m==1 | v414n==1)) ~ 0)) %>%
  mutate(nt_eggs  = case_when(v414g==1  ~ 1 , v414g!=1~ 0)) %>% # Given eggs
  mutate(nt_dairy  = case_when(v414p==1 | v414v==1 ~ 1 , v414p!=1 | v414v!=1 ~ 0)) %>% # Given dairy
  mutate(nt_solids = case_when( nt_bbyfood==1 | nt_grains==1 | nt_vita==1 | nt_frtveg==1 | nt_root==1 | nt_nuts==1 | nt_meatfish==1 | 
                                  nt_eggs==1 | nt_dairy==1 | v414s==1 ~ 1 ,
                                nt_bbyfood!=1 | nt_grains!=1 | nt_vita!=1 | nt_frtveg!=1 | nt_root!=1 | nt_nuts!=1 | nt_meatfish!=1 | 
                                  nt_eggs!=1 | nt_dairy!=1 | v414s!=1 ~ 0) )
# //Min dietary diversity
df <- df %>%
  # 1. breastmilk
  mutate(group1 = case_when(m4==95  ~ 1 ,  m4!=95 ~ 0)) %>% 
  #2. infant formula, milk other than breast milk, cheese or yogurt or other milk products
  mutate(group2 = case_when(nt_formula==1 | nt_milk==1 | nt_dairy==1  ~ 1 , nt_formula!=1 | nt_milk!=1 | nt_dairy!=1 ~ 0)) %>%
  #3. foods made from grains, roots, tubers, and bananas/plantains, including porridge and fortified baby food from grains
  mutate(group3  = case_when(nt_grains==1 | nt_root==1 | nt_bbyfood==1 ~ 1 , nt_grains!=1 | nt_root!=1 | nt_bbyfood!=1 ~ 0)) %>%
  #4. vitamin A-rich fruits and vegetables
  mutate(group4  = case_when(nt_vita==1  ~ 1 , nt_vita!=1 ~ 0)) %>%
  #5. other fruits and vegetables
  mutate(group5  = case_when(nt_frtveg==1 ~ 1 , nt_frtveg!=1~ 0)) %>% 
  #6. eggs
  mutate(group6  = case_when(nt_eggs==1 ~ 1 , nt_eggs!=1~ 0)) %>% 
  #7. meat, poultry, fish, and shellfish (and organ meats)
  mutate(group7  = case_when(nt_meatfish==1 ~ 1 , nt_meatfish!=1~ 0)) %>% 
  #8. legumes and nuts
  mutate(group8  = case_when(nt_nuts==1 ~ 1 , nt_nuts!=1~ 0)) %>% 
  #add the food groups
  mutate(foodsum  = group1+group2+group3+group4+group5+group6+group7+group8) %>% 
  
  # Create minimum dietary diversity variable
  mutate(
    nt_mdd = factor(
      case_when(
        between(age, 6, 23) & foodsum < 5 ~ 0,
        between(age, 6, 23) & foodsum >= 5 ~ 1,
        TRUE ~ NA_real_
      ),
    )
  ) %>% 
  set_variable_labels(nt_mdd = "Child with minimum dietary diversity, 5 out of 8 food groups- last-born 6-23 months")

# df$nt_mdd = as.factor(df$nt_mdd)
# df$nt_bf_curr = as.factor(df$nt_bf_curr)

#******Exposure variable*******#

df <- df %>%
  # Drop rows where all 4 UPF variables are NA
  filter(!(is.na(v414t) & is.na(v414r) & is.na(v413c) & is.na(v413d))) %>%
  
  # Create binary UPF exposure variable
  mutate(upf_consumed = factor(if_else(
    v414t == 1 | v414r == 1 | v413c == 1 | v413d == 1,
    1, 0),
    levels = c(0, 1),
    labels = c("No UPF", "Consumed UPF"))) %>%
  
  # Create frequency-based UPF exposure variable
  mutate(upf_freq = rowSums(across(c(v414t, v414r, v413c, v413d), ~ .x == 1), na.rm = TRUE))

#alternate definition of upf_frequency upf_frq3

df <- df %>%
  mutate(upf_freq3 = case_when(
    upf_freq == 0 ~ "none",
    upf_freq == 1 ~ "low",
    upf_freq == 2 ~ "mid",
    upf_freq >= 3 ~ "high", # This covers 3 and 4 (and any higher values if they appeared)
    TRUE ~ NA_character_ # Catches any other unexpected values, assigning NA
  ))
