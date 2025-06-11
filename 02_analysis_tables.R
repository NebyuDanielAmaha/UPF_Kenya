# 02_analysis_tables.R
# Purpose: Generate summary tables and models on UPF consumption and nutritional status
# Author: Nebyu D. Amaha
# Date: 2025-06-11

#Checking for missingness

# Check missing values in all 4 outcome variables at once
outcome_vars <- c("nt_ch_stunt", "nt_ch_wast", "nt_ch_underwt", "nt_ch_ovwt")
colSums(is.na(df[outcome_vars]))
sapply(df[outcome_vars], function(x) round(mean(is.na(x))*100, 1))

#UPF variables
upf_vars <- c("v414t", "v414r", "v413c", "v413d")
colSums(is.na(df[upf_vars]))
sapply(df[upf_vars], function(x) round(mean(is.na(x))*100, 1))



# Create the survey design object
svy_design <- svydesign(
  ids = ~v021,         # Cluster variable
  strata = ~v022,      # Stratification variable 
  weights = ~wt,       # Your weight variable
  data = df,           # Your dataframe
  nest = TRUE          # For DHS data
)

# Check the design
summary(svy_design)

#Checking for multicollinearity 
#sunting
model <- glm(
  model_formula_stunt,
  data = df,
  family = binomial()  # For binary outcome
)
vif(model)

#wast
model <- glm(
  model_formula_wast,
  data = df,
  family = binomial()  # For binary outcome
)
vif(model)

#underwt
model <- glm(
  model_formula_underwt,
  data = df,
  family = binomial()  # For binary outcome
)
vif(model)

#ovwt
model <- glm(
  model_formula_ovwt,
  data = df,
  family = binomial()  # For binary outcome
)
vif(model)

#TABLE 1
#Define variables
vars <- c("age", "sex", "child_agegrp", "mom_agegrp", "wealth", 
          "residence", "ph_wtr_improve", "ph_sani_improve",
          "nt_mdd", "nt_bf_curr", "fever", "diarrhea")

#Specify categorical variables
cat_vars <- c("sex", "child_agegrp", "mom_agegrp", "wealth", "residence",
              "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
              "fever", "diarrhea")

#Create Table 1
table1 <- CreateTableOne(
  vars = vars,
  factorVars = cat_vars,
  data = df,
  includeNA = FALSE
)
#print table 1
print(table1, 
      catDigits = 1,
      contDigits = 1,
      showAllLevels = TRUE,
      quote = FALSE)

#TABLE 2
#Ensure upf_consumed is factor
table2 <- CreateTableOne(
  vars = vars,
  factorVars = cat_vars,
  strata = "upf_consumed",
  data = df,
  includeNA = FALSE
)
#With chi-square
print(table2, 
      catDigits = 1,
      contDigits = 1,
      pDigits = 3,
      showAllLevels = TRUE,
      test = TRUE,        # Adds chi-square p-values
      smd = FALSE,         # Remove SMD if not needed
      format = "fp",       # Format as count (percentage)
      quote = FALSE)


#TABLE 3
detailed_analysis <- function(formula) {
  model <- svyglm(
    formula = formula,
    design = svy_design,
    family = quasibinomial()
  )
  
  # Get full model results
  model_summary <- summary(model)
  
  # Create and return detailed table (all in one pipeline)
  bind_cols(
    tibble(
      term = names(coef(model)),
      estimate = round(coef(model), 4),
      std_error = round(model_summary$coefficients[, "Std. Error"], 4),
      p_value = format.pval(model_summary$coefficients[, "Pr(>|t|)"], eps = 0.001)
    ),
    exp(cbind(
      OR = coef(model),
      confint(model)
    )) %>% 
      as.data.frame() %>% 
      rename(Lower_CI = `2.5 %`, Upper_CI = `97.5 %`)
  ) %>% 
    mutate(
      OR = round(OR, 2),
      Lower_CI = round(Lower_CI, 2),
      Upper_CI = round(Upper_CI, 2)
    )
}

detailed_results <- map_dfr(model_formulas, detailed_analysis, .id = "outcome")
print(detailed_results,n=92)

#TABLE 4

# Ensure survey design contains all variables
svy_design <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df,  # Make sure df contains all needed variables
  nest = TRUE
)

# Define common covariates
covariates <- c("child_agegrp", "sex", "birth", "mom_agegrp", "fever", "diarrhea",
                "working", "anc_cat2", "wealth", "residence", "hhnum",
                "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr")

# 3. Analyze all outcomes
outcomes <- c("nt_ch_stunt", "nt_ch_wast", "nt_ch_underwt", "nt_ch_ovwt")
results <- lapply(outcomes, function(outcome) {
  formula <- as.formula(paste(outcome, "~ upf_freq +", paste(covariates, collapse = "+")))
  
  model <- svyglm(formula, design = svy_design, family = quasibinomial())
  
  # Extract UPF frequency results
  coef <- summary(model)$coefficients["upf_freq", ]
  data.frame(
    Outcome = outcome,
    Term = "UPF Frequency (0-4)",
    OR = exp(coef["Estimate"]),
    Lower_CI = exp(coef["Estimate"] - 1.96 * coef["Std. Error"]),
    Upper_CI = exp(coef["Estimate"] + 1.96 * coef["Std. Error"]),
    p_value = coef["Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
})

# 4. Combine results
final_results <- do.call(rbind, results)
print(final_results)



#TABLE 5
upf_foods <- c("v413c", "v413d", "v414r", "v414t")
food_labels <- c("Chocholate-flavored drink", "Sodas/malt/energy drinks", "Chocolates/sweets/candies", "Chips/crisps/fries")


food_results <- lapply(1:4, function(i) {
  formula <- as.formula(paste("nt_ch_wast ~", upf_foods[i], "+", paste(covariates, collapse = "+")))
  
  model <- svyglm(
    formula = formula,
    design = svy_design,
    family = quasibinomial()
  )
  
  # Extract results for the food item
  coef_summary <- summary(model)$coefficients[upf_foods[i], ]
  ci <- confint(model)[upf_foods[i], ]
  
  data.frame(
    Food_item = food_labels[i],
    Variable = upf_foods[i],
    OR = exp(coef_summary["Estimate"]),
    Lower_CI = exp(ci[1]),
    Upper_CI = exp(ci[2]),
    p_value = coef_summary["Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
})

# 4. Combine and format results
food_analysis <- do.call(rbind, food_results)
food_analysis[, 3:6] <- round(food_analysis[, 3:6], 3)

# 5. Present clean results
cat("\nAssociation between specific UPF foods and wasting (nt_ch_wast):\n")
print(food_analysis, row.names = FALSE)


#Checking power of the study
# Combine rare categories (recommended)
 df$soda_consumption <- case_when(
   df$v413d == 1 ~ "Yes",
   df$v413d %in% c(0,8) ~ "No",  # Assuming 8=missing/no
   TRUE ~ NA_character_
 )

# Verify
 table(df$soda_consumption, useNA = "always") 
 df <- df %>% 
   mutate(soda = case_when(
     v413d == 1 ~ "Yes",
     v413d == 0 ~ "No",
     TRUE ~ NA_character_  # Treat 8 ("don't know") as missing
   ))

 # Check revised counts
 table(df$soda, df$nt_ch_wast, useNA = "always")
 
 fisher.test(table(df$soda, df$nt_ch_wast))


 # Subset design to exclude missing
 svy_sub <- subset(svy_design, v413d %in% 0:1)
 
 # Run model with warning
 if(sum(df$v413d==1 & df$nt_ch_wast==1, na.rm=TRUE) >= 5) {
   svyglm(nt_ch_wast ~ soda, design=svy_sub, family=quasibinomial()) %>% 
     summary()
 } else {
   message("Insufficient cases for reliable regression (only ", 
           sum(df$v413d==1 & df$nt_ch_wast==1), " exposed cases)")
 }


 #Graph the consumption of UPF with nutritional status of children 
 # Data frame for bar plot
 df <- data.frame(
   Age_Group = rep(c("6-11 months", "12-17 months", "18-23 months"), each = 2),
   UPF = rep(c("Yes", "No"), 3),
   Percentage = c(
     13.7, 86.3,  # 6-11 months: Yes, No
     27.1, 72.9,  # 12-17 months: Yes, No
     33.2, 66.8   # 18-23 months: Yes, No
   )
 )

# Ensure Age_Group is a factor with correct order
 df$Age_Group <- factor(df$Age_Group, levels = c("6-11 months", "12-17 months", "18-23 months"))
 
 # Create stacked bar plot
 ggplot(df, aes(x = Age_Group, y = Percentage, fill = UPF)) +
   geom_bar(stat = "identity", position = "stack") +
   geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
             position = position_stack(vjust = 0.5), size = 3.5, color = "white") +
   scale_fill_manual(values = c("Yes" = "#F44336", "No" = "#4CAF50"), 
                     labels = c("Yes", "No")) +
   theme_minimal() +
   labs(
     title = "UPF Consumption by Age Group in Kenyan Children (KDHS 2022)",
     subtitle = "Percentage of Children with Yes/No UPF Consumption (n = 2,824)",
     x = "Age Group (Months)",
     y = "Percentage (%)",
     fill = "UPF Consumption",
     caption = "Data from Table 2, p < 0.001 for age group differences"
   ) +
   theme(
     plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
     plot.subtitle = element_text(hjust = 0.5, size = 12),
     plot.caption = element_text(hjust = 0, size = 10),
     axis.text.x = element_text(size = 10),
     axis.text.y = element_text(size = 10),
     legend.position = "top"
   )
 ggsave("upf_by_age_bar_plot.png", width = 8, height = 6, dpi = 300)
 
