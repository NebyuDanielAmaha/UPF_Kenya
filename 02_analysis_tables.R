#*TABLES

# List the key covariates you want to keep complete (excluding nt_mdd and bmi_cat)
complete_vars <- c(
  "upf_consumed",
  "child_agegrp",
  "sex",
  "birth",
  "mom_agegrp",
  "mom_edu",
  "working",
  "anc_cat2",
  "bmi_cat",
  "wealth",
  "residence",
  "hhnum",
  "ph_wtr_improve",
  "ph_sani_improve",
  "nt_bf_curr",
  "fever",
  "diarrhea",
  "nt_mdd" 
)

# Filter to keep only complete cases for the selected variables
df_clean <- df %>% filter(if_all(all_of(complete_vars), ~ !is.na(.)))

#check how many were dropped
n_original <- nrow(df)
n_clean <- nrow(df_clean)
cat("Dropped", n_original - n_clean, "cases due to missing covariates.\n")


#TABLE 1
#Define variables
# Define all variables you want in the table
vars <- c("age", "sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
          "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
          "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
          "fever", "diarrhea", "upf_consumed")  # include exposure if needed

#Specify categorical variables
cat_vars <- c(
  "upf_consumed",
  "child_agegrp",
  "sex",
  "birth",
  "mom_agegrp",
  "mom_edu",
  "working",
  "anc_cat2",
  "bmi_cat",
  "wealth",
  "residence",
  "hhnum",
  "ph_wtr_improve",
  "ph_sani_improve",
  "nt_mdd",
  "nt_bf_curr",
  "fever",
  "diarrhea"
)

# Run Table 1
table1 <- CreateTableOne(
  vars = vars,
  factorVars = cat_vars,
  data = df_clean,
  includeNA = FALSE
)

print(table1, catDigits = 1, contDigits = 1, showAllLevels = TRUE, quote = FALSE)


#TABLE 2
#Ensure upf_consumed is factor
table2 <- CreateTableOne(
  vars = vars,
  factorVars = cat_vars,
  strata = "upf_consumed",
  data = df_clean,
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

#Table 3

# Build model formulas for each outcome
outcomes <- c("nt_ch_stunt", "nt_ch_wast", "nt_ch_underwt", "nt_ch_ovwt")

svy_design <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_clean,  # Make sure df contains all needed variables
  nest = TRUE
)

model_formulas <- setNames(
  lapply(outcomes, function(outcome) {
    as.formula(paste(outcome, "~ upf_consumed +", paste(cat_vars, collapse = " + ")))
  }),
  outcomes
)

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
print(detailed_results,n=120)

#TABLE 5

# Ensure survey design contains all variables
svy_design <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_clean,  # Make sure df contains all needed variables
  nest = TRUE
)


# 3. Analyze all outcomes
# Analyze all outcomes
results <- lapply(outcomes, function(outcome) {
  formula <- as.formula(paste(outcome, "~ upf_freq +", paste(vars, collapse = "+")))
  
  model <- svyglm(formula, design = svy_design, family = quasibinomial())
  
  coef_summary <- summary(model)$coefficients
  
  # Get rows matching UPF frequency levels
  upf_terms <- rownames(coef_summary)[grepl("^upf_freq", rownames(coef_summary))]
  
  # For each level (excluding reference), extract ORs
  data.frame(
    Outcome = outcome,
    Term = upf_terms,
    OR = exp(coef_summary[upf_terms, "Estimate"]),
    Lower_CI = exp(coef_summary[upf_terms, "Estimate"] - 1.96 * coef_summary[upf_terms, "Std. Error"]),
    Upper_CI = exp(coef_summary[upf_terms, "Estimate"] + 1.96 * coef_summary[upf_terms, "Std. Error"]),
    p_value = coef_summary[upf_terms, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
})

# Combine into one table
final_results <- do.call(rbind, results)
print(final_results)



#TABLE 6
upf_foods <- c("v413c", "v413d", "v414r", "v414t")
food_labels <- c("Chocholate-flavored drink", "Sodas/malt/energy drinks", "Chocolates/sweets/candies", "Chips/crisps/fries")


food_results <- lapply(1:4, function(i) {
  formula <- as.formula(paste("nt_ch_wast ~", upf_foods[i], "+", paste(vars, collapse = "+")))
  
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
