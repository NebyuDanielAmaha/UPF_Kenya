#STRATIFICATION

# Stratification variables to summarize
strat_vars <- c("child_agegrp", "birth", "mom_edu", "working", 
                "anc_cat2", "bmi_cat", "wealth", "residence",
                "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
                "fever", "diarrhea")


# Function to summarize wasting distribution and UPF consumption % among wasted by stratification variable
summarize_wasting_upf_pct <- function(var) {
  df %>%
    dplyr::select(all_of(c(var, "nt_ch_wast", "upf_consumed"))) %>%
    filter(!is.na(.data[[var]]), !is.na(nt_ch_wast), !is.na(upf_consumed)) %>%
    group_by(level = .data[[var]]) %>%
    summarise(
      total = n(),
      wasted = sum(nt_ch_wast == 1),
      percent_wasted = round(100 * wasted / total, 2),
      wasted_upf = sum(nt_ch_wast == 1 & upf_consumed == "Consumed UPF"),
      wasted_no_upf = sum(nt_ch_wast == 1 & upf_consumed == "No UPF"),
      pct_wasted_upf = round(100 * wasted_upf / wasted, 2),
      pct_wasted_no_upf = round(100 * wasted_no_upf / wasted, 2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(variable = var) %>%
    dplyr::select(variable, level, total, wasted, percent_wasted, 
                  wasted_upf, pct_wasted_upf, wasted_no_upf, pct_wasted_no_upf)
}

# Apply for all stratification variables and combine results
wasting_distribution_upf_pct <- purrr::map_dfr(strat_vars, summarize_wasting_upf_pct)

# View the full table
print(wasting_distribution_upf_pct, n = 100)



# REGRESSION ANALYSIS
# 1) AGE
co_age <- c("sex", "birth", "mom_agegrp", "mom_edu", "working", 
            "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
            "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
            "fever", "diarrhea")

# Function to perform stratified analysis by any variable
# Create your complete-case data first
model_vars <- c("nt_ch_wast", "upf_consumed", "child_agegrp", "v021", "v022", "wt", co_age)

df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

# Stratification variable
age_groups <- levels(df_sub$child_agegrp)

# Function to run stratified model
run_strat_model <- function(age_level) {
  design_stratum <- subset(svy_design_sub, child_agegrp == age_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_age, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = age_level)
}

# Apply across all age groups
results_age <- purrr::map_dfr(age_groups, possibly(run_strat_model, otherwise = NULL))
print(results_age)


# 2) RESIDENCE
# Covariates (same full model as before)
co_red <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
            "anc_cat2", "bmi_cat", "wealth", "hhnum",
            "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
            "fever", "diarrhea")

# Base variables needed for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "residence", "v021", "v022", "wt", co_red)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

# Get residence categories
residence_groups <- levels(df_sub$residence)

# Function to run stratified model
run_strat_model <- function(res_level) {
  design_stratum <- subset(svy_design_sub, residence == res_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_red, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = res_level)
}

# Apply model for each residence group
results_residence <- map_dfr(residence_groups, possibly(run_strat_model, otherwise = NULL))

# View results
print(results_residence)


# 3) WEALTH
# Covariates
co_wealth <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
               "anc_cat2", "bmi_cat", "residence", "hhnum",
               "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
               "fever", "diarrhea")


# Base variables needed for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "wealth", "v021", "v022", "wt", co_wealth)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

# Get wealth categories
wealth_groups <- levels(df_sub$wealth)

# Function to run stratified model
run_strat_model <- function(wealth_level) {
  design_stratum <- subset(svy_design_sub, wealth == wealth_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_wealth, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = wealth_level)
}

# Apply model for each wealth group
results_wealth <- map_dfr(wealth_groups, possibly(run_strat_model, otherwise = NULL))

# View results
print(results_wealth)

# 4) DIARRHEA
co_dia <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
            "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
            "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
            "fever")

# Base variables needed for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "diarrhea", "v021", "v022", "wt", co_dia)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

diarrhea_groups <- levels(df_sub$diarrhea)

# Function to run stratified model
run_strat_model <- function(diarrhea_level) {
  design_stratum <- subset(svy_design_sub, diarrhea == diarrhea_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_dia, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = diarrhea_level)
}

# Apply model for each diarrhea group
results_diarrhea <- map_dfr(diarrhea_groups, possibly(run_strat_model, otherwise = NULL))

# View results
print(results_diarrhea)

# 5) FEVER

# Covariates excluding the stratification variable
co_fever <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
              "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
              "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
              "diarrhea")

# Base variables needed for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "fever", "v021", "v022", "wt", co_fever)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

# Get fever categories
fever_groups <- levels(df_sub$fever)

# Function to run stratified model by fever
run_strat_model <- function(fever_level) {
  design_stratum <- subset(svy_design_sub, fever == fever_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_fever, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(str_detect(term, "upf_consumed")) %>%
    mutate(stratum = fever_level)
}

# Apply model for each fever group
results_fever <- map_dfr(fever_groups, possibly(run_strat_model, otherwise = NULL))

# View results
print(results_fever)


# 6) MOM EDU
# Maternal education stratification
co_momedu <- c("sex", "child_agegrp", "birth", "mom_agegrp", "working", 
               "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
               "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
               "fever", "diarrhea")

# Base variables needed for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "mom_edu", "v021", "v022", "wt", co_momedu)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

mom_edu_groups <- levels(df_sub$mom_edu)

# Function to run stratified model
run_strat_model <- function(mom_edu_level) {
  design_stratum <- subset(svy_design_sub, mom_edu == mom_edu_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_momedu, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = mom_edu_level)
}

# Apply model for each maternal education group
results_mom_edu <- map_dfr(mom_edu_groups, possibly(run_strat_model, otherwise = NULL))

# View results
print(results_mom_edu)


# 7) MATERNAL BMI
# Maternal BMI Category
co_bmi <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
            "anc_cat2", "wealth", "residence", "hhnum",
            "ph_wtr_improve", "ph_sani_improve", "nt_mdd", "nt_bf_curr",
            "fever", "diarrhea")

# Base variables needed for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "bmi_cat", "v021", "v022", "wt", co_bmi)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

# Get BMI categories
bmi_groups <- levels(df_sub$bmi_cat)

# Function to run stratified model
run_strat_model <- function(bmi_level) {
  design_stratum <- subset(svy_design_sub, bmi_cat == bmi_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_bmi, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = bmi_level)
}

# Apply model for each BMI category
results_bmi <- purrr::map_dfr(bmi_groups, possibly(run_strat_model, otherwise = NULL))

# View results
print(results_bmi)



#8) WATER SOURCE
# WATER SOURCE
co_water <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
              "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
              "ph_sani_improve", "nt_mdd", "nt_bf_curr",
              "fever", "diarrhea")

# Base variables needed for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "ph_wtr_improve", "v021", "v022", "wt", co_water)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

# Get water source categories
water_groups <- levels(df_sub$ph_wtr_improve)

# Function to run stratified model
run_strat_model <- function(water_level) {
  design_stratum <- subset(svy_design_sub, ph_wtr_improve == water_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_water, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = water_level)
}

# Apply model for each water group
results_water <- purrr::map_dfr(water_groups, possibly(run_strat_model, otherwise = NULL))

# View results
print(results_water)

# 9) SANITATION
# Define covariates
co_sani <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
             "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
             "ph_wtr_improve", "nt_mdd", "nt_bf_curr",
             "fever", "diarrhea")

# Define base variables for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "ph_sani_improve", "v021", "v022", "wt", co_sani)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design object
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

# Get sanitation levels
sani_groups <- levels(df_sub$ph_sani_improve)

# Function to run stratified model
run_strat_model <- function(sani_level) {
  design_stratum <- subset(svy_design_sub, ph_sani_improve == sani_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_sani, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = sani_level)
}

# Apply model to each sanitation group
results_sanitation <- map_dfr(sani_groups, possibly(run_strat_model, otherwise = NULL))

# Display results
print(results_sanitation)


#10 breastfeeding
# Define covariates
co_bf <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
           "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
           "ph_wtr_improve", "ph_sani_improve", "nt_mdd",
           "fever", "diarrhea")  # Removed nt_bf_curr because it's the stratifier

# Define base variables for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "nt_bf_curr", "v021", "v022", "wt", co_bf)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design object
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

# Get breastfeeding categories
bf_groups <- levels(df_sub$nt_bf_curr)

# Function to run stratified model
run_strat_model <- function(bf_level) {
  design_stratum <- subset(svy_design_sub, nt_bf_curr == bf_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_bf, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = bf_level)
}

# Apply model to each breastfeeding group
results_bf <- purrr::map_dfr(bf_groups, possibly(run_strat_model, otherwise = NULL))

# Display results
print(results_bf)

#11 Dietary Diversity
# Define covariates excluding the stratifier nt_mdd
co_mdd <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
            "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
            "ph_wtr_improve", "ph_sani_improve",
            "fever", "diarrhea")  # removed nt_mdd

# Define base variables for survey design and model
model_vars <- c("nt_ch_wast", "upf_consumed", "nt_mdd", "v021", "v022", "wt", co_mdd)

# Prepare complete-case dataset
df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

# Create survey design object
svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

# Get nt_mdd categories
mdd_groups <- levels(df_sub$nt_mdd)

# Function to run stratified model
run_strat_model <- function(mdd_level) {
  design_stratum <- subset(svy_design_sub, nt_mdd == mdd_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_mdd, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = mdd_level)
}

# Apply model to each nt_mdd group
results_mdd <- purrr::map_dfr(mdd_groups, possibly(run_strat_model, otherwise = NULL))

# Display results
print(results_mdd)

#12 Working status

# Stratification by working status

co_working <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", 
                "anc_cat2", "bmi_cat", "wealth", "residence", "hhnum",
                "ph_wtr_improve", "ph_sani_improve",
                "nt_mdd", "nt_bf_curr",
                "fever", "diarrhea")  # exclude 'working'

model_vars <- c("nt_ch_wast", "upf_consumed", "working", "v021", "v022", "wt", co_working)

df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

working_groups <- levels(df_sub$working)

run_strat_model <- function(work_level) {
  design_stratum <- subset(svy_design_sub, working == work_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_working, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = work_level)
}

results_working <- purrr::map_dfr(working_groups, possibly(run_strat_model, otherwise = NULL))

print(results_working)

#13 ANC visits
# Stratification by ANC visits

co_anc <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", 
            "bmi_cat", "wealth", "residence", "hhnum",
            "ph_wtr_improve", "ph_sani_improve",
            "nt_mdd", "nt_bf_curr",
            "fever", "diarrhea")  # exclude 'anc_cat2'

model_vars <- c("nt_ch_wast", "upf_consumed", "anc_cat2", "v021", "v022", "wt", co_anc)

df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

anc_groups <- levels(df_sub$anc_cat2)

run_strat_model <- function(anc_level) {
  design_stratum <- subset(svy_design_sub, anc_cat2 == anc_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_anc, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = anc_level)
}

results_anc <- purrr::map_dfr(anc_groups, possibly(run_strat_model, otherwise = NULL))

print(results_anc)

#14 Birth order
# Stratification by birth order

co_birth <- c("sex", "child_agegrp", "mom_agegrp", "mom_edu", "working", "anc_cat2", 
              "bmi_cat", "wealth", "residence", "hhnum",
              "ph_wtr_improve", "ph_sani_improve",
              "nt_mdd", "nt_bf_curr",
              "fever", "diarrhea")  # exclude 'birth'

model_vars <- c("nt_ch_wast", "upf_consumed", "birth", "v021", "v022", "wt", co_birth)

df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

birth_groups <- levels(df_sub$birth)

run_strat_model <- function(birth_level) {
  design_stratum <- subset(svy_design_sub, birth == birth_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_birth, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = birth_level)
}

results_birth <- purrr::map_dfr(birth_groups, possibly(run_strat_model, otherwise = NULL))

print(results_birth)

#15 Household size
# Stratification by household size

co_hhnum <- c("sex", "child_agegrp", "birth", "mom_agegrp", "mom_edu", "working", "anc_cat2", 
              "bmi_cat", "wealth", "residence",
              "ph_wtr_improve", "ph_sani_improve",
              "nt_mdd", "nt_bf_curr",
              "fever", "diarrhea")  # exclude 'hhnum'

model_vars <- c("nt_ch_wast", "upf_consumed", "hhnum", "v021", "v022", "wt", co_hhnum)

df_sub <- df %>%
  dplyr::select(all_of(model_vars)) %>%
  na.omit()

svy_design_sub <- svydesign(
  ids = ~v021,
  strata = ~v022,
  weights = ~wt,
  data = df_sub,
  nest = TRUE
)

hhnum_groups <- levels(df_sub$hhnum)

run_strat_model <- function(hhnum_level) {
  design_stratum <- subset(svy_design_sub, hhnum == hhnum_level)
  
  model <- svyglm(
    as.formula(paste("nt_ch_wast ~ upf_consumed +", paste(co_hhnum, collapse = " + "))),
    design = design_stratum,
    family = quasibinomial()
  )
  
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "upf_consumedConsumed UPF") %>%
    mutate(stratum = hhnum_level)
}

results_hhnum <- purrr::map_dfr(hhnum_groups, possibly(run_strat_model, otherwise = NULL))

print(results_hhnum)
