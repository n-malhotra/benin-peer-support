library(here)
#========================================================================
# RUN ANALYSIS
#========================================================================
# 1. OLS regression
# 2. Probit regressions

primary <- c("hsv_pos", "hsv_pos_recent", "sti_selfreport",
             "sti_abstinence_condoms", "sti_advice")

primary_labels <- c("HSV+", "HSV+ (recent)", 
                    "STI infection <br> (self-report)",
                    "Abstinence/condom use <br> during STI infection",
                    "Sought advice for <br> STI infection")

secondary_knowledge <- c("hiv_prevention", "sti_symptoms", "sti_actions")

secondary_knowledge_labels <- c("HIV prevention knowledge",
                                "STI symptoms knowledge",
                                "Knows STI actions if infected")

secondary_attitudes <- c("attitudes_plhiv", "hiv_testing")

secondary_attitudes_labels <- c("Attitudes towards PLHIV",
                                "Ever tested for HIV")

secondary_behaviors <- c("sex_any", "sex_multiple", 
                         "sex_older", "condom_always", 
                         "condom_last", "condom_noncommercial", 
                         "condom_commercial")

secondary_behaviors_labels <- c("Any sex (last 12 months)",
                                "Sexual concurrency (last 12 months)",
                                "Cross-generational sex (last 12 months)",
                                "Condom use (always)",
                                "Condom use (most recent sex activity)",
                                "Condom use (with non-commercial partner)",
                                "Condom use (with commercial partner)")

# Dependent vars grouped according to outcomes:
# a. primary
# b. secondary_knowledge
# c. secondary_attitudes
# d. secondary_behaviors

## 1. OLS MODELS

# a. primary
for (i in 1:length(primary)) {
  
  model <- paste("ols_primary",i, sep="_")
  
  ols_cl <- felm(as.formula(paste(primary[i], 
                    "~ treat + female + urban + age",
                    "| factor(department) | 0 | school")),
                na.action = na.omit,
                data = benin_clean)
  
  assign(model, ols_cl)
}


# b. secondary_knowledge
for (i in 1:length(secondary_knowledge)) {
  
  model <- paste("ols_secondary_knowledge",i, sep="_")
  
  ols_cl <- felm(as.formula(paste(secondary_knowledge[i], 
                                  "~ treat + female + urban + age",
                                  "| factor(department) | 0 | school")),
                 na.action = na.omit,
                 data = benin_clean)
  assign(model, ols_cl)
}


# c. secondary_attitudes
for (i in 1:length(secondary_attitudes)) {
  
  model <- paste("ols_secondary_attitudes",i, sep="_")
  
  ols_cl <- felm(as.formula(paste(secondary_attitudes[i], 
                                  "~ treat + female + urban + age",
                                  "| factor(department) | 0 | school")),
                 na.action = na.omit,
                 data = benin_clean)
  
  assign(model, ols_cl)
}


# d. secondary_behaviors
for (i in 1:length(secondary_behaviors)) {
  
  model <- paste("ols_secondary_behaviors",i, sep="_")
  
  ols_cl <- felm(as.formula(paste(secondary_behaviors[i], 
                                  "~ treat + female + urban + age",
                                  "| factor(department) | 0 | school")),
                 na.action = na.omit,
                 data = benin_clean)
  
  assign(model, ols_cl)
}


## 2. PROBIT MODELS

# a. primary
for (i in 1:length(primary)) {
  
  model <- paste("pr_primary",i, sep="_")
  
  probit <- glm(paste(primary[i], 
                      "~ treat + female + urban + age"),
                family = binomial(link = "probit"),
                data = benin_clean)
  
  probit_cl <- coeftest(probit, vcov = vcovCL, cluster = ~school)
  
  assign(model, probit_cl)
}

# b. secondary_knowledge
for (i in 1:length(secondary_knowledge)) {
  
  model <- paste("pr_secondary_knowledge",i, sep="_")

  probit <- glm(paste(secondary_knowledge[i], 
                      "~ treat_1 + treat_2 + female + urban + age"),
                family = binomial(link = "probit"),
                data = benin_clean)
  
  probit_cl <- coeftest(probit, vcov = vcovCL, cluster = ~school)

  assign(model, probit_cl)
}

# c. secondary_attitudes
for (i in 1:length(secondary_attitudes)) {
  
  model <- paste("pr_secondary_attitudes",i, sep="_")
  
  probit <- glm(paste(secondary_attitudes[i], 
                      "~ treat_1 + treat_2 + female + urban + age"),
                family = binomial(link = "probit"),
                data = benin_clean)
  
  probit_cl <- coeftest(probit, vcov = vcovCL, cluster = ~school)
  
  assign(model, probit_cl)
}

# d. secondary_behaviors
for (i in 1:length(secondary_behaviors)) {
  
  model <- paste("pr_secondary_behaviors",i, sep="_")
  
  probit <- glm(paste(secondary_behaviors[i], 
                      "~ treat_1 + treat_2 + female + urban + age"),
                family = binomial(link = "probit"),
                data = benin_clean)
  
  probit_cl <- coeftest(probit, vcov = vcovCL, cluster = ~school)
  
  assign(model, probit_cl)
}

#========================================================================
# EXPORT OLS RESULTS
#========================================================================

covar_labels <- c("Treatment: NGO",
                  "Treatment: Peer Support",
                  "Sex: Female",
                  "Location: Urban",
                  "Age: 18-20 yrs (ref = 16-17)",
                  "Age: 21+ yrs (ref = 16-17)")
# OLS - primary outcomes
stargazer(ols_primary_1, ols_primary_2, ols_primary_3, ols_primary_4, ols_primary_5,
          covariate.labels = covar_labels,
          column.labels = primary_labels,
          dep.var.caption = "Outcome:",
          dep.var.labels.include = FALSE,
          omit.stat = c("rsq", "ser"),
          notes = c("Standard errors clustered by school.",
                    "Fixed effects reported at the department level."),
          type = 'html',
          out = here('output', 'primary.html'))

# OLS - Secondary knowledge
stargazer(ols_secondary_knowledge_1, ols_secondary_knowledge_2, ols_secondary_knowledge_3,
          covariate.labels = covar_labels,
          column.labels = secondary_knowledge_labels,
          dep.var.caption = "Outcome:",
          dep.var.labels.include = FALSE,
          omit.stat = c("rsq", "ser"),
          notes = c("Standard errors clustered by school.",
                    "Fixed effects reported at the department level."),
          type = 'html',
          out = here('output', 'ols_secondary_knowledge.html'))

# OLS - Secondary attitudes
stargazer(ols_secondary_attitudes_1, ols_secondary_attitudes_2,
          covariate.labels = covar_labels,
          column.labels = secondary_attitudes_labels,
          dep.var.caption = "Outcome:",
          dep.var.labels.include = FALSE,
          omit.stat = c("rsq", "ser"),
          notes = c("Standard errors clustered by school.",
                    "Fixed effects reported at the department level."),
          type = 'html',
          out = here('output', 'ols_secondary_attitudes.html'))

# OLS - Secondary behaviors

stargazer(ols_secondary_behaviors_1, 
          ols_secondary_behaviors_2,
          ols_secondary_behaviors_3,
          ols_secondary_behaviors_4,
          ols_secondary_behaviors_5,
          ols_secondary_behaviors_6,
          ols_secondary_behaviors_7,
          covariate.labels = covar_labels,
          column.labels = secondary_behaviors_labels,
          dep.var.caption = "Outcome:",
          dep.var.labels.include = FALSE,
          omit.stat = c("rsq", "ser"),
          notes = c("Standard errors clustered by school.",
                    "Fixed effects reported at the department level."),
          type = 'html',
          out = here('output', 'ols_secondary_behaviors.html'))

#========================================================================
# EXPORT PROBIT RESULTS
#========================================================================

# Probit - primary outcomes
stargazer(pr_primary_1, pr_primary_2, pr_primary_3, pr_primary_4, pr_primary_5,
          covariate.labels = covar_labels,
          column.labels = primary_labels,
          dep.var.caption = "Outcome:",
          dep.var.labels.include = FALSE,
          omit.stat = c("rsq", "ser"),
          notes = c("Standard errors clustered by school."),
          type = 'html',
          out = here('output', 'pr_primary.html'))

# Probit - secondary knowledge
stargazer(pr_secondary_knowledge_1,
          pr_secondary_knowledge_2,
          pr_secondary_knowledge_3,
          covariate.labels = covar_labels,
          column.labels = secondary_knowledge_labels,
          dep.var.caption = "Outcome:",
          dep.var.labels.include = FALSE,
          omit.stat = c("rsq", "ser"),
          notes = c("Standard errors clustered by school."),
          type = 'html',
          out = here('output', 'pr_secondary_knowledge.html'))

# Probit - secondary attitudes
stargazer(pr_secondary_attitudes_1,
          pr_secondary_attitudes_2,
          covariate.labels = covar_labels,
          column.labels = secondary_attitudes_labels,
          dep.var.caption = "Outcome:",
          dep.var.labels.include = FALSE,
          omit.stat = c("rsq", "ser"),
          notes = c("Standard errors clustered by school."),
          type = 'html',
          out = here('output', 'pr_secondary_attitudes.html'))

# Probit - secondary behaviors
stargazer(pr_secondary_behaviors_1,
          pr_secondary_behaviors_2,
          pr_secondary_behaviors_3,
          pr_secondary_behaviors_4,
          pr_secondary_behaviors_5,
          pr_secondary_behaviors_6,
          pr_secondary_behaviors_7,
          covariate.labels = covar_labels,
          column.labels = secondary_behaviors_labels,
          dep.var.caption = "Outcome:",
          dep.var.labels.include = FALSE,
          omit.stat = c("rsq", "ser"),
          notes = c("Standard errors clustered by school."),
          type = 'html',
          out = here('output', 'pr_secondary_behaviors.html'))
