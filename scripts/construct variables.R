library(here)
#========================================================================
# CONSTRUCT VARIABLES
#========================================================================
# The Benin HIV Peer Support Project has legacy data which was cleaned
# and prepared for analysis - this data is used here to construct variables
# necessary for analysis

# Run master script
source(here(file.path('scripts', 'master.R')))

# 1. PRIMARY OUTCOME: STI INCIDENCE
#========================================================================
# 1.1 - HSV+
# 1.2 = HSV+ (recent infection)
# 1.3 - Self-reported infection
# 1.4 - Self-reported abstinence or condom use during infection
# 1.5 - Self-reported treatment and advice-seeking during infection

# 2. SECONDARY OUTCOME: HIV AND STI KNOWLEDGE
#========================================================================
# 2.1 HIV prevention (got at least 3 out of 4 knowledge questions correct)
# 2.2 STI symptoms (identified at least 3 male or female STI symptoms)
# 2.3 STI actions if infected (at least 3 right actions identified)

# 3. SECONDARY OUTCOME: HIV AND STI ATTITUDES
#========================================================================
# 3.1 Attitudes towards PWLHIV (people who live with HIV) 
#   (= 1 if answered yes to QG13, 14)
# 3.2 Ever tested for HIV *** confirm that this is an attitude indicator

# 4. STI AND HIV RELATED BEHAVIORS
#========================================================================
# 4.1 Sexual activity = any sex in the last 12 months
# 4.2 Sexual concurrency = sex with multiple partners in the last 12 months
# 4.3 Inter-generational sex = partner more than 10 years older in the last 12 months
# 4.4 Condom use (partner who never uses condoms, condom use in last sexual
#     encounter, condom use with commercial/non-commercial partners)


benin_clean <- benin_data %>% 
 # Location and control variables
  mutate(department = factor(id_1),
         establishment = id_2,
         school = factor(ecole),
         student = eleve,
         is_pe = qa01,
         district = qa08,
         village = qa09,
         urban = urbain,
         female = feminin,
         treat_1 = if_else(interv == 1,1,0),
         treat_2 = if_else(interv == 2,1,0),
         control = if_else(interv == 3,1,0),
         treat = as.factor(case_when(treat_1 == 1 ~ "Peer+",
                                     treat_2 == 1 ~ "NGO",
                                     TRUE ~ "Control")),
         secondary_2 = seccyle2,
         age = as.factor(case_when(age1617 == 1 ~ "16-17 years",
                                   age1820 == 1 ~ "18-20 years",
                                   age21plus == 1 ~ "21+ years"))) %>% 
         # Primary outcomes
          mutate(hsv_pos = (igg==1),
                 hsv_pos_recent = (HSV2r==1),
                 sti_selfreport = case_when(qf16 == 1 ~ 1,
                                            qf16 == 2 ~ 0,
                                            qf16 == 88 ~ as.numeric(NA),
                                            qf16 == 99 ~ as.numeric(NA)),
                 sti_abstinence_condoms = case_when(qf18 == 1 ~ 1,
                                                    qf18 == 2 ~ 1,
                                                    qf18 %in% c(0,7) ~ 0,
                                                    TRUE ~ as.numeric(NA)),
                 sti_advice = case_when(qf19 == 1 ~ 1,
                                        qf19 == 2 ~ 0,
                                        TRUE ~ as.numeric(NA))) %>% 
  # 2. Secondary outcomes: knowledge
  mutate(hiv_prevention = case_when(prevvih2 == 1 ~ 1,
                                    TRUE ~ 0),
         sti_symptoms = (conm3ISTfem == 1 | conm3ISThom == 1),
         sti_actions = (cond3IST==1)) %>% 
  # 3. Secondary outcomes: attitudes
  mutate(attitudes_plhiv = case_when(qg13==1 | qg14==0 ~ 1,
                                     qg13 %in% c(88,99) | qg14 %in% c(NA,88,99) |
                                     is.na(qg13) ~ as.numeric(NA),
                                     TRUE ~ 0),
         hiv_testing = case_when(qg16==1 ~ 1,
                                 qg16==2 ~ 0,
                                 qg16==88 ~ as.numeric(NA),
                                 TRUE ~ as.numeric(NA))) %>% 
  # 4. Secondary outcomes: behaviors
  mutate(sex_any = case_when(qc04>0 ~ 1,
                             qc04==0 ~ 0,
                             TRUE ~ as.numeric(NA)),
         sex_multiple = case_when(qc04>1 ~ 1,
                                  qc04 %in% c(0,1) ~ 0,
                                  TRUE ~ as.numeric(NA)),
         sex_older = sugdady4,
         condom_always = utpr1,
         condom_last = utpr3,
         condom_noncommercial = utpr4,
         condom_commercial = utpr5) %>% 
  # Keep only relevant columns

  dplyr::select(department, establishment, school,
         student, is_pe, district, village, 
         urban, female, treat_1, treat_2, 
         control, treat, secondary_2, age, hsv_pos, hsv_pos_recent,
         sti_selfreport, sti_abstinence_condoms, 
         sti_advice, hiv_prevention, sti_symptoms, sti_actions,
         attitudes_plhiv, hiv_testing, sex_any,
         sex_multiple, sex_older, condom_always, 
         condom_last, condom_noncommercial, condom_commercial)




