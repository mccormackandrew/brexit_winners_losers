# When the same questions are asked across multiple waves in the BES,
# each question is it's own variable. The dataset is *wide*, with each row 
# containing the same individual across all waves. I need the dataset
# to be *long*, i.e. in respondent-wave format to run my analysis
# Because some questions were repeated in some waves, some not in others, etc.,
# I cannot simply use gather. It needs to be done manually with each question.

# Bring in local election dummies
election_dummies <- read.csv("data/localelectiondummies.csv")

# Select time invariant control variables and gather them (we don't want all the variables the final data frame)
bes_controls <- bes %>% dplyr::select(leave_intent, education, occupation_cats, 
                              income, age, gender, lr, attention, pid, 
                              immigecon, immigcult, local_authority, country)

bes_long <- bes_controls %>% 
  # The mutate below is an identifying row to indicate the respondent number, 
  # this will become important when data are gathered into person-wave format
  mutate(number = 1:nrow(bes)) %>%
  # Bring in local authority election dummies
  left_join(election_dummies, by = c("local_authority" = "district")) %>%
  ## This part is crucial---used with gather to make the dataframe long, then gathered DVs can be merged with this master DF
  mutate(w1 = 1, w2 = 2, w3 = 3, w4 = 4, w5 = 5, w6 = 6, w7 = 7, w8 = 8, w9 = 9, w10 = 10, w11 = 11, w12 = 12, w13 = 13, w14 = 14) %>%
  gather("wave_num", "wave", w1:w14) 


# Satifaction with democracy ------------------------------------
bes_long$satdem <- bes %>% dplyr::select(starts_with("satDemUK")) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate(satDemUKW5 = rep(NA, nrow(bes))) %>%
  mutate(satDemUKW12 = rep(NA, nrow(bes))) %>% 
  mutate(satDemUKW14 = rep(NA, nrow(bes))) %>% 
  dplyr::select(satDemUKW1:satDemUKW4, satDemUKW5, satDemUKW6:satDemUKW11, satDemUKW12, satDemUKW13, everything()) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse(. == 9999, NA, .))) %>%
  gather(key = "satdem_wave", value = "satdem", -number) %>%
  dplyr::select(satdem) %>%
  as.matrix() %>%
  as.vector()

# External efficacy ---------------------------------------------
## "Politicians care about what people like me think"
bes_long$efficacypolcare <- bes %>% dplyr::select(starts_with("efficacyPolCare")) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate(efficacyPolCareW5 = rep(NA, nrow(bes))) %>%
  mutate(efficacyPolCareW12 = rep(NA, nrow(bes))) %>%  
  mutate(efficacyPolCareW13 = rep(NA, nrow(bes))) %>%  
  mutate(efficacyPolCareW14 = rep(NA, nrow(bes))) %>%  
  dplyr::select(efficacyPolCareW1:efficacyPolCareW4, efficacyPolCareW5, efficacyPolCareW6:efficacyPolCareW11, efficacyPolCareW12, efficacyPolCareW13, efficacyPolCareW14, everything()) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse(. == 9999, NA, .))) %>%
  gather(key = "efficacy_polcare_wave", value = "efficacy_polcare", -number) %>%
  dplyr::select(efficacy_polcare) %>%
  as.matrix() %>%
  as.vector()

# Internal efficacy ---------------------------------------------
## "I understand the issues facing the country"
bes_long$efficacyunderstand <- bes %>% dplyr::select(starts_with("efficacyUnderstandW")) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate(efficacyUnderstandW5 = rep(NA, nrow(bes))) %>%
  mutate(efficacyUnderstandW12 = rep(NA, nrow(bes))) %>%
  mutate(efficacyUnderstandW13 = rep(NA, nrow(bes))) %>%
  mutate(efficacyUnderstandW14 = rep(NA, nrow(bes))) %>%
  dplyr::select(efficacyUnderstandW1:efficacyUnderstandW4, efficacyUnderstandW5, efficacyUnderstandW6, efficacyUnderstandW7,
                efficacyUnderstandW8, efficacyUnderstandW9, efficacyUnderstandW10, efficacyUnderstandW11, 
                efficacyUnderstandW12, efficacyUnderstandW13, efficacyUnderstandW14, number) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse(. == 9999, NA, .))) %>%
  gather(key = "efficacyunderstand_wave", value = "efficacyunderstand", -number) %>%
  dplyr::select(efficacyunderstand) %>%
  as.matrix() %>%
  as.vector()

# Retrospective sociotropic -------------------------------------
## "How has general economic situation in the country changed over last 12 months"
bes_long$econgenretro <- bes %>% dplyr::select(starts_with("econGenRetroW")) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate(econGenRetroW5 = rep(NA, nrow(bes))) %>%
  mutate(econGenRetroW9 = rep(NA, nrow(bes))) %>%
  mutate(econGenRetroW14 = rep(NA, nrow(bes))) %>%
  dplyr::select(econGenRetroW1:econGenRetroW4, econGenRetroW5, econGenRetroW6:econGenRetroW8, 
                econGenRetroW9, econGenRetroW10:econGenRetroW13, econGenRetroW14, number) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse(. == 9999, NA, .))) %>%
  gather(key = "econgenretro_wave", value = "econgenretro", -number) %>%
  dplyr::select(econgenretro) %>%
  as.matrix() %>%
  as.vector()

# Retrospective egocentric --------------------------------------
## "How has financial situation of your household compare to 12 months ago"
bes_long$econpersonalretro <- bes %>% dplyr::select(starts_with("econPersonalRetroW")) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate(econPersonalRetroW5 = rep(NA, nrow(bes))) %>%
  mutate(econPersonalRetroW9 = rep(NA, nrow(bes))) %>%
  mutate(econPersonalRetroW14 = rep(NA, nrow(bes))) %>%
  dplyr::select(econPersonalRetroW1:econPersonalRetroW4, econPersonalRetroW5, econPersonalRetroW6:econPersonalRetroW8, 
                econPersonalRetroW9, econPersonalRetroW10:econPersonalRetroW13, econPersonalRetroW14, number) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse(. == 9999, NA, .))) %>%
  gather(key = "econpersonalretro_wave", value = "econpersonalretro", -number) %>%
  dplyr::select(econpersonalretro) %>%
  as.matrix() %>%
  as.vector()

# Economy getting better ----------------------------------------
## "Do you think economy getting better, worse, staying same?"
bes_long$economybetter <- bes %>% dplyr::select(starts_with("changeEc")) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate(changeEconomyW5 = rep(NA, nrow(bes))) %>%
  mutate(changeEconomyW6 = rep(NA, nrow(bes))) %>%
  mutate(changeEconomyW13 = rep(NA, nrow(bes))) %>%
  mutate(changeEconomyW14 = rep(NA, nrow(bes))) %>%
  dplyr::select(changeEconomyW1, changeEconomyW2, changeEconomyW3, changeEconomyW4, changeEconomyW5,
                changeEconomyW6, changeEconomyW7, changeEconomyW8, changeEconomyW9, changeEconomyW10,
                changeEconomyW11, changeEconomyW12, changeEconomyW13, changeEconomyW14, number) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse(. == 9999, NA, .))) %>%
  gather(key = "economybetter_wave", value = "economybetter", -number) %>%
  dplyr::select(economybetter) %>%
  as.matrix() %>%
  as.vector()

# Risk of poverty -----------------------------------------------
## "During next 12 months, how likely/unlikely that there will be not enough money for daily living costs"
bes_long$riskpoverty <- bes %>% dplyr::select(starts_with("riskPovertyW")) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate(riskPovertyW5 = rep(NA, nrow(bes))) %>%
  mutate(riskPovertyW12 = rep(NA, nrow(bes))) %>%
  mutate(riskPovertyW13 = rep(NA, nrow(bes))) %>%
  mutate(riskPovertyW14 = rep(NA, nrow(bes))) %>%
  dplyr::select(riskPovertyW1:riskPovertyW4, riskPovertyW5, riskPovertyW6, riskPovertyW7, riskPovertyW8, 
                riskPovertyW9, riskPovertyW10, riskPovertyW11, riskPovertyW12, riskPovertyW13, riskPovertyW14, number) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse(. == 9999, NA, .))) %>%
  gather(key = "riskpoverty_wave", value = "riskpoverty", -number) %>%
  dplyr::select(riskpoverty) %>%
  as.matrix() %>%
  as.vector()

# Trust in MPs --------------------------------------------------
## "How much trust in MPs in general?"
bes_long$trustmps <- bes %>% dplyr::select(starts_with("trustMPsW")) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate(trustMPsW5 = rep(NA, nrow(bes))) %>%
  mutate(trustMPsW11 = rep(NA, nrow(bes))) %>%
  mutate(trustMPsW13 = rep(NA, nrow(bes))) %>%
  mutate(trustMPsW14 = rep(NA, nrow(bes))) %>%
  dplyr::select(trustMPsW1, trustMPsW2, trustMPsW3, trustMPsW4, trustMPsW5, trustMPsW6, trustMPsW7,
                trustMPsW8, trustMPsW9, trustMPsW10, trustMPsW11, trustMPsW12, trustMPsW13, trustMPsW14, number) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse(. == 9999, NA, .))) %>%
  gather(key = "trustmps_wave", value = "trustmps", -number) %>%
  dplyr::select(trustmps) %>%
  as.matrix() %>%
  as.vector()

# Brexit conduct ------------------------------------------------
## Brexit will be/was conducted fairly
bes$expectGoodConductEURefW9 <- bes$goodConductEURefW9

bes_long$euconduct <- bes %>% dplyr::select(expectGoodConductEURefW7, 
                                   expectGoodConductEURefW9, 
                                   expectGoodConductEURefW10, 
                                   expectGoodConductEURefW11) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate(expectGoodConductEURefW1 = rep(NA, nrow(bes))) %>%
  mutate(expectGoodConductEURefW2 = rep(NA, nrow(bes))) %>%
  mutate(expectGoodConductEURefW3 = rep(NA, nrow(bes))) %>%
  mutate(expectGoodConductEURefW4 = rep(NA, nrow(bes))) %>%
  mutate(expectGoodConductEURefW5 = rep(NA, nrow(bes))) %>%
  mutate(expectGoodConductEURefW6 = rep(NA, nrow(bes))) %>%
  mutate(expectGoodConductEURefW8 = rep(NA, nrow(bes))) %>%
  mutate(expectGoodConductEURefW12 = rep(NA, nrow(bes))) %>%
  mutate(expectGoodConductEURefW13 = rep(NA, nrow(bes))) %>%
  mutate(expectGoodConductEURefW14 = rep(NA, nrow(bes))) %>%
  dplyr::select(expectGoodConductEURefW1, expectGoodConductEURefW2, 
                expectGoodConductEURefW3, expectGoodConductEURefW4, 
                expectGoodConductEURefW5,expectGoodConductEURefW6, 
                expectGoodConductEURefW7, expectGoodConductEURefW8, 
                expectGoodConductEURefW9, expectGoodConductEURefW10,
                expectGoodConductEURefW11, expectGoodConductEURefW12, 
                expectGoodConductEURefW13, expectGoodConductEURefW14, number) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse(. == 9999, NA, .))) %>%
  gather(key = "euconduct_wave", value = "euconduct", -number) %>%
  dplyr::select(euconduct) %>%
  as.matrix() %>%
  as.vector()

# Survey weights ------------------------------------------------
bes_long$weight <- bes %>% dplyr::select(wt_full_W1, wt_full_W2, wt_full_W3, wt_full_W4, wt_full_W5, wt_full_W6, wt_full_W7,
                                  wt_full_W8, wt_full_W9, wt_full_W10, wt_full_W11, wt_new_W12, wt_new_W13, wt_new_W14) %>%
  mutate(number = 1:nrow(bes)) %>%
  mutate_all(funs(as.numeric)) %>%
  gather(key = "weight_wave", value = "weight", -number) %>%
  dplyr::select(weight) %>%
  as.matrix() %>%
  as.vector()

# Create "leave" and "brexit" indicators ------------------------

bes_long$leave <- bes_long$leave_intent
bes_long$brexit <- ifelse(bes_long$wave > 8, 1, 0)

# Scale the variables to range from 0 to 1 ----------------------
## Satdem
bes_long$satdem <- (bes_long$satdem - 1)/3

## External efficacy
bes_long$efficacypolcare <- (bes_long$efficacypolcare - 1)/4
# Reverse scale so that 1 means higher efficacy
bes_long$efficacypolcare <- car::recode(bes_long$efficacypolcare, "0=1;0.25=0.75;0.75=0.25;1=0") 

## Internal efficacy
bes_long$efficacyunderstand <- (bes_long$efficacyunderstand - 1)/4

## Risk of poverty
bes_long$riskpoverty <- (bes_long$riskpoverty - 1)/4

## Economy getting better
bes_long$economybetter <- (bes_long$economybetter - 1)/4

## Trust in MPs
bes_long$trustmps <- (bes_long$trustmps - 1)/6

## How fairly will Brexit be conducted
bes_long$euconduct <- (bes_long$euconduct - 1)/4
# Reverse scale so that 1 means conducted fairly
bes_long$euconduct <- car::recode(bes_long$euconduct, "0=1;0.25=0.75;0.75=0.25;1=0")

# Retrospective general economic evals
bes_long$econgenretro <- (bes_long$econgenretro - 1)/4
# Retrospective personal economic evals
bes_long$econpersonalretro <- (bes_long$econpersonalretro - 1)/4


# Controls
bes_long$income <- (bes_long$income - 1)/14
bes_long$lr <- (bes_long$lr)/10
bes_long$attention <- (bes_long$attention)/10
bes_long$immigcult <- (bes_long$immigcult - 1)/6
bes_long$immigecon <- (bes_long$immigecon - 1)/6

# Make occupation a dummy 1 if routine/semi-routine/technical job
bes_long$occupation <- ifelse(bes$occupation_cats == "Routine occupations" | 
                                bes_long$occupation_cats == "Semi-routine occupations" | 
                                bes_long$occupation_cats == "Lower supervisory and technical occupations", 1, 0)




