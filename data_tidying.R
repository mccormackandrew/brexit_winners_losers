## Tidying BES data
library(tidyverse)
library(haven)

# Data source: https://www.britishelectionstudy.com/data-object/british-election-study-combined-wave-internet-panel/

bes_mstr <- read_dta("data/BES2017_W14_Panel_v0.3.dta")

bes <- dplyr::select(
  bes_mstr,
  country,
  profile_oslaua,
  #local_authority,
  gor,
  gorW2,
  gorW3,
  gorW6,
  onscode,
  #######################
  ## Control variables ##
  #######################
  # Party ID
  partyIdW1, partyIdW2, partyIdW3, partyIdW4, partyIdW6, partyIdW7, partyIdW8,
  # Income (gross household)
  profile_gross_household,
  profile_gross_householdW10,
  profile_gross_householdW11,
  profile_gross_householdW12,
  profile_gross_householdW13,
  # Income (gross personal)
  profile_gross_personal,
  # Gender
  gender,
  # Age
  Age,
  # Occupation
  ns_sec_analyticW1W2W3W4W5,
  ns_sec_analyticW6W7W8W9,
  # Education
  edlevelW1_W6,
  edlevelW7,
  edlevelW8W9W10,
  edlevelW11,
  edlevelW12,
  edlevelW13,
  # Left right
  leftRightW1,
  leftRightW2,
  leftRightW3,
  leftRightW4,
  leftRightW5,
  leftRightW6,
  leftRightW7,
  leftRightW8,
  leftRightW9,
  leftRightW10,
  leftRightW11,
  leftRightW12,
  leftRightW13,
  # Participation
  participation_1W5,
  participation_2W5,
  participation_3W5,
  participation_4W5,
  participation_5W5,
  participation_6W5,
  participation_1W6,
  participation_2W6,
  participation_3W6,
  participation_4W6,
  participation_1W8,
  participation_2W8,
  participation_3W8,
  participation_4W8,
  participation_5W8,
  participation_6W8,
  participation_1W12,
  participation_2W12,
  participation_3W12,
  participation_4W12,
  participation_5W12,
  participation_6W12,
  participation_1W13,
  participation_2W13,
  participation_3W13,
  participation_4W13,
  participation_5W13,
  # Attention to politics
  polAttentionW1,
  polAttentionW2,
  polAttentionW3,
  polAttentionW4,
  polAttentionW6,
  polAttentionW7,
  polAttentionW8,
  polAttentionW10,
  polAttentionW11,
  polAttentionW13,
  ###################################
  ####Satisfaction with democracy####
  ###################################
  # UK
  satDemUKW1,
  satDemUKW2,
  satDemUKW3,
  satDemUKW4,
  satDemUKW6,
  satDemUKW7,
  satDemUKW8,
  satDemUKW9,
  satDemUKW10,
  satDemUKW11,
  satDemUKW13,
  # Scotland
  satDemScotW1,
  satDemScotW2,
  satDemScotW3,
  satDemScotW4,
  satDemScotW6,
  satDemScotW7,
  satDemScotW8,
  satDemScotW9,
  satDemScotW10,
  satDemScotW11,
  satDemScotW13,
  # Wales
  satDemWalesW1,
  satDemWalesW2,
  satDemWalesW3,
  satDemWalesW4,
  satDemWalesW6,
  satDemWalesW7,
  satDemWalesW8,
  satDemWalesW9,
  satDemWalesW10,
  satDemWalesW11,
  satDemWalesW13,
  # England
  satDemEngW1,
  satDemEngW2,
  satDemEngW3,
  satDemEngW4,
  satDemEngW6,
  satDemEngW7,
  satDemEngW8,
  satDemEngW9,
  satDemEngW10,
  satDemEngW11,
  satDemEngW13,
  # EU
  satDemEUW1,
  satDemEUW2,
  satDemEUW3,
  satDemEUW4,
  satDemEUW6,
  satDemEUW7,
  satDemEUW8,
  satDemEUW9,
  satDemEUW10,
  ## EU VOTE
  euRefVoteW1,
  euRefVoteW2,
  euRefVoteW3,
  euRefVoteW4,
  euRefVoteW6,
  euRefVoteW7,
  euRefVoteW8,
  euRefVoteW9,
  euRefVoteW10,
  euRefVoteW11,
  euRefVoteW12,
  euRefVoteW13,
  ## EXPECT GOOD CONDUCT
  expectGoodConductEURefW7,
  expectGoodConductEURefW10,
  expectGoodConductEURefW11,
  ## Weights
  wt_full_W3,
  wt_full_W1W2W3,
  wt_full_W4,
  wt_full_W1W2W3W4,
  wt_full_W5,
  wt_full_W1W2W3W4W5,
  wt_full_W4W5,
  wt_full_W6,
  wt_full_W1W2W3W4W5W6,
  wt_full_W4W5W6,
  wt_full_W4W6,
  wt_full_W7,
  wt_full_W8,
  wt_full_W1W2W3W4W5W6W7W8,
  wt_full_W7W8,
  wt_full_W9,
  wt_full_W1W2W3W4W5W6W7W8W9,
  wt_full_W7W8W9,
  wt_full_W10,
  wt_full_W11,
  wt_full_W1_W11,
  wt_full_W1,
  wt_full_W2,
  wt_full_W1W2,
  wt_full_W1W2W3W4W5W6W7,
  wt_new_W11,
  wt_new_W6W7,
  wt_new_W2W6,
  wt_new_W2W6W7,
  wt_new_W9_W13,
  wt_new_W12,
  wt_new_W13,
  wt_new_W14,
  wt_new_W1_W11,
  wt_new_W1_W12,
  wt_new_W1_W13,
  wt_new_W6_W11,
  wt_new_W6_W12,
  wt_new_W6_W13,
  wt_new_W11_W13,
  wt_new_W10,
  ## Efficacy Politicians Don't Care What People like Me think
  efficacyPolCareW1,
  efficacyPolCareW2,
  efficacyPolCareW3,
  efficacyPolCareW4,
  efficacyPolCareW6,
  efficacyPolCareW7,
  efficacyPolCareW8,
  efficacyPolCareW9,
  efficacyPolCareW10,
  efficacyPolCareW11,
  ## Efficacy doesn't matter what party is in power
  efficacyNoMatterW4,
  efficacyNoMatterW6,
  efficacyNoMatterW7,
  efficacyNoMatterW8,
  efficacyNoMatterW9,
  efficacyNoMatterW10,
  efficacyNoMatterW11,
  ## Efficacy I have understanding of important issues facing our country
  efficacyUnderstandW1,
  efficacyUnderstandW2,
  efficacyUnderstandW3,
  efficacyUnderstandW4,
  efficacyUnderstandW6,
  efficacyUnderstandW7,
  efficacyUnderstandW8,
  efficacyUnderstandW9,
  efficacyUnderstandW10,
  efficacyUnderstandW11,
  ## Too much effort
  efficacyTooMuchEffortW1,
  efficacyTooMuchEffortW2,
  efficacyTooMuchEffortW3,
  efficacyTooMuchEffortW4,
  efficacyTooMuchEffortW6,
  efficacyTooMuchEffortW7,
  efficacyTooMuchEffortW8,
  efficacyTooMuchEffortW9,
  efficacyTooMuchEffortW10,
  efficacyTooMuchEffortW11,
  ## Efficacy I have understanding of important issues facing our country
  efficacyNotUnderstandW1,
  efficacyNotUnderstandW2,
  efficacyNotUnderstandW3,
  efficacyNotUnderstandW4,
  efficacyNotUnderstandW6,
  efficacyNotUnderstandW7,
  efficacyNotUnderstandW8,
  efficacyNotUnderstandW9,
  efficacyNotUnderstandW10,
  efficacyNotUnderstandW11,
  ## Trust MPs
  trustMPsW1,
  trustMPsW2,
  trustMPsW3,
  trustMPsW4,
  trustMPsW6,
  trustMPsW7,
  trustMPsW8,
  trustMPsW9,
  trustMPsW10,
  trustMPsW12,
  ## Econ egotropic retrospective/prospective
  econGenRetroW1,
  econGenProspW1,
  econGenRetroW2,
  econGenProspW2,
  econGenRetroW3,
  econGenRetroW4,
  econGenRetroW6,
  econGenProspW6,
  econGenRetroW7,
  econGenRetroW8,
  econGenRetroW10,
  econGenProspW10,
  econGenRetroW11,
  econGenRetroW12,
  econGenRetroW13,
  ## Econ socio retrospective
  econPersonalRetroW1,
  econPersonalProspW1,
  econPersonalRetroW2,
  econPersonalProspW2,
  econPersonalRetroW3,
  econPersonalRetroW4,
  econPersonalRetroW6,
  econPersonalProspW6,
  econPersonalRetroW7,
  econPersonalRetroW8,
  econPersonalRetroW10,
  econPersonalProspW10,
  econPersonalRetroW11,
  econPersonalRetroW12,
  econPersonalRetroW13,
  ## Risk unemployment
  riskUnemploymentW1,
  riskUnemploymentW2,
  riskUnemploymentW3,
  riskUnemploymentW4,
  riskUnemploymentW6,
  riskUnemploymentW7,
  riskUnemploymentW8,
  riskUnemploymentW9,
  riskUnemploymentW10,
  riskUnemploymentW11,
  ## Risk poverty
  riskPovertyW1,
  riskPovertyW2,
  riskPovertyW3,
  riskPovertyW4,
  riskPovertyW6,
  riskPovertyW7,
  riskPovertyW8,
  riskPovertyW9,
  riskPovertyW10,
  riskPovertyW11,
  ## Economy getting better
  changeEconomyW1,
  changeEconomyW2,
  changeEconomyW3,
  changeEconomyW4,
  changeEconomyW7,
  changeEconomyW8,
  changeEconomyW9,
  changeEconomyW10,
  changeEconomyW11,
  changeEconomyW12,
  ##
  immigCulturalW1,
  immigCulturalW2,
  immigCulturalW3,
  immigCulturalW4,
  immigCulturalW7,
  immigCulturalW8,
  immigCulturalW10,
  immigCulturalW11,
  immigCulturalW13,
  ##
  immigEconW1,
  immigEconW2,
  immigEconW3,
  immigEconW4,
  immigEconW7,
  immigEconW8,
  immigEconW10,
  immigEconW11,
  immigEconW13,
  ## EU Conduct
  expectGoodConductEURefW7,
  expectGoodConductEURefW10,
  expectGoodConductEURefW11,
  goodConductEURefW9,
  profile_eurefturnout
)


# Brexit vote intention variable --------------------------------
## Recode 9999 as missing in Brexit vote intention variables
bes <- bes %>%
  mutate(brexitW6 = ifelse(euRefVoteW6 == 9999, NA, euRefVoteW6)) %>%
  mutate(brexitW7 = ifelse(euRefVoteW7 == 9999, NA, euRefVoteW7)) %>%
  mutate(brexitW8 = ifelse(euRefVoteW8 == 9999, NA, euRefVoteW8))

## Construct Brexit vote dummy
## Because this question was asked across three pre-Brexit waves,
## I use all three variables to pick up Brexit vote intention,
## This way, if the respondent did not participate in a given wave, I can pick up their intention from another wave

bes <- bes %>%
  mutate(leave_intent = brexitW8) %>%
  mutate(leave_intent = case_when(is.na(leave_intent) ~ brexitW7, 
                                  !is.na(leave_intent) ~ leave_intent)) %>%
  mutate(leave_intent = case_when(is.na(leave_intent) ~ brexitW6, 
                                  !is.na(leave_intent) ~ leave_intent))


# Education variable --------------------------------------------
# Here, I take the mean of all education answers across waves. 
# If the value is greater than or equal to 4 (undergraduate), the dummy takes on a value of one. 
# If it is lower, I code as zero.

bes$education <- bes %>% 
  select(starts_with("edlevel")) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate(education = rowMeans(., na.rm = T)) %>%
  select(education) %>%
  mutate(education = ifelse(education >= 4, 1, 0)) %>%
  as.matrix() %>% as.vector()

# Occupation variable -------------------------------------------
## Socioeconomic classification
# Here I recode occupation to take on the most recent value of occupation
# Because there a lack of continuity in how people answer this question, I'm not sure what the best strategy is here. 
# Since it makes no sense to take a mean or mode (only 2 variables) value, 
# I take the value reported before Brexit, and if that is missing, I take the value after 

bes <- bes %>%
  mutate(occupation_cats = ns_sec_analyticW1W2W3W4W5) %>%
  mutate(occupation_cats = ifelse(is.na(occupation_cats), ns_sec_analyticW6W7W8W9, occupation_cats)) %>%
  mutate(occupation_cats = case_when(occupation_cats == 11 ~ "Employers in large organisations and higher managerial",
                                     occupation_cats == 12 ~ "Higher professional occupations",
                                     occupation_cats == 20 ~ "Lower professional and managerial and higher supervisory",
                                     occupation_cats == 30 ~ "Intermediate occupations",
                                     occupation_cats == 40 ~ "Employers in small organisations and own account workers",
                                     occupation_cats == 50 ~ "Lower supervisory and technical occupations",
                                     occupation_cats == 60 ~ "Semi-routine occupations",
                                     occupation_cats == 70 ~ "Routine occupations",
                                         TRUE ~ NA_character_))


# Income --------------------------------------------------------
## 16 is don't know, 17 is prefer not to answer
bes <- bes %>%
  mutate(income = ifelse(profile_gross_household %in% c(9999, 17, 16), NA, profile_gross_household))


# Age and gender ------------------------------------------------
## I wish that all variables were this easy
bes$age <- bes$Age
bes$gender <- bes$gender


# Left-right orientation ----------------------------------------
## Because this question was asked in multiple waves, I take the mean pre-Brexit value across waves
bes$lr <- bes %>% 
  dplyr::select(leftRightW1, leftRightW2, leftRightW3, 
                leftRightW4, leftRightW5, leftRightW6, 
                leftRightW7, leftRightW8) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse( . == 9999, NA, .))) %>%
  mutate(leftRight_multi = rowMeans(., na.rm = T)) %>%
  select(leftRight_multi) %>%
  as.matrix() %>%
  as.vector()

# Attention to politics -----------------------------------------
## I also take the mean value across pre-Brexit waves here
bes$attention <- bes %>% 
  select(polAttentionW1, polAttentionW2, polAttentionW3, 
         polAttentionW4, polAttentionW6, polAttentionW7, 
         polAttentionW8) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse( . == 9999, NA, .))) %>%
  mutate(polAttention_multi = rowMeans(., na.rm = T)) %>%
  select(polAttention_multi) %>%
  as.matrix() %>%
  as.vector()


# Party ID ------------------------------------------------------
## Recoding this variable is a little more involved
## PID was asked in all but one wave (i.e. it was asked in 12/13 waves). 
## That being the case, I took the modal pre-Brexit response for each person 

# Select party variable
pid_df <- bes %>% select(partyIdW1, partyIdW2, partyIdW3, partyIdW4, partyIdW6, partyIdW7, partyIdW8) %>%
  mutate_all(funs(as_factor)) %>%
  mutate_all(funs(as.character)) %>%
  # Paste all values together "NANANANANANANA" means all missing
  mutate(all_party = paste0(bes$partyIdW1, bes$partyIdW2, bes$partyIdW3, 
                            bes$partyIdW4, bes$partyIdW6, bes$partyIdW7, bes$partyIdW8))

# When all values are NA, this creates problems for apply, 
# Does not return a vector with the length of the dataframe
# But less, because of null values

pid_df$partyId_missing <- ifelse(pid_df$all_party == "NANANANANANANA", "Missing", NA)

pid_df <- pid_df %>%
  dplyr::select(-all_party)

pid_mode <- apply(pid_df, 1, function(x) names(which.max(table(x))))

pid_mode[pid_mode == "Missing"] <- NA
pid_mode[pid_mode == "No - none"] <- "Other/don't know/no one"
pid_mode[pid_mode == "Don't know"] <- "Other/don't know/no one"
pid_mode[pid_mode == "Other"] <- "Other/don't know/no one"

bes$pid <- pid_mode

rm(pid_df, pid_mode)

# Immigration attitudes -----------------------------------------
## Here I take the mean value across pre-Brexit waves

# Cultural
bes$immigecon <- bes %>% 
  dplyr::select(immigEconW1, immigEconW2, immigEconW3, immigEconW4, immigEconW7, immigEconW8) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse( . == 9999, NA, .))) %>%
  mutate(immigecon_multi = rowMeans(., na.rm = T)) %>%
  dplyr::select(immigecon_multi) %>%
  as.matrix() %>%
  as.vector()

bes$immigcult <- bes %>% 
  dplyr::select(immigCulturalW1, immigCulturalW2, immigCulturalW3, immigCulturalW4, immigCulturalW7, immigCulturalW8) %>%
  mutate_all(funs(as.numeric)) %>%
  mutate_all(funs(ifelse( . == 9999, NA, .))) %>%
  mutate(immigcult_multi = rowMeans(., na.rm = T)) %>%
  dplyr::select(immigcult_multi) %>%
  as.matrix() %>%
  as.vector()

# Local authority -----------------------------------------------
## Used to restrict respondents where local elections were being held (which might affect Brexit effect)
## as_factor/as.character business is to convert labelled doubles to character
bes$local_authority <- bes %>% dplyr::select(profile_oslaua) %>%
  mutate(local_authority = as_factor(profile_oslaua)) %>%
  mutate(local_authority = as.character(local_authority)) %>%
  dplyr::select(local_authority) %>%
  as.matrix() %>%
  as.vector()

# Country -------------------------------------------------------
bes <- bes %>%
  mutate(country = as.character(as_factor(country)))

