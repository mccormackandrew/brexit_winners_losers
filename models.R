library(multiwayvcov)
library(lmtest)
library(texreg)

# Remove "I would/will not vote"
bes_long <- subset(bes_long, leave != 2)

# Formula
form <-
  as.formula(
    ~ brexit * leave + income + age + I(age ^ 2) + education + lr +
      gender + country + factor(pid) + factor(occupation) + attention +
      immigcult + immigecon
  )

# Model function
brexit_lm <- function(dv, data) {
  lm_form <- paste0(dv, c(form))
  lm(lm_form,
     data = data, weights = weight)
}

# Cluster standard errors by individual, to be used in regression output
cluster_errors <- function(model, data) {
  vcv <- cluster.vcov(model, as.data.frame(data$number))
  coefs <- coeftest(model, vcv)
  return(broom::tidy(coefs))
}

# Clustered VCV, to be used in marginal effects plot
cluster_vcv <- function(model, data) {
  vcv <- cluster.vcov(model, as.data.frame(data$number))
  return(vcv)
}

# Wave 8 and 9 --------------------------------------------------
## Short term effect
dv_list_short <- c(
  "satdem",
  "euconduct",
  "economybetter",
  "riskpoverty",
  "efficacypolcare",
  "efficacyunderstand"
)

models_shortterm <- list()
models_shortterm_se <- list()
models_shortterm_vcv <- list()


for (i in 1:length(dv_list_short)) {
  if (dv_list_short[i] == "euconduct") {
    # euconduct question not asked in wave 8
    # MODELS
    models_shortterm[[i]] <- brexit_lm(dv_list_short[i],
                                       subset(bes_long, wave == 7 |
                                                wave == 9))
    names(models_shortterm)[i] <- dv_list_short[i]
    # CLUSTERED SEs
    models_shortterm_se[[i]] <-
      cluster_errors(models_shortterm[[i]], subset(bes_long, wave == 7 |
                                                     wave == 9))
    names(models_shortterm_se)[i] <- dv_list_short[i]
    models_shortterm_vcv[[i]] <-
      cluster_vcv(models_shortterm[[i]], subset(bes_long, wave == 7 |
                                                     wave == 9))
    names(models_shortterm_vcv)[i] <- dv_list_short[i]
  } else {
    # MODELS
    models_shortterm[[i]] <- brexit_lm(dv_list_short[i],
                                       subset(bes_long, wave == 8 |
                                                wave == 9))
    names(models_shortterm)[i] <- dv_list_short[i]
    # CLUSTERED SEs
    models_shortterm_se[[i]] <-
      cluster_errors(models_shortterm[[i]], subset(bes_long, wave == 8 |
                                                     wave == 9))
    names(models_shortterm_se)[i] <- dv_list_short[i]
    models_shortterm_vcv[[i]] <-
      cluster_vcv(models_shortterm[[i]], subset(bes_long, wave == 8 |
                                                     wave == 9))
    names(models_shortterm_vcv)[i] <- dv_list_short[i]
  }
}


# Wave 8 and 10 --------------------------------------------------
## Six month effect

dv_list_6mo <- c(
  "satdem",
  "euconduct",
  "economybetter",
  "econgenretro",
  "riskpoverty",
  "trustmps",
  "efficacypolcare",
  "efficacyunderstand"
)

models_6mo <- list()
models_6mo_se <- list()
models_6mo_vcv <- list()

for (i in 1:length(dv_list_6mo)) {
  if (dv_list_6mo[i] == "euconduct") {
    # euconduct question not asked in wave 8
    models_6mo[[i]] <- brexit_lm(dv_list_6mo[i],
                                 subset(bes_long, wave == 7 |
                                          wave == 10))
    names(models_6mo)[i] <- dv_list_6mo[i]
    # CLUSTERED SEs
    models_6mo_se[[i]] <-
      cluster_errors(models_6mo[[i]], subset(bes_long, wave == 7 |
                                               wave == 10))
    names(models_6mo_se)[i] <- dv_list_6mo[i]
    models_6mo_vcv[[i]] <-
      cluster_vcv(models_6mo[[i]], subset(bes_long, wave == 7 |
                                               wave == 10))
    names(models_6mo_vcv)[i] <- dv_list_6mo[i]
  } else {
    models_6mo[[i]] <- brexit_lm(dv_list_6mo[i],
                                 subset(bes_long, wave == 8 |
                                          wave == 10))
    names(models_6mo)[i] <- dv_list_6mo[i]
    models_6mo_se[[i]] <-
      cluster_errors(models_6mo[[i]], subset(bes_long, wave == 8 |
                                               wave == 10))
    names(models_6mo_se)[i] <- dv_list_6mo[i]
    models_6mo_vcv[[i]] <-
      cluster_vcv(models_6mo[[i]], subset(bes_long, wave == 8 |
                                               wave == 10))
    names(models_6mo_vcv)[i] <- dv_list_6mo[i]
  }
}

# Wave 8 and 11 --------------------------------------------------
## Ten month effect

dv_list_10mo <- c(
  "satdem",
  "euconduct",
  "economybetter",
  "econgenretro",
  "riskpoverty",
  "trustmps",
  "efficacypolcare",
  "efficacyunderstand"
)

models_10mo <- list()
models_10mo_se <- list()
models_10mo_vcv <- list()

for (i in 1:length(dv_list_10mo)) {
  if (dv_list_10mo[i] == "euconduct") {
    # euconduct question not asked in wave 8
    models_10mo[[i]] <- brexit_lm(dv_list_10mo[i],
                                  subset(bes_long, wave == 7 |
                                           wave == 11))
    names(models_10mo)[i] <- dv_list_10mo[i]
    # CLUSTERED SEs
    models_10mo_se[[i]] <-
      cluster_errors(models_10mo[[i]], subset(bes_long, wave == 7 |
                                                wave == 11))
    names(models_10mo_se)[i] <- dv_list_10mo[i]
    models_10mo_vcv[[i]] <-
      cluster_vcv(models_10mo[[i]], subset(bes_long, wave == 7 |
                                                wave == 11))
    names(models_10mo_vcv)[i] <- dv_list_10mo[i]
  } else if (dv_list_10mo[i] == "trustmps") {
    models_10mo[[i]] <- brexit_lm(dv_list_10mo[i],
                                  subset(bes_long, wave == 8 |
                                           wave == 12))
    names(models_10mo)[i] <- dv_list_10mo[i]
    # CLUSTERED SEs
    models_10mo_se[[i]] <-
      cluster_errors(models_10mo[[i]], subset(bes_long, wave == 8 |
                                                wave == 12))
    names(models_10mo_se)[i] <- dv_list_10mo[i]
    models_10mo_vcv[[i]] <-
      cluster_vcv(models_10mo[[i]], subset(bes_long, wave == 8 |
                                                wave == 12))
    names(models_10mo_vcv)[i] <- dv_list_10mo[i]
  } else {
    models_10mo[[i]] <- brexit_lm(dv_list_10mo[i],
                                  subset(bes_long, wave == 8 |
                                           wave == 11))
    names(models_10mo)[i] <- dv_list_10mo[i]
    # CLUSTERED SEs
    models_10mo_se[[i]] <-
      cluster_errors(models_10mo[[i]], subset(bes_long, wave == 8 |
                                                wave == 11))
    names(models_10mo_se)[i] <- dv_list_10mo[i]
    models_10mo_vcv[[i]] <-
      cluster_vcv(models_10mo[[i]], subset(bes_long, wave == 8 |
                                                wave == 11))
    names(models_10mo_vcv)[i] <- dv_list_10mo[i]
  }
}

## Produce regression output for latex

texreg(
  models_shortterm,
  override.se = lapply(models_shortterm_se, function(x)
    x$std.error),
  override.pvalues = lapply(models_shortterm_se, function(x)
    x$p.value),
  custom.coef.names = c(
    "Constant",
    "Brexit",
    "Leave",
    "Income",
    "Age",
    "Age^2",
    "Education",
    "Ideology",
    "Gender",
    "Scotland",
    "Wales",
    "Conservative",
    "Green",
    "Labour",
    "Liberal Democrat",
    "Other/don't know/none",
    "Plaid Cyrmu",
    "Scottish National Party",
    "UKIP",
    "Routine/semi-routine occupation",
    "Attention to politics",
    "Immigration attitudes (cultural)",
    "Immigration attitudes (economic)",
    "Brexit X leave"
  ),
  digits = 3
)

texreg(
  models_6mo,
  override.se = lapply(models_6mo_se, function(x)
    x$std.error),
  override.pvalues = lapply(models_6mo_se, function(x)
    x$p.value),
  custom.coef.names = c(
    "Constant",
    "Brexit",
    "Leave",
    "Income",
    "Age",
    "Age^2",
    "Education",
    "Ideology",
    "Gender",
    "Scotland",
    "Wales",
    "Conservative",
    "Green",
    "Labour",
    "Liberal Democrat",
    "Other/don't know/none",
    "Plaid Cyrmu",
    "Scottish National Party",
    "UKIP",
    "Routine/semi-routine occupation",
    "Attention to politics",
    "Immigration attitudes (cultural)",
    "Immigration attitudes (economic)",
    "Brexit X leave"
  ),
  digits = 3
)

texreg(
  models_10mo,
  override.se = lapply(models_10mo_se, function(x)
    x$std.error),
  override.pvalues = lapply(models_10mo_se, function(x)
    x$p.value),
  custom.coef.names = c(
    "Constant",
    "Brexit",
    "Leave",
    "Income",
    "Age",
    "Age^2",
    "Education",
    "Ideology",
    "Gender",
    "Scotland",
    "Wales",
    "Conservative",
    "Green",
    "Labour",
    "Liberal Democrat",
    "Other/don't know/none",
    "Plaid Cyrmu",
    "Scottish National Party",
    "UKIP",
    "Routine/semi-routine occupation",
    "Attention to politics",
    "Immigration attitudes (cultural)",
    "Immigration attitudes (economic)",
    "Brexit X leave"
  ),
  digits = 3
)



## Interaction plots

margeff_shortterm <- list()

for(i in 1:length(models_shortterm)) {
  margeff_shortterm[[i]] <- interaction_plot_binary(models_shortterm[[i]], effect = "brexit",
                          moderator = "leave", interaction = "brexit:leave",
                          models_shortterm_vcv[[i]])
  margeff_shortterm[[i]] <- data.frame(margeff_shortterm[[i]])
  margeff_shortterm[[i]]$variable <- rep(dv_list_short[i], 2)
  margeff_shortterm[[i]]$time <- rep("Short-term impact", 2)
}

margeff_shortterm <- do.call("rbind", margeff_shortterm)

margeff_6mo <- list()

for(i in 1:length(models_6mo)) {
  margeff_6mo[[i]] <- interaction_plot_binary(models_6mo[[i]], effect = "brexit",
                                                    moderator = "leave", interaction = "brexit:leave",
                                                    models_6mo_vcv[[i]])
  margeff_6mo[[i]] <- data.frame(margeff_6mo[[i]])
  margeff_6mo[[i]]$variable <- rep(dv_list_6mo[i], 2)
  margeff_6mo[[i]]$time <- rep("Six months after", 2)
}

margeff_6mo <- do.call("rbind", margeff_6mo)

margeff_10mo <- list()

for(i in 1:length(models_10mo)) {
  margeff_10mo[[i]] <- interaction_plot_binary(models_10mo[[i]], effect = "brexit",
                                              moderator = "leave", interaction = "brexit:leave",
                                              models_10mo_vcv[[i]])
  margeff_10mo[[i]] <- data.frame(margeff_10mo[[i]])
  margeff_10mo[[i]]$variable <- rep(dv_list_10mo[i], 2)
  margeff_10mo[[i]]$time <- rep("Ten months after", 2)
}

margeff_10mo <- do.call("rbind", margeff_10mo)

margeff_all <- rbind(margeff_shortterm, margeff_6mo, margeff_10mo)


margeff_all$variable_name <- dplyr::recode(margeff_all$variable,
                                           `satdem` = "Satisfaction with democracy",
                                           `euconduct` = "Brexit conducted fairly",
                                           `economybetter` = "Economy getting better",
                                           `econgenretro` = "Retrospective sociotropic",
                                           `riskpoverty` = "Risk of poverty",
                                           `trustmps` = "Trust in MPs",
                                           `efficacypolcare` = "Politicians care about \n people like me",
                                           `efficacyunderstand` = "I understand the issues \n facing this country")


margeff_all$variable_name <- factor(margeff_all$variable_name, levels = 
                                      c("I understand the issues \n facing this country",
                                        "Politicians care about \n people like me",
                                        "Trust in MPs",
                                        "Risk of poverty",
                                        "Retrospective sociotropic",
                                        "Economy getting better",
                                        "Brexit conducted fairly",
                                        "Satisfaction with democracy"))

margeff_plot <- ggplot(data = margeff_all, aes(variable_name, y = y,fill = factor(x), width=20)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound, color = factor(x)), position = position_dodge(width = c(0.4)), width = 0.3) +
  geom_point(aes(color = factor(x), shape = factor(x)), position = position_dodge(width = c(0.4)), size = 2) +
  scale_colour_manual(values = c("gray60", "black"),
                      labels = c("Remain","Leave"),
                      name = "") +
  scale_shape_manual(values=c(1,16),
                     labels = c("Remain","Leave"),
                     name = "") +
  scale_fill_manual(values=c(1,16),
                    labels = c("Remain","Leave"),
                    name = "") +
  theme_linedraw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick") +
  coord_flip() +
  ylim(-0.35, 0.35) +
  ylab("Average marginal effect of Brexit") +
  xlab("") +
  facet_wrap(~time, strip.position = "bottom") +
  theme(legend.position = "bottom")

ggsave("tex/margeff_plot.png", margeff_plot)
