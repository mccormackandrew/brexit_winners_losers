# Descriptive statistics
## Un-select variables for which numeric descriptives are not relevant
bes_desc <- bes_long %>%
  dplyr::select(-c(occupation_cats, pid, local_authority, 
                   country, number, X, local2014, local2015, 
                   local2016, local2017, wave_num, brexit, weight, leave)) %>%
  filter(leave_intent != 2, wave < 9) %>%
  mutate(gender = ifelse(gender == 1, 0, 1))
         
bes_desc$leave_intent <- bes_desc$leave_intent %>% dplyr::recode(`1` = "Leave",
                                                                 `0` = "Remain")

bes_desc_ttest <- bes_desc %>%
  gather(key = variable, value = value, -leave_intent) %>%
  group_by(leave_intent, variable) %>%
  summarise(value = list(value)) %>%
  spread(leave_intent, value) %>%
  group_by(variable) %>%
  mutate(p_value = t.test(unlist(Leave), unlist(Remain))$p.value,
         t_value = t.test(unlist(Leave), unlist(Remain))$statistic) %>%
  select(t_value, p_value)
bes_desc_ttest

# Create data frame with variable column
bes_desc_table <- data.frame(variable = names(bes_desc))
# Make the column character, not factor (for merging)
bes_desc_table$variable <- as.character(bes_desc_table$variable)


# Calculate mean and variance of leavers and remainers
bes_desc_table$mean_leave <- sapply(bes_desc[bes_desc$leave_intent == "Leave", ], mean, na.rm = T)
bes_desc_table$mean_remain <- sapply(bes_desc[bes_desc$leave_intent == "Remain", ], mean, na.rm = T)
bes_desc_table$sd_leave <- sapply(bes_desc[bes_desc$leave_intent == "Leave", ], sd, na.rm = T)
bes_desc_table$sd_remain <- sapply(bes_desc[bes_desc$leave_intent == "Remain", ], sd, na.rm = T)

# Calculate n of leavers and remainers
bes_desc_table <- bes_desc %>%
  summarise_all(funs(`/n_leave` = length(na.omit(.[which(leave_intent == "Leave")])),
                     `/n_remain` = length(na.omit(.[which(leave_intent == "Remain")])))) %>%
  gather() %>%
  separate(key, into = c("variable", "stat"), sep = "_/") %>%
  spread(stat, value) %>%
  left_join(bes_desc_table)

bes_desc_table <- bes_desc_table %>%
  inner_join(bes_desc_ttest) %>%
  mutate(diff = mean_leave - mean_remain)

bes_desc_table2 <- bes_desc_table %>%
  select(variable, mean_leave, sd_leave, n_leave, mean_remain, sd_remain, n_remain, diff, p_value)

bes_desc_table
library(xtable)
print(xtable(bes_desc_table2), include.rownames = F)
