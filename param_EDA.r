# A&E attendance rate exploratory data analysis

library(readr)
library(ggplot2)
library(dplyr)
library(phsstyles)
library(tidyverse)
library(patchwork)
phs_colours()

#load and prep population attendance data 
model_data <- read_csv("ae_attendance_rates_postcovid.csv", show_col_types = FALSE)
age_levels <- c("Under 18", "18-24", "25-39", "40-64", "65-74", "75 plus")
model_data$Age <- factor(model_data$Age, levels = age_levels)
model_data$Deprivation <- factor(model_data$Deprivation, levels = 1:5)

# aggregate attendances and population across time then calculate rate
aggregate_rate <- function(data, group_vars) {
  agg <- aggregate(cbind(NumberOfAttendances, Population) ~ .,
                   data = data[, c(group_vars, "NumberOfAttendances", "Population")],
                   FUN = sum)
  agg$Rate <- agg$NumberOfAttendances / agg$Population * 1000
  return(agg)
}

# plot theme
theme_eda <- theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, colour = "grey40"))


# AGE
# population attendance rate by age
by_age <- aggregate_rate(model_data, "Age")
plot_age <- ggplot(by_age, aes(x = Age, y = Rate)) +
  geom_col(fill = phs_colours("phs-magenta")) +
  labs(title = "Age", y = "Attendances per 1,000 population", x = "Age band") +
  theme_eda
print(plot_age)

#SEX
# population attendance rate by sex
by_sex <- aggregate_rate(model_data, "Sex")
plot_sex <- ggplot(by_sex, aes(x = Sex, y = Rate)) +
  geom_col(fill = phs_colours("phs-magenta")) +
  labs(title = "Sex", y = "Attendances per 1,000 population", x = "Sex") +
  theme_eda
print(plot_sex)

#DEPRIVATION
#population attendance rate by deprivation
by_deprivation <- aggregate_rate(model_data, "Deprivation")
plot_deprivation <- ggplot(by_deprivation, aes(x = Deprivation, y = Rate)) +
  geom_col(fill = phs_colours("phs-magenta")) +
  labs(title = "Deprivation", y = "Attendances per 1,000 population", x = "SIMD Quintile") +
  theme_eda
print(plot_deprivation)

#HEALTH BOARD
#population attendance rate by health board
by_hb <- aggregate_rate(model_data, "HBT")
plot_hb <- ggplot(by_hb, aes(x = HBT, y = Rate)) +
  geom_col(fill = phs_colours("phs-magenta")) +
  coord_flip() +
  labs(title = "Health Board",y = "Attendances per 1,000 population", x = "Health Board") +
  theme_eda
print(plot_hb)

combined_maineffects <- (plot_age|plot_sex)/(plot_deprivation|plot_hb)
print(combined_maineffects)



# INTERACTIONS
# Age x Sex
# diverging lines, evidence for an interaction
agg <- aggregate(cbind(NumberOfAttendances, Population) ~ Age + Sex, data = model_data, FUN = sum)
agg$Rate <- agg$NumberOfAttendances / agg$Population * 1000
plot_age_sex <- ggplot(agg, aes(x = Age, y = Rate, colour = Sex, group = Sex)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title = "Age x Sex", y = "Attendances per 1,000 population", x = "Age band") +
  scale_colour_manual(values = c("Male" = phs_colours("phs-blue"), "Female" = phs_colours("phs-rust"))) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text = element_text(size = 12), axis.title = element_text(size = 12))
print(plot_age_sex)

# Sex x Deprivation
# parallel lines, little to no interaction
agg <- aggregate(cbind(NumberOfAttendances, Population) ~ Sex + Deprivation, data = model_data, FUN = sum)
agg$Rate <- agg$NumberOfAttendances / agg$Population * 1000
plot_dep_sex <- ggplot(agg, aes(x = Deprivation, y = Rate, colour = Sex, group = Sex)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title = "Deprivation x Sex", y = "Attendances per 1,000 population", x = "SIMD Quintile") +
  scale_colour_manual(values = c("Male" = phs_colours("phs-blue"), "Female" = phs_colours("phs-rust"))) +
  theme_minimal()+
  theme(legend.position = "bottom", axis.text = element_text(size = 12), axis.title = element_text(size = 12))
print(plot_dep_sex)

# Age x Deprivation
# diverging lines, evidence for an interaction
agg <- aggregate(cbind(NumberOfAttendances, Population) ~ Age + Deprivation, data = model_data, FUN = sum)
agg$Rate <- agg$NumberOfAttendances / agg$Population * 1000
plot_age_dep<-ggplot(agg, aes(x = Deprivation, y = Rate, colour = Age, group = Age)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  labs(title = "Deprivation x Age", y = "Attendances per 1,000 population", x = "SIMD Quintile") +
  scale_colour_manual(values = c("Under 18" = phs_colours("phs-blue"), "18-24" = phs_colours("phs-green"),
                                 "25-39" = phs_colours("phs-graphite"), "40-64" = phs_colours("phs-magenta"),
                                 "65-74" = phs_colours("phs-rust"), "75 plus" = phs_colours("phs-teal"))) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text = element_text(size = 12), axis.title = element_text(size = 12)) 
# guides(colour = guide_legend(nrow = 1))
print(plot_age_dep)
# print(ggplot(agg, aes(x = Age, y = Rate, colour = Deprivation, group = Deprivation)) +
#         geom_line(linewidth = 1) + geom_point(size = 2) +
#         labs(title = "Age x Deprivation", y = "Attendances per 1,000 population", x = "Age band") +
#         scale_colour_manual(values = c("1" = phs_colours("phs-blue"), "2" = phs_colours("phs-green"),
#                                "3" = phs_colours("phs-graphite"), "4" = phs_colours("phs-magenta"),
#                                "5" = phs_colours("phs-rust"))) +
#         theme_eda+
#         theme(legend.position = "bottom"))


# Deprivation x HBT
# use aes(x = reorder(HBT, Rate) instead of just x=HBT if you want it ordered by HBTs attendance rates from low to high
agg <- aggregate(cbind(NumberOfAttendances, Population) ~ Deprivation + HBT, data = model_data, FUN = sum)
agg$Rate <- agg$NumberOfAttendances / agg$Population * 1000
# ggplot(agg, aes(x = Deprivation, y = Rate, group = HBT, colour = HBT)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 1.5) +
#   labs(title = "Deprivation by Health Board", x = "SIMD Quintile", y = "Attendances per 1,000 population", colour = "Health Board") +
#   theme_minimal()
# plot_dep_hbt <- ggplot(agg, aes(x = HBT, y = Rate, group = Deprivation, colour = Deprivation)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 1.5) +
#   labs(title = "Health Board x Deprivation", x = "Health Board", y = "Attendances per 1,000 population", colour = "SIMD Quintile") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   scale_colour_manual(values = c("1" = phs_colours("phs-blue"), "2" = phs_colours("phs-green"),
#                                  "3" = phs_colours("phs-graphite"), "4" = phs_colours("phs-magenta"),
#                                  "5" = phs_colours("phs-rust"))) +
#   theme(axis.text.x = element_text(angle = 55, hjust = 1))
plot_dep_hbt <- ggplot(agg, aes(x = Rate, y = HBT, group = HBT)) +
  geom_line(colour = "grey", linewidth = 0.8) +
  geom_point(aes(colour = Deprivation), size = 3) +
  labs(title = "Health Board x Deprivation", x = "Attendances per 1,000 population", y = "Health Board", colour = "SIMD Quintile") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("1" = phs_colours("phs-blue"), "2" = phs_colours("phs-green"),
                                 "3" = phs_colours("phs-magenta"), "4" = phs_colours("phs-graphite"),
                                 "5" = phs_colours("phs-rust")))
print(plot_dep_hbt)



# Age x HBT
# use aes(x = reorder(HBT, Rate) in stead of just x=HBT if you want it ordered by HBTs attendance rates from low to high
agg <- aggregate(cbind(NumberOfAttendances, Population) ~ Age + HBT, data = model_data, FUN = sum)
agg$Rate <- agg$NumberOfAttendances / agg$Population * 1000
# ggplot(agg, aes(x = Age, y = Rate, group = HBT, colour = HBT)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 1.5) +
#   labs(title = "Age Pattern in Attendance Rate, by Health Board",
#        x = "Age band", y = "Attendances per 1,000 population", colour = "Health Board") +
#   theme_minimal()
# plot_age_hbt <- ggplot(agg, aes(x = HBT, y = Rate, group = Age, colour = Age)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 1.5) +
#   labs(title = "Health Board x Age", x = "Health Board", y = "Attendances per 1,000 population", colour = "Age") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   scale_colour_manual(values = c("Under 18" = phs_colours("phs-blue"), "18-24" = phs_colours("phs-green"),
#                                  "25-39" = phs_colours("phs-graphite"), "40-64" = phs_colours("phs-magenta"),
#                                  "65-74" = phs_colours("phs-rust"), "75 plus" = phs_colours("phs-teal"))) +
#   theme(axis.text.x = element_text(angle = 55, hjust = 1))
plot_age_hbt <- ggplot(agg, aes(x = Rate, y = HBT, group = HBT)) +
  geom_line(colour = "grey", linewidth = 0.8) +
  geom_point(aes(colour = Age), size = 3) +
  labs(title = "Health Board x Age", x = "Attendances per 1,000 population", y = "Health Board", colour = "Age") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("Under 18" = phs_colours("phs-blue"), "18-24" = phs_colours("phs-green"),
                                 "25-39" = phs_colours("phs-graphite"), "40-64" = phs_colours("phs-magenta"),
                                 "65-74" = phs_colours("phs-rust"), "75 plus" = phs_colours("phs-teal")))
print(plot_age_hbt)


# sex x HBT
# use aes(x = reorder(HBT, Rate) in stead of just x=HBT if you want it ordered by HBTs attendance rates from low to high
agg <- aggregate(cbind(NumberOfAttendances, Population) ~ Sex + HBT, data = model_data_post, FUN = sum)
agg$Rate <- agg$NumberOfAttendances / agg$Population * 1000
# plot_sex_hbt <- ggplot(agg, aes(x = HBT, y = Rate, group = Sex, colour = Sex)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 1.5) +
#   labs(title = "Health Board x Sex", x = "Health Board", y = "Attendances per 1,000 population", colour = "Sex") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   scale_colour_manual(values = c("Male" = phs_colours("phs-blue"), "Female" = phs_colours("phs-rust"))) +
#   theme(axis.text.x = element_text(angle = 55, hjust = 1))
plot_sex_hbt <- ggplot(agg, aes(x = Rate, y = HBT, group = HBT)) +
  geom_line(colour = "grey", linewidth = 0.8) +
  geom_point(aes(colour = Sex), size = 3) +
  labs(title = "Health Board x Sex", x = "Attendances per 1,000 population", y = "Health Board", colour = "Sex") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("Male" = phs_colours("phs-blue"), "Female" = phs_colours("phs-rust")))
print(plot_sex_hbt)


combined_interactions_a <- (plot_age_sex|plot_dep_sex)
print(combined_interactions_a)
combined_interactions_b <- (plot_age_dep|plot_dep_hbt)
print(combined_interactions_b)
combined_interactions_c <- (plot_age_hbt|plot_sex_hbt)
print(combined_interactions_c)