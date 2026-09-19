# modeled rates

library(readr)
library(MASS)
library(ggplot2)
library(phsstyles)

model_data <- read_csv("ae_attendance_rates_postcovid.csv", show_col_types = FALSE)
model_data$Age <- factor(model_data$Age, levels = c("Under 18", "18-24", "25-39", "40-64", "65-74", "75 plus"))
model_data$Deprivation <- factor(model_data$Deprivation, levels = 1:5)
model_data$Sex <- factor(model_data$Sex)
model_data$HBT <- factor(model_data$HBT)

# reference levels for factors
ref_age <- levels(model_data$Age)[1]
ref_sex <- levels(model_data$Sex)[1]
ref_dep <- levels(model_data$Deprivation)[1]
ref_hbt <- levels(model_data$HBT)[1]

#final model
model_c <- glm.nb(NumberOfAttendances ~ (Age + Sex + Deprivation + HBT)^2 + offset(log_offset), data = model_data)

# population mean predicted rate, for each level of each factor
model_data$FittedRate <- predict(model_c, type = "response") / model_data$Population * 1000
marginal_age <- aggregate(FittedRate ~ Age, data = model_data, FUN = mean)
marginal_sex <- aggregate(FittedRate ~ Sex, data = model_data, FUN = mean)
marginal_dep <- aggregate(FittedRate ~ Deprivation, data = model_data, FUN = mean)
marginal_hbt <- aggregate(FittedRate ~ HBT, data = model_data, FUN = mean)
marginal_age
marginal_sex
marginal_dep
marginal_hbt


# top and bottom specific sub populations
subpop_rates <- aggregate(cbind(NumberOfAttendances, Population) ~ Age + Sex + Deprivation + HBT, data = model_data, FUN = sum)
subpop_rates$Rate <- subpop_rates$NumberOfAttendances / subpop_rates$Population * 1000
top10 <- subpop_rates[order(-subpop_rates$Rate), ][1:10, ]
bottom10 <- subpop_rates[order(subpop_rates$Rate), ][1:10, ]
#top 10, highest risk sub-populations
print(top10[, c("Age", "Sex", "Deprivation", "HBT", "Rate", "Population")])
#bottom 10, lowest risk sub-populations
print(bottom10[, c("Age", "Sex", "Deprivation", "HBT", "Rate", "Population")])



# plots for modeled rates
#Age x Sex
grid <- expand.grid(Age = levels(model_data$Age), Sex = levels(model_data$Sex), Deprivation = ref_dep, HBT = ref_hbt)
grid$log_offset <- log(1000)
grid$PredictedRate <- predict(model_c, newdata = grid, type = "response")
plt_age_sex <- ggplot(grid, aes(x = Age, y = PredictedRate, fill = Sex)) +
  geom_col(position = "dodge") +
  labs(title = "Age x Sex", y = "Predicted attendances per 1,000 population", x = "Age band") +
  scale_fill_manual(values = c("Male" = phs_colours("phs-green"), "Female" = phs_colours("phs-magenta"))) +
  theme_minimal()+
  # theme(legend.position = "none", axis.text.x = element_text(size = 10, vjust = 2))
  theme(axis.text.x = element_text(size = 12))
print(plt_age_sex)

# Sex x Deprivation
grid <- expand.grid(Age = ref_age, Sex = levels(model_data$Sex), Deprivation = levels(model_data$Deprivation), HBT = ref_hbt)
grid$log_offset <- log(1000)
grid$PredictedRate <- predict(model_c, newdata = grid, type = "response")
plt_dep_sex<-ggplot(grid, aes(x = Deprivation, y = PredictedRate, fill = Sex)) +
  geom_col(position = "dodge") +
  labs(title = "Sex x Deprivation", y = "Predicted attendances per 1,000 population", x = "SIMD Quintile") +
  scale_fill_manual(values = c("Male" = phs_colours("phs-green"), "Female" = phs_colours("phs-magenta"))) +
  theme_minimal() + 
  # theme(legend.position = "None", axis.text.x = element_text(size = 10, vjust = 2))
  theme(axis.text.x = element_text(size = 10, vjust = 2))
print(plt_dep_sex)


# Age x Deprivation
grid <- expand.grid(Age = levels(model_data$Age), Sex = ref_sex, Deprivation = levels(model_data$Deprivation), HBT = ref_hbt)
grid$log_offset <- log(1000)
grid$PredictedRate <- predict(model_c, newdata = grid, type = "response")
plt_age_dep<-ggplot(grid, aes(x = Age, y = PredictedRate, fill = Deprivation)) +
        geom_col(position = "dodge") +
        labs(title = "Age x Deprivation", y = "Predicted attendances per 1,000 population", x = "Age band", fill = "SIMD\nQuintile") +
        # scale_fill_manual(values = c("1" = phs_colours("phs-blue"), "2" = phs_colours("phs-rust"),
        #                                "3" = phs_colours("phs-graphite"), "4" = phs_colours("phs-green"),
        #                                "5" = phs_colours("phs-magenta"))) +
        scale_fill_manual(values = c("1" = "#6A0361", "2" = "#B717AA",
                                     "3" = "#A27365", "4" = "#517911",
                                     "5" = "#8CCF1F")) +
        # scale_fill_viridis_d(option = "viridis")+
        theme_minimal()+
        theme(axis.text.x = element_text(size=12))
print(plt_age_dep)

# Dep x HBT
grid <- expand.grid(Age = ref_age, Sex = ref_sex, Deprivation = levels(model_data$Deprivation), HBT = levels(model_data$HBT))
grid$log_offset <- log(1000)
grid$PredictedRate <- predict(model_c, newdata = grid, type = "response")
# remove those quintile/HBT combos that were NA coefficient since there was no existing population
grid <- grid[!(grid$HBT == "S08000025" & grid$Deprivation %in% c(4, 5)) &
               !(grid$HBT == "S08000026" & grid$Deprivation %in% c(4, 5)) &
               !(grid$HBT == "S08000028" & grid$Deprivation %in% c(3, 4, 5)), ]
print(ggplot(grid, aes(x = Deprivation, y = PredictedRate, fill = Deprivation)) +
        geom_col() +
        facet_wrap(~ HBT, ncol = 7) +
        labs(title = "Health Board x Deprivation",x = "SIMD Quintile", y = "Predicted attendances per 1,000 population") +
        theme_minimal(base_size = 12) +
        # scale_fill_manual(values = c("1" = phs_colours("phs-blue"), "2" = phs_colours("phs-rust"),
        #                              "3" = phs_colours("phs-graphite"), "4" = phs_colours("phs-green"),
        #                              "5" = phs_colours("phs-magenta"))) +
        scale_fill_viridis_d(option = "viridis")+
        theme(legend.position = "none", axis.text.x = element_text(size = 10)))

# Age x HBT
grid <- expand.grid(Age = levels(model_data$Age), Sex = ref_sex, Deprivation = ref_dep, HBT = levels(model_data$HBT))
grid$log_offset <- log(1000)
grid$PredictedRate <- predict(model_c, newdata = grid, type = "response")
print(ggplot(grid, aes(x = Age, y = PredictedRate, fill = Age)) +
        geom_col() +
        facet_wrap(~ HBT, ncol = 7) +
        labs(title = "Health Board x Age", x = "Age band", y = "Predicted attendances per 1,000 population") +
        theme_minimal(base_size = 12) +
        # scale_fill_manual(values = c("Under 18" = phs_colours("phs-blue"), "18-24" = phs_colours("phs-green"),
        #                              "25-39" = phs_colours("phs-graphite"), "40-64" = phs_colours("phs-magenta"),
        #                              "65-74" = phs_colours("phs-rust"), "75 plus" = phs_colours("phs-teal"))) +
        scale_fill_viridis_d(option = "viridis")+
        theme(legend.position = "none", axis.text.x = element_text(angle = 70, hjust = 1.15, vjust = 1.2, size = 10)))

#Sex x HBT
grid <- expand.grid(Age = ref_age, Sex = levels(model_data$Sex), Deprivation = ref_dep, HBT = levels(model_data$HBT))
grid$log_offset <- log(1000)
grid$PredictedRate <- predict(model_c, newdata = grid, type = "response")
plt_hbt_sex<-ggplot(grid, aes(x = HBT, y = PredictedRate, fill = Sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Male" = phs_colours("phs-green"), "Female" = phs_colours("phs-magenta"))) +
  labs(title = "Sex x Health Board", y = "Predicted attendances per 1,000 population", x = "Health Board", fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1.2, size=10))
print(plt_hbt_sex)

combined_model_interactions_a <- ((plt_age_sex|plt_dep_sex)/plt_hbt_sex)
print(combined_model_interactions_a)
combined_model_interactions_b <- ((plt_age_sex|plt_dep_sex)/(plt_hbt_sex|plt_age_dep))
print(combined_model_interactions_b)
