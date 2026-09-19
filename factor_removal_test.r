# investigate when removing factors and their interactions how much effect each has on model fit

library(readr)
library(MASS)

# load and prep data
model_data <- read_csv("ae_attendance_rates_postcovid.csv", show_col_types = FALSE)
model_data$Age <- factor(model_data$Age, levels = c("Under 18", "18-24", "25-39", "40-64", "65-74", "75 plus"))
model_data$Deprivation <- factor(model_data$Deprivation, levels = 1:5)
model_data$Sex <- factor(model_data$Sex)
model_data$HBT <- factor(model_data$HBT)

# Full model
model_full <- glm.nb(NumberOfAttendances ~ (Age + Sex + Deprivation + HBT)^2 + offset(log_offset), data = model_data)
AIC(model_full)
summary(model_full)

# reduced model where Age and its interactions removed (Age, Age:Sex, Age:Deprivation, Age:HBT)
model_no_age <- glm.nb(NumberOfAttendances ~ Sex + Deprivation + HBT + Sex:Deprivation + Sex:HBT + Deprivation:HBT + offset(log_offset), data = model_data)
AIC(model_no_age)
anova(model_no_age, model_full, test = "Chisq")

# reduced model where Sex and its interactions removed removed (Sex, Age:Sex, Sex:Deprivation, Sex:HBT)
model_no_sex <- glm.nb(NumberOfAttendances ~ Age + Deprivation + HBT + Age:Deprivation + Age:HBT + Deprivation:HBT + offset(log_offset), data = model_data)
AIC(model_no_sex)
anova(model_no_sex, model_full, test = "Chisq")

# reduced model where Deprivation and its interactions removed (Dep, Age:Dep, Sex:Dep, Dep:HBT)
model_no_dep <- glm.nb(NumberOfAttendances ~ Age + Sex + HBT + Age:Sex + Age:HBT + Sex:HBT + offset(log_offset), data = model_data)
AIC(model_no_dep)
anova(model_no_dep, model_full, test = "Chisq")

# reduced model where HBT and its interactions removed removed (HBT, Age:HBT, Sex:HBT, Deprivation:HBT)
model_no_hbt <- glm.nb(NumberOfAttendances ~ Age + Sex + Deprivation + Age:Sex + Age:Deprivation + Sex:Deprivation + offset(log_offset), data = model_data)
AIC(model_no_hbt)
anova(model_no_hbt, model_full, test = "Chisq")

# drop1 applied to just look at interaction removal, main effects remain in models
print(drop1(model_c, test = "Chisq"))
