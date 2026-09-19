# EDA checking for a potential COVID-19 effect on A&E attendance

library(readr)
library(ggplot2)

# load attendance data
model_data <- read_csv("ae_attendance_rates_merged_notype.csv", show_col_types = FALSE)

# add date so the time series plots in correct order
model_data$Date <- as.Date(paste(model_data$Year, model_data$MonthNum, "01", sep = "-"))

# overall monthly attendance rate over time
monthly_agg <- aggregate(cbind(NumberOfAttendances, Population) ~ Date + Year,
                         data = model_data,
                         FUN = sum)
monthly_agg$Rate <- monthly_agg$NumberOfAttendances / monthly_agg$Population * 1000

plot_timeseries <- ggplot(monthly_agg, aes(x = Date, y = Rate)) +
  geom_line(colour = phs_colours("phs-blue"), linewidth = 0.8) +
  geom_vline(xintercept = as.Date(c("2020-03-01", "2021-06-01")), linetype = "dashed", colour = phs_colours("phs-rust")) +
  annotate("rect", 
           xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-01"),
           ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = phs_colours("phs-rust")) +
  labs(x = "Month", y = "Attendances per 1,000 population") +
  theme_minimal()
print(plot_timeseries)

# numerical check by percent change between pre and post Covid outbreak 
rate_2019 <- yearly_agg$Rate[yearly_agg$Year == 2019]
rate_2020 <- yearly_agg$Rate[yearly_agg$Year == 2020]
rate_2021 <- yearly_agg$Rate[yearly_agg$Year == 2021]
# 2019 to 2020
cat(round((rate_2020 - rate_2019) / rate_2019 * 100, 1), "%\n")
# 2020 to 2021
cat(round((rate_2021 - rate_2020) / rate_2020 * 100, 1), "%\n")