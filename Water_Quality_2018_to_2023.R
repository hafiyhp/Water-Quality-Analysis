# Read CSV and format 'Date' in a recognized date format for each Date(num)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(rstatix)
library(mgcv)

# Water Quality 2023 is in separate csv file  due to data collection not fully completed as of today

# Read CSV and format 'Date' in a recognized date format for each Date(num)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(rstatix)
library(mgcv)
library(stats)


# Read csv and change the format of Date_(year) into a correct format

water_quality_2023 <- read.csv("water_quality_cabrach_2023.csv")
water_quality_2018_2022 <- read.csv("cabrach_2018_2022_new.csv")
View(water_quality_2023)
View(water_quality_2018_2022)
water_quality_2023$Date_2023 <- as.Date(water_quality_2023$Date_2023, format = "%d/%m/%Y")
water_quality_2018_2022$Date_2022 <- as.Date(water_quality_2018_2022$Date_2022, format = "%d/%m/%Y")
water_quality_2018_2022$Date_2021 <- as.Date(water_quality_2018_2022$Date_2021, format = "%d/%m/%Y")
water_quality_2018_2022$Date_2020 <- as.Date(water_quality_2018_2022$Date_2020, format = "%d/%m/%Y")
water_quality_2018_2022$Date_2019 <- as.Date(water_quality_2018_2022$Date_2019, format = "%d/%m/%Y")
water_quality_2018_2022$Date_2018 <- as.Date(water_quality_2018_2022$Date_2018, format = "%d/%m/%Y")

# Let's convert all years into time series object separately
ts_water_temp_2023 <- ts(water_quality_2023$water_temp_avg, frequency = 365)
ts_water_temp_2022 <- ts(water_quality_2018_2022$water_temp_2022, frequency = 365)
ts_water_temp_2021 <- ts(water_quality_2018_2022$water_temp_2021, frequency = 365)
ts_water_temp_2020 <- ts(water_quality_2018_2022$water_temp_2020, frequency = 365)
ts_water_temp_2019 <- ts(water_quality_2018_2022$water_temp_2019, frequency = 365)
ts_water_temp_2018 <- ts(water_quality_2018_2022$water_temp_2018, frequency = 365)     


# Create a data frame for ggplot to include date from original dataframe; establish water temperature as numeric
ts_water_temp_2023_gg <- data.frame(Date2023 = water_quality_2023$Date_2023, WaterTemp2023 = as.numeric(ts_water_temp_2023))
ts_water_temp_2022_gg <- data.frame(Date2022 = water_quality_2018_2022$Date_2022, WaterTemp2022 = as.numeric(ts_water_temp_2022))
ts_water_temp_2021_gg <- data.frame(Date2021 = water_quality_2018_2022$Date_2021, WaterTemp2021 = as.numeric(ts_water_temp_2021))
ts_water_temp_2020_gg <- data.frame(Date2020 = water_quality_2018_2022$Date_2020, WaterTemp2020 = as.numeric(ts_water_temp_2020))
ts_water_temp_2019_gg <- data.frame(Date2019 = water_quality_2018_2022$Date_2019, WaterTemp2019 = as.numeric(ts_water_temp_2019))
ts_water_temp_2018_gg <- data.frame(Date2018 = water_quality_2018_2022$Date_2018, WaterTemp2018 = as.numeric(ts_water_temp_2018))

# Create a new column with the abbreviated month name for ordering via lubridate for facet_wrap
ts_water_temp_2023_gg$Month_2023 <- month(ts_water_temp_2023_gg$Date2023, label = TRUE)


ts_water_temp_2022_gg$Month_2022 <- month(ts_water_temp_2022_gg$Date2022, label = TRUE)


ts_water_temp_2021_gg$Month_2021 <- month(ts_water_temp_2021_gg$Date2021, label = TRUE)


ts_water_temp_2020_gg$Month_2020 <- month(ts_water_temp_2020_gg$Date2020, label = TRUE)


ts_water_temp_2019_gg$Month_2019 <- month(ts_water_temp_2019_gg$Date2019, label = TRUE)


ts_water_temp_2018_gg$Month_2018 <- month(ts_water_temp_2018_gg$Date2018, label = TRUE)

# Convert ts_water_temp_year_gg's Date into proper format using as.Date (NEED TO DO IT AGAIN BECAUSE OF NEW DATA FRAMES)
ts_water_temp_2023_gg$Date2023 <- as.Date(ts_water_temp_2023_gg$Date2023, format = "%d/%m/%Y")
ts_water_temp_2022_gg$Date2022 <- as.Date(ts_water_temp_2022_gg$Date2022, format = "%d/%m/%Y")
ts_water_temp_2021_gg$Date2021 <- as.Date(ts_water_temp_2021_gg$Date2021, format = "%d/%m/%Y")
ts_water_temp_2020_gg$Date2020 <- as.Date(ts_water_temp_2020_gg$Date2020, format = "%d/%m/%Y")
ts_water_temp_2019_gg$Date2019 <- as.Date(ts_water_temp_2019_gg$Date2019, format = "%d/%m/%Y")
ts_water_temp_2018_gg$Date2018 <- as.Date(ts_water_temp_2018_gg$Date2018, format = "%d/%m/%Y")



# Create a new column with the abbreviated month name for ordering via lubridate for facet_wrap
# This helps to create box-and-whisker plot
ts_water_temp_2023_gg$Season_2023 <- ifelse(ts_water_temp_2023_gg$Month_2023 %in% c("Dec", "Jan", "Feb"), "Winter",
                                     ifelse(ts_water_temp_2023_gg$Month_2023 %in% c("Mar", "Apr", "May"), "Spring",
                                     ifelse(ts_water_temp_2023_gg$Month_2023 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_water_temp_2022_gg$Season_2022 <- ifelse(ts_water_temp_2022_gg$Month_2022 %in% c("Dec", "Jan", "Feb"), "Winter",
                                     ifelse(ts_water_temp_2022_gg$Month_2022 %in% c("Mar", "Apr", "May"), "Spring",
                                     ifelse(ts_water_temp_2022_gg$Month_2022 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_water_temp_2021_gg$Season_2021 <- ifelse(ts_water_temp_2021_gg$Month_2021 %in% c("Dec", "Jan", "Feb"), "Winter",
                                     ifelse(ts_water_temp_2021_gg$Month_2021 %in% c("Mar", "Apr", "May"), "Spring",
                                     ifelse(ts_water_temp_2021_gg$Month_2021 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))    

ts_water_temp_2020_gg$Season_2020 <- ifelse(ts_water_temp_2020_gg$Month_2020 %in% c("Dec", "Jan", "Feb"), "Winter",
                                     ifelse(ts_water_temp_2020_gg$Month_2020 %in% c("Mar", "Apr", "May"), "Spring",
                                     ifelse(ts_water_temp_2020_gg$Month_2020 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_water_temp_2019_gg$Season_2019 <- ifelse(ts_water_temp_2019_gg$Month_2019 %in% c("Dec", "Jan", "Feb"), "Winter",
                                     ifelse(ts_water_temp_2019_gg$Month_2019 %in% c("Mar", "Apr", "May"), "Spring",
                                     ifelse(ts_water_temp_2019_gg$Month_2019 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_water_temp_2018_gg$Season_2018 <- ifelse(ts_water_temp_2018_gg$Month_2018 %in% c("Dec", "Jan", "Feb"), "Winter",
                                     ifelse(ts_water_temp_2018_gg$Month_2018 %in% c("Mar", "Apr", "May"), "Spring",
                                     ifelse(ts_water_temp_2018_gg$Month_2018 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))



# Create a box-and-whisker plot

temp_season_2023 <- ggplot(ts_water_temp_2023_gg, aes(x = Season_2023, y = WaterTemp2023)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Water Temperature (°C)", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

temp_season_2022 <- ggplot(ts_water_temp_2022_gg, aes(x = Season_2022, y = WaterTemp2022)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Water Temperature (°C)", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

temp_season_2021 <- ggplot(ts_water_temp_2021_gg, aes(x = Season_2021, y = WaterTemp2021)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Water Temperature (°C)", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

temp_season_2020 <- ggplot(ts_water_temp_2020_gg, aes(x = Season_2020, y = WaterTemp2020)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Water Temperature (°C)", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

temp_season_2019 <- ggplot(ts_water_temp_2019_gg, aes(x = Season_2019, y = WaterTemp2019)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Water Temperature (°C)", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

temp_season_2018 <- ggplot(ts_water_temp_2018_gg, aes(x = Season_2018, y = WaterTemp2018)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Water Temperature (°C)", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(temp_season_2018, temp_season_2019, temp_season_2020, temp_season_2021, temp_season_2022, temp_season_2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# ANOVA test
stempmodel1 <- lm (WaterTemp2023 ~ Season_2023, data = ts_water_temp_2023_gg)
anova(stempmodel1)
summary(stempmodel1)


stempmodel2 <- lm (WaterTemp2022 ~ Season_2022, data = ts_water_temp_2022_gg)
anova(stempmodel2)
summary(stempmodel2)


stempmodel3 <- lm (WaterTemp2021 ~ Season_2021, data = ts_water_temp_2021_gg)
anova(stempmodel3)
summary(stempmodel3)

stempmodel3 <- lm (WaterTemp2020 ~ Season_2020, data = ts_water_temp_2020_gg)
anova(stempmodel3)
summary(stempmodel3)

stempmodel4 <- lm (WaterTemp2019 ~ Season_2019, data = ts_water_temp_2019_gg)
anova(stempmodel4)
summary(stempmodel4)

stempmodel5 <- lm (WaterTemp2018 ~ Season_2018, data = ts_water_temp_2018_gg)
anova(stempmodel5)
summary(stempmodel5)



# Plot the time series with ggplot 
ggplot(ts_water_temp_2023_gg, aes(x = Date2023, y = WaterTemp2023)) +
  geom_line(color = "darkgreen", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Seasonal Fluctuations in Water Temperature at River Cabrach (2023)",
       x = "Month", y = "Water Temperature (°C)")

ggplot(ts_water_temp_2022_gg, aes(x = Date2022, y = WaterTemp2022)) +
  geom_line(color = "blue", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Seasonal Fluctuations in Water Temperature at River Cabrach (2022)",
       x = "Month", y = "Water Temperature (°C)")

ggplot(ts_water_temp_2021_gg, aes(x = Date2021, y = WaterTemp2021)) +
  geom_line(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Seasonal Fluctuations in Water Temperature at River Cabrach (2021)",
       x = "Month", y = "Water Temperature (°C)")

ggplot(ts_water_temp_2020_gg, aes(x = Date2020, y = WaterTemp2020)) +
  geom_line(color = "orange", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Seasonal Fluctuations in Water Temperature at River Cabrach (2020)",
       x = "Month", y = "Water Temperature (°C)")

ggplot(ts_water_temp_2019_gg, aes(x = Date2019, y = WaterTemp2019)) +
  geom_line(color = "purple", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Seasonal Fluctuations in Water Temperature at River Cabrach (2019)",
       x = "Month", y = "Water Temperature (°C)")

ggplot(ts_water_temp_2018_gg, aes(x = Date2018, y = WaterTemp2018)) +
  geom_line(color = "gold", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Seasonal Fluctuations in Water Temperature at River Cabrach (2018)",
       x = "Month", y = "Water Temperature (°C)")


# Plot the time series with ggplot after which put all in one image 
ts_wt_2023 <- ggplot(ts_water_temp_2023_gg, aes(x = Date2023, y = WaterTemp2023)) +
  geom_line(color = "darkgreen", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

ts_wt_2022 <- ggplot(ts_water_temp_2022_gg, aes(x = Date2022, y = WaterTemp2022)) +
  geom_line(color = "blue", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

ts_wt_2021 <- ggplot(ts_water_temp_2021_gg, aes(x = Date2021, y = WaterTemp2021)) +
  geom_line(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

ts_wt_2020 <- ggplot(ts_water_temp_2020_gg, aes(x = Date2020, y = WaterTemp2020)) +
  geom_line(color = "orange", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

ts_wt_2019 <- ggplot(ts_water_temp_2019_gg, aes(x = Date2019, y = WaterTemp2019)) +
  geom_line(color = "purple", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

ts_wt_2018 <- ggplot(ts_water_temp_2018_gg, aes(x = Date2018, y = WaterTemp2018)) +
  geom_line(color = "gold", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))


# Create a list of plots
plot_list <- list(ts_wt_2018, ts_wt_2019, ts_wt_2020, ts_wt_2021, ts_wt_2022, ts_wt_2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))


# Plot the MONTHLY-TREND type time series with ggplot via facet_wrap
# Use scales = "free_x"  so that x-axis are scales independent WHILE y-axis scales are consistent
# Other scales = "free_y" or "free" or "fixed" are available but "free_x" is more suitable
ts_wt_2023_monthly <- ggplot(ts_water_temp_2023_gg, aes(x = Date2023, y = WaterTemp2023)) +
  geom_line(color = "darkgreen", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2023, ncol = 3, scales = "free_x")

ts_wt_2022_monthly <- ggplot(ts_water_temp_2022_gg, aes(x = Date2022, y = WaterTemp2022)) +
  geom_line(color = "blue", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2022, ncol = 3, scales = "free_x")

ts_wt_2021_monthly <- ggplot(ts_water_temp_2021_gg, aes(x = Date2021, y = WaterTemp2021)) +
  geom_line(color = "red", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2021, ncol = 3, scales = "free_x")

ts_wt_2020_monthly <- ggplot(ts_water_temp_2020_gg, aes(x = Date2020, y = WaterTemp2020)) +
  geom_line(color = "orange", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2020, ncol = 3, scales = "free_x")

ts_wt_2019_monthly <- ggplot(ts_water_temp_2019_gg, aes(x = Date2019, y = WaterTemp2019)) +
  geom_line(color = "purple", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2019, ncol = 3, scales = "free_x")

ts_wt_2018_monthly <- ggplot(ts_water_temp_2018_gg, aes(x = Date2018, y = WaterTemp2018)) +
  geom_line(color = "gold", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2018, ncol = 3, scales = "free_x")


# Create a list of plots
plot_list <- list(ts_wt_2018_monthly, ts_wt_2019_monthly, ts_wt_2020_monthly, ts_wt_2021_monthly, ts_wt_2022_monthly, ts_wt_2023_monthly)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list_1, nrow = 4))


# Fit a polynomial regression model for Water Temperature
temp_poly_model1 <- lm(WaterTemp2023 ~ poly(Date2023, degree = 2), data = ts_water_temp_2023_gg)
temp_poly_model2 <- lm(WaterTemp2022 ~ poly(Date2022, degree = 2), data = ts_water_temp_2022_gg)
temp_poly_model3 <- lm(WaterTemp2021 ~ poly(Date2021, degree = 2), data = ts_water_temp_2021_gg)
temp_poly_model4 <- lm(WaterTemp2020 ~ poly(Date2020, degree = 2), data = ts_water_temp_2020_gg)
temp_poly_model5 <- lm(WaterTemp2019 ~ poly(Date2019, degree = 2), data = ts_water_temp_2019_gg)
temp_poly_model6 <- lm(WaterTemp2018 ~ poly(Date2018, degree = 2), data = ts_water_temp_2018_gg)


# Get the summary of the model
summary(temp_poly_model1)
summary(temp_poly_model2)
summary(temp_poly_model3)
summary(temp_poly_model4)
summary(temp_poly_model5)
summary(temp_poly_model6)

# Get the predicted values from polynomial regression models
predicted_temp_seasonal_values2023 <- predict(temp_poly_model1)
predicted_temp_seasonal_values2022 <- predict(temp_poly_model2)
predicted_temp_seasonal_values2021 <- predict(temp_poly_model3)
predicted_temp_seasonal_values2020 <- predict(temp_poly_model4)
predicted_temp_seasonal_values2019 <- predict(temp_poly_model5)
predicted_temp_seasonal_values2018 <- predict(temp_poly_model6)

# Get the residuals from polynomial regressions
temp_residuals_seasonal2023 <- residuals(temp_poly_model1)
temp_residuals_seasonal2022 <- residuals(temp_poly_model2)
temp_residuals_seasonal2021 <- residuals(temp_poly_model3)
temp_residuals_seasonal2020 <- residuals(temp_poly_model4)
temp_residuals_seasonal2019 <- residuals(temp_poly_model5)
temp_residuals_seasonal2018 <- residuals(temp_poly_model6)

# Calculate the residuals for the scatter plot
temp_residuals_seasonal1 <- ts_water_temp_2023_gg$WaterTemp2023 - predicted_temp_seasonal_values2023
temp_residuals_seasonal2 <- ts_water_temp_2022_gg$WaterTemp2022 - predicted_temp_seasonal_values2022
temp_residuals_seasonal3 <- ts_water_temp_2021_gg$WaterTemp2021 - predicted_temp_seasonal_values2021
temp_residuals_seasonal4 <- ts_water_temp_2020_gg$WaterTemp2020 - predicted_temp_seasonal_values2020
temp_residuals_seasonal5 <- ts_water_temp_2019_gg$WaterTemp2019 - predicted_temp_seasonal_values2019
temp_residuals_seasonal6 <- ts_water_temp_2018_gg$WaterTemp2018 - predicted_temp_seasonal_values2018

# Create the scatter plot of residuals against the predictor variable

res_temp_seasonal2023 <- ggplot(ts_water_temp_2023_gg, aes(x = Date2023, y = temp_residuals_seasonal1)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_seasonal2022 <- ggplot(ts_water_temp_2022_gg, aes(x = Date2022, y = temp_residuals_seasonal2)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_seasonal2021 <- ggplot(ts_water_temp_2021_gg, aes(x = Date2021, y = temp_residuals_seasonal3)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_seasonal2020 <- ggplot(ts_water_temp_2020_gg, aes(x = Date2020, y = temp_residuals_seasonal4)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_seasonal2019 <- ggplot(ts_water_temp_2019_gg, aes(x = Date2019, y = temp_residuals_seasonal5)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_seasonal2018 <- ggplot(ts_water_temp_2018_gg, aes(x = Date2018, y = temp_residuals_seasonal6)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(res_temp_seasonal2018, res_temp_seasonal2019, res_temp_seasonal2020, res_temp_seasonal2021, res_temp_seasonal2022, res_temp_seasonal2023)

# Combine all the plots and add legends
combined_plot<- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# Q-Q plot = normality of residuals using residuals obtain from polynomial models 
# set up the multi-panel plot
par(mfrow = c(2,3)) # 2 rows, 3 column

qqnorm(temp_residuals_seasonal2023, main = "") 
qqline(temp_residuals_seasonal2023)
mtext("Year 2023", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(temp_residuals_seasonal2022, main = "") 
qqline(temp_residuals_seasonal2022)
mtext("Year 2022", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(temp_residuals_seasonal2021, main = "")
qqline(temp_residuals_seasonal2021)
mtext("Year 2021", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(temp_residuals_seasonal2020, main = "")
qqline(temp_residuals_seasonal2020)
mtext("Year 2020", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(temp_residuals_seasonal2019, main = "")
qqline(temp_residuals_seasonal2019)
mtext("Year 2019", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(temp_residuals_seasonal2018, main = "")
qqline(temp_residuals_seasonal2018)
mtext("Year 2018", side = 1, line = 4, cex = 0.8, font = 3)


# Create histogram of water temperature residuals to confirm or deny distribution
temp_seasonal_residuals_hist2023 <- ggplot(data = ts_water_temp_2023_gg, aes (x = temp_residuals_seasonal2023)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

temp_seasonal_residuals_hist2022 <- ggplot(data = ts_water_temp_2022_gg, aes (x = temp_residuals_seasonal2022)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

temp_seasonal_residuals_hist2021 <- ggplot(data = ts_water_temp_2021_gg, aes (x = temp_residuals_seasonal2021)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

temp_seasonal_residuals_hist2020 <- ggplot(data = ts_water_temp_2020_gg, aes (x = temp_residuals_seasonal2020)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

temp_seasonal_residuals_hist2019 <- ggplot(data = ts_water_temp_2019_gg, aes (x = temp_residuals_seasonal2019)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

temp_seasonal_residuals_hist2018 <- ggplot(data = ts_water_temp_2018_gg, aes (x = temp_residuals_seasonal2018)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(temp_seasonal_residuals_hist2018, temp_seasonal_residuals_hist2019, temp_seasonal_residuals_hist2020, temp_seasonal_residuals_hist2021, temp_seasonal_residuals_hist2022, temp_seasonal_residuals_hist2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))




##############################################################################################



# Electrical conductivity 

# Let's convert all years into time series object separately for Electrical Conductivity
ts_EC_25_2023 <- ts(water_quality_2023$EC_25_avg, frequency = 365)
ts_EC_25_2022 <- ts(water_quality_2018_2022$EC_25_2022, frequency = 365)
ts_EC_25_2021 <- ts(water_quality_2018_2022$EC_25_2021, frequency = 365)
ts_EC_25_2020 <- ts(water_quality_2018_2022$EC_25_2020, frequency = 365)
ts_EC_25_2019 <- ts(water_quality_2018_2022$EC_25_2019, frequency = 365)
ts_EC_25_2018 <- ts(water_quality_2018_2022$EC_25_2018, frequency = 365)   

# Create EC data frames for ggplot to include Electrical Conductivity as numeric and Date from original dataframe
ts_EC_2023_gg <- data.frame(Date2023 = water_quality_2023$Date_2023, EC2023 = as.numeric(ts_EC_25_2023))
ts_EC_2022_gg <- data.frame(Date2022 = water_quality_2018_2022$Date_2022, EC2022 = as.numeric(ts_EC_25_2022))
ts_EC_2021_gg <- data.frame(Date2021 = water_quality_2018_2022$Date_2021, EC2021 = as.numeric(ts_EC_25_2021))
ts_EC_2020_gg <- data.frame(Date2020 = water_quality_2018_2022$Date_2020, EC2020 = as.numeric(ts_EC_25_2020))
ts_EC_2019_gg <- data.frame(Date2019 = water_quality_2018_2022$Date_2019, EC2019 = as.numeric(ts_EC_25_2019))
ts_EC_2018_gg <- data.frame(Date2018 = water_quality_2018_2022$Date_2018, EC2018 = as.numeric(ts_EC_25_2018))


# Create a new column with the abbreviated month name for ordering via lubridate for facet_wrap
ts_EC_2023_gg$Month_2023 <- month(ts_EC_2023_gg$Date2023, label = TRUE)


ts_EC_2022_gg$Month_2022 <- month(ts_EC_2022_gg$Date2022, label = TRUE)


ts_EC_2021_gg$Month_2021 <- month(ts_EC_2021_gg$Date2021, label = TRUE)


ts_EC_2020_gg$Month_2020 <- month(ts_EC_2020_gg$Date2020, label = TRUE)


ts_EC_2019_gg$Month_2019 <- month(ts_EC_2019_gg$Date2019, label = TRUE)


ts_EC_2018_gg$Month_2018 <- month(ts_EC_2018_gg$Date2018, label = TRUE)

# Convert ts_EC_year_gg's Date into proper format using as.Date (NEED TO DO IT AGAIN BECAUSE OF NEW DATA FRAMES)
ts_EC_2023_gg$Date2023 <- as.Date(ts_EC_2023_gg$Date2023, format = "%d/%m/%Y")
ts_EC_2022_gg$Date2022 <- as.Date(ts_EC_2022_gg$Date2022, format = "%d/%m/%Y")
ts_EC_2021_gg$Date2021 <- as.Date(ts_EC_2021_gg$Date2021, format = "%d/%m/%Y")
ts_EC_2020_gg$Date2020 <- as.Date(ts_EC_2020_gg$Date2020, format = "%d/%m/%Y")
ts_EC_2019_gg$Date2019 <- as.Date(ts_EC_2019_gg$Date2019, format = "%d/%m/%Y")
ts_EC_2018_gg$Date2018 <- as.Date(ts_EC_2018_gg$Date2018, format = "%d/%m/%Y")

# Create a new column with the abbreviated month name for ordering via lubridate for facet_wrap
# This helps to create box-and-whisker plot
ts_EC_2023_gg$Season_2023 <- ifelse(ts_EC_2023_gg$Month_2023 %in% c("Dec", "Jan", "Feb"), "Winter",
                                            ifelse(ts_EC_2023_gg$Month_2023 %in% c("Mar", "Apr", "May"), "Spring",
                                                   ifelse(ts_EC_2023_gg$Month_2023 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_EC_2022_gg$Season_2022 <- ifelse(ts_EC_2022_gg$Month_2022 %in% c("Dec", "Jan", "Feb"), "Winter",
                                            ifelse(ts_EC_2022_gg$Month_2022 %in% c("Mar", "Apr", "May"), "Spring",
                                                   ifelse(ts_EC_2022_gg$Month_2022 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_EC_2021_gg$Season_2021 <- ifelse(ts_EC_2021_gg$Month_2021 %in% c("Dec", "Jan", "Feb"), "Winter",
                                            ifelse(ts_EC_2021_gg$Month_2021 %in% c("Mar", "Apr", "May"), "Spring",
                                                   ifelse(ts_EC_2021_gg$Month_2021 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))    

ts_EC_2020_gg$Season_2020 <- ifelse(ts_EC_2020_gg$Month_2020 %in% c("Dec", "Jan", "Feb"), "Winter",
                                            ifelse(ts_EC_2020_gg$Month_2020 %in% c("Mar", "Apr", "May"), "Spring",
                                                   ifelse(ts_EC_2020_gg$Month_2020 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_EC_2019_gg$Season_2019 <- ifelse(ts_EC_2019_gg$Month_2019 %in% c("Dec", "Jan", "Feb"), "Winter",
                                            ifelse(ts_EC_2019_gg$Month_2019 %in% c("Mar", "Apr", "May"), "Spring",
                                                   ifelse(ts_EC_2019_gg$Month_2019 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_EC_2018_gg$Season_2018 <- ifelse(ts_EC_2018_gg$Month_2018 %in% c("Dec", "Jan", "Feb"), "Winter",
                                            ifelse(ts_EC_2018_gg$Month_2018 %in% c("Mar", "Apr", "May"), "Spring",
                                                   ifelse(ts_EC_2018_gg$Month_2018 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))



# Create a box-and-whisker plot

EC_season_2023 <- ggplot(ts_EC_2023_gg, aes(x = Season_2023, y = EC2023)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Electrical Conductivity at 25°C", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

EC_season_2022 <- ggplot(ts_EC_2022_gg, aes(x = Season_2022, y = EC2022)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Electrical Conductivity at 25°C", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

EC_season_2021 <- ggplot(ts_EC_2021_gg, aes(x = Season_2021, y = EC2021)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Electrical Conductivity at 25°C", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

EC_season_2020 <- ggplot(ts_EC_2020_gg, aes(x = Season_2020, y = EC2020)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Electrical Conductivity at 25°C", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

EC_season_2019 <- ggplot(ts_EC_2019_gg, aes(x = Season_2019, y = EC2019)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Electrical Conductivity at 25°C", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

EC_season_2018 <- ggplot(ts_EC_2018_gg, aes(x = Season_2018, y = EC2018)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Electrical Conductivity at 25°C", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(EC_season_2018, EC_season_2019, EC_season_2020, EC_season_2021, EC_season_2022, EC_season_2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# ANOVA test
ECmodel1 <- lm (EC2023 ~ Season_2023, data = ts_EC_2023_gg)
anova(ECmodel1)
summary(ECmodel1)


ECmodel2 <- lm (EC2022 ~ Season_2022, data = ts_EC_2022_gg)
anova(ECmodel2)
summary(ECmodel2)


ECmodel3 <- lm (EC2021 ~ Season_2021, data = ts_EC_2021_gg)
anova(ECmodel3)
summary(ECmodel3)

ECmodel4 <- lm (EC2020 ~ Season_2020, data = ts_EC_2020_gg)
anova(ECmodel4)
summary(ECmodel4)

ECmodel5 <- lm (EC2019 ~ Season_2019, data = ts_EC_2019_gg)
anova(ECmodel5)
summary(ECmodel5)

ECmodel6 <- lm (EC2018 ~ Season_2018, data = ts_EC_2018_gg)
anova(ECmodel6)
summary(ECmodel6)


# Plot the time series with ggplot after which put all in one image 
ts_EC_2023 <- ggplot(ts_EC_2023_gg, aes(x = Date2023, y = EC2023)) +
  geom_line(color = "darkgreen", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

ts_EC_2022 <- ggplot(ts_EC_2022_gg, aes(x = Date2022, y = EC2022)) +
  geom_line(color = "blue", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

ts_EC_2021 <- ggplot(ts_EC_2021_gg, aes(x = Date2021, y = EC2021)) +
  geom_line(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

ts_EC_2020 <- ggplot(ts_EC_2020_gg, aes(x = Date2020, y = EC2020)) +
  geom_line(color = "orange", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

ts_EC_2019 <- ggplot(ts_EC_2019_gg, aes(x = Date2019, y = EC2019)) +
  geom_line(color = "purple", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

ts_EC_2018 <- ggplot(ts_EC_2018_gg, aes(x = Date2018, y = EC2018)) +
  geom_line(color = "gold", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))


# Create a list of plots
plot_list <- list(ts_EC_2018, ts_EC_2019, ts_EC_2020, ts_EC_2021, ts_EC_2022, ts_EC_2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# Plot the MONTHLY-TREND type time series with ggplot via facet_wrap using free_x scale
ts_EC_2023_monthly <- ggplot(ts_EC_2023_gg, aes(x = Date2023, y = EC2023)) +
  geom_line(color = "darkgreen", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2023, ncol = 3, scales = "free_x")

ts_EC_2022_monthly <- ggplot(ts_EC_2022_gg, aes(x = Date2022, y = EC2022)) +
  geom_line(color = "blue", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Temperature (°C)", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2022, ncol = 3, scales = "free_x")

ts_EC_2021_monthly <- ggplot(ts_EC_2021_gg, aes(x = Date2021, y = EC2021)) +
  geom_line(color = "red", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2021, ncol = 3, scales = "free_x")

ts_EC_2020_monthly <- ggplot(ts_EC_2020_gg, aes(x = Date2020, y = EC2020)) +
  geom_line(color = "orange", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2020, ncol = 3, scales = "free_x")

ts_EC_2019_monthly <- ggplot(ts_EC_2019_gg, aes(x = Date2019, y = EC2019)) +
  geom_line(color = "purple", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2019, ncol = 3, scales = "free_x")

ts_EC_2018_monthly <- ggplot(ts_EC_2018_gg, aes(x = Date2018, y = EC2018)) +
  geom_line(color = "gold", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Electrical Conductivity at 25°C", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2018, ncol = 3, scales = "free_x")


# Create a list of plots
plot_list <- list(ts_EC_2018_monthly, ts_EC_2019_monthly, ts_EC_2020_monthly, ts_EC_2021_monthly, ts_EC_2022_monthly, ts_EC_2023_monthly)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 4))


# Fit a polynomial regression model for Electrical Conductivity
EC_poly_model1 <- lm(EC2023 ~ poly(Date2023, degree = 2), data = ts_EC_2023_gg)
EC_poly_model2 <- lm(EC2022 ~ poly(Date2022, degree = 2), data = ts_EC_2022_gg)
EC_poly_model3 <- lm(EC2021 ~ poly(Date2021, degree = 2), data = ts_EC_2021_gg)
EC_poly_model4 <- lm(EC2020 ~ poly(Date2020, degree = 2), data = ts_EC_2020_gg)
EC_poly_model5 <- lm(EC2019 ~ poly(Date2019, degree = 2), data = ts_EC_2019_gg)
EC_poly_model6 <- lm(EC2018 ~ poly(Date2018, degree = 2), data = ts_EC_2018_gg)


# Get the summary of the model
summary(EC_poly_model1)
summary(EC_poly_model2)
summary(EC_poly_model3)
summary(EC_poly_model4)
summary(EC_poly_model5)
summary(EC_poly_model6)

# Get the predicted values from polynomial regression models
predicted_EC_seasonal_values2023 <- predict(EC_poly_model1)
predicted_EC_seasonal_values2022 <- predict(EC_poly_model2)
predicted_EC_seasonal_values2021 <- predict(EC_poly_model3)
predicted_EC_seasonal_values2020 <- predict(EC_poly_model4)
predicted_EC_seasonal_values2019 <- predict(EC_poly_model5)
predicted_EC_seasonal_values2018 <- predict(EC_poly_model6)

# Get the residuals from polynomial regressions for Q-Q line
EC_residuals_seasonal2023 <- residuals(EC_poly_model1)
EC_residuals_seasonal2022 <- residuals(EC_poly_model2)
EC_residuals_seasonal2021 <- residuals(EC_poly_model3)
EC_residuals_seasonal2020 <- residuals(EC_poly_model4)
EC_residuals_seasonal2019 <- residuals(EC_poly_model5)
EC_residuals_seasonal2018 <- residuals(EC_poly_model6)

# Calculate the residuals for the scatter plot
EC_residuals_seasonal1 <- ts_EC_2023_gg$EC2023 - predicted_EC_seasonal_values2023
EC_residuals_seasonal2 <- ts_EC_2022_gg$EC2022 - predicted_EC_seasonal_values2022
EC_residuals_seasonal3 <- ts_EC_2021_gg$EC2021 - predicted_EC_seasonal_values2021
EC_residuals_seasonal4 <- ts_EC_2020_gg$EC2020 - predicted_EC_seasonal_values2020
EC_residuals_seasonal5 <- ts_EC_2019_gg$EC2019 - predicted_EC_seasonal_values2019
EC_residuals_seasonal6 <- ts_EC_2018_gg$EC2018 - predicted_EC_seasonal_values2018

# Create the scatter plot of residuals against the predictor variable

res_EC_seasonal2023 <- ggplot(ts_EC_2023_gg, aes(x = Date2023, y = EC_residuals_seasonal1)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

res_EC_seasonal2022 <- ggplot(ts_EC_2022_gg, aes(x = Date2022, y = EC_residuals_seasonal2)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

res_EC_seasonal2021 <- ggplot(ts_EC_2021_gg, aes(x = Date2021, y = EC_residuals_seasonal3)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

res_EC_seasonal2020 <- ggplot(ts_EC_2020_gg, aes(x = Date2020, y = EC_residuals_seasonal4)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

res_EC_seasonal2019 <- ggplot(ts_EC_2019_gg, aes(x = Date2019, y = EC_residuals_seasonal5)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

res_EC_seasonal2018 <- ggplot(ts_EC_2018_gg, aes(x = Date2018, y = EC_residuals_seasonal6)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(res_EC_seasonal2018, res_EC_seasonal2019, res_EC_seasonal2020, res_EC_seasonal2021, res_EC_seasonal2022, res_EC_seasonal2023)

# Combine all the plots and add legends
combined_plot<- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# Q-Q plot = normality of residuals using residuals obtain from polynomial models 
# set up the multi-panel plot
par(mfrow = c(2,3)) # 2 rows, 3 column

qqnorm(EC_residuals_seasonal2023, main = "") 
qqline(EC_residuals_seasonal2023)
mtext("Year 2023", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(EC_residuals_seasonal2022, main = "") 
qqline(EC_residuals_seasonal2022)
mtext("Year 2022", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(EC_residuals_seasonal2021, main = "")
qqline(EC_residuals_seasonal2021)
mtext("Year 2021", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(EC_residuals_seasonal2020, main = "")
qqline(EC_residuals_seasonal2020)
mtext("Year 2020", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(EC_residuals_seasonal2019, main = "")
qqline(EC_residuals_seasonal2019)
mtext("Year 2019", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(EC_residuals_seasonal2018, main = "")
qqline(EC_residuals_seasonal2018)
mtext("Year 2018", side = 1, line = 4, cex = 0.8, font = 3)


# Create histogram of electrical conductivity residuals to confirm or deny distribution
EC_seasonal_residuals_hist2023 <- ggplot(data = ts_EC_2023_gg, aes (x = EC_residuals_seasonal2023)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

EC_seasonal_residuals_hist2022 <- ggplot(data = ts_EC_2022_gg, aes (x = EC_residuals_seasonal2022)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

EC_seasonal_residuals_hist2021 <- ggplot(data = ts_EC_2021_gg, aes (x = EC_residuals_seasonal2021)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

EC_seasonal_residuals_hist2020 <- ggplot(data = ts_EC_2020_gg, aes (x = EC_residuals_seasonal2020)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

EC_seasonal_residuals_hist2019 <- ggplot(data = ts_EC_2019_gg, aes (x = EC_residuals_seasonal2019)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

EC_seasonal_residuals_hist2018 <- ggplot(data = ts_EC_2018_gg, aes (x = EC_residuals_seasonal2018)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))



# Create a list of plots
plot_list <- list(EC_seasonal_residuals_hist2018, EC_seasonal_residuals_hist2019, EC_seasonal_residuals_hist2020, EC_seasonal_residuals_hist2021, EC_seasonal_residuals_hist2022, EC_seasonal_residuals_hist2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))






############################################################################################ 

# Water Depth

# Let's convert all years into time series object separately for WATER DEPTH
ts_depth_2023 <- ts(water_quality_2023$water_level_avg, frequency = 365)
ts_depth_2022 <- ts(water_quality_2018_2022$water_depth_2022, frequency = 365)
ts_depth_2021 <- ts(water_quality_2018_2022$water_depth_2021, frequency = 365)
ts_depth_2020 <- ts(water_quality_2018_2022$water_depth_2020, frequency = 365)
ts_depth_2019 <- ts(water_quality_2018_2022$water_depth_2019, frequency = 365)
ts_depth_2018 <- ts(water_quality_2018_2022$water_depth_2018, frequency = 365)   

# Create depth data frames for ggplot to include depth as numeric and Date from original dataframe
ts_depth_2023_gg <- data.frame(Date2023 = water_quality_2023$Date_2023, depth2023 = as.numeric(ts_depth_2023))
ts_depth_2022_gg <- data.frame(Date2022 = water_quality_2018_2022$Date_2022, depth2022 = as.numeric(ts_depth_2022))
ts_depth_2021_gg <- data.frame(Date2021 = water_quality_2018_2022$Date_2021, depth2021 = as.numeric(ts_depth_2021))
ts_depth_2020_gg <- data.frame(Date2020 = water_quality_2018_2022$Date_2020, depth2020 = as.numeric(ts_depth_2020))
ts_depth_2019_gg <- data.frame(Date2019 = water_quality_2018_2022$Date_2019, depth2019 = as.numeric(ts_depth_2019))
ts_depth_2018_gg <- data.frame(Date2018 = water_quality_2018_2022$Date_2018, depth2018 = as.numeric(ts_depth_2018))

# Create a new column with the abbreviated month name for ordering via lubridate for facet_wrap
ts_depth_2023_gg$Month_2023 <- month(ts_depth_2023_gg$Date2023, label = TRUE)


ts_depth_2022_gg$Month_2022 <- month(ts_depth_2022_gg$Date2022, label = TRUE)


ts_depth_2021_gg$Month_2021 <- month(ts_depth_2021_gg$Date2021, label = TRUE)


ts_depth_2020_gg$Month_2020 <- month(ts_depth_2020_gg$Date2020, label = TRUE)


ts_depth_2019_gg$Month_2019 <- month(ts_depth_2019_gg$Date2019, label = TRUE)


ts_depth_2018_gg$Month_2018 <- month(ts_depth_2018_gg$Date2018, label = TRUE)

# Convert ts_water_temp_year_gg's Date into proper format using as.Date (NEED TO DO IT AGAIN BECAUSE OF NEW DATA FRAMES)
ts_depth_2023_gg$Date2023 <- as.Date(ts_depth_2023_gg$Date2023, format = "%d/%m/%Y")
ts_depth_2022_gg$Date2022 <- as.Date(ts_depth_2022_gg$Date2022, format = "%d/%m/%Y")
ts_depth_2021_gg$Date2021 <- as.Date(ts_depth_2021_gg$Date2021, format = "%d/%m/%Y")
ts_depth_2020_gg$Date2020 <- as.Date(ts_depth_2020_gg$Date2020, format = "%d/%m/%Y")
ts_depth_2019_gg$Date2019 <- as.Date(ts_depth_2019_gg$Date2019, format = "%d/%m/%Y")
ts_depth_2018_gg$Date2018 <- as.Date(ts_depth_2018_gg$Date2018, format = "%d/%m/%Y")


# Create a new column with the abbreviated month name for ordering via lubridate for facet_wrap
# This helps to create box-and-whisker plot
ts_depth_2023_gg$Season_2023 <- ifelse(ts_depth_2023_gg$Month_2023 %in% c("Dec", "Jan", "Feb"), "Winter",
                                    ifelse(ts_depth_2023_gg$Month_2023 %in% c("Mar", "Apr", "May"), "Spring",
                                           ifelse(ts_depth_2023_gg$Month_2023 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_depth_2022_gg$Season_2022 <- ifelse(ts_depth_2022_gg$Month_2022 %in% c("Dec", "Jan", "Feb"), "Winter",
                                    ifelse(ts_depth_2022_gg$Month_2022 %in% c("Mar", "Apr", "May"), "Spring",
                                           ifelse(ts_depth_2022_gg$Month_2022 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_depth_2021_gg$Season_2021 <- ifelse(ts_depth_2021_gg$Month_2021 %in% c("Dec", "Jan", "Feb"), "Winter",
                                    ifelse(ts_depth_2021_gg$Month_2021 %in% c("Mar", "Apr", "May"), "Spring",
                                           ifelse(ts_depth_2021_gg$Month_2021 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))    

ts_depth_2020_gg$Season_2020 <- ifelse(ts_depth_2020_gg$Month_2020 %in% c("Dec", "Jan", "Feb"), "Winter",
                                    ifelse(ts_depth_2020_gg$Month_2020 %in% c("Mar", "Apr", "May"), "Spring",
                                           ifelse(ts_depth_2020_gg$Month_2020 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_depth_2019_gg$Season_2019 <- ifelse(ts_depth_2019_gg$Month_2019 %in% c("Dec", "Jan", "Feb"), "Winter",
                                    ifelse(ts_depth_2019_gg$Month_2019 %in% c("Mar", "Apr", "May"), "Spring",
                                           ifelse(ts_depth_2019_gg$Month_2019 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))

ts_depth_2018_gg$Season_2018 <- ifelse(ts_depth_2018_gg$Month_2018 %in% c("Dec", "Jan", "Feb"), "Winter",
                                    ifelse(ts_depth_2018_gg$Month_2018 %in% c("Mar", "Apr", "May"), "Spring",
                                           ifelse(ts_depth_2018_gg$Month_2018 %in% c("Jun", "Jul", "Aug"), "Summer", "Autumn")))



# Create a box-and-whisker plot

depth_season_2023 <- ggplot(ts_depth_2023_gg, aes(x = Season_2023, y = depth2023)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Average Water Depth (m)", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

depth_season_2022 <- ggplot(ts_depth_2022_gg, aes(x = Season_2022, y = depth2022)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Average Water Depth (m)", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

depth_season_2021 <- ggplot(ts_depth_2021_gg, aes(x = Season_2021, y = depth2021)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Average Water Depth (m)", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

depth_season_2020 <- ggplot(ts_depth_2020_gg, aes(x = Season_2020, y = depth2020)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Average Water Depth (m)", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

depth_season_2019 <- ggplot(ts_depth_2019_gg, aes(x = Season_2019, y = depth2019)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Average Water Depth (m)", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

depth_season_2018 <- ggplot(ts_depth_2018_gg, aes(x = Season_2018, y = depth2018)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Season", y = "Average Water Depth (m)", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(depth_season_2018, depth_season_2019, depth_season_2020, depth_season_2021, depth_season_2022, depth_season_2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# ANOVA test
depthmodel1 <- lm (depth2023 ~ Season_2023, data = ts_depth_2023_gg)
anova(depthmodel1)
summary(depthmodel1)


depthmodel2 <- lm (depth2022 ~ Season_2022, data = ts_depth_2022_gg)
anova(depthmodel2)
summary(depthmodel2)


depthmodel3 <- lm (depth2021 ~ Season_2021, data = ts_depth_2021_gg)
anova(ECmodel3)
summary(ECmodel3)

depthmodel4 <- lm (depth2020 ~ Season_2020, data = ts_depth_2020_gg)
anova(ECmodel4)
summary(ECmodel4)

depthmodel5 <- lm (depth2019 ~ Season_2019, data = ts_depth_2019_gg)
anova(ECmodel5)
summary(ECmodel5)

depthmodel6 <- lm (depth2018 ~ Season_2018, data = ts_depth_2018_gg)
anova(ECmodel6)
summary(ECmodel6)


# Plot the time series with ggplot after which put all in one image 
ts_depth2023 <- ggplot(ts_depth_2023_gg, aes(x = Date2023, y = depth2023)) +
  geom_line(color = "darkgreen", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

ts_depth2022 <- ggplot(ts_depth_2022_gg, aes(x = Date2022, y = depth2022)) +
  geom_line(color = "blue", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

ts_depth2021 <- ggplot(ts_depth_2021_gg, aes(x = Date2021, y = depth2021)) +
  geom_line(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

ts_depth2020 <- ggplot(ts_depth_2020_gg, aes(x = Date2020, y = depth2020)) +
  geom_line(color = "orange", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

ts_depth2019 <- ggplot(ts_depth_2019_gg, aes(x = Date2019, y = depth2019)) +
  geom_line(color = "purple", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

ts_depth2018 <- ggplot(ts_depth_2018_gg, aes(x = Date2018, y = depth2018)) +
  geom_line(color = "gold", size = 1.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))


# Create a list of plots
plot_list <- list(ts_depth2018, ts_depth2019, ts_depth2020, ts_depth2021, ts_depth2022, ts_depth2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# Plot the MONTHLY-TREND type time series with ggplot via facet_wrap
# i used scales = "free_x"  so that x-axis are scales independent WHILE y-axis scales are consistent
# other scales = "free_y" or "free" or "fixed" are available but "free_x" is more suitable
ts_depth2023_monthly <- ggplot(ts_depth_2023_gg, aes(x = Date2023, y = depth2023)) +
  geom_line(color = "darkgreen", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2023, ncol = 3, scales = "free_x")

ts_depth2022_monthly <- ggplot(ts_depth_2022_gg, aes(x = Date2022, y = depth2022)) +
  geom_line(color = "blue", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2022, ncol = 3, scales = "free_x")

ts_depth2021_monthly <- ggplot(ts_depth_2021_gg, aes(x = Date2021, y = depth2021)) +
  geom_line(color = "red", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2021, ncol = 3, scales = "free_x")

ts_depth2020_monthly <- ggplot(ts_depth_2020_gg, aes(x = Date2020, y = depth2020)) +
  geom_line(color = "orange", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2020, ncol = 3, scales = "free_x")

ts_depth2019_monthly <- ggplot(ts_depth_2019_gg, aes(x = Date2019, y = depth2019)) +
  geom_line(color = "purple", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2019, ncol = 3, scales = "free_x")

ts_depth2018_monthly <- ggplot(ts_depth_2018_gg, aes(x = Date2018, y = depth2018)) +
  geom_line(color = "gold", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Month", y = "Water Depth (m)", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold")) +
  facet_wrap( ~ Month_2018, ncol = 3, scales = "free_x")


# Create a list of plots
plot_list <- list(ts_depth2018_monthly, ts_depth2019_monthly, ts_depth2020_monthly, ts_depth2021_monthly, ts_depth2022_monthly, ts_depth2023_monthly)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 4))


# Fit a polynomial regression model for Electrical Conductivity
depth_poly_model1 <- lm(depth2023 ~ poly(Date2023, degree = 2), data = ts_depth_2023_gg)
depth_poly_model2 <- lm(depth2022 ~ poly(Date2022, degree = 2), data = ts_depth_2022_gg)
depth_poly_model3 <- lm(depth2021 ~ poly(Date2021, degree = 2), data = ts_depth_2021_gg)
depth_poly_model4 <- lm(depth2020 ~ poly(Date2020, degree = 2), data = ts_depth_2020_gg)
depth_poly_model5 <- lm(depth2019 ~ poly(Date2019, degree = 2), data = ts_depth_2019_gg)
depth_poly_model6 <- lm(depth2018 ~ poly(Date2018, degree = 2), data = ts_depth_2018_gg)


# Get the summary of the model
summary(depth_poly_model1)
summary(depth_poly_model2)
summary(depth_poly_model3)
summary(depth_poly_model4)
summary(depth_poly_model5)
summary(depth_poly_model6)

# Get the predicted values from polynomial regression models
predicted_depth_seasonal_values2023 <- predict(depth_poly_model1)
predicted_depth_seasonal_values2022 <- predict(depth_poly_model2)
predicted_depth_seasonal_values2021 <- predict(depth_poly_model3)
predicted_depth_seasonal_values2020 <- predict(depth_poly_model4)
predicted_depth_seasonal_values2019 <- predict(depth_poly_model5)
predicted_depth_seasonal_values2018 <- predict(depth_poly_model6)

# Get the residuals from polynomial regressions for Q-Q line
depth_residuals_seasonal2023 <- residuals(depth_poly_model1)
depth_residuals_seasonal2022 <- residuals(depth_poly_model2)
depth_residuals_seasonal2021 <- residuals(depth_poly_model3)
depth_residuals_seasonal2020 <- residuals(depth_poly_model4)
depth_residuals_seasonal2019 <- residuals(depth_poly_model5)
depth_residuals_seasonal2018 <- residuals(depth_poly_model6)

# Calculate the residuals for the scatter plot
depth_residuals_seasonal1 <- ts_depth_2023_gg$depth2023 - predicted_depth_seasonal_values2023
depth_residuals_seasonal2 <- ts_depth_2022_gg$depth2022 - predicted_depth_seasonal_values2022
depth_residuals_seasonal3 <- ts_depth_2021_gg$depth2021 - predicted_depth_seasonal_values2021
depth_residuals_seasonal4 <- ts_depth_2020_gg$depth2020 - predicted_depth_seasonal_values2020
depth_residuals_seasonal5 <- ts_depth_2019_gg$depth2019 - predicted_depth_seasonal_values2019
depth_residuals_seasonal6 <- ts_depth_2018_gg$depth2018 - predicted_depth_seasonal_values2018

# Create the scatter plot of residuals against the predictor variable

res_depth_seasonal2023 <- ggplot(ts_depth_2023_gg, aes(x = Date2023, y = depth_residuals_seasonal1)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

res_depth_seasonal2022 <- ggplot(ts_depth_2022_gg, aes(x = Date2022, y = depth_residuals_seasonal2)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

res_depth_seasonal2021 <- ggplot(ts_depth_2021_gg, aes(x = Date2021, y = depth_residuals_seasonal3)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

res_depth_seasonal2020 <- ggplot(ts_depth_2020_gg, aes(x = Date2020, y = depth_residuals_seasonal4)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

res_depth_seasonal2019 <- ggplot(ts_depth_2019_gg, aes(x = Date2019, y = depth_residuals_seasonal5)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

res_depth_seasonal2018 <- ggplot(ts_depth_2018_gg, aes(x = Date2018, y = depth_residuals_seasonal6)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Month", y = "Residuals", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(res_depth_seasonal2018, res_depth_seasonal2019, res_depth_seasonal2020, res_depth_seasonal2021, res_depth_seasonal2022, res_depth_seasonal2023)

# Combine all the plots and add legends
combined_plot<- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# Q-Q plot = normality of residuals using residuals obtain from polynomial models 
# set up the multi-panel plot
par(mfrow = c(2,3)) # 2 rows, 3 column

qqnorm(depth_residuals_seasonal2023, main = "") 
qqline(depth_residuals_seasonal2023)
mtext("Year 2023", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(depth_residuals_seasonal2022, main = "") 
qqline(depth_residuals_seasonal2022)
mtext("Year 2022", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(depth_residuals_seasonal2021, main = "")
qqline(depth_residuals_seasonal2021)
mtext("Year 2021", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(depth_residuals_seasonal2020, main = "")
qqline(depth_residuals_seasonal2020)
mtext("Year 2020", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(depth_residuals_seasonal2019, main = "")
qqline(depth_residuals_seasonal2019)
mtext("Year 2019", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(depth_residuals_seasonal2018, main = "")
qqline(depth_residuals_seasonal2018)
mtext("Year 2018", side = 1, line = 4, cex = 0.8, font = 3)


# Create histogram of electrical conductivity residuals to confirm or deny distribution
depth_seasonal_residuals_hist2023 <- ggplot(data = ts_depth_2023_gg, aes (x = depth_residuals_seasonal2023)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

depth_seasonal_residuals_hist2022 <- ggplot(data = ts_depth_2022_gg, aes (x = depth_residuals_seasonal2022)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

depth_seasonal_residuals_hist2021 <- ggplot(data = ts_depth_2021_gg, aes (x = depth_residuals_seasonal2021)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

depth_seasonal_residuals_hist2020 <- ggplot(data = ts_depth_2020_gg, aes (x = depth_residuals_seasonal2020)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

depth_seasonal_residuals_hist2019 <- ggplot(data = ts_depth_2019_gg, aes (x = depth_residuals_seasonal2019)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

depth_seasonal_residuals_hist2018 <- ggplot(data = ts_depth_2018_gg, aes (x = depth_residuals_seasonal2018)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))



# Create a list of plots
plot_list <- list(depth_seasonal_residuals_hist2018, depth_seasonal_residuals_hist2019, depth_seasonal_residuals_hist2020, depth_seasonal_residuals_hist2021, depth_seasonal_residuals_hist2022, depth_seasonal_residuals_hist2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))




######################################################################################

# Combine time series water temperature, electrical conductivity and water depth objects into separate data frames for each year
ts_combined_wtEC_2023 <- data.frame(
  Date2023 = water_quality_2023$Date_2023,
  water_temp_avg2023 = as.numeric(ts_water_temp_2023),
  electrical_conductivity2023 = as.numeric(ts_EC_25_2023),
  depth2023 = as.numeric(ts_depth_2023)
)

ts_combined_wtEC_2022 <- data.frame(
  Date2022 = water_quality_2018_2022$Date_2022,
  water_temp_avg2022 = as.numeric(ts_water_temp_2022),
  electrical_conductivity2022 = as.numeric(ts_EC_25_2022),
  depth2022 = as.numeric(ts_depth_2022)
)

ts_combined_wtEC_2021 <- data.frame(
  Date2021 = water_quality_2018_2022$Date_2021,
  water_temp_avg2021 = as.numeric(ts_water_temp_2021),
  electrical_conductivity2021 = as.numeric(ts_EC_25_2021),
  depth2021 = as.numeric(ts_depth_2021)
)

ts_combined_wtEC_2020 <- data.frame(
  Date2020 = water_quality_2018_2022$Date_2020,
  water_temp_avg2020 = as.numeric(ts_water_temp_2020),
  electrical_conductivity2020 = as.numeric(ts_EC_25_2020),
  depth2020 = as.numeric(ts_depth_2020)
)

ts_combined_wtEC_2019 <- data.frame(
  Date2019 = water_quality_2018_2022$Date_2019,
  water_temp_avg2019 = as.numeric(ts_water_temp_2019),
  electrical_conductivity2019 = as.numeric(ts_EC_25_2019),
  depth2019 = as.numeric(ts_depth_2019)
)

ts_combined_wtEC_2018 <- data.frame(
  Date2018 = water_quality_2018_2022$Date_2018,
  water_temp_avg2018 = as.numeric(ts_water_temp_2018),
  electrical_conductivity2018 = as.numeric(ts_EC_25_2018),
  depth2018 = as.numeric(ts_depth_2018)
)

# Create a new column with the abbreviated month name for ordering via lubridate for facet_wrap
ts_combined_wtEC_2023$Month_2023 <- month(ts_combined_wtEC_2023$Date2023, label = TRUE)


ts_combined_wtEC_2022$Month_2022 <- month(ts_combined_wtEC_2022$Date2022, label = TRUE)


ts_combined_wtEC_2021$Month_2021 <- month(ts_combined_wtEC_2021$Date2021, label = TRUE)


ts_combined_wtEC_2020$Month_2020 <- month(ts_combined_wtEC_2020$Date2020, label = TRUE)


ts_combined_wtEC_2019$Month_2019 <- month(ts_combined_wtEC_2019$Date2019, label = TRUE)


ts_combined_wtEC_2018$Month_2018 <- month(ts_combined_wtEC_2018$Date2018, label = TRUE)


# Plot for 2023
ggplot(data = ts_combined_wtEC_2023, aes(x = Date2023)) +
  geom_line(aes(y = water_temp_avg2023, color = "Water Temperature (°C)")) +
  geom_line(aes(y = electrical_conductivity2023, color = "Electrical Conductivity (μS/cm) at 25°C")) +
  labs(title = "Time Series Analysis for 2023",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2022, aes(x = Date2022)) +
  geom_line(aes(y = water_temp_avg2022, color = "Water Temperature (°C)")) +
  geom_line(aes(y = electrical_conductivity2022, color = "Electrical Conductivity (μS/cm) at 25°C")) +
  labs(title = "Time Series Analysis for 2022",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2021, aes(x = Date2021)) +
  geom_line(aes(y = water_temp_avg2021, color = "Water Temperature (°C)")) +
  geom_line(aes(y = electrical_conductivity2021, color = "Electrical Conductivity (μS/cm) at 25°C")) +
  labs(title = "Time Series Analysis for 2021",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2020, aes(x = Date2020)) +
  geom_line(aes(y = water_temp_avg2020, color = "Water Temperature (°C)")) +
  geom_line(aes(y = electrical_conductivity2020, color = "Electrical Conductivity (μS/cm) at 25°C")) +
  labs(title = "Time Series Analysis for 2020",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2019, aes(x = Date2019)) +
  geom_line(aes(y = water_temp_avg2019, color = "Water Temperature (°C)")) +
  geom_line(aes(y = electrical_conductivity2019, color = "Electrical Conductivity (μS/cm) at 25°C")) +
  labs(title = "Time Series Analysis for 2019",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2018, aes(x = Date2018)) +
  geom_line(aes(y = water_temp_avg2018, color = "Water Temperature (°C)")) +
  geom_line(aes(y = electrical_conductivity2018, color = "Electrical Conductivity (μS/cm) at 25°C")) +
  labs(title = "Time Series Analysis for 2018",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()



ggplot(data = ts_combined_wtEC_2023, aes(x = Date2023)) +
  geom_line(aes(y = depth2023, color = "Water Depth (m)")) +
  geom_line(aes(y = electrical_conductivity2023, color = "Electrical Conductivity at 25°C")) +
  labs(title = "Time Series Analysis for 2022",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2022, aes(x = Date2022)) +
  geom_line(aes(y = depth2022, color = "Water Depth (m)")) +
  geom_line(aes(y = electrical_conductivity2022, color = "Electrical Conductivity at 25°C")) +
  labs(title = "Time Series Analysis for 2022",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2021, aes(x = Date2021)) +
  geom_line(aes(y = depth2021, color = "Water Depth (m)")) +
  geom_line(aes(y = electrical_conductivity2021, color = "Electrical Conductivity at 25°C")) +
  labs(title = "Time Series Analysis for 2021",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2020, aes(x = Date2020)) +
  geom_line(aes(y = depth2020, color = "Water Depth (m)")) +
  geom_line(aes(y = electrical_conductivity2020, color = "Electrical Conductivity at 25°C")) +
  labs(title = "Time Series Analysis for 2020",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2019, aes(x = Date2019)) +
  geom_line(aes(y = depth2019, color = "Water Depth (m)")) +
  geom_line(aes(y = electrical_conductivity2019, color = "Electrical Conductivity at 25°C")) +
  labs(title = "Time Series Analysis for 2019",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()

ggplot(data = ts_combined_wtEC_2018, aes(x = Date2018)) +
  geom_line(aes(y = depth2018, color = "Water Depth (m)")) +
  geom_line(aes(y = electrical_conductivity2018, color = "Electrical Conductivity at 25°C")) +
  labs(title = "Time Series Analysis for 2018",
       x = "Month", y = "Value", color = "Water Parameter") +
  theme_minimal()



#######################################################################

# The effect of water depth on electrical conductivity

# Applying Log transformation to Electrical conductivity using mutate
View(water_quality_2018_2022)
transformed_water_quality_2018_2022 <- water_quality_2018_2022 %>%
  mutate(log_EC_25_2022 = log(EC_25_2022),
         log_EC_25_2021 = log(EC_25_2021),
         log_EC_25_2020 = log(EC_25_2020),
         log_EC_25_2019 = log(EC_25_2019),
         log_EC_25_2018 = log(EC_25_2018))


transformed_water_quality_2023 <- water_quality_2023 %>%
  mutate(log_EC_25_2023 = log(EC_25_avg))


View(transformed_water_quality_2023)
View(transformed_water_quality_2018_2022)

transformed_water_quality_2023$Date_2023 <- as.Date(transformed_water_quality_2023$Date_2023, format = "%d/%m/%Y")
transformed_water_quality_2018_2022$Date_2022 <- as.Date(transformed_water_quality_2018_2022$Date_2022, format = "%d/%m/%Y")
transformed_water_quality_2018_2022$Date_2021 <- as.Date(transformed_water_quality_2018_2022$Date_2021, format = "%d/%m/%Y")
transformed_water_quality_2018_2022$Date_2020 <- as.Date(transformed_water_quality_2018_2022$Date_2020, format = "%d/%m/%Y")
transformed_water_quality_2018_2022$Date_2019 <- as.Date(transformed_water_quality_2018_2022$Date_2019, format = "%d/%m/%Y")
transformed_water_quality_2018_2022$Date_2018 <- as.Date(transformed_water_quality_2018_2022$Date_2018, format = "%d/%m/%Y")


# Plot log Electrical Conductivity against temperature and Water depth

# log(electrical conductivity) against water depth

logec_depth2023 <- ggplot(transformed_water_quality_2023, aes(x = water_level_avg, y = log_EC_25_2023)) +
  geom_point(size = 1.0, color = "blue") +
  labs(x = "Water depth (m)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2023" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_depth2022 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2022, y = log_EC_25_2022)) +
  geom_point(size = 1.0, color = "blue") +
  labs(x = "Water depth (m)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2022" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_depth2021 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2021, y = log_EC_25_2021)) +
  geom_point(size = 1.0, color = "red") +
  labs(x = "Water depth (m)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2021" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_depth2020 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2020, y = log_EC_25_2020)) +
  geom_point(size = 1.0, color = "orange") +
  labs(x = "Water depth (m)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2020" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_depth2019 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2019, y = log_EC_25_2019)) +
  geom_point(size = 1.0, color = "purple") +
  labs(x = "Water depth (m)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2019" )  +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_depth2018 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2018, y = log_EC_25_2018)) +
  geom_point(size = 1.0, color = "gold") +
  labs(x = "Water depth (m)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2018" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(logec_depth2018, logec_depth2019, logec_depth2020, logec_depth2021, logec_depth2022, logec_depth2023)

# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))


# General Additive Model
gam_logEC_depth_2023_model <- gam(log_EC_25_2023 ~ s(water_level_avg), data = transformed_water_quality_2023, family = gaussian)
gam_logEC_depth_2022_model <- gam(log_EC_25_2022 ~ s(water_depth_2022), data = transformed_water_quality_2018_2022, family = gaussian)
gam_logEC_depth_2021_model <- gam(log_EC_25_2021 ~ s(water_depth_2021), data = transformed_water_quality_2018_2022, family = gaussian)
gam_logEC_depth_2020_model <- gam(log_EC_25_2020 ~ s(water_depth_2020), data = transformed_water_quality_2018_2022, family = gaussian)
gam_logEC_depth_2019_model <- gam(log_EC_25_2019 ~ s(water_depth_2019), data = transformed_water_quality_2018_2022, family = gaussian)
gam_logEC_depth_2018_model <- gam(log_EC_25_2018 ~ s(water_depth_2018), data = transformed_water_quality_2018_2022, family = gaussian)

summary(gam_logEC_depth_2023_model)
summary(gam_logEC_depth_2022_model)
summary(gam_logEC_depth_2021_model)
summary(gam_logEC_depth_2020_model)
summary(gam_logEC_depth_2019_model)
summary(gam_logEC_depth_2018_model)

# Get the predicted residual values from GAM models
predicted_EC_depth_values2023 <- predict(gam_logEC_depth_2023_model)
predicted_EC_depth_values2022 <- predict(gam_logEC_depth_2022_model)
predicted_EC_depth_values2021 <- predict(gam_logEC_depth_2021_model)
predicted_EC_depth_values2020 <- predict(gam_logEC_depth_2020_model)
predicted_EC_depth_values2019 <- predict(gam_logEC_depth_2019_model)
predicted_EC_depth_values2018 <- predict(gam_logEC_depth_2018_model)

# Get the residuals from GAM models for Q-Q line
logEC_residuals_depth2023 <- residuals(gam_logEC_depth_2023_model)
logEC_residuals_depth2022 <- residuals(gam_logEC_depth_2022_model)
logEC_residuals_depth2021 <- residuals(gam_logEC_depth_2021_model)
logEC_residuals_depth2020 <- residuals(gam_logEC_depth_2020_model)
logEC_residuals_depth2019 <- residuals(gam_logEC_depth_2019_model)
logEC_residuals_depth2018 <- residuals(gam_logEC_depth_2018_model)

# Calculate the observed residual values
logEC_residuals_depth1 <- transformed_water_quality_2023$log_EC_25_2023 - predicted_EC_depth_values2023
logEC_residuals_depth2 <- transformed_water_quality_2018_2022$log_EC_25_2022 - predicted_EC_depth_values2022
logEC_residuals_depth3 <- transformed_water_quality_2018_2022$log_EC_25_2021 - predicted_EC_depth_values2021
logEC_residuals_depth4 <- transformed_water_quality_2018_2022$log_EC_25_2020 - predicted_EC_depth_values2020
logEC_residuals_depth5 <- transformed_water_quality_2018_2022$log_EC_25_2019 - predicted_EC_depth_values2019
logEC_residuals_depth6 <- transformed_water_quality_2018_2022$log_EC_25_2018 - predicted_EC_depth_values2018

# Create the scatter plot of residuals against the predictor variable

res_logEC_depth2023 <- ggplot(transformed_water_quality_2023, aes(x = water_level_avg, y = logEC_residuals_depth1)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water depth (m)", y = "Residuals", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_depth2022 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2022, y = logEC_residuals_depth2)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water depth (m)", y = "Residuals", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_depth2021 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2021, y = logEC_residuals_depth3)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water depth (m)", y = "Residuals", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_depth2020 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2020, y = logEC_residuals_depth4)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water depth (m)", y = "Residuals", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_depth2019 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2019, y = logEC_residuals_depth5)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water depth (m)", y = "Residuals", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_depth2018 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_depth_2018, y = logEC_residuals_depth6)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water depth (m)", y = "Residuals", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(res_logEC_depth2018, res_logEC_depth2019, res_logEC_depth2020, res_logEC_depth2021, res_logEC_depth2022, res_logEC_depth2023)

# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# Q-Q plot = normality of residuals using residuals obtain from GAM models 
# set up the multi-panel plot
par(mfrow = c(2,3)) # 2 rows, 3 column

qqnorm(logEC_residuals_depth2023, main = "") 
qqline(logEC_residuals_depth2023)
mtext("Year 2023", side = 1, line = 4, cex = 0.8, font = 3)


qqnorm(logEC_residuals_depth2022, main = "")
qqline(logEC_residuals_depth2022)
mtext("Year 2022", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(logEC_residuals_depth2021, main = "")
qqline(logEC_residuals_depth2021)
mtext("Year 2021", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(logEC_residuals_depth2020, main = "")
qqline(logEC_residuals_depth2020)
mtext("Year 2020", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(logEC_residuals_depth2019, main = "")
qqline(logEC_residuals_depth2019)
mtext("Year 2019", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(logEC_residuals_depth2018, main = "")
qqline(logEC_residuals_depth2018)
mtext("Year 2018", side = 1, line = 4, cex = 0.8, font = 3)

# Create histogram of log EC residuals to confirm fat-tailed distribution
logEC_depth_residuals_hist2023 <- ggplot(data = transformed_water_quality_2023, aes (x = logEC_residuals_depth2023)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

logEC_depth_residuals_hist2022 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_depth2022)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

logEC_depth_residuals_hist2021 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_depth2021)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

logEC_depth_residuals_hist2020 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_depth2020)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

logEC_depth_residuals_hist2019 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_depth2019)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

logEC_depth_residuals_hist2018 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_depth2018)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(logEC_depth_residuals_hist2018, logEC_depth_residuals_hist2019, logEC_depth_residuals_hist2020, logEC_depth_residuals_hist2021, logEC_depth_residuals_hist2022, logEC_depth_residuals_hist2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))


# Plot log(electrical conductivity) against water temperature

logec_temp2023 <- ggplot(transformed_water_quality_2023, aes(x = water_temp_avg, y = log_EC_25_2023)) +
  geom_point(size = 1.0, color = "blue") +
  labs(x = "Water temperature (°C)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2023" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_temp2022 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2022, y = log_EC_25_2022)) +
  geom_point(size = 1.0, color = "blue") +
  labs(x = "Water temperature (°C)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2022" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_temp2021 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2021, y = log_EC_25_2021)) +
  geom_point(size = 1.0, color = "red") +
  labs(x = "Water temperature (°C)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2021" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_temp2020 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2020, y = log_EC_25_2020)) +
  geom_point(size = 1.0, color = "orange") +
  labs(x = "Water temperature (°C)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2020" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_temp2019 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2019, y = log_EC_25_2019)) +
  geom_point(size = 1.0, color = "purple") +
  labs(x = "Water temperature (°C)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2019" )  +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

logec_temp2018 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2018, y = log_EC_25_2018)) +
  geom_point(size = 1.0, color = "gold") +
  labs(x = "Water temperature (°C)", y = "Electrical Conductivity (Log Scale)", caption = "Year: 2018" ) +
  scale_y_continuous(limits = c(0.0, 5.5), breaks = seq(0.0, 5.5, 1.0)) +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(logec_temp2018, logec_temp2019, logec_temp2020, logec_temp2021, logec_temp2022, logec_temp2023)

# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))


# General Additive Model
gam_logEC_temp_2023_model <- gam(log_EC_25_2023 ~ s(water_temp_avg), data = transformed_water_quality_2023, family = gaussian)
gam_logEC_temp_2022_model <- gam(log_EC_25_2022 ~ s(water_temp_2022), data = transformed_water_quality_2018_2022, family = gaussian)
gam_logEC_temp_2021_model <- gam(log_EC_25_2021 ~ s(water_temp_2021), data = transformed_water_quality_2018_2022, family = gaussian)
gam_logEC_temp_2020_model <- gam(log_EC_25_2020 ~ s(water_temp_2020), data = transformed_water_quality_2018_2022, family = gaussian)
gam_logEC_temp_2019_model <- gam(log_EC_25_2019 ~ s(water_temp_2019), data = transformed_water_quality_2018_2022, family = gaussian)
gam_logEC_temp_2018_model <- gam(log_EC_25_2018 ~ s(water_temp_2018), data = transformed_water_quality_2018_2022, family = gaussian)

summary(gam_logEC_temp_2023_model)
summary(gam_logEC_temp_2022_model)
summary(gam_logEC_temp_2021_model)
summary(gam_logEC_temp_2020_model)
summary(gam_logEC_temp_2019_model)
summary(gam_logEC_temp_2018_model)

# Get the predicted residual values from GAM models
predicted_EC_temp_values2023 <- predict(gam_logEC_temp_2023_model)
predicted_EC_temp_values2022 <- predict(gam_logEC_temp_2022_model)
predicted_EC_temp_values2021 <- predict(gam_logEC_temp_2021_model)
predicted_EC_temp_values2020 <- predict(gam_logEC_temp_2020_model)
predicted_EC_temp_values2019 <- predict(gam_logEC_temp_2019_model)
predicted_EC_temp_values2018 <- predict(gam_logEC_temp_2018_model)

# Get the residuals from GAM models for Q-Q line
logEC_residuals_temp2023 <- residuals(gam_logEC_temp_2023_model)
logEC_residuals_temp2022 <- residuals(gam_logEC_temp_2022_model)
logEC_residuals_temp2021 <- residuals(gam_logEC_temp_2021_model)
logEC_residuals_temp2020 <- residuals(gam_logEC_temp_2020_model)
logEC_residuals_temp2019 <- residuals(gam_logEC_temp_2019_model)
logEC_residuals_temp2018 <- residuals(gam_logEC_temp_2018_model)

# Calculate the observed residual values
logEC_residuals_temp1 <- transformed_water_quality_2023$log_EC_25_2023 - predicted_EC_temp_values2023
logEC_residuals_temp2 <- transformed_water_quality_2018_2022$log_EC_25_2022 - predicted_EC_temp_values2022
logEC_residuals_temp3 <- transformed_water_quality_2018_2022$log_EC_25_2021 - predicted_EC_temp_values2021
logEC_residuals_temp4 <- transformed_water_quality_2018_2022$log_EC_25_2020 - predicted_EC_temp_values2020
logEC_residuals_temp5 <- transformed_water_quality_2018_2022$log_EC_25_2019 - predicted_EC_temp_values2019
logEC_residuals_temp6 <- transformed_water_quality_2018_2022$log_EC_25_2018 - predicted_EC_temp_values2018

# Create the scatter plot of residuals against the predictor variable

res_logEC_temp2023 <- ggplot(transformed_water_quality_2023, aes(x = water_temp_avg, y = logEC_residuals_temp1)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_temp2022 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2022, y = logEC_residuals_temp2)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_temp2021 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2021, y = logEC_residuals_temp3)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_temp2020 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2020, y = logEC_residuals_temp4)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_temp2019 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2019, y = logEC_residuals_temp5)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

res_logEC_temp2018 <- ggplot(transformed_water_quality_2018_2022, aes(x = water_temp_2018, y = logEC_residuals_temp6)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(res_logEC_temp2018, res_logEC_temp2019, res_logEC_temp2020, res_logEC_temp2021, res_logEC_temp2022, res_logEC_temp2023)

# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# Q-Q plot = normality of residuals using residuals obtain from GAM models 
# set up the multi-panel plot
par(mfrow = c(2,3)) # 2 rows, 3 column

qqnorm(logEC_residuals_temp2023, main = "") 
qqline(logEC_residuals_temp2023)
mtext("Year 2023", side = 1, line = 4, cex = 0.8, font = 3)


qqnorm(logEC_residuals_temp2022, main = "")
qqline(logEC_residuals_temp2022)
mtext("Year 2022", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(logEC_residuals_temp2021, main = "")
qqline(logEC_residuals_temp2021)
mtext("Year 2021", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(logEC_residuals_temp2020, main = "")
qqline(logEC_residuals_temp2020)
mtext("Year 2020", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(logEC_residuals_temp2019, main = "")
qqline(logEC_residuals_temp2019)
mtext("Year 2019", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(logEC_residuals_temp2018, main = "")
qqline(logEC_residuals_temp2018)
mtext("Year 2018", side = 1, line = 4, cex = 0.8, font = 3)

# Create histogram of log EC residuals to confirm fat-tailed distribution
logEC_temp_residuals_hist2023 <- ggplot(data = transformed_water_quality_2023, aes (x = logEC_residuals_temp2023)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

logEC_temp_residuals_hist2022 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_temp2022)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

logEC_temp_residuals_hist2021 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_temp2021)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

logEC_temp_residuals_hist2020 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_temp2020)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

logEC_temp_residuals_hist2019 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_temp2019)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

logEC_temp_residuals_hist2018 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = logEC_residuals_temp2018)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(logEC_temp_residuals_hist2018, logEC_temp_residuals_hist2019, logEC_temp_residuals_hist2020, logEC_temp_residuals_hist2021, logEC_temp_residuals_hist2022, logEC_temp_residuals_hist2023)


# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))


##############################################################################


# Water temperature against water depth

wt_depth2023 <- ggplot(water_quality_2023, aes(x = water_level_avg, y = water_temp_avg)) +
  geom_point(size = 1.0, color = "blue") +
  labs(x = "Water depth (m)", y = "Water temperature (°C)", caption = "Year: 2023" ) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5))  +
  theme(plot.caption = element_text(face = "bold"))


wt_depth2022 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2022, y = water_temp_2022)) +
  geom_point(size = 1.0, color = "blue") +
  labs(x = "Water depth (m)", y = "Water temperature (°C)", caption = "Year: 2022" ) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5))  +
  theme(plot.caption = element_text(face = "bold"))


wt_depth2021 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2021, y = water_temp_2021)) +
  geom_point(size = 1.0, color = "red") +
  labs(x = "Water depth (m)", y = "Water temperature (°C)", caption = "Year: 2021" ) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5))  +
  theme(plot.caption = element_text(face = "bold"))


wt_depth2020 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2020, y = water_temp_2020)) +
  geom_point(size = 1.0, color = "orange") +
  labs(x = "Water depth (m)", y = "Water temperature (°C)", caption = "Year: 2020" ) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5))  +
  theme(plot.caption = element_text(face = "bold"))

wt_depth2019 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2019, y = water_temp_2019)) +
  geom_point(size = 1.0, color = "purple") +
  labs(x = "Water depth (m)", y = "Water temperature (°C)", caption = "Year: 2019" ) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5))  +
  theme(plot.caption = element_text(face = "bold"))

wt_depth2018 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2018, y = water_temp_2018)) +
  geom_point(size = 1.0, color = "gold") +
  labs(x = "Water depth (m)", y = "Water temperature (°C)", caption = "Year: 2018" ) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5))  +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(wt_depth2018, wt_depth2019, wt_depth2020, wt_depth2021, wt_depth2022, wt_depth2023)

# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))



# Create a GAM using water temp
temp_depth_gam_model2023 <- gam(water_temp_avg ~ s(water_level_avg),
                                data = water_quality_2023, family = gaussian)
temp_depth_gam_model2022 <- gam(water_temp_2022 ~ s(water_depth_2022),
                         data = water_quality_2018_2022, family = gaussian)
temp_depth_gam_model2021 <- gam(water_temp_2021 ~ s(water_depth_2021),
                         data = water_quality_2018_2022, family = gaussian)
temp_depth_gam_model2020 <- gam(water_temp_2020 ~ s(water_depth_2020),
                         data = water_quality_2018_2022, family = gaussian)
temp_depth_gam_model2019 <- gam(water_temp_2019 ~ s(water_depth_2019),
                         data = water_quality_2018_2022, family = gaussian)
temp_depth_gam_model2018 <- gam(water_temp_2018 ~ s(water_depth_2018),
                         data = water_quality_2018_2022, family = gaussian)


# Print the summary of the GAM
summary(temp_depth_gam_model2023)
summary(temp_depth_gam_model2022)
summary(temp_depth_gam_model2021)
summary(temp_depth_gam_model2020)
summary(temp_depth_gam_model2019)
summary(temp_depth_gam_model2018)


# Get the predicted residual values from GAM models
predicted_temp_depth_values2023 <- predict(temp_depth_gam_model2023)
predicted_temp_depth_values2022 <- predict(temp_depth_gam_model2022)
predicted_temp_depth_values2021 <- predict(temp_depth_gam_model2021)
predicted_temp_depth_values2020 <- predict(temp_depth_gam_model2020)
predicted_temp_depth_values2019 <- predict(temp_depth_gam_model2019)
predicted_temp_depth_values2018 <- predict(temp_depth_gam_model2018)

# Get the residuals from GAM models for Q-Q line and histogram
temp_residuals_depth2023 <- residuals(temp_depth_gam_model2023)
temp_residuals_depth2022 <- residuals(temp_depth_gam_model2022)
temp_residuals_depth2021 <- residuals(temp_depth_gam_model2021)
temp_residuals_depth2020 <- residuals(temp_depth_gam_model2020)
temp_residuals_depth2019 <- residuals(temp_depth_gam_model2019)
temp_residuals_depth2018 <- residuals(temp_depth_gam_model2018)

# Calculate the observed residual values for scatter plot
temp_residuals_depth1 <- water_quality_2023$water_temp_avg - predicted_temp_depth_values2023
temp_residuals_depth2 <- water_quality_2018_2022$water_temp_2022 - predicted_temp_depth_values2022
temp_residuals_depth3 <- water_quality_2018_2022$water_temp_2021 - predicted_temp_depth_values2021
temp_residuals_depth4 <- water_quality_2018_2022$water_temp_2020 - predicted_temp_depth_values2020
temp_residuals_depth5 <- water_quality_2018_2022$water_temp_2019 - predicted_temp_depth_values2019
temp_residuals_depth6 <- water_quality_2018_2022$water_temp_2018 - predicted_temp_depth_values2018

# Create the scatter plot of residuals against the predictor variable

res_temp_depth2023 <- ggplot(water_quality_2023, aes(x = water_level_avg, y = temp_residuals_depth1)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_depth2022 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2022, y = temp_residuals_depth2)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_depth2021 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2021, y = temp_residuals_depth3)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_depth2020 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2020, y = temp_residuals_depth4)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_depth2019 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2019, y = temp_residuals_depth5)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))

res_temp_depth2018 <- ggplot(water_quality_2018_2022, aes(x = water_depth_2018, y = temp_residuals_depth6)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, shape = 21, color = "black", alpha = 0.75) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Water temperature (°C)", y = "Residuals", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))

# Create a list of plots
plot_list <- list(res_temp_depth2018, res_temp_depth2019, res_temp_depth2020, res_temp_depth2021, res_temp_depth2022, res_temp_depth2023)

# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))

# Q-Q plot = normality of residuals using residuals obtain from GAM models 
# set up the multi-panel plot
par(mfrow = c(2,3)) # 2 rows, 3 column

qqnorm(temp_residuals_depth2023, main = "") 
qqline(temp_residuals_depth2023)
mtext("Year 2023", side = 1, line = 4, cex = 0.8, font = 3)


qqnorm(temp_residuals_depth2022, main = "")
qqline(temp_residuals_depth2022)
mtext("Year 2022", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(temp_residuals_depth2021, main = "")
qqline(temp_residuals_depth2021)
mtext("Year 2021", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(temp_residuals_depth2020, main = "")
qqline(temp_residuals_depth2020)
mtext("Year 2020", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(temp_residuals_depth2019, main = "")
qqline(temp_residuals_depth2019)
mtext("Year 2019", side = 1, line = 4, cex = 0.8, font = 3)

qqnorm(temp_residuals_depth2018, main = "")
qqline(temp_residuals_depth2018)
mtext("Year 2018", side = 1, line = 4, cex = 0.8, font = 3)

# Checking summary of the column
summary(water_quality_2018_2022$temp_residuals_depth2022)

# Checking unique values in the column
unique(temp_residuals_depth2022)

# Create histogram of log EC residuals to confirm fat-tailed distribution
temp_depth_residuals_hist2023 <- ggplot(data = water_quality_2023, aes (x = temp_residuals_depth2023)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 50) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2023") +
  theme(plot.caption = element_text(face = "bold"))

  
temp_depth_residuals_hist2022 <- ggplot(data = water_quality_2018_2022, aes (x = temp_residuals_depth2022)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 50) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2022") +
  theme(plot.caption = element_text(face = "bold"))


temp_depth_residuals_hist2021 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = temp_residuals_depth2021)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 50) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2021") +
  theme(plot.caption = element_text(face = "bold"))
print(temp_depth_residuals_hist2023)

temp_depth_residuals_hist2020 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = temp_residuals_depth2020)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 50) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2020") +
  theme(plot.caption = element_text(face = "bold"))
print(temp_depth_residuals_hist2023)

temp_depth_residuals_hist2019 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = temp_residuals_depth2019)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 50) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2019") +
  theme(plot.caption = element_text(face = "bold"))
print(temp_depth_residuals_hist2023)

temp_depth_residuals_hist2018 <- ggplot(data = transformed_water_quality_2018_2022, aes (x = temp_residuals_depth2018)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 50) +
  labs(x = "Residuals", y = "Frequency", caption = "Year: 2018") +
  theme(plot.caption = element_text(face = "bold"))
print(temp_depth_residuals_hist2018)

# Create a list of plots
plot_list <- list(temp_depth_residuals_hist2018, temp_depth_residuals_hist2019, temp_depth_residuals_hist2020, temp_depth_residuals_hist2021, temp_depth_residuals_hist2022, temp_depth_residuals_hist2023)

# Combine all the plots and add legends
combined_plot <- do.call(grid.arrange, c(plot_list, nrow = 3.5))


