# Read CSV and format 'Date' in a recognized date format

water_temp_years <- read.csv("water_temp_years.csv")
water_temp_years$Date <- as.Date(water_temp_years$Date, format = "%d/%m/%Y")
View(water_temp_years)