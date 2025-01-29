
## load library
library(caret)
library(tidyverse)
library(mlbench)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)



mydatatc4 <- read_csv("WaterData_for_ML/TC4.csv")
mydatatc12 <- read_csv("WaterData_for_ML/TC12.csv")
mydatat7A <- read_csv("WaterData_for_ML/TC7A.csv") ## have missing value
mydatatW4A <- read_csv("WaterData_for_ML/TW4A.csv") #  missing values 176 Row
 

mydata <- mydatat7A



head(mydata)
View(mydata)
class(mydata$datetime)



mydata$hour <- hour(mydata$datetime)
mydata$minute <- minute(mydata$datetime)
mydata$second <- second(mydata$datetime)
print(mydata)


# create min-max Scaling  --------------------------------------------
min_max_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# create Z- score -------------------------------------------- 
z_score_normalization <- function(x) {
  return((x - mean(x)) / sd(x))
}

# Do Z-score with colum -> value -------------------------------------------- 
mydata_normalized <- mydata %>%
  mutate(value_normalized = z_score_normalization(value))

# Do Min-Max Scaling with colum value -------------------------------------------- 
mydata_scaled <- mydata %>%
  mutate(value_scaled = min_max_scaling(value))
print(mydata_scaled)

# plot original_plot MinMax Scaling -------------------------------------------- 

original_plot_m <- ggplot(mydata, aes(x = value, y = 1)) +
  geom_point(color = "blue") +
  ggtitle("Original Data")

# plotscaled_plot MinMax Scaling  --------------------------------------------
scaled_plot <- ggplot(mydata_scaled, aes(x = value_scaled, y = 1)) +
  geom_point(color = "red") +
  ggtitle("Scaled Data")

# plot original_plot Z-score --------------------------------------------

original_plot_z <- ggplot(mydata, aes(x = value, y = 1)) +
  geom_point(color = "blue") +
  ggtitle("Original Data")


# plot normalized_plot Z-score --------------------------------------------
normalized_plot <- ggplot(mydata_normalized, aes(x = value_normalized, y = 1)) +
  geom_point(color = "red") +
  ggtitle("Normalized Data")

library(patchwork)
normalized_plot + original_plot_z
#-------------------------------------------------------------------------
library(ggplot2)

# พล็อตข้อมูลในรูปแบบ scater หลังจากการทำ Z-score normalization
z_score_plot <- ggplot(mydata_normalized, aes(x = datetime, y = value_normalized)) +
  geom_point(color = "green") +
  ggtitle("Z-Score Normalized Data")

# แสดงพล็อต
z_score_plot

#-------------------------------------------------------------------------
library(ggplot2)

# พล็อต Time series plot ของข้อมูลที่ผ่านการ Z-score normalization
time_series_plot <- ggplot(mydata_normalized, aes(x = datetime, y = value_normalized)) +
  geom_line(color = "blue") +
  ggtitle("Time Series Plot of Z-Score Normalized Data") +
  xlab("Time") +
  ylab("Z-Score Normalized Data")


z_score_plot

library(ggplot2)

# พล็อต scater plot ของข้อมูลที่ผ่านการ Z-score normalization
scater_plot <- ggplot(mydata_normalized, aes(x = datetime, y = value_normalized)) +
  geom_point(color = "green") +
  ggtitle("Scater Plot with Time Series Line") +
  xlab("Time") +
  ylab("Z-Score Normalized Data")

# เพิ่ม Time series plot ของข้อมูลที่ผ่านการ Z-score normalization เป็นเส้น
scater_plot + geom_line(aes(y = value_normalized), color = "blue")

  