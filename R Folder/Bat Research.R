library(googlesheets4)
library(lubridate)
library(hms)
library(ggplot2)
library(scales)

austinweatherdata = read.csv("Austin,United States 2023-07-01 to 2024-07-01.csv")
batdata = read.csv("best_bats.csv")
originalbatdata = read.csv("best_bats.csv")
write.csv(batdata2, "batdatamerged.csv", row.names=FALSE)

#Histogram cloudcover data for june 2024
June_2024_data = austinweatherdata[337:366, ]

June_2024_data$index <- 1:nrow(June_2024_data)

ggplot(June_2024_data, aes(x = index, y = cloudcover)) +
  geom_histogram(stat="identity") + 
  labs(title = "Cloud Cover in June 2024",
       x = "June",
       y = "Cloud Cover (% of sky)") +
  theme_classic() +
  scale_x_continuous(breaks = seq(1, 30, by = 1)) 

#Line cloudcover data from 7/1/2023 to 7/1/2024
austinweatherdata$date <- as.Date(austinweatherdata$date)

ggplot(austinweatherdata, aes(x = date, y = cloudcover)) +
  geom_line(col = "skyblue", lwd = 0.8) + 
  labs(title = "Cloud Cover over Austin from 7/1/2023 to 7/1/2024",
       x = "Date", y = "Cloud Cover (% of Sky)") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.major = element_line(color = "grey80"), 
    panel.grid.minor = element_blank() 
  )

#Determine which month has the most cloud cover
austinweatherdata$month = floor_date(austinweatherdata$date, "month")

monthly_cloudcover = austinweatherdata %>%
  group_by(month) %>%
  summarise(total_cloudcover = sum(cloudcover, na.rm = TRUE))

max_cloudcover_month = monthly_cloudcover %>%
  filter(total_cloudcover == max(total_cloudcover))

print(max_cloudcover_month)

#Filtering out all rows containing "CongressScouts"
nocongressScouts = dplyr::filter(batdata, !grepl("CongressScouts", all_data))
batdata = nocongressScouts
batdata = batdata[-30, ]

#Plotting bat departure
batdata$utc_date <- as.Date(batdata$utc_date)

ggplot(batdata, aes(x = utc_date, y = as.POSIXct(utc_time, format = "%H:%M"))) +
  geom_line(color = "black", alpha=0.5) +
  geom_jitter() +
  labs(title = "Bat Departure Time by Date",
       x = "Date",
       y = "Departure Time (UTC)") +
  theme_minimal() 

#Making a separate df for cloud cover
clouddata <- austinweatherdata %>%
  transmute(datetime = datetime, cloudcover = cloudcover)

#Merge batdata & clouddata
clouddata = clouddata %>%
  rename(utc_date = datetime)

batdata2 = batdata

batdata2 = batdata2 %>%
  full_join(clouddata,
            by=c('utc_date' = 'date'))

clouddata$date <- as.Date(clouddata$utc_date)
