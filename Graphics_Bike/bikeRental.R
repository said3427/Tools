# packages --------------------------------------------------
library(tidyverse)
library(rsample) # data splitting
library(randomForest) # basic implementation
library(ranger) # a faster implementation of randomForest
library(caret) # an aggregator package for performing many machine learning models
library(ggthemes)
library(scales)
library(wesanderson)
library(styler)

bike <- readr::read_csv("data/day.csv")


# this recodes the weekday variable into a character variable
# test 
# bike %>%
#   mutate(
#     weekday_chr =
#       case_when(
#         weekday == 0 ~ "Sunday",
#         weekday == 1 ~ "Monday",
#         weekday == 2 ~ "Tuesday",
#         weekday == 3 ~ "Wednesday",
#         weekday == 4 ~ "Thursday",
#         weekday == 5 ~ "Friday",
#         weekday == 6 ~ "Saturday",
#         TRUE ~ "other")) %>% 
#     dplyr::count(weekday, weekday_chr) %>%
#     tidyr::spread(weekday, n)

# assign
bike <- bike %>%
  mutate(
    weekday_chr =
      case_when(
        weekday == 0 ~ "Sunday",
        weekday == 1 ~ "Monday",
        weekday == 2 ~ "Tuesday",
        weekday == 3 ~ "Wednesday",
        weekday == 4 ~ "Thursday",
        weekday == 5 ~ "Friday",
        weekday == 6 ~ "Saturday",
        TRUE ~ "other"))

# verify
# bike %>% 
#   dplyr::count(weekday, weekday_chr) %>% 
#   tidyr::spread(weekday, n)

# Weekdays (factor) ---

# test factor variable
# bike %>%
#   mutate(
#     weekday_fct = factor(x = weekday,
#              levels = c(0,1,2,3,4,5,6),
#              labels = c("Sunday",
#                        "Monday",
#                        "Tuesday",
#                        "Wednesday",
#                        "Thursday",
#                        "Friday",
#                        "Saturday"))) %>%
#   dplyr::count(weekday, weekday_fct) %>%
#   tidyr::spread(weekday, n)

# assign factor variable
bike <- bike %>%
  mutate(
    weekday_fct = factor(x = weekday,
             levels = c(0,1,2,3,4,5,6),
             labels = c("Sunday",
                       "Monday",
                       "Tuesday",
                       "Wednesday",
                       "Thursday",
                       "Friday",
                       "Saturday")))

# verify factor variable
# bike %>% 
#   dplyr::count(weekday, weekday_fct) %>% 
#   tidyr::spread(weekday, n)


# Holidays ----
# test
# bike %>%
#   mutate(holiday_chr =
#       case_when(
#         holiday == 0 ~ "Non-Holiday",
#         holiday == 1 ~ "Holiday")) %>% 
#   dplyr::count(holiday, holiday_chr) %>%
#   tidyr::spread(holiday, n)

# assign
bike <- bike %>%
  mutate(holiday_chr =
      case_when(
        holiday == 0 ~ "Non-Holiday",
        holiday == 1 ~ "Holiday"))

# verify
# bike %>%
#   dplyr::count(holiday, holiday_chr) %>%
#   tidyr::spread(holiday, n)

# test
# bike %>%
#   mutate(
#     holiday_fct = factor(x = holiday,
#              levels = c(0,1),
#              labels = c("Non-Holiday",
#                        "Holiday"))) %>% 
#     dplyr::count(holiday, holiday_fct) %>%
#     tidyr::spread(holiday, n)

# assign
bike <- bike %>%
  mutate(
    holiday_fct = factor(x = holiday,
             levels = c(0,1),
             labels = c("Non-Holiday",
                       "Holiday")))

# # verify
# bike %>%
#   dplyr::count(holiday_chr, holiday_fct) %>%
#   tidyr::spread(holiday_chr, n)

# Working days ----
# test
 # bike %>%
 #  mutate(
 #    workingday_chr =
 #      case_when(
 #        workingday == 0 ~ "Non-Working Day",
 #        workingday == 1 ~ "Working Day",
 #        TRUE ~ "other")) %>% 
 #    dplyr::count(workingday, workingday_chr) %>%
 #    tidyr::spread(workingday, n)

# assign
 bike <- bike %>%
  mutate(
    workingday_chr =
      case_when(
        workingday == 0 ~ "Non-Working Day",
        workingday == 1 ~ "Working Day",
        TRUE ~ "other")) 
 
 # verify
 # bike %>% 
 #    dplyr::count(workingday, workingday_chr) %>%
 #    tidyr::spread(workingday, n)
   
# test
# bike %>%
#   mutate(
#     workingday_fct = factor(x = workingday,
#              levels = c(0,1),
#              labels = c("Non-Working Day",
#                        "Working Day"))) %>%
#   dplyr::count(workingday, workingday_fct) %>%
#   tidyr::spread(workingday, n)

# assign
bike <- bike %>%
  mutate(
    workingday_fct = factor(x = workingday,
             levels = c(0,1),
             labels = c("Non-Working Day",
                       "Working Day")))

# verify
# bike %>%
#   dplyr::count(workingday_chr, workingday_fct) %>%
#   tidyr::spread(workingday_chr, n)


# Seasons
bike <- bike %>%
  mutate(
    season_chr =
      case_when(
        season == 1 ~ "Spring",
        season == 2 ~ "Summer",
        season == 3 ~ "Fall",
        season == 4 ~ "Winter",
        TRUE ~ "other"
      ))

# test
# bike %>%
#   mutate(
#     season_fct = factor(x = season,
#              levels = c(1, 2, 3, 4),
#              labels = c("Spring",
#                        "Summer",
#                        "Fall",
#                        "Winter"))) %>%
#   dplyr::count(season_chr, season_fct) %>%
#   tidyr::spread(season_chr, n)

# assign
bike <- bike %>%
  mutate(
    season_fct = factor(x = season,
             levels = c(1, 2, 3, 4),
             labels = c("Spring",
                       "Summer",
                       "Fall",
                       "Winter"))) 

# verify
# bike %>%
#   dplyr::count(season_chr, season_fct) %>%
#   tidyr::spread(season_chr, n)


# Weather situation ----
# test
# bike %>%
#   mutate(
#     weathersit_chr =
#       case_when(
#         weathersit == 1 ~ "Good",
#         weathersit == 2 ~ "Clouds/Mist",
#         weathersit == 3 ~ "Rain/Snow/Storm",
#         TRUE ~ "other")) %>% 
#   dplyr::count(weathersit, weathersit_chr) %>%
#   tidyr::spread(weathersit, n)

# assign
bike <- bike %>%
  mutate(
    weathersit_chr =
      case_when(
        weathersit == 1 ~ "Good",
        weathersit == 2 ~ "Clouds/Mist",
        weathersit == 3 ~ "Rain/Snow/Storm"))

# verify
# bike %>% 
#   dplyr::count(weathersit, weathersit_chr) %>%
#   tidyr::spread(weathersit, n)

# test
# bike %>%
#   mutate(
#     weathersit_fct = factor(x = weathersit,
#              levels = c(1, 2, 3),
#              labels = c("Good",
#                        "Clouds/Mist",
#                        "Rain/Snow/Storm"))) %>%
#   dplyr::count(weathersit, weathersit_fct) %>%
#   tidyr::spread(weathersit, n)

# assign 
bike <- bike %>%
  mutate(
    weathersit_fct = factor(x = weathersit,
                       levels = c(1, 2, 3),
                       labels = c("Good",
                                 "Clouds/Mist",
                                 "Rain/Snow/Storm")))
# verify
# bike %>%
#   dplyr::count(weathersit_chr, weathersit_fct) %>%
#   tidyr::spread(weathersit_chr, n)


# Months ----
# huge shoutout to Thomas Mock over at RStudio for showing me 
# lubridate::month() (and stopping my case_when() obsession)
# https://twitter.com/thomas_mock/status/1113105497480183818

# test 
# bike %>% 
#   mutate(month_ord = 
#            lubridate::month(mnth, label = TRUE)) %>% 
#   dplyr::count(month_ord, mnth) %>% 
#   tidyr::spread(month_ord, n)

# assign
bike <- bike %>% 
  mutate(month_ord = 
           lubridate::month(mnth, label = TRUE))

# verify
# bike %>% 
#   dplyr::count(month_ord, mnth) %>% 
#   tidyr::spread(month_ord, n)
  

# test
# bike %>%
#   mutate(
#     month_fct = factor(x = mnth,
#              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
#              labels = c("January", "February", "March", "April", "May",
#                         "June", "July", "August", "September", "October",
#                         "November", "December"))) %>%
#   dplyr::count(mnth, month_fct) %>%
#   tidyr::spread(month_fct, n)

# assign
bike <- bike %>%
  mutate(
    month_fct = factor(x = mnth,
             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
             labels = c("January", "February", "March", "April", "May",
                        "June", "July", "August", "September", "October",
                        "November", "December")))

# verify
# bike %>% 
#   dplyr::count(month_chr, month_fct) %>%
#   tidyr::spread(month_fct, n)

# Year ----
# test
# bike %>%
#   mutate(
#     yr_chr =
#       case_when(
#         yr == 0 ~ "2011",
#         yr == 1 ~ "2012",
#         TRUE ~ "other")) %>% 
#     dplyr::count(yr, yr_chr) %>%
#     tidyr::spread(yr, n)

# assign
bike <- bike %>%
  mutate(
    yr_chr =
      case_when(
        yr == 0 ~ "2011",
        yr == 1 ~ "2012"))
# verify
# bike %>%
#     dplyr::count(yr, yr_chr) %>%
#     tidyr::spread(yr, n)

# test
# bike %>%
#   mutate(
#     yr_fct = factor(x = yr,
#              levels = c(0, 1),
#              labels = c("2011",
#                        "2012"))) %>%
#   dplyr::count(yr, yr_fct) %>%
#   tidyr::spread(yr, n)

# assign
bike <- bike %>%
  mutate(
    yr_fct = factor(x = yr,
             levels = c(0, 1),
             labels = c("2011",
                       "2012")))
# verify
# bike %>%
#   dplyr::count(yr_chr, yr_fct) %>%
#   tidyr::spread(yr_chr, n)

# normalize temperatures ----
bike <- bike %>%
  mutate(temp = as.integer(temp * (39 - (-8)) + (-8)))

bike <- bike %>%
  mutate(atemp = atemp * (50 - (16)) + (16))

# ~ windspeed ----
bike <- bike %>%
  mutate(windspeed = as.integer(67 * bike$windspeed))

# ~ humidity ----
bike <- bike %>%
  mutate(hum = as.integer(100 * bike$hum))

# ~ convert to date ----
bike <- bike %>%
  mutate(dteday = as.Date(dteday))

# check df
# bike %>% dplyr::glimpse(78)

# rename the data frame so these don't get confused
BikeData <- bike

# reorganize variables for easier inspection

BikeData <- BikeData %>% 
  dplyr::select(
    dplyr::starts_with("week"),
    dplyr::starts_with("holi"),
    dplyr::starts_with("seas"),
    dplyr::starts_with("work"),
    dplyr::starts_with("month"),
    dplyr::starts_with("yr"),
    dplyr::starts_with("weath"),
    dplyr::everything())



    BikeDplyrSummary <- BikeData %>%
  select(temp, atemp, hum, windspeed, casual, registered, cnt) %>%
  summarise_each(list(
    min = ~min,
    q25 = ~quantile(., 0.25),
    median = ~median,
    q75 = ~quantile(., 0.75),
    max = ~max,
    mean = ~mean,
    sd = ~sd
  )) %>%
  gather(stat, val) %>%
  separate(stat, 
           into = c("var", "stat"), 
           sep = "_") %>%
  spread(stat, val) %>%
  select(var, min, q25, median, q75, max, mean, sd)

knitr::kable(BikeDplyrSummary)




BikeSkimrSummary <- bike %>%
  skimr::skim_to_wide() %>%
  dplyr::select(type,
    variable,
    missing,
    complete,
    min,
    max,
    mean,
    sd,
    median = p50,
    hist)
knitr::kable(BikeSkimrSummary)





BikeMosaicInspect <- mosaic::inspect(BikeData)
# categorical
knitr::kable(BikeMosaicInspect$categorical)


knitr::kable(BikeMosaicInspect$Date)


knitr::kable(BikeMosaicInspect$quantitative)





ggRentalsByTemp <- BikeData %>% 
  ggplot(aes(y = cnt, 
                 x = temp, 
                 color = weekday_fct)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(se = FALSE,
              show.legend = FALSE) +
  facet_grid(~weekday_fct) +
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("Bike Rentals") +
  xlab("Temperature (°C)") +
  ggtitle("Bike Rental Volume By Temperature")
ggRentalsByTemp


`geom_smooth()` using method = 'loess' and formula 'y ~ x'



# ggRentalVolByWindSpeed ----
ggRentalVolByWindSpeed <- ggplot(bike) +
  geom_point(aes(y = cnt, 
                 x = windspeed, 
                 color = weekday_fct),
             show.legend = FALSE) +
  facet_grid(~weekday_fct) +
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ylab("Bike Rentals") +
  xlab("Windspeed") +
  ggtitle("Rental Volume By Windspeed")
ggRentalVolByWindSpeed




ggRentalVolByHoliday <- ggplot(BikeData) +
  geom_density(aes(x = cnt,
                   fill = holiday_chr), 
                   alpha = 0.2) +
  scale_fill_brewer(palette = "Paired") +
  
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  labs(title = "Bike Rental Density By Holiday",
               fill = "Holiday",
               x = "Average Bike Rentals",
               y = "Density")

ggRentalVolByHoliday


