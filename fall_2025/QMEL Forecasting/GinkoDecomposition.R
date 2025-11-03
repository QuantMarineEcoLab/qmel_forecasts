library(fpp3)

#Load data
ginko<-read.csv("GingkoTreeData.csv")


str(ginko)

#Fill missing  years

ginko<-ginko %>%
  complete(Year = full_seq(Year, 1)) 

ginko$Julian.day[is.na(ginko$Julian.day)]<-round(mean(na.omit(ginko$Julian.day)),0)

ginko<-ginko|>mutate(
  # Convert Julian day to a proper Date (using the Year column)
  date = as.Date(Julian.day - 1, origin = paste0(Year, "-01-01")),
  # Extract month name (e.g., "January") or number
  month = month(date, label = TRUE, abbr = FALSE)  # change to TRUE for short names
)|>select(Year,month,Julian.day)


# ginko$fall<-ginko$Julian.day
ginko$key<-1



#Transform the data to a tsible so that functions can work
ginko_ts<-ginko|>as_tsibble(
  key=NULL,index=Year)


#complex transform to add 0s and seasonal components

# Define all months as ordered factor
all_months <- factor(month.name, levels = month.name, ordered = TRUE)

ginko_ts_zeroes<-ginko %>%
  as_tibble() %>%
  complete(
    Year = full_seq(Year, 1),
    month = all_months,
    fill = list(Julian.day = 0, key = 1) #add 0s to combinations that dont have values for julian day
  ) %>%
  mutate(
    date = yearmonth(paste(Year, month, "1")),  # Create a monthly date index
  ) %>%
  arrange(date) %>%
  as_tsibble(index = date, key = key)  # Use monthly index



#Basic Visualization
ginko_ts|>autoplot(Julian.day)

#Basic Visualization
ginko_ts_zeroes|>autoplot(Julian.day)

#Check autocorrelation

ginko_ts|>ACF(Julian.day, lag_max = 100)|>autoplot()

ginko_ts_zeroes|>ACF(Julian.day, lag_max = 100)|>autoplot()

#Interesting trend around 12 timesteps. Obviously because leaves fall either in october or november each year. Altough aparently they are falling later each year...

#Time series decomposition

ginko_ts|>model(
    STL(Julian.day ~ trend(window = 2), #Picked 2 based on ACF
        robust = TRUE)) |>
  components() |>
  autoplot()


ginko_ts_zeroes|>model(
  STL(Julian.day ~ trend(window = 12), #Picked 12 based on acf
      robust = TRUE)) |>
  components() |>
  autoplot()


#Forecasting
#I added seasonality as +season("year"), but it messed up predictions. So i went for trend as the only predictor


fit <- ginko_ts %>%
  model(
    arima = ARIMA(Julian.day ~  trend())
  )

report(fit)

fit %>% forecast(h = 2) %>%
  autoplot(ginko_ts)


fit <- ginko_ts_zeroes %>%
  model(
    arima = ARIMA(Julian.day ~ trend())
  )


report(fit)

fit %>% forecast(h = 12) %>%
  autoplot(ginko_ts_zeroes)
View(fit %>% forecast(h = 12))

