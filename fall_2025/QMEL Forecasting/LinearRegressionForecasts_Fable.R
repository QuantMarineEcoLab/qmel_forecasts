#Linear Regression Forecasts with Fable
# Code taken from the examples in the book Forecasting: Principles and Practice by Rob J Hyndman and George Athanasopoulos. 

#Code was written, modified and annotated by Miguel de Jesus Gomez Garcia.
#First updated on:
#Last Editted on:


#Packages

library(pacman)

pacman::p_load(patchwork, dplyr, tidyr, ggplot2,stringr,knitr,tidyquant,ggpubr, forcats, viridis, sf,AICcmodavg,nlme,fpp3,USgas)



## Souvenir Shop Example -----------------------------------------------------------------------

#This one follows the functions and instructions given in the book to analyze different ways to produce forecasts for a souvenir shop

#a) plots and patterns

souvenirs|>autoplot()
#Noticeable increase in sales during december each year. Small predictable peaks, probably holiday effect. Marked increase in maximum sales, almost exponential

#Strong autocorrelation with last month and at 12 month steps

souvenirs|>model(NAIVE(Sales)) |>
  gg_tsresiduals()
#Increasd uncertainity in recent years. residual autocorrelation at the 11 and 12th month step. 


#b) Why its necesary to log transform the data?

#Data shows non linear patterns. This pipelimne produces three models that will be used to forecast souvenir sales

fit_souvenirs <- souvenirs |>
  model(
    linear = TSLM(Sales ~ trend()),       #Simple linear model
    exponential = TSLM(log(Sales) ~ trend()),  #Log transform
    piecewise = TSLM(Sales ~ trend(knots = unique(year(souvenirs$Month)))) #Adding knots to the model, but NOT log transforming
  )

#This produces the forecast towards 24 timesteps
fc_trends <- fit_souvenirs |> forecast(h = 24) 

#And this plots it
souvenirs |>
  autoplot(Sales) +
  geom_line(data = fitted(fit_souvenirs),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "souvenirs",
       title = "Souvenir sales with naive models")


#d) Fit a regression model to the logarithms of these sales data with a linear trend, seasonal dummies and a “surfing festival” dummy variable. Festival occurs in march

#Here I am adding a separate "Month" variable and a dummy variable for March (month 3), when the summer festival takes place
souvenirs<-souvenirs|>mutate(
  Months=month(Month),
  Festival=ifelse(Months==3,1,0)
)


#Logarithmic model adding the new stuff
fit_souvenirs <- souvenirs |>
  model(
    linear = TSLM(log(Sales) ~ trend()+ season()+ Festival),
  )

report(fit_souvenirs)

#Create a future timeframe for forecasts
New_Sales<-souvenirs|>select(-Sales)|>
  mutate(
    Month=Month+83
  )


#Forecast projecting to our timeframe
fc_trends <- fit_souvenirs |> forecast(New_Sales)

#And plot
souvenirs |>
  autoplot(Sales) +
  geom_line(data = fitted(fit_souvenirs),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "souvenirs",
       title = "Projected souvenur sales")

#The same but trying different variations of covariates and modeling approaches.
fit_souvenir_all <- souvenirs |>
  model(
    linear = TSLM(Sales ~ trend()+ season()+ Festival),
    logarithmic = TSLM(log(Sales) ~ trend()+ season()+ Festival),
    root = TSLM(sqrt(Sales) ~ trend()+ season()+ Festival))


#Forecast
fc_trends <- fit_souvenir_all |> forecast(New_Sales)

#plot
souvenirs |>
  autoplot(Sales) +
  geom_line(data = fitted(fit_souvenir_all),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "souvenirs",
       title = "Boston marathon winning times")

#D) Plot the residuals

#Observed vs fitted values

augment(fit_souvenir_all) |>
  ggplot(aes(x = Sales, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in Electricity Demand"
  ) +
  geom_abline(intercept = 0, slope = 1)+
  facet_wrap(~.model)

#Logarithmic seems to be a better fit



#Observed vs fitted values Boxplot. Another way to visualize residual dispersal per model

augment(fit_souvenir_all) |>
  ggplot(aes(x = year(Month), y = Sales-.fitted)) +
  geom_boxplot(aes(group=month(Month))) +
  labs(
    y = "Residuals(Observed - fitted)",
    x = "Years",
    title = "Percent change in Electricity Demand"
  ) +
  facet_wrap(~.model)


#F) Report coefficient values

#Linear
report(fit_souvenir_all|>select(linear))

#Root
report(fit_souvenir_all|>select(linear))

#logarithmic
report(fit_souvenir_all|>select(linear))

#g Ljun-Box test. Simple forecast selection criterion. Smaler is better

fit_souvenir_all|> augment()|> features(.fitted,ljung_box)

##Souvenirs alternative ------------------------------------------------

#This is me mesing up and finding out. The plan was to rteplicate the exercices from the book in a more involved, less black-boxy way. I try to use base R functions for which documentation is easy to acces. 

#We start by adding the new covariates for month and festival
souvenirs_base<-souvenirs|>mutate(
  Months=as.factor(month(Month)),
  Festival=ifelse(Months==3,1,0)
)


#Souvenirs exponential model using base r and lm functions

souvenir_lm<-lm(data=souvenirs_base, log(Sales)~   #predicting sales as an exponential
                  Month   +      # Tend componen
                  Months  +      # Seasonal component factorized
                  Festival       # "Dummy" for summer festival 
)

summary(souvenir_lm)


#Same method for predicting, First create new data points to be predicted with each covariate value, then dataframes of error and plot

New_Sales_2 <- tibble(
  Month = yearmonth(seq(as.Date(last(souvenirs_base$Month)), #pick last entry of actual data
                        by = "1 month", #sequenced by 1 month
                        length.out = 84)) # Project 84 month in the future!!
) %>%
  mutate(
    Months = factor(month(Month)),     # Create factorial months 
    Festival = if_else(month(Month) == 3, 1, 0) # 1 only for March
  ) %>%
  as_tsibble(index = Month)


# Predict function. Uses model outputs to create predictions. Remember to back-transform log values!!
New_Sales_2$Sales <- predict(souvenir_lm, newdata = New_Sales_2)
New_Sales_2$Sales<- exp(New_Sales_2$Sales)  #back transform


print(New_Sales_2)

# 95% confidence interval. You can also use predict() to set custom confidence intervals. The option "interval" has several options. I am using "prediction" because it gives me a simmilar result to Fable. 
New_Sales_2_95 <- New_Sales_2
New_Sales_2_95_pred <- exp(predict(souvenir_lm, newdata = New_Sales_2_95, interval = "prediction", level = 0.95))
New_Sales_2_95 <- dplyr::bind_cols(New_Sales_2_95, New_Sales_2_95_pred)

# 80% confidence interval
New_Sales_2_80 <- New_Sales_2
New_Sales_2_80_pred <- exp(predict(souvenir_lm, newdata = New_Sales_2_80, interval = "prediction", level = 0.80))
New_Sales_2_80 <- dplyr::bind_cols(New_Sales_2_80, New_Sales_2_80_pred)

# Add Actual and fitted values to our dataframe for plotting
souvenirs_base$souvenir_fitted <- fitted(souvenir_lm)
souvenirs_base$souvenir_fitted <-exp(souvenirs_base$souvenir_fitted)


#Plot with ggplot
ggplot(souvenirs_base, aes(x = Month, y = Sales)) +
  geom_line(color = "black") +
  geom_line(aes(y = souvenir_fitted), color = "blue", linetype = "dashed") +
  # 95% CI ribbon
  geom_ribbon(data = New_Sales_2_95, aes(x = Month, ymin = lwr, ymax = upr), fill = "red", alpha = 0.2) +
  # 80% CI ribbon, on top
  geom_ribbon(data = New_Sales_2_80, aes(x = Month, ymin = lwr, ymax = upr), fill = "orange", alpha = 0.4) +
  # Forecast mean (red line)
  geom_line(data = New_Sales_2_95, aes(x = Month, y = fit), color = "red") +
  labs(
    title = "Sales in Australian goftshop",
    y = "Sales"
  )+ theme_minimal() 

#Compare with TSL

souvenirs |>
  autoplot(Sales) +
  geom_line(data = fitted(fit_souvenir_all|>select(logarithmic)),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends|>filter(.model=="logarithmic"), alpha = 0.5, level = 95) +
  labs(y = "Sales",
       title = "Sailz in uh-stray-lee-un gift shop") +theme_minimal() +
  theme(legend.position = "na")





# Example using real  data from a marine management publication. -------------------------------------------------------------

#This code produces figure 4 from White etal (2023). 10.32942/X2J60N


#Packages

library(pacman)

pacman::p_load(patchwork, dplyr, tidyr, ggplot2,stringr,knitr,tidyquant,ggpubr, forcats, viridis, sf,AICcmodavg,nlme)

#A theme function used in the paper

theme_cdf <- function(){
  theme_classic() + theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8),
                          axis.title=element_text(size=14)) 
}

#Import data
combined <- read.csv(file = 'Data/hermandad_vessel_id_large_vessels.csv',header=TRUE)


#This arranges the huge GFW dataset into somethin more manageable
combined_by_year <- combined %>%
  mutate(flag = fct_collapse(Flag, ECU='ECU', other_level = 'Foreign')) %>%
  mutate(Time.Range = str_sub(Time.Range,end=-4)) %>%
  mutate(Time.Range = as.Date(paste(Time.Range,'-1',sep=''))) %>%
  mutate(Gear.Type = fct_recode(Gear.Type, longlines = 'drifting_longlines', "purse seines" = 'tuna_purse_seines')) %>%
  group_by(Time.Range,Gear.Type) %>%
  summarize(total_fishing =sum(Apparent.Fishing.Hours)) %>% 
  ungroup() %>%
  complete(Time.Range, Gear.Type, fill=list(total_fishing=0)) %>%
  filter(Gear.Type != 'fishing')


#Data Exploration
ggplot(data=combined_by_year,aes(x=Time.Range, y=total_fishing))+
  geom_line()+
  facet_wrap(~Gear.Type, scales = 'free_y',ncol=1) + theme_cdf() 



# Extract drifting longlines dataframe
df <- combined_by_year %>% filter(Gear.Type=='longlines')


#Aaand format it
df$month <- as.numeric(format(as.Date(df$Time.Range), '%m'))
df$year <- as.numeric(format(as.Date(df$Time.Range), '%Y'))


#this adds cyclic transformations for month. Akin to a furier series or the "seasonal" function in Fable/TLSM
df$sin <- sin(2*pi*df$month/12)
df$cos <- cos(2*pi*df$month/12)

#We want to use data from 2019
df <- filter(df, year>2019)

##and add a dummy variable for the closing of fisheries
df$pre_closure <- 0
df$pre_closure[df$Time.Range<as.Date('2022-02-01')] <- 1
df$post_closure<- 0
df$post_closure[df$Time.Range>as.Date('2022-02-01')] <- 1

#Ok, not lets build the model!! 
model.a = glm(total_fishing ~  sin + cos + year, data = df %>% filter(year<2022) %>% mutate(total_fishing=round(total_fishing)),family = 'poisson')


#And add predictions to the dataframe for plotting
df<-df %>% mutate(
  model.a.predictions = predictSE.gls(model.a, df, se.fit=T)$fit,
  model.a.se = predictSE.gls(model.a, df, se.fit=T)$se
)


#Lets repeat the process for Purse seines
df2 <- combined_by_year %>% filter(Gear.Type=='purse seines')

df2$month <- as.numeric(format(as.Date(df2$Time.Range), '%m'))
df2$year <- as.numeric(format(as.Date(df2$Time.Range), '%Y'))
df2$sin <- sin(2*pi*df2$month/12)
df2$cos <- cos(2*pi*df2$month/12)
df2 <- filter(df2, year>2019)

df2$pre_closure <- 0
df2$pre_closure[df2$Time.Range<as.Date('2022-02-01')] <- 1
df2$post_closure<- 0
df2$post_closure[df2$Time.Range>as.Date('2022-02-01')] <- 1


model.a = glm(total_fishing ~  sin + cos + year, data = df2 %>% filter(year<2022) %>% mutate(total_fishing=round(total_fishing)),family = 'poisson')


df2<-df2 %>% mutate(
  model.a.predictions = predictSE.gls(model.a, df2, se.fit=T)$fit,
  model.a.se = predictSE.gls(model.a, df2, se.fit=T)$se
)


#This combines both dataframes so we can use facettes in ggplot
df_combined <- rbind(df,df2)

#And this is the plot that they used for the preprint

ggplot(data=df_combined,aes(x=Time.Range, y=total_fishing)) + geom_line() + geom_ribbon(aes(ymin = exp(model.a.predictions - 0.99*exp(model.a.se)), ymax = exp(model.a.predictions + 0.99*exp(model.a.se))), fill = "lightgrey",alpha=0.5)+
  geom_line(aes(Time.Range,exp(model.a.predictions)),color="grey",lty=2) + ylab('Total fishing hours') +xlab('Time')+ ggtitle( 'Hermandad Marine Reserve') +
  scale_color_viridis(discrete = T,begin = 0.2,end=0.8)+ theme_cdf() + geom_vline(xintercept=as.Date('2022-01-15'), linetype=2) +facet_wrap(~Gear.Type, scales = 'free_y',ncol=1) + theme_cdf() 




##Example using Fable functions -------------------------------------------------------------------


#First, we need to transform the data to a tsible so that functions can work
ts_combined<-df_combined|>as_tsibble(
  key = Gear.Type,
  index = Time.Range
)

#We can visualize
ts_combined|>autoplot(total_fishing)

#Divide the data in pre and post closing of the fisheries
ts_Pre<-ts_combined|>filter(year< 2022)

ts_Post<-ts_combined|>filter(year>= 2022)|>
  select(-total_fishing)


#And build a simple model

fit_combined <-ts_Pre |>
  model(
    poisson = TSLM(log(total_fishing+1) ~ season() ) #log transformed, incorporating seasonality, which in theory should produce simmilar results... I am adding +1 to fishing as a quick way to deal with 0s for logarithmic transformations
  )

#Check model outputs
fit_combined|>filter(Gear.Type =="longlines")|>report()
fit_combined|>filter(Gear.Type =="purse seines")|>report()


#And forecast
combined_trends <- fit_combined |> forecast(fit_combined, new_data = ts_Post)


#Plot
ts_combined |>
  autoplot(total_fishing) +
  geom_line(data = fitted(fit_combined),
            aes(y = .fitted, colour = .model),lty=2) +
  autolayer(combined_trends, alpha = 0.5, level = 95,lty=2) +
  labs(y = "Fishing",
       title = "Predicted fishing")+ geom_vline(xintercept=as.Date('2022-01-15'), linetype=2)+
  facet_wrap(~Gear.Type,ncol=1, scales = 'free_y')

#Its not thaaat far from the preprint plot, but my theory is that differences were caused by how Fable handles seasonal/cyclic terms, and how the authors incorporated in the model as sin+cos terms. hard ti tell with the blackbox approach of Fable.


#So I built a model using an explicit sin+cos predictor for season

ts_Pre <- ts_Pre %>%
  mutate(
    month = month(Time.Range),
    sin = sin(2*pi*month/12),
    cos = cos(2*pi*month/12),
    year = year(Time.Range)
  )

#Modeling using exponential and our fourier terms
fit_combined <- ts_Pre |>
  model(
    poisson = TSLM(log(total_fishing + 1) ~ sin + cos + trend())
  )

fit_combined|>filter(Gear.Type =="longlines")|>report()
fit_combined|>filter(Gear.Type =="purse seines")|>report()


#Create prediction/forecasting dataframe
ts_Post <- ts_Post %>%
  mutate(
    month = month(Time.Range),
    sin = sin(2*pi*month/12),
    cos = cos(2*pi*month/12),
    year = year(Time.Range)
  )


#Forecast
combined_trends <- fit_combined |>
  forecast(new_data = ts_Post)

#Plot
ts_combined |>
  autoplot(total_fishing) +
  geom_line(data = fitted(fit_combined),
            aes(y = .fitted, colour = .model),lty=2) +
  autolayer(combined_trends, alpha = 0.5, level = 95,lty=2) +
  labs(y = "Fishing",
       title = "Predicted fishing")+ geom_vline(xintercept=as.Date('2022-01-15'), linetype=2)+
  facet_wrap(~Gear.Type,ncol=1, scales = 'free_y')




