##Part 2
library(tidyverse)
library(fpp3)
library(seasonal)   

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")
head(retail_raw_long)

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)

has_gaps(retail_ts)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

## We want the sum as opposed to the average of each month because we want to see the total volume of gas sold in each quarter, not just the average monthly volume. Summing the monthly volumes gives us a more accurate representation of the overall gas sales for that quarter.
## Yes, the new series does appear to be seasonal because the volume of gas sold tends to fluctuate around a regular pattern.

retail_ts |> 
  autoplot(sales) + 
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

## The seasonal data shows clear annual fluctuations with a big spike in December and a big drop in what looks like February. The seasonally adjusted series does not have this pattern. 
## Yes, the two series do follow the same trend over time including during recessions.

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gasoline Sales",
    subtitle = "Monthly volume of gasoline sold in Canada",
    x = NULL, y = "Millions of litres"
  ) +
  theme_minimal()

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gasoline Sales",
    subtitle = "Monthly volume of gasoline sold in Canada",
    x = NULL, y = "Millions of litres"
  ) +
  geom_smooth() +
  theme_minimal()
  
##I added geom smooth to see an overall trend line that takes into account all the messy seasonality.

##Part 3

#Yes, at first the seasonal swings do grow with the level of the can_gas series. But then, in the 1990's and 2000's, as the level grows, the seasonal swings become more consistent and do not grow with the level of the series.
  
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

autoplot(can_gas, log_volume)

#The log graph shows a sharper relative increase in the volume of gas sold from 1960-1975 than 1970-2005. I would say that logging can_gas was a good move because the seasonal swings are now a more uniform size.

## 3.1c
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales (NSA)",
    subtitle = "Level",
    x = NULL, y = "Millions of dollars"
  ) +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail and Food Services Sales (NSA)",
    subtitle = "Log scale",
    x = NULL, y = "Log of millions of dollars"
  ) +
  theme_minimal()

##I think that there is a stronger case for logging US retail and food service sales since it removes the increasing seasonal swings that are present in the level graph. The log graph shows a more uniform seasonal pattern over time.

# 3.2a

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Canadian Gasoline Sales",
    subtitle = "Direct growth rate",
    x = NULL, y = "Percent"
  ) +
  theme_minimal()

can_gas <- can_gas |> 
  mutate( 
    growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Canadian Gasoline Sales",
    subtitle = "Absolute difference between direct and log growth rates",
    x = NULL, y = "Percent"
  ) +
  theme_minimal()

##Yes, I do think that the log difference of the growth rate is a good approximation for the direct growth rate. Only once are they more than 3% different, and most of the time, they're no more than 2% different. 

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

## The non-seasonally adjusted series has a lot of volatility in the growth rate, while the seasonally adjusted series is much more stable, which makes it significantly noisier. The seasonality around December and January causes large increases and decreases in the growth rate. The seasonally adjusted growth rate is much more stable.

## Part 4

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

## The seasonally adjusted data shows much more linear trends over time. Additionally, it is much easier to see the drop in sales during the covid shock.

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gasoline Sales",
    y = "Millions of litres"
  ) +
  theme_minimal()

##The seasonal pattern for canada gas sales tends to be higher in the winter months and lower in the summer months. This is likely due to the fact that people have to use gas to heat their homes in the cold canadian winter months. 

##4.2

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )


retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

##The seasonal swings slowly look like they are decreasing in magnitude as we get closer to present day. It is hard to tell exactly what month has the highest seasonal factor since the X-axis is in years. 

##4.3b

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

##The difference between my X-13 seasonal adjustment and the Census Bureau's is very small with the largest differences being slightly less than 2%. The biggest differences occur within the last 10 years or so.

##4.4

can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

##The estimated average monthly growth rate is 0.004 which is 0.4% growth per month. The month with the most significant seasonal effect is June at 4.77.

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

##The fitted values from the linear trend + seasonal dummies model fit the actual values better in the second half of the time series. The growth in the first half is much faster than the fitted model.

can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

##The residuals from the linear trend + seasonal dummies model are not white noise. Looking at the ACF, it is clear that there is high autocorrelation in the residuals. The Ljung-Box test also shows that the residuals are not white noise with a p-value of 0.0 and we reject the null. 

##Part 5

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

## The first evaluated forecast is for 1980 Jan and the last is for 2005 Feb. The model with the lower MAE and RMSE is the seasonal random walk model, with an MAE at 0.562 and RMSE at 0.702. The metrics do agree.

errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

## The DM stat is -31.278 and the p-value is 0.00000000000000022. Based on these results, we reject the null hypothesis that the two models have equal predictive accuracy. The seasonal random walk model is significantly better than the linear trend + seasonal dummies model.

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

##For the seasonal random walk model, its MAE is 0.562 and RMSE is 0.702. For the linear trend + seasonal dummies model, its MAE is 2.09 and its RMSE is 2.42. After switching to a rolling window, the linear trend + seasonal dummies model changed the most while the seasonal random walk model stayed the exact same. The TSLM model improved because it fits one straight trend through all the training data, and since the gas series’ growth isn’t constant, old data pulls that line away from recent behavior. Dropping old observations lets the trend fit the last 20 years.






