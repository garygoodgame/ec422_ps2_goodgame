##Part 2
library(tidyverse)

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























