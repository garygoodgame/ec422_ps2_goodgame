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
  geom_smooth()
  theme_minimal()
  
  ##I added geom smooth to see an overall trend line that takes into account all the messy seasonality.









