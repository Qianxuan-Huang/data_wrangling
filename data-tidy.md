data tidy
================

## load in the PULSE data in sas format

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") |>
  janitor::clean_names()
```

## make the wide table a long table

Put column name “bdi_score_bl:bdi_score_12m” into “visit”, value in
these colum into “bdi”. And delete all “bdi_score\_” in every “visit”
value.

``` r
pulse_data_tidy = pulse_data %>%
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score_",
    values_to = "bdi"
    )

pulse_data_tidy
```

    ## # A tibble: 4,348 × 5
    ##       id   age sex   visit   bdi
    ##    <dbl> <dbl> <chr> <chr> <dbl>
    ##  1 10003  48.0 male  bl        7
    ##  2 10003  48.0 male  01m       1
    ##  3 10003  48.0 male  06m       2
    ##  4 10003  48.0 male  12m       0
    ##  5 10015  72.5 male  bl        6
    ##  6 10015  72.5 male  01m      NA
    ##  7 10015  72.5 male  06m      NA
    ##  8 10015  72.5 male  12m      NA
    ##  9 10022  58.5 male  bl       14
    ## 10 10022  58.5 male  01m       3
    ## # ℹ 4,338 more rows

Combine previous steps together, and add mutate:

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") |>
  janitor::clean_names() %>%
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score_",
    values_to = "bdi"
    ) %>%
  relocate(id, visit) %>%
  mutate(visit = recode(visit, "bl" = "00m"))
```

## pivot_wider

make up new dataset “analysis_result”

pivot_wider is the inverse of pivot_longer

``` r
analysis_result = 
  tibble(
    group = c("treatment", "treatment", "placebo", "placebo"),
    time = c("pre", "post", "pre", "post"),
    mean = c(4, 8, 3.5, 4)
  )

analysis_result %>% 
  pivot_wider(
    names_from = "time", 
  values_from = "mean"
  )
```

    ## # A tibble: 2 × 3
    ##   group       pre  post
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment   4       8
    ## 2 placebo     3.5     4

## Binding rows

using LotR data. 1. import each table

``` r
fellowship_ring = 
  readxl::read_excel("./data/LotR_words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = 
  readxl::read_excel("./data/LotR_words.xlsx", range = "F3:H6") %>%
  mutate(movie = "fellowship_ring")

return_king = 
  readxl::read_excel("./data/LotR_words.xlsx", range = "J3:L6") %>%
  mutate(movie = "fellowship_ring")
```

2.  bind 3 tables together

``` r
lotr_tidy = 
  bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "gender", 
    values_to = "words") |>
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything()) 
```

## Joining datasets

import FAS datasets separate(group, into = c(“dose”, “day_of_tx”), sep =
3) means in group colum, first 3 character go to “dose”, other go to
“day_of_tx”.

``` r
pup_df = 
  read_csv("./data/FAS_pups.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  mutate(sex = recode(sex, "1" = "male", "2" = "female")) 
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litter_df = 
  read_csv("./data/FAS_litters.csv", na = c("NA", ".", "")) |>
  janitor::clean_names() |>
  relocate(litter_number) |>
  separate(group, into = c("dose", "day_of_tx"), sep = 3) 
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

then join them

``` r
fas_df = 
  left_join(pup_df, litter_df, by = "litter_number") %>%
  arrange(litter_number) %>%
  relocate(litter_number, dose, day_of_tx)
```
