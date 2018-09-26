data\_manupilation
================
rj2543
September 19, 2018

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts --------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
options(tibble.print_min = 3)

litters_data = read_csv("./data/FAS_litters.csv",
  col_types = "ccddiiii")
litters_data = janitor::clean_names(litters_data)

pups_data = read_csv("./data/FAS_pups.csv",
  col_types = "ciiiii")
pups_data = janitor::clean_names(pups_data)
```

``` r
select(litters_data, group, litter_number, gd0_weight, pups_born_alive)
```

    ## # A tibble: 49 x 4
    ##   group litter_number gd0_weight pups_born_alive
    ##   <chr> <chr>              <dbl>           <int>
    ## 1 Con7  #85                 19.7               3
    ## 2 Con7  #1/2/95/2           27                 8
    ## 3 Con7  #5/5/3/83/3-3       26                 6
    ## # ... with 46 more rows

``` r
select(litters_data, group:gd_of_birth)
```

    ## # A tibble: 49 x 5
    ##   group litter_number gd0_weight gd18_weight gd_of_birth
    ##   <chr> <chr>              <dbl>       <dbl>       <int>
    ## 1 Con7  #85                 19.7        34.7          20
    ## 2 Con7  #1/2/95/2           27          42            19
    ## 3 Con7  #5/5/3/83/3-3       26          41.4          19
    ## # ... with 46 more rows

``` r
select(litters_data, -pups_survive)
```

    ## # A tibble: 49 x 7
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 Con7  #85                 19.7        34.7          20               3
    ## 2 Con7  #1/2/95/2           27          42            19               8
    ## 3 Con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 46 more rows, and 1 more variable: pups_dead_birth <int>

``` r
select(litters_data, GROUP = group, LiTtEr_NuMbEr = litter_number)
```

    ## # A tibble: 49 x 2
    ##   GROUP LiTtEr_NuMbEr
    ##   <chr> <chr>        
    ## 1 Con7  #85          
    ## 2 Con7  #1/2/95/2    
    ## 3 Con7  #5/5/3/83/3-3
    ## # ... with 46 more rows

``` r
rename(litters_data, GROUP = group, LiTtEr_NuMbEr = litter_number)
```

    ## # A tibble: 49 x 8
    ##   GROUP LiTtEr_NuMbEr gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 Con7  #85                 19.7        34.7          20               3
    ## 2 Con7  #1/2/95/2           27          42            19               8
    ## 3 Con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>

``` r
select(litters_data, starts_with("gd"))
```

    ## # A tibble: 49 x 3
    ##   gd0_weight gd18_weight gd_of_birth
    ##        <dbl>       <dbl>       <int>
    ## 1       19.7        34.7          20
    ## 2       27          42            19
    ## 3       26          41.4          19
    ## # ... with 46 more rows

``` r
select(litters_data, litter_number, everything())
```

    ## # A tibble: 49 x 8
    ##   litter_number group gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr>         <chr>      <dbl>       <dbl>       <int>           <int>
    ## 1 #85           Con7        19.7        34.7          20               3
    ## 2 #1/2/95/2     Con7        27          42            19               8
    ## 3 #5/5/3/83/3-3 Con7        26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>

``` r
select(pups_data, litter_number, sex, pd_ears)
```

    ## # A tibble: 313 x 3
    ##   litter_number   sex pd_ears
    ##   <chr>         <int>   <int>
    ## 1 #85               1       4
    ## 2 #85               1       4
    ## 3 #1/2/95/2         1       5
    ## # ... with 310 more rows

``` r
filter(litters_data, gd0_weight < 25)
```

    ## # A tibble: 20 x 8
    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ##  1 Con7  #85                 19.7        34.7          20               3
    ##  2 Mod7  #59                 17          33.4          19               8
    ##  3 Mod7  #103                21.4        42.1          19               9
    ##  4 Mod7  #4/2/95/2           23.5        NA            19               9
    ##  5 Mod7  #5/3/83/5-2         22.6        37            19               5
    ##  6 Mod7  #106                21.7        37.8          20               5
    ##  7 Mod7  #94/2               24.4        42.9          19               7
    ##  8 Mod7  #62                 19.5        35.9          19               7
    ##  9 Low7  #84/2               24.3        40.8          20               8
    ## 10 Low7  #107                22.6        42.4          20               9
    ## 11 Low7  #85/2               22.2        38.5          20               8
    ## 12 Low7  #98                 23.8        43.8          20               9
    ## 13 Low7  #102                22.6        43.3          20              11
    ## 14 Low7  #101                23.8        42.7          20               9
    ## 15 Low7  #112                23.9        40.5          19               6
    ## 16 Mod8  #97                 24.5        42.8          20               8
    ## 17 Low8  #53                 21.8        37.2          20               8
    ## 18 Low8  #100                20          39.2          20               8
    ## 19 Low8  #4/84               21.8        35.2          20               4
    ## 20 Low8  #99                 23.5        39            20               6
    ## # ... with 2 more variables: pups_dead_birth <int>, pups_survive <int>

``` r
filter(litters_data, pups_born_alive == 0)
```

    ## # A tibble: 0 x 8
    ## # ... with 8 variables: group <chr>, litter_number <chr>,
    ## #   gd0_weight <dbl>, gd18_weight <dbl>, gd_of_birth <int>,
    ## #   pups_born_alive <int>, pups_dead_birth <int>, pups_survive <int>

``` r
filter(litters_data, !is.na(gd0_weight))
```

    ## # A tibble: 34 x 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 Con7  #85                 19.7        34.7          20               3
    ## 2 Con7  #1/2/95/2           27          42            19               8
    ## 3 Con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 31 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>

``` r
filter(litters_data, group == "Low8" | group == "Low7")
```

    ## # A tibble: 15 x 8
    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ##  1 Low7  #84/2               24.3        40.8          20               8
    ##  2 Low7  #107                22.6        42.4          20               9
    ##  3 Low7  #85/2               22.2        38.5          20               8
    ##  4 Low7  #98                 23.8        43.8          20               9
    ##  5 Low7  #102                22.6        43.3          20              11
    ##  6 Low7  #101                23.8        42.7          20               9
    ##  7 Low7  #111                25.5        44.6          20               3
    ##  8 Low7  #112                23.9        40.5          19               6
    ##  9 Low8  #53                 21.8        37.2          20               8
    ## 10 Low8  #79                 25.4        43.8          19               8
    ## 11 Low8  #100                20          39.2          20               8
    ## 12 Low8  #4/84               21.8        35.2          20               4
    ## 13 Low8  #108                25.6        47.5          20               8
    ## 14 Low8  #99                 23.5        39            20               6
    ## 15 Low8  #110                25.5        42.7          20               7
    ## # ... with 2 more variables: pups_dead_birth <int>, pups_survive <int>

``` r
filter(litters_data, group %in% c("Low8", "Low7"))
```

    ## # A tibble: 15 x 8
    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ##  1 Low7  #84/2               24.3        40.8          20               8
    ##  2 Low7  #107                22.6        42.4          20               9
    ##  3 Low7  #85/2               22.2        38.5          20               8
    ##  4 Low7  #98                 23.8        43.8          20               9
    ##  5 Low7  #102                22.6        43.3          20              11
    ##  6 Low7  #101                23.8        42.7          20               9
    ##  7 Low7  #111                25.5        44.6          20               3
    ##  8 Low7  #112                23.9        40.5          19               6
    ##  9 Low8  #53                 21.8        37.2          20               8
    ## 10 Low8  #79                 25.4        43.8          19               8
    ## 11 Low8  #100                20          39.2          20               8
    ## 12 Low8  #4/84               21.8        35.2          20               4
    ## 13 Low8  #108                25.6        47.5          20               8
    ## 14 Low8  #99                 23.5        39            20               6
    ## 15 Low8  #110                25.5        42.7          20               7
    ## # ... with 2 more variables: pups_dead_birth <int>, pups_survive <int>

``` r
mutate(litters_data,
  wt_gain = gd18_weight - gd0_weight,
  group = tolower(group)
)
```

    ## # A tibble: 49 x 9
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 con7  #85                 19.7        34.7          20               3
    ## 2 con7  #1/2/95/2           27          42            19               8
    ## 3 con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 46 more rows, and 3 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>, wt_gain <dbl>

``` r
mutate(litters_data,
  wt_gain = gd18_weight - gd0_weight,
  wt_gain_squared = wt_gain^2
)
```

    ## # A tibble: 49 x 10
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 Con7  #85                 19.7        34.7          20               3
    ## 2 Con7  #1/2/95/2           27          42            19               8
    ## 3 Con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 46 more rows, and 4 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>, wt_gain <dbl>, wt_gain_squared <dbl>

``` r
arrange(litters_data, gd0_weight)
```

    ## # A tibble: 49 x 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 Mod7  #59                 17          33.4          19               8
    ## 2 Mod7  #62                 19.5        35.9          19               7
    ## 3 Con7  #85                 19.7        34.7          20               3
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>

``` r
arrange(litters_data, desc(pups_born_alive), gd0_weight)
```

    ## # A tibble: 49 x 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 Low7  #102                22.6        43.3          20              11
    ## 2 Mod8  #5/93               NA          41.1          20              11
    ## 3 Mod7  #103                21.4        42.1          19               9
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>

``` r
litters_data_raw = read_csv("./data/FAS_litters.csv",
  col_types = "ccddiiii")
litters_data_clean_names = janitor::clean_names(litters_data_raw)
litters_data_selected_cols = select(litters_data_clean_names, -pups_survive)
litters_data_with_vars = mutate(litters_data_selected_cols, 
  wt_gain = gd18_weight - gd0_weight,
  group = tolower(group))
litters_data_with_vars
```

    ## # A tibble: 49 x 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 con7  #85                 19.7        34.7          20               3
    ## 2 con7  #1/2/95/2           27          42            19               8
    ## 3 con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   wt_gain <dbl>

``` r
litters_data_clean = 
  mutate(
    select(
      janitor::clean_names(
        read_csv("./data/FAS_litters.csv", col_types = "ccddiiii")
        ), 
    -pups_survive
    ),
  wt_gain = gd18_weight - gd0_weight,
  group = tolower(group)
  )
litters_data_clean
```

    ## # A tibble: 49 x 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 con7  #85                 19.7        34.7          20               3
    ## 2 con7  #1/2/95/2           27          42            19               8
    ## 3 con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   wt_gain <dbl>

``` r
litters_data = 
  read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>%
  janitor::clean_names() %>%
  select(-pups_survive) %>%
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    group = tolower(group)
  )
litters_data
```

    ## # A tibble: 49 x 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 con7  #85                 19.7        34.7          20               3
    ## 2 con7  #1/2/95/2           27          42            19               8
    ## 3 con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   wt_gain <dbl>

``` r
read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>%
  janitor::clean_names() %>%
  select(-pups_survive) %>%
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    group = tolower(group)
  ) %>%
  filter(!is.na(gd0_weight)) %>%
  lm(gd18_weight ~ gd0_weight, data = .)
```

    ## 
    ## Call:
    ## lm(formula = gd18_weight ~ gd0_weight, data = .)
    ## 
    ## Coefficients:
    ## (Intercept)   gd0_weight  
    ##      15.342        1.084

``` r
pulse_data = haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names()
pulse_data
```

    ## # A tibble: 1,087 x 7
    ##      id   age sex   bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##   <dbl> <dbl> <chr>        <dbl>         <dbl>         <dbl>         <dbl>
    ## 1 10003  48.0 male             7             1             2             0
    ## 2 10015  72.5 male             6            NA            NA            NA
    ## 3 10022  58.5 male            14             3             8            NA
    ## # ... with 1,084 more rows

``` r
pulse_tidy_data = gather(pulse_data, key = visit, value = bdi, bdi_score_bl:bdi_score_12m)
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

``` r
pulse_tidy_data
```

    ## # A tibble: 4,348 x 5
    ##      id   age sex   visit          bdi
    ##   <dbl> <dbl> <chr> <chr>        <dbl>
    ## 1 10003  48.0 male  bdi_score_bl     7
    ## 2 10015  72.5 male  bdi_score_bl     6
    ## 3 10022  58.5 male  bdi_score_bl    14
    ## # ... with 4,345 more rows

``` r
separate(pulse_tidy_data, visit, into = c("remove_1", "remove_2", "visit"), sep = "_") %>%
  select(-remove_1, -remove_2) %>%
  mutate(visit = replace(visit, visit == "bl", "00m")) 
```

    ## # A tibble: 4,348 x 5
    ##      id   age sex   visit   bdi
    ##   <dbl> <dbl> <chr> <chr> <dbl>
    ## 1 10003  48.0 male  00m       7
    ## 2 10015  72.5 male  00m       6
    ## 3 10022  58.5 male  00m      14
    ## # ... with 4,345 more rows

``` r
pulse_data = haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names() %>%
  gather(key = visit, value = bdi, bdi_score_bl:bdi_score_12m) %>%
  separate(visit, into = c("remove_1", "remove_2", "visit"), sep = "_") %>%
  select(id, visit, everything(), -starts_with("remove")) %>%
  mutate(visit = replace(visit, visit == "bl", "00m"),
         visit = factor(visit, levels = str_c(c("00", "01", "06", "12"), "m"))) %>%
  arrange(id, visit)
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

``` r
print(pulse_data, n = 12)
```

    ## # A tibble: 4,348 x 5
    ##       id visit   age sex     bdi
    ##    <dbl> <fct> <dbl> <chr> <dbl>
    ##  1 10003 00m    48.0 male      7
    ##  2 10003 01m    48.0 male      1
    ##  3 10003 06m    48.0 male      2
    ##  4 10003 12m    48.0 male      0
    ##  5 10015 00m    72.5 male      6
    ##  6 10015 01m    72.5 male     NA
    ##  7 10015 06m    72.5 male     NA
    ##  8 10015 12m    72.5 male     NA
    ##  9 10022 00m    58.5 male     14
    ## 10 10022 01m    58.5 male      3
    ## 11 10022 06m    58.5 male      8
    ## 12 10022 12m    58.5 male     NA
    ## # ... with 4,336 more rows

``` r
read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>% 
  janitor::clean_names() %>%
separate(group, into = c("dose", "day"), 3) %>%
mutate(dose = tolower(dose),
         wt_gain = gd18_weight - gd0_weight) %>%
  arrange(litter_number)
```

    ## # A tibble: 49 x 10
    ##   dose  day   litter_number gd0_weight gd18_weight gd_of_birth
    ##   <chr> <chr> <chr>              <dbl>       <dbl>       <int>
    ## 1 con   7     #1/2/95/2             27          42          19
    ## 2 con   7     #1/5/3/83/3-~         NA          NA          20
    ## 3 con   8     #1/6/2/2/95-2         NA          NA          20
    ## # ... with 46 more rows, and 4 more variables: pups_born_alive <int>,
    ## #   pups_dead_birth <int>, pups_survive <int>, wt_gain <dbl>

``` r
litters_data
```

    ## # A tibble: 49 x 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 con7  #85                 19.7        34.7          20               3
    ## 2 con7  #1/2/95/2           27          42            19               8
    ## 3 con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   wt_gain <dbl>

``` r
litters_data %>% 
  select(litter_number, ends_with("weight")) %>% 
  gather(key = gd, value = weight, gd0_weight:gd18_weight) %>% 
  mutate(gd = recode(gd, "gd0_weight" = 0, "gd18_weight" = 18))
```

    ## # A tibble: 98 x 3
    ##   litter_number    gd weight
    ##   <chr>         <dbl>  <dbl>
    ## 1 #85               0   19.7
    ## 2 #1/2/95/2         0   27  
    ## 3 #5/5/3/83/3-3     0   26  
    ## # ... with 95 more rows

``` r
analysis_result = tibble(
  group = c("treatment", "treatment", "placebo", "placebo"),
  time = c("pre", "post", "pre", "post"),
  mean = c(4, 8, 3.5, 4)
)
analysis_result
```

    ## # A tibble: 4 x 3
    ##   group     time   mean
    ##   <chr>     <chr> <dbl>
    ## 1 treatment pre     4  
    ## 2 treatment post    8  
    ## 3 placebo   pre     3.5
    ## 4 placebo   post    4

``` r
spread(analysis_result, key = time, value = mean) %>%
knitr::kable()
```

| group     |  post|  pre|
|:----------|-----:|----:|
| placebo   |     4|  3.5|
| treatment |     8|  4.0|

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")
```

``` r
lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) %>%
  janitor::clean_names() %>%
  gather(key = sex, value = words, female:male) %>%
  mutate(race = tolower(race)) %>% 
  select(movie, everything()) 

lotr_tidy
```

    ## # A tibble: 18 x 4
    ##    movie           race   sex    words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring hobbit female    14
    ##  3 fellowship_ring man    female     0
    ##  4 two_towers      elf    female   331
    ##  5 two_towers      hobbit female     0
    ##  6 two_towers      man    female   401
    ##  7 return_king     elf    female   183
    ##  8 return_king     hobbit female     2
    ##  9 return_king     man    female   268
    ## 10 fellowship_ring elf    male     971
    ## 11 fellowship_ring hobbit male    3644
    ## 12 fellowship_ring man    male    1995
    ## 13 two_towers      elf    male     513
    ## 14 two_towers      hobbit male    2463
    ## 15 two_towers      man    male    3589
    ## 16 return_king     elf    male     510
    ## 17 return_king     hobbit male    2673
    ## 18 return_king     man    male    2459

``` r
pup_data = read_csv("./data/FAS_pups.csv", col_types = "ciiiii") %>%
  janitor::clean_names() %>%
  mutate(sex = recode(sex, `1` = "male", `2` = "female")) 

litter_data = read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>%
  janitor::clean_names() %>%
  select(-pups_survive) %>%
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    group = tolower(group))

FAS_data = left_join(pup_data, litter_data, by = "litter_number")
FAS_data
```

    ## # A tibble: 313 x 13
    ##   litter_number sex   pd_ears pd_eyes pd_pivot pd_walk group gd0_weight
    ##   <chr>         <chr>   <int>   <int>    <int>   <int> <chr>      <dbl>
    ## 1 #85           male        4      13        7      11 con7        19.7
    ## 2 #85           male        4      13        7      12 con7        19.7
    ## 3 #1/2/95/2     male        5      13        7       9 con7        27  
    ## # ... with 310 more rows, and 5 more variables: gd18_weight <dbl>,
    ## #   gd_of_birth <int>, pups_born_alive <int>, pups_dead_birth <int>,
    ## #   wt_gain <dbl>
