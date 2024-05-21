# typical.sleep
An R package to compute the typical sleep period from Fitbit sleep segment data. 
Can be used in the All of Us Researcher's Workbench or from a direct export from Fitbit.

# Installation
```r
devtools::install_github("annisjs/typical.sleep",updrade=F)
```

# Usage
```r
library(typical.sleep)
```

Data is assumed to be in the format provided by the sleep_levels table in the All of Us Researcher's Workbench.
The table has the following columns: person_id, sleep_date, start_datetime, is_main_sleep, level, duration.
Below we compute some sleep metrics when is_main_sleep is TRUE.
```r
# Read in some sleep data.
dat <- data.table::fread("all_of_us_sleep_levels_data.csv")
sleep_metrics <- compute_sleep_metrics(dat[is_main_sleep == TRUE])
```
The typical.sleep function returns the original dataset plus 2 additional columns: typical_sleep_date and is_typical_sleep.
The typical_sleep_date is the recomputed date of sleep via the typical sleep algorithm. is_typical_sleep is TRUE when the
sleep segment is part of the typical sleep period and FALSE otherwise. 

To run the typical sleep algorithm and compute some 
sleep metrics is easy:
```r
dat <- typical.sleep(dat)
tsp_metrics <- compute_sleep_metrics(dat[is_typical_sleep == TRUE])
```

The package can also handle direct exports from Fitbit: [How do I export my Fitbit data?](https://support.google.com/fitbit/answer/14236615?hl=en#zippy=%2Chow-do-i-export-my-fitbit-data).
```r
json_test_data <- parse_fitbit_json("sleep-2023-03-14.json")
dat <- typical.sleep(json_test_data)
```
