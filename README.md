# typical.sleep
An R package to compute the typical sleep period from Fitbit sleep segment data. 
Can be used in the All of Us Researcher's Workbench or from a direct export from Fitbit.

# Installation
```r
devtools::install_github("annisjs/typical.sleep")
```

# Usage
```r
library(typical.sleep)

# Assuming dat is a data.table that shares the same schema as the sleep_level table in AoU.
dat <- as_sleep_logs(dat)
sleep_metrics <- get_sleep_metrics(dat)
dat <- as_typical_sleep(dat)
tsp_metrics <- get_sleep_metrics(dat)
```

The package can also handle direct exports from Fitbit. 
[How do I export my Fitbit data?](https://support.google.com/fitbit/answer/14236615?hl=en#zippy=%2Chow-do-i-export-my-fitbit-data).
```r
example_file <- example_dataset()
json_test_data <- parse_fitbit_json(example_file)
dat <- as_sleep_logs(json_test_data)
dat <- as_typical_sleep(dat)
```
