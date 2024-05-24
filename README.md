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
```

Data is assumed to be in the format provided by the sleep_levels table in the All of Us Researcher's Workbench.
The table has the following columns: ```person_id```, ```sleep_date```, ```start_datetime```, ```is_main_sleep```, ```level```, ```duration```.
Below we compute some sleep metrics when ```is_main_sleep``` is ```TRUE```.
Tutorials on how to work with AoU sleep data can be found here: [How to Work With Wearable Device Data (v7)](https://workbench.researchallofus.org/workspaces/aou-rw-f7c56f30/howtoworkwithwearabledevicedatav7/analysis).
```r
# Assuming dat is a data.table that shares the same schema as the sleep_level table in AoU.
sleep_metrics <- compute_sleep_metrics(dat[is_main_sleep == TRUE],"sleep_date")
```
The ```typical.sleep``` function returns the original dataset plus 2 additional columns: ```typical_sleep_date``` and ```is_typical_sleep```.
The ```typical_sleep_date``` is the recomputed date of sleep via the typical sleep algorithm. ```is_typical_sleep``` is ```TRUE``` when the
sleep segment is part of the typical sleep period and ```FALSE``` otherwise. 

To run the typical sleep algorithm and compute some 
sleep metrics is easy:
```r
dat <- typical.sleep(dat)
tsp_metrics <- compute_sleep_metrics(dat[is_typical_sleep == TRUE],"typical_sleep_date")
```

The package can also handle direct exports from Fitbit. 
[How do I export my Fitbit data?](https://support.google.com/fitbit/answer/14236615?hl=en#zippy=%2Chow-do-i-export-my-fitbit-data).
```r
json_test_data <- parse_fitbit_json("my-sleep-data.json")
dat <- typical.sleep(json_test_data)
```
# The typical sleep algorithm
Finds the typical sleep period in three steps: 
  1. Finds relevant sleep logs are those that fall within +/-8 hours of the midsleep point. 
  2. Determines the median bedtime and waketime from the relevant logs.
  3. Labels each sleep log as either part of the typical sleep period or not by filtering out those
    that are outside of the bedtime/waketime interval.