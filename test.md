Untitled
================

``` r
library(readxl)

attributes <- read_excel("oriole_specimen_info.xlsx")
table(attributes$Code)
```

    ## 
    ## Baltimore Oriole Bullock's Oriole 
    ##               39               80
