library(did)
library(DIDmultiplegt)


# set seed so everything is reproducible
set.seed(1814)

# generate dataset with 4 time periods
time.periods <- 4

# add dynamic effects
sp$te.e <- 1:time.periods

# generate data set with these parameters
# here, we dropped all units who are treated in time period 1 as they do not help us recover ATT(g,t)'s.
dta <- build_sim_dataset(sp)

# How many observations remained after dropping the ``always-treated'' units
nrow(dta)
#> [1] 15916
#This is what the data looks like
head(dta)
#> # A tibble: 6 × 7
#>       G      X    id cluster period     Y treat
#>   <dbl>  <dbl> <int>   <int>  <dbl> <dbl> <dbl>
#> 1     3 -0.876     1       5      1  5.56     1
#> 2     3 -0.876     1       5      2  4.35     1
#> 3     3 -0.876     1       5      3  7.13     1
#> 4     3 -0.876     1       5      4  6.24     1
#> 5     2 -0.874     2      36      1 -3.66     1
#> 6     2 -0.874     2      36      2 -1.27     1

