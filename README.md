Getting multiple EC50 values from csv files
================
2022-07-13

# These a SUPER EASY WAY TO get EC50 data just substitute you own data with csv files as appear in *folder* DATA2 with the same format as the TEMPLATE

## Examples used here are recent fungicide sensitivity results from multiple fungicides and for multiple fungi from Mexico about to get published in PEER REVIEW JOURNAL

## The main collaborator here is [Dr. Juan Manuel Tovar](https://ciad.edu.mx/dr-juan-manuel-tovar-pedraza/)

.

``` r
#Reading function
reading_data <- function(filename) {
  data <-
    read.csv(filename)
  # data <- subset(data, select = -X)
  data$repeats <- rep_len(1:3, length.out = nrow(data))
    data <- data %>%
    mutate(polar = replace(growth, growth == 0, 6)) %>%  #replacing 0 cm growth for the size of plug that is 0.6
    group_by(ID, experimental_replicate, concentration, growth, repeats) %>%
    rename(dose = concentration)
}

# Function to move away outliers  
get_range <- function(mynumber) {
  bb <- boxplot.stats(mynumber)
  cc <- bb$stats
  dd <- max(cc)
  ee <- min(cc)
  return(data.frame(upper = dd, lower = ee))
}

# Function to get EC50
getting_EC50 <- function(filename) {
  getting.EC50 <- EC_table(filename, form = response ~ dose)
  getting.EC50 <- getting.EC50 %>%
    rename(ID = sample) %>% # renaming
    rename(EC50 = Estimate.50) %>%
    mutate(ID = as.factor(ID))
}
  
# Function to filtering
filtering <- function(filename) {
  data <- filename  %>%
    group_by(ID, dose) %>%
    mutate(growth_range = list(get_range(growth))) %>%
    unnest(cols = c(growth_range)) %>%
    filter(growth <= upper & growth >= lower) %>%
    rename(response = growth) %>%
    ungroup() %>%
    select(c(ID, experimental_replicate, repeats, dose, response))
  return(data)
}

# Summary function
summary_EC50 <- function(filename) {
  data <- filename
  data <- data %>%
    summarise_at(vars(EC50), list(
      Min = min,
      Mean = mean,
      Max = max,
      Sd = sd
    ), na.rm = TRUE)
  }
```

``` r
##Check how CREATE OUTPUTS IN OTHER FOLDER OR MEANWHILE READ DATA@ COPY
files <- list.files("data2", pattern = ".csv", full.names = T)

data <- lapply(files, reading_data)
names(data) <- gsub(".csv", "", files)
list2env(data, globalenv())
```

    ## <environment: R_GlobalEnv>

``` r
data.2 <- lapply(data, filtering)
```

``` r
data.3 <- lapply(data.2, getting_EC50)
```

![](README_files/figure-gfm/EC50%20serial%20dilution-1.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-2.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-3.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-4.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-5.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-6.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-7.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-8.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-9.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-10.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-11.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-12.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-13.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-14.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-15.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-16.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-17.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-18.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-19.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-20.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-21.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-22.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-23.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-24.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-25.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-26.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-27.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-28.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-29.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-30.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-31.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-32.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-33.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-34.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-35.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-36.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-37.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-38.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-39.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-40.png)<!-- -->

    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite finite-difference value [3]
    ## Not evaluated: 7

![](README_files/figure-gfm/EC50%20serial%20dilution-41.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-42.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-43.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-44.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-45.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-46.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-47.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-48.png)<!-- -->

    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 2 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 3 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 4 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 12 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 13 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 41 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 42 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 43 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 46 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 47 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 48 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 50 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 60 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 62 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 64 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 73 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 78 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 79 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 80 
    ## Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
    ##   non-finite value supplied by optim
    ## Not evaluated: 82

![](README_files/figure-gfm/EC50%20serial%20dilution-49.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-50.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-51.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-52.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-53.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-54.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-55.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-56.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-57.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-58.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-59.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-60.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-61.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-62.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-63.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-64.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-65.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-66.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-67.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-68.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-69.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-70.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-71.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-72.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-73.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-74.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-75.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-76.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-77.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-78.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-79.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-80.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-81.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-82.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-83.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-84.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-85.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-86.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-87.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-88.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-89.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-90.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-91.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-92.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-93.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-94.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-95.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-96.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-97.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-98.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-99.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-100.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-101.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-102.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-103.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-104.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-105.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-106.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-107.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-108.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-109.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-110.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-111.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-112.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-113.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-114.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-115.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-116.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-117.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-118.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-119.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-120.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-121.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-122.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-123.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-124.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-125.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-126.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-127.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-128.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-129.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-130.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-131.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-132.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-133.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-134.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-135.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-136.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-137.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-138.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-139.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-140.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-141.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-142.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-143.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-144.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-145.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-146.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-147.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-148.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-149.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-150.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-151.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-152.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-153.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-154.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-155.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-156.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-157.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-158.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-159.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-160.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-161.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-162.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-163.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-164.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-165.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-166.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-167.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-168.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-169.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-170.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-171.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-172.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-173.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-174.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-175.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-176.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-177.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-178.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-179.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-180.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-181.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-182.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-183.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-184.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-185.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-186.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-187.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-188.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-189.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-190.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-191.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-192.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-193.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-194.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-195.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-196.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-197.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-198.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-199.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-200.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-201.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-202.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-203.png)<!-- -->![](README_files/figure-gfm/EC50%20serial%20dilution-204.png)<!-- -->

``` r
for (i in seq_along(data.3)) {
  filename = paste(names(data.3)[i], "OUTPUT.csv")
    write.csv(data.3[[i]], filename)
  }

data.4 <- lapply(data.3, summary_EC50)

summay_data_table <- data.4 %>%  bind_rows( .id = "NAME",  )
summay_data_table
```

    ## # A tibble: 8 × 5
    ##   NAME                                               Min    Mean     Max      Sd
    ##   <chr>                                            <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 data2/DATOS SENSIBILIDAD Lasiodiplodia spp_ c… 3.33e-1 4.14e+0 1.38e+1 4.81e+0
    ## 2 data2/DATOS SENSIBILIDAD Lasiodiplodia spp_bo… 4.49e+2 8.79e+2 1.23e+3 2.98e+2
    ## 3 data2/DATOS SENSIBILIDAD Lasiodiplodia spp_ex… 2.49e+1 8.49e+1 1.56e+2 4.20e+1
    ## 4 data2/DATOS SENSIBILIDAD Lasiodiplodia spp_py… 6.68e-1 1.38e+1 9.11e+1 3.14e+1
    ## 5 data2/DATOS SENSIBILIDAD Lasiodiplodia spp_py… 5.67e+1 1.59e+2 2.99e+2 8.25e+1
    ## 6 data2/DATOS SENSIBILIDAD Lasiodiplodia spp_th… 1.89e-3 5.82e-3 1.62e-2 4.84e-3
    ## 7 data2/pyraclostrobin_rhizoctonia               2.71e-2 4.12e+0 2.23e+1 5.98e+0
    ## 8 data2/thiophanate_rhizoctonia                  5.11e+0 1.66e+1 4.76e+1 1.06e+1

``` r
write.csv(summay_data_table, "summay_data_table.csv")
```

# Now see Data2 folder and all the files with summary results appear ending as “OUTPUT”
