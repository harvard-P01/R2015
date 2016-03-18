## ---- eval = FALSE-------------------------------------------------------
## d <- data.table::fread("~/Downloads/allyears.csv")

## ----eval=FALSE----------------------------------------------------------
## library("ff")
## x <- read.csv.ffdf(file = "~/Downloads/allyears.csv",
##                    header = TRUE, VERBOSE = TRUE,
##                    nrows = 1000000,
##                    first.rows = 50000, next.rows = 50000,
##                    colClasses = NA)
## typeof(x) # list
## is.data.frame(x) # FALSE

## ----eval=FALSE----------------------------------------------------------
## library(bigmemory)
## library(biganalytics)
## 
## x <- read.big.matrix(
##   "~/Downloads/allyears.csv", header = TRUE ,
##   backingfile = "~/Downloads/airline.bin",
##   descriptorfile = "~/Downloads/airline.desc"
##   )

## ----eval=FALSE----------------------------------------------------------
## if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
## if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
## install.packages("h2o")

## ----eval=FALSE----------------------------------------------------------
## library(h2o)
## localH2O <- h2o.init(min_mem_size = "20g")

## ----eval=FALSE----------------------------------------------------------
## # h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,
## #          forceDL = FALSE, enable_assertions = TRUE, license = NULL,
## #          nthreads = -2, max_mem_size = NULL, min_mem_size = NULL,
## #          ice_root = tempdir(), strict_version_check = TRUE,
## #          proxy = NA_character_, https = FALSE, insecure = FALSE,
## #          username = NA_character_, password = NA_character_)

## ----eval=FALSE----------------------------------------------------------
## java version "1.8.0_73"
## Java(TM) SE Runtime Environment (build 1.8.0_73-b02)
## Java HotSpot(TM) 64-Bit Server VM (build 25.73-b02, mixed mode)
## 
## Starting H2O JVM and connecting: .. Connection successful!
## 
## R is connected to the H2O cluster:
##     H2O cluster uptime:         1 seconds 772 milliseconds
##     H2O cluster version:        3.8.1.3
##     H2O cluster name:           H2O_started_from_R_cchoirat_nwc910
##     H2O cluster total nodes:    1
##     H2O cluster total memory:   19.17 GB
##     H2O cluster total cores:    8
##     H2O cluster allowed cores:  2
##     H2O cluster healthy:        TRUE
##     H2O Connection ip:          localhost
##     H2O Connection port:        54321
##     H2O Connection proxy:       NA
##     R Version:                  R version 3.2.3 (2015-12-10)
## 

## ----eval=FALSE----------------------------------------------------------
## h2o.clusterInfo()

## ----eval=FALSE----------------------------------------------------------
## R is connected to the H2O cluster:
##     H2O cluster uptime:         8 minutes 44 seconds
##     H2O cluster version:        3.8.1.3
##     H2O cluster name:           H2O_started_from_R_cchoirat_nwc910
##     H2O cluster total nodes:    1
##     H2O cluster total memory:   19.17 GB
##     H2O cluster total cores:    8
##     H2O cluster allowed cores:  2
##     H2O cluster healthy:        TRUE
##     H2O Connection ip:          localhost
##     H2O Connection port:        54321
##     H2O Connection proxy:       NA
##     R Version:                  R version 3.2.3 (2015-12-10)

## ----eval=FALSE----------------------------------------------------------
## data(cars)
## cars_to_h2o <- as.h2o(cars, destination_frame = "cars_from_r")
## is.data.frame(cars_to_h2o) # FALSE
## class(cars_to_h2o) # H2OFrame

## ----eval=FALSE----------------------------------------------------------
## summary(cars_to_h2o) # calls h2o::summary(cars_to_h2o)

## ----eval=FALSE----------------------------------------------------------
##  speed          dist
##  Min.   : 4.0   Min.   :  2.00
##  1st Qu.:12.0   1st Qu.: 26.00
##  Median :15.0   Median : 36.00
##  Mean   :15.4   Mean   : 42.98
##  3rd Qu.:19.0   3rd Qu.: 56.00
##  Max.   :25.0   Max.   :120.00
## Warning message:
## In summary.H2OFrame(cars_to_h2o) :
##   Approximated quantiles computed! If you are interested in exact quantiles,
##   please pass the `exact_quantiles=TRUE` parameter.

## ----eval=FALSE----------------------------------------------------------
## airlines_path <- "/Users/cchoirat/Downloads/allyears2k.csv" # full path
## airlines_to_h2o <- h2o.importFile(path = airlines_path,
##                                   destination_frame = "airlines_from_r")
## summary(airlines_to_h2o)

## ----eval=FALSE----------------------------------------------------------
## airlines_full_path <- "/Users/cchoirat/Downloads/allyears.csv" # full path
## airlines_full_to_h2o <- h2o.importFile(path = airlines_full_path,
##                                        destination_frame = "airlines_full_from_r")
## summary(airlines_to_h2o)

## ----eval=FALSE----------------------------------------------------------
## summary(airlines_full_to_h2o)
## dim(airlines_full_to_h2o) # 123534969 x 31

## ----eval=FALSE----------------------------------------------------------
## h2ofit <- h2o.glm(y = "IsArrDelayed", x = "Distance",
##                   training_frame = airlines_full_to_h2o,
##                   family = "binomial")
## h2ofit
## summary(h2ofit)

## ----eval=FALSE----------------------------------------------------------
## airlines <- read.csv("~/Downloads/allyears2k.csv")
## rfit <- glm(IsArrDelayed ~ Distance, data = airlines, family = binomial(link = "logit"))
## summary(rfit)

## ----eval=FALSE----------------------------------------------------------
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept) 1.757e-01  1.546e-02  11.361  < 2e-16 ***
## Distance    6.642e-05  1.666e-05   3.986 6.71e-05 ***

## ----eval=FALSE----------------------------------------------------------
## h2ofit <- h2o.glm(y = "IsArrDelayed", x = "Distance",
##                   training_frame = airlines_to_h2o,
##                   standardize = FALSE,
##                   family = "binomial")
## h2ofit

## ----eval=FALSE----------------------------------------------------------
## Coefficients: glm coefficients
##       names coefficients
## 1 Intercept     0.175590
## 2  Distance     0.000066

## ----eval=FALSE----------------------------------------------------------
## h2ofit_full <- h2o.glm(y = "IsArrDelayed", x = "Distance",
##                        training_frame = airlines_full_to_h2o,
##                        standardize = FALSE,
##                        family = "binomial")
## h2ofit_full

## ----eval=FALSE----------------------------------------------------------
## Coefficients: glm coefficients
##       names coefficients
## 1 Intercept    -0.071491
## 2  Distance     0.000040

## ----eval=FALSE----------------------------------------------------------
## h2o.shutdown()

