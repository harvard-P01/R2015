library(data.table)
flight <- fread("~/Downloads/allyears2k.csv")
head(flight)

##----- glm logit in base R

flight[, binDelay := 0]
flight[IsArrDelayed == "YES", binDelay := 1]
flight <- as.data.frame(flight)

fit <- glm(binDelay ~ Distance, data = flight, family = binomial(link = "logit"))
summary(fit)

##----- Big glm

library(biglm)
# Regression for data too large to fit in memory
bigfit <- bigglm(binDelay ~ Distance, data = flight, family = binomial(link = "logit"))
summary(bigfit)

##----- speedglm

library(speedglm)
speedfit <- speedglm(binDelay ~ Distance, data = flight, family = binomial(link = "logit"))
speedfit

##----- h20

library(h2o)
localH2O <- h2o.init() # session not persistent if started from R
# localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,min_mem_size = "3g")
h2o.clusterInfo()
dim(flight)
is.data.frame(flight)
flight_h2o <- as.h2o(flight)
dim(flight_h2o)
typeof(flight_h2o)
is.data.frame(flight_h2o)
# open http://localhost:54321
summary(flight)
summary(flight_h2o)
h2ofit <- h2o.glm(y = "binDelay", x = "Distance", training_frame = flight_h2o, family = "binomial")
h2ofit
h2o.shutdown()

##----- What if data fits on drive but not in RAM?

# 1. ----- Database to the rescue: SQLite

# https://www.sqlite.org/cvstrac/wiki?p=ImportingFiles
# library(dplyr)
# library(RSQLite)
# flight_db <- src_sqlite("flights.sqlite3", create = TRUE)
# copy_to(dest = flight_db, df = flight)

sqlite    <- dbDriver("SQLite")
flight_db <- dbConnect(sqlite,"flights.sqlite3")
dbWriteTable(conn = flight_db, name = "fligh_table", value = flight, row.names = FALSE)
dbListTables(flight_db)
bigglm(binDelay ~ Distance, data = flight_db, tablename = "fligh_table",
       family = binomial(link = "logit"))

# 2. ----- h20 again
# path can be a URL

flights_hex <- h2o.importFile(path = "/Users/cchoirat/Downloads/allyears2k.csv", 
                              destination_frame = "flights_hex_cloud")
is.data.frame(flights_hex)
class(flights_hex)
h2ofit <- h2o.glm(y = "binDelay", x = "Distance", training_frame = flights_hex, family = "binomial")

##----- Where can it be deployed?


