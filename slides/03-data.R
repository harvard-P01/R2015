## ------------------------------------------------------------------------
data(mtcars)
head(mtcars)

## ------------------------------------------------------------------------
summary(fit <- lm(mpg ~ cyl + disp + hp, data = mtcars))

## ------------------------------------------------------------------------
formula(fit)

## ------------------------------------------------------------------------
head(model.frame(fit))
head(model.response(model.frame(fit)))

## ------------------------------------------------------------------------
head(model.matrix(fit))

## ------------------------------------------------------------------------
summary(update(fit, mpg ~ . - 1 - hp))

## ------------------------------------------------------------------------
linmodEst <- function(x, y) {
  ## compute QR-decomposition of x
  qx <- qr(x)
  ## compute (x’x)^(-1) x’y
  coef <- solve.qr(qx, y)
  ## degrees of freedom and standard deviation of residuals
  df <- nrow(x) - ncol(x)
  sigma2 <- sum((y - x %*% coef) ^ 2) / df
  ## compute sigma^2 * (x’x)^-1
  vcov <- sigma2 * chol2inv(qx$qr)
  colnames(vcov) <- rownames(vcov) <- colnames(x)
  list(
    coefficients = coef,
    vcov = vcov,
    sigma = sqrt(sigma2),
    df = df
  )
}

## ------------------------------------------------------------------------
data(mtcars)
X <- as.matrix(mtcars[, c("cyl", "disp", "hp")])
y <- as.matrix(mtcars[, "mpg"])
linmodEst(y, cbind(1, X))

## ------------------------------------------------------------------------
linmod <- function(x, ...)
  UseMethod("linmod")

linmod.default <- function(x, y, ...) {
  x <- as.matrix(x)
  y <- as.numeric(y)
  est <- linmodEst(x, y)
  est$fitted.values <- as.vector(x %*% est$coefficients)
  est$residuals <- y - est$fitted.values
  est$call <- match.call()
  class(est) <- "linmod"
  return(est)
}

## ------------------------------------------------------------------------
X <- mtcars[, c("cyl", "disp", "hp")]
y <- mtcars[, "mpg"]
fit2 <- linmod(cbind(1, X), y)
fit2$call

## ----eval=FALSE----------------------------------------------------------
## formula(fit2)
## model.frame(fit2)
## model.matrix(fit2)

## ------------------------------------------------------------------------
print.linmod <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}

## ------------------------------------------------------------------------
fit2

## ------------------------------------------------------------------------
linmod.formula <- function(formula, data = list(), ...) {
  mf <- model.frame(formula = formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)
  est <- linmod.default(x, y, ...)
  est$call <- match.call()
  est$formula <- formula
  return(est)
}

## ------------------------------------------------------------------------
X <- mtcars[, c("cyl", "disp", "hp")]
y <- mtcars[, "mpg"]
fit3 <- linmod(mpg ~ 1 + disp + cyl, data = mtcars)
fit3
formula(fit3)

## ------------------------------------------------------------------------
update(fit3, mpg ~ 1 + disp) # Intercept to be dealt with
model.frame(fit3, data = mtcars)

## ------------------------------------------------------------------------
attr(model.frame(fit3, data = mtcars), "terms")

## ------------------------------------------------------------------------
model.matrix(attr(model.frame(fit3, data = mtcars), "terms"), data = mtcars)

## ------------------------------------------------------------------------
linmod(mpg ~ ., data = mtcars)

## ------------------------------------------------------------------------
authors <- data.frame(
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))
books <- data.frame(
  name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA,
                   "Venables & Smith"))

## ------------------------------------------------------------------------
authors

## ------------------------------------------------------------------------
books

## ------------------------------------------------------------------------
merge(authors, books, by.x = "surname", by.y = "name")

## ------------------------------------------------------------------------
merge(books, authors, by.x = "name", by.y = "surname")

## ------------------------------------------------------------------------
merge(authors, books, by.x = "surname", by.y = "name", all = TRUE)
merge(authors, books, by.x = "surname", by.y = "name", all.x = TRUE)
merge(authors, books, by.x = "surname", by.y = "name", all.y = TRUE)

## ------------------------------------------------------------------------
library(dplyr)
dp_authors <- tbl_df(authors)
dp_books <- tbl_df((books))

## ------------------------------------------------------------------------
inner_join(dp_books, dp_authors, by = c("name" = "surname"))

## ------------------------------------------------------------------------
right_join(dp_books, dp_authors, by = c("name" = "surname"))

## ------------------------------------------------------------------------
left_join(dp_books, dp_authors, by = c("name" = "surname"))

## ------------------------------------------------------------------------
library(data.table)
dt_authors <- setDT(authors)
dt_books <- data.table((books)) # same as 'setDT'

## ------------------------------------------------------------------------
dt_authors
dt_books

## ------------------------------------------------------------------------
setkey(dt_authors, surname); dt_authors
setkey(dt_books, name); dt_books

## ------------------------------------------------------------------------
dt_authors[dt_books]
dt_books[dt_authors]

## ------------------------------------------------------------------------
library(nycflights13)
head(flights)

## ------------------------------------------------------------------------
library(dplyr)
library(data.table)

## Default is dplyr table
dp_flights <- flights
## Explicit conversion
## dp_flights <- tbl_df(flights)
## Base data frame
df_flights <- data.frame(flights)
## Data table
dt_flights <- data.table(flights)

## ------------------------------------------------------------------------
head(df_flights)

## ------------------------------------------------------------------------
dp_flights # Note column types

## ------------------------------------------------------------------------
dt_flights

## ------------------------------------------------------------------------
head(df_flights[df_flights$month == 1 & df_flights$day == 1, ])
head(subset(df_flights, month == 1 & day ==1))

## ------------------------------------------------------------------------
head(df_flights[1:10, ])

## ----eval=FALSE----------------------------------------------------------
## head(df_flights[1:10, ])
## slice(dp_flights, 1:10)
## dt_flights[1:10]

## ------------------------------------------------------------------------
head(with(df_flights, df_flights[order(year, month, day), ]))

## ----eval=FALSE----------------------------------------------------------
## head(with(df_flights, df_flights[order(year, month, day), ]))
## arrange(dp_flights, year, month, day)
## dt_flights[order(year, month, day)]

## ------------------------------------------------------------------------
head(with(df_flights, df_flights[order(-arr_delay), ]))

## ----eval=FALSE----------------------------------------------------------
## head(with(df_flights, df_flights[order(-arr_delay), ]))
## arrange(flights, desc(arr_delay))
## dt_flights[order(-arr_delay)]

## ------------------------------------------------------------------------
head(select(df_flights, year, month, day))

## ----eval=FALSE----------------------------------------------------------
## head(select(df_flights, year, month, day))
## select(dp_flights, year, month, day)
## dt_flights[, .(year, month, day)]
## dt_flights[, list(year, month, day)]

## ------------------------------------------------------------------------
## Not as expected!
dt_flights[, c("year", "month", "day")]
dt_flights[, c("year", "month", "day"), with = FALSE]

## ------------------------------------------------------------------------
head(unique(df_flights, by = "tailnum"))

## ----eval=FALSE----------------------------------------------------------
## head(unique(df_flights, by = "tailnum"))
## distinct(select(flights, tailnum))
## unique(dt_flights, by = "tailnum")

## ------------------------------------------------------------------------
## Copy
df_flights$gain <-
  df_flights$arr_delay - df_flights$dep_delay
df_flights$speed <-
  df_flights$distance / df_flights$air_time * 60

## ------------------------------------------------------------------------
## Copy
mutate(dp_flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

## ------------------------------------------------------------------------
## In-place
dt_flights[, gain := arr_delay - dep_delay]
dt_flights[, speed := arr_delay - dep_delay,
           distance / air_time * 60]

## ------------------------------------------------------------------------
df_flights_2 <- data.frame(gain = df_flights$arr_delay - df_flights$dep_delay,
                           speed = df_flights$distance / df_flights$air_time * 60)
head(df_flights_2)

## ------------------------------------------------------------------------
transmute(dp_flights,
          gain = arr_delay - dep_delay,
          speed = distance / air_time * 60)

## ------------------------------------------------------------------------
dt_flights[, .(gain = arr_delay - dep_delay,
               gain_per_hour = gain / (air_time / 60))]

## ------------------------------------------------------------------------
mean(df_flights$dep_delay, na.rm = TRUE)

## ----eval=FALSE----------------------------------------------------------
## summarise(dp_flights,
##           delay = mean(dep_delay, na.rm = TRUE))

## ----eval=FALSE----------------------------------------------------------
## dt_flights[, .(delay = mean(dep_delay, na.rm = TRUE))]

## ----eval=FALSE----------------------------------------------------------
## set.seed(1234)
## head(df_flights[sample(nrow(df_flights), 10), ])
## sample_n(dp_flights, 10)
## dt_flights[sample(.N, 10)]

## ----eval=FALSE----------------------------------------------------------
## head(df_flights[sample(round(.N * 0.01)), ])
## sample_frac(dp_flights, 0.01)
## dt_flights[sample(round(.N * 0.01))]

## ------------------------------------------------------------------------
by_tailnum <- group_by(dp_flights, tailnum)
dp_delay <- summarise(by_tailnum,
                      count = n(),
                      dist = mean(distance, na.rm = TRUE),
                      delay = mean(arr_delay, na.rm = TRUE))
dp_delay <- filter(dp_delay, count > 20, dist < 2000)
dp_delay

## ------------------------------------------------------------------------
dt_delay <-
  dt_flights[, .(count = .N,
                 dist = mean(distance, na.rm = TRUE),
                 delay = mean(arr_delay, na.rm = TRUE)),
             by = list(tailnum)][count > 20 &  dist < 2000]
dt_delay

## ------------------------------------------------------------------------
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)
a4

## ------------------------------------------------------------------------
filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
      ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
    ),
  arr > 30 | dep > 30
  )

## ------------------------------------------------------------------------
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
    ) %>%
  filter(arr > 30 | dep > 30)

## ------------------------------------------------------------------------
dt_delay <- dt_flights[, .(count = .N,
                           dist = mean(distance, na.rm = TRUE),
                           delay = mean(arr_delay, na.rm = TRUE)),
                       by = list(tailnum)][count > 20 &  dist < 2000]
dt_delay

## ------------------------------------------------------------------------
library(magrittr)
car_data <- 
  mtcars %>%
  subset(hp > 100) %>%
  aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
  transform(kpl = mpg %>% multiply_by(0.4251)) %>%
  print

## ------------------------------------------------------------------------
dt_mtcars <- data.table(mtcars)
dt_car_data <- dt_mtcars[hp > 100][, lapply(.SD, function(x) round(mean(x), 2)),
                                   by = "cyl"][, kpl := mpg * 0.4251]
print(dt_car_data)

## ------------------------------------------------------------------------
dt_mtcars <- data.table(mtcars)
names(dt_mtcars)

f <- function(dt, selected_variables = c("gear", "carb")) {
  return(dt[, selected_variables])
}

f(dt_mtcars)

## ------------------------------------------------------------------------
f <- function(dt, selected_variables = c("gear", "carb")) {
  return(dt[get(selected_variables)])
}

f(dt_mtcars)

## ------------------------------------------------------------------------
dt_mtcars[, .N, by = "cyl"]

## ------------------------------------------------------------------------
dt_mtcars[, .SD]

## ------------------------------------------------------------------------
setkey(dt_mtcars, "cyl")
dt_mtcars

## ------------------------------------------------------------------------
setkeyv(dt_mtcars, c("cyl", "gear"))
dt_mtcars

## ------------------------------------------------------------------------
tables()

## ------------------------------------------------------------------------
dt_mtcars[, sum(disp)]

## ------------------------------------------------------------------------
dt_mtcars[, lapply(.SD, sum), .SDcols = names(dt_mtcars)]

## ------------------------------------------------------------------------
dt_mtcars[list(6, 4)] # on 'cyl' and 'gear' resp.

## ------------------------------------------------------------------------
dt_mtcars[, list(6, 4)]

