library(dplyr)
d <- tbl_df(read.csv("annual_all_2015.csv"))
my_db <- src_sqlite("mydb.sqlite3", create = TRUE)

copy_to(my_db, d, temporary = FALSE)

d_sql <- tbl(my_db, "d")

names(d)

names(d_sql)

d_sql %>%
  group_by(State.Code) %>%
  select(State.Code, Latitude:Longitude, Arithmetic.Mean) %>%
  summarize(mean(Arithmetic.Mean))
dd$query
