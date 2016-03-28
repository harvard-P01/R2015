## ----eval=FALSE----------------------------------------------------------
## chmod 600 Downloads/rstudio.pem
## ssh -i Downloads/rstudio.pem ubuntu@ec2-54-175-200-1.compute-1.amazonaws.com

## ----eval=FALSE----------------------------------------------------------
## sudo apt-get update
## sudo apt-get install r-base

## ----eval=FALSE----------------------------------------------------------
## sudo apt-get install gdebi-core
## wget https://download2.rstudio.org/rstudio-server-0.99.893-amd64.deb
## sudo gdebi rstudio-server-0.99.893-amd64.deb

## ----eval=FALSE----------------------------------------------------------
## sudo adduser rstudio

## ----eval=FALSE----------------------------------------------------------
## www-port=80

## ----eval=FALSE----------------------------------------------------------
## library("RStudioAMI")
## linkDropbox()
## excludeSyncDropbox("*")
## includeSyncDropbox("Dropbox/DataAWS/")

## ----eval=FALSE----------------------------------------------------------
## flights <- read.csv("Dropbox/DataAWS/allyears2k.csv")
## names(flights)
## glm(IsArrDelayed ~ Distance, data = flights, family = binomial("logit"))

