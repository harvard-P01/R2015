library(data.table)

## Subset
d <- data.table::fread("~/Downloads/allyears2k.csv")
d

## H20

# http://www.h2o.ai/download/h2o/r

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tukey/6/R")))

library(h2o)
# localH2O = h2o.init()
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,min_mem_size = "3g")

# Finally, let's run a demo to see H2O at work.
demo(h2o.glm)

## Updload the dataset to the h2o instance

# iris.hex <-  h2o.importFile(localH2O, path = "~/Downloads/allyears2k.csv")
mnistPath = '~/Downloads/allyears2k.csv'
mnist.hex = h2o.uploadFile(path = mnistPath, destination_frame = "mnist.hex")

read.csv("~/Downloads/allyears2k.csv")
