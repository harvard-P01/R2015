h2o.shutdown()
# library(h2o)

# localH2O = h2o.init(ip = 'localhost', port = 54321, nthreads = -1)
localH2O = h2o.init(min_mem_size = "20g")
h2o.clusterInfo()
data(cars)
as.h2o(cars)

airlinesURL = "/Users/cchoirat/Downloads/allyears2k.csv"
airlines.hex = h2o.importFile(path = airlinesURL, destination_frame = "airlines.hex")
summary(airlines.hex)

airlinesURL2 = "/Users/cchoirat/Downloads/allyears.csv"
airlines2.hex = h2o.importFile(path = airlinesURL2, destination_frame = "airlines2.hex")
summary(airlines2.hex)
