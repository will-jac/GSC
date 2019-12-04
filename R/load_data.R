filename = "C:/Users/Jack/College/Fall2019/Green-Supply-Chain/code/data/geotiff/hrsl_pri_pop.tif"

load_data = function(filename, reduceByFacility=100, reduceByClient=1000, dropZeros=TRUE, ...) {
  print("loading data")
  # look in the current directory
  data.raster <- raster::raster(filename)
  data.raster = raster::readAll(data.raster)
  crs = "+proj=utm +zone=20 +ellps=clrk66 +towgs84=11,72,-101,0,0,0,0 +units=m +no_defs"

  # project it into km!
  data.raster <-raster::projectRaster(data.raster, crs=crs)

  # we expect 30 m by 30 m resolution

  dimms = dim(data.raster)
  x = dimms[1]
  y = dimms[2]


  x.c = x / reduceByClient
  y.c = y / reduceByClient
  x.f = x / reduceByFacility
  y.f = y / reduceByFacility


  # subset it into something more granular
  rc = raster::aggregate(data.raster, fun="sum", fact=c(x.c, y.c))
  rf = raster::aggregate(data.raster, fun="sum", fact=c(x.f, y.f))
  #print("dimensions")
  #print(dim(rc))
  #print(dim(rf))

  data.df <- raster::as.data.frame(rc, xy=TRUE, na.rm=TRUE)
  names(data.df) <- c("x", "y", "d")
  if (dropZeros) {
    data.df <- dplyr::filter(data.df, d != 0)
  }

  reduced.df <- raster::as.data.frame(rf, xy=TRUE, na.rm=TRUE)
  names(reduced.df) <- c("x", "y", "d")

  print(paste("customers:", length(data.df$x), "facilities:", length(reduced.df$x),  sep=" "))

  return(list("customer.df" = data.df, "facility.df" = reduced.df))
}
