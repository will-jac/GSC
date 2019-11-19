
show_results = function(customer.df, facility.df, connect, open, numSizes) {
  #int read in the files
  #connect <- read.csv(file=connect_filename, header=FALSE, sep=",")
  #open <- read.csv(file=open_filename, header=FALSE, sep=",")

  plot(customer.df$x, customer.df$y, asp=1, type = "n")

  mod = length(facility.df$x)

  colors = rainbow(mod)

  # display customers connecting
  ## get an array of sizes to index into
  ## we'll be indexing into this via the d variable
  min = min(customer.df$d)
  max = max(customer.df$d)
  n = 5 ## manually set - this sets the max cex size
  sizeRange = pracma::logspace(min, max, n)


  find_size = function(d, range) {
    size = 1
    for (s in range) {
      if (s < d) {
        size = size + 1
      }
      else {
        break
      }
    }
    return(size)
  }

  for (i in 1:nrow(connect)) {
    f = FALSE
    for (j in 1:ncol(connect)) {
      if (connect[i,j] == 1) {
        f = TRUE
        k = j %% mod
        if (k == 0) {
          k = mod
        }
        customer_size = find_size(customer.df$d[i], sizeRange)

        points(customer.df$x[i], customer.df$y[i], col=colors[k], cex = customer_size)
        #lines(c(customer.df$x[i], facility.df$x[k]), c(customer.df$y[i], facility.df$y[k]), col="red")
        break
      }
    }
    #if (f == FALSE) {
    #  print(i)
    #}
  }

  # display the open facilities
  k = -1
  i = 1
  sizes = c(5, 10, 15)
  for (facility in open) {
    #print(facility)
    if (facility == 1) {
      k = (i %% mod) + 1
      if (k == 0) {
        k = mod
      }
      size = sizes[ceiling(i / mod)]
      points(facility.df$x[k], facility.df$y[k], pch=".", cex=size, col=colors[k])
      points(facility.df$x[k], facility.df$y[k], pch=22,  cex=size / 5 , col="black")

    }
    i = i + 1
  }
}
