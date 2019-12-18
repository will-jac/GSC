
show_results_obj = function(result_object) {
  show_results(result_object$cust.loc, result_object$fac.loc, result_object$connect, result_object$open, 3)
}

show_results = function(customer.df, facility.df, connect, open, numSizes) {

  plot(customer.df$x, customer.df$y, asp=1, type = "n")

  mod = length(facility.df$x)

  colors = rainbow(mod)

  # display customers connecting
  ## get an array of sizes to index into
  ## we'll be indexing into this via the d variable

  sizeRange = pracma::linspace(0,  max(customer.df$d), 10)

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
      if (connect[i,j]) {
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
    if (f == FALSE) {
      points(customer.df$x[i], customer.df$y[i])
    }
  }

  # display the open facilities
  sizes = c(5, 10, 15)
  i = 1
  for (i in i:length(open)) {
    if (open[i]) {
      k = i %% mod
      if (k == 0) {
        k = mod
      }
      size = sizes[ceiling(i / mod)]
      points(facility.df$x[k], facility.df$y[k], pch=".", cex=size, col=colors[k])
      points(facility.df$x[k], facility.df$y[k], pch=22,  cex=size / 5 , col="black")
    }
  }
}
