generate_uniform = function(x=20, y=20, incrementByCust = 0.1, incrementByFac = 1) {

  num_x = x / incrementByCust
  num_y = y / incrementByCust

  region = data.frame(
    "x" = rep(pracma::linspace(1, x, num_x), each = num_y),
    "y" = rep(pracma::linspace(1, y, num_y), times = num_x),
    "d" = rep(10000, times = num_x * num_y))

  num_x = x / incrementByFac
  num_y = y / incrementByFac

  reduced_region = data.frame(
    "x" = rep(pracma::linspace(1, x, num_x), each = num_y),
    "y" = rep(pracma::linspace(1, y, num_y), times = num_x),
    "d" = rep(1000, times = num_x * num_y))

  return(list("customer.df" = region, "facility.df" = reduced_region))
}
