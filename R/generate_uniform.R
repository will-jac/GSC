generate_uniform = function(x=20, y=20, d=1000, incrementByCust = 0.1, incrementByFac = 1) {

  num_x = x / incrementByCust
  num_y = y / incrementByCust

  region = data.frame(
    "x" = rep(pracma::linspace(1, x, num_x), times = num_y),
    "y" = rep(pracma::linspace(1, y, num_y), each = num_x),
    "d" = rep(d, times = num_x * num_y))

  num_x = x / incrementByFac
  num_y = y / incrementByFac

  reduced_region = data.frame(
    "x" = rep(pracma::linspace(1, x, num_x), times = num_y),
    "y" = rep(pracma::linspace(1, y, num_y), each = num_x),
    "d" = rep(1, times = num_x * num_y))

  return(list("customer.df" = region, "facility.df" = reduced_region))
}
