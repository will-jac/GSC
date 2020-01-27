run_GSC = function(filename=GSC::filename, c = 200, f = 400, em = 100,
                 time_lim=60*60, optim_lim=0.01, emphasis=3, sol_lim=10,
                 emissions = TRUE, operating = TRUE) {
  a = load_data(filename, f, c)

  if (operating) {
    package$OPERATING_COST_INDICATOR=1
  }
  else {
    package$OPERATING_COST_INDICATOR=0
  }
  if (emissions) {
    package$EMISSIONS_PRICE_TON=em
  }
  else {
    package$EMISSIONS_PRICE_TON=0
  }

  b = compute_diss(a$customer.df, a$facility.df)

  reticulate::source_python(paste(system.file(package="GSC"), "partition.py", sep="/"))

  c = partition(b$c, as.vector(t(b$f)), as.vector(t(b$s)), as.vector(b$d), 3, sol_lim, time_lim, optim_lim, emphasis)
  to_ret = list('cust.loc'=a$customer.df, 'fac.loc'=a$facility.df,
                'connect' = c$connect, 'open' = c$open,
                'cost'=c$cost, 'em.cost' = package$EMISSIONS_PRICE_TON,
                'cust.cost' = b$c, 'fac.cost' = b$f)

  return (to_ret)
}


#' Runs a batch of GSC simulations, based on the emissions sequence (cost of emissions, $/ton CO2)
#'
#' number of generated models is length(em_seq) + 2, with the first and last elements being the
#' operating cost and the emissions cost minimizing simulations, respectively
#'
#' @param c divide by customer (bigger = smaller model)
#' @param f divide by facility (bigger = smaller model)
#' @param em_seq sequence of emissions cost.
#'
#' @return list of GSC simulations, with an operating minimizing and emissions cost minimizing system at the
#' beginning and end of the list
#'
#' @export
#'
#' results = batch()
batch = function(c = 100, f = 300, em_seq = seq(10, 300, 10), ...) {
  n = length(em_seq)
  result = vector(mode='list', length=n + 2)
  result[[1]] = GSC::run_GSC(c=c, f=f, em=0, operating = TRUE, emissions = FALSE, ...)
  for (i in 1:n) {
    result[[i+1]] = GSC::run_GSC(c=c, f=f, em = em_seq[i], ...)
  }
  result[[n+2]] = GSC::run_GSC(c=c, f=f, em=1, operating = FALSE, emissions = TRUE, ...)
  return(result)
}

vehicle_batch = function(car_coef = 1, truck_coef = 1, car_fuel_coef = 1, truck_fuel_coef = 1, ...) {
  package$car_coef = car_coef
  package$truck_coef= truck_coef
  package$car_p_f = car_fuel_coef * package$car_p_f
  package$truck_p_f = truck_fuel_coef * package$truck_p_f
  return(batch(...))
}


run_simulations = function(...) {
  library(GSC)
  to_return = list()
  emissions = batch(...)
  car = vehicle_batch(car_coef = 1/2, ...)
  truck = vehicle_batch(truck_coef = 1/2, ...)
  car_truck = vehicle_batch(truck_coef = 1/2, car_coef = 1/2, ...)
  fuel = vehicle_batch(car_fuel_coef = 2, truck_fuel_coef = 2, ...)
  return(list(
    "emissions" = emissions,
    "car" = car,
    "truck" = truck,
    "car_truck" = car_truck,
    "fuel" = fuel
  ))
}


