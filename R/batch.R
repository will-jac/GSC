run_GSC = function(filename=GSC::filename, c = 100, f = 200, em = 100,
                 time_lim=60*60, optim_lim=0.01, emphasis=3, sol_lim=10,
                 emissions = TRUE, operating = TRUE, data_cache = NULL) {
  if (is.null(data_cache)) {
    a = load_data(filename=filename, c=c, f=f)
  }
  else {
    a = data_cache
  }

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
batch = function(em_seq = seq(10, 300, 10), ...) {
  n = length(em_seq)
  result = vector(mode='list', length=n + 2)
  result[[1]] = GSC::run_GSC(operating = TRUE, emissions = FALSE, ...)
  for (i in 1:n) {
    result[[i+1]] = GSC::run_GSC(em = em_seq[i], ...)
  }
  result[[n+2]] = GSC::run_GSC(em=1, operating = FALSE, emissions = TRUE, ...)
  return(result)
}

vehicle_batch = function(car_coef = 1, truck_coef = 1, car_fuel_coef = 1, truck_fuel_coef = 1, ...) {
  package$car_coef = car_coef
  package$truck_coef= truck_coef
  package$car_p_f = car_fuel_coef * package$car_p_f
  package$truck_p_f = truck_fuel_coef * package$truck_p_f
  return(batch(...))
}

store_batch = function(store_e = 126.1, store_p_f = 22.9, rent = 212.8, ...) {
  package$store_e = store_e
  package$store_p_f = store_p_f
  package$store_v = rent
  return(batch(data_cache=data_cache, ...))
}

run_simulations = function(filename=GSC::filename, c=100, f=125, ...) {
  library(GSC)

  data_cache = load_data(filename=filename, c=c, f=f)

  to_return = list()
  base = batch(c=c, f=f, data_cache=data_cache, ...)
  car = vehicle_batch(car_coef = 1/2, c=c, f=f, data_cache=data_cache, ...)
  truck = vehicle_batch(truck_coef = 1/2, data_cache=data_cache, ...)
  car_truck = vehicle_batch(truck_coef = 1/2, car_coef = 1/2, c=c, f=f, data_cache=data_cache,...)
  fuel = vehicle_batch(car_fuel_coef = 2, truck_fuel_coef = 2, c=c, f=f, data_cache=data_cache,...)
  low_elec = store_batch(store_e = 60, c=c, f=f, data_cache=data_cache,...)
  high_elec = store_batch(store_e = 183.9, c=c, f=f, data_cache=data_cache,...)
  high_rent = store_batch(rent = 425.7, c=c, f=f, data_cache=data_cache,...)
  low_e_high_r = store_batch(rent=425.7, store_e=60, c=c, f=f, data_cache=data_cache,...)
  store_fuel = store_batch(store_e = 304, store_p_f = 55.4, c=c, f=f, data_cache=data_cache,...)
  store_fuel_rent = store_batch(rent=425.7, store_e = 304, store_p_f = 55.4, c=c, f=f, data_cache=data_cache,...)

  return(list(
    "base" = base,
    "car" = car,
    "truck" = truck,
    "car_truck" = car_truck,
    "gas_price" = fuel,
    "low_elec" = low_elec,
    "high_elec" = high_elec,
    "high_rent" = high_rent,
    "low_e_high_r" = low_e_high_r,
    "store_high_fuel" = store_fuel,
    "store_high_fuel_rent" = store_fuel_rent
    ))
}


