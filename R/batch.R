batch_old = function() {
  setwd("r1")
  r = GSC::do_batch(c=200, f=400, em=100)
  setwd("../r2")
  r = GSC::do_batch(c=200, f=400, em=200)
  setwd("../r3")
  r = GSC::do_batch(c=200, f=400, em=500)
  setwd("../r4")
  r = GSC::do_batch(c=100, f=300)
  setwd("../r5")
  r = GSC::do_batch(c=100, f=300, em=200)
  setwd("../r6")
  r = GSC::do_batch(c=100, f=300, em=500)
}

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
  to_ret = list('cust.loc'=a$customer.df, 'fac.loc'=a$facility.df, 'connect' = c2$connect, 'open' = c2$open, 'cost'=c2$cost, 'cust.cost' = b2$c, 'fac.cost' = b2$f)

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
  result = vector(mode='list', length=n + 2, operating = TRUE, emissions = FALSE)
  result[[1]] = GSC::run_batch(c=c, f=f, em=0, ...)
  for (i in 1:n) {
    result[[i+1]] = GSC::run_batch(c=c, f=f, em = em_seq[i], ...)
  }
  result[[n+2]] = GSC::run_batch(c=c, f=f, em=0, operating = FALSE, emissions = TRUE, ...)
  return(result)
}

vehicle_batch = function(car_coef = 1, truck_coef = 1, car_fuel_coef = 1, truck_fuel_coef = 1, ...) {
  package$car_coef = car_coef
  package$truck_coef= truck_coef
  package$car_p_f = car_fuel_coef * package$car_p_f
  package$truck_p_f = truck_fuel_coef * package$truck_p_f
  return(batch(...))
}

convert_list = function(results_list) {
  n = length(results_list)
  result = vector(mode='list', length=n/3)
  j = 1
  for (i in seq(1, n, 3)) {
    r = list("operating" = results_list[i]$operating, "hybrid" = results_list[i+1]$hybrid, "emissions" = results_list[i+2]$emissions)
    result[[j]] = r
    j = j + 1
  }
  return(result)
}


batch_stats = function(results_list) {
  n = length(results_list)
  stats = vector(mode='list', length=n)
  for (i in 1:n) {s
    r = results_list[[i]]
    stats[[i]] = list(
      "stores" = GSC::num_stores(r),
      "em_cost" = GSC::emissions_cost_penalty(r),
      "op_cost" = GSC::operating_cost_penalty(r),
      "hybrid_em_cost" = GSC::hybrid_emissions_penalty(r),
      "hybrid_op_cost" = GSC::hybrid_operating_penalty(r)
    )
  }
  return(stats)
}

extract_stat = function(stats_list) {
  n = length(stats_list)
  stat = c()
  for (i in 1:n) {
    s = stats_list[[i]]
    ## CHANGE THIS LINE DEPENDING ON WHAT YOU WANT
    stat = append(stat, s$hybrid_op_cost)
  }
  return(stat)
}
