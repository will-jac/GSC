test_uniform = function(x=10, y=10, op = 1, em = 1, sol_lim=10, time_lim=60*60, optim_lim=0.01, emphasis=3) {
  #a = GSC::load_data(filename, reduce_by_facility, reduce_by_customer)
  a = GSC::generate_uniform(incrementByCust=0.5, incrementByFac = 2)

  package$OPERATING_COST_INDICATOR = op
  package$EMISSIONS_PRICE_TON=em
  b = GSC::compute_diss(a$customer.df, a$facility.df)
  reticulate::source_python(paste(system.file(package="GSC"), "partition.py", sep="/"))
  # We need to transpose the matrixes that get transformed into vectors first
  # (not b$d because that's already 1-D)
  # this is because the stores are ordered as
  # 1a 2a 3a 4a
  # 1b 2b 3b 4b
  # 1c 2c 3c 4c
  # so as.vector would produce(1a 1b 1c 2a 2b 2c ...), but we want (1a 2a 3a ...)
  # t = transpose
  # 0 = balanced
  # 1 = feasibility
  # 2 = optimality
  # 3 = best bound
  c = partition(b$c, as.vector(t(b$f)), as.vector(t(b$s)), as.vector(b$d), 3, sol_lim, time_lim, optim_lim, emphasis=emphasis)
  GSC::show_results(a$customer.df, a$facility.df, c$connect, c$open, 3)
  return(list('cust.loc'=a$customer.df, 'fac.loc'=a$facility.df, 'connect'=c$connect, 'open'=c$open, 'cost'=c$cost))
}
