gsc = function(filename=GSC::filename, c = 30, f = 15, op = 1, em = 1, time_lim=60*60, optim_lim=0.01, emphasis=3, sol_lim=10) {
  a = GSC::load_data(filename, c, f)
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
  c = GSC::partition(b$c, as.vector(t(b$f)), as.vector(t(b$s)), as.vector(b$d), 3, sol_lim, time_lim, optim_lim, emphasis)
  GSC::show_results(a$customer.df, a$facility.df, c$connect, c$open, 3)
  return(list('cust.loc'=a$customer.df, 'fac.loc'=a$facility.df, 'connect'=c$connect, 'open'=c$open, 'cost'=c$cost, 'cust.cost' = b$c, 'fac.cost' = b$f))
}


gsc_transform_data = function(filename=GSC::filename,
               c = 100, f = 200, em = 100,
               emissions = TRUE, operating = TRUE, ...)
{
  a = GSC::load_data(filename, c, f, ...)
  b = GSC::compute_diss(a$customer.df, a$facility.df, TRUE)
  print("done")
}
