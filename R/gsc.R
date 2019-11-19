gsc = function(filename, reduce_by_facility=1000, reduce_by_customer=1000) {
  a = load_data(filename, reduce_by_facility, reduce_by_customer)
  b = compute_diss(a$data.df, a$reduced.df)
  reticulate::source_python(paste(system.file(package="GSC"), "partition.py", sep="/"))
  # We need to transpose the matrixes that get transformed into vectors first
  # (not b$d because that's already 1-D)
  # this is because the stores are ordered as
  # 1a 2a 3a 4a
  # 1b 2b 3b 4b
  # 1c 2c 3c 4c
  # so as.vector would produce(1a 1b 1c 2a 2b 2c ...), but we want (1a 2a 3a ...)
  # t = transpose
  c = partition(b$c, as.vector(t(b$f)), as.vector(t(b$s)), as.vector(b$d), 3)
  show_results(a$data.df, a$reduced.df, c$connect, c$open, 3)
  return(list('cust.loc'=a$data.df, 'fac.loc'=a$reduced.df, 'connect'=c$connect, 'open'=c$open, 'cost'=c$cost))
}
