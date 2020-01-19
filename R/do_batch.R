do_batch = function(filename=GSC::filename, c = 200, f = 400, em = 100, time_lim=60*60, optim_lim=0.01, emphasis=3, sol_lim=10) {
  a = load_data(filename, f, c)
  write.csv(as.matrix(a$customer.df), file="customer_data.csv", row.names=FALSE)
  write.csv(as.matrix(a$facility.df), file="facility_data.csv", row.names=FALSE)

  package$OPERATING_COST_INDICATOR=1
  package$EMISSIONS_PRICE_TON=0
  b1 = compute_diss(a$customer.df, a$facility.df)
  write.csv(as.matrix(b1$c), file="b1-c.csv", row.names=FALSE)
  write.csv(as.matrix(b1$f), file="b1-f.csv", row.names=FALSE)
  write.csv(as.matrix(b1$s), file="b1-s.csv", row.names=FALSE)
  write.csv(as.matrix(b1$d), file="b1-d.csv", row.names=FALSE)

  package$OPERATING_COST_INDICATOR=1
  package$EMISSIONS_PRICE_TON=em
  b2 = compute_diss(a$customer.df, a$facility.df)
  write.csv(as.matrix(b2$c), file="b2-c.csv", row.names=FALSE)
  write.csv(as.matrix(b2$f), file="b2-f.csv", row.names=FALSE)
  write.csv(as.matrix(b2$s), file="b2-s.csv", row.names=FALSE)
  write.csv(as.matrix(b2$d), file="b2-d.csv", row.names=FALSE)

  package$OPERATING_COST_INDICATOR=0
  package$EMISSIONS_PRICE_TON=em
  b3 = GSC::compute_diss(a$customer.df, a$facility.df)
  write.csv(as.matrix(b3$c), file="b3-c.csv", row.names=FALSE)
  write.csv(as.matrix(b3$f), file="b3-f.csv", row.names=FALSE)
  write.csv(as.matrix(b3$s), file="b3-s.csv", row.names=FALSE)
  write.csv(as.matrix(b3$d), file="b3-d.csv", row.names=FALSE)

  reticulate::source_python(paste(system.file(package="GSC"), "partition.py", sep="/"))

  c1 = partition(b1$c, as.vector(t(b1$f)), as.vector(t(b1$s)), as.vector(b1$d), 3, sol_lim, time_lim, optim_lim, emphasis)
  write.csv(as.matrix(c1$open), file="c1-open.csv", row.names=FALSE)
  write.csv(as.matrix(c1$connect), file="c1-connect.csv", row.names=FALSE)
  write.csv(as.matrix(c1$cost), file="c1-cost.csv", row.names=FALSE)

  c2 = partition(b2$c, as.vector(t(b2$f)), as.vector(t(b2$s)), as.vector(b2$d), 3, sol_lim, time_lim, optim_lim, emphasis)
  write.csv(as.matrix(c2$open), file="c2-open.csv", row.names=FALSE)
  write.csv(as.matrix(c2$connect), file="c2-connect.csv", row.names=FALSE)
  write.csv(as.matrix(c2$cost), file="c2-cost.csv", row.names=FALSE)

  c3 = partition(b3$c, as.vector(t(b3$f)), as.vector(t(b3$s)), as.vector(b3$d), 3, sol_lim, time_lim, optim_lim, emphasis)
  write.csv(as.matrix(c3$open), file="c3-open.csv", row.names=FALSE)
  write.csv(as.matrix(c3$connect), file="c3-connect.csv", row.names=FALSE)
  write.csv(as.matrix(c3$cost), file="c3-cost.csv", row.names=FALSE)

  return (
    list(
      "operating"=list('cust.loc'=a$customer.df, 'fac.loc'=a$facility.df, 'connect' = c1$connect, 'open' = c1$open, 'cost'=c1$cost, 'cust.cost' = b1$c, 'fac.cost' = b1$f),
      "hybrid"   =list('cust.loc'=a$customer.df, 'fac.loc'=a$facility.df, 'connect' = c2$connect, 'open' = c2$open, 'cost'=c2$cost, 'cust.cost' = b2$c, 'fac.cost' = b2$f),
      "emissions"=list('cust.loc'=a$customer.df, 'fac.loc'=a$facility.df, 'connect' = c3$connect, 'open' = c3$open, 'cost'=c3$cost, 'cust.cost' = b3$c, 'fac.cost' = b3$f)
    )
  )
}
