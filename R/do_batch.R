do_batch = function() {
  facility_factor = 30
  customer_factor = 30
  a = load_data(filename, facility_factor, customer_factor)
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
  package$EMISSIONS_PRICE_TON=100
  b2 = compute_diss(a$customer.df, a$facility.df)
  write.csv(as.matrix(b2$c), file="b2-c.csv", row.names=FALSE)
  write.csv(as.matrix(b2$f), file="b2-f.csv", row.names=FALSE)
  write.csv(as.matrix(b2$s), file="b2-s.csv", row.names=FALSE)
  write.csv(as.matrix(b2$d), file="b2-d.csv", row.names=FALSE)

  package$OPERATING_COST_INDICATOR=0
  package$EMISSIONS_PRICE_TON=100
  b3 = GSC::compute_diss(a$customer.df, a$facility.df)
  write.csv(as.matrix(b3$c), file="b3-c.csv", row.names=FALSE)
  write.csv(as.matrix(b3$f), file="b3-f.csv", row.names=FALSE)
  write.csv(as.matrix(b3$s), file="b3-s.csv", row.names=FALSE)
  write.csv(as.matrix(b3$d), file="b3-d.csv", row.names=FALSE)

  reticulate::source_python(paste(system.file(package="GSC"), "partition.py", sep="/"))

  sol_lim=10
  iter_lim=100000
  optim_lim=0.01

  c1 = partition(b1$c, as.vector(t(b1$f)), as.vector(t(b1$s)), as.vector(b1$d), 3, sol_lim, iter_lim, optim_lim)
  write.csv(as.matrix(c1$open), file="c1-open.csv", row.names=FALSE)
  write.csv(as.matrix(c1$connect), file="c1-connect.csv", row.names=FALSE)
  write.csv(as.matrix(c1$cost), file="c1-cost.csv", row.names=FALSE)

  c2 = partition(b2$c, as.vector(t(b2$f)), as.vector(t(b2$s)), as.vector(b2$d), 3, sol_lim, iter_lim, optim_lim)
  write.csv(as.matrix(c2$open), file="c2-open.csv", row.names=FALSE)
  write.csv(as.matrix(c2$connect), file="c2-connect.csv", row.names=FALSE)
  write.csv(as.matrix(c2$cost), file="c2-cost.csv", row.names=FALSE)

  c3 = partition(b3$c, as.vector(t(b3$f)), as.vector(t(b3$s)), as.vector(b3$d), 3, sol_lim, iter_lim, optim_lim)
  write.csv(as.matrix(c3$open), file="c3-open.csv", row.names=FALSE)
  write.csv(as.matrix(c3$connect), file="c3-connect.csv", row.names=FALSE)
  write.csv(as.matrix(c3$cost), file="c3-cost.csv", row.names=FALSE)


  return (list("operating"=list('a'=a,'b'=b1,'c'=c1),"hybrid"=list('a'=a,'b'=b2,'c'=c2),"emissions"=list('a'=a,'b'=b3,'c'=c3)))
}
