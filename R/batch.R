batch = function() {
r1 = GSC::do_batch(c=100, f=200)
r2 = GSC::do_batch(c=100, f=200, em=200)
r3 = GSC::do_batch(c=100, f=200, em=500)
r4 = GSC::do_batch(c=50, f=100)
r5 = GSC::do_batch(c=50, f=100, em=200)
r6 = GSC::do_batch(c=50, f=100, em=500)
return(c(r1, r2, r3, r4, r5, r6))
}
