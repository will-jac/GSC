do_uniform_batch = function(x, y, time) {
  png("hybrid-uniform.png", bg="transparent")
  GSC::test_uniform(x, y, time_lim=time, emphasis=1)
  dev.off()
  png("emissions-uniform.png", bg="transparent")
  GSC::test_uniform(x, y, em=1, op=0, time_lim=time, emphasis=1)
  dev.off()
  png("operating-uniform.png", bg="transparent")
  GSC::test_uniform(x, y, em=0, op=1, time_lim=time, emphasis=1)
  dev.off()
}
