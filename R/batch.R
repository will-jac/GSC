batch = function() {
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

em_batch = function(c = 200, f = 600, em_seq = seq(10, 300, 10)) {
  n = length(em_seq)
  result = vector(mode='list', length=n)
  for (i in 1:n) {
    r = GSC::do_batch(c=c, f=f, em = em_seq[i])
    result[[i]] = r
  }
  return(result)
}

sweep_batch = function() {

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
