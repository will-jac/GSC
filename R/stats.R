

batch_stats = function(results_list) {
  return (list(
    stores = GSC::num_stores(results_list),
    em_cost = GSC::emissions_cost(results_list),
    op_cost = GSC::operating_cost(results_list)
  ))
}

num_stores = function(gsc_batch_obj) {
  nums = c()
  for (i in 1:length(gsc_batch_obj)) {
    nums[i]  = sum(gsc_batch_obj[[i]]$open)
  }
  return(nums)
}

emissions_gap_reduction = function(em_cost) {
  n = length(em_cost)
  gap_denom = em_cost[1] - em_cost[n]
  gap = c()
  for (i in 1:n) {
    gap[i] = (em_cost[1] - em_cost[i]) / gap_denom
  }
  return(gap)
}

operating_cost_penalty = function(gsc_batch_obj) {
  op = GSC::operating_cost(gsc_batch_obj)

  return(op / op[1])
}

emissions_cost_penalty = function(gsc_batch_obj) {
  em = GSC::emissions_cost(gsc_batch_obj)
  n = length(gsc_batch_obj)

  return(em / em[n])
}

operating_cost = function(gsc_batch_obj) {
  return(type_cost(gsc_batch_obj[[1]], gsc_batch_obj))
}

emissions_cost = function(gsc_batch_obj) {
  n = length(gsc_batch_obj)
  return(type_cost(gsc_batch_obj[[n]], gsc_batch_obj))
}

type_cost = function(type_obj, gsc_batch_obj) {
  n = length(gsc_batch_obj)
  type_cost = vector(length=n)
  for (i in 1:length(gsc_batch_obj)) {
    type_cost[i] = type_cost_customer(type_obj, gsc_batch_obj[[i]]) + type_cost_facility(type_obj, gsc_batch_obj[[i]])
  }
  return(type_cost);
}

type_cost_customer = function(type_obj, gsc_obj) {

  cust.cost = type_obj$cust.cost
  connect = gsc_obj$connect
  cost = 0

  for (i in 1:nrow(connect)) {
    for (j in 1:ncol(connect)) {
      if (connect[i,j]) {
        cost = cost + bit64::as.integer64(cust.cost[i,j])
        break
      }
    }
  }
  return(cost)
}

type_cost_facility = function(type_obj, gsc_obj) {

  fac.cost = type_obj$fac.cost
  open = gsc_obj$open
  cost = 0

  for (i in 1:length(open)) {
    if (open[i]) {
      cost = cost + bit64::as.integer64(fac.cost[i])
    }
  }

  return(cost)
}

