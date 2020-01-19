stats = function(gsc_obj, num_sizes = 3) {
  stats = list("connect" = gsc_obj$cost, "stores" = store_stats(gsc_obj), "customers" = nrow(gsc_obj$connect))
}

customer_stats = function(gsc_obj, k) {

}

store_stats = function(gsc_obj, k) {
  x = length(gsc_obj$open) / k
  z = sum(gsc_obj$open[1:(2*x)])
  y = c(z)
  stats = list("length" = sum(gsc_obj$open))
  stats = append(stats, z)
  for (i in 1:k) {
    z = sum(gsc_obj$open[((k-1)*x):(k*x)])
    y = append(y, z)
    stats = append(stats, z)
  }
  # this comes from compute_diss
  w = matrix(locate_warehouse(facility.df), ncol=2)
  # euclidean distance
  # can also use the manhattan distance
  truck.dist = apply(facility.mat, 1, function(row) { sqrt(rowSums((row - w)^2)) })
  truck.dist =
  stats = append(stats, "avg_size" = sum(y*c(1:k) / sum(y)))
  return(stats)
}

num_stores = function(gsc_batch_obj) {
  return(list(
    "emissions" = num_stores_indiv(gsc_batch_obj$emissions),
    "hybrid"    = num_stores_indiv(gsc_batch_obj$hybrid),
    "operating" = num_stores_indiv(gsc_batch_obj$operating)
  ))
}

num_stores_indiv = function(gsc_obj) {
  return(sum(gsc_obj$open))
}

operating_cost_penalty = function(gsc_batch_obj) {
  op = GSC::operating_cost(gsc_batch_obj)
  return(op$emissions / op$operating)
}

emissions_cost_penalty = function(gsc_batch_obj) {
  em = GSC::emissions_cost(gsc_batch_obj)
  return(em$operating / em$emissions)
}

hybrid_penalty = function(gsc_batch_obj) {
  return(hybrid_emissions_penalty(gsc_batch_obj) + hybrdi_operating_penalty(gsc_batch_obj))
}

hybrid_emissions_penalty = function(gsc_batch_obj) {
  em = GSC::emissions_cost(gsc_batch_obj)
  return(em$hybrid / em$emissions)
}

hybrid_operating_penalty = function(gsc_batch_obj) {
  op = GSC::operating_cost(gsc_batch_obj)
  return(op$hybrid / op$operating)
}


operating_cost = function(gsc_batch_obj) {
  return(list(
    "emissions" = type_cost(gsc_batch_obj$operating, gsc_batch_obj$emissions),
    "hybrid"    = type_cost(gsc_batch_obj$operating, gsc_batch_obj$hybrid),
    "operating" = type_cost(gsc_batch_obj$operating, gsc_batch_obj$operating)
  ))
}

emissions_cost = function(gsc_batch_obj) {
  return(list(
    "emissions" = type_cost(gsc_batch_obj$emissions, gsc_batch_obj$emissions),
    "hybrid"    = type_cost(gsc_batch_obj$emissions, gsc_batch_obj$hybrid),
    "operating" = type_cost(gsc_batch_obj$emissions, gsc_batch_obj$operating)
  ))
}

type_cost = function(type_obj, gsc_obj) {
  return (type_cost_customer(type_obj, gsc_obj) + type_cost_facility(type_obj, gsc_obj))
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

