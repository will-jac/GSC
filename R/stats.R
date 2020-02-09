
plot_em_gap = function(gsc_stats_obj) {
  library(ggplot2)
x = c(seq(0,3100,100))
gsc_stats_obj = gap_stats
ggplot() +
  labs(y="Emissions Gap Reduction %", x="$ / metric ton CO^2", title="Emissions Gap Reduction") +
  geom_line(aes(x,gsc_stats_obj$emissions$em_gap, color="baseline")) +
  geom_line(aes(x,gsc_stats_obj$car      $em_gap,       color="car")) +
  geom_line(aes(x,gsc_stats_obj$truck    $em_gap, color="truck")) +
  geom_line(aes(x,gsc_stats_obj$car_truck$em_gap, color="car_truck")) +
  geom_line(aes(x,gsc_stats_obj$fuel     $em_gap, color="fuel"))
}


batch_stats = function(results_obj) {
  stats_list = c()

  for (i in 1:length(results_obj)) {
    s = stats(results_obj[[i]])
    #stats_list = append(stats_list, s)
    stats_list[[i]] = s
    names(stats_list[i]) = names(results_obj[i])
  }
  #stats = matrix(stats_list, nrow=length(results_obj), ncol=length(s), byrow=TRUE)
  #rownames(stats) = names(results_obj)
  #colnames(stats) = names(s)
  names(stats_list) = names(results_obj)
  return(stats_list)
}

stats = function(results_list) {
  e = GSC::emissions_cost(results_list)
  o = GSC::operating_cost(results_list)
  return (data.frame(
    stores  =
      GSC::num_stores(results_list),
    store_cap =
      store_cap_list(results_list),
    em_cost =
      e,
    em_gap  =
      GSC::emissions_gap_reduction(e),
    em_p    =
      GSC::emissions_cost_penalty(e),
    op_cost =
      o,
    op_gap  =
      GSC::operating_gap_reduction(o),
    op_p    =
      GSC::operating_cost_penalty(o)
  ))
}

store_cap_list = function(results_list) {
  ret = c()
  for (i in 1:length(results_list)) {
    ret = append(ret, store_cap(results_list[[i]]))
  }
  return(ret)
}


store_cap = function(results_obj, n=3) {
  open = results_obj$open
  connect = results_obj$connect

  cost = 0
  mod = length(open / n)
  cap = GSC::get_store_capacities()

  num.con = 0
  num.cap = 0

  for (j in 1:length(open)) {
    if (open[j] > 0.9) {
      k = ceiling(j / mod)
      num.cap = num.cap + cap[k]
      num.con = num.con + sum(connect[,j])
    }
  }
  return(100 * (num.con / num.cap))
}


operating_gap_reduction = function(op_cost) {
  n = length(op_cost)
  gap_denom = op_cost[n] - op_cost[1]
  return((op_cost[n] - op_cost) / gap_denom)
}

emissions_gap_reduction = function(em_cost) {
  n = length(em_cost)
  gap_denom = em_cost[1] - em_cost[n]
  return((em_cost[1] - em_cost) / gap_denom)
}

num_stores = function(gsc_batch_obj) {
  nums = c()
  for (i in 1:length(gsc_batch_obj)) {
    nums[i]  = sum(gsc_batch_obj[[i]]$open)
  }
  return(nums)
}

operating_cost_penalty = function(op_cost) {
  return((op_cost - op_cost[1]) / op_cost[1])
}

emissions_cost_penalty = function(em_cost) {
  n = length(em_cost)
  return((em_cost - em_cost[n]) / em_cost[n])
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
  type_cost = c()
  for (i in 1:n) {
    type_cost = append(type_cost,
                       type_cost_customer(type_obj, gsc_batch_obj[[i]])
                       + type_cost_facility(type_obj, gsc_batch_obj[[i]]))
  }
  return(type_cost);
}

type_cost_customer = function(type_obj, gsc_obj) {

  cust.cost = type_obj$cust.cost
  connect = gsc_obj$connect
  cost = 0

  for (i in 1:nrow(connect)) {
    for (j in 1:ncol(connect)) {
      if (connect[i,j] > 0.9) {
        cost = cost + as.double(cust.cost[i,j])
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
    if (open[i] > 0.9) {
      cost = cost + as.double(fac.cost[i])
    }
  }

  return(cost)
}

