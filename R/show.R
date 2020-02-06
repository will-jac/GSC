show_em = function(stats_obj, d=1, r=1) {
  n = length(stats_obj[[1]]$em_cost)
  s = c()
  for (i in 1:length(stats_obj)) {
    s = append(s, c(
      round(as.double(stats_obj[[i]]$em_cost[1] / d), r),
      round(as.double(stats_obj[[i]]$em_cost[n] / d), r)
    ))
  }
  st = matrix(s, nrow=length(stats_obj), ncol=2, byrow=TRUE)
  rownames(st) = names(stats_obj)
  colnames(st) = c("op_min", "em_min")
  return (as.table(st))
}

show_op = function(stats_obj, d=1, r=1) {
  n = length(stats_obj[[1]]$em_cost)
  s = c()
  for (i in 1:length(stats_obj)) {
    s = append(s, c(
      round(as.double(stats_obj[[i]]$op_cost[1] / d), r),
      round(as.double(stats_obj[[i]]$op_cost[n] / d), r)
    ))
  }
  st = matrix(s, nrow=length(stats_obj), ncol=2, byrow=TRUE)
  rownames(st) = names(stats_obj)
  colnames(st) = c("op_min", "em_min")
  return (as.table(st))
}


show_em_p = function(stats_obj, d=1, r=1) {
  n = length(stats_obj[[1]]$em_cost)
  s = c()
  for (i in 1:length(stats_obj)) {
    s = append(s, c(
      round(as.double(stats_obj[[i]]$em_p[1] / d), r),
      round(as.double(stats_obj[[i]]$em_p[n] / d), r)
    ))
  }
  st = matrix(s, nrow=length(stats_obj), ncol=2, byrow=TRUE)
  rownames(st) = names(stats_obj)
  colnames(st) = c("op_min", "em_min")
  return (as.table(st))
}

show_op_p = function(stats_obj, d=1, r=1) {
  n = length(stats_obj[[1]]$em_cost)
  s = c()
  for (i in 1:length(stats_obj)) {
    s = append(s, c(
      round(as.double(stats_obj[[i]]$op_p[1] / d), r),
      round(as.double(stats_obj[[i]]$op_p[n] / d), r)
    ))
  }
  st = matrix(s, nrow=length(stats_obj), ncol=2, byrow=TRUE)
  rownames(st) = names(stats_obj)
  colnames(st) = c("op_min", "em_min")
  return (as.table(st))
}

show_em_gap = function(stats_obj, d=1, r=1) {
  n = length(stats_obj[[1]]$em_cost)
  s = c()
  for (i in 1:length(stats_obj)) {
    for (j in 2:(n-1)) {
      s = append(s, round(as.double(stats_obj[[i]]$em_gap[j] / d), r))
    }
  }
  st = matrix(s, nrow=length(stats_obj), ncol=(n-2), byrow=TRUE)
  rownames(st) = names(stats_obj)
  return (as.table(st))
}

show_op_gap = function(stats_obj, d=1, r=1) {
  n = length(stats_obj[[1]]$em_cost)
  s = c()
  for (i in 1:length(stats_obj)) {
    for (j in 2:(n-1)) {
      s = append(s, round(as.double(stats_obj[[i]]$op_gap[j] / d), r))
    }
  }
  st = matrix(s, nrow=length(stats_obj), ncol=(n-2), byrow=TRUE)
  rownames(st) = names(stats_obj)
  return (as.table(st))
}


show_em_cost_over_baseline = function(stats_obj, d=1, r=1) {
  n = length(stats_obj[[1]]$em_cost)
  base = stats_obj[[1]]$em_cost[n]
  s = c()
  for (i in 1:length(stats_obj)) {
    a = as.double(stats_obj[[i]]$em_cost[n]) / base
    a = 1 - a
    a = a / d
    s = append(s, c(
      #round(as.double(stats_obj[[i]]$em_cost[1] / (b_e*d)), r),
      round(a, r)
    ))
  }
  st = matrix(s, nrow=length(stats_obj), ncol=1, byrow=TRUE)
  rownames(st) = names(stats_obj)
  #colnames(st) = c("op_min", "em_min")
  return (as.table(st))
}

show_op_cost_over_baseline = function(stats_obj, d=1, r=1) {
  n = length(stats_obj[[1]]$op_cost)
  base = stats_obj[[1]]$op_cost[1]
  s = c()
  for (i in 1:length(stats_obj)) {
    a = as.double(stats_obj[[i]]$op_cost[1]) / base
    a = 1 - a
    a = a / d
    s = append(s, c(
      #round(as.double(stats_obj[[i]]$em_cost[1] / (b_e*d)), r),
      round(a, r)
    ))
  }
  st = matrix(s, nrow=length(stats_obj), ncol=1, byrow=TRUE)
  rownames(st) = names(stats_obj)
  #colnames(st) = c("op_min", "em_min")
  return (as.table(st))
}

