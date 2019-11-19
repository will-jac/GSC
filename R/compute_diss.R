

compute_diss = function(connect.df, facility.df) {

  sizes = get_store_capacities()
  max_store_size = sizes[length(sizes)]

  ## ensure the problem is (probably) solvable
  if (length(facility.df$x) * max_store_size <= sum(connect.df$d)) {
    warn("There may not be enough facilities to address every customer!")
  }
  if (sum(as.vector(connect.df$d) > max_store_size) > 0) {
    warn("There are some customer nodes with more people than the max size of a store!")
  }

  period = 1 # number of periods in a year - cachon defines this to be 1, so we re-use that
  # demand = kg / period, ie how much a customer purchases in one period
  inventory_cost = store_cost(1/period)
  #print(inventory_cost)

  print("starting...")

  facility.df = subset(facility.df, TRUE, select = -c(d))
  facility.mat = as.matrix(facility.df)

  # compute the truck cost, eg [cost of truck * ( dist(facility -> warehouse))]
  w = locate_warehouse(facility.df)

  w = matrix(w, ncol=2)

  truck.cost = apply(facility.mat, 1, function(row) { sqrt(rowSums((row - w)^2)) })

  print("... warehouse located")

  # compute the distance between every customer and every store
  connect.d.df = subset(connect.df, TRUE, select = c(d))
  connect.mat = as.matrix(subset(connect.df, TRUE, select = -c(d)))
  print("...connect matrix initialized")
  #write.csv(connect.mat, file="connect_cost_temp1.csv", row.names = FALSE)

  connect.mat = as.matrix(pdist::pdist(connect.mat, facility.mat))

  print("... diss matrix done")

  # compute the car cost
  car_demand_cost = function(d) {
    return(get_demand() * (car_cost(d)))
  }

  connect.mat[] = vapply(connect.mat, car_demand_cost, numeric(1))

  # three times for the three store sizes
  connect.mat = cbind(connect.mat, connect.mat, connect.mat)

  print("... car cost done")

  # multiply by demand in the area

  connect.mat = sweep(connect.mat, MARGIN=1, as.array(as.matrix(connect.d.df)), FUN="*")

  # compute the truck cost, add it to the car cost
  truck.cost = as.matrix(truck.cost, nrow=1)
  truck.cost = rbind(truck.cost, truck.cost, truck.cost)

  connect.demand = as.matrix(connect.d.df*get_demand() / 20000)
  demand.mat = connect.demand
  for (col in 2:ncol(connect.mat)) {
    demand.mat = cbind(demand.mat, connect.demand)
  }

  #for (col in 1:ncol(connect.mat)) {
  #  for (row in 1:nrow(connect.mat)) {
  #    # divide by the amount a truck can carry
  #    connect.mat[row,col] = connect.mat[row,col] + (get_demand() * connect.d.df[row, 1] / 20000) * truck.cost[col]
  #  }
  #}

  print(dim(connect.mat))
  print(dim(demand.mat))
  connect = sweep(connect.mat, MARGIN=c(1,2), as.array(as.matrix(demand.mat)), FUN="+")

  print("... connect cost done")



  ## compute the facility cost matrix
  ## every facility location gets k possible facilities
  facility.cost = matrix(get_costs_per_size(), ncol = 1)
  for (row in 2:nrow(facility.mat)) {
    facility.cost = cbind(facility.cost, get_costs_per_size())
  }

  ## store capacities
  facility.size = matrix( get_store_capacities(), ncol = 1)
  for (row in 2:nrow(facility.mat)) {
    facility.size = cbind(facility.size, get_store_capacities())
  }

  #write.csv(as.matrix(connect), file="connect_cost.csv", row.names = FALSE)
  #write.csv(as.matrix(facility.cost), file="facility_cost.csv", row.names = FALSE)
  #write.csv(as.matrix(facility.size), file="facility_capacity.csv", row.names = FALSE)
  #write.csv(as.matrix(connect.d.df), file="customer_size.csv", row.names=FALSE)

  return (list('f' = as.matrix(facility.cost), 'c'=as.matrix(connect), 's'=as.matrix(facility.size), 'd' = as.matrix(connect.d.df)))
}


