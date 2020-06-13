
compute_diss = function(connect.df, facility.df, write_data = FALSE) {

  print(package$OPERATING_COST_INDICATOR)
  print(package$EMISSIONS_PRICE_TON)

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

  # euclidean distance
  # can also use the manhattan distance
  truck.dist = apply(facility.mat, 1, function(row) { sqrt(rowSums((row - w)^2)) })
  truck.cost = apply(as.matrix(truck.dist), MARGIN = c(1,2), FUN=GSC::truck_cost)

  print("... warehouse located")

  # compute the distance between every customer and every store
  connect.d.df = subset(connect.df, TRUE, select = c(d))
  connect.mat = as.matrix(subset(connect.df, TRUE, select = -c(d)))
  print("...connect matrix initialized")
  write.csv(connect.mat, file="car_distance_matrix.csv", row.names = FALSE)

  connect.mat = as.matrix(pdist::pdist(connect.mat, facility.mat))

  print("... diss matrix done")

  # compute the car cost
  car_demand_cost = function(d) {
    return(GSC::get_demand() * GSC::car_cost(d))
  }

  connect.mat[] = vapply(connect.mat, car_demand_cost, numeric(1))
  print("... car cost done")

  # compute the partial truck cost and add it to the car cost
  cust_amt = connect.d.df * GSC::get_demand() / GSC::truck_capacity()
  truck = truck.cost * cust_amt[1, 1]
  for (i in 2:nrow(cust_amt)) {
    truck = cbind(truck, truck.cost * cust_amt[i, 1])
  }
  connect.mat = connect.mat + t(truck)

  # multiply by number of people at a location
  # connect = num_people * (car_cost_per_person + truck_cost_per_person)
  connect.mat = sweep(connect.mat, MARGIN=1, as.array(as.matrix(connect.d.df)), FUN="*")

  # three times for the three store sizes
  connect = cbind(connect.mat, connect.mat, connect.mat)



  # multiply by demand in the area


  # compute the truck cost, add it to the car cost
  # truck.cost.row = as.matrix(truck.cost, nrow=1)
  # truck.cost = truck.cost.row
  # for (col in 2:num_sizes()) {
  #   truck.cost = rbind(truck.cost, truck.cost.row)
  #}

  #connect.demand = as.matrix(connect.d.df*get_demand() / truck_capacity()) # divide by the carrying capacity of a truck
  #demand.mat = connect.demand * truck.cost
  #for (col in 2:ncol(connect.mat)) {
  #  demand.mat = cbind(demand.mat, connect.demand)
  #}

  # add the connect cost (customer cost) to the partial truck cost
  #connect = sweep(connect.mat, MARGIN=c(1,2), as.array(as.matrix(demand.mat)), FUN="+")

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

  if (write_data) {
  write.csv(as.matrix(connect), file="connect_cost.csv", row.names = FALSE)
  write.csv(as.matrix(facility.cost), file="facility_cost.csv", row.names = FALSE)
  write.csv(as.matrix(facility.size), file="facility_capacity.csv", row.names = FALSE)
  write.csv(as.matrix(connect.d.df), file="customer_size.csv", row.names=FALSE)
  }
  return (list('f' = as.matrix(facility.cost), 'c'=as.matrix(connect), 's'=as.matrix(facility.size), 'd' = as.matrix(connect.d.df)))
}


