

#'Returns the demand (kg of products a customer purchases in a period)
#' @keywords demand
#' @return The demand
get_demand = function() {
  return(18)
}
#'Returns the floor sizes of the facilities (ft^2)
#' @keywords floor sizes store
#' @export
#' @examples
#' @return the floor sizes of the facilities (ft^2)
get_floor_sizes = function(){
  return(c(50000, 100000, 200000))
}

#'Returns the capacities of the facilities (number of people)
#' @keywords capactity store
#' @export
#' @examples
#' @return the capacities of the facilities
get_store_capacities = function() {
  return(c(5000, 50000, 500000))
}

#'Returns the costs per size of the facility ($ / ft^2)
#' @keywords cost size store
#' @export
#' @return the cost per size of the facility
get_costs_per_size = function() {
   return(sapply(get_floor_sizes(), FUN=store_cost_foot))
}

# Cost functions


#'calculates the cost for a vehichle. Units: $ / (Kg*Km)
#' @param v nonfuel variable cost / distance
#' @param f amount of fuel / distance
#' @param p_f price of fuel ($ / amount of fuel)
#' @param e amount of emissions / unit of fuel
#' @param p_e price of emissions
#' @param q load carried by the vehicle
#' @keywords vehichle car truck cost
#' @return the vehicle cost
vehicle_cost = function(v, f, p_f, e, p_e, q) {

  return((v+(f*(p_f+e*p_e)))/q)
}

#'calculates the cost for a car, given a distance Units: $ / (Kg*Km)
#' @param d the distance the car travels (in Km)
#' @export
#' @keywords vehichle car cost
#' @return the car cost for d distance
car_cost = function(d) {
  v = 0.0804
  f = 0.111
  p_f = 0.98
  e = 2.325
  q = get_demand()
  return(d*vehicle_cost(v, f, p_f, e, p_e=1, q))
}

#'calculates the cost for a truck, given a distance. Units: $ / (Kg*Km)
#' @param d the distance the truck travels (in Km)
#' @export
#' @keywords vehichle truck cost
#' @return the truck cost for d distance
truck_cost = function(d) {
  v = 0.484
  f = 0.392
  p_f = 1.05
  e = 2.669
  q_t = 20000
  return(d*vehicle_cost(v, f, p_f, e, p_e=1, q_t))
}

#'calculates the space cost for a facility. Units: $
#' @param v variable space cost (eg rent) / time
#' @param f amount of energy / time
#' @param p_f price of energy ($ / unit of energy)
#' @param e amount of emissions / unit of energy
#' @param p_e price of emissions
#' @keywords store cost
#' @return the truck cost for d distance
space_cost = function(v, f, p_f, e, p_e, q) {
  return(v+(f*(p_f+e*p_e)) / q)
}

# $ / kg
store_cost = function(t) {
  v = 212.8
  fe  = 126.1
  fp = 22.9
  q = 141
  return(t*space_cost(v, f=1, fp, fe, p_e=1, q))
}

#'calculates the space cost for a facility. Units: $ / sqft
#' @keywords store space cost
#' @export
#' @return the truck cost for d distance
store_cost_foot = function(sq_ft) {
  ## TODO: verify all of these
  v = 212.8
  fe  = 126.1
  fp = 22.9
  q = 141
  t = 1 # period
  return(t*sq_ft*space_cost(v, f=1, fp, fe, p_e=1, q))
}

#'calculates the cost per kg / period
#' @keywords store space cost
#' @param d the distance to the store
#' @param t the unit of time (number of periods)
#' @return the truck cost for d distance
cost_per_kg_per_period = function(d,t=1) {
  return(car_cost(d) + truck_cost(d) + store_cost(t))
}

# $ / km
# store_open_cost = function(d, q) {
  # d = distance from store to warehouse
  # q = kg needed / period
#  q_t = 30 #TODO: max carrying cap of truck
#  period = 1
#  q*(1/q_t*truck_cost(d, q_t) + store_cost(period))
#}

#'Centrally locates a warehouse across a region defined by the data frame x
#'@param x the data frame containing locations to locate the warehouse across
#'@export
#'@returnt the location of the warehouse
locate_warehouse = function(x) {
  loca = orloca::as.loca.p(x)
  w = orloca::distsummin(loca)
  return(w)
}

