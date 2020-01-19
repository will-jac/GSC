package <- new.env(parent=emptyenv())

# $ / metric ton CO2
package$EMISSIONS_PRICE_TON <- 500
package$OPERATING_COST_INDICATOR <- 1
package$DEMAND <- 18

#'Returns the demand (kg of products a customer purchases in a period)
#' @keywords demand
#' @return The demand
get_demand = function() {
  return(package$DEMAND)
}
#'Returns the floor sizes of the facilities (m^2)
#' @keywords floor sizes store
#' @export
#' @examples
#' @return the floor sizes of the facilities (m^2)
get_floor_sizes = function(){
  # supercenter  = 16500 sq meters, 120000 'items'
  # regular      = 9800 sq meters
  # neighborhood = 3900 sq meters
  return (c(3900, 9800, 16500))
  #return(c(50000, 100000, 200000))
}

#'Returns the capacities of the facilities (number of people)
#' @keywords capactity store
#' @export
#' @examples
#' @return the capacities of the facilities
get_store_capacities = function() {
  # supercenter  = 100,000
  # regular      = 30,000
  # neighborhood = 5,000
  return(c(5000, 50000, 300000))

  #return(c(5000, 50000, 500000))
}

#'Returns the costs per size of the facility ($ / ft^2)
#' @keywords cost size store
#' @export
#' @return the cost per size of the facility
get_costs_per_size = function() {
   return(sapply(get_floor_sizes(), FUN=GSC::store_cost_meter))
}

num_sizes = function() {
  return(3)
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

package$car_coef = 1
package$car_v = 0.0804
package$car_f = 0.111
package$car_p_f = 0.98
package$car_e = 2.325
package$car_q = package$DEMAND

#'calculates the cost for a car, given a distance Units: $ / (Kg*Km)
#' @param d the distance the car travels (in Km)
#' @export
#' @keywords vehichle car cost
#' @return the car cost for d distance
car_cost = function(d) {
  return(car_coef*2*d*vehicle_cost(
    package$OPERATING_COST_INDICATOR * package$car_v,
    package$car_f,
    package$OPERATING_COST_INDICATOR * package$car_p_f,
    package$car_e,
    p_e=package$EMISSIONS_PRICE_TON / 1000,
    package$car_q))
}

package$truck_coef = 1
package$truck_v = 0.484
package$truck_f = 0.392
package$truck_p_f = 1.05
package$truck_e = 2.669
package$truck_q_t = 20000

#'calculates the cost for a truck, given a distance. Units: $ / (Kg*Km)
#' @param d the distance the truck travels (in Km)
#' @export
#' @keywords vehichle truck cost
#' @return the truck cost for d distance
truck_cost = function(d) {
  return(truck_coef*2*d*vehicle_cost(
    package$OPERATING_COST_INDICATOR* package$truck_v,
    package$truck_f,
    package$OPERATING_COST_INDICATOR * package$truck_p_f,
    package$truck_e,
    p_e=package$EMISSIONS_PRICE_TON / 1000,
    package$truck_q_t))
}

truck_capacity = function() {
  return(package$truck_q_t)
}

#'calculates the space cost for a facility. Units: $ / kg
#' @param v variable space cost (eg rent) / time
#' @param f amount of energy / time
#' @param p_f price of energy ($ / unit of energy)
#' @param e amount of emissions / unit of energy
#' @param p_e price of emissions
#' @keywords store cost
#' @return the space cost
space_cost = function(v, f, p_f, e, p_e, q) {
  return(v+(f*(p_f+e*p_e)) / q)
}

package$store_v = 212.8
package$store_f = 1      #this is already multiplied into e and p_f
package$store_p_f = 22.9
package$store_e = 126.1
package$store_q = 141

period = 52

# $ / kg
store_cost = function(t) {
  return(period*space_cost(
    package$OPERATING_COST_INDICATOR * package$store_v,
    package$store_f,
    package$OPERATING_COST_INDICATOR * package$store_p_f,
    package$store_e,
    p_e=package$EMISSIONS_PRICE_TON / 1000,
    package$store_q))
}

#'calculates the space cost for a facility. Units: $
#' @keywords store space cost
#' @export
#' @return the space cost per sq meters
store_cost_meter = function(sq_meter) {
  return(period * sq_meter * space_cost(
    package$OPERATING_COST_INDICATOR * package$store_v,
    package$store_f,
    package$OPERATING_COST_INDICATOR * package$store_p_f,
    package$store_e,
    p_e=package$EMISSIONS_PRICE_TON / 1000,
    package$store_q))
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

