run_simulations = function() {
  library(GSC)
  to_return = list()
  emissions = batch()
  car = vehicle_batch(car_coef = 1/2)
  truck = vehicle_batch(truck_coef = 1/2)
  car_truck = vehicle_batch(truck_coef = 1/2, car_coef = 1/2)
  fuel = vehicle_batch(car_fuel_coef = 2, truck_fuel_coef = 2)
  return(list(
    "emissions" = emissions,
    "car" = car,
    "truck" = truck,
    "car_truck" = car_truck,
    "fuel" = fuel
  ))
}
