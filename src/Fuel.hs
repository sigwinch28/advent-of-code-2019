module Fuel (fuelForMass, totalFuel) where

fuelForMass mass = (mass `div` 3) - 2

totalFuel mass = totalFuel' mass 0

totalFuel' mass acc =
  let fuel = fuelForMass mass in
    if fuel > 0 then
      totalFuel' fuel (acc + fuel)
    else
      acc
