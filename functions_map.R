#install.packages('FinTS')
library(FinTS)

future_pop <- function(country, year) {
  
  if (year <= 2021)
    "The value of year inputted must be greater than current year!"
  else if (country %in% world$name_long == F)
    "The name of the country not found!"
  else {
  pop = world$pop[world$name_long == country][1]
  pop_growth = world$pop_growth[world$name_long == country][1]
  final_pop = pop * compoundInterest((pop_growth/100), (year-2021))
  cat(country, "would have", final_pop, "people if it's going to grow at the current rate of", round(pop_growth, 2), "% per year")
  }
}



