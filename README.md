# GasStationQueue

R package for gas station queueing simulation using Monte Carlo methods.

## Installation

```r
devtools::install_github("YunxueWu/GasStationQueue")
```
## Quick Example
```r
library(GasStationQueue)
# Run simulation
results <- simulate_gas_station(
  arrival_rate = 8,
  service_rate = 8, 
  n_pumps = 3,
  sim_time = 480
)
# Key metrics
get_avg_waiting_time(results)    # Returns average waiting time
get_avg_queue_length(results)    # Returns average queue length
get_pump_utilization(results)    # Returns utilization rate (0-1)

# Visualization
plot_simulation(results, "both")

```
## Main Functions
```r

# Core simulation function
results <- simulate_gas_station(
  arrival_rate = 8,    # vehicles per hour
  service_rate = 6,    # vehicles per hour per pump
  n_pumps = 3,         # number of pumps
  sim_time = 480       # simulation time in minutes
)

# Performance metrics
avg_wait <- get_avg_waiting_time(results)     # Average waiting time
avg_queue <- get_avg_queue_length(results)    # Average queue length  
utilization <- get_pump_utilization(results)  # Pump utilization rate

# Visualization
plot_simulation(results, type = "both")      # Combined plots
plot_simulation(results, type = "queue")     # Queue length only
plot_simulation(results, type = "utilization") # Utilization only
```

## Features

```r
# Discrete-event simulation
results <- simulate_gas_station(
  arrival_rate = 12,
  service_rate = 6,
  n_pumps = 3,
  sim_time = 480
)

# Scenario analysis
scenarios <- list(
  low = list(arrival_rate = 4, service_rate = 8, n_pumps = 2, sim_time = 480),
  medium = list(arrival_rate = 12, service_rate = 8, n_pumps = 3, sim_time = 480), 
  high = list(arrival_rate = 20, service_rate = 8, n_pumps = 4, sim_time = 480)
)

# Input validation
simulate_gas_station(arrival_rate = -1)   # Throws error
simulate_gas_station(n_pumps = 2.5)       # Throws error
```



For detailed examples: `vignette("gas_station_simulation")`
