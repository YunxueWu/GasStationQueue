# GasStationQueue

R package for gas station queueing simulation using Monte Carlo methods.

## Installation

```r
devtools::install_github("YunxueWu/GasStationQueue")
```
## Quick Example
```r
library(GasStationQueue) 
results <- simulate_gas_station(
  arrival_rate = 8,    # vehicles/hour (automatically converted to minutes)
  service_rate = 8,    # vehicles/hour/pump (automatically converted)
  n_pumps = 3,         
  sim_time = 480       # simulation time in minutes
)
# Key metrics
get_avg_waiting_time(results)    # Average waiting time (minutes)
get_avg_queue_length(results)    # Average queue length (vehicles)
get_pump_utilization(results)    # Utilization rate (0-1)

# Visualization
plot_simulation(results, "both")

```
## Advanced Analysis
```r
# System performance analysis
performance <- analyze_system_performance(results)

# Access specific metrics
performance$idle_probability     # System idle probability
performance$traffic_intensity    # ρ = λ/(cμ)
performance$system_stable        # Stability check
performance$avg_busy_period      # Average busy period duration
```

## Main Functions
```r

# Core simulation function
results <- simulate_gas_station(
  arrival_rate = 8,    # vehicles/hour (automatically converted to minutes)
  service_rate = 6,    # vehicles/hour/pump (automatically converted)
  n_pumps = 3,         
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