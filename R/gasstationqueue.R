# GasStationQueue: Gas Station Queuing Simulation Using Monte Carlo Methods

# Core simulation function
#' Simulate gas station queueing system
#'
#' @param arrival_rate Arrival rate in vehicles per hour
#' @param service_rate Service rate in vehicles per hour per pump
#' @param n_pumps Number of gas pumps
#' @param sim_time Simulation time in minutes
#' @param random_seed Random seed for reproducibility
#' @return A list with simulation results
#' @export
simulate_gas_station <- function(arrival_rate = 2, service_rate = 1,
                                 n_pumps = 3, sim_time = 480, random_seed = NULL) {
  
  if (arrival_rate < 0) stop("arrival_rate must be non-negative")
  if (service_rate <= 0) stop("service_rate must be positive")
  if (n_pumps <= 0) stop("n_pumps must be positive")
  if (n_pumps != round(n_pumps)) stop("n_pumps must be an integer")
  if (sim_time <= 0) stop("sim_time must be positive")
  
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }
  
  current_time <- 0
  queue <- numeric(0)
  pumps <- rep(0, n_pumps)
  waiting_times <- numeric(0)
  queue_length_history <- numeric(0)
  pump_utilization_history <- numeric(0)
  
  if (arrival_rate == 0) {
    next_arrival <- Inf  
  } else {
    next_arrival <- rexp(1, rate = arrival_rate / 60)
  }
  
  while (current_time < sim_time) {
    if (current_time >= next_arrival && current_time < sim_time) {
      queue <- c(queue, current_time)
      if (arrival_rate > 0) {
        next_arrival <- current_time + rexp(1, rate = arrival_rate / 60)
      } else {
        next_arrival <- Inf
      }
    }
    
    for (i in 1:n_pumps) {
      if (pumps[i] <= current_time && length(queue) > 0) {
        arrival_time <- queue[1]
        queue <- queue[-1]
        service_time <- rexp(1, rate = service_rate / 60)
        pumps[i] <- current_time + service_time
        wait_time <- current_time - arrival_time
        waiting_times <- c(waiting_times, wait_time)
      }
    }
    
    queue_length_history <- c(queue_length_history, length(queue))
    busy_pumps <- sum(pumps > current_time)
    pump_utilization_history <- c(pump_utilization_history, busy_pumps / n_pumps)
    next_events <- c(next_arrival, pumps[pumps > current_time])
    next_event <- min(next_events, na.rm = TRUE)
    
    if (is.infinite(next_event) || next_event > sim_time) {
      break
    }
    current_time <- next_event
  }
  
  results <- list(
    waiting_times = waiting_times,
    queue_lengths = queue_length_history,
    pump_utilization = pump_utilization_history,
    total_customers = length(waiting_times),
    simulation_time = sim_time,
    parameters = list(
      arrival_rate = arrival_rate,
      service_rate = service_rate,
      n_pumps = n_pumps
    )
  )
  
  class(results) <- "gas_station_simulation"
  return(results)
}
#' Calculate average waiting time
#'
#' @param simulation_results Results from simulate_gas_station function
#' @return Average waiting time in minutes
#' @export
get_avg_waiting_time <- function(simulation_results) {
  if (length(simulation_results$waiting_times) == 0) return(0)
  return(mean(simulation_results$waiting_times))
}
#' Calculate average queue length
#'
#' @param simulation_results Results from simulate_gas_station function  
#' @return Average queue length
#' @export
get_avg_queue_length <- function(simulation_results) {
  return(mean(simulation_results$queue_lengths))
}
#' Calculate pump utilization rate
#'
#' @param simulation_results Results from simulate_gas_station function
#' @return Pump utilization rate (0 to 1)
#' @export
get_pump_utilization <- function(simulation_results) {
  return(mean(simulation_results$pump_utilization))
}
#' Plot simulation results
#'
#' @param simulation_results Results from simulate_gas_station function
#' @param type Type of plot: "both", "queue", or "utilization"
#' @return ggplot2 object or combined plot
#' @export
plot_simulation <- function(simulation_results, type = "both") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2: install.packages(\"ggplot2\")")
  }
  
  time_seq <- 1:length(simulation_results$queue_lengths)
  
  # Explicitly create data frame variables
  queue_data <- data.frame(
    Time = time_seq, 
    QueueLength = simulation_results$queue_lengths
  )
  util_data <- data.frame(
    Time = time_seq, 
    Utilization = simulation_results$pump_utilization * 100
  )
  
  if (type == "queue" || type == "both") {
    p1 <- ggplot2::ggplot(
      data = queue_data, 
      mapping = ggplot2::aes(x = Time, y = QueueLength)
    ) +
      ggplot2::geom_line(color = "steelblue") +
      ggplot2::labs(title = "Queue Length Over Time", y = "Queue Length", x = "Time")
  }
  
  if (type == "utilization" || type == "both") {
    p2 <- ggplot2::ggplot(
      data = util_data, 
      mapping = ggplot2::aes(x = Time, y = Utilization, group = 1)  
    ) +
      ggplot2::geom_line(color = "firebrick") +
      ggplot2::labs(title = "Pump Utilization", y = "Utilization (%)", x = "Time")
  }
  
  if (type == "both") {
    if (!requireNamespace("gridExtra", quietly = TRUE)) {
      stop("Please install gridExtra")
    }
    gridExtra::grid.arrange(p1, p2, ncol = 1)
  } else if (type == "queue") {
    return(p1)
  } else if (type == "utilization") {
    return(p2)
  }
}
