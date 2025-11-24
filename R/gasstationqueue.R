# GasStationQueue: Gas Station Queuing Simulation Using Monte Carlo Methods

utils::globalVariables(c("Time", "QueueLength", "Utilization"))

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
    next_arrival <- stats::rexp(1, rate = arrival_rate / 60)
  }
  
  while (current_time < sim_time) {
    if (current_time >= next_arrival && current_time < sim_time) {
      queue <- c(queue, current_time)
      if (arrival_rate > 0) {
        next_arrival <- current_time + stats::rexp(1, rate = arrival_rate / 60)
      } else {
        next_arrival <- Inf
      }
    }
    
    for (i in 1:n_pumps) {
      if (pumps[i] <= current_time && length(queue) > 0) {
        arrival_time <- queue[1]
        queue <- queue[-1]
        service_time <- stats::rexp(1, rate = service_rate / 60)
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
  
  n_points <- length(simulation_results$queue_lengths)
  time_points <- seq(0, simulation_results$simulation_time, length.out = n_points)
  
  
  # Explicitly create data frame variables
  queue_data <- data.frame(
    Time = time_points, 
    QueueLength = simulation_results$queue_lengths
  )
  util_data <- data.frame(
    Time = time_points, 
    Utilization = simulation_results$pump_utilization * 100
  )
  
  if (type == "queue" || type == "both") {
    p1 <- ggplot2::ggplot(
      data = queue_data, 
      mapping = ggplot2::aes(x = Time, y = QueueLength)
    ) +
      ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
      ggplot2::labs(
        title = "Queue Length Over Time", 
        y = "Queue Length", 
        x = "Time (minutes)"
      ) +
      
      ggplot2::scale_x_continuous(limits = c(0, simulation_results$simulation_time)) +
      ggplot2::scale_y_continuous(limits = c(0, max(1.5, max(queue_data$QueueLength)))) +
      ggplot2::theme_minimal()
  }
  
  if (type == "utilization" || type == "both") {
    p2 <- ggplot2::ggplot(
      data = util_data, 
      mapping = ggplot2::aes(x = Time, y = Utilization)
    ) +
      ggplot2::geom_line(color = "firebrick", linewidth = 0.8) +
      ggplot2::labs(
        title = "Pump Utilization", 
        y = "Utilization (%)", 
        x = "Time (minutes)"
      ) +
      ggplot2::scale_x_continuous(limits = c(0, simulation_results$simulation_time)) +
      ggplot2::scale_y_continuous(limits = c(0, 100)) +
      ggplot2::theme_minimal()
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
#' Advanced System Performance Analysis
#'
#' This function provides comprehensive analysis of gas station queueing system
#' performance, including idle time analysis and queueing theory metrics.
#'
#' @param simulation_results Results from simulate_gas_station function
#' @return A list containing:
#' \item{idle_probability}{Probability that all pumps are idle}
#' \item{avg_busy_period}{Average duration of busy periods (in time units)}
#' \item{traffic_intensity}{System traffic intensity (ρ = λ/cμ)}
#' \item{system_stable}{Boolean indicating if system is stable (ρ < 1)}
#' @export
#'
#' @examples
#' \donttest{
#' results <- simulate_gas_station(arrival_rate = 8, service_rate = 8, n_pumps = 3)
#' analysis <- analyze_system_performance(results)
#' print(analysis)
#' }
analyze_system_performance <- function(simulation_results) {
  # Input validation
  if (!inherits(simulation_results, "gas_station_simulation")) {
    stop("Input must be a gas_station_simulation object")
  }
  
  utilization <- simulation_results$pump_utilization
  
  # Idle time analysis 
  idle_prob <- mean(utilization == 0)
  
  # Busy period analysis 
  busy_periods <- rle(utilization > 0.1)
  
  # Queueing theory metrics
  arrival_rate <- simulation_results$parameters$arrival_rate
  service_rate <- simulation_results$parameters$service_rate
  n_pumps <- simulation_results$parameters$n_pumps
  
  traffic_intensity <- arrival_rate / (n_pumps * service_rate)
  
  return(list(
    idle_probability = idle_prob,
    avg_busy_period = ifelse(length(busy_periods$lengths[busy_periods$values == TRUE]) > 0, 
                             mean(busy_periods$lengths[busy_periods$values == TRUE]), 
                             0),
    traffic_intensity = traffic_intensity,
    system_stable = traffic_intensity < 1
  ))
}

