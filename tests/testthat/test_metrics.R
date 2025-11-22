test_that("performance metrics functions work correctly", {
  # 创建测试数据
  test_results <- list(
    waiting_times = c(1.5, 2.3, 0.8, 3.1),
    queue_lengths = c(1, 2, 1, 0, 1),
    pump_utilization = c(0.6, 0.7, 0.8, 0.5),
    total_customers = 4,
    simulation_time = 60,
    parameters = list(arrival_rate = 8, service_rate = 8, n_pumps = 3)
  )
  class(test_results) <- "gas_station_simulation"
  
  # 测试平均等待时间
  avg_wait <- get_avg_waiting_time(test_results)
  expect_type(avg_wait, "double")
  expect_equal(avg_wait, mean(c(1.5, 2.3, 0.8, 3.1)))
  
  # 测试平均队列长度
  avg_queue <- get_avg_queue_length(test_results)
  expect_type(avg_queue, "double")
  expect_equal(avg_queue, mean(c(1, 2, 1, 0, 1)))
  
  # 测试油泵利用率
  utilization <- get_pump_utilization(test_results)
  expect_type(utilization, "double")
  expect_equal(utilization, mean(c(0.6, 0.7, 0.8, 0.5)))
})

test_that("metrics handle empty inputs", {
  # 测试无顾客的情况
  empty_results <- list(
    waiting_times = numeric(0),
    queue_lengths = c(0, 0, 0),
    pump_utilization = c(0, 0, 0),
    total_customers = 0,
    simulation_time = 60,
    parameters = list(arrival_rate = 8, service_rate = 8, n_pumps = 3)
  )
  class(empty_results) <- "gas_station_simulation"
  
  expect_equal(get_avg_waiting_time(empty_results), 0)
  expect_equal(get_avg_queue_length(empty_results), 0)
  expect_equal(get_pump_utilization(empty_results), 0)
})
