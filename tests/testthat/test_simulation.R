test_that("simulate_gas_station function works correctly", {
  # 测试基本功能
  results <- simulate_gas_station(
    arrival_rate = 8,
    service_rate = 8, 
    n_pumps = 3,
    sim_time = 60,
    random_seed = 123
  )
  
  # 检查返回对象结构
  expect_s3_class(results, "gas_station_simulation")
  expect_named(results, c("waiting_times", "queue_lengths", "pump_utilization", 
                         "total_customers", "simulation_time", "parameters"))
  
  # 检查数据类型
  expect_type(results$waiting_times, "double")
  expect_type(results$queue_lengths, "double")
  expect_type(results$pump_utilization, "double")
  expect_type(results$total_customers, "integer")
  
  # 检查数值合理性
  expect_true(results$total_customers >= 0)
  expect_true(all(results$waiting_times >= 0))
  expect_true(all(results$queue_lengths >= 0))
  expect_true(all(results$pump_utilization >= 0 & results$pump_utilization <= 1))
})

test_that("simulation handles edge cases", {
  # 测试零到达率 - 现在应该正常工作
  results_zero <- simulate_gas_station(arrival_rate = 0, sim_time = 10, random_seed = 123)
  expect_equal(results_zero$total_customers, 0)
  expect_equal(length(results_zero$waiting_times), 0)
  
  # 测试很短模拟时间
  results_short <- simulate_gas_station(sim_time = 1, random_seed = 123)
  expect_true(results_short$total_customers >= 0)
})

test_that("random seed produces reproducible results", {
  # 相同种子应该产生相同结果
  results1 <- simulate_gas_station(random_seed = 456, sim_time = 30)
  results2 <- simulate_gas_station(random_seed = 456, sim_time = 30)
  
  expect_equal(results1$total_customers, results2$total_customers)
  expect_equal(results1$waiting_times, results2$waiting_times)
})
