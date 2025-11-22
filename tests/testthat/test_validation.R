test_that("input validation works", {
  # 测试无效输入 - 匹配新的错误信息
  expect_error(simulate_gas_station(arrival_rate = -1), "arrival_rate must be non-negative")
  expect_error(simulate_gas_station(service_rate = 0), "service_rate must be positive")
  expect_error(simulate_gas_station(n_pumps = 0), "n_pumps must be positive")
  expect_error(simulate_gas_station(n_pumps = 2.5), "n_pumps must be an integer")
  expect_error(simulate_gas_station(sim_time = -10), "sim_time must be positive")
})

test_that("valid inputs work correctly", {
  # 测试有效输入应该正常工作
  expect_silent(simulate_gas_station(arrival_rate = 0, sim_time = 10))
  expect_silent(simulate_gas_station(arrival_rate = 5, service_rate = 3, n_pumps = 2))
})
