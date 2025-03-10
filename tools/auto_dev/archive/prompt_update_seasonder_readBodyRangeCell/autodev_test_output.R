# Tests using testthat

library(testthat)

# Test: Verify that when a scaling block is provided, the reduced data block is scaled correctly

test_that("scaling is applied to reduced data block when scal is present", {
  # Simulate a sequence of keys using an overridden function
  counter <- 1
  keys <- list(
    list(key = "scal", size = 16),
    list(key = "cs1a", size = 10),
    list(key = "END ", size = 0)
  )
  seasonder_readSeaSondeCSFileBlock <<- function(specs_key_size, connection, endian) {
    res <- keys[[counter]]
    counter <<- counter + 1
    return(res)
  }

  # Override seasonder_readCSSYFields to simulate reading scaling parameters
  seasonder_readCSSYFields <<- function(connection, spec, endian, parent_key) {
    return(list(fmax = 5, fmin = 0, fscale = 1000, dbRef = -20))
  }

  # Override seasonder_read_reduced_encoded_data to simulate reading a reduced data block
  seasonder_read_reduced_encoded_data <<- function(connection, key, endian) {
    return(c(1000, 2000, 3000))
  }

  specs <- list(
    scal = list(dummy = TRUE),
    cs1a = list(dummy = TRUE),
    "END " = list(dummy = TRUE)
  )

  con <- rawConnection(raw(0), "rb")
  on.exit(close(con))

  result <- seasonder_readBodyRangeCell(con, specs, endian = "big", specs_key_size = NULL)

  expected <- sapply(c(1000, 2000, 3000), function(val){
    if(val == 0xFFFFFFFF) return(NaN)
    intermediate <- val * (5 - 0) / 1000 + 0
    10^((intermediate - 20)/10)
  })

  expect_equal(result$cs1a, expected)
})

# Test: Verify that when no scaling block is provided, the reduced data block remains unscaled

test_that("reduced data block is unscaled when scal is absent", {
  counter <- 1
  keys <- list(
    list(key = "cs1a", size = 10),
    list(key = "END ", size = 0)
  )
  seasonder_readSeaSondeCSFileBlock <<- function(specs_key_size, connection, endian) {
    res <- keys[[counter]]
    counter <<- counter + 1
    return(res)
  }

  seasonder_read_reduced_encoded_data <<- function(connection, key, endian) {
    return(c(1000, 2000, 3000))
  }

  specs <- list(
    cs1a = list(dummy = TRUE),
    "END " = list(dummy = TRUE)
  )

  con <- rawConnection(raw(0), "rb")
  on.exit(close(con))
  result <- seasonder_readBodyRangeCell(con, specs, endian = "big", specs_key_size = NULL)

  expect_equal(result$cs1a, c(1000, 2000, 3000))
})
