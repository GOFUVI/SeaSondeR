test_that("Related functions are defined",{

  expect_true(is.function(seasonder_read_reduced_encoded_data),
              info = "seasonder_read_reduced_encoded_data must be defined")
  expect_true(is.function(seasonder_read_csign), info = "The function seasonder_read_csign must be implemented")
  expect_true(is.function(seasonder_read_asign),
              info = "The function seasonder_read_asign should exist")
  expect_true(exists("seasonder_SeaSondeRCSSYApplyScaling", mode = "function"),
              info = "seasonder_SeaSondeRCSSYApplyScaling should be defined")


})




describe("Tests for seasonder_read_reduced_encoded_data function", {

  it("processes command 0x9C properly (unsigned 32-bit integer)", {
    # Prepare a stream: command 0x9C followed by 4 bytes representing 1000 (0x00 0x00 0x03 0xE8 in big endian)
    raw_vec <- as.raw(c(0x9C, 0x00, 0x00, 0x03, 0xE8))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    expect_equal(result, 1000L,
                 info = "Command 0x9C must read four bytes as an unsigned 32-bit integer and set tracking value")
  })

  it("processes command 0x94 properly (multiple 32-bit unsigned integers)", {
    # Prepare a stream: command 0x94 then count byte (0x01 => 1+1=2 integers)
    # followed by two unsigned 32-bit integers: 100 (0x00,0x00,0x00,0x64) and 200 (0x00,0x00,0x00,0xC8)
    raw_vec <- as.raw(c(0x94, 0x01,
                        0x00, 0x00, 0x00, 0x64,
                        0x00, 0x00, 0x00, 0xC8))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    expect_equal(result, c(100L, 200L),
                 info = "Command 0x94 must read (count+1) unsigned 32-bit integers and update the tracking value")
  })

  it("processes command 0xAC properly (24-bit signed integer)", {
    # Prepare a stream: command 0xAC followed by 3 bytes representing a 24-bit signed integer=5 (0x00,0x00,0x05)
    raw_vec <- as.raw(c(0xAC, 0x00, 0x00, 0x05))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    expect_equal(result, 5L,
                 info = "Command 0xAC must add the 24-bit signed value (here 5) to the tracking value")
  })

  it("processes command 0xA4 properly (multiple 24-bit signed integers)", {
    # Prepare a stream: command 0xA4 then count byte (0x01 => count+1 = 2 values)
    # followed by two SInt24 values; first: 0x00,0x00,0x03 (3) and second: 0xFF,0xFF,0xFD (should be -3)
    raw_vec <- as.raw(c(0xA4, 0x01,
                        0x00, 0x00, 0x03,
                        0xFF, 0xFF, 0xFD))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    expect_equal(result, c(3L, 0L),
                 info = "Command 0xA4 must process a sequence of 24-bit signed integers, updating the tracking value each time")
  })

  it("processes command 0x89 properly (signed 8-bit integer)", {
    # Prepare a stream: command 0x89 followed by one 8-bit signed integer; use 0xFD which is -3 in two's complement.
    raw_vec <- as.raw(c(0x89, 0xFD))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    expect_equal(result, -3L,
                 info = "Command 0x89 should read a signed 8-bit integer and add it to the tracking value")
  })

  it("processes command 0x8A properly (signed 16-bit integer)", {
    # Prepare a stream: command 0x8A followed by 2 bytes representing a 16-bit signed integer: 20 (0x00, 0x14 in big endian)
    raw_vec <- as.raw(c(0x8A, 0x00, 0x14))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    expect_equal(result, 20L,
                 info = "Command 0x8A should read a signed 16-bit integer and update the tracking value")
  })

  it("processes command 0x82 properly (multiple 16-bit signed integers)", {
    # Prepare a stream: command 0x82 then count byte (0x00 => 0+1 = 1 value)
    # followed by one 16-bit signed integer: 10 (0x00, 0x0A in big endian)
    raw_vec <- as.raw(c(0x82, 0x00, 0x00, 0x0A))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    expect_equal(result, 10L,
                 info = "Command 0x82 should read one 16-bit signed integer and update the tracking value")
  })

  it("processes command 0x81 properly (multiple 8-bit signed integers)", {
    # Prepare a stream: command 0x81 then count byte (0x02 => count+1 = 3 values)
    # followed by three 8-bit signed integers: 1, 2, and 3.
    raw_vec <- as.raw(c(0x81, 0x02, 0x01, 0x02, 0x03))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    # Tracking accumulation: 0+1 = 1, 1+2 = 3, 3+3 = 6.
    expect_equal(result, c(1L, 3L, 6L),
                 info = "Command 0x81 must correctly accumulate a series of 8-bit signed integers")
  })

  it("processes multiple commands sequentially", {
    # Combine two commands: first 0x9C sets tracking to 50,
    # then 0xAC adds a 24-bit signed integer (-2) resulting in 48.
    raw_vec <- as.raw(c(
      0x9C, 0x00, 0x00, 0x00, 0x32,   # 0x9C: sets tracking to 50
      0xAC, 0xFF, 0xFF, 0xFE          # 0xAC: adds -2 (encoded as 0xFF,0xFF,0xFE)
    ))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    expect_equal(result, c(50L, 48L),
                 info = "Multiple commands should update the tracking value sequentially (50 then 50-2=48)")
  })

  it("respects the reduced block size limit", {
    # Prepare a stream with two commands; however the key's size limits processing to only the first command.
    # Here, command 0x89 (two bytes) should be processed and the subsequent bytes ignored.
    raw_vec <- as.raw(c(0x89, 0x03, 0x8A, 0x00, 0x14))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = 2)  # Process only the first two bytes (command 0x89 and its parameter)
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "big")
    expect_equal(result, 3L,
                 info = "The function should only process bytes within the block size specified by key$size")
  })

  it("throws an error for an unknown command byte", {

    raw_vec <- as.raw(c(0xFF))  # 0xFF is not a recognized command
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con), add = TRUE)
    key <- list(size = length(raw_vec))
    expect_error(seasonder_read_reduced_encoded_data(con, key, endian = "big"),
                 "Invalid command encountered",
                 info = "An unrecognized command byte should trigger an error")
  })

  it("processes command 0x9C correctly with little endian format", {
    # In little endian, the 32-bit unsigned integer 1000 is encoded as: 0xE8,0x03,0x00,0x00.
    raw_vec <- as.raw(c(0x9C, 0xE8, 0x03, 0x00, 0x00))
    con <- rawConnection(raw_vec, open = "rb")
    on.exit(close(con))
    key <- list(size = length(raw_vec))
    result <- seasonder_read_reduced_encoded_data(con, key, endian = "little")
    expect_equal(result, 1000L,
                 info = "Command 0x9C must correctly process a 32-bit unsigned integer in little endian format")
  })

})

describe("seasonder_read_csign", {

  it("returns a list with 6 groups for valid input with nDopplers = 8", {
    # For nDopplers = 8, key$size should be 6 bytes (6 * (8/8) = 6)
    key <- list(key = "csign", size = 6)
    # Prepare a raw vector of 6 bytes with known values
    raw_data <- as.raw(c(0xFF, 0x00, 0xA5, 0x5A, 0xCC, 0x33))
    con <- rawConnection(raw_data, "rb")
    on.exit(close(con), add = TRUE)

    result <- seasonder_read_csign(con, key)

    # Check that the result is a list and has 6 elements
    expect_true(is.list(result), info = "The function should return a list")
    expect_equal(length(result), 6, info = "The result list should have 6 elements")

    # Verify that the list names match the expected groups
    expected_names <- c("C13r", "C13i", "C23r", "C23i", "C12r", "C12i")
    expect_equal(names(result), expected_names,
                 info = "The list names should be C13r, C13i, C23r, C23i, C12r, C12i")

    # For nDopplers = 8, each group comes from 1 byte so each vector must have 8 bits
    for (group in result) {
      expect_equal(length(group), 8,
                   info = "Each group should have 8 bits when nDopplers = 8")
      expect_true(all(group %in% c(0, 1)),
                  info = "Each bit in a group should be either 0 or 1")
    }

    # Verify individual groups by comparing with rawToBits conversion of the corresponding byte(s)
    expected_C13r <- as.integer(rawToBits(raw_data[1]))
    expect_equal(result$C13r, expected_C13r,
                 info = "C13r should match the bit conversion of the first byte")

    expected_C13i <- as.integer(rawToBits(raw_data[2]))
    expect_equal(result$C13i, expected_C13i,
                 info = "C13i should match the bit conversion of the second byte")
  })

  it("properly splits input into groups for nDopplers = 16", {
    # For nDopplers = 16, key$size should be 12 bytes (6 * (16/8) = 12)
    key <- list(key = "csign", size = 12)
    # Prepare a raw vector with 12 bytes (2 bytes per group)
    raw_data <- as.raw(c(
      0xFF, 0x00,   # C13r group: 2 bytes
      0xAA, 0x55,   # C13i group: 2 bytes
      0x11, 0x22,   # C23r group: 2 bytes
      0x33, 0x44,   # C23i group: 2 bytes
      0x55, 0x66,   # C12r group: 2 bytes
      0x77, 0x88    # C12i group: 2 bytes
    ))
    con <- rawConnection(raw_data, "rb")
    on.exit(close(con), add = TRUE)

    result <- seasonder_read_csign(con, key)

    # Each group should have 2 bytes worth of bits, i.e., 16 bits in total.
    for (group in result) {
      expect_equal(length(group), 16,
                   info = "Each group should have 16 bits when nDopplers = 16")
      expect_true(all(group %in% c(0, 1)),
                  info = "Each bit in a group should be either 0 or 1")
    }

    # Verify that C13r is the concatenation of the bit conversions for its 2 bytes
    expected_C13r <- c(as.integer(rawToBits(raw_data[1])),
                       as.integer(rawToBits(raw_data[2])))
    expect_equal(result$C13r, expected_C13r,
                 info = "C13r should correctly combine bits from its two bytes")
  })

  it("errors when the connection provides fewer bytes than key$size", {
    # Setup a scenario where the connection returns fewer bytes than required
    key <- list(key = "csign", size = 6)
    # Provide only 4 bytes intentionally
    raw_data <- as.raw(c(0xFF, 0x00, 0xA5, 0x5A))
    con <- rawConnection(raw_data, "rb")
    on.exit(close(con), add = TRUE)

    expect_error(
      seasonder_read_csign(con, key),
      regexp = "([Nn]ot enough|insufficient)",
      info = "The function should error when the connection does not provide enough bytes"
    )
  })
})





describe("seasonder_read_asign", {

  it("returns a list with 3 groups for valid input with nDopplers = 8", {
    # For nDopplers = 8, the size should be 3 * (8/8) = 3 bytes
    key <- list(key = "asign", size = 3)
    # Prepare a raw vector with 3 bytes; each byte will represent one group
    raw_data <- as.raw(c(0xFF, 0x0A, 0x55))
    con <- rawConnection(raw_data, "rb")
    on.exit(close(con), add = TRUE)

    result <- seasonder_read_asign(con, key)

    # Verify that the result is a list with 3 groups
    expect_true(is.list(result),
                info = "The function should return a list")
    expect_equal(length(result), 3,
                 info = "The result list should have 3 groups for cs1a, cs2a, and cs3a")

    # Check that the list has the expected group names
    expected_names <- c("cs1a", "cs2a", "cs3a")
    expect_equal(names(result), expected_names,
                 info = "The list names should be cs1a, cs2a, cs3a")

    # For nDopplers = 8, each group comes from 1 byte so each vector should have 8 bits
    for (group in result) {
      expect_equal(length(group), 8,
                   info = "Each group should have 8 bits when nDopplers = 8")
      expect_true(all(group %in% c(0, 1)),
                  info = "Each bit in a group should be either 0 or 1")
    }

    # Verify each group by comparing with the rawToBits conversion of the corresponding byte
    expected_cs1a <- as.integer(rawToBits(raw_data[1]))
    expected_cs2a <- as.integer(rawToBits(raw_data[2]))
    expected_cs3a <- as.integer(rawToBits(raw_data[3]))

    expect_equal(result$cs1a, expected_cs1a,
                 info = "cs1a should match the bit conversion of the first byte")
    expect_equal(result$cs2a, expected_cs2a,
                 info = "cs2a should match the bit conversion of the second byte")
    expect_equal(result$cs3a, expected_cs3a,
                 info = "cs3a should match the bit conversion of the third byte")
  })

  it("properly splits input into groups for nDopplers = 16", {
    # For nDopplers = 16, key$size should be 3 * (16/8) = 6 bytes
    key <- list(key = "asign", size = 6)
    # Create a raw vector of 6 bytes; each group will consist of 2 bytes (16 bits)
    raw_data <- as.raw(c(
      0xFF, 0x00,   # cs1a group: 2 bytes
      0xAA, 0x55,   # cs2a group: 2 bytes
      0x11, 0x22    # cs3a group: 2 bytes
    ))
    con <- rawConnection(raw_data, "rb")
    on.exit(close(con), add = TRUE)

    result <- seasonder_read_asign(con, key)

    # Each group should have 16 bits after conversion (2 bytes * 8 bits)
    for (group in result) {
      expect_equal(length(group), 16,
                   info = "Each group should have 16 bits for nDopplers = 16")
      expect_true(all(group %in% c(0, 1)),
                  info = "Each bit in a group should be either 0 or 1")
    }

    # Verify that each group correctly combines the bits from its two bytes
    expected_cs1a <- c(as.integer(rawToBits(raw_data[1])),
                       as.integer(rawToBits(raw_data[2])))
    expected_cs2a <- c(as.integer(rawToBits(raw_data[3])),
                       as.integer(rawToBits(raw_data[4])))
    expected_cs3a <- c(as.integer(rawToBits(raw_data[5])),
                       as.integer(rawToBits(raw_data[6])))

    expect_equal(result$cs1a, expected_cs1a,
                 info = "cs1a should correctly combine bits from its two bytes")
    expect_equal(result$cs2a, expected_cs2a,
                 info = "cs2a should correctly combine bits from its two bytes")
    expect_equal(result$cs3a, expected_cs3a,
                 info = "cs3a should correctly combine bits from its two bytes")
  })

  it("errors when the connection provides fewer bytes than key$size", {
    # Set key to require 6 bytes (e.g., for nDopplers = 16)
    key <- list(key = "asign", size = 6)
    # Provide only 4 bytes intentionally to simulate insufficient data
    raw_data <- as.raw(c(0xFF, 0x00, 0xAA, 0x55))
    con <- rawConnection(raw_data, "rb")
    on.exit(close(con), add = TRUE)

    expect_error(
      seasonder_read_asign(con, key),
      regexp = "([Nn]ot enough|insufficient)",
      info = "The function should error when the connection does not provide enough bytes"
    )
  })

  it("errors when key$size is not divisible by 3", {
    # For seasonder_read_asign, the total size should be divisible by 3.
    key <- list(key = "asign", size = 5)  # 5 is not divisible by 3
    raw_data <- as.raw(c(0xFF, 0x00, 0xAA, 0x55, 0x11))
    con <- rawConnection(raw_data, "rb")
    on.exit(close(con), add = TRUE)

    expect_error(
      seasonder_read_asign(con, key),
      regexp = "Invalid total size",
      info = "The function should error when key$size is not divisible by 3"
    )
  })
})





# Group tests for seasonder_SeaSondeRCSSYApplyScaling

describe("seasonder_SeaSondeRCSSYApplyScaling", {

  it("correctly scales normal numeric values", {
    # Use sample parameters
    values <- list(c(1000, 2000, 3000))
    fmax <- 5
    fmin <- 0
    fscale <- 1000
    dbRef <- -20

    # Expected calculations:
    # For 1000: intermediate = 1000*5/1000 = 5, voltage = 10^((5-20)/10) = 10^(-1.5) ~ 0.03162278
    # For 2000: intermediate = 2000*5/1000 = 10, voltage = 10^((10-20)/10) = 10^(-1) = 0.1
    # For 3000: intermediate = 3000*5/1000 = 15, voltage = 10^((15-20)/10) = 10^(-0.5) ~ 0.3162278
    expected <- c(10^((5 - 20)/10), 10^((10 - 20)/10), 10^((15 - 20)/10))

    result <- seasonder_SeaSondeRCSSYApplyScaling(values, fmax, fmin, fscale, dbRef)

    expect_equal(length(result), length(values), info = "Output list should have the same number of elements as input list")
    expect_true(is.numeric(result[[1]]), info = "The output vector elements should be numeric")

    expect_equal(result[[1]], expected, tolerance = 1e-6, info = "The scaled values do not match the expected calculations")
  })

  it("returns NaN for value 0xFFFFFFFF", {
    values <- list(c(4294967295 , 1500))
    fmax <- 4
    fmin <- 1
    fscale <- 2000
    dbRef <- -10

    result <- seasonder_SeaSondeRCSSYApplyScaling(values, fmax, fmin, fscale, dbRef)
    expect_true(is.nan(result[[1]][1]), info = "Expected NaN for value 0xFFFFFFFF")
  })

  it("preserves the structure of the input list", {
    values <- list(a = c(1000, 2000), b = c(3000, 4000, 5000))
    fmax <- 6
    fmin <- 2
    fscale <- 1500
    dbRef <- -15

    result <- seasonder_SeaSondeRCSSYApplyScaling(values, fmax, fmin, fscale, dbRef)

    # Check if the names are preserved
    expect_equal(names(result), names(values), info = "The names of the list elements must be preserved")

    # Check if the lengths of individual vectors are preserved
    expect_equal(sapply(result, length), sapply(values, length),
                 info = "Each output vector should have the same length as the corresponding input vector")
  })

  describe("Parameter validation", {
    it("errors when 'values' is not a list", {
      expect_error(seasonder_SeaSondeRCSSYApplyScaling(1000, 5, 0, 1000, -20),
                   info = "Function should error when values is not a list")
    })

    it("errors when 'fscale' is zero", {
      values <- list(c(1000,2000))
      expect_error(seasonder_SeaSondeRCSSYApplyScaling(values, 5, 0, 0, -20),
                   info = "Function should error when fscale is zero")
    })

    it("errors when an element in values is not a numeric vector", {
      values <- list(c(1000, 2000), "not numeric")
      expect_error(seasonder_SeaSondeRCSSYApplyScaling(values, 5, 0, 1000, -20),
                   info = "Function should error when an element in values is not numeric")
    })
  })

})


describe("seasonder_readBodyRangeCell",{

  # Test: Verify that when a scaling block is provided, the reduced data block is scaled correctly

  it("scaling is applied to reduced data block when scal is present", {
    # Simulate a sequence of keys using an overridden function
    counter <- 1
    keys <- list(
      list(key = "scal", size = 16),
      list(key = "cs1a", size = 10),
      list(key = "END ", size = 0)
    )


    specs <- list(
      scal = list(dummy = TRUE),
      cs1a = list(dummy = TRUE),
      "END " = list(dummy = TRUE)
    )

    con <- rawConnection(raw(0), "rb")
    on.exit(close(con))
    with_mocked_bindings(
      seasonder_readSeaSondeCSFileBlock = function(specs_key_size, connection, endian) {
        res <- keys[[counter]]
        counter <<- counter + 1
        return(res)
      },

      # Override seasonder_readCSSYFields to simulate reading scaling parameters
      seasonder_readCSSYFields = function(connection, spec, endian, parent_key) {
        return(list(fmax = 5, fmin = 0, fscale = 1000))
      },

      # Override seasonder_read_reduced_encoded_data to simulate reading a reduced data block
      seasonder_read_reduced_encoded_data = function(connection, key, endian) {
        return(c(1000, 2000, 3000))
      },
    result <- seasonder_readBodyRangeCell(con, specs, dbRef = -20, endian = "big", specs_key_size = NULL)
)
    expected <- sapply(c(1000, 2000, 3000), function(val){
      if(val == 0xFFFFFFFF) return(NaN)
      intermediate <- val * (5 - 0) / 1000 + 0
      10^((intermediate - 20)/10)
    })

    expect_equal(result$cs1a, expected)
  })

  # Test: Verify that when no scaling block is provided, the reduced data block remains unscaled

  it("reduced data block is unscaled when scal is absent", {
    counter <- 1
    keys <- list(
      list(key = "cs1a", size = 10),
      list(key = "END ", size = 0)
    )



    specs <- list(
      cs1a = list(dummy = TRUE),
      "END " = list(dummy = TRUE)
    )

    con <- rawConnection(raw(0), "rb")
    on.exit(close(con))
    testthat::with_mocked_bindings(
      seasonder_read_reduced_encoded_data = function(connection, key, endian) {
        return(c(1000, 2000, 3000))
      },
      seasonder_readSeaSondeCSFileBlock = function(specs_key_size, connection, endian) {
        res <- keys[[counter]]
        counter <<- counter + 1
        return(res)
      },

      result <- seasonder_readBodyRangeCell(con, specs, endian = "big", specs_key_size = NULL)
    )


    expect_equal(result$cs1a, c(1000, 2000, 3000))
  })

})

#### seasonder_CSSY2CSData ####
describe("seasonder_CSSY2CSData transformation tests", {

  it("transforms a single cell correctly", {
    # Create a single cell with required fields
    cell <- list(
      indx  = list(index = 1),
      cs1a  = c(1, 2, 3),
      cs2a  = c(4, 5, 6),
      cs3a  = c(7, 8, 9),
      c12r  = c(10, 11, 12),
      c12i  = c(13, 14, 15),
      c13r  = c(16, 17, 18),
      c13i  = c(19, 20, 21),
      c23r  = c(22, 23, 24),
      c23i  = c(25, 26, 27),
      csqf  = c(28, 29, 30)
    )
    # The body is a list containing one cell
    body <- list(cell)

    # Invoke the function to transform the CSSY body structure
    result <- seasonder_CSSY2CSData(body)

    # Check that result is a list with required keys
    required_keys <- c("SSA1", "SSA2", "SSA3", "CS12", "CS13", "CS23", "QC")
    expect_true(all(required_keys %in% names(result)), info = "Result must contain required keys")

    # Verify that each output is a matrix
    lapply(required_keys, function(key) {
      expect_true(is.matrix(result[[key]]), info = paste0(key, " should be a matrix"))
    })


    # For self-spectra, the row corresponding to index 1 must equal the input vector value
    expect_equal(result$SSA1[1, ], cell$cs1a, info = "SSA1 should equal cs1a for cell at index 1")
    expect_equal(result$SSA2[1, ], cell$cs2a, info = "SSA2 should equal cs2a for cell at index 1")
    expect_equal(result$SSA3[1, ], cell$cs3a, info = "SSA3 should equal cs3a for cell at index 1")

    # For cross spectra, check formation of complex numbers
    expected_CS12 <- complex(real = cell$c12r, imaginary = cell$c12i)
    expected_CS13 <- complex(real = cell$c13r, imaginary = cell$c13i)
    expected_CS23 <- complex(real = cell$c23r, imaginary = cell$c23i)
    expect_equal(result$CS12[1, ], expected_CS12, info = "CS12 should be formed from c12r and c12i")
    expect_equal(result$CS13[1, ], expected_CS13, info = "CS13 should be formed from c13r and c13i")
    expect_equal(result$CS23[1, ], expected_CS23, info = "CS23 should be formed from c23r and c23i")

    # Check quality control matrix (QC) equals csqf
    expect_equal(result$QC[1, ], cell$csqf, info = "QC should equal csqf")
  })

  it("transforms multiple cells correctly", {
    # Create two cells with different indices and 4-element vectors
    cell1 <- list(
      indx  = list(index = 1),
      cs1a  = c(1,2,3,4),
      cs2a  = c(5,6,7,8),
      cs3a  = c(9,10,11,12),
      c12r  = c(13,14,15,16),
      c12i  = c(17,18,19,20),
      c13r  = c(21,22,23,24),
      c13i  = c(25,26,27,28),
      c23r  = c(29,30,31,32),
      c23i  = c(33,34,35,36),
      csqf  = c(37,38,39,40)
    )
    cell2 <- list(
      indx  = list(index = 2),
      cs1a  = c(101,102,103,104),
      cs2a  = c(105,106,107,108),
      cs3a  = c(109,110,111,112),
      c12r  = c(113,114,115,116),
      c12i  = c(117,118,119,120),
      c13r  = c(121,122,123,124),
      c13i  = c(125,126,127,128),
      c23r  = c(129,130,131,132),
      c23i  = c(133,134,135,136),
      csqf  = c(137,138,139,140)
    )
    body <- list(cell1, cell2)

    # Call the transformation function
    result <- seasonder_CSSY2CSData(body)

    # Expected dimensions: matrix should have rows equal to the highest cell index (here 2) and columns equal to vector lengths (4)
    expected_dim <- c(2, 4)

    required_keys <- c("SSA1", "SSA2", "SSA3", "CS12", "CS13", "CS23", "QC")
    lapply(required_keys, function(key) {
      expect_equal(dim(result[[key]]), expected_dim, info = paste0(key, " should have dimensions 2 x 4"))
    })

    # Check self spectra: row 1 should correspond to cell1, row 2 to cell2
    expect_equal(result$SSA1[1, ], cell1$cs1a, info = "SSA1 row1 should equal cs1a of cell1")
    expect_equal(result$SSA1[2, ], cell2$cs1a, info = "SSA1 row2 should equal cs1a of cell2")

    # Check cross spectra complex formation
    expected_CS12_cell1 <- complex(real = cell1$c12r, imaginary = cell1$c12i)
    expected_CS12_cell2 <- complex(real = cell2$c12r, imaginary = cell2$c12i)
    expect_equal(result$CS12[1, ], expected_CS12_cell1, info = "CS12 row1 should be formed correctly")
    expect_equal(result$CS12[2, ], expected_CS12_cell2, info = "CS12 row2 should be formed correctly")

    expected_CS13_cell1 <- complex(real = cell1$c13r, imaginary = cell1$c13i)
    expected_CS13_cell2 <- complex(real = cell2$c13r, imaginary = cell2$c13i)
    expect_equal(result$CS13[1, ], expected_CS13_cell1, info = "CS13 row1 should be formed correctly")
    expect_equal(result$CS13[2, ], expected_CS13_cell2, info = "CS13 row2 should be formed correctly")

    expected_CS23_cell1 <- complex(real = cell1$c23r, imaginary = cell1$c23i)
    expected_CS23_cell2 <- complex(real = cell2$c23r, imaginary = cell2$c23i)
    expect_equal(result$CS23[1, ], expected_CS23_cell1, info = "CS23 row1 should be formed correctly")
    expect_equal(result$CS23[2, ], expected_CS23_cell2, info = "CS23 row2 should be formed correctly")

    # Check QC matrix
    expect_equal(result$QC[1, ], cell1$csqf, info = "QC row1 should equal csqf of cell1")
    expect_equal(result$QC[2, ], cell2$csqf, info = "QC row2 should equal csqf of cell2")
  })
})
