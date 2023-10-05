test_that("Related functions are defined",{

  expect_true(is.function(seasonder_raw_to_int))
  expect_true(is.function(seasonder_readCSField))
  expect_true(is.function(seasonder_readSeaSondeCSFileBlock))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeader))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeaderV1))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeaderV2))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeaderV3))

})


describe("seasonder_raw_to_int",{

  it("should convert an unsigned 64bit integer correctly", {

    r <- seasonder_int_to_raw(0)


    expect_equal(as.character(seasonder_raw_to_int(r, signed=F)),  "0")


      r <- seasonder_int_to_raw(300)


    expect_equal(as.character(seasonder_raw_to_int(r, signed=F)),  "300")





    r <-seasonder_int_to_raw(bit64::lim.integer64()[2])

    expect_equal(as.character(seasonder_raw_to_int(r, signed=F)),  as.character(bit64::lim.integer64()[2]))


  })

  it("should convert a signed 64bit integer correctly", {

    r <- seasonder_int_to_raw(0)
    expect_equal(as.character(seasonder_raw_to_int(r, signed=T)),  "0")

    r <- as.raw(c(0x12,0x34,0x56,0x78,0x90,0xAB,0xCD,0xEF))
    expect_equal(as.character(seasonder_raw_to_int(r, signed=T)),  "1311768467294899695")


    r <- seasonder_int_to_raw(-2)
    expect_equal(as.character(seasonder_raw_to_int(r, signed=T)),  "-2")

    r <- seasonder_int_to_raw(-1)
    expect_equal(as.character(seasonder_raw_to_int(r, signed=T)),  "-1")



    r <- seasonder_int_to_raw(bit64::lim.integer64()[1])
    expect_equal(as.character(seasonder_raw_to_int(r, signed=T)), as.character(bit64::lim.integer64()[1]))



    r <- seasonder_int_to_raw(bit64::lim.integer64()[2])
    expect_equal(as.character(seasonder_raw_to_int(r, signed=T)), as.character(bit64::lim.integer64()[2]))

    r <- seasonder_int_to_raw(NA)
    expect_equal(as.character(seasonder_raw_to_int(r, signed=T)), as.character(NA))


  })


})

describe("seasonder_readCSField", {



  it("should read an unsigned 8bit integer correctly", {
    con <- rawConnection(as.raw(c(0x12)))  # Create a raw connection with a single byte 0x12
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "UInt8"), 0x12)

  })

  it("should read a signed 8bit integer correctly", {
    con <- rawConnection(as.raw(c(0xFE)))  # Create a raw connection with a single byte 0xFE (-2 in signed 8-bit)
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "SInt8"), -2)

  })

  it("should read a Float correctly", {
    # Float for 42.41 in IEEE single precision is 0x4229a3d7
    con <- rawConnection(as.raw(c(0x42, 0x29, 0xa3, 0xd7)))
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "Float"), 42.41, tolerance = 0.0001)

  })

  it("should read an unsigned 16bit integer correctly", {
    con <- rawConnection(as.raw(c(0x12, 0x34)))
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "UInt16"), 0x1234)

  })

  it("should read a signed 16bit integer correctly", {
    con <- rawConnection(as.raw(c(0xFF, 0xFE)))  # -2 in signed 16-bit
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "SInt16"), -2)

  })

  it("should read an unsigned 32bit integer correctly", {
    con <- rawConnection(as.raw(c(0x12, 0x34, 0x56, 0x78)))
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "UInt32"), 0x12345678)

  })

  it("should read a signed 32bit integer correctly", {
    con <- rawConnection(as.raw(c(0xFF, 0xFF, 0xFF, 0xFE)))  # -2 in signed 32-bit
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "SInt32"), -2)

  })

  it("should read an unsigned 64bit integer correctly", {
    r <- seasonder_int_to_raw(300)

    con <- rawConnection(r)
    on.exit(close(con))

    mk  <- mockthat::mock(300)

    mockthat::with_mock(seasonder_raw_to_int=mk,
                        seasonder_readCSField(con,"UInt64"))

    expect_equal(mockthat::mock_n_called(mk),1 )
    expect_equal(mockthat::mock_arg(mk,"signed"),FALSE)



  })

  it("should read a signed 64bit integer correctly", {
    r <- seasonder_int_to_raw(300)

    con <- rawConnection(r)
    on.exit(close(con))

    mk  <- mockthat::mock(300)

    mockthat::with_mock(seasonder_raw_to_int=mk,
                        seasonder_readCSField(con,"SInt64"))

    expect_equal(mockthat::mock_n_called(mk),1 )
    expect_equal(mockthat::mock_arg(mk,"signed"),TRUE)

  })

  it("should read a Double correctly", {
    # Double for 42.42 in IEEE double precision is 0x40452d4fdf3b645a
    con <- rawConnection(as.raw(c(0x40, 0x45, 0x2d, 0x4f, 0xdf, 0x3b, 0x64, 0x5a)))
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "Double"), 42.354, tolerance = 0.0001)

  })

  it("should read a Complex correctly", {
    # Complex for real 2.0 and imaginary 3.0
    # Real part (2.0) in IEEE single precision is 0x40000000
    # Imaginary part (3.0) in IEEE single precision is 0x40400000
    con <- rawConnection(as.raw(c(0x40, 0x00, 0x00, 0x00, 0x40, 0x40, 0x00, 0x00)))
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "Complex"), complex(real = 2, imaginary = 3))

  })

  it("should read a String correctly", {
    con <- rawConnection(c(charToRaw("Test String"), as.raw(0)))  # Include null terminator
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "String"), "Test String")

  })



  it("should throw an error if connection is not open", {
    con <- rawConnection(as.raw(c(0x12)))
    close(con)  # Close the connection before using it
    expect_error(seasonder_readCSField(con, "UInt8"), "Connection is not open.")
  })

  it("should throw an error if there is any problem while reading", {
    con <- textConnection("Test")
    on.exit(close(con))
    expect_warning(seasonder_readCSField(con, "UInt8"), "Error while reading value.")
  })

  it("should handle unknown types using seasonder_LogAndMessage", {
    con <- rawConnection(as.raw(c(0x12)))
    on.exit(close(con))
    expect_warning(seasonder_readCSField(con, "UnknownType"), "seasonder_readCSField: Type Unknown: 'UnknownType'.")

  })


})


describe("read_and_qc_field", {

  # Simulate the behavior of seasonder_readCSField
  mocked_seasonder_readCSField <- mockthat::mock(123)  # Returns 123 by default

  assign("qc_check_range", function(field_value, min, max) {
    if (is.null(field_value) || field_value < min || field_value > max) {
      return(NULL)
    }
    return(field_value)
  }, envir = seasonder_the)

  assign("qc_check_not_null", function(field_value) {
    if (is.null(field_value)) {
      return(-999)
    }
    return(field_value)
  }, envir = seasonder_the)


  it("reads a simple field and applies QC", {
    field_spec <- list(type = "double", qc_fun = "qc_check_range", qc_params = list(min = 100, max = 200))


    result <- mockthat::with_mock(
      read_and_qc_field(field_spec, connection = NULL),
      seasonder_readCSField = mocked_seasonder_readCSField
    )

    expect_equal(result, 123)  # Given our mock returns 123
  })

  it("returns NULL for values out of QC range", {

      field_spec <- list(type = "double", qc_fun = "qc_check_range", qc_params = list(min = 124, max = 200))


    result <- mockthat::with_mock(
      read_and_qc_field(field_spec, connection = NULL),
      seasonder_readCSField = mocked_seasonder_readCSField
    )

    expect_null(result)  # Value 123 is outside the allowed range
  })

  it("returns default for NULL values using QC", {
    # Changing our mock to return NULL
    mocked_seasonder_readCSField <- mockthat::mock(NULL)

    field_spec <-  list(type = "double", qc_fun = "qc_check_not_null", qc_params = list())


    result <- mockthat::with_mock(
      read_and_qc_field(field_spec, connection = NULL),
      seasonder_readCSField = mocked_seasonder_readCSField
    )

    expect_equal(result, -999)  # -999 is our default value in case of NULL
  })

})


describe("seasonder_readSeaSondeCSFileBlock", {

  it("returns a named list of values", {

    create_sequence_generator <- function(values) {
      i <- 1
      function() {
        value <- values[i]
        if (i < length(values)) {
          i <<- i + 1
        }
        value
      }
    }

    # Uso:
    seq_gen <- create_sequence_generator(c(123, 456))
    mocked_seasonder_readCSField <- mockthat::mock(seq_gen())

    spec <- list(
      temp = list(type = "double", qc_fun = "qc_check_range", qc_params = list(min = 100, max = 200)),
      pressure = list(type = "double", qc_fun = "qc_check_not_null", qc_params = list())
    )

    result <- mockthat::with_mock(
      seasonder_readSeaSondeCSFileBlock(spec, connection = NULL),
      read_and_qc_field = mocked_seasonder_readCSField  # Mock returns to simulate two field values
    )

    expect_true(is.list(result))
    expect_equal(length(result), 2)
    expect_equal(names(result), c("temp", "pressure"))
    expect_equal(result$temp, 123)
    expect_equal(result$pressure, 456)
  })

})









describe("seasonder_readSeaSondeCSFileHeaderV1 works as expected", {

  # Define your mock for seasonder_readSeaSondeCSFileBlock
  mocked_block_function <- function(spec, connection, endian="big") {
    # Here, you can return any predefined value to simulate the function's behavior.
    list(
      nCsFileVersion = 10,
      nDateTime = as.integer(Sys.Date()), # convert today's date to integer
      nV1Extent = 1000
    )
  }

  mockthat::with_mock(
    seasonder_readSeaSondeCSFileBlock = mocked_block_function,

    {
      # Mocked specs based on YAML
      specs <- list(
        nCsFileVersion = list(type = "SInt16", qc_fun = "qc_check_range", qc_params = list(min = 1, max = 32, expected_type = "integer")),
        nDateTime = list(type = "UInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer")),
        nV1Extent = list(type = "SInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer"))
      )

      describe("Function seasonder_readSeaSondeCSFileHeaderV1", {
        it("returns correct date-time transformation", {
          results <- seasonder_readSeaSondeCSFileHeaderV1(specs, NULL)

          # Check if nDateTime is transformed to POSIXct correctly
          expect_s3_class(results$nDateTime, "POSIXct")
        })

        it("returns expected mocked values", {
          results <- seasonder_readSeaSondeCSFileHeaderV1(specs, NULL)
          expect_equal(results$nCsFileVersion, 10)
          expect_equal(results$nV1Extent, 1000)
        })
      })
    }
  )
})

#### QC ####

describe("QC functions work as expected", {

  mocked_abort_function <- function(message) {
    stop(message)
  }

  mockthat::with_mock(
    seasonder_logAndAbort = mocked_abort_function,

    {
      describe("Function qc_check_type", {


        it("returns value when type matches", {
          result <- qc_check_type(100L, "integer")
          expect_equal(result, 100L)
        })

        it("throws error when type doesn't match", {
          expect_error(qc_check_type("100", "integer"), "QC Error: Value does not have the expected type: integer")
        })

      })

      describe("Function qc_check_range", {


        it("returns value when within range and type matches", {

          result <- qc_check_range(15L, 1, 32,"integer")

          expect_equal(result, 15)
        })

        it("throws error when value is out of range", {
          expect_error(qc_check_range(40L, 1, 32, "integer"), "QC Error: Value out of range. Expected between 1 and 32")
        })

        it("throws error when type doesn't match", {
          expect_error(qc_check_range("15", 1, 32, "integer"), "QC Error: Value does not have the expected type: integer")
        })

        it("works without expected_type argument", {
          result <- qc_check_range(15L, 1, 32)
          expect_equal(result, 15)
        })

      })

    }
  )
})

