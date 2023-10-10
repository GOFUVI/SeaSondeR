suppressMessages(here::i_am("tests/testthat/test-SeaSondeCS.R"))

test_that("Related functions are defined",{

  expect_true(is.function(seasonder_raw_to_int))
  expect_true(is.function(seasonder_readCSField))
  expect_true(is.function(seasonder_readSeaSondeCSFileBlock))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeader))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeaderV1))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeaderV2))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeaderV3))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeaderV4))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeaderV5))
  expect_true(is.function(seasonder_readSeaSondeCSFileHeaderV6))

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


  it("should read CharN types correctly", {
    # Create a test for Char4
    con <- rawConnection(charToRaw("Test"))  # Create a raw connection with the string "Test"
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "Char4"), "Test")
    close(con)
    # Another example for Char5
    con <- rawConnection(charToRaw("Hello"))  # Create a raw connection with the string "Hello"
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "Char5"), "Hello")
  })

  it("should read CharN types with special characters", {
    # Testing Char5 with space and special characters
    con <- rawConnection(charToRaw("T st!"))  # Create a raw connection with the string "T st!"
    on.exit(close(con))
    expect_equal(seasonder_readCSField(con, "Char5"), "T st!")
  })

})


describe("read_and_qc_field", {

  # Simulate the behavior of seasonder_readCSField
  mocked_seasonder_readCSField <- mockthat::mock(123)  # Returns 123 by default

  seasonder_the$qc_functions[["qc_check_range"]]<- function(field_value, min, max) {
    if (is.null(field_value) || field_value < min || field_value > max) {
      return(NULL)
    }
    return(field_value)
  }

  seasonder_the$qc_functions[["qc_check_not_null"]]<-  function(field_value) {
    if (is.null(field_value)) {
      return(-999)
    }
    return(field_value)
  }


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

  seasonder_load_qc_functions()

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





describe("seasonder_readSeaSondeCSFileHeader", {

  # Test for version 3
  it("processes the headers correctly when file version is 3", {

    # Mocked headers
    mock_header_v1 <- mockthat::mock(
      list(
        nCsFileVersion = 3,  # Indicamos que es versión 3 para el test
        nDateTime = as.POSIXct("2023-09-23 10:00:00"),
        nV1Extent = 62
      )
    )

    mock_header_v2 <- mockthat::mock(
      list(
        nCsKind = 2,
        someField = "example"
      )
    )

    mock_header_v3 <- mockthat::mock(
      list(
        someField = "overwritten_example",  # This field will overwrite the previous value
        anotherField = 12345,
        yetAnotherField = TRUE
      )
    )

    # Empty specs for this test
    specs <- list(
      V1 = list(),
      V2 = list(),
      V3 = list()
    )

    # Execute with mocks
    result <- mockthat::with_mock(
      seasonder_readSeaSondeCSFileHeader(specs, NULL),
      seasonder_readSeaSondeCSFileHeaderV1 = mock_header_v1,
      seasonder_readSeaSondeCSFileHeaderV2 = mock_header_v2,
      seasonder_readSeaSondeCSFileHeaderV3 = mock_header_v3
    )

    # Check results
    expect_equal(result$nCsFileVersion, 3)
    expect_equal(result$nDateTime, as.POSIXct("2023-09-23 10:00:00"))
    expect_equal(result$someField, "overwritten_example")  # Check overwritten value
    expect_equal(result$anotherField, 12345)

    # Ensure helper functions are called
    expect_equal(mockthat::mock_n_called(mock_header_v1), 1)
    expect_equal(mockthat::mock_n_called(mock_header_v2), 1)
    expect_equal(mockthat::mock_n_called(mock_header_v3), 1)

  })

  # Test for version 2, to ensure no unexpected calls to non-existent version functions
  it("processes the headers correctly when file version is 2", {

    # Mocked headers
    mock_header_v1 <- mockthat::mock(
      list(
        nCsFileVersion = 2,  # Indicate that it's version 2 for this test
        nDateTime = as.POSIXct("2023-09-23 10:00:00"),
        nV1Extent = 62
      )
    )

    mock_header_v2 <- mockthat::mock(
      list(
        nCsKind = 2,
        someField = "example"
      )
    )

    # Empty specs for this test
    specs <- list(
      V1 = list(),
      V2 = list(),
      V3 = list()
    )

    # Execute with mocks
    result <- mockthat::with_mock(
      seasonder_readSeaSondeCSFileHeader(specs, NULL),
      seasonder_readSeaSondeCSFileHeaderV1 = mock_header_v1,
      seasonder_readSeaSondeCSFileHeaderV2 = mock_header_v2
      # Note: No V3 mock here as it shouldn't be called for file version 2
    )

    # Check results
    expect_equal(result$nCsFileVersion, 2)
    expect_equal(result$nDateTime, as.POSIXct("2023-09-23 10:00:00"))
    expect_equal(result$someField, "example")

    # Ensure helper functions are called
    expect_equal(mockthat::mock_n_called(mock_header_v1), 1)
    expect_equal(mockthat::mock_n_called(mock_header_v2), 1)

  })

})




describe("seasonder_readSeaSondeCSFileHeaderV1 works as expected", {

  # Define your mock for seasonder_readSeaSondeCSFileBlock
  mocked_block_function <- function(spec, connection, endian="big") {
    # Here, you can return any predefined value to simulate the function's behavior.
    list(
      nCsFileVersion = 10L,
      nDateTime = as.integer(Sys.Date()), # convert today's date to integer
      nV1Extent = 1000L
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

describe("seasonder_readSeaSondeCSFileHeaderV2 works as expected", {

  # Define your mock for seasonder_readSeaSondeCSFileBlock
  mocked_block_function <- function(spec, connection, endian="big") {
    # Return predefined values to simulate the function's behavior.
    list(
      nCsKind = 1L,
      nV2Extent = 1000L
    )
  }

  mockthat::with_mock(
    seasonder_readSeaSondeCSFileBlock = mocked_block_function,
    {
      # Mocked specs based on YAML
      specs <- list(
        nCsKind = list(type = "SInt16", qc_fun = "qc_check_range", qc_params = list(min = 1, max = 2, expected_type = "integer")),
        nV2Extent = list(type = "SInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer"))
      )

      describe("Function seasonder_readSeaSondeCSFileHeaderV2", {

        it("returns expected mocked values for nCsKind and nV2Extent", {
          results <- seasonder_readSeaSondeCSFileHeaderV2(specs, NULL)

          # Check if nCsKind and nV2Extent have expected mocked values
          expect_equal(results$nCsKind, 1)
          expect_equal(results$nV2Extent, 1000)
        })

        it("ensures that the type of nCsKind is an integer", {
          results <- seasonder_readSeaSondeCSFileHeaderV2(specs, NULL)
          expect_type(results$nCsKind, "integer")
        })

        it("ensures that the type of nV2Extent is an integer", {
          results <- seasonder_readSeaSondeCSFileHeaderV2(specs, NULL)
          expect_type(results$nV2Extent, "integer")
        })

      })
    }
  )
})

describe("seasonder_readSeaSondeCSFileHeaderV3 works as expected", {

  # Define your mock for seasonder_readSeaSondeCSFileBlock
  mocked_block_function <- function(spec, connection, endian="big") {
    # Here, you can return any predefined value to simulate the function's behavior.
    list(
      nSiteCodeName = "TEST",
      nV3Extent = 1500L
    )
  }

  mockthat::with_mock(
    seasonder_readSeaSondeCSFileBlock = mocked_block_function,

    {
      # Mocked specs based on YAML
      specs <- list(
        nSiteCodeName = list(type = "Char4", qc_fun = "qc_check_type", qc_params = list(expected_type = "character")),
        nV3Extent = list(type = "SInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer"))
      )

      describe("Function seasonder_readSeaSondeCSFileHeaderV3", {

        it("returns expected mocked values", {
          results <- seasonder_readSeaSondeCSFileHeaderV3(specs, NULL)

          # Check if nSiteCodeName and nV3Extent return expected values
          expect_equal(results$nSiteCodeName, "TEST")
          expect_equal(results$nV3Extent, 1500)
        })

        it("returns hardcoded values for nRangeCells, nDopplerCells, and nFirstRangeCell", {
          results <- seasonder_readSeaSondeCSFileHeaderV3(specs, NULL)

          # Check hardcoded values
          expect_equal(results$nRangeCells, 31)
          expect_equal(results$nDopplerCells, 512)
          expect_equal(results$nFirstRangeCell, 1)
        })
      })
    }
  )
})

describe("seasonder_readSeaSondeCSFileHeaderV4 works as expected", {

  # Define your mock for seasonder_readSeaSondeCSFileBlock
  mocked_block_function <- function(spec, connection, endian="big") {
    # Here, you can return any predefined value to simulate the function's behavior.
    list(
      nCoverMinutes = 15L,
      bDeletedSource = 1L,
      bOverrideSrcInfo = 0L,
      fStartFreqMHz = 100.5,
      fRepFreqHz = 1000.0,
      fBandwidthKHz = 25.5,
      bSweepUp = 0L,
      nDopplerCells = 512L,
      nRangeCells = 31L,
      nFirstRangeCell = 1L,
      fRangeCellDistKm = 0.5,
      nV4Extent = 0L
    )
  }

  mockthat::with_mock(
    seasonder_readSeaSondeCSFileBlock = mocked_block_function,

    {
      # Mocked specs based on YAML
      specs <- list(
        nCoverMinutes = list(type = "SInt32", qc_fun = "qc_check_range", qc_params = list(min = 0, max = 60, expected_type = "integer")),
        bDeletedSource = list(type = "SInt32", qc_fun = "qc_check_range", qc_params = list(min = 0, max = 1, expected_type = "integer")),
        bOverrideSrcInfo = list(type = "SInt32", qc_fun = "qc_check_range", qc_params = list(min = 0, max = 1, expected_type = "integer")),
        fStartFreqMHz = list(type = "Float", qc_fun = "qc_check_type", qc_params = list(expected_type = "numeric")),
        fRepFreqHz = list(type = "Float", qc_fun = "qc_check_type", qc_params = list(expected_type = "numeric")),
        fBandwidthKHz = list(type = "Float", qc_fun = "qc_check_type", qc_params = list(expected_type = "numeric")),
        bSweepUp = list(type = "SInt32", qc_fun = "qc_check_range", qc_params = list(min = 0, max = 1, expected_type = "integer")),
        nDopplerCells = list(type = "SInt32", qc_fun = "qc_check_range", qc_params = list(min = 0, max = 1024, expected_type = "integer")), # assuming a reasonable max for illustration
        nRangeCells = list(type = "SInt32", qc_fun = "qc_check_range", qc_params = list(min = 0, max = 32, expected_type = "integer")), # max is 32 based on provided info
        nFirstRangeCell = list(type = "SInt32", qc_fun = "qc_check_range", qc_params = list(min = -10, max = 32, expected_type = "integer")), # min is -10 just for illustration, adjust as necessary
        fRangeCellDistKm = list(type = "Float", qc_fun = "qc_check_type", qc_params = list(expected_type = "numeric")),
        nV4Extent = list(type = "SInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer"))
      )


      describe("Function seasonder_readSeaSondeCSFileHeaderV4", {
        it("returns CenterFreq correctly", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$CenterFreq, 75) # This is based on mocked values.
        })

        it("returns expected mocked values for nCoverMinutes and nDopplerCells", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$nCoverMinutes, 15)
          expect_equal(results$nDopplerCells, 512)
        })

        it("returns expected mocked value for bDeletedSource", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$bDeletedSource, 1)
        })

        it("returns expected mocked value for bOverrideSrcInfo", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$bOverrideSrcInfo, 0)
        })

        it("returns expected mocked values for fStartFreqMHz and fRepFreqHz", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$fStartFreqMHz, 100.5)
          expect_equal(results$fRepFreqHz, 1000.0)
        })

        it("returns expected mocked value for fBandwidthKHz", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$fBandwidthKHz, 25.5)
        })

        it("returns expected mocked value for bSweepUp", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$bSweepUp, 0)
        })

        it("returns expected mocked values for nRangeCells and nFirstRangeCell", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$nRangeCells, 31)
          expect_equal(results$nFirstRangeCell, 1)
        })

        it("returns expected mocked value for fRangeCellDistKm", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$fRangeCellDistKm, 0.5)
        })

        it("returns expected mocked value for nV4Extent", {
          results <- seasonder_readSeaSondeCSFileHeaderV4(specs, NULL)
          expect_equal(results$nV4Extent, 0)
        })

      })
    }
  )
})


describe("seasonder_readSeaSondeCSFileHeaderV5 works as expected", {

  # Mock for the seasonder_readSeaSondeCSFileBlock function
  mocked_block_function_v5 <- function(spec, connection, endian="big") {
    # Here, you can return any predefined value to simulate the function's behavior.
    list(
      nOutputInterval = 10L,
      nCreateTypeCode = 1L,
      nCreatorVersion = 3L,
      nActiveChannels = 4L,
      nSpectraChannels = 10L,
      nActiveChanBits = bitwOr(1, bitwShiftL(1, 3)), # setting bits for 1st and 4th channels
      nV5Extent = 5000L
    )
  }

  mockthat::with_mock(
    seasonder_readSeaSondeCSFileBlock = mocked_block_function_v5,

    {
      # Mocked specs based on your function's requirements
      specs <- list(
        nOutputInterval = list(type = "SInt16", qc_fun = "qc_check_range", qc_params = list(min = 1, max = 32, expected_type = "integer")),
        nCreateTypeCode = list(type = "UInt8", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer")),
        nCreatorVersion = list(type = "UInt8", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer")),
        nActiveChannels = list(type = "SInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer")),
        nSpectraChannels = list(type = "SInt16", qc_fun = "qc_check_range", qc_params = list(min = 1, max = 16, expected_type = "integer")),
        nActiveChanBits = list(type = "UInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer")),
        nV5Extent = list(type = "SInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer"))
      )

      describe("Function seasonder_readSeaSondeCSFileHeaderV5", {
        it("returns the correct transformation for ActiveChannels", {
          results <- seasonder_readSeaSondeCSFileHeaderV5(specs, NULL)

          # Check if nActiveChanBits is transformed to ActiveChannels correctly
          expect_equal(results$ActiveChannels, c(1, 4))
        })

        it("returns expected mocked values", {
          results <- seasonder_readSeaSondeCSFileHeaderV5(specs, NULL)
          expect_equal(results$nOutputInterval, 10)
          expect_equal(results$nCreateTypeCode, 1)
          expect_equal(results$nCreatorVersion, 3)
          expect_equal(results$nActiveChannels, 4)
          expect_equal(results$nSpectraChannels, 10)
          expect_equal(results$nV5Extent, 5000)
        })
      })
    }
  )
})


describe("readV6BlockData works as expected", {


  mocked_regular_block_function <- function(spec, connection, endian="big") {

    list(
      Var1=1L,
      Var2=2L
    )
  }


  mock_factory <- function(n){


    .n <- n

    .regular_block <- TRUE


    function(spec, connection, endian, prev_data) {

      if(.regular_block){
        out <- list(Var1=1L,
                    Var2=2L)
        .regular_block <<- FALSE
      }else{
        out <- list(
          Var4 = paste0("sampleValueVar4_",.n),
          Var5 = paste0("sampleValueVar5_",.n))
        .n <<- .n -1
      }


      out
    }



  }





  reg_specs <-list(
    Var1 = list(type = "varType1", qc_fun = "qc_fun_name1", qc_params = list(param1 = "value1")),
    Var2 = list(type = "varType2", qc_fun = "qc_fun_name2", qc_params = list(param1 = "value2"))


  )



  rep_specs <-list(
    Var1 = list(type = "varType1", qc_fun = "qc_fun_name1", qc_params = list(param1 = "value1")),
    Var2 = list(type = "varType2", qc_fun = "qc_fun_name2", qc_params = list(param1 = "value2")),
    "repeat"=list(how_many=c("L1","L2"),what=list(
      Var3 = list(type = "varType3", qc_fun = "qc_fun_name3", qc_params = list(param1 = "value3")),
      Var4 = list(type = "varType4", qc_fun = "qc_fun_name4", qc_params = list(param1 = "value4"))
    ))


  )

  mock_prev_data <- list(L1=2,L2=3)


  it("reads regular blocks correctly", {

    results <- mockthat::with_mock(
      seasonder_readSeaSondeCSFileBlock = mocked_regular_block_function,

      {
        readV6BlockData(reg_specs, NULL,prev_data=mock_prev_data)


      })

    expect_equal(results$Var1,1L)
    expect_equal(results$Var2,2L)
  })

  it("reads repeated blocks correctly", {

    mocked_repeated_block_function <- mock_factory(6)

    results <- mockthat::with_mock(
      seasonder_readSeaSondeCSFileBlock = mocked_repeated_block_function,

      {
        readV6BlockData(rep_specs, NULL,prev_data=mock_prev_data)


      })

    expect_equal(results$Var1,1L)
    expect_equal(results$Var2,2L)

    targetV4 <- list(loop="L1",
                     data=list(
                       list(loop="L2",
                            data=c("sampleValueVar4_6","sampleValueVar4_5","sampleValueVar4_4")
                       ),
                       list(loop="L2",
                            data=c("sampleValueVar4_3","sampleValueVar4_2","sampleValueVar4_1")
                       )

                     )
    )

    expect_equal(results$Var4,targetV4)

    targetV5 <- list(loop="L1",
                     data=list(
                       list(loop="L2",
                            data=c("sampleValueVar5_6","sampleValueVar5_5","sampleValueVar5_4")
                       ),
                       list(loop="L2",
                            data=c("sampleValueVar5_3","sampleValueVar5_2","sampleValueVar5_1")
                       )

                     )
    )


    expect_equal(results$Var5,targetV5)
  })

})


describe("seasonder_readSeaSondeCSFileHeaderV6 works as expected", {

  mock_factory <- function(){


    .outputs <- list(list(nCS6ByteSize=130L),
                     list(
                       nBlockKey = "REGB",
                       nBlockDataSize = 52L
                     ),
                     list(
                       nBlockKey = "UNKW",
                       nBlockDataSize = 32L
                     ),
                     list(
                       nBlockKey = "BRGR",
                       nBlockDataSize = 22L
                     )
    )

    .i <- 1


    function(spec, connection, endian, prev_data) {

      out <- .outputs[[.i]]
      .i <<- .i +1

      out
    }



  }



  # Define your mock for readV6BlockData
  mocked_block_data_function_factory <- function(){

    .outputs <- list(
      list(
        Var1 = "TestValue1",
        Var2 = "TestValue2"
      ),
      list(nBraggReject=list(loop="L1",data=c(0L,1L,2L,3L)))
    )

    .i <- 1

    function(block_specs, connection, endian="big", prev_data = list()) {

      out <- .outputs[[.i]]
      .i <<- .i +1

      out
    }
  }


  # Mocked specs based on YAML
  specs <- list(
    nCS6ByteSize = list(type = "UInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer")),
    block_spec = list(
      nBlockKey = list(type = "Char4", qc_fun = "qc_check_type", qc_params = list(expected_type = "character")),
      nBlockDataSize = list(type = "UInt32", qc_fun = "qc_check_type", qc_params = list(expected_type = "integer"))
    ),
    blocks = list(
      REGB = list(
        Var1 = list(type = "varType", qc_fun = "qc_fun_name", qc_params = list(param1 = "value1")),
        Var2 = list(type = "varType", qc_fun = "qc_fun_name", qc_params = list(param1 = "value"))
      ),
      BRGR = list("repeat"=list(how_many="L1",what=list(nBraggReject=list(type = "varType", qc_fun = "qc_fun_name", qc_params = list(param1 = "value1")))))
    )
  )

  it("returns correct block key", {
    # Define your mock for seasonder_readSeaSondeCSFileBlock

    con <- rawConnection(openssl::rand_bytes(100))
    on.exit(close(con))
    mocked_block_function <- mock_factory()
    mocked_block_data_function <- mocked_block_data_function_factory()
    results <- mockthat::with_mock(
      seasonder_readSeaSondeCSFileBlock = mocked_block_function,
      readV6BlockData = mocked_block_data_function,

      {
        seasonder_readSeaSondeCSFileHeaderV6(specs, con)
      })
    expect_true(!is.null(results$REGB))
    expect_equal(results$REGB$Var1, "TestValue1")
    expect_equal(results$REGB$Var2, "TestValue2")

    expect_null(results$UNKW)

    expect_equal(as.character(results$BRGR$nBraggReject$data),c("OK", "RejectNegBragg", "RejectPosBragg", "RejectBoth"))
  })



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




#### Real data tests ####


describe("CSS file",{


  describe("Read header",{

           specs <- seasonder_readYAMLSpecs(here::here("inst/specs/CS_V1.yaml"),"header")

           it("should read the header",{
              con <- file(here::here("tests/testthat/data/CSS_V6.cs"),"rb")
              on.exit(close(con))

              test <- expect_silent(seasonder_readSeaSondeCSFileHeader(specs,con))

              expect_snapshot_value(test,style="serialize")

           })

           })

})
