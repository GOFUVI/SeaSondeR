

suppressMessages(here::i_am("tests/testthat/test-SeaSondeRCS.R"))

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
  expect_true(is.function(seasonder_readSeaSondeCSFileData))
  expect_true(is.function(seasonder_readSeaSondeCSFile))

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

  describe("should read a Float correctly", {
    it("case 1",{
      # Float for 42.41 in IEEE single precision is 0x4229a3d7
      con <- rawConnection(as.raw(c(0x42, 0x29, 0xa3, 0xd7)))
      on.exit(close(con))
      expect_equal(seasonder_readCSField(con, "Float"), 42.41, tolerance = 0.0001)
    })

    it("case 2",{

      # Float for 42.41 in IEEE single precision is 0x4229a3d7
      con <- rawConnection(as.raw(c(0x42, 0x2a, 0x00, 0x00)))
      on.exit(close(con))
      expect_equal(seasonder_readCSField(con, "Float"), 42.500001, tolerance = 0.0001)

    })


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



  it("should throw a seasonder_cs_field_reading_error if connection is not open", {
    con <- rawConnection(as.raw(c(0x12)))
    close(con)  # Close the connection before using it
    expect_error(seasonder_readCSField(con, "UInt8"), "Connection is not open.",class = "seasonder_cs_field_reading_error")

  })

  it("should throw a seasonder_cs_field_reading_error if there is any problem while reading", {
    con <- textConnection("Test")
    on.exit(close(con))
    expect_error(seasonder_readCSField(con, "UInt8"), "Error while reading value.",class = "seasonder_cs_field_reading_error")
  })

  it("should throw a seasonder_cs_field_reading_error for unknown types", {
    con <- rawConnection(as.raw(c(0x12)))
    on.exit(close(con))
    expect_error(seasonder_readCSField(con, "UnknownType"), "seasonder_readCSField: Type Unknown: 'UnknownType'.",class = "seasonder_cs_field_reading_error")

  })

  it("should throw a seasonder_cs_field_reading_error when reaching the end of the file", {
    con <- rawConnection(as.raw(c(0x12)))
    on.exit(close(con))
    seasonder_readCSField(con, "UInt8")
    expect_error(seasonder_readCSField(con, "UInt8"), "Read value of length 0. Possibly reached end of file.",class = "seasonder_cs_field_reading_error")

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

  it("should provide a seasonder_skip_cs_field restart that returns a given value",{

    con <- rawConnection(as.raw(c(0x12)))
    close(con)  # Close the connection before using it
    expect_warning({

      res <- withCallingHandlers(seasonder_cs_field_reading_error = function(cond)  seasonder_skip_cs_field(cond,NA),
                                 seasonder_readCSField(con, "UInt8")
      )
    }, "Connection is not open.")

    expect_true(is.na(res))

  })

  it("should provide a seasonder_skip_cs_field restart that returns a given value",{

    con <- rawConnection(as.raw(c(0x12)))
    on.exit(close(con))
    seasonder_readCSField(con, "UInt8")
    expect_warning({

      res <- withCallingHandlers(seasonder_cs_field_reading_error= function(cond)  seasonder_skip_cs_field(cond,NA),
                                 seasonder_readCSField(con, "UInt8")
      )
    }, "Read value of length 0. Possibly reached end of file.")

    expect_true(is.na(res))

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

  describe("Error handling",{

    describe("when a seasonder_cs_field_skipped condition is raised",{
      it("should rethrown it and skip the QC",{


        qc_called <- FALSE


        seasonder_the$qc_functions$qc_check_not_null<-function(...){
          qc_called <<-TRUE
        }



        field_spec <-  list(type = "UInt8", qc_fun = "qc_check_not_null", qc_params = list())


        expect_condition({

          result <-   withCallingHandlers(seasonder_cs_field_reading_error= function(cond){

            seasonder_skip_cs_field(cond,NA)

          },

          read_and_qc_field(field_spec, connection = con)
          )},class = "seasonder_cs_field_skipped")

        expect_false(qc_called)
        expect_true(is.na(result))



      })
    })
    describe("When a qc function is not found",{
      it("should thrown an seasonder_cs_field_qc_fun_error error",{
        con <- rawConnection(as.raw(c(0x12)))
        on.exit(close(con))
        field_spec <-  list(type = "UInt8", qc_fun = "qc_error", qc_params = list())
        mocked_seasonder_readCSField <- mockthat::mock(123)

        seasonder_the$qc_functions$qc_error<-function(...){

          rlang::abort("QC Error")
        }


        mockthat::with_mock(seasonder_readCSField=mocked_seasonder_readCSField,
                            expect_error(read_and_qc_field(field_spec, connection = con ),"An issue happened while applying QC function 'qc_error'.",class = "seasonder_cs_field_qc_fun_error"))

      })
    })

    describe("When a qc function throws an error",{
      it("should thrown an seasonder_cs_field_qc_fun_not_defined_error error",{
        con <- rawConnection(as.raw(c(0x12)))
        on.exit(close(con))
        field_spec <-  list(type = "UInt8", qc_fun = "qc_not existing", qc_params = list())
        mocked_seasonder_readCSField <- mockthat::mock(123)

        mockthat::with_mock(seasonder_readCSField=mocked_seasonder_readCSField,
                            expect_error(read_and_qc_field(field_spec, connection = con ),"QC function 'qc_not existing' not defined.",class = "seasonder_cs_field_qc_fun_not_defined_error"))

      })
    })


    describe("seasonder_rerun_qc_with_fun restart",{

      it("should apply the new qc function to the value and throw a seasonder_cs_field_qc_fun_rerun condition",{
skip("Fails on checks, but passes interactively")
        con <- rawConnection(as.raw(c(0x12)))
        on.exit(close(con))
        field_spec <-  list(type = "UInt8", qc_fun = "qc_error", qc_params = list())
        mocked_seasonder_readCSField <- mockthat::mock(123)

        seasonder_the$qc_functions$qc_error <- function(...){

          rlang::abort("QC Error")
        }
        alternate_qc_called <- FALSE

        alternate_qc <- function(x){
          alternate_qc_called <<- TRUE
          6
        }

        expect_condition({
          result <- withCallingHandlers(seasonder_cs_field_qc_fun_error=function(cond) seasonder_rerun_qc_with_fun(cond,alternate_qc),
                                        mockthat::with_mock(seasonder_readCSField=mocked_seasonder_readCSField,
                                                            read_and_qc_field(field_spec, connection = con ))
          )
        },"Rerunning QC on value 123.",class = "seasonder_cs_field_qc_fun_rerun")
        expect_equal(result,6)

      })
    })
    seasonder_load_qc_functions()

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

  describe("when a field is skipped",{

    it("should throw a warning and return the value",{


      con <- rawConnection(as.raw(c(0x12)))
      on.exit(close(con))
      spec <- list(
        temp = list(type = "UInt8", qc_fun = "qc_check_type", qc_params = list(expected_type="integer")),
        pressure = list(type = "UInt8", qc_fun = "qc_check_type", qc_params = list(expected_type="integer"))
      )

      expect_warning(

        result <- withCallingHandlers(seasonder_cs_field_reading_error= function(cond){

          seasonder_skip_cs_field(cond,NA)

        },
        seasonder_readSeaSondeCSFileBlock(spec, connection = con)
        ),"Field pressure: seasonder_readCSField: Error while reading value: Read value of length 0. Possibly reached end of file.")


      expect_equal(result,list(temp=18,pressure=NA))



    })

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

  describe("when there is an error in one of the headers",{
    it("should rethrown the error indicating which header was responsible",{

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



        # Empty specs for this test
        specs <- list(
          V1 = list(),
          V2 = list(),
          V3 = list()
        )

        expect_error( # Execute with mocks
          result <- mockthat::with_mock(
            seasonder_readSeaSondeCSFileHeader(specs, NULL),
            seasonder_readSeaSondeCSFileHeaderV1 = mock_header_v1,
            seasonder_readSeaSondeCSFileHeaderV2 = mock_header_v2
          ),"Header version 2",class="spsr_field_specification_missing_error")



      })

    })
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


  mock_block_header_outputs <- list(list(nCS6ByteSize=130L),
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
                                    ))


  mock_block_data_outputs <- list(
    list(
      Var1 = "TestValue1",
      Var2 = "TestValue2"
    ),
    list(nBraggReject=list(loop="L1",data=c(0L,1L,2L,3L)))
  )





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
      BRGR = list("repeat" = list(how_many = "L1",what = list(nBraggReject = list(type = "varType", qc_fun = "qc_fun_name", qc_params = list(param1 = "value1")))))
    )
  )

  it("returns correct block key", {
    # Define your mock for seasonder_readSeaSondeCSFileBlock

    con <- rawConnection(openssl::rand_bytes(100))
    on.exit(close(con))
    mocked_block_function <- mock_output_factory(!!!mock_block_header_outputs)
    mocked_block_data_function <- mock_output_factory(!!!mock_block_data_outputs)
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

  describe("when a transform function results in an error",{

    it("raises a seasonder_v6_transform_function_error",{


      seasonder_the$transform_functions[["REGB"]] <- function(x) {
        rlang::abort("Transform error")
      }


      con <- rawConnection(openssl::rand_bytes(100))
      on.exit(close(con))
      mocked_block_function <- mock_output_factory(!!!mock_block_header_outputs)
      mocked_block_data_function <- mock_output_factory(!!!mock_block_data_outputs)
      err <- expect_error({
        results <- mockthat::with_mock(
          seasonder_readSeaSondeCSFileBlock = mocked_block_function,
          readV6BlockData = mocked_block_data_function,

          {
            seasonder_readSeaSondeCSFileHeaderV6(specs, con)
          })

      },"Error while applying transform function for V6 header block 'REGB': Transform error",class = "seasonder_v6_transform_function_error")
      expect_equal(err$seasonder_block_data,list(
        Var1 = "TestValue1",
        Var2 = "TestValue2"
      ))
    })

    it("provides a seasonder_v6_skip_transformation restart",{


      seasonder_the$transform_functions[["REGB"]] <- function(x) {
        rlang::abort("Transform error")
      }


      con <- rawConnection(openssl::rand_bytes(100))
      on.exit(close(con))
      mocked_block_function <- mock_output_factory(!!!mock_block_header_outputs)
      mocked_block_data_function <- mock_output_factory(!!!mock_block_data_outputs)
      expect_condition({


        results <- withCallingHandlers(seasonder_v6_transform_function_error = function(cond){

          val <- cond$seasonder_block_data

          seasonder_v6_skip_transformation(cond,list(other_var = "alternative value"))
        },
        {
          mockthat::with_mock(
            seasonder_readSeaSondeCSFileBlock = mocked_block_function,
            readV6BlockData = mocked_block_data_function,

            {
              seasonder_readSeaSondeCSFileHeaderV6(specs, con)
            })

        })
      },"Skipping transformation for block 'REGB'",class = "seasonder_v6_block_transformacion_skipped")

      expect_equal(results$REGB,list(other_var = "alternative value"))
    })

  })

  describe("when a block is not recognized and there is a problem skipping it",{

    it("raises a seasonder_v6_skip_block_error",{


      seasonder_the$transform_functions[["REGB"]] <- function(x) {
        close(con)
      }


      con <- rawConnection(openssl::rand_bytes(100))

      mocked_block_function <- mock_output_factory(!!!mock_block_header_outputs)
      mocked_block_data_function <- mock_output_factory(!!!mock_block_data_outputs)
      expect_error({
        results <- mockthat::with_mock(
          seasonder_readSeaSondeCSFileBlock = mocked_block_function,
          readV6BlockData = mocked_block_data_function,

          {
            seasonder_readSeaSondeCSFileHeaderV6(specs, con)
          })

      },"Error while skipping block",class = "seasonder_v6_skip_block_error")

    })

  })

})

describe("seasonder_validateCSFileData",{
  describe("file size <=10",{
    it("should throw an error",{

      header_mock <- list("fake_header_data")

      tmp <- tempfile()
      writeBin(openssl::rand_bytes(10),tmp)

      expect_error(seasonder_validateCSFileData(tmp, header_mock),
                   "Invalid file size",
                   class="seasonder_validate_cs_file_error"
      )
    })
  })
  describe("wrong CS file version",{
    it("should throw an error",{

      header_mock <- list(nCsFileVersion=0)

      tmp <- tempfile()
      writeBin(openssl::rand_bytes(11),tmp)

      expect_error(seasonder_validateCSFileData(tmp, header_mock),
                   "Invalid nCsFileVersion",
                   class="seasonder_validate_cs_file_error"
      )
    })
  })

  describe("invalid CS file version 1",{
    describe("case 0: correct",{
      it("should not throw an error",{


        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 1,
                            nV1Extent = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769482),tmp)

        expect_no_error(seasonder_validateCSFileData(tmp, header_mock))
      })
    })
    describe("case 1: wrong v1 extent",{
      it("should throw an error",{


        header_mock <- list(nCsFileVersion = 1,
                            nV1Extent = -1)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(11),tmp)

        expect_error(
          seasonder_validateCSFileData(tmp, header_mock),
          "Invalid file for version 1",
          class="seasonder_validate_cs_file_error"
        )
      })
    })

  })

  describe("invalid CS file version 2",{
    describe("case 0: correct",{
      it("should not throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 2,
                            nV1Extent = 6,
                            nV2Extent = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769488),tmp)

        expect_no_error(seasonder_validateCSFileData(tmp, header_mock))
      })
    })
    describe("case 1: wrong file size",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nCsFileVersion = 2,
                            nV1Extent = 6,
                            nV2Extent = 0)
        tmp <- tempfile()
        writeBin(openssl::rand_bytes(16),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 2",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
    describe("case 2: wrong v1 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nCsFileVersion = 2,
                            nV1Extent = 5,
                            nV2Extent = 0)
        tmp <- tempfile()
        writeBin(openssl::rand_bytes(17),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 2",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
    describe("case 3: wrong v2 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nCsFileVersion = 2,
                            nV1Extent = 6,
                            nV2Extent = -1)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(17),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 2",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
  })

  describe("invalid CS file version 3",{
    describe("case 0: correct",{
      it("should not throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,

                            nCsFileVersion = 3,
                            nV1Extent = 14,
                            nV2Extent = 8,
                            nV3Extent = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769496),tmp)

        expect_no_error(seasonder_validateCSFileData(tmp, header_mock))
      })
    })
    describe("case 1: wrong file size",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nCsFileVersion = 3,
                            nV1Extent = 14,
                            nV2Extent = 8,
                            nV3Extent = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(24),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 3",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
    describe("case 2: wrong v1 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nCsFileVersion = 3,
                            nV1Extent = 13,
                            nV2Extent = 8,
                            nV3Extent = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(25),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 3",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
    describe("case 3: wrong v2 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nCsFileVersion = 3,
                            nV1Extent = 14,
                            nV2Extent = 7,
                            nV3Extent = 0)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(25),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 3",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })

    describe("case 4: wrong v3 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nCsFileVersion = 3,
                            nV1Extent = 14,
                            nV2Extent = 8,
                            nV3Extent = -1)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(25),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 3",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
  })

  describe("invalid CS file version 4",{
    describe("case 0: correct",{
      it("should not throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 4,
                            nV1Extent = 62,
                            nV2Extent = 56,
                            nV3Extent = 48,
                            nV4Extent = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769544),tmp)

        expect_no_error(seasonder_validateCSFileData(tmp, header_mock))
      })
    })
    describe("case 1: wrong file size",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nCsFileVersion = 4,
                            nV1Extent = 62,
                            nV2Extent = 56,
                            nV3Extent = 48,
                            nV4Extent = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(72),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 4",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
    describe("case 2: wrong v1 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nCsFileVersion = 4,
                            nV1Extent = 61,
                            nV2Extent = 56,
                            nV3Extent = 48,
                            nV4Extent = 0)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(73),tmp)

        expect_error(
          seasonder_validateCSFileData(tmp, header_mock),
          "Invalid file for version 4",
          class="seasonder_validate_cs_file_error"
        )
      })
    })
    describe("case 3: wrong v2 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nCsFileVersion = 4,
                            nV1Extent = 62,
                            nV2Extent = 55,
                            nV3Extent = 48,
                            nV4Extent = 0)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(73),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 4",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })

    describe("case 4: wrong v3 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nCsFileVersion = 4,
                            nV1Extent = 62,
                            nV2Extent = 56,
                            nV3Extent = 47,
                            nV4Extent = 0)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(73),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 4",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })


    describe("case 5: wrong v4 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nCsFileVersion = 4,
                            nV1Extent = 62,
                            nV2Extent = 56,
                            nV3Extent = 48,
                            nV4Extent = -1)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(73),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version 4",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
  })

  describe("invalid CS file version 5",{
    describe("case 0: correct",{
      it("should not throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 5,
                            nV1Extent = 90,
                            nV2Extent = 84,
                            nV3Extent = 76,
                            nV4Extent = 28,
                            nV5Extent = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769572),tmp)

        expect_no_error(seasonder_validateCSFileData(tmp, header_mock))
      })
    })
    describe("case 1: wrong file size",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 5,
                            nV1Extent = 90,
                            nV2Extent = 84,
                            nV3Extent = 76,
                            nV4Extent = 28,
                            nV5Extent = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(100),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version >= 5",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
    describe("case 2: wrong v1 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 5,
                            nV1Extent = 89,
                            nV2Extent = 84,
                            nV3Extent = 76,
                            nV4Extent = 28,
                            nV5Extent = 0)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(101),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version >= 5",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })
    describe("case 3: wrong v2 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 5,
                            nV1Extent = 90,
                            nV2Extent = 83,
                            nV3Extent = 76,
                            nV4Extent = 28,
                            nV5Extent = 0)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(101),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version >= 5",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })

    describe("case 4: wrong v3 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 5,
                            nV1Extent = 90,
                            nV2Extent = 84,
                            nV3Extent = 75,
                            nV4Extent = 28,
                            nV5Extent = 0)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(101),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version >= 5",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })


    describe("case 5: wrong v4 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 5,
                            nV1Extent = 90,
                            nV2Extent = 84,
                            nV3Extent = 76,
                            nV4Extent = 27,
                            nV5Extent = 0)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(101),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version >= 5",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })

    describe("case 6: wrong v5 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 5,
                            nV1Extent = 90,
                            nV2Extent = 84,
                            nV3Extent = 76,
                            nV4Extent = 28,
                            nV5Extent = -1)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(101),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version >= 5",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })

  })


  describe("invalid CS file version 6",{
    describe("case 0: correct",{
      it("should not throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769576),tmp)

        expect_no_error(seasonder_validateCSFileData(tmp, header_mock))
      })
    })
    describe("case 1: wrong v5 and v6 extent",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 3,
                            nCS6ByteSize = 0)


        tmp <- tempfile()
        writeBin(openssl::rand_bytes(105),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),
                     "Invalid file for version >= 6",
                     class="seasonder_validate_cs_file_error"
        )
      })
    })


  })


  describe("wrong nRangeCells or nDopplerCells",{
    describe("case 0: correct",{
      it("should not throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769576),tmp)

        expect_no_error(seasonder_validateCSFileData(tmp, header_mock))
      })
    })

    describe("case 1: 0 nRangeCells",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 0,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769576),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),"Invalid nRangeCells or nDopplerCells in file",
                     class = "seasonder_validate_cs_file_error"
        )
      })
    })


    describe("case 2: nRangeCells >8192",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 8193,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769576),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),"Invalid nRangeCells or nDopplerCells in file",
                     class = "seasonder_validate_cs_file_error"
        )
      })
    })

    describe("case 3: 0 nDopplerCells ",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 0,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769576),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),"Invalid nRangeCells or nDopplerCells in file",
                     class = "seasonder_validate_cs_file_error"
        )
      })
    })

    describe("case 4: nDopplerCells > 32768",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 32769,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(1769576),tmp)

        expect_error(seasonder_validateCSFileData(tmp, header_mock),"Invalid nRangeCells or nDopplerCells in file",
                     class = "seasonder_validate_cs_file_error"
        )
      })
    })
  })


  describe("Invalid file size for nCsKind 1 in file",{
    describe("case 0: correct",{
      it("should not throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(104 + 32 * 512 * 3 * 36),tmp)

        expect_no_error(seasonder_validateCSFileData(tmp, header_mock))
      })
    })

    describe("case 1: wrong size",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 1,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(-1 + 104 + 32 * 512 * 3 * 36),tmp)


        expect_error(seasonder_validateCSFileData(tmp, header_mock),"Invalid file size for nCsKind 1 in file",
                     class = "seasonder_validate_cs_file_error"
        )
      })
    })


  })


  describe("Invalid file size for nCsKind 1 in file",{
    describe("case 0: correct",{
      it("should not throw an error",{

        header_mock <- list(nCsKind = 2,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(104 + 32 * 512 * 3 * 40),tmp)

        expect_no_error(seasonder_validateCSFileData(tmp, header_mock))
      })
    })

    describe("case 1: wrong file size",{
      it("should throw an error",{

        header_mock <- list(nCsKind = 2,
                            nRangeCells = 32,
                            nDopplerCells = 512,
                            nSpectraChannels = 3,
                            nCsFileVersion = 6,
                            nV1Extent = 94,
                            nV2Extent = 88,
                            nV3Extent = 80,
                            nV4Extent = 32,
                            nV5Extent = 4,
                            nCS6ByteSize = 0)

        tmp <- tempfile()
        writeBin(openssl::rand_bytes(-1 + 104 + 32 * 512 * 3 * 40),tmp)


        expect_error(seasonder_validateCSFileData(tmp, header_mock),"Invalid file size for nCsKind 2 in file",
                     class = "seasonder_validate_cs_file_error"
        )
      })
    })


  })
})

describe("seasonder_readSeaSondeCSFile",{


  describe("seasonder_readSeaSondeCSFile works correctly", {
    # Setup mock functions


    describe("error handling",{




      describe("if there is a problem opening the connection",{
        it("should throw an error",{

          expect_error(seasonder_readSeaSondeCSFile("fakepath_CS","fakepath_YAML"),
                       "Could no open connection to file",
                       class="seasonder_read_cs_file_error")

        })

      })

      describe("if the specs file version is not correct",{
        it("should thrown an error",{

          mock_readYAMLSpecs <- mock_output_factory(list(version = "wrong version"),
                                                    list("fake_header_specs")
          )



          tmp <- tempfile()
          writeBin(openssl::rand_bytes(10),tmp)
          expect_error(
            mockthat::with_mock(seasonder_readYAMLSpecs=mock_readYAMLSpecs,
                                seasonder_readSeaSondeCSFile(tmp, "fakepath")
            ),
            "Unsupported version",
            class="seasonder_read_cs_file_error"
          )




        })
      })




      describe("restarts",{

        describe("seasonder_skip_cs_file",{

          it("should throw a seasonder_cs_file_skipped condition and return an empty file",{

            expect_warning({
              test <- withCallingHandlers(seasonder_read_cs_file_error = function(cond) seasonder_skip_cs_file(cond),
                                          {
                                            seasonder_readSeaSondeCSFile(NULL,NULL)

                                          })

            },class="seasonder_cs_file_skipped")

            expect_equal(test,list(header=NULL,data=NULL))

          })

        })

      })

    })



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



    it("should read the header",{
      specs <- seasonder_readYAMLSpecs(system.file("specs","CS_V1.yaml",package = "SeaSondeR"),"header")
      con <- file(here::here("tests/testthat/data/CSS_V6.cs"),"rb")
      on.exit(close(con))

      test <- expect_silent(seasonder_readSeaSondeCSFileHeader(specs,con))

      expect_snapshot_value(test,style="serialize")

    })

  })


  describe("Full File",{




    it("should read the data",{

      specs_header <- seasonder_readYAMLSpecs(system.file("specs","CS_V1.yaml",package = "SeaSondeR"),"header")

      con <- file(here::here("tests/testthat/data/CSS_V6.cs"),"rb")
      on.exit(close(con))

      header <- seasonder_readSeaSondeCSFileHeader(specs_header,con)

      test <- expect_silent(seasonder_readSeaSondeCSFileData(con,header))

      expect_snapshot_value(test,style="serialize")



    })

  })

})



#### SeaSondeRCS ####

describe("SeaSondeRCS",{
  test_that("SeaSondeRCS Related functions are defined",{

    expect_true(is.function(seasonder_createSeaSondeRCS))
    expect_true(is.function(seasonder_createSeaSondeRCS.character))
    expect_true(is.function(seasonder_createSeaSondeRCS.list))
    expect_true(is.function(seasonder_getCSHeaderByPath))
    expect_true(is.function(seasonder_getVersion))
    expect_true(is.function(seasonder_getVersion.SeaSondeRCS))
    expect_true(is.function(seasonder_getnDopplerCells))
    expect_true(is.function(seasonder_getnRangeCells))
    expect_true(is.function(seasonder_validateCSDataStructure))

  })


  describe("seasonder_createSeaSondeRCS", {

    # Mock de seasonder_validateCSDataStructure para comprobar que se llama
describe("seasonder_createSeaSondeRCS.character",{
  it("creates a SeaSondeRCS object from a character input (file path)", {
    mocked_validate_function <- mockthat::mock(NULL)
    mocked_read_function <- mockthat::mock(list(header = list(nRangeCells = 10, nDopplerCells = 20), data = list()))
    tmp_file <- tempfile()
    writeLines(" ", tmp_file)
    mockthat::with_mock(seasonder_readSeaSondeCSFile = mocked_read_function, seasonder_validateCSDataStructure = mocked_validate_function, {
      result <- seasonder_createSeaSondeRCS(tmp_file,"fake/specs/path")
      expect_equal(class(result), "SeaSondeRCS")
      expect_equal(result$header$nRangeCells, 10)
      expect_equal(result$header$nDopplerCells, 20)
      expect_equal(mockthat::mock_n_called(mocked_read_function),1)
      expect_equal(mockthat::mock_args(mocked_read_function),list(filepath=tmp_file,specs_path="fake/specs/path"))
      expect_equal(mockthat::mock_n_called(mocked_validate_function),1)
    })
  })

  it("should use the package CS specs definitions file as default for the specs path",{
    mocked_validate_function <- mockthat::mock(NULL)
    mocked_read_function <- mockthat::mock(list(header = list(nRangeCells = 10, nDopplerCells = 20), data = list()))
    tmp_file <- tempfile()
    writeLines(" ", tmp_file)
    mockthat::with_mock(seasonder_readSeaSondeCSFile = mocked_read_function, seasonder_validateCSDataStructure = mocked_validate_function, {
      result <- seasonder_createSeaSondeRCS(tmp_file)
      expect_equal(class(result), "SeaSondeRCS")
      expect_equal(result$header$nRangeCells, 10)
      expect_equal(result$header$nDopplerCells, 20)
      expect_equal(mockthat::mock_n_called(mocked_read_function),1)
      expect_equal(mockthat::mock_arg(mocked_read_function,"specs_path"),system.file("specs","CS_V1.yaml",package = "SeaSondeR"))
      expect_equal(mockthat::mock_n_called(mocked_validate_function),1)
    })


  })
})



    it("creates a SeaSondeRCS object from a list input", {
      mocked_validate_function <- mockthat::mock(NULL)
      mockthat::with_mock(seasonder_validateCSDataStructure = mocked_validate_function, {
        result <- seasonder_createSeaSondeRCS(list(header = list(nRangeCells = 15, nDopplerCells = 30), data = list()))
        expect_equal(class(result), "SeaSondeRCS")
        expect_equal(result$header$nRangeCells, 15)
        expect_equal(result$header$nDopplerCells, 30)
        expect_equal(mockthat::mock_n_called(mocked_validate_function),1)
      })
    })

  })

  describe("seasonder_validateCSDataStructure", {

    it("validates correct data structure", {
      data <- list(
        SSA1 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        SSA2 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        SSA3 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        CS12 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        CS13 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        CS23 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        QC = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE)
      )
      expect_silent(seasonder_validateCSDataStructure(data, 10, 20))
    })

    it("throws error on missing fields", {
      data <- list()
      expect_error(seasonder_validateCSDataStructure(data, 10, 20), class="seasonder_CS_data_structure_validation_error")
    })

    it("throws error on incorrect dimensions", {
      data <- list(
        SSA1 = matrix(rep(NA_real_, 10 * 19), ncol = 19, byrow = TRUE),
        SSA2 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        SSA3 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        CS12 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        CS13 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        CS23 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        QC = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE)
      )
      expect_error(seasonder_validateCSDataStructure(data, 10, 20), class="seasonder_CS_data_structure_validation_error")
    })

    it("throws error on incorrect data type for SSA fields", {
      data <- list(
        SSA1 = matrix(rep("string", 10 * 20), ncol = 20, byrow = TRUE),
        SSA2 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        SSA3 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        CS12 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        CS13 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        CS23 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        QC = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE)
      )
      expect_error(seasonder_validateCSDataStructure(data, 10, 20), class="seasonder_CS_data_structure_validation_error")
    })

    it("throws error on incorrect data type for CS fields", {
      data <- list(
        SSA1 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        SSA2 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        SSA3 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        CS12 = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE),
        CS13 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        CS23 = matrix(rep(complex(real = NA_real_, imaginary = NA_real_), 10 * 20), ncol = 20, byrow = TRUE),
        QC = matrix(rep(NA_real_, 10 * 20), ncol = 20, byrow = TRUE)
      )
      expect_error(seasonder_validateCSDataStructure(data, 10, 20), class="seasonder_CS_data_structure_validation_error")
    })

  })




  describe("SeaSondeRCS version getter", {
    mocked_validate_function <- mockthat::mock(NULL)
    test_obj <-  mockthat::with_mock(seasonder_validateCSDataStructure = mocked_validate_function, {
      seasonder_createSeaSondeRCS(list(header = list(nRangeCells = 15, nDopplerCells = 30), data = list()))})


    # Test para el getter de version
    it("retrieves the correct version value", {
      expect_equal(seasonder_getVersion(test_obj), 1)
    })

  })

  describe("seasonder_getCSHeaderByPath function", {

    # Creamos un objeto de prueba con un header complejo
    mocked_validate_function <- mockthat::mock(NULL)
    test_obj <-  mockthat::with_mock(seasonder_validateCSDataStructure = mocked_validate_function, {
      seasonder_createSeaSondeRCS(list(header = list(nRangeCells = 15,
                                                     nDopplerCells = 30,
                                                     details = list(version = "1.0.0",
                                                                    subdetails = list(info = "sample"))),
                                       data = list()))})

    # Test para obtener un valor directamente del header
    it("retrieves a top-level header value", {
      expect_equal(seasonder_getCSHeaderByPath(test_obj, "nRangeCells"), 15)
    })

    # Test para obtener un valor de una sublista
    it("retrieves a value from a nested list in the header", {
      expect_equal(seasonder_getCSHeaderByPath(test_obj, c("details","version")), "1.0.0")
    })

    # Test para obtener un valor de una sub-sublista
    it("retrieves a value from a deeper nested list in the header", {
      expect_equal(seasonder_getCSHeaderByPath(test_obj, c("details","subdetails","info")), "sample")
    })

    # Test para manejar paths inválidos
    it("returns NULL for an invalid path and thows a warning", {
      expect_warning(expect_null(seasonder_getCSHeaderByPath(test_obj, c("invalid","path"))),
      "seasonder_getCSHeaderByPath: Field 'invalid/path' not found in header.",class="seasonder_SeaSonderCS_field_not_found_in_header")
    })

  })


  describe("SeaSondeRCS header getters", {

    mocked_validate_function <- mockthat::mock(NULL)
    test_obj <-  mockthat::with_mock(seasonder_validateCSDataStructure = mocked_validate_function, {
      seasonder_createSeaSondeRCS(list(header = list(nRangeCells = 15, nDopplerCells = 30), data = list()))
      })





    it("retrieves the correct nRangeCells value", {

      # Mock de la función seasonder_getCSHeaderByPath
      mock_getHeaderByPath <- mockthat::mock(
      15
      )
      mockthat::with_mock(seasonder_getCSHeaderByPath = mock_getHeaderByPath, {
        expect_equal(seasonder_getnRangeCells(test_obj), 15)
      })
      expect_equal(mockthat::mock_n_called(mock_getHeaderByPath),1)
    })

    it("retrieves the correct nDopplerCells value", {
      # Mock de la función seasonder_getCSHeaderByPath
      mock_getHeaderByPath <- mockthat::mock(
        30
      )
      mockthat::with_mock(seasonder_getCSHeaderByPath = mock_getHeaderByPath, {
        expect_equal(seasonder_getnDopplerCells(test_obj), 30)
      })
      expect_equal(mockthat::mock_n_called(mock_getHeaderByPath),1)
    })

  })


})
