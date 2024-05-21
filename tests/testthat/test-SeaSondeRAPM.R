# Test for seasonder_createSeaSondeRAPM

# Test case 1: Checks if the function returns an object of class "SeaSondeRAPM"
test_that("seasonder_createSeaSondeRAPM returns an object of class 'SeaSondeRAPM'", {
  seasonder_disableMessages()
  result <- seasonder_createSeaSondeRAPM()
  expect_type(result, "complex")  # Expecting the matrix to be complex
  expect_s3_class(result, "SeaSondeRAPM")
})

# Test case 2: Checks if the function throws an error for non-matrix inputs
test_that("seasonder_createSeaSondeRAPM throws an error for non-matrix inputs", {
  seasonder_disableMessages()
  expect_error(seasonder_createSeaSondeRAPM("Not a matrix"), "seasonder_validateCalibrationMatrixSeaSondeRAPM: Input calibration_matrix must be a matrix.")
})

# Test case 3: Checks if the function throws an error for matrices with rows != 2
test_that("seasonder_createSeaSondeRAPM throws an error for matrices with rows != 2", {
  seasonder_disableMessages()
  expect_error(seasonder_createSeaSondeRAPM(matrix(complex(real=NA, imaginary=NA), nrow = 3, ncol = 0)), "seasonder_validateCalibrationMatrixSeaSondeRAPM: Calibration matrix must have two rows.")
})

# Test case 4: Checks if the function throws an error for non-complex matrices
test_that("seasonder_createSeaSondeRAPM throws an error for non-complex matrices", {
  seasonder_disableMessages()
  expect_error(seasonder_createSeaSondeRAPM(matrix(numeric(0), nrow = 2, ncol = 0)), "seasonder_validateCalibrationMatrixSeaSondeRAPM: Calibration matrix must contain complex numbers.")
})

# Test case 5: Checks if the attributes are properly initialized
test_that("seasonder_createSeaSondeRAPM properly initializes attributes", {

  Type <- "Some Type"
  result <- seasonder_createSeaSondeRAPM(Type = Type)

  expect_equal(attributes(result)$Type, Type)
  expect_equal(attributes(result)$BEAR, numeric(0))
  expect_equal(attributes(result)$Creator, character(0))
  expect_equal(attributes(result)$SiteName, character(0))
  expect_equal(attributes(result)$SiteOrigin, numeric(2))
  expect_equal(attributes(result)$FileName, character(0))
  expect_equal(attributes(result)$CreateTimeStamp, Sys.time())
  expect_equal(attributes(result)$ProcessingSteps, character(0))
  expect_equal(attributes(result)$AmplitudeFactors, numeric(2))
  expect_equal(attributes(result)$AntennaBearing, numeric(0))
  expect_equal(attributes(result)$StationCode, character(0))
  expect_equal(attributes(result)$BearingResolution, numeric(0))
  expect_equal(attributes(result)$Smoothing, numeric(0))
  expect_equal(attributes(result)$CommentLine, character(0))
  expect_equal(attributes(result)$FileID, character(0))
  expect_equal(attributes(result)$PhaseCorrections, numeric(2))

  # Verify that 'quality_matrix' is a complex matrix with zero rows and zero columns
  expect_true(is.matrix(attributes(result)$quality_matrix))
  expect_true(is.complex(attributes(result)$quality_matrix))
  expect_equal(dim(attributes(result)$quality_matrix), c(2, 0))
})



#### Funciones de validacion ####

describe("Validation",{
  # Testing validate_SeaSondeRAPM_quality_matrix function
  seasonder_disableMessages()

  it("validate_SeaSondeRAPM_quality_matrix works as expected", {
    seasonde_apm_obj <- matrix(0, nrow=2, ncol=2)

    # This should pass
    expect_silent(validate_SeaSondeRAPM_quality_matrix(matrix(complex(real=1:4, imaginary=1:4), nrow=2), seasonde_apm_obj))

    # These should fail
    expect_error(validate_SeaSondeRAPM_quality_matrix(matrix(1:4, nrow=2), seasonde_apm_obj))
    expect_error(validate_SeaSondeRAPM_quality_matrix(matrix(complex(real=1:6, imaginary=1:6), nrow=3), seasonde_apm_obj))
  })

  # Testing validate_SeaSondeRAPM_BEAR function
  it("validate_SeaSondeRAPM_BEAR works as expected", {
    seasonde_apm_obj <- matrix(0, nrow=2, ncol=3)

    expect_silent(validate_SeaSondeRAPM_BEAR(c(1, 2, 3), seasonde_apm_obj)) # should pass
    expect_error(validate_SeaSondeRAPM_BEAR("string", seasonde_apm_obj))   # should fail
  })

  # Testing validate_SeaSondeRAPM_SiteOrigin function
  it("validate_SeaSondeRAPM_SiteOrigin works as expected", {
    expect_silent(validate_SeaSondeRAPM_SiteOrigin(c(1, 2))) # should pass
    expect_error(validate_SeaSondeRAPM_SiteOrigin("SiteOrigin"))  # should fail
  })

  # Testing validate_SeaSondeRAPM_AmplitudeFactors function
  it("validate_SeaSondeRAPM_AmplitudeFactors works as expected", {
    expect_silent(validate_SeaSondeRAPM_AmplitudeFactors(c(1, 2))) # should pass
    expect_error(validate_SeaSondeRAPM_AmplitudeFactors("invalid"))  # should fail
  })

  # Testing validate_SeaSondeRAPM_StationCode function
  it("validate_SeaSondeRAPM_StationCode works as expected", {
    expect_silent(validate_SeaSondeRAPM_StationCode("XYZA")) # should pass
    expect_error(validate_SeaSondeRAPM_StationCode(123))  # should fail
  })

  # Testing validate_SeaSondeRAPM_PhaseCorrections function
  it("validate_SeaSondeRAPM_PhaseCorrections works as expected", {
    expect_silent(validate_SeaSondeRAPM_PhaseCorrections(c(1, 2))) # should pass
    expect_error(validate_SeaSondeRAPM_PhaseCorrections("invalid"))  # should fail
  })

it("validate_SeaSondeRAPM_Type works as expected", {
  expect_silent(validate_SeaSondeRAPM_Type("Measured Antenna Pattern")) # should pass
  expect_error(validate_SeaSondeRAPM_Type(123))                        # should fail
})


# Testing validate_SeaSondeRAPM_Creator function
it("validate_SeaSondeRAPM_Creator works as expected", {
  expect_silent(validate_SeaSondeRAPM_Creator("")) # should pass
  expect_silent(validate_SeaSondeRAPM_Creator("John Doe")) # should pass
  expect_error(validate_SeaSondeRAPM_Creator(123))  # should fail
})

# Testing validate_SeaSondeRAPM_SiteName function
it("validate_SeaSondeRAPM_SiteName works as expected", {
  expect_silent(validate_SeaSondeRAPM_SiteName("SiteName")) # should pass
  expect_error(validate_SeaSondeRAPM_SiteName(123))  # should fail
})



# Testing validate_SeaSondeRAPM_FileName function
it("validate_SeaSondeRAPM_FileName works as expected", {
  expect_silent(validate_SeaSondeRAPM_FileName("")) # should pass
  expect_silent(validate_SeaSondeRAPM_FileName("file.txt")) # should pass
  expect_error(validate_SeaSondeRAPM_FileName(123))  # should fail
})

# Testing validate_SeaSondeRAPM_CreateTimeStamp function
it("validate_SeaSondeRAPM_CreateTimeStamp works as expected", {
  expect_silent(validate_SeaSondeRAPM_CreateTimeStamp(Sys.time())) # should pass
  expect_error(validate_SeaSondeRAPM_CreateTimeStamp("invalid_date"))  # should fail
})



# Testing validate_SeaSondeRAPM_ProcessingSteps function
it("validate_SeaSondeRAPM_ProcessingSteps works as expected", {
  expect_silent(validate_SeaSondeRAPM_ProcessingSteps(character(0))) # should pass
  expect_silent(validate_SeaSondeRAPM_ProcessingSteps(c("step1", "step2"))) # should pass
  expect_error(validate_SeaSondeRAPM_ProcessingSteps(123))  # should fail
})


# Testing validate_SeaSondeRAPM_AntennaBearing function
it("validate_SeaSondeRAPM_AntennaBearing works as expected", {
  expect_silent(validate_SeaSondeRAPM_AntennaBearing(90)) # should pass
  expect_error(validate_SeaSondeRAPM_AntennaBearing("90"))  # should fail
})


# Testing validate_SeaSondeRAPM_BearingResolution function
it("validate_SeaSondeRAPM_BearingResolution works as expected", {
  expect_silent(validate_SeaSondeRAPM_BearingResolution(1.5)) # should pass
  expect_error(validate_SeaSondeRAPM_BearingResolution("1.5"))  # should fail
})

# Testing validate_SeaSondeRAPM_Smoothing function
it("validate_SeaSondeRAPM_Smoothing works as expected", {
  expect_silent(validate_SeaSondeRAPM_Smoothing(1.5)) # should pass
  expect_error(validate_SeaSondeRAPM_Smoothing("1.5"))  # should fail
})

# Testing validate_SeaSondeRAPM_CommentLine function
it("validate_SeaSondeRAPM_CommentLine works as expected", {
  expect_silent(validate_SeaSondeRAPM_CommentLine("This is a comment")) # should pass
  expect_error(validate_SeaSondeRAPM_CommentLine(123))  # should fail
})

# Testing validate_SeaSondeRAPM_FileID function
it("validate_SeaSondeRAPM_FileID works as expected", {
  expect_silent(validate_SeaSondeRAPM_FileID("file_001")) # should pass
  expect_error(validate_SeaSondeRAPM_FileID(1))  # should fail
})


describe("seasonder_validateAttributesSeaSondeRAPM",{

  it("should call each validation function",{


    mock_quality_matrix <- mockthat::mock(c("Called validate_quality_matrix"))
    mock_BEAR <- mockthat::mock(c("Called validate_BEAR"))
    mock_Type <- mockthat::mock(c("Called validate_Type"))
    mock_Creator <- mockthat::mock(c("Called validate_Creator"))
    mock_SiteName <- mockthat::mock(c("Called validate_SiteName"))
    mock_SiteOrigin <- mockthat::mock(c("Called validate_SiteOrigin"))
    mock_FileName <- mockthat::mock(c("Called validate_FileName"))
    mock_CreateTimeStamp <- mockthat::mock(c("Called validate_CreateTimeStamp"))
    mock_ProcessingSteps <- mockthat::mock(c("Called validate_ProcessingSteps"))
    mock_AmplitudeFactors <- mockthat::mock(c("Called validate_AmplitudeFactors"))
    mock_AntennaBearing <- mockthat::mock(c("Called validate_AntennaBearing"))
    mock_StationCode <- mockthat::mock(c("Called validate_StationCode"))
    mock_BearingResolution <- mockthat::mock(c("Called validate_BearingResolution"))
    mock_Smoothing <- mockthat::mock(c("Called validate_Smoothing"))
    mock_CommentLine <- mockthat::mock(c("Called validate_CommentLine"))
    mock_FileID <- mockthat::mock(c("Called validate_FileID"))
    mock_PhaseCorrections <- mockthat::mock(c("Called validate_PhaseCorrections"))


    # Assume seasonde_obj is your test object
    seasonde_obj <- list() # (create a mock or test object as necessary)

    mockthat::with_mock(
      validate_SeaSondeRAPM_quality_matrix = mock_quality_matrix,
      validate_SeaSondeRAPM_BEAR = mock_BEAR,
      validate_SeaSondeRAPM_Type = mock_Type,
      validate_SeaSondeRAPM_Creator = mock_Creator,
      validate_SeaSondeRAPM_SiteName = mock_SiteName,
      validate_SeaSondeRAPM_SiteOrigin = mock_SiteOrigin,
      validate_SeaSondeRAPM_FileName = mock_FileName,
      validate_SeaSondeRAPM_CreateTimeStamp = mock_CreateTimeStamp,
      validate_SeaSondeRAPM_ProcessingSteps = mock_ProcessingSteps,
      validate_SeaSondeRAPM_AmplitudeFactors = mock_AmplitudeFactors,
      validate_SeaSondeRAPM_AntennaBearing = mock_AntennaBearing,
      validate_SeaSondeRAPM_StationCode = mock_StationCode,
      validate_SeaSondeRAPM_BearingResolution = mock_BearingResolution,
      validate_SeaSondeRAPM_Smoothing = mock_Smoothing,
      validate_SeaSondeRAPM_CommentLine = mock_CommentLine,
      validate_SeaSondeRAPM_FileID = mock_FileID,
      validate_SeaSondeRAPM_PhaseCorrections = mock_PhaseCorrections
      , {
        # Run your function
        seasonder_validateAttributesSeaSondeRAPM(seasonde_obj)
      }
    )
    expect_equal(mockthat::mock_n_called(mock_quality_matrix), 1)
    expect_equal(mockthat::mock_n_called(mock_BEAR), 1)
    expect_equal(mockthat::mock_n_called(mock_Type), 1)
    expect_equal(mockthat::mock_n_called(mock_Creator), 1)
    expect_equal(mockthat::mock_n_called(mock_SiteName), 1)
    expect_equal(mockthat::mock_n_called(mock_SiteOrigin), 1)
    expect_equal(mockthat::mock_n_called(mock_FileName), 1)
    expect_equal(mockthat::mock_n_called(mock_CreateTimeStamp), 1)
    expect_equal(mockthat::mock_n_called(mock_ProcessingSteps), 1)
    expect_equal(mockthat::mock_n_called(mock_AmplitudeFactors), 1)
    expect_equal(mockthat::mock_n_called(mock_AntennaBearing), 1)
    expect_equal(mockthat::mock_n_called(mock_StationCode), 1)
    expect_equal(mockthat::mock_n_called(mock_BearingResolution), 1)
    expect_equal(mockthat::mock_n_called(mock_Smoothing), 1)
    expect_equal(mockthat::mock_n_called(mock_CommentLine), 1)
    expect_equal(mockthat::mock_n_called(mock_FileID), 1)
    expect_equal(mockthat::mock_n_called(mock_PhaseCorrections), 1)

  })
})

})


#### File reading ####


describe("file reading",{

  test_that("File reading works", {
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    msg <- glue::glue("2022-01-02 03:45:03: Created from {file_path}.")
    mk <- mockthat::mock(msg)
    mockthat::with_mock(SeaSondeRAPM_creation_step_text=mk,
    test <- seasonder_readSeaSondeRAPMFile(file_path,
                                          SiteName="Estación Cies",
                                          Type="Measured Pattern",
                                          Creator="Me")
    )

    expect_snapshot_value(test,style ="serialize")
  })

  test_that("longitude and latitude override works",{


    file_path <- here::here("tests/testthat/data/TORA/IdealPattern.txt")

    target <- c(Latitude = 42, Longitude = -8)
    test <- seasonder_readSeaSondeRAPMFile(file_path,
                                           override_SiteOrigin = target,
) %>% seasonder_getSeaSondeRAPM_SiteOrigin()

expect_equal(test,target)

  })
})
