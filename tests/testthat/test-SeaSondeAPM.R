# Test for seasonder_createSeaSondeAPM

# Test case 1: Checks if the function returns an object of class "SeaSondeAPM"
test_that("seasonder_createSeaSondeAPM returns an object of class 'SeaSondeAPM'", {
  seasonder_disableMessages()
  result <- seasonder_createSeaSondeAPM()
  expect_type(result, "complex")  # Expecting the matrix to be complex
  expect_s3_class(result, "SeaSondeAPM")
})

# Test case 2: Checks if the function throws an error for non-matrix inputs
test_that("seasonder_createSeaSondeAPM throws an error for non-matrix inputs", {
  seasonder_disableMessages()
  expect_error(seasonder_createSeaSondeAPM("Not a matrix"), "seasonder_validateCalibrationMatrixSeaSondeAPM: Input calibration_matrix must be a matrix.")
})

# Test case 3: Checks if the function throws an error for matrices with rows != 2
test_that("seasonder_createSeaSondeAPM throws an error for matrices with rows != 2", {
  seasonder_disableMessages()
  expect_error(seasonder_createSeaSondeAPM(matrix(complex(real=NA, imaginary=NA), nrow = 3, ncol = 0)), "seasonder_validateCalibrationMatrixSeaSondeAPM: Calibration matrix must have two rows.")
})

# Test case 4: Checks if the function throws an error for non-complex matrices
test_that("seasonder_createSeaSondeAPM throws an error for non-complex matrices", {
  seasonder_disableMessages()
  expect_error(seasonder_createSeaSondeAPM(matrix(numeric(0), nrow = 2, ncol = 0)), "seasonder_validateCalibrationMatrixSeaSondeAPM: Calibration matrix must contain complex numbers.")
})

# Test case 5: Checks if the attributes are properly initialized
test_that("seasonder_createSeaSondeAPM properly initializes attributes", {

  Type <- "Some Type"
  result <- seasonder_createSeaSondeAPM(Type = Type)

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
  # Testing validate_SeaSondeAPM_quality_matrix function
  seasonder_disableMessages()

  it("validate_SeaSondeAPM_quality_matrix works as expected", {
    seasonde_apm_obj <- matrix(0, nrow=2, ncol=2)

    # This should pass
    expect_silent(validate_SeaSondeAPM_quality_matrix(matrix(complex(real=1:4, imaginary=1:4), nrow=2), seasonde_apm_obj))

    # These should fail
    expect_error(validate_SeaSondeAPM_quality_matrix(matrix(1:4, nrow=2), seasonde_apm_obj))
    expect_error(validate_SeaSondeAPM_quality_matrix(matrix(complex(real=1:6, imaginary=1:6), nrow=3), seasonde_apm_obj))
  })

  # Testing validate_SeaSondeAPM_BEAR function
  it("validate_SeaSondeAPM_BEAR works as expected", {
    seasonde_apm_obj <- matrix(0, nrow=2, ncol=3)

    expect_silent(validate_SeaSondeAPM_BEAR(c(1, 2, 3), seasonde_apm_obj)) # should pass
    expect_error(validate_SeaSondeAPM_BEAR("string", seasonde_apm_obj))   # should fail
  })

  # Testing validate_SeaSondeAPM_SiteOrigin function
  it("validate_SeaSondeAPM_SiteOrigin works as expected", {
    expect_silent(validate_SeaSondeAPM_SiteOrigin(c(1, 2))) # should pass
    expect_error(validate_SeaSondeAPM_SiteOrigin("SiteOrigin"))  # should fail
  })

  # Testing validate_SeaSondeAPM_AmplitudeFactors function
  it("validate_SeaSondeAPM_AmplitudeFactors works as expected", {
    expect_silent(validate_SeaSondeAPM_AmplitudeFactors(c(1, 2))) # should pass
    expect_error(validate_SeaSondeAPM_AmplitudeFactors("invalid"))  # should fail
  })

  # Testing validate_SeaSondeAPM_StationCode function
  it("validate_SeaSondeAPM_StationCode works as expected", {
    expect_silent(validate_SeaSondeAPM_StationCode("XYZA")) # should pass
    expect_error(validate_SeaSondeAPM_StationCode(123))  # should fail
  })

  # Testing validate_SeaSondeAPM_PhaseCorrections function
  it("validate_SeaSondeAPM_PhaseCorrections works as expected", {
    expect_silent(validate_SeaSondeAPM_PhaseCorrections(c(1, 2))) # should pass
    expect_error(validate_SeaSondeAPM_PhaseCorrections("invalid"))  # should fail
  })

it("validate_SeaSondeAPM_Type works as expected", {
  expect_silent(validate_SeaSondeAPM_Type("Measured Antenna Pattern")) # should pass
  expect_error(validate_SeaSondeAPM_Type(123))                        # should fail
})


# Testing validate_SeaSondeAPM_Creator function
it("validate_SeaSondeAPM_Creator works as expected", {
  expect_silent(validate_SeaSondeAPM_Creator("")) # should pass
  expect_silent(validate_SeaSondeAPM_Creator("John Doe")) # should pass
  expect_error(validate_SeaSondeAPM_Creator(123))  # should fail
})

# Testing validate_SeaSondeAPM_SiteName function
it("validate_SeaSondeAPM_SiteName works as expected", {
  expect_silent(validate_SeaSondeAPM_SiteName("SiteName")) # should pass
  expect_error(validate_SeaSondeAPM_SiteName(123))  # should fail
})



# Testing validate_SeaSondeAPM_FileName function
it("validate_SeaSondeAPM_FileName works as expected", {
  expect_silent(validate_SeaSondeAPM_FileName("")) # should pass
  expect_silent(validate_SeaSondeAPM_FileName("file.txt")) # should pass
  expect_error(validate_SeaSondeAPM_FileName(123))  # should fail
})

# Testing validate_SeaSondeAPM_CreateTimeStamp function
it("validate_SeaSondeAPM_CreateTimeStamp works as expected", {
  expect_silent(validate_SeaSondeAPM_CreateTimeStamp(Sys.time())) # should pass
  expect_error(validate_SeaSondeAPM_CreateTimeStamp("invalid_date"))  # should fail
})



# Testing validate_SeaSondeAPM_ProcessingSteps function
it("validate_SeaSondeAPM_ProcessingSteps works as expected", {
  expect_silent(validate_SeaSondeAPM_ProcessingSteps(character(0))) # should pass
  expect_silent(validate_SeaSondeAPM_ProcessingSteps(c("step1", "step2"))) # should pass
  expect_error(validate_SeaSondeAPM_ProcessingSteps(123))  # should fail
})


# Testing validate_SeaSondeAPM_AntennaBearing function
it("validate_SeaSondeAPM_AntennaBearing works as expected", {
  expect_silent(validate_SeaSondeAPM_AntennaBearing(90)) # should pass
  expect_error(validate_SeaSondeAPM_AntennaBearing("90"))  # should fail
})


# Testing validate_SeaSondeAPM_BearingResolution function
it("validate_SeaSondeAPM_BearingResolution works as expected", {
  expect_silent(validate_SeaSondeAPM_BearingResolution(1.5)) # should pass
  expect_error(validate_SeaSondeAPM_BearingResolution("1.5"))  # should fail
})

# Testing validate_SeaSondeAPM_Smoothing function
it("validate_SeaSondeAPM_Smoothing works as expected", {
  expect_silent(validate_SeaSondeAPM_Smoothing(1.5)) # should pass
  expect_error(validate_SeaSondeAPM_Smoothing("1.5"))  # should fail
})

# Testing validate_SeaSondeAPM_CommentLine function
it("validate_SeaSondeAPM_CommentLine works as expected", {
  expect_silent(validate_SeaSondeAPM_CommentLine("This is a comment")) # should pass
  expect_error(validate_SeaSondeAPM_CommentLine(123))  # should fail
})

# Testing validate_SeaSondeAPM_FileID function
it("validate_SeaSondeAPM_FileID works as expected", {
  expect_silent(validate_SeaSondeAPM_FileID("file_001")) # should pass
  expect_error(validate_SeaSondeAPM_FileID(1))  # should fail
})


describe("seasonder_validateAttributesSeaSondeAPM",{

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
      validate_SeaSondeAPM_quality_matrix = mock_quality_matrix,
      validate_SeaSondeAPM_BEAR = mock_BEAR,
      validate_SeaSondeAPM_Type = mock_Type,
      validate_SeaSondeAPM_Creator = mock_Creator,
      validate_SeaSondeAPM_SiteName = mock_SiteName,
      validate_SeaSondeAPM_SiteOrigin = mock_SiteOrigin,
      validate_SeaSondeAPM_FileName = mock_FileName,
      validate_SeaSondeAPM_CreateTimeStamp = mock_CreateTimeStamp,
      validate_SeaSondeAPM_ProcessingSteps = mock_ProcessingSteps,
      validate_SeaSondeAPM_AmplitudeFactors = mock_AmplitudeFactors,
      validate_SeaSondeAPM_AntennaBearing = mock_AntennaBearing,
      validate_SeaSondeAPM_StationCode = mock_StationCode,
      validate_SeaSondeAPM_BearingResolution = mock_BearingResolution,
      validate_SeaSondeAPM_Smoothing = mock_Smoothing,
      validate_SeaSondeAPM_CommentLine = mock_CommentLine,
      validate_SeaSondeAPM_FileID = mock_FileID,
      validate_SeaSondeAPM_PhaseCorrections = mock_PhaseCorrections
      , {
        # Run your function
        seasonder_validateAttributesSeaSondeAPM(seasonde_obj)
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

  it("File reading works", {
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    msg <- glue::glue("2022-01-02 03:45:03: Created from {file_path}.")
    mk <- mockthat::mock(msg)
    mockthat::with_mock(creation_step_text=mk,
    test <- seasonder_readSeaSondeAPMFile(file_path,
                                          SiteName="Estación Cies",
                                          Type="Measured Pattern",
                                          Creator="Me")
    )

    expect_snapshot_value(test,style ="serialize")
  })


})
