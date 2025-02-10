#### seasonder_createSeaSondeRAPM ####

#### All Tested Functions Exist ####
test_that("All tested functions exist", {
  funs <- c(
    "seasonder_createSeaSondeRAPM", "seasonder_disableMessages",
    "validate_SeaSondeRAPM_quality_matrix", "validate_SeaSondeRAPM_BEAR",
    "validate_SeaSondeRAPM_SiteOrigin", "validate_SeaSondeRAPM_AmplitudeFactors",
    "validate_SeaSondeRAPM_StationCode", "validate_SeaSondeRAPM_PhaseCorrections",
    "validate_SeaSondeRAPM_Type", "validate_SeaSondeRAPM_Creator",
    "validate_SeaSondeRAPM_SiteName", "validate_SeaSondeRAPM_FileName",
    "validate_SeaSondeRAPM_CreateTimeStamp", "validate_SeaSondeRAPM_ProcessingSteps",
    "validate_SeaSondeRAPM_AntennaBearing", "validate_SeaSondeRAPM_BearingResolution",
    "validate_SeaSondeRAPM_Smoothing", "validate_SeaSondeRAPM_CommentLine",
    "validate_SeaSondeRAPM_FileID",
    "seasonder_validateAttributesSeaSondeRAPM", "seasonder_initializeAttributesSeaSondeRAPM",
    "seasonder_setSeaSondeRAPM_quality_matrix", "seasonder_getSeaSondeRAPM_quality_matrix",
    "seasonder_setSeaSondeRAPM_BEAR", "seasonder_getSeaSondeRAPM_BEAR",
    "seasonder_setSeaSondeRAPM_PhaseCorrections", "seasonder_getSeaSondeRAPM_PhaseCorrections",
    "seasonder_setSeaSondeRAPM_Type", "seasonder_getSeaSondeRAPM_Type",
    "seasonder_setSeaSondeRAPM_Creator", "seasonder_getSeaSondeRAPM_Creator",
    "seasonder_setSeaSondeRAPM_SiteName", "seasonder_getSeaSondeRAPM_SiteName",
    "seasonder_setSeaSondeRAPM_SiteOrigin", "seasonder_getSeaSondeRAPM_SiteOrigin",
    "seasonder_setSeaSondeRAPM_FileName", "seasonder_getSeaSondeRAPM_FileName",
    "seasonder_setSeaSondeRAPM_CreateTimeStamp", "seasonder_getSeaSondeRAPM_CreateTimeStamp",
    "seasonder_setSeaSondeRAPM_ProcessingSteps", "seasonder_getSeaSondeRAPM_ProcessingSteps",
    "seasonder_setSeaSondeRAPM_AmplitudeFactors", "seasonder_getSeaSondeRAPM_AmplitudeFactors",
    "seasonder_setSeaSondeRAPM_AntennaBearing", "seasonder_getSeaSondeRAPM_AntennaBearing",
    "seasonder_setSeaSondeRAPM_StationCode", "seasonder_getSeaSondeRAPM_StationCode",
    "seasonder_setSeaSondeRAPM_BearingResolution", "seasonder_getSeaSondeRAPM_BearingResolution",
    "seasonder_setSeaSondeRAPM_Smoothing", "seasonder_getSeaSondeRAPM_Smoothing",
    "seasonder_setSeaSondeRAPM_CommentLine", "seasonder_getSeaSondeRAPM_CommentLine",
    "seasonder_setSeaSondeRAPM_FileID", "seasonder_getSeaSondeRAPM_FileID",
    "seasonder_smoothAPM", "seasonder_trimAPM", "seasonder_applyAPMAmplitudeAndPhaseCorrections",
    "seasonder_readSeaSondeRAPMFile", "seasonder_readPhaseFile",
    "SeaSondeRAPM_creation_step_text", "SeaSondeRAPM_antenna_bearing_override_step_text",
    "SeaSondeRAPM_smoothing_step_text", "SeaSondeRAPM_trimming_step_text",
    "SeaSondeRAPM_amplitude_and_phase_corrections_step_text", "SeaSondeRAPM_phase_correction_override_step_text",
    "SeaSondeRAPM_amplitude_factors_override_step_text", "SeaSondeRAPM_SiteOrigin_override_step_text",
    "seasonder_plotAPMLoops", "parse_metadata_line"
  )
  for (f in funs) {
    expect_true(exists(f, mode = "function"), info = paste("Function", f, "does not exist"))
  }
})



describe("seasonder_createSeaSondeRAPM", {

  it("returns an object of class 'SeaSondeRAPM'", {
    seasonder_disableMessages()
    result <- seasonder_createSeaSondeRAPM()
    expect_type(result, "complex")  # Expecting the underlying matrix to be complex
    expect_s3_class(result, "SeaSondeRAPM")
  })

  it("throws an error for non-matrix inputs", {
    seasonder_disableMessages()
    expect_error(
      seasonder_createSeaSondeRAPM("Not a matrix"),
      "seasonder_validateCalibrationMatrixSeaSondeRAPM: Input calibration_matrix must be a matrix."
    )
  })

  it("throws an error for matrices with rows != 3", {
    seasonder_disableMessages()
    expect_error(
      seasonder_createSeaSondeRAPM(matrix(complex(real = NA, imaginary = NA), nrow = 2, ncol = 0)),
      "seasonder_validateCalibrationMatrixSeaSondeRAPM: Calibration matrix must have three rows."
    )
  })

  it("throws an error for non-complex matrices", {
    seasonder_disableMessages()
    expect_error(
      seasonder_createSeaSondeRAPM(matrix(numeric(0), nrow = 3, ncol = 0)),
      "seasonder_validateCalibrationMatrixSeaSondeRAPM: Calibration matrix must contain complex numbers."
    )
  })

  it("properly initializes attributes", {
    Type <- "Some Type"
    result <- seasonder_createSeaSondeRAPM(Type = Type)

    expect_equal(attributes(result)$Type, Type)
    expect_equal(attributes(result)$BEAR, numeric(0))
    expect_equal(attributes(result)$Creator, character(0))
    expect_equal(attributes(result)$SiteName, character(0))
    # Se asigna SiteOrigin con nombres para latitud y longitud:
    expect_equal(attributes(result)$SiteOrigin, c(Latitude = 0, Longitude = 0))
    expect_equal(attributes(result)$FileName, character(0))
    expect_equal(attributes(result)$ProcessingSteps, character(0))
    expect_equal(attributes(result)$AmplitudeFactors, numeric(2))
    expect_equal(attributes(result)$AntennaBearing, numeric(0))
    expect_equal(attributes(result)$StationCode, character(0))
    expect_equal(attributes(result)$BearingResolution, numeric(0))
    expect_equal(attributes(result)$Smoothing, numeric(0))
    expect_equal(attributes(result)$CommentLine, character(0))
    expect_equal(attributes(result)$FileID, character(0))
    expect_equal(attributes(result)$PhaseCorrections, numeric(2))

    # Verificar que 'quality_matrix' es una matriz compleja de 3 filas y 0 columnas
    expect_true(is.matrix(attributes(result)$quality_matrix))
    expect_true(is.complex(attributes(result)$quality_matrix))
    expect_equal(dim(attributes(result)$quality_matrix), c(3, 0))
  })
})

#### Validation Functions ####
describe("Validation Functions", {

  it("validate_SeaSondeRAPM_quality_matrix works as expected", {
    seasonde_apm_obj <- matrix(0, nrow = 3, ncol = 2)
    # Caso exitoso
    expect_silent(
      validate_SeaSondeRAPM_quality_matrix(
        matrix(complex(real = 1:6, imaginary = 1:6), nrow = 3),
        seasonde_apm_obj
      )
    )
    # Casos de error
    expect_error(
      validate_SeaSondeRAPM_quality_matrix(
        matrix(1:6, nrow = 3),
        seasonde_apm_obj
      )
    )
    expect_error(
      validate_SeaSondeRAPM_quality_matrix(
        matrix(complex(real = 1:2, imaginary = 1:2), nrow = 2),
        seasonde_apm_obj
      )
    )
  })

  it("validate_SeaSondeRAPM_BEAR works as expected", {
    seasonde_apm_obj <- matrix(0, nrow = 3, ncol = 3)
    expect_silent(validate_SeaSondeRAPM_BEAR(c(1, 2, 3), seasonde_apm_obj))
    expect_error(validate_SeaSondeRAPM_BEAR("string", seasonde_apm_obj))
  })

  it("validate_SeaSondeRAPM_SiteOrigin works as expected", {
    expect_silent(validate_SeaSondeRAPM_SiteOrigin(c(1, 2)))
    expect_error(validate_SeaSondeRAPM_SiteOrigin("SiteOrigin"))
  })

  it("validate_SeaSondeRAPM_AmplitudeFactors works as expected", {
    expect_silent(validate_SeaSondeRAPM_AmplitudeFactors(c(1, 2)))
    expect_error(validate_SeaSondeRAPM_AmplitudeFactors("invalid"))
  })

  it("validate_SeaSondeRAPM_StationCode works as expected", {
    expect_silent(validate_SeaSondeRAPM_StationCode("XYZA"))
    expect_error(validate_SeaSondeRAPM_StationCode(123))
  })

  it("validate_SeaSondeRAPM_PhaseCorrections works as expected", {
    expect_silent(validate_SeaSondeRAPM_PhaseCorrections(c(1, 2)))
    expect_error(validate_SeaSondeRAPM_PhaseCorrections("invalid"))
  })

  it("validate_SeaSondeRAPM_Type works as expected", {
    expect_silent(validate_SeaSondeRAPM_Type("Measured Antenna Pattern"))
    expect_error(validate_SeaSondeRAPM_Type(123))
  })

  it("validate_SeaSondeRAPM_Creator works as expected", {
    expect_silent(validate_SeaSondeRAPM_Creator(""))
    expect_silent(validate_SeaSondeRAPM_Creator("John Doe"))
    expect_error(validate_SeaSondeRAPM_Creator(123))
  })

  it("validate_SeaSondeRAPM_SiteName works as expected", {
    expect_silent(validate_SeaSondeRAPM_SiteName("SiteName"))
    expect_error(validate_SeaSondeRAPM_SiteName(123))
  })

  it("validate_SeaSondeRAPM_FileName works as expected", {
    expect_silent(validate_SeaSondeRAPM_FileName(""))
    expect_silent(validate_SeaSondeRAPM_FileName("file.txt"))
    expect_error(validate_SeaSondeRAPM_FileName(123))
  })

  it("validate_SeaSondeRAPM_CreateTimeStamp works as expected", {
    expect_silent(validate_SeaSondeRAPM_CreateTimeStamp(Sys.time()))
    expect_error(validate_SeaSondeRAPM_CreateTimeStamp("invalid_date"))
  })

  it("validate_SeaSondeRAPM_ProcessingSteps works as expected", {
    expect_silent(validate_SeaSondeRAPM_ProcessingSteps(character(0)))
    expect_silent(validate_SeaSondeRAPM_ProcessingSteps(c("step1", "step2")))
    expect_error(validate_SeaSondeRAPM_ProcessingSteps(123))
  })

  it("validate_SeaSondeRAPM_AntennaBearing works as expected", {
    expect_silent(validate_SeaSondeRAPM_AntennaBearing(90))
    expect_error(validate_SeaSondeRAPM_AntennaBearing("90"))
  })

  it("validate_SeaSondeRAPM_BearingResolution works as expected", {
    expect_silent(validate_SeaSondeRAPM_BearingResolution(1.5))
    expect_error(validate_SeaSondeRAPM_BearingResolution("1.5"))
  })

  it("validate_SeaSondeRAPM_Smoothing works as expected", {
    expect_silent(validate_SeaSondeRAPM_Smoothing(1.5))
    expect_error(validate_SeaSondeRAPM_Smoothing("1.5"))
  })

  it("validate_SeaSondeRAPM_CommentLine works as expected", {
    expect_silent(validate_SeaSondeRAPM_CommentLine("This is a comment"))
    expect_error(validate_SeaSondeRAPM_CommentLine(123))
  })

  it("validate_SeaSondeRAPM_FileID works as expected", {
    expect_silent(validate_SeaSondeRAPM_FileID("file_001"))
    expect_error(validate_SeaSondeRAPM_FileID(1))
  })
})

#### seasonder_validateAttributesSeaSondeRAPM ####
describe("seasonder_validateAttributesSeaSondeRAPM", {
  it("calls each individual validation function", {
    # Configurar mocks para cada función de validación
    mock_quality_matrix  <- mockthat::mock("Called validate_quality_matrix")
    mock_BEAR            <- mockthat::mock("Called validate_BEAR")
    mock_Type            <- mockthat::mock("Called validate_Type")
    mock_Creator         <- mockthat::mock("Called validate_Creator")
    mock_SiteName        <- mockthat::mock("Called validate_SiteName")
    mock_SiteOrigin      <- mockthat::mock("Called validate_SiteOrigin")
    mock_FileName        <- mockthat::mock("Called validate_FileName")
    mock_CreateTimeStamp <- mockthat::mock("Called validate_CreateTimeStamp")
    mock_ProcessingSteps <- mockthat::mock("Called validate_ProcessingSteps")
    mock_AmplitudeFactors<- mockthat::mock("Called validate_AmplitudeFactors")
    mock_AntennaBearing  <- mockthat::mock("Called validate_AntennaBearing")
    mock_StationCode     <- mockthat::mock("Called validate_StationCode")
    mock_BearingResolution <- mockthat::mock("Called validate_BearingResolution")
    mock_Smoothing       <- mockthat::mock("Called validate_Smoothing")
    mock_CommentLine     <- mockthat::mock("Called validate_CommentLine")
    mock_FileID          <- mockthat::mock("Called validate_FileID")
    mock_PhaseCorrections<- mockthat::mock("Called validate_PhaseCorrections")

    seasonde_obj <- list()  # objeto de prueba (dummy)

    mockthat::with_mock(
      validate_SeaSondeRAPM_quality_matrix = mock_quality_matrix,
      validate_SeaSondeRAPM_BEAR           = mock_BEAR,
      validate_SeaSondeRAPM_Type           = mock_Type,
      validate_SeaSondeRAPM_Creator        = mock_Creator,
      validate_SeaSondeRAPM_SiteName       = mock_SiteName,
      validate_SeaSondeRAPM_SiteOrigin     = mock_SiteOrigin,
      validate_SeaSondeRAPM_FileName       = mock_FileName,
      validate_SeaSondeRAPM_CreateTimeStamp= mock_CreateTimeStamp,
      validate_SeaSondeRAPM_ProcessingSteps= mock_ProcessingSteps,
      validate_SeaSondeRAPM_AmplitudeFactors= mock_AmplitudeFactors,
      validate_SeaSondeRAPM_AntennaBearing = mock_AntennaBearing,
      validate_SeaSondeRAPM_StationCode    = mock_StationCode,
      validate_SeaSondeRAPM_BearingResolution= mock_BearingResolution,
      validate_SeaSondeRAPM_Smoothing      = mock_Smoothing,
      validate_SeaSondeRAPM_CommentLine    = mock_CommentLine,
      validate_SeaSondeRAPM_FileID         = mock_FileID,
      validate_SeaSondeRAPM_PhaseCorrections = mock_PhaseCorrections, {
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

#### seasonder_readSeaSondeRAPMFile ####
describe("File Reading", {

  it("File reading works", {
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    msg <- glue::glue("2022-01-02 03:45:03: Created from {file_path}.")
    mk <- mockthat::mock(msg)
    mockthat::with_mock(SeaSondeRAPM_creation_step_text = mk, {
      test_obj <- seasonder_readSeaSondeRAPMFile(
        file_path,
        SiteName = "Estación Cies",
        Type = "Measured Pattern",
        Creator = "Me"
      )
    })
    expect_snapshot_value(test_obj, style = "serialize")
  })

  it("longitude and latitude override works", {
    file_path <- here::here("tests/testthat/data/TORA/IdealPattern.txt")
    target <- c(Latitude = 42, Longitude = -8)
    test_obj <- seasonder_readSeaSondeRAPMFile(
      file_path,
      override_SiteOrigin = target
    ) %>% seasonder_getSeaSondeRAPM_SiteOrigin()
    expect_equal(test_obj, target)
  })
})

#### Plots ####
describe("Plots", {

  it("Loops plot works", {
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    seasonder_apm_object <- seasonder_readSeaSondeRAPMFile(file_path)
    p <- seasonder_plotAPMLoops(seasonder_apm_object)
    expect_s3_class(p, "ggplot")
  })

  it("Ambiguity plot works", {
    file_path <- here::here("tests/testthat/data/2018-06-21  170220.txt")
    seasonder_apm_object <- seasonder_readSeaSondeRAPMFile(file_path)
    seasonder_apm_object %<>% seasonder_smoothAPM(10)

    distances <- 1:ncol(seasonder_apm_object) %>% purrr::map(\(i) {
      x <- matrix(
        rep(seasonder_apm_object[, i, drop = FALSE],
            ncol(seasonder_apm_object)),
        nrow = 3, byrow = FALSE
      ) - as.matrix(seasonder_apm_object)
      y <- pracma::Real(sqrt(diag(Conj(t(x)) %*% (x)))) %>% matrix(nrow = 1)
      as.data.frame(y)
    }) %>% purrr::list_rbind() %>% as.matrix()

    inverse_distance <- 10 * log10(1 / distances)
    inverse_distance[is.infinite(inverse_distance)] <- 25

    p <- as.data.frame(as.table(inverse_distance)) %>%
      ggplot2::ggplot(ggplot2::aes(Var2, Var1, fill = Freq)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradientn(colours = rainbow(256)) +
      ggplot2::theme_minimal()

    expect_s3_class(p, "ggplot")
  })
})

#### seasonder_smoothAPM ####
describe("Smoothing APM", {
  it("applies smoothing and updates processing steps", {
    # Crear una matriz dummy con 3 filas y 10 columnas
    calib <- matrix(complex(real = 1:30, imaginary = 31:60), nrow = 3)
    apm <- seasonder_createSeaSondeRAPM(calibration_matrix = calib, BEAR = 1:10)

    orig_channel1 <- apm[1, ]
    smoothing_value <- 2
    apm_smoothed <- seasonder_smoothAPM(apm, smoothing = smoothing_value)

    # Verificar que se añade un mensaje de smoothing en ProcessingSteps
    expect_true(any(grepl("APM smoothed with smoothing", seasonder_getSeaSondeRAPM_ProcessingSteps(apm_smoothed))))
    # Verificar que los valores de la primera fila han sido modificados
    expect_false(all(apm_smoothed[1, ] == orig_channel1))
  })
})

#### seasonder_trimAPM ####
describe("Trimming APM", {
  it("trims the data correctly and updates BEAR and processing steps", {
    calib <- matrix(complex(real = 1:30, imaginary = 31:60), nrow = 3)
    apm <- seasonder_createSeaSondeRAPM(calibration_matrix = calib, BEAR = 1:10)
    orig_dim <- dim(apm)
    orig_bear <- seasonder_getSeaSondeRAPM_BEAR(apm)

    trimming_value <- 2
    apm_trimmed <- seasonder_trimAPM(apm, trimming = trimming_value)

    # El número de columnas debe reducirse en 2 * trimming_value
    expect_equal(ncol(apm_trimmed), orig_dim[2] - 2 * trimming_value)
    # La longitud de BEAR se debe ajustar
    expect_equal(length(seasonder_getSeaSondeRAPM_BEAR(apm_trimmed)), length(orig_bear) - 2 * trimming_value)
    expect_true(any(grepl("trimmed", seasonder_getSeaSondeRAPM_ProcessingSteps(apm_trimmed))))
  })
})

#### seasonder_applyAPMAmplitudeAndPhaseCorrections ####
describe("Amplitude and Phase Corrections", {
  it("applies corrections correctly", {
    calib <- matrix(complex(real = rep(1, 15), imaginary = rep(0, 15)), nrow = 3)
    apm <- seasonder_createSeaSondeRAPM(calibration_matrix = calib, BEAR = 1:5)

    # Configurar los factores de amplitud y correcciones de fase
    apm <- seasonder_setSeaSondeRAPM_AmplitudeFactors(apm, c(2, 3))
    apm <- seasonder_setSeaSondeRAPM_PhaseCorrections(apm, c(0, 90))

    orig_ch1 <- apm[1, ]
    orig_ch2 <- apm[2, ]

    apm_corrected <- seasonder_applyAPMAmplitudeAndPhaseCorrections(apm)

    expected_ch1 <- orig_ch1 * 2 * exp(1i * 0 * pi / 180)
    expected_ch2 <- orig_ch2 * 3 * exp(1i * 90 * pi / 180)

    expect_equal(apm_corrected[1, ], expected_ch1)
    expect_equal(apm_corrected[2, ], expected_ch2)
    expect_true(any(grepl("Phase corrections", seasonder_getSeaSondeRAPM_ProcessingSteps(apm_corrected))))
  })
})

#### parse_metadata_line ####
describe("Metadata Parsing", {
  it("parses a valid metadata line correctly", {
    line <- "1.0 2.0 ! Amplitude Factors"
    parsed <- parse_metadata_line(line)
    expect_equal(parsed$attribute_name, "AmplitudeFactors")
    expect_equal(parsed$value, c(1.0, 2.0))
  })

  it("returns 'Unknown' for metadata lines with missing delimiter", {
    line <- "This is a comment without delimiter"
    parsed <- parse_metadata_line(line)
    expect_equal(parsed$attribute_name, "Unknown")
    expect_equal(parsed$value, "This is a comment without delimiter")
  })
})

#### Step Text Functions ####
describe("Step Text Functions", {
  it("creation step text includes the file path", {
    file_path <- "dummy_file.txt"
    text <- SeaSondeRAPM_creation_step_text(file_path)
    expect_true(grepl(file_path, text))
  })

  it("antenna bearing override text includes the new value", {
    new_bearing <- 45
    text <- SeaSondeRAPM_antenna_bearing_override_step_text(new_bearing)
    expect_true(grepl(as.character(new_bearing), text))
  })

  it("smoothing step text includes the smoothing parameter", {
    smoothing <- 5
    text <- SeaSondeRAPM_smoothing_step_text(smoothing)
    expect_true(grepl(as.character(smoothing), text))
  })

  it("trimming step text includes the trimming value", {
    trimming <- 3
    text <- SeaSondeRAPM_trimming_step_text(trimming)
    expect_true(grepl(as.character(trimming), text))
  })

  it("amplitude and phase corrections text includes correction values", {
    amp1 <- 2; amp2 <- 3; phase1 <- 10; phase2 <- 20
    text <- SeaSondeRAPM_amplitude_and_phase_corrections_step_text(amp1, amp2, phase1, phase2)
    expect_true(grepl(as.character(phase1), text))
    expect_true(grepl(as.character(amp1), text))
  })

  it("phase correction override text includes the new phase values", {
    phases <- c(15, 30)
    text <- SeaSondeRAPM_phase_correction_override_step_text(phases)
    expect_true(grepl(as.character(phases[1]), text))
    expect_true(grepl(as.character(phases[2]), text))
  })

  it("amplitude factors override text includes the new factors", {
    amps <- c(0.5, 0.75)
    text <- SeaSondeRAPM_amplitude_factors_override_step_text(amps)
    expect_true(grepl(as.character(amps[1]), text))
    expect_true(grepl(as.character(amps[2]), text))
  })

  it("SiteOrigin override text includes the new latitude and longitude", {
    site_origin <- c(10, 20)
    text <- SeaSondeRAPM_SiteOrigin_override_step_text(site_origin)
    expect_true(grepl(as.character(site_origin[1]), text))
    expect_true(grepl(as.character(site_origin[2]), text))
  })
})

#### seasonder_readPhaseFile ####
describe("Phase File Reading", {
  it("reads a phase file and returns correct phase values", {
    tf <- tempfile()
    writeLines("45.0 90.0", tf)
    phases <- seasonder_readPhaseFile(tf)
    expect_equal(phases, c(phase1 = 45.0, phase2 = 90.0))
    unlink(tf)
  })
})

#### Setter and Getter Functions ####
describe("Setter and Getter Functions", {
  it("sets and retrieves the Type attribute correctly", {
    calib <- matrix(complex(real = 1:30, imaginary = 31:60), nrow = 3)
    apm <- seasonder_createSeaSondeRAPM(calibration_matrix = calib, BEAR = 1:10)
    apm <- seasonder_setSeaSondeRAPM_Type(apm, "Test Type")
    expect_equal(seasonder_getSeaSondeRAPM_Type(apm), "Test Type")
  })

  it("sets and retrieves the Creator attribute correctly", {
    calib <- matrix(complex(real = 1:30, imaginary = 31:60), nrow = 3)
    apm <- seasonder_createSeaSondeRAPM(calibration_matrix = calib, BEAR = 1:10)
    apm <- seasonder_setSeaSondeRAPM_Creator(apm, "John Doe")
    expect_equal(seasonder_getSeaSondeRAPM_Creator(apm), "John Doe")
  })

  it("sets multiple attributes and passes overall validation", {
    calib <- matrix(complex(real = 1:30, imaginary = 31:60), nrow = 3)
    apm <- seasonder_createSeaSondeRAPM(calibration_matrix = calib, BEAR = 1:10)
    apm <- seasonder_setSeaSondeRAPM_SiteName(apm, "Test Site")
    apm <- seasonder_setSeaSondeRAPM_SiteOrigin(apm, c(10, 20))
    apm <- seasonder_setSeaSondeRAPM_FileName(apm, "file.txt")
    expect_silent(seasonder_validateAttributesSeaSondeRAPM(apm))
  })
})


#### seasonder_readSeaSondeRAPMFile Overrides ####
describe("seasonder_readSeaSondeRAPMFile Overrides", {

  it("overrides antenna_bearing correctly when provided", {
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    override_val <- 90
    obj <- seasonder_readSeaSondeRAPMFile(file_path, override_antenna_bearing = override_val)
    expect_equal(seasonder_getSeaSondeRAPM_AntennaBearing(obj), override_val)
    expect_true(any(grepl("AntennaBearing overriden", seasonder_getSeaSondeRAPM_ProcessingSteps(obj))))
  })

  it("overrides phase_corrections when provided as a valid numeric vector", {
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    override_phase <- c(15, -15)
    obj <- seasonder_readSeaSondeRAPMFile(file_path, override_phase_corrections = override_phase)
    expect_equal(seasonder_getSeaSondeRAPM_PhaseCorrections(obj), override_phase)
    expect_true(any(grepl("PhaseCorrection overriden", seasonder_getSeaSondeRAPM_ProcessingSteps(obj))))
  })

  it("overrides phase_corrections when provided as a valid file path", {
    tf <- tempfile()
    writeLines("30.0 60.0", tf)
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    obj <- seasonder_readSeaSondeRAPMFile(file_path, override_phase_corrections = tf)
    expect_equal(seasonder_getSeaSondeRAPM_PhaseCorrections(obj), c(phase1 = 30.0, phase2 = 60.0))
    expect_true(any(grepl("PhaseCorrection overriden", seasonder_getSeaSondeRAPM_ProcessingSteps(obj))))
    unlink(tf)
  })

  it("does not override phase_corrections when provided an invalid numeric vector", {
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    default_phase <- c(0, 0)  # Valor por defecto definido en seasonder_initializeAttributesSeaSondeRAPM
    obj <- seasonder_readSeaSondeRAPMFile(file_path, override_phase_corrections = c(10))
    expect_equal(seasonder_getSeaSondeRAPM_PhaseCorrections(obj), default_phase)
  })

  it("overrides amplitude_factors when provided a valid numeric vector", {
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    override_amp <- c(1.5, 2.5)
    obj <- seasonder_readSeaSondeRAPMFile(file_path, override_amplitude_factors = override_amp)
    expect_equal(seasonder_getSeaSondeRAPM_AmplitudeFactors(obj), override_amp)
    expect_true(any(grepl("AmplitudeFactors overriden", seasonder_getSeaSondeRAPM_ProcessingSteps(obj))))
  })

  it("does not override amplitude_factors when provided an invalid numeric vector", {
    file_path <- here::here("tests/testthat/data/MeasPattern.txt")
    default_amp <- c(0, 0)  # Valor por defecto
    obj <- seasonder_readSeaSondeRAPMFile(file_path, override_amplitude_factors = c(1))
    expect_equal(seasonder_getSeaSondeRAPM_AmplitudeFactors(obj), default_amp)
  })
})

#### validate_SeaSondeRAPM_StationCode Coverage ####
describe("validate_SeaSondeRAPM_StationCode", {

  it("accepts an empty character vector", {
    expect_silent(validate_SeaSondeRAPM_StationCode(character(0)))
  })

  it("accepts a valid 4-character station code", {
    expect_silent(validate_SeaSondeRAPM_StationCode("ABCD"))
  })

  it("rejects a station code with less than 4 characters", {
    expect_error(validate_SeaSondeRAPM_StationCode("ABC"),
                 "StationCode must have 4 characters")
  })

  it("rejects a station code with more than 4 characters", {
    expect_error(validate_SeaSondeRAPM_StationCode("ABCDE"),
                 "StationCode must have 4 characters")
  })

  it("rejects a station code vector with length > 1", {
    expect_error(validate_SeaSondeRAPM_StationCode(c("ABCD", "EFGH")),
                 "StationCode must have length 0 or 1")
  })

  it("rejects non-character input", {
    expect_error(validate_SeaSondeRAPM_StationCode(123),
                 "StationCode must be a character string")
  })
})

#### validate_SeaSondeRAPM_BEAR Coverage ####
describe("validate_SeaSondeRAPM_BEAR", {

  it("accepts a valid numeric vector within -180 to 180 and matching calibration matrix columns", {
    dummy_obj <- matrix(0, nrow = 3, ncol = 4)
    valid_bear <- c(-90, 0, 90, 180)
    expect_silent(validate_SeaSondeRAPM_BEAR(valid_bear, dummy_obj))
  })

  it("rejects non-numeric BEAR input", {
    dummy_obj <- matrix(0, nrow = 3, ncol = 4)
    expect_error(validate_SeaSondeRAPM_BEAR("not numeric", dummy_obj),
                 "BEAR must be a numeric vector")
  })

  it("rejects a numeric vector with incorrect length", {
    dummy_obj <- matrix(0, nrow = 3, ncol = 4)
    invalid_bear <- c(-90, 0, 90)  # Longitud 3 en lugar de 4
    expect_error(validate_SeaSondeRAPM_BEAR(invalid_bear, dummy_obj),
                 "BEAR must be a numeric vector of length 4")
  })

  it("rejects a numeric vector with values out of the -180 to 180 range", {
    dummy_obj <- matrix(0, nrow = 3, ncol = 2)
    invalid_bear <- c(-200, 50)  # -200 está fuera de rango
    expect_error(validate_SeaSondeRAPM_BEAR(invalid_bear, dummy_obj),
                 "BEAR must be a numeric vector of values between -180 and 180")
  })
})
