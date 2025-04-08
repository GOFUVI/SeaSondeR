test_that("Related functions exist", {
  # Check if the function 'seasonder_readYAMLSpecs' exists and is a function
  expect_true(is.function(seasonder_readYAMLSpecs))
})

describe("seasonder_readYAMLSpecs", {
  # Create a temporary directory to store test YAML files
  temp_dir <- tempfile(pattern = "test_yaml_")
  dir.create(temp_dir)
  # Ensure that the temporary directory is removed after the tests run
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)  # Clean up after tests

  # Function to create a test YAML file with provided file name and content
  create_test_yaml <- function(file_name, content) {
    file_path <- file.path(temp_dir, file_name)
    # Write the YAML content to the file
    writeLines(as.character(yaml::as.yaml(content)), file_path)
    file_path
  }

  # Define valid YAML content for testing purposes
  valid_content <- list(
    metadata = list(version = "1.0.0", type = "SeaSondeCS"),
    header = list(
      general = list(field1 = "type1", field2 = "type2"),
      versions = list(
        V1 = list(field3 = "type3"),
        V2 = list(field4 = "type4")
      )
    )
  )

  # Create an invalid version content by modifying the metadata version
  invalid_version_content <- modifyList(valid_content, list(metadata = list(version = "0.9.0")))

  it("returns error when YAML file does not exist", {
    # Expect an error when attempting to read a non-existent YAML file
    expect_error(seasonder_readYAMLSpecs("non_existent.yaml", c("header", "general")),
                 "File 'non_existent.yaml' not found.",
                 class = "seasonder_read_yaml_file_error")
  })

  it("can extract the general header", {
    # Create a valid YAML file using the valid content
    file_path <- create_test_yaml("valid.yaml", valid_content)
    # Extract the 'general' header section from the YAML file
    result <- seasonder_readYAMLSpecs(file_path, c("header", "general"))
    # Check that the result has the expected names
    expect_named(result, c("field1", "field2"))
    # Verify that the value of 'field1' is as expected
    expect_equal(result$field1, "type1")
  })

  it("can extract version-specific header fields", {
    # Create a valid YAML file using the valid content
    file_path <- create_test_yaml("valid.yaml", valid_content)
    # Extract the header for version V1 from the YAML file
    result <- seasonder_readYAMLSpecs(file_path, c("header", "versions", "V1"))
    # Check that the result is named correctly
    expect_named(result, "field3")
    # Verify that the value of 'field3' is as expected
    expect_equal(result$field3, "type3")
  })

  it("returns error for invalid path", {
    # Create a valid YAML file using the valid content
    file_path <- create_test_yaml("valid.yaml", valid_content)
    # Expect an error when trying to extract a non-existent path from the YAML file
    expect_error(seasonder_readYAMLSpecs(file_path, c("header", "nonexistent")),
                 glue::glue("Invalid specs path 'header' for file '{file_path}'."),
                 class = "seasonder_read_yaml_file_error")
  })

  it("returns error for invalid YAML content", {
    # Create a YAML file with invalid content
    file_path <- create_test_yaml("invalid_content.yaml", "invalid: yaml:: content")
    # Expect an error due to invalid YAML structure in the file
    expect_error(seasonder_readYAMLSpecs(file_path, c("header", "general")),
                 glue::glue("Invalid YAML structure in file '{file_path}'."),
                 class = "seasonder_read_yaml_file_error")
  })
})

describe("determining spectra file type", {
  describe("CS", {
    # Define file path for a CS file example using the 'here' package
    filepath <- here::here("tests/testthat/data/TORA/CSS_TORA_24_04_04_0640.cs")
    endian <- "big"

    describe("seasonder_find_spectra_file_type", {
      it("should return CS", {
        # Call the function and expect it to return "CS" for the given file
        test <- seasonder_find_spectra_file_type(filepath, endian)
        expect_equal(test, "CS")
      })
    })

    describe("seasonder_defaultSpecsPathForFile", {
      it("should return the default specs file for CS", {
        # Get the expected default specs file path for CS
        target <- seasonder_defaultSpecsFilePath("CS")
        # Mock the function to force the return of "CS" and then test the default specs path function
        test <- mockthat::with_mock(
          seasonder_find_spectra_file_type = function(filepath, endian) "CS",
          seasonder_defaultSpecsPathForFile(filepath, endian = endian)
        )
        expect_equal(test, target)
      })
    })
  })

  describe("CSSY", {
    # Define file path for a CSSY file example using the 'here' package
    filepath <- here::here("tests/testthat/data/SUNS/CSS/CSS_SUNS_2025_02_17_060000.csr")
    endian <- "big"

    describe("seasonder_find_spectra_file_type", {
      it("should return CSSY", {
        # Call the function and expect it to return "CSSY" for the given file
        test <- seasonder_find_spectra_file_type(filepath, endian)
        expect_equal(test, "CSSY")
      })
    })

    describe("seasonder_defaultSpecsPathForFile", {
      it("should return the default specs file for CSSY", {
        # Get the expected default specs file path for CSSY
        target <- seasonder_defaultSpecsFilePath("CSSY")
        # Mock the function to force the return of "CSSY" and then test the default specs path function
        test <- mockthat::with_mock(
          seasonder_find_spectra_file_type = function(filepath, endian) "CSSY",
          seasonder_defaultSpecsPathForFile(filepath, endian = endian)
        )
        expect_equal(test, target)
      })
    })
  })
})
