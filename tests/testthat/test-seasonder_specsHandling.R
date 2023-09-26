test_that("Related functions exist",{

  expect_true(is.function(seasonder_readYAMLSpecs))

})


describe("seasonder_readYAMLSpecs", {

  temp_dir <- tempfile(pattern = "test_yaml_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)  # Para limpiar después de los tests

  # Creando un YAML de ejemplo
  create_test_yaml <- function(file_name, content) {
    file_path <- file.path(temp_dir, file_name)
    writeLines(as.character(yaml::as.yaml(content)), file_path)
    file_path
  }

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

  invalid_version_content <- modifyList(valid_content, list(metadata = list(version = "0.9.0")))

  it("returns error when YAML file does not exist", {
    expect_error(seasonder_readYAMLSpecs("non_existent.yaml", c("header", "general")),
                 "File not found.")
  })

  it("returns error for invalid YAML version", {
    file_path <- create_test_yaml("invalid_version.yaml", invalid_version_content)
    expect_error(seasonder_readYAMLSpecs(file_path, c("header", "general")),
                 "Unsupported version.")
  })

  it("can extract the general header", {
    file_path <- create_test_yaml("valid.yaml", valid_content)
    result <- seasonder_readYAMLSpecs(file_path, c("header", "general"))
    expect_named(result, c("field1", "field2"))
    expect_equal(result$field1, "type1")
  })

  it("can extract version-specific header fields", {
    file_path <- create_test_yaml("valid.yaml", valid_content)
    result <- seasonder_readYAMLSpecs(file_path, c("header", "versions", "V1"))
    expect_named(result, "field3")
    expect_equal(result$field3, "type3")
  })

  it("returns error for invalid path", {
    file_path <- create_test_yaml("valid.yaml", valid_content)
    expect_error(seasonder_readYAMLSpecs(file_path, c("header", "nonexistent")),
                 "Invalid path.")
  })

  it("returns error for invalid YAML content", {
    file_path <- create_test_yaml("invalid_content.yaml", "invalid: yaml:: content")
    expect_error(seasonder_readYAMLSpecs(file_path, c("header", "general")),
                 "Invalid YAML structure.")
  })



})
