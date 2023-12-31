test_that("seasonder_log correctly handles different log levels", {

  expect_silent(seasonder_log("This is an info message", "info"))


  expect_silent(seasonder_log("This is an error message", "error"))


  expect_silent(seasonder_log("This is a fatal message", "fatal"))

  # Test that non-standard log levels are not allowed
  expect_error(seasonder_log("This is a non-standard message", "non-standard"))
})

test_that("seasonder_log correctly logs messages", {
  # Test that the log message is correct for info level
  expect_condition(seasonder_log("This is an info message", "info"), "This is an info message")

  # Test that the log message is correct for error level
  expect_condition(seasonder_log("This is an error message", "error"), "This is an error message")

  # Test that the log message is correct for fatal level
  expect_condition(seasonder_log("This is a fatal message", "fatal"), "This is a fatal message")
})



# Test 1: Check if the info logs are written correctly
test_that("Info logs are written correctly", {

  seasonder_log(level = "info", message = "This is an information")

  # Clear the existing log file or create a new one
  log_file_path <- tempfile()
  write("", log_file_path)

  # Call the function
  seasonder_logArchiver(log_info_path = log_file_path)

  # Read the log file
  log_content <- readLines(log_file_path)

  # Check if the log file contains the correct content
  expect_true(any(grepl("\\[INFO\\].*This is an information", log_content)))
  unlink(log_file_path)

})

# Similar tests can be written for "error" and "fatal" levels

# Test 2: Check if the error logs are written correctly
test_that("Error logs are written correctly", {

  seasonder_log(level = "error", message = "This is an information")

  # Clear the existing log file or create a new one
  log_file_path <- tempfile()
  write("", log_file_path)

  # Call the function
  seasonder_logArchiver(log_error_path = log_file_path)

  # Read the log file
  log_content <- readLines(log_file_path)

  # Check if the log file contains the correct content
  expect_true(any(grepl("\\[ERROR\\].*This is an information", log_content)))
  unlink(log_file_path)
})

# Test 3: Check if the fatal logs are written correctly
test_that("Fatal logs are written correctly", {

  seasonder_log(level = "fatal", message = "This is an information")

  # Clear the existing log file or create a new one
  log_file_path <- tempfile()
  write("", log_file_path)

  # Call the function
  seasonder_logArchiver(log_fatal_path = log_file_path)

  # Read the log file
  log_content <- readLines(log_file_path)

  # Check if the log file contains the correct content
  expect_true(any(grepl("\\[FATAL\\].*This is an information", log_content)))
  unlink(log_file_path)
})






#### seasonder_the ####

test_that("seasonder_enableLogs sets logs_enabled to TRUE", {
  seasonder_disableLogs()
  expect_false(seasonder_areLogsEnabled())
  seasonder_enableLogs()
  expect_true(seasonder_areLogsEnabled())
})

test_that("seasonder_disableLogs sets logs_enabled to FALSE", {
  seasonder_enableLogs()
  expect_true(seasonder_areLogsEnabled())
  seasonder_disableLogs()
  expect_false(seasonder_areLogsEnabled())
})

test_that("seasonder_areLogsEnabled returns correct status", {
  seasonder_enableLogs()
  expect_true(seasonder_areLogsEnabled())
  seasonder_disableLogs()
  expect_false(seasonder_areLogsEnabled())
})


#### seasonder_logAndMessage ####

describe("seasonder_logAndMessage",{

  it("behaves as expected", {

    msg_en_mk <-  mockthat::mock(TRUE)
    log_en_mk <-  mockthat::mock(FALSE)
    log_mk <- mockthat::mock(NULL)

    # Test 1: When messages are enabled but logs are not
    mockthat::with_mock(
      seasonder_areMessagesEnabled = msg_en_mk,
      seasonder_areLogsEnabled = log_en_mk,
      seasonder_log = log_mk,
      {
        msg <- "This is a test message."
        suppressMessages(seasonder_logAndMessage(msg))
        expect_message(seasonder_logAndMessage(msg), "This is a test message.")
        expect_equal(mockthat::mock_n_called(log_mk),0) # Expect no calls to seasonder_log
      }
    )

    msg_en_mk <-  mockthat::mock(FALSE)
    log_en_mk <-  mockthat::mock(TRUE)
    log_mk <- mockthat::mock(NULL)
    # Test 2: When logs are enabled but messages are not
    mockthat::with_mock(
      seasonder_areMessagesEnabled = msg_en_mk,
      seasonder_areLogsEnabled = log_en_mk,
      seasonder_log = log_mk,
      {
        msg <- "This is a test message."
        seasonder_logAndMessage(msg)
        expect_equal(mockthat::mock_n_called(log_mk),1) # Expect one call to seasonder_log
        expect_no_message(seasonder_logAndMessage(msg))

      }
    )

    msg_en_mk <-  mockthat::mock(TRUE)
    log_en_mk <-  mockthat::mock(TRUE)
    log_mk <- mockthat::mock(NULL)
    # Test 3: When both messages and logs are enabled
    mockthat::with_mock(
      seasonder_areMessagesEnabled = msg_en_mk,
      seasonder_areLogsEnabled = log_en_mk,
      seasonder_log = log_mk,
      {
        msg <- "This is a test message."
        suppressMessages(seasonder_logAndMessage(msg))
        expect_equal(mockthat::mock_n_called(log_mk),1) # Expect one call to seasonder_log
        expect_message(seasonder_logAndMessage(msg), "This is a test message.")

      }
    )

    msg_en_mk <-  mockthat::mock(FALSE)
    log_en_mk <-  mockthat::mock(FALSE)
    log_mk <- mockthat::mock(NULL)
    # Test 4: When neither messages nor logs are enabled
    mockthat::with_mock(
      seasonder_areMessagesEnabled = msg_en_mk,
      seasonder_areLogsEnabled = log_en_mk,
      seasonder_log = log_mk,
      {
        msg <- "This is a test message."
        seasonder_logAndMessage(msg)
        expect_no_message(seasonder_logAndMessage(msg))
        expect_equal(mockthat::mock_n_called(log_mk),0) # Expect no calls to seasonder_log
      }
    )
  })

  describe("whan calling it inside tryCatch",{

  expect_warning(tryCatch(rlang::abort("stop"),error=function(e) seasonder_logAndMessage("An error happened.","error")),"An error happened.")


  })

})




