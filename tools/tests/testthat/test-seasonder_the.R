test_that("seasonder_enableMessages sets messages_enabled to TRUE", {
  seasonder_disableMessages()
  expect_false(seasonder_areMessagesEnabled())
  seasonder_enableMessages()
  expect_true(seasonder_areMessagesEnabled())
})

test_that("seasonder_disableMessages sets messages_enabled to FALSE", {
  seasonder_enableMessages()
  expect_true(seasonder_areMessagesEnabled())
  seasonder_disableMessages()
  expect_false(seasonder_areMessagesEnabled())
})

test_that("seasonder_areMessagesEnabled returns correct status", {
  seasonder_enableMessages()
  expect_true(seasonder_areMessagesEnabled())
  seasonder_disableMessages()
  expect_false(seasonder_areMessagesEnabled())
})

