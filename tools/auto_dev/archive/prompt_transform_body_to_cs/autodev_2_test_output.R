
library(testthat)

# First, check that the function exists
test_that("seasonder_CSSY2CSData exists", {
  expect_true(exists("seasonder_CSSY2CSData", mode = "function"), info = "seasonder_CSSY2CSData must be defined")
})

# Group tests using describe/it blocks

describe("seasonder_CSSY2CSData transformation tests", {

  it("transforms a single cell correctly", {
    # Create a single cell with required fields
    cell <- list(
      indx  = list(index = 1),
      cs1a  = c(1, 2, 3),
      cs2a  = c(4, 5, 6),
      cs3a  = c(7, 8, 9),
      c12r  = c(10, 11, 12),
      c12i  = c(13, 14, 15),
      c13r  = c(16, 17, 18),
      c13i  = c(19, 20, 21),
      c23r  = c(22, 23, 24),
      c23i  = c(25, 26, 27),
      csqf  = c(28, 29, 30)
    )
    # The body is a list containing one cell
    body <- list(cell)

    # Invoke the function to transform the CSSY body structure
    result <- seasonder_CSSY2CSData(body)

    # Check that result is a list with required keys
    required_keys <- c("SSA1", "SSA2", "SSA3", "CS12", "CS13", "CS23", "QC")
    expect_true(all(required_keys %in% names(result)), info = "Result must contain required keys")

    # Verify that each output is a matrix
    lapply(required_keys, function(key) {
      expect_true(is.matrix(result[[key]]), info = paste0(key, " should be a matrix"))
    })

    # For self-spectra, the row corresponding to index 1 must equal the input vector value
    expect_equal(result$SSA1[1, ], cell$cs1a, info = "SSA1 should equal cs1a for cell at index 1")
    expect_equal(result$SSA2[1, ], cell$cs2a, info = "SSA2 should equal cs2a for cell at index 1")
    expect_equal(result$SSA3[1, ], cell$cs3a, info = "SSA3 should equal cs3a for cell at index 1")

    # For cross spectra, check formation of complex numbers
    expected_CS12 <- complex(real = cell$c12r, imaginary = cell$c12i)
    expected_CS13 <- complex(real = cell$c13r, imaginary = cell$c13i)
    expected_CS23 <- complex(real = cell$c23r, imaginary = cell$c23i)
    expect_equal(result$CS12[1, ], expected_CS12, info = "CS12 should be formed from c12r and c12i")
    expect_equal(result$CS13[1, ], expected_CS13, info = "CS13 should be formed from c13r and c13i")
    expect_equal(result$CS23[1, ], expected_CS23, info = "CS23 should be formed from c23r and c23i")

    # Check quality control matrix (QC) equals csqf
    expect_equal(result$QC[1, ], cell$csqf, info = "QC should equal csqf")
  })

  it("transforms multiple cells correctly", {
    # Create two cells with different indices and 4-element vectors
    cell1 <- list(
      indx  = list(index = 1),
      cs1a  = c(1,2,3,4),
      cs2a  = c(5,6,7,8),
      cs3a  = c(9,10,11,12),
      c12r  = c(13,14,15,16),
      c12i  = c(17,18,19,20),
      c13r  = c(21,22,23,24),
      c13i  = c(25,26,27,28),
      c23r  = c(29,30,31,32),
      c23i  = c(33,34,35,36),
      csqf  = c(37,38,39,40)
    )
    cell2 <- list(
      indx  = list(index = 2),
      cs1a  = c(101,102,103,104),
      cs2a  = c(105,106,107,108),
      cs3a  = c(109,110,111,112),
      c12r  = c(113,114,115,116),
      c12i  = c(117,118,119,120),
      c13r  = c(121,122,123,124),
      c13i  = c(125,126,127,128),
      c23r  = c(129,130,131,132),
      c23i  = c(133,134,135,136),
      csqf  = c(137,138,139,140)
    )
    body <- list(cell1, cell2)

    # Call the transformation function
    result <- seasonder_CSSY2CSData(body)

    # Expected dimensions: matrix should have rows equal to the highest cell index (here 2) and columns equal to vector lengths (4)
    expected_dim <- c(2, 4)

    required_keys <- c("SSA1", "SSA2", "SSA3", "CS12", "CS13", "CS23", "QC")
    lapply(required_keys, function(key) {
      expect_equal(dim(result[[key]]), expected_dim, info = paste0(key, " should have dimensions 2 x 4"))
    })

    # Check self spectra: row 1 should correspond to cell1, row 2 to cell2
    expect_equal(result$SSA1[1, ], cell1$cs1a, info = "SSA1 row1 should equal cs1a of cell1")
    expect_equal(result$SSA1[2, ], cell2$cs1a, info = "SSA1 row2 should equal cs1a of cell2")

    # Check cross spectra complex formation
    expected_CS12_cell1 <- complex(real = cell1$c12r, imaginary = cell1$c12i)
    expected_CS12_cell2 <- complex(real = cell2$c12r, imaginary = cell2$c12i)
    expect_equal(result$CS12[1, ], expected_CS12_cell1, info = "CS12 row1 should be formed correctly")
    expect_equal(result$CS12[2, ], expected_CS12_cell2, info = "CS12 row2 should be formed correctly")

    expected_CS13_cell1 <- complex(real = cell1$c13r, imaginary = cell1$c13i)
    expected_CS13_cell2 <- complex(real = cell2$c13r, imaginary = cell2$c13i)
    expect_equal(result$CS13[1, ], expected_CS13_cell1, info = "CS13 row1 should be formed correctly")
    expect_equal(result$CS13[2, ], expected_CS13_cell2, info = "CS13 row2 should be formed correctly")

    expected_CS23_cell1 <- complex(real = cell1$c23r, imaginary = cell1$c23i)
    expected_CS23_cell2 <- complex(real = cell2$c23r, imaginary = cell2$c23i)
    expect_equal(result$CS23[1, ], expected_CS23_cell1, info = "CS23 row1 should be formed correctly")
    expect_equal(result$CS23[2, ], expected_CS23_cell2, info = "CS23 row2 should be formed correctly")

    # Check QC matrix
    expect_equal(result$QC[1, ], cell1$csqf, info = "QC row1 should equal csqf of cell1")
    expect_equal(result$QC[2, ], cell2$csqf, info = "QC row2 should equal csqf of cell2")
  })
})
