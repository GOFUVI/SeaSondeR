test_that("Related functions are defined",{

  expect_true(is.function(seasonder_readCSField))

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

describe("Tests for seasonder_readCSField", {



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


})
