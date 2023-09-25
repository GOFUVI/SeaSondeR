#' @export
create_SeaSondeCS <- function(){

}

#' @export
seasonder_readSeaSondeCSFile <- function(filepath){


  # Crear conexión y manejar endianidad
  ...

  # Leer encabezado
  header <- seasonder_readSeaSondeCSFileHeader(connection)

  # Leer datos (podrías tener otro helper para esto)
  data <- seasonder_readSeaSondeCSFileData(connection, header)

  # Retornar lista con encabezado y datos
  list(header = header, data = data)

}

seasonder_readSeaSondeCSFileHeader <- function(connection){

}

seasonder_readSeaSondeCSFileData <- function(connection,header){

}



seasonder_int_to_raw <- function(x){

  out <- as.raw(packBits(c(t(matrix(as.integer(strsplit(bit64::as.bitstring(bit64::as.integer64(x)),"")[[1]]),ncol=8,byrow = T)[,8:1])),"raw"))

  out

}

#' Convert a Raw Vector to a 64-bit Integer
#'
#' This function converts a raw vector to a 64-bit integer,
#' handling both signed and unsigned conversions.
#'
#' @param r A raw vector to be converted.
#' @param signed Logical, indicating whether the conversion should consider the value as signed (default is FALSE for unsigned).
#' @return A 64-bit integer representation of the raw vector.
#' @examples
#' r <- as.raw(c(0x12,0x34,0x56,0x78,0x90,0xAB,0xCD,0xEF))
#' seasonder_raw_to_int(r, signed=TRUE)
seasonder_raw_to_int <- function(r,signed=F){
  # Convert raw values to bits and collapse into a single bit string.
  bit_str <- sapply(r,FUN = function(x) rev(rawToBits(x))) %>% as.integer() %>% paste0(collapse="")
  class(bit_str) <-"bitstring"

  # Convert the bit string into a 64-bit integer.
  int_val <-   bit64::as.integer64(bit_str)

  return(int_val)

}

#' Read a CSField from a Binary Connection
#'
#' This function reads specific data types from a binary connection,
#' supporting various types including integer, float, double, complex, and strings.
#'
#' @param con A connection object to a binary file.
#' @param type A character string identifying the type of data to read.
#' @param endian A character string indicating the byte order (default is "big").
#' @return The value read from the connection.
#' @examples
#' con <- rawConnection(as.raw(c(0x12)))
#' seasonder_readCSField(con, "UInt8")
#' @export
#' @seealso seasonder_raw_to_int For converting raw to 64-bit integers.
seasonder_readCSField <- function(con, type, endian="big") {

  # Check if the connection is open. If not, raise an error.
  open_con <- try(isOpen(con),silent = T)
  if (!inherits(open_con,"try-error")) {
    if(!open_con){
      seasonder_logAndAbort("Connection is not open.")
    }
  }else{
    seasonder_logAndAbort("Connection is not open.")
  }

  # Helper function to read values from the connection. This function takes care of potential
  # reading errors and uses a specific format and byte size to read the values.
  read_values <- function(bytes, format, n=1) {
    res <- tryCatch(readBin(con, what=format, n=n, size=bytes, endian=endian),
                    error = function(e) {
                      seasonder_logAndMessage("Error while reading value.", "error")
                      NULL
                    })
    return(res)
  }

  # Determine the data type specified and read it from the connection accordingly.
  # If the data type is not recognized, raise an error.
  switch(type,
         "UInt8"   = as.integer(read_values(1, "raw")),
         "SInt8"   = as.integer(read_values(1, "integer")),
         "UInt16"  = as.integer(read_values(2, "int")),
         "SInt16"  = as.integer(read_values(2, "int")),
         "UInt32"  = as.integer(read_values(4, "int")),
         "SInt32"  = as.integer(read_values(4, "int")),
         "Float"   = as.numeric(read_values(4, "double")),
         "Double"  = as.numeric(read_values(8, "double")),
         "UInt64"  = {
           v <- read_values(1, "raw", n=8)
           seasonder_raw_to_int(v,signed=FALSE)
         },
         "SInt64"  = {
           v <- read_values(1, "raw", n=8)
           seasonder_raw_to_int(v,signed=TRUE)
         },
         "Complex" = {
           # Read real and imaginary parts separately and then combine them into a complex number.
           real_part <- as.numeric(read_values(4, "double"))
           imag_part <- as.numeric(read_values(4, "double"))
           complex(real = real_part, imaginary = imag_part)
         },
         "String"  = {
           # Read characters until a null terminator (0) is encountered.
           chars <- NULL
           repeat {
             char <- read_values(1, "raw")
             if (char == as.raw(0)) break
             chars <- c(chars, char)
           }
           rawToChar(do.call(c, list(chars)))
         },
         {
           # Handle unrecognized data types.
           seasonder_logAndMessage(glue::glue("Type Unknown: '{type}'."),"error")
           return(NULL)
         }
  )
}
