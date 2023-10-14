mock_output_factory <- function(...){

  .output <- rlang::list2(...)
  .out_len <-length(.output)
  .i <- 1

  function(...){

    if(.i<=.out_len){

      .out <- .output[[.i]]
      .i <<- .i +1
    }else{
      .out <- NULL
    }

    return(.out)
  }


}
