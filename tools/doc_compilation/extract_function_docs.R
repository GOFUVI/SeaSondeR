here::i_am("tools/doc_compilation/extract_function_docs.R")


doc_path <- here::here("man")

files <- list.files(doc_path,"*.Rd",full.names = T)

docs <- files %>% purrr::map(\(f) readLines(f) %>% purrr::discard(\(line) stringr::str_detect(line,"^%")) %>% c(glue::glue("Start function {stringr::str_remove(basename(f),'\\\\.Rd$')}"),.,glue::glue("End function {stringr::str_remove(basename(f),'\\\\.Rd$')}"),"")) %>% magrittr::set_names(stringr::str_remove(basename(files),"\\.Rd$"))


source_files <- c("SeaSondeRAPM","SeaSondeRCS")

source_files %>% purrr::walk(\(sf){

  f_names <- readLines(here::here(glue::glue("R/{sf}.R"))) %>% purrr::keep(\(x) stringr::str_detect(x,"^.*?\\s*?<-\\s*?function")) %>%
    purrr::map_chr(\(x) stringr::str_extract(x,"^(.*?)\\s*?<-\\s*?function",group=1))

  out <- docs[f_names] %>% purrr::compact() %>% unlist()

  writeLines(out,here::here(glue::glue("tools/doc_compilation/{sf}_docs.txt")))
})
