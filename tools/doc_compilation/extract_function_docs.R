here::i_am("tools/doc_compilation/extract_function_docs.R")


doc_path <- here::here("man")

files <- list.files(doc_path,"*.Rd",full.names = T)

docs <- files %>% purrr::map(\(f) readLines(f) %>% purrr::discard(\(line) stringr::str_detect(line,"^%")) %>% c(glue::glue("Start {basename(f)}"),.,glue::glue("End {basename(f)}"),"")) %>% unlist()

writeLines(docs,here::here("tools/doc_compilation/function_docs.txt"))
