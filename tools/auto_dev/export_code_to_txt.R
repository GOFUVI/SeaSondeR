here::i_am("tools/auto_dev/export_code_to_txt.R")

files <- list.files(here::here("R"), full.names = T)

out_path <- here::here("tools/auto_dev/vector_store/")
if(!dir.exists(out_path)){
  dir.create(out_path)
}

purrr::walk(files,\(file) file.copy(file, file.path(out_path, paste0(tools::file_path_sans_ext(basename(file)),".txt")),overwrite = T))
