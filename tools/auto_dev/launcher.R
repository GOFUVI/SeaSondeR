rm(list=ls())

here::i_am("tools/auto_dev/launcher.R")
# library(testthat)
library(magrittr)
devtools::document()
devtools::load_all(".")


current_branch <- gert::git_branch()

if(current_branch != "auto_dev"){
  if(!"auto_dev" %in% gert::git_branch_list()$name){
    gert::git_branch_create("auto_dev")
  }else{
    gert::git_branch_checkout("auto_dev")
  }
}

tasks_to_do_folder <- here::here("tools/auto_dev/tasks_to_do/")
tasks_to_review_folder <- here::here("tools/auto_dev/tasks_to_review/")
tasks_done_folder <- here::here("tools/auto_dev/tasks_done/")

input_code_file <- here::here("R/SeaSondeRCSSY.R")

prompt_files <- list.files(tasks_to_do_folder,pattern = "\\.txt$",full.names = T)
files_changed <- TRUE
while(TRUE){


if(files_changed){
for(prompt_file in prompt_files){

  prompt_name <- tools::file_path_sans_ext(basename(prompt_file))

  cat(glue::glue("{Sys.time()}: launching task {prompt_name}\n\n"))
  auto_dev_folder <- file.path(here::here("tools/auto_dev/"),prompt_name)

  unlink(auto_dev_folder,recursive = T)
  if(!dir.exists(auto_dev_folder)){
    dir.create(auto_dev_folder)
  }

  rstudioapi::jobRunScript(here::here("tools/auto_dev/autodev_script.R"),workingDir = here::here(),importEnv = T,name = prompt_name)



}
  files_changed <- FALSE
}
Sys.sleep(20)
cat(glue::glue("{Sys.time()}: checking for new tasks\n\n"))
gert::git_pull()

new_to_do <- setdiff( list.files(tasks_to_do_folder,pattern = "\\.txt$",full.names = T),prompt_files)

if(length(new_to_do) >0){
  files_changed <- T
  prompt_files <- new_to_do
}

}
