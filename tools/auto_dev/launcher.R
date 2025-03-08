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



input_code_file <- here::here("R/SeaSondeRCSSY.R")

prompt_files <- list.files(here::here("tools/auto_dev/tasks_to_do/"),pattern = "\\.txt$",full.names = T)



for(prompt_file in prompt_files){

  prompt_name <- tools::file_path_sans_ext(basename(prompt_file))
  auto_dev_folder <- file.path(here::here("tools/auto_dev/"),prompt_name)

  unlink(auto_dev_folder,recursive = T)
  if(!dir.exists(auto_dev_folder)){
    dir.create(auto_dev_folder)
  }

  rstudioapi::jobRunScript(here::here("tools/auto_dev/autodev_script.R"),workingDir = here::here(),importEnv = T,name = prompt_name)



}
