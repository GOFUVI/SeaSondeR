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



task_files <- list.files(tasks_to_do_folder,pattern = "\\.yml$",full.names = T)
files_changed <- TRUE
while(TRUE){


  if(files_changed){
    for(task_file in task_files){


      task <- yaml::read_yaml(task_file)

      task_name <- task$name



      cat(glue::glue("{Sys.time()}: launching task {task_name}\n\n"))
      auto_dev_folder <- file.path(here::here("tools/auto_dev/"),task_name)
      unlink(auto_dev_folder,recursive = T)


      if(!dir.exists(auto_dev_folder)){
        dir.create(auto_dev_folder)
      }

      prompt_file <- here::here(task$prompt_file)
      file.copy(prompt_file,auto_dev_folder)

      input_code_file <- file.path(auto_dev_folder, "autodev_code_input.R")

      input_test_file <- file.path(auto_dev_folder, "autodev_test_input.R")
      input_code <- ""
      if(!is.null(task$input_code_files)){
        input_code <- purrr::map_chr(task$input_code_files, \(file) readLines(file) %>% paste0(collapse = "\n")) %>% paste0(collapse = "\n")
      }
      write(input_code, input_code_file)

      input_test_code <- ""
      if(!is.null(task$input_test_files)){
        input_test_code <- purrr::map_chr(task$input_test_files, \(file) readLines(file) %>% paste0(collapse = "\n")) %>% paste0(collapse = "\n")
      }
      write(input_test_code, input_test_file)



      rstudioapi::jobRunScript(here::here("tools/auto_dev/autodev_script.R"),workingDir = here::here(),importEnv = T,name = task_name)



    }
    files_changed <- FALSE
  }
  Sys.sleep(20)
  cat(glue::glue("{Sys.time()}: checking for new tasks\n\n"))
  gert::git_pull()


  task_files <- purrr::discard(task_files, \(x) basename(x) %in% c(list.files(tasks_to_review_folder,pattern = "\\.yml$",full.names = F),list.files(tasks_done_folder,pattern = "\\.yml$",full.names = F)))

  new_to_do <- setdiff( list.files(tasks_to_do_folder,pattern = "\\.yml$",full.names = T),task_files)


  if(length(new_to_do) >0){
    files_changed <- T
    task_files <- new_to_do
  }

}
