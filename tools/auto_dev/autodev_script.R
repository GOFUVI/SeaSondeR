library(magrittr)

local({

  log_file <-  file.path(auto_dev_folder, "log.txt")

  if(!file.exists(log_file)){
    file.create(log_file)
  }
  sink(log_file)

  on.exit(sink())





  code_file <- file.path(auto_dev_folder, "code.R")

  tests_file <- file.path(auto_dev_folder, "tests.R")

  tests_output_file <- file.path(auto_dev_folder, "test_output.txt")

  output_code_file <- file.path(auto_dev_folder, "autodev_code_output.R")

  output_test_file <- file.path(auto_dev_folder, "autodev_test_output.R")



  prompt <- readLines(prompt_file) %>% paste0(collapse = "\n")

  # files_to_process <- list.files(here::here("R"),pattern = "*\\.R", full.names = T) %>% purrr::discard(\(x) basename(x) %in%c())



  if(!file.exists(input_code_file)){
    file.create(input_code_file)
  }

  if(!file.exists(input_test_file)){
    file.create(input_test_file)
  }

  if(!file.exists(output_code_file)){
    file.create(output_code_file)
  }


  if(!file.exists(output_test_file)){
    file.create(output_test_file)
  }




  unit_testing_agent <- TeamFunctionBuilder::tfb_get_unit_testing_agent(force_update = F)
  coding_agent       <- TeamFunctionBuilder::tfb_get_coding_agent(force_update = F)
  doc_agent         <- TeamFunctionBuilder::tfb_get_doc_agent(force_update = F)

  coordinator_agent <- TeamFunctionBuilder::tfb_create_tdd_coordinator_agent(unit_testing_agent = unit_testing_agent,
                                                                             coding_agent = coding_agent,
                                                                             doc_agent = doc_agent,
                                                                             force_update = F)

  result <- try({


    TeamFunctionBuilder::tfb_tdd_coordinator_agent_develop_from_files(coordinator_agent,
                                                                      prompt = prompt,
                                                                      input_test_file = input_test_file,
                                                                      input_code_file = input_code_file,
                                                                      output_test_file = output_test_file,
                                                                      output_code_file = output_code_file,

                                                                      temp_tests_file = tests_file,
                                                                      temp_code_file = code_file,
                                                                      temp_tests_output_file = tests_output_file,
                                                                      thread_options = list(tool_resources = list(file_search = list(vector_store_ids = list(task$vector_store_ids)))))

  })

  file.copy(task_file,to = tasks_to_review_folder)
  file.remove(task_file)


  files_to_commit <- file.path(glue::glue("tools/auto_dev/{task_name}"),list.files(auto_dev_folder))
  files_to_commit <- c(files_to_commit, glue::glue("tools/auto_dev/tasks_to_do/{basename(task_file)}"))
  files_to_commit <- c(files_to_commit, glue::glue("tools/auto_dev/tasks_to_review/{basename(task_file)}"))

  gert::git_add(files = files_to_commit)

  gert::git_commit(glue::glue("{task_name}"))

  gert::git_push()
})
