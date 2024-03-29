here::i_am("tools/doc_compilation/extract_function_docs.R")

devtools::document(roclets = c('rd', 'collate', 'namespace'))

list.files(here::here("tools/doc_compilation"),pattern = "\\.txt$",full.names = T) %>% purrr::walk(file.remove)

doc_path <- here::here("man")

files <- list.files(doc_path,"*.Rd",full.names = T)

# docs <- files %>% purrr::map(\(f) readLines(f) %>% purrr::discard(\(line) stringr::str_detect(line,"^%")) %>% c(glue::glue("Start function {stringr::str_remove(basename(f),'\\\\.Rd$')}"),.,glue::glue("End function {stringr::str_remove(basename(f),'\\\\.Rd$')}"),"")) %>% magrittr::set_names(stringr::str_remove(basename(files),"\\.Rd$"))

docs <- files %>% purrr::map(\(f) readLines(f) %>% purrr::discard(\(line) stringr::str_detect(line,"^%"))) %>% magrittr::set_names(stringr::str_remove(basename(files),"\\.Rd$"))

docs_descriptions <- docs %>% purrr::map(\(doc) paste0(doc,collapse = "") %>% stringr::str_extract("\\\\description\\{(.*?)\\}",group = 1)) %>% purrr::map2(names(.),\(description,doc_name) glue::glue("Function {doc_name}: {description}")) %>% magrittr::set_names(names(docs))



source_files <- list(
  "SeaSondeRAPM" = list(
    "SeaSondeRAPM class"=c("seasonder_createSeaSondeRAPM","seasonder_initializeAttributesSeaSondeRAPM","SeaSondeRAPM_creation_step_text"),
    "Read APM files" = c("seasonder_readSeaSondeRAPMFile","parse_metadata_line","read_matrix_row"),
    "Validate APM" = c("validate_SeaSondeRAPM_AmplitudeFactors","validate_SeaSondeRAPM_AntennaBearing","validate_SeaSondeRAPM_BEAR","validate_SeaSondeRAPM_BearingResolution","validate_SeaSondeRAPM_CommentLine","validate_SeaSondeRAPM_CreateTimeStamp","validate_SeaSondeRAPM_Creator","validate_SeaSondeRAPM_FileID","validate_SeaSondeRAPM_FileName","validate_SeaSondeRAPM_PhaseCorrections","validate_SeaSondeRAPM_ProcessingSteps","validate_SeaSondeRAPM_quality_matrix","validate_SeaSondeRAPM_SiteName","validate_SeaSondeRAPM_SiteOrigin","validate_SeaSondeRAPM_Smoothing","validate_SeaSondeRAPM_StationCode","validate_SeaSondeRAPM_Type","seasonder_validateAttributesSeaSondeRAPM","seasonder_validateCalibrationMatrixSeaSondeRAPM"),
    "APM getters and setters" = c("seasonder_getSeaSondeRAPM_AmplitudeFactors","seasonder_getSeaSondeRAPM_AntennaBearing","seasonder_getSeaSondeRAPM_BEAR","seasonder_getSeaSondeRAPM_BearingResolution","seasonder_getSeaSondeRAPM_CommentLine","seasonder_getSeaSondeRAPM_CreateTimeStamp","seasonder_getSeaSondeRAPM_Creator","seasonder_getSeaSondeRAPM_FileID","seasonder_getSeaSondeRAPM_FileName","seasonder_getSeaSondeRAPM_PhaseCorrections","seasonder_getSeaSondeRAPM_ProcessingSteps","seasonder_getSeaSondeRAPM_quality_matrix","seasonder_getSeaSondeRAPM_SiteName","seasonder_getSeaSondeRAPM_SiteOrigin","seasonder_getSeaSondeRAPM_Smoothing","seasonder_getSeaSondeRAPM_StationCode","seasonder_getSeaSondeRAPM_Type","seasonder_getVersion.SeaSondeRAPM","seasonder_setSeaSondeRAPM_AmplitudeFactors","seasonder_setSeaSondeRAPM_AntennaBearing","seasonder_setSeaSondeRAPM_BEAR","seasonder_setSeaSondeRAPM_BearingResolution","seasonder_setSeaSondeRAPM_CommentLine","seasonder_setSeaSondeRAPM_CreateTimeStamp","seasonder_setSeaSondeRAPM_Creator","seasonder_setSeaSondeRAPM_FileID","seasonder_setSeaSondeRAPM_FileName","seasonder_setSeaSondeRAPM_PhaseCorrections","seasonder_setSeaSondeRAPM_ProcessingSteps","seasonder_setSeaSondeRAPM_quality_matrix","seasonder_setSeaSondeRAPM_SiteName","seasonder_setSeaSondeRAPM_SiteOrigin","seasonder_setSeaSondeRAPM_Smoothing","seasonder_setSeaSondeRAPM_StationCode","seasonder_setSeaSondeRAPM_Type")
  ),
  "SeaSondeRCS" = list(
    "SeaSondeRCS class" = c("seasonder_createSeaSondeRCS","new_SeaSondeRCS","seasonder_getCSHeaderByPath","seasonder_getnDopplerCells","seasonder_getnRangeCells","seasonder_getVersion.SeaSondeRCS","SeaSondeRCS_creation_step_text"),
    "SeaSondeRCS validation"=c("seasonder_validateCSDataStructure","validate_SeaSondeRCS_ProcessingSteps"),
    "SeasondeRCS getters and setters"=c("seasonder_getSeaSondeRCS_ProcessingSteps","seasonder_getVersion.SeaSondeRCS","seasonder_setSeaSondeRCS_ProcessingSteps"),
    "Read CS file" = c("seasonder_readSeaSondeCSFile","seasonder_readSeaSondeCSFileData","seasonder_raw_to_int"),
    "Read CS file header" = c("seasonder_readCSField","seasonder_readSeaSondeCSFileBlock","seasonder_readSeaSondeCSFileHeader","seasonder_readSeaSondeCSFileHeaderV1","seasonder_readSeaSondeCSFileHeaderV2","seasonder_readSeaSondeCSFileHeaderV3","seasonder_readSeaSondeCSFileHeaderV4","seasonder_readSeaSondeCSFileHeaderV5","seasonder_readSeaSondeCSFileHeaderV6","seasonder_int_to_raw","read_and_qc_field","seasonder_check_specs","readV6BlockData","process_version_header","seasonder_validateCSFileData"),
    "Read CS file restarts"=c("seasonder_v6_skip_transformation","seasonder_rerun_qc_with_fun","seasonder_skip_cs_field","seasonder_skip_cs_file")
  ),
  "seasonder_log" = list()
)


library(reticulate)

use_virtualenv("r-reticulate",required = T)

py_run_string(glue::glue("import jiggybase;
                         jiggybase.JiggyBase('{Sys.getenv('JIGGYBASE_KEY')}').collection('SeaSondeR').delete_docs(delete_all=True)"
))


source_files %>% purrr::walk2(names(.),\(sf,sf_name) {


  sf %>% purrr::walk2(names(.),\(fun_group,fun_group_name) {
    f_names <- readLines(here::here(glue::glue("R/{sf_name}.R"))) %>% purrr::keep(\(x) stringr::str_detect(x,"^.*?\\s*?<-\\s*?function")) %>%
      purrr::map_chr(\(x) stringr::str_extract(x,"^(.*?)\\s*?<-\\s*?function",group=1)) %>% purrr::keep(\(f_name) f_name %in% fun_group)

    f_names %>% purrr::walk(\(f_name) {

      doc_path <- here::here(glue::glue("tools/doc_compilation/{f_name}_docs.txt"))
      out <- docs[[f_name]]
      writeLines(out,doc_path)

      doc_description <- glue::glue("Source file: {sf_name}.Doc group: {fun_group_name}. Description: {docs_descriptions[[f_name]]}")
      title <-  glue::glue('R documentation for function {f_name}.')
      py_run_string(glue::glue('import jiggybase;
                           from  jiggybase.models import DocumentMetadata, Source;
                           jiggybase.JiggyBase("{Sys.getenv("JIGGYBASE_KEY")}").collection("SeaSondeR").upsert_file("{doc_path}", metadata=DocumentMetadata(source=Source.file, title="{title}",description="{doc_description}"))'
      ))

    })
  })



})





