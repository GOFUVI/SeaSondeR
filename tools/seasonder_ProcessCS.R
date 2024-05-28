

seasonder_ProcessCSRuntime <- function( file_to_read, APMs_root_folder) {



  if(file.info(file_to_read)$size == 0){
    stop(paste0("File: '",file_to_read,"' has size 0."))
  }

    cs <- SeaSondeR::seasonder_createSeaSondeRCS(file_to_read)

    header <- SeaSondeR::seasonder_getSeaSondeRCS_header(cs)



    if(header$nSiteCodeName %in% c("VILA","PRIO")){


      FOS <-   list(nsm = 4,
                    fdown = 10^(10/10),
                    flim = 10^(20/10),
                    noisefact = 10^(6/10),
                    currmax = 2,
                    reject_distant_bragg = TRUE, #  Default is to apply this test
                    reject_noise_ionospheric = TRUE, #  Default is to apply this test (except for 42 MHz)

                    reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
      )


    }

    cs <- SeaSondeR::seasonder_computeFORs(cs, method = "SeaSonde", FOR_control = FOS)

    FOR <- SeaSondeR::seasonder_SeaSondeRCSExportFORBoundaries(cs)

    APM_folder <- file.path(APMs_root_folder,header$nSiteCodeName)

    cs_date_time <- header$nDateTime

    APM_files <- list.files(APM_folder,full.names = T)
    APM_files <- sort(APM_files, decreasing = F)

    APM_dates <- lubridate::parse_date_time(APM_files,orders = "Y-m-d HMS")

    if(all(APM_dates > cs_date_time)){
      stop("No APM available.")
    }

    current_APM <- APM_files[max(which(APM_dates < cs_date_time))]




    seasonder_apm_obj <- SeaSondeR::seasonder_readSeaSondeRAPMFile(current_APM)

    trimming <- 1

    seasonder_apm_obj <-  SeaSondeR::seasonder_trimAPM(seasonder_apm_obj,trimming)

    smoothing <- 20

    seasonder_apm_obj <- SeaSondeR::seasonder_smoothAPM(seasonder_apm_obj,smoothing)



    cs <- SeaSondeR::seasonder_setSeaSondeRCS_APM(cs, seasonder_apm_obj)

    cs <- SeaSondeR::seasonder_runMUSIC_in_FOR(cs, doppler_interpolation = 1L)



    MUSIC <- SeaSondeR::seasonder_exportMUSICTable(cs)

    dual_solutions_proportion <- SeaSondeR::seasonder_getSeaSondeRCS_MUSIC_dual_solutions_proportion(cs)

    MUSIC_config <- SeaSondeR::seasonder_getSeaSondeRCS_MUSICConfig(cs)

    FOR_config <- SeaSondeR::seasonder_getSeaSondeRCS_FORConfig(cs)

    CS_processing_steps <- SeaSondeR::seasonder_getSeaSondeRCS_ProcessingSteps(cs)

    APM_processing_steps <- SeaSondeR::seasonder_getSeaSondeRAPM_ProcessingSteps(SeaSondeR::seasonder_getSeaSondeRCS_APM(cs))


           list(input_file = file_to_read,
                header = header,
                FOR_config = FOR_config,
                MUSIC_config = MUSIC_config,
                FOR = FOR,
                MUSIC = MUSIC,
                dual_solutions_proportion = dual_solutions_proportion,
                CS_processing_steps = CS_processing_steps,
                APM_processing_steps = APM_processing_steps
           )



  }
