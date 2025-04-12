CS_file_path <- "tools/CSR_SILL_2023_03_01_0000.csr"
APM_path <- "tools/MeasPattern_2022.txt"

seasonder_apm_obj <- SeaSondeR::seasonder_readSeaSondeRAPMFile(APM_path)

seasonder_cs_obj <- SeaSondeR::seasonder_createSeaSondeRCS(CS_file_path, seasonder_apm_object = seasonder_apm_obj)

SeaSondeR::seasonder_SeaSondeRCS_plotSelfSpectrum(seasonder_cs_obj,3,5,plot_FORs = T)

first_order_settings <- list(
      nsm = 2,
      fdown = 10^(10 / 10),
      flim = 10^(20 / 10),
      noisefact = 10^(6 / 10),
      currmax = 2,
      reject_distant_bragg = TRUE, #  Default is to apply this test
      reject_noise_ionospheric = F, #  Default is to apply this test (except for 42 MHz)

      reject_noise_ionospheric_threshold = 0 # Default is 0 dB threshold. Typically 0 dB should be used.
)


seasonder_cs_obj <- seasonder_computeFORs(seasonder_cs_obj, FOR_control = first_order_settings)

SeaSondeR::seasonder_SeaSondeRCS_plotSelfSpectrum(seasonder_cs_obj,3,5,plot_FORs = T)

MUSIC_options <- list(
      doppler_interpolation = 2,
      smoothNoiseLevel = T,
      PPMIN = 5, PWMAX = 50
)

seasonder_cs_obj <- SeaSondeR::seasonder_setMUSICOptions(seasonder_cs_obj, MUSIC_options)


seasonder_cs_obj <- SeaSondeR::seasonder_runMUSICInFOR(seasonder_cs_obj)


radial_metrics <- SeaSondeR::seasonder_exportLLUVRadialMetrics(seasonder_cs_obj, LLUV_path = "tools/CSR_SILL_2023_03_01_0000.ruv")
