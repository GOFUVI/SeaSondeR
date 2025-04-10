CS_file_path <- system.file("css_data/CSS_TORA_24_04_04_0700.cs", package = "SeaSondeR")
APM_path <- system.file("css_data/MeasPattern.txt", package = "SeaSondeR")

seasonder_apm_obj <- SeaSondeR::seasonder_readSeaSondeRAPMFile(APM_path)

seasonder_cs_obj <- SeaSondeR::seasonder_createSeaSondeRCS(CS_file_path, seasonder_apm_object = seasonder_apm_obj)

MUSIC_options <- list(
      doppler_interpolation = 2,
      smoothNoiseLevel = T,
      PPMIN = 5, PWMAX = 50
)

seasonder_cs_obj <- SeaSondeR::seasonder_setMUSICOptions(seasonder_cs_obj, MUSIC_options)

seasonder_cs_obj <- SeaSondeR::seasonder_runMUSICInFOR(seasonder_cs_obj)
AngSeg <- purrr::list_c(lapply(45:61, function(i) list(c(i, 313, 360), c(i, 0, 31))))

radial_metrics <- SeaSondeR::seasonder_exportLLUVRadialMetrics(seasonder_cs_obj, LLUV_path = "tools/CSS_TORA_24_04_04_0700.ruv", AngSeg = AngSeg)


range_info <- SeaSondeR::seasonder_exportCTFRangeInfo(seasonder_cs_obj, file = "tools/RangeInfo.ctf")
