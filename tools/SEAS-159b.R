rm(list=ls())

library(magrittr)

  
seasonder_apm_obj <- SeaSondeR::seasonder_readSeaSondeRAPMFile(
  "tools/MeasPattern.txt"
)

AngSeg <- purrr::reduce(2:25, \(x,i) c(x,list(c(i,0,191),c(i,331,360))) ,.init = list())

filepath <- "tools/CSS_SUNS_25_02_17_0600.cs"
  
  seasonder_cs_obj <- SeaSondeR::seasonder_createSeaSondeRCS(filepath, seasonder_apm_object = seasonder_apm_obj)
  seasonder_cs_obj <- SeaSondeR::seasonder_setFOR_noisefact(seasonder_cs_obj, 5.01)
  seasonder_cs_obj <- SeaSondeR::seasonder_setMUSICOptions(seasonder_cs_obj, list(doppler_interpolation = 2, smoothNoiseLevel = T))
  seasonder_cs_obj <- SeaSondeR::seasonder_runMUSICInFOR(seasonder_cs_obj)
  
  

  radial_metrics <- SeaSondeR::seasonder_exportLLUVRadialMetrics(seasonder_cs_obj, "tools/CSS_SUNS_20_02_17_0600.ruv", AngSeg = AngSeg)
  
  
filepath <- "tools/CSS_SUNS_2025_02_17_060000.csr"
  
  seasonder_cs_obj <- SeaSondeR::seasonder_createSeaSondeRCS(filepath, seasonder_apm_object = seasonder_apm_obj)
  seasonder_cs_obj <- SeaSondeR::seasonder_setFOR_noisefact(seasonder_cs_obj, 5.01)
  seasonder_cs_obj <- SeaSondeR::seasonder_setMUSICOptions(seasonder_cs_obj, list(doppler_interpolation = 2, smoothNoiseLevel = T))
  seasonder_cs_obj <- SeaSondeR::seasonder_runMUSICInFOR(seasonder_cs_obj)
  
  

  radial_metrics <- SeaSondeR::seasonder_exportLLUVRadialMetrics(seasonder_cs_obj, "tools/CSR_SUNS_20_02_17_0600.ruv", AngSeg = AngSeg)

