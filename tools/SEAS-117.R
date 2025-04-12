CS_file_path <- system.file("css_data/CSS_TORA_24_04_04_0700.cs", package = "SeaSondeR")
APM_path <- system.file("css_data/MeasPattern.txt", package = "SeaSondeR")

seasonder_apm_obj <- SeaSondeR::seasonder_readSeaSondeRAPMFile(APM_path)

seasonder_cs_obj <- SeaSondeR::seasonder_createSeaSondeRCS(CS_file_path, seasonder_apm_object = seasonder_apm_obj)