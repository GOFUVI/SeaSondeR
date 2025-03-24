 seasonder_cs_obj %>% seasonder_getSeaSondeRCS_reference_noise_normalized_limits_estimation_interval()
  

  seasonder_getFOR_parameters(seasonder_cs_obj)$reference_noise_normalized_limits


seasonder_cs_object %>% seasonder_getSeaSondeRCS_NoiseLevel()
plot(results)
seasonder_getReceiverGain_dB(seasonder_cs_obj)

seasonder_cs_obj %>% seasonder_getSeaSondeRCS


x <- seasonder_cs_obj %>%  seasonder_getSeaSondeRCS_SelfSpectra(3)


y <- x$A3$all_ranges$all_doppler[2,1]


10*log10(y) -34.2


a <- 10 ^((test_rng$NF03 + 34.2)/10) 
b <- 10 ^((ref_rng$NF03+ 34.2)/10) 
a
b
