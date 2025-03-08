library(rlang)
library(magrittr)



FOS <-   list(nsm = 4,
              fdown = 10^(10/10),
              flim = 10^(20/10),
              noisefact = 10^(6/10),
              currmax = 2,
              reject_distant_bragg = TRUE, #  Default is to apply this test
              reject_noise_ionospheric = TRUE, #  Default is to apply this test (except for 42 MHz)

              reject_noise_ionospheric_threshold = 0# Default is 0 dB threshold. Typically 0 dB should be used.
)





seasonder_apm_obj <- SeaSondeR::seasonder_readSeaSondeRAPMFile("tests/testthat/data/2018-06-21  170220.txt")

SeaSondeR:::seasonder_plotAPMLoops(seasonder_apm_obj)

trimming <- 1

seasonder_apm_obj <-  SeaSondeR::seasonder_trimAPM(seasonder_apm_obj,trimming)

smoothing <- 20

seasonder_apm_obj <- SeaSondeR::seasonder_smoothAPM(seasonder_apm_obj,smoothing)

SeaSondeR:::seasonder_plotAPMLoops(seasonder_apm_obj)



cs <- seasonder_createSeaSondeRCS(here::here("tests/testthat/data/CSS_VILA_21_02_15_1700.cs"))




SeaSondeR:::seasonder_SeaSondeRCS_plotSelfSpectrum(cs, 3 , 7,plot_FORs = TRUE)

cs <- SeaSondeR::seasonder_computeFORs(cs, method = "SeaSonde", FOR_control = FOS)

SeaSondeR:::seasonder_SeaSondeRCS_plotSelfSpectrum(cs, 3 , 20,plot_FORs = TRUE)




cs <- SeaSondeR::seasonder_setSeaSondeRCS_APM(cs, seasonder_apm_obj)

cs <- SeaSondeR::seasonder_runMUSIC_in_FOR(cs, doppler_interpolation = 1L)

MUSIC <- seasonder_exportMUSICTable(cs)

MUSIC <-seasonder_getSeaSondeRCS_MUSIC(cs) %>%  dplyr::filter(range_cell %in% c(7) & doppler_bin %in%  c(288))

test <- MUSIC$DOA_solutions[[1]]

# single solution

distances <- MUSIC$distances[[1]]

target_bearing <- attr(distances, "bearings",exact = T)[which.max(1/Mod(distances["single",]))]

expect_equal(target_bearing,test$single$bearing)

plot(1/Mod(distances["single",]))

target_a <- seasonder_apm_obj[,which.max(1/Mod(distances["single",])), drop = F]

expect_equal(target_a,test$single$a)

# dual solution

peaks <- pracma::findpeaks(1/Mod(distances["dual",]),npeaks = 2, sortstr = TRUE)

target_bearing <- attr(distances, "bearings",exact = T)[peaks[,2]]

plot(1/Mod(distances["dual",]))

expect_equal(target_bearing,test$dual$bearing)

target_a <- seasonder_apm_obj[,peaks[,2], drop = F]

expect_equal(target_a,test$dual$a)





