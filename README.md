# SeaSondeR

<!-- badges: start -->
<!-- badges: end -->

The goal of SeaSondeR is to provide a comprehensive set of tools for processing and analyzing data 
    from the SeaSonde(R) High-Frequency Radar (HF-Radar) instrument. This package is intended to 
    facilitate the creation of radial metrics files from spectra data.


SeaSondeR is an R package developed as an open-source alternative to proprietary tools for processing HF-Radar spectra and generating Radial Metrics. SeaSondeR integrates all key processing steps into a single workflow. It reads and processes both spectrum (CS) files and SeaSonde(R) antenna patterns, identifies the first-order spectral region according to CODAR methodology, and applies the MUSIC algorithm to estimate signal arrival directions. Drawing on technical manuals, patent literature, and prior scientific work, SeaSondeR is designed for both experimental and synthetic data, facilitating cloud-based analyses of extensive HF-Radar datasets while promoting free software solutions in operational oceanography and coastal monitoring.

## Installation

You can install the development version of SeaSondeR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("GOFUVI/SeaSondeR")
```


## Package pages

https://gofuvi.github.io/SeaSondeR/

## Acknowledgements

This work has been funded by the HF-EOLUS project (TED2021-129551B-I00), financed by MICIU/AEI /10.13039/501100011033 and by the European Union NextGenerationEU/PRTR - BDNS 598843 - Component 17 - Investment I3.

## Disclaimer

This software is provided "as is", without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose, and noninfringement. In no event shall the authors or copyright holders be liable for any claim, damages, or other liability, whether in an action of contract, tort, or otherwise, arising from, out of, or in connection with the software or the use or other dealings in the software.

## Trademark Notice

SeaSonde(R) is a trademark of CODAR Ocean Sensors Ltd.

