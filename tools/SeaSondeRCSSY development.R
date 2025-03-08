
rm(list=ls())
devtools::document()
devtools::load_all()
library(magrittr)
  filepath <- here::here("tests/testthat/data/SUNS/CSS/CSS_SUNS_2025_02_17_060000.csr")
  specs_path <- here::here("inst/specs/CSSY_V1.yaml")
  endian <-  "big"

  key_specs <-  seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CSSY"),c("key_size_block"))



# specs <-   seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CSSY"),c("CSSY","HEAD"))
#

# con <-   file(filepath, "rb")
# seek(con,16,origin = "start")
# header <- seasonder_readCSSYHeader(con, specs, specs_key_size = key_specs)
# names(header)

# seasonder_readSeaSondeRCSSYFile(filepath)


con <-   file(filepath, "rb")
seek(con,8,origin = "start")

key <- seasonder_readSeaSondeCSFileBlock(key_specs,con, endian)

seek(con,key$size, origin = "current")
key <- seasonder_readSeaSondeCSFileBlock(key_specs,con, endian)
specs <-   seasonder_readYAMLSpecs(seasonder_defaultSpecsFilePath("CSSY"),c("CSSY","BODY"))

body <- seasonder_readCSSYBody(connection = con, size= key$size, specs = specs,specs_key_size = key_specs)

?str()
