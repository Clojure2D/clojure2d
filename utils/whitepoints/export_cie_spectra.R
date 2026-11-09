# Export the official CIE D65 (1 nm) and D50 (5 nm) spectra, 300-830 nm, from R colorSpec into
# utils/whitepoints/data/cie_spectra.csv (input of gen_resources.py).
#
# Run from the repository root:
#
#     Rscript utils/whitepoints/export_cie_spectra.R
#
# colorSpec D65.1nm: ISO/CIE 11664-2:2022 Table B.1; D50.5nm: CIE 15:2004 (colorSpec documentation).
# colorSpec scales the spectra differently from colour-science; chromaticities do not depend on it.

suppressPackageStartupMessages(library(colorSpec))

OUT <- "utils/whitepoints/data/cie_spectra.csv"
num <- function(x) sprintf("%.17g", x)
rows <- c(sprintf("D65,%d,%s", as.integer(wavelength(D65.1nm)), num(as.numeric(D65.1nm))),
          sprintf("D50,%d,%s", as.integer(wavelength(D50.5nm)), num(as.numeric(D50.5nm))))
writeLines(c(sprintf("# colorSpec %s", as.character(packageVersion("colorSpec"))),
             "illuminant,lambda,value", rows), OUT)
