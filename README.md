# Technology Forecast Data Envelopment Analysis (TFDEA)

This project provides a web interface for a user to perform the Data Envelopment Analysis and is developed using R language and R Shiny.

### Installation and Setup instructions:

1. Download and install R language (current version 4.2.2): https://cran.r-project.org/bin/windows/base/R-4.2.2-win.exe \
(More details at https://cran.r-project.org/)

2. Download and install R Studio: https://download1.rstudio.org/desktop/windows/RStudio-2022.07.2-576.exe \
(More Details at https://posit.co/download/rstudio-desktop/)

3. Open RStudio and open this TFDEA project:
> File > Open Project > "Path to Repo" > Shiny_TFDEA.Rproj

4. Install packages by running this command in the R Console:
> `install.packages(c("Benchmarking", "car", "deaR", "googlesheets4", "MultiplierDEA"))`

( Multiple other dependent packages will be installed automatically. if not, please refer to the list in the Extra details section)

5. Install TFDEA package from the repo manually:
> `install.packages("TFDEA_0.9.8.3.tar.gz", repos = NULL, type = "source")`

(source: https://cran.r-project.org/src/contrib/Archive/TFDEA/TFDEA_0.9.8.3.tar.gz)

6. Run the application:
> `shiny::runApp()`


### Extra details:
These dependent packages are installed automatically. In case of any issues with automatic installation, please run this command to install them manually:

> `install.packages(c("rprojroot", "crayon", "fs", "diffobj", "rematch2", "sys", "stringi", "brio", "callr", "desc", "pkgload", "praise", "processx", "ps", "waldo", "askpass", "backports", "stringr", "evaluate", "highr", "yaml", "xfun", "testthat", "colorspace", "fansi", "curl", "mime", "openssl", "fastmap", "later", "utf8", "broom", "magrittr", "numDeriv", "knitr", "SparseM", "MatrixModels", "minqa", "nloptr", "RcppEigen", "farver", "labeling", "lifecycle", "munsell", "R6", "RColorBrewer", "rlang", "viridisLite", "cli", "glue", "gtable", "isoband", "tibble", "vctrs", "withr", "ellipsis", "purrr", "tidyselect", "cpp11", "httr", "jsonlite", "digest", "base64enc", "htmltools", "htmlwidgets", "lazyeval", "crosstalk", "data.table", "promises", "pkgconfig", "generics", "pillar", "lpSolveAPI", "ucminf", "quadprog", "Rcpp", "carData", "abind", "pbkrtest", "quantreg", "lme4", "scales", "lpSolve", "ggplot2", "tidyr", "plotly", "igraph", "writexl", "dplyr", "gridExtra", "rematch", "uuid", "cellranger", "gargle", "googledrive", "ids", "registry", "slam", "Rglpk", "listcomp", "ROI", "ROI.plugin.glpk", "ompr", "ompr.roi", "credentials", "zip", "gitcreds", "ini", "systemfonts", "textshaping", "tinytex", "clipr", "gert", "gh", "rstudioapi", "whisker", "prettyunits", "downlit", "ragg", "rmarkdown", "xml2", "xopen", "brew", "bitops", "usethis", "miniUI", "pkgbuild", "pkgdown", "profvis", "rcmdcheck", "remotes", "roxygen2", "rversions", "sessioninfo", "urlchecker", "assertthat"))`