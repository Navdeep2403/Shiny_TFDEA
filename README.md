# Technology Forecast Data Envelopment Analysis (TFDEA)

This project provides a web interface for a user to perform the Data Envelopment Analysis and is developed using R language and R Shiny.

### Installation and Setup instructions:

1. Download and install R language (current version 4.2.2): https://cran.r-project.org/bin/windows/base/R-4.2.2-win.exe \
(More details at https://cran.r-project.org/)

2. Download and install R Studio: https://download1.rstudio.org/desktop/windows/RStudio-2022.07.2-576.exe \
(More Details at https://posit.co/download/rstudio-desktop/)

3. Open RStudio, open TFDEA project using instructions below:
    1. From the menu options on the top bar, choose `File > New project`.
    2. From the New project wizard, choose `Version Control (Checkout a project from a version control repository)`.
    3. From the next options, choose `Git (Clone a project from a Git repository)`.
    4. On the next window, fill the details as below:
        1. In `Repository URL`, enter this repository link - https://github.com/Navdeep2403/Shiny_TFDEA
        2. In `Project directory name` - change the directory name if you like. (optional).
    5. In the end, Click on the button `Create Project`.
    6. It will clone the git repository and create a new project and open the R console.

4. Open the file `global.R` and click on the `Run App` button on top-right `OR` run the below command in R console:
> `shiny::runApp()`
