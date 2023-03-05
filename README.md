# Analysing and visualising bike-sharing demand with outliers

This repository contains the Quarto source code relating to the arXiv version of the manuscript *Analysing and visualising bike-sharing demand with outliers*. The arXiv pre-print can be found at [doi.org/10.48550/arXiv.2204.06112](https://doi.org/10.48550/arXiv.2204.06112).

### Template

This repository makes use of the [Quarto template for arXiv preprints](https://github.com/mikemahoney218/quarto-arxiv) developed by [Mike Mahoney](https://github.com/mikemahoney218).

### Data

The datasets analysed during this study are available from Capital Bikeshare in a public repository that does not issue datasets with DOIs at [s3.amazonaws.com/capitalbikeshare-data/index.html](https://s3.amazonaws.com/capitalbikeshare-data/index.html). Further information on any pre-processing performed by Capital Bikeshare is available at [ride.capitalbikeshare.com/system-data](https://ride.capitalbikeshare.com/system-data), and the license agreement at [ride.capitalbikeshare.com/data-license-agreement](https://ride.capitalbikeshare.com/data-license-agreement). The data used in this paper spans January 2017 to December 2019.

### R code

The source code used to create the figures is contained within the .qmd files, with the input data for each plot being saved in the `Data/` folder in `.rds` format. Cleaning scripts which produce these `.rds` files from the raw data are included in the `Cleaning Scripts` directory.

The following .rds files:

* Data/OD_data.rds
* Data/d_2017.rds
* Data/d_2018.rds
* Data/d_2019.rds

are not included in this repository due to their large file size. These files can be re-created by running the `Cleaning Scripts/1_process_raw_data.R` script.
