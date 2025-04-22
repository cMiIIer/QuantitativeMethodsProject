# Project Objective

The objective of this study is to quantify the trophic interactions of a
model freshwater community under the effects of salinzation. There are
two main sub-objectives that I want to accomplish during the data
analysis phase.

1.  Quantify the temporal effects of the system, looking how
    physiochemical variables, primarily salt content, effects biomass
    accumulation and transfer.

2.  Quantify the time to metamorphosis of our two macrofauna groups,
    seeing of salinity impacts metamorphic success.

# Result Replication

Results replication can be done within the code provided, and results
are not dependent on any per mutational analysis so model outputs should
be consistent.

## Code Base Structure and Dependencies

There are multiple iterations of data analysis. Relevant code is found
in the ‘Code’ folder. I am working with BOTH the SICB and
TimeToEventAnalysis projects, to help separate analysis types.

I am working with multiple packages and all packages are loaded via name
in the respective project and are also referenced here. You will need
the following packages are required at a minimum to preform all
analyses, if you want to install them before loading the scripts the
code is provided here:

### Requried Packages

    install.packages(c("tidyverse", "readxl", "lubridate", "glmmTMB", "piecewiseSEM", "tidySEM","survival", "survminer", "data.table", "nlraa", "multcompView", "ggsurvfit", "tidycmprsk"))

## Data Strcuture

The code structure uses the read\_excel function and referencing as data
is stored in excel files as opposed to CSV format. The scripts include
necessary corrections for erroneous data, NA values and transformations,
so please run every line in each data loading and cleanup sections.

Note there are some discrepancies within the names of data columns:
primarily that tadpole weight is reflective of mass, not actual weight.

Data can be loaded through the following working directory:

    setwd("Thesis/Data/Grad_3-14-25_Miller")

There are also large changes to data formats, so please run every line
*sequentially* so analyses are working as intended.

# Acknowledgements

I would like to thank my Advisor, Dr. Allison Welch, and two
undergraduate coauthors: Maya Mylott and Alex Barron. Further support
was provided by Greg Townsley, Pete Meier, Dr. Bob Podoslky, Dr. Freya
Rowland, Dr. A. Challen Hyman, Dr. Tony Harold, Dr. Norm Levine and
lastly Dr. Dan McGlinn.
