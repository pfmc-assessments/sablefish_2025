
## Sablefish 2025

Code and stock assessment documentation for the 2025 assessment of
sablefish off the U.S. West Coast. Files posted should not be considered
as finalized products. Below are key dates for the 2025 assessment:

- Pre-assessment data workshop: March 18 or 19, 2025, with the specific
  date to be selected later
- Final data deadline: April 21, 2025
- STAR panel date: July 14-18, 2025 in Seattle, WA

The pre-assessment data workshop and the STAR panel will each provide
virtual participation (workshop) and listening options (STAR Panel).
Before the pre-assessment data workshop, materials for the
pre-assessment data workshop will be posted
[online](https://connect.fisheries.noaa.gov/sablefish-2025/) to
facilitate participants’ review and robust conversations during the
workshop. The finalized stock assessment document and file will be
posted to the [Pacific Fishery Management
Council](https://www.pcouncil.org/) website in the September 2025
Briefing Book.

Dr. Chantel Wetzel (<chantel.wetzel@noaa.gov>) at the Northwest Fishery
Science Center is the lead assessor for the 2025 assessment.

## Installation

You can install {sablefish} from [GitHub](https://github.com/) with

``` r
# install.packages("pak")
pak::pkg_install("pfmc-assessments/sablefish_2025")
```

## Workflow

### File structure

This repository is structured like an R package to ease the installation
process and increase the ability to easily test code. Please follow the
following guidelines for the placement of files.

``` bash
├───data
├───data-raw
├───date-exploration
├───date-processed
├───quarto_website
├───man
├───R
└───report
```

Where

- data contains rda file of processed data in its final form that will be included in the
  assessment or assessment document, only data in its non-confidential
  form should be saved here;
- data-raw is for data files and .R scripts used to transform these raw
  files into something that is saved in data or in data-processed; all data files
  are saved within data-raw, but not saved to github. A list of the files are in
  .gitignore: 2023, ageing_error, ahsop, biology, bds, discard, landings, maturity,
  survey, weight_at_age. 
- data-processed is for csv or other fully processed and formatted data that 
  will be included in the assessment or assessment document, only data in its 
  non-confidential form should be saved here;
- data-exploration is for exploratory scripts;
- quarto_website contains files for the website;
- man stores .Rd files and figures used in the documentation; and
- R stores .R scripts that are loaded when building the package, this
  means functions only. These scripts are either called within the assessment
  document or .R scripts in the data-raw folder. 

## Disclaimer

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis, and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal laws. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise does not constitute or imply their
endorsement, recommendation, or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.
