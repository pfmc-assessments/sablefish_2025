# https://pfmc-assessments.github.io/AgeingError/articles/getting_started.html
require(AgeingError)
require(TMB)
require(dplyr)
require(tidyr)
# devtools::load_all()

# number of lines: max = 778 [between readers]
# number of readers: 13
# minus group; plus group; reference age: 1 50 4 (taken from vignette - sablefish example)
# unbiased reader: Patrick McDonald (reader 'x')
# bias option: 0 [ages assumed to be unbiased; sensu Haltuch et al. 2019]
# sigma option: -x (for now) relationship between variance of ageing error = that of reader 'x'

# Format age data:
CAP_NPT = read.csv(here::here("data-raw", "ageing_error", "ages_CAP_NPT.csv"))
CAP_NPT = CAP_NPT |>
# filter(Source != "AFSC") |> # (Haltuch et al. 2019 used both labs)
  select(c(SpecimenID,
           Original_AgerID, 
           Original_Age_Estimate,
           DoubleRead_AgerID,
           DoubleRead_Age_Estimate)) |>
  na.omit(CAP_NPT) |>
  rename(ID = SpecimenID)

# Code original age readers as numbers (put similar labs and readers closer together):
CAP_NPT$orig = with(CAP_NPT, 
                        ifelse(Original_AgerID == "pmcdonald", "1",
                        ifelse(Original_AgerID == "bpederson", "2",
                        ifelse(Original_AgerID == "npaige", "3",
                        ifelse(Original_AgerID == "dparker", "4",
                        ifelse(Original_AgerID == "jhale", "5",
                        ifelse(Original_AgerID == "lsullivan", "6",
                        ifelse(Original_AgerID == "mmann", "7",
                        ifelse(Original_AgerID == "mcavanagh", "8",
                        ifelse(Original_AgerID == "bkamikawa", "9",
                        ifelse(Original_AgerID == "orodriguez", "10",
                        ifelse(Original_AgerID == "jomalley", "11",
                        ifelse(Original_AgerID == "bhiggins", "12",
                        ifelse(Original_AgerID == "ltaylor", "13",
                               Original_AgerID))))))))))))))

# Code double age readers as numbers:
CAP_NPT$dbl = with(CAP_NPT, 
                        ifelse(DoubleRead_AgerID == "pmcdonald", "1",
                        ifelse(DoubleRead_AgerID == "bpederson", "2",
                        ifelse(DoubleRead_AgerID == "npaige", "3",
                        ifelse(DoubleRead_AgerID == "dparker", "4",
                        ifelse(DoubleRead_AgerID == "jhale", "5",
                        ifelse(DoubleRead_AgerID == "lsullivan", "6",
                        ifelse(DoubleRead_AgerID == "Lsullivan", "6",
                        ifelse(DoubleRead_AgerID == "mmann", "7",
                        ifelse(DoubleRead_AgerID == "mcavanagh", "8",
                        ifelse(DoubleRead_AgerID == "bkamikawa", "9",
                        ifelse(DoubleRead_AgerID == "orodriguez", "10",
                        ifelse(DoubleRead_AgerID == "jomalley", "11",
                        ifelse(DoubleRead_AgerID == "Jomalley", "11",
                        ifelse(DoubleRead_AgerID == "bhiggins", "12",
                        ifelse(DoubleRead_AgerID == "ltaylor", "13",
                               DoubleRead_AgerID))))))))))))))))

# Format data frame as long (to prep for wide):
ages.CAP_NPT = CAP_NPT |>
  select(-c(Original_AgerID, DoubleRead_AgerID)) |>
  pivot_longer(cols = c(orig, dbl),
               names_to = "Reader.ID",
               values_to = "Reader.No") |>
  pivot_longer(cols = c(Original_Age_Estimate, DoubleRead_Age_Estimate),
               names_to = "Reader.Type",
               values_to = "Age.Est")

ages.CAP_NPT$Reader.Type = with(ages.CAP_NPT,
                                ifelse(Reader.Type == "Original_Age_Estimate", "orig",
                                ifelse(Reader.Type == "DoubleRead_Age_Estimate", "dbl", 
                                       NA)))

# Order age readers by number (aesthetics):
ages.CAP_NPT$Reader.No = ordered(ages.CAP_NPT$Reader.No, 
                                 levels = c("1","2","3","4","5",
                                            "6","7","8","9","10",
                                            "11","12","13"))

# Prepare data for estimating within reader error (precision):
ages.CAP_NPT = ages.CAP_NPT |>
  filter(Reader.ID == Reader.Type) |>
  select(-c(Reader.ID, Reader.Type)) |>
  mutate(row = row_number()) 

CAP_NPT.within = ages.CAP_NPT |>
  group_by(Reader.No, ID) |>
  mutate(n = n()) |>
  filter(n > 1) |>
  select(-n) |>
  pivot_wider(id_cols = c(row, ID),
              names_expand = T,
              names_from = Reader.No,
              values_from = Age.Est,
              values_fill = -999) |>
              as.data.frame()

age_matrix.within = CAP_NPT.within |>
  select(-ID) |>
  group_by_all() |>
  add_count() |>
  filter(n > 1) |>
  distinct() |>
  select(n, everything()) |>
  rename(count = n)

# Prepare data for estimating between reader error (bias):
CAP_NPT.between = ages.CAP_NPT |>
  group_by(Reader.No, ID) |>
  mutate(n = n()) |>
  filter(n < 2) |>
  select(-n) |>
  pivot_wider(id_cols = c(ID),
              names_expand = T,
              names_from = Reader.No,
              values_from = Age.Est,
              values_fill = -999) |>
              as.data.frame() 
  
CAP_NPT.between = CAP_NPT.between |>
  select(-ID) |>
  group_by_all() |>
  add_count() |>
  filter(n > 1) |>
  distinct() |>
  select(n, everything()) |>
  rename(count = n)

# Write data and specs files for AgeingError package (between reader [bias] only):
source("AgeingError-dev/R/write_files.R")
  source("AgeingError-dev/R/write_data_file.R")
  source("AgeingError-dev/R/write_specs_file.R")

# Condense ageing error matrix by combining readers 2-13 and comparing them to reader 1:
age_matrix.between = ages.CAP_NPT
age_matrix.between$Reader.No = with(age_matrix.between,
                                    ifelse(Reader.No == "1", "1", "2"))
  
age_matrix.between = age_matrix.between |>
  group_by(Reader.No, ID) |>
  mutate(n = n()) |>
  filter(n < 2) |>
  select(-n) |>
  pivot_wider(id_cols = c(ID),
              names_expand = T,
              names_from = Reader.No,
              values_from = Age.Est,
              values_fill = -999) |>
              as.data.frame() 
  
age_matrix.between = age_matrix.between |>
  select(-ID) |>
  group_by_all() |>
  add_count() |>
  filter(n > 1) |>
  distinct() |>
  select(n, everything()) |>
  rename(count = n) |>
  filter(`2` != "-999")

setwd(here::here("data-raw", "ageing_error"))

# ageing error matrix for reader 1 = "true age":
write_files(dat=age_matrix.between, 
            dir=getwd(),
            file_dat = "data.dat",
            file_specs = "data.spc",
            minage = 1,
            maxage = 70,
            refage = 4,
            minusage = 1,
            plusage = 50,
            biasopt = NULL, # 0: assumed unbiased (sensu Haltuch et al. 2019)
            sigopt = NULL, # -1: functional form of reading error with true age same as reader 1
            knotages = NULL)
  
# Construct ageing error matrix:
dat = AgeingError::CreateData(file.path(getwd(), "data.dat"),
  NDataSet = 1,
  verbose = T, 
  EchoFile = "SableEcho.out")
spc = AgeingError::CreateSpecs(file.path(getwd(), "data.spc"),
  DataSpecs = dat, 
  verbose = T)

mod = AgeingError::DoApplyAgeError(Species = "Sablefish",
                                   DataSpecs = dat,
                                   ModelSpecsInp = spc,
                                   AprobWght = 1e-06,
                                   SlopeWght = 0.01,
                                   SaveDir = "default",
                                   verbose = F)

out = AgeingError::ProcessResults(Species = "Sablefish", 
                                  SaveDir = "default", 
                                  CalcEff = F, # T generates a 'subscript out of bounds' error
                                  verbose = F)


# Specifications from Chantel Wetzel:
  # combinations: bias=0,sd=1; bias=0,sd=2; bias=1,sd=1; bias=1,sd=2
spc.1 = read.delim("specifications_bias=0_sd=1 (2024_09_13 13_44_01 UTC).dat")
spc.2 = read.delim("specifications_bias=0_sd=2 (2024_09_13 13_44_01 UTC).dat")
spc.3 = read.delim("specifications_bias=1_sd=1 (2024_09_13 13_44_01 UTC).dat")
spc.4 = read.delim("specifications_bias=1_sd=2 (2024_09_13 13_44_01 UTC).dat")

## ageing error matrix, bias=0, sd=1:
write_files(dat=age_matrix.between, 
            dir=getwd(),
            file_dat = "data_bias0_sig1.dat",
            file_specs = "data_bias0_sig1.spc",
            minage = 1,
            maxage = 70,
            refage = 4,
            minusage = 1,
            plusage = 50,
            biasopt = c(0,0), # unbiased 
            sigopt = c(1,-1), # constant CV shared among readers
            knotages = NULL)

dat_bias0_sig1 = AgeingError::CreateData(file.path(getwd(), "data_bias0_sig1.dat"),
  NDataSet = 1,
  verbose = T, 
  EchoFile = "SableEcho_bias0_sig1.out")
spc_bias0_sig1 = AgeingError::CreateSpecs(file.path(getwd(), "data_bias0_sig1.spc"),
  DataSpecs = dat, 
  verbose = T)

mod_bias0_sig1 = AgeingError::DoApplyAgeError(Species = "Sablefish",
                                   DataSpecs = dat_bias0_sig1,
                                   ModelSpecsInp = spc_bias0_sig1,
                                   AprobWght = 1e-06,
                                   SlopeWght = 0.01,
                                   SaveDir = "bias0_sig1",
                                   verbose = F)

out_bias0_sig1 = AgeingError::ProcessResults(Species = "Sablefish", 
                                  SaveDir = "bias0_sig1", 
                                  CalcEff = F, # T generates a 'subscript out of bounds' error
                                  verbose = F)

## ageing error matrix, bias=0, sd=2:
write_files(dat=age_matrix.between, 
            dir=getwd(),
            file_dat = "data_bias0_sig2.dat",
            file_specs = "data_bias0_sig2.spc",
            minage = 1,
            maxage = 70,
            refage = 4,
            minusage = 1,
            plusage = 50,
            biasopt = c(0,0), # unbiased 
            sigopt = c(2,-1), # curvilinear standard deviation
            knotages = NULL)

dat_bias0_sig2 = AgeingError::CreateData(file.path(getwd(), "data_bias0_sig2.dat"),
  NDataSet = 1,
  verbose = T, 
  EchoFile = "SableEcho_bias0_sig2.out")
spc_bias0_sig2 = AgeingError::CreateSpecs(file.path(getwd(), "data_bias0_sig2.spc"),
  DataSpecs = dat, 
  verbose = T)

mod_bias0_sig2 = AgeingError::DoApplyAgeError(Species = "Sablefish",
                                   DataSpecs = dat_bias0_sig2,
                                   ModelSpecsInp = spc_bias0_sig2,
                                   AprobWght = 1e-06,
                                   SlopeWght = 0.01,
                                   SaveDir = "bias0_sig2",
                                   verbose = F)

out_bias0_sig2 = AgeingError::ProcessResults(Species = "Sablefish", 
                                  SaveDir = "bias0_sig2", 
                                  CalcEff = F, # T generates a 'subscript out of bounds' error
                                  verbose = F)

## ageing error matrix, bias=1, sd=1:
write_files(dat=age_matrix.between, 
            dir=getwd(),
            file_dat = "data_bias1_sig1.dat",
            file_specs = "data_bias1_sig1.spc",
            minage = 1,
            maxage = 70,
            refage = 4,
            minusage = 1,
            plusage = 50,
            biasopt = c(0,1), # 0 = unbiased, 1 = constant CV
            sigopt = c(1,-1), # constant CV
            knotages = NULL)

dat_bias1_sig1 = AgeingError::CreateData(file.path(getwd(), "data_bias1_sig1.dat"),
  NDataSet = 1,
  verbose = T, 
  EchoFile = "SableEcho_bias1_sig1.out")
spc_bias1_sig1 = AgeingError::CreateSpecs(file.path(getwd(), "data_bias1_sig1.spc"),
  DataSpecs = dat, 
  verbose = T)

mod_bias1_sig1 = AgeingError::DoApplyAgeError(Species = "Sablefish",
                                   DataSpecs = dat_bias1_sig1,
                                   ModelSpecsInp = spc_bias1_sig1,
                                   AprobWght = 1e-06,
                                   SlopeWght = 0.01,
                                   SaveDir = "bias1_sig1",
                                   verbose = F)

out_bias1_sig1 = AgeingError::ProcessResults(Species = "Sablefish", 
                                  SaveDir = "bias1_sig1", 
                                  CalcEff = F, # T generates a 'subscript out of bounds' error
                                  verbose = F)

## ageing error matrix, bias=1, sd=2:
write_files(dat=age_matrix.between, 
            dir=getwd(),
            file_dat = "data_bias1_sig2.dat",
            file_specs = "data_bias1_sig2.spc",
            minage = 1,
            maxage = 64, # had to change here...why?
            refage = 4,
            minusage = 1,
            plusage = 50,
            biasopt = c(0,1), # 0 = unbiased, 1 = constant CV
            sigopt = c(2,-1), # curvilinear standard deviation
            knotages = NULL)

dat_bias1_sig2 = AgeingError::CreateData(file.path(getwd(), "data_bias1_sig2.dat"),
  NDataSet = 1,
  verbose = T, 
  EchoFile = "SableEcho_bias1_sig2.out")
spc_bias1_sig2 = AgeingError::CreateSpecs(file.path(getwd(), "data_bias1_sig2.spc"),
  DataSpecs = dat, 
  verbose = T)

mod_bias1_sig2 = AgeingError::DoApplyAgeError(Species = "Sablefish",
                                   DataSpecs = dat_bias1_sig2,
                                   ModelSpecsInp = spc_bias1_sig2,
                                   AprobWght = 1e-06,
                                   SlopeWght = 0.01,
                                   SaveDir = "bias1_sig2",
                                   verbose = F)

out_bias1_sig2 = AgeingError::ProcessResults(Species = "Sablefish", 
                                  SaveDir = "bias1_sig2", 
                                  CalcEff = F, # T generates a 'subscript out of bounds' error
                                  verbose = F)

## ageing error matrix, bias=2, sd=1:
write_files(dat=age_matrix.between, 
            dir=getwd(),
            file_dat = "data_bias2_sig1.dat",
            file_specs = "data_bias2_sig1.spc",
            minage = 1,
            maxage = 70, 
            refage = 4,
            minusage = 1,
            plusage = 50,
            biasopt = c(0,2), # 0 = unbiased, 1 = curvilinear std dev.
            sigopt = c(1,-1), # constant CV
            knotages = NULL)

dat_bias2_sig1 = AgeingError::CreateData(file.path(getwd(), "data_bias2_sig1.dat"),
  NDataSet = 1,
  verbose = T, 
  EchoFile = "SableEcho_bias2_sig1.out")
spc_bias2_sig1 = AgeingError::CreateSpecs(file.path(getwd(), "data_bias2_sig1.spc"),
  DataSpecs = dat, 
  verbose = T)

mod_bias2_sig1 = AgeingError::DoApplyAgeError(Species = "Sablefish",
                                   DataSpecs = dat_bias2_sig1,
                                   ModelSpecsInp = spc_bias2_sig1,
                                   AprobWght = 1e-06,
                                   SlopeWght = 0.01,
                                   SaveDir = getwd(),
                                   verbose = F)

out_bias2_sig1 = AgeingError::ProcessResults(Species = "Sablefish", 
                                  SaveDir = getwd(), 
                                  CalcEff = F, # T generates a 'subscript out of bounds' error
                                  verbose = F)

## ageing error matrix, bias=2, sd=2:
write_files(dat=age_matrix.between, 
            dir=getwd(),
            file_dat = "data_bias2_sig2.dat",
            file_specs = "data_bias2_sig2.spc",
            minage = 1,
            maxage = 70,
            refage = 4,
            minusage = 1,
            plusage = 50,
            biasopt = c(0,2), # 0 = unbiased, 1 = curvilinear std dev.
            sigopt = c(2,-1), # curvilinear std dev.
            knotages = NULL)

dat_bias2_sig2 = AgeingError::CreateData(file.path(getwd(), "data_bias2_sig2.dat"),
  NDataSet = 1,
  verbose = T, 
  EchoFile = "SableEcho_bias2_sig2.out")
spc_bias2_sig1 = AgeingError::CreateSpecs(file.path(getwd(), "data_bias2_sig2.spc"),
  DataSpecs = dat, 
  verbose = T)

mod_bias2_sig2 = AgeingError::DoApplyAgeError(Species = "Sablefish",
                                   DataSpecs = dat_bias2_sig2,
                                   ModelSpecsInp = spc_bias2_sig2,
                                   AprobWght = 1e-06,
                                   SlopeWght = 0.01,
                                   SaveDir = getwd(),
                                   verbose = F)

out_bias2_sig2 = AgeingError::ProcessResults(Species = "Sablefish", 
                                  SaveDir = getwd(), 
                                  CalcEff = F, # T generates a 'subscript out of bounds' error
                                  verbose = F)