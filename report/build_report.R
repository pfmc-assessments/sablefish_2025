pak::pkg_install("nmfs-ost/asar")
pak::pkg_install("nmfs-ost/satf")

# Read in the output from an SS3 and create a csv file with estimates:
# I have created the relative directory of the project where there is
# folder called "model" where the SS3 model files are.  This directory
# is passed to the function in the `outdir` and `savedir` arguments below:
asar::convert_output(
  output_file = "Report.sso", 
  outdir = here::here("model", "_bridging", "13_m_prior"),
  model = "ss3", 
  file_save = TRUE,
  savedir = here::here("report"),
  save_name = "sablefish_model_output"
)

asar::create_template(
  format = "pdf",
  office = "NWFSC",
  region = "U.S. West Coast",
  species = "sablfefish",
  spp_latin = "Anoplopoma fimbria",
  year = 2025,
  author = c("Chantel R. Wetzel", "Aaron M. Berger", "Cheryl Barnes", "Joshua A. Zahner", "Nick Tolimieri"),
  include_affiliation = TRUE,
  simple_affiliation = FALSE,
  resdir = here::here("report"),
  model_results = "sablefish_convert_output.csv",
  model = "SS3"
)
