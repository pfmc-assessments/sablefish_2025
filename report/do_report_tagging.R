withr::with_dir(
  here::here("report"),
  add_tagging(
    x = "SAR_USWC_sablefish_skeleton.tex",
    dir = getwd(),
    compile = TRUE
  )
)

# 8/26/2025 Error
#______Tagging structure added to tex file.______
#______Compiling in progress - This can take a while...______
#tlmgr.pl: package repository https://us.mirrors.cicku.me/ctan/systems/texlive/tlnet (not verified: pubkey missing)
#tlmgr.pl install: package already present: bookmark
#! Undefined control sequence.
#<argument> \ERRORtbl 
#/row
#l.576 \midrule
#\addlinespace[2.5pt]
#
#Error: LaTeX failed to compile C:/Assessments/2025/sablefish_2025/report/SAR_USWC_sablefish_skeleton.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See SAR_USWC_sablefish_skeleton.log for more info.
#  
#  