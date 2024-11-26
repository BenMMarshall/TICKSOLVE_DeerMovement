usethis::create_package(here::here(), check_name = FALSE)
roxygen2::roxygenise()
dir.create("notebook")
dir.create("notebook/extraneous_code")
dir.create("notebook/ext_images")
usethis::use_gpl3_license()
# add data to build ignore
usethis::use_build_ignore("data")
usethis::use_git_ignore("data")

targets::use_targets()
