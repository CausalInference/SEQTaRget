set working-directory := 'SEQTaRget'

docs:
    R -e "devtools::document()"
check: docs
    R -e "devtools::check()"
install: docs
    R -e "devtools::install(build_vignettes = TRUE)"
dev:
    R -e "pak::local_install_dev_deps()"
# Render README.Rmd and mirror it to the repo root, which is what GitHub renders
readme:
    R -e "rmarkdown::render('README.Rmd', output_options = list(html_preview = FALSE))"
    cp README.md ../README.md
