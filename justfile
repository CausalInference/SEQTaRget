set working-directory := 'SEQTaRget'

docs:
    Rscript -e "devtools::document()"
check: docs
    Rscript -e "devtools::check()"
install: docs
    Rscript -e "devtools::install(build_vignettes = TRUE)"
dev:
    Rscript -e "pak::local_install_dev_deps()"
# Render README.Rmd and mirror it to the repo root, which is what GitHub renders
readme:
    Rscript -e "rmarkdown::render('README.Rmd', output_options = list(html_preview = FALSE))"
    cp README.md ../README.md
