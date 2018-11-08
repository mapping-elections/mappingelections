update-data :
	git submodule update --recursive --remote
	Rscript data-raw/elections-data.R
	Rscript -e "devtools::install(dependencies = FALSE)"

