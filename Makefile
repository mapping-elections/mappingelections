update-data :
	git submodule update --recursive --remote
	Rscript --vanilla data-raw/elections-data.R

