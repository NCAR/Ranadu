#!/usr/bin/env Rscript
#
# Attempt to build and install Ranadu from an R script
#

build:
	# If a previous build exists, remove it.
	if [ -f Ranadu*gz ] ;\
	then \
		rm Ranadu*gz ;\
	fi;
	# Build Ranadu - creates a new .gz file.
	R CMD build .

install:
	# Install Ranadu in /usr/lib64/R/library
	set -e ;\
	BUILD_FILE=$$(ls *gz) ;\
	sudo R CMD INSTALL $$BUILD_FILE
	
	# Restart shiny server
	sudo systemctl restart shiny-server

clean:
	rm *gz
