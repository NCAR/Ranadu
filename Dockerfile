# This file is tested on fedora 33 and ubunto 20.04, not yet on windows.
# Also not tested on Mac, but probably will work there?
# To use this Dockerfile:
# 1. Install docker following the instructions at 
#    https://docs.docker.com/engine/install/ and start the docker server.
# 2. Copy this Dockerfile to a new directory (e.g., ~/Docker) and 
#    change to that directory.
# 3. Build the image via a command like:
#    sudo docker build -t ranadu .
#    --> Note the "." at the end of the line
#    Expect this to take around 45 min; an entire operating system
#    will be downloaded along with rstudio, R, R packages, utilities, etc.
# 4. Place an appropriate project directory and associated netCDF files 
#    on the host system.
#   * Example: /home/[userName]/Data/SOCRATES/SOCRATESrf01.nc, etc.
#   The Data directory can have multiple projects.
# 5. After changing "YourPasswd" to your choice for a passward and replacing
#    [userName] with your home-directory name (e.g., cooperw in my case),
#    run the image via a command like:
#    sudo docker run -dp 8787:8787 --mount type=bind,source=/home/[userName]/Data,target=/home/rstudio/Data -e ROOT=TRUE -e PASSWORD=YourPasswd ranadu:latest
# 6. Direct your browser to http://localhost:8787 and login as user rstudio, 
#    using the password you assigned in step 5.
#    (Note that rstudio server is running at this address, so you could
#    optionally log in to the localhost URL from another computer.)
# 7. Specify a new project:
#    * "File -> new project -> existing directory -> browse -> RStudio -> Ranadu -> choose -> create project
# 8. If you want to start the Shiny app, select "global.R" from the 
#    "Files" option at the bottom right panel, then click "Run App"
# --> NOTE: any changes you make to the source routines will be lost
#           when the container stops, so read "docker" documentation
#           for ways to avoid this.
FROM rocker/r-ver:4.0.4

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/rocker-org/rocker-versioned" \
      org.label-schema.vendor="Rocker Project" \
      maintainer="Carl Boettiger <cboettig@ropensci.org>"

ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=latest
ENV PATH=/usr/lib/rstudio-server/bin:$PATH


RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh
RUN apt-get install -y --no-install-recommends \
  libxml2 \
  libnetcdf-dev \
  libtk8.6
RUN R -e "install.packages(c('maps',\
  'ncdf4',\
  'ggplot2'))"
RUN R -e "install.packages(c(\
  'scales',\
  'ggthemes',\
  'nleqslv',\
  'zoo',\
  'fields',\
  'signal'))"
RUN R -e "install.packages(c(\
  'reshape2',\
  'bspec',\
  'dplyr',\
  'plyr',\
  'tibble',\
  'magrittr',\
  'shiny',\
  'XML',\
  'devtools',\
  'png',\
  'shinyBS'))"
RUN R -e "install.packages('allanvar')"

RUN mkdir /home/rstudio/RStudio /home/rstudio/Data
RUN chown rstudio:rstudio /home/rstudio/RStudio /home/rstudio/Data
RUN cd /home/rstudio/RStudio
RUN mkdir /home/rstudio/RStudio/Ranadu
RUN chown rstudio:rstudio /home/rstudio/RStudio/Ranadu
RUN git clone https://github.com/NCAR/Ranadu /home/rstudio/RStudio/Ranadu
RUN cd /home/rstudio/RStudio/Ranadu
RUN chown rstudio:rstudio -R /home/rstudio/RStudio/Ranadu/*
RUN R -e "devtools::build('/home/rstudio/RStudio/Ranadu')"
RUN R -e "devtools::install('/home/rstudio/RStudio/Ranadu')"
RUN chown rstudio:rstudio -R /home/rstudio/RStudio/Ranadu/.git

EXPOSE 8787

CMD ["/init"]

