# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:3.6.3

# update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean  

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libudunits2-dev \
    gdal-bin \
    proj-bin \
    libgdal-dev \
    libproj-dev
    
# install R packages required 
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('argonR', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('argonDash', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rgdal', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('cleangeo', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('highcharter', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tmap', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('htmltools', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('spdep', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('spatialreg', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reshape2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plm', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('splm', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('statquotes', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('gpiras/sphet')"

# copy the app to the image
COPY tobler_app.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY footer.R /srv/shiny-server/
COPY header.R /srv/shiny-server/
COPY sidebar.R /srv/shiny-server/
COPY LICENSE /srv/shiny-server/
COPY modules/* /srv/shiny-server/modules/
COPY reports_rmd/* /srv/shiny-server/reports_rmd/
COPY www/* /srv/shiny-server/www/

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server"]