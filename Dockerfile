# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest
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
    gdal-bin \
    proj-bin \
    libgdal-dev \
    libproj-dev
    
# update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean  
    
# install R packages required 
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
COPY tobler_app.Rproj ./app
COPY app.R ./app
COPY footer.R ./app
COPY header.R ./app
COPY sidebar.R ./app
COPY LICENSE ./app
ADD modules/ ./app/modules/
ADD reports_rmd/ ./app/reports_rmd/
ADD www/ ./app/www/
# select port
EXPOSE 3838
# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]