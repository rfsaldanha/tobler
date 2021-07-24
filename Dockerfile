FROM rocker/r-ver:3.6.3

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    gdal-bin \
    proj-bin \
    libgdal-dev \
    libproj-dev


# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown'), repos='$MRAN')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    chown shiny:shiny /var/lib/shiny-server

# install R packages required 
RUN R -e "install.packages('argonR', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('argonDash', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
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
COPY modules /srv/shiny-server/modules
COPY reports_rmd /srv/shiny-server/reports_rmd
COPY www /srv/shiny-server/www

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]