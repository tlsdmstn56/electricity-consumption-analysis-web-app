FROM r-base:latest

MAINTAINER Eunsoo Sheen "eunsoo.sheen@gmail.com"

RUN NCPUS=$(nproc --all) && apt-get update && apt-get install -y -t unstable \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libssl-dev \
    git \
    libxt-dev && \
    wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'rmarkdown','DT','readr','dplyr'), repos='https://cran.rstudio.com/', verbose=FALSE, quiet=FALSE, Ncpus=$NCPUS)" && \
    R -e "install.packages(c('ggplot2','plotly','shinycssloaders','gridExtra','tidyr','tibble','processx'), repos='https://cran.rstudio.com/', verbose=FALSE, quiet=FALSE, Ncpus=$NCPUS)" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 3838

COPY ../../external/shiny-docker/shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
