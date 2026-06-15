FROM rocker/rstudio:4.4.1

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    # font rendering — systemfonts, ragg, ggplot2
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    # text shaping — textshaping, ragg, svglite, kableExtra
    libharfbuzz-dev \
    libfribidi-dev \
    # string encoding — stringi
    libicu-dev \
    # the binary — renv uses this to download
    curl \
    # web/auth — curl, httr, openssl
    libcurl4-openssl-dev \
    libssl-dev \
    # xml parsing — xml2, rvest
    libxml2-dev \
    # compilation
    cmake \
    make \
    # document rendering
    pandoc \
    # extra
    libx11-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

USER rstudio
RUN R -e "install.packages('rstudioapi')"
USER root

WORKDIR /home/rstudio/within-group-gcomp
COPY . .
RUN chown -R rstudio:rstudio /home/rstudio/within-group-gcomp


USER rstudio
RUN R -e "renv::restore()"
USER root

COPY create_rprofile.sh /tmp/create_rprofile.sh
RUN /bin/bash /tmp/create_rprofile.sh && rm /tmp/create_rprofile.sh
