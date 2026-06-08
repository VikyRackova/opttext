FROM rocker/r-ver:4.4.0

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libtbb-dev \
    libxml2-dev \
    make \
    g++ \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('remotes', 'devtools', 'Rcpp', 'Matrix', 'ggplot2', 'stopwords', 'quanteda'))"
RUN R -e "remotes::install_version('RcppParallel', version='5.1.7', repos='https://cran.r-project.org')"
RUN Rscript -e "cat('RcppParallelLibs:', RcppParallel::RcppParallelLibs(), '\n')"

WORKDIR /opttext
COPY . .
RUN R CMD build .
RUN R CMD INSTALL *.tar.gz
CMD ["R"]
