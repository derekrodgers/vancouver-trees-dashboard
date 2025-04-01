FROM rocker/r-ver:4.4.0

WORKDIR /app
COPY . .

RUN apt-get update && apt-get install -y \
    gdal-bin \
    libcurl4-openssl-dev \
    libgdal-dev \
    libicu-dev \
    libpng-dev \
    libssl-dev \
    libx11-dev \
    pandoc \
    zlib1g-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
RUN Rscript -e "renv::restore()"

EXPOSE 8080
CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 8080)))"]