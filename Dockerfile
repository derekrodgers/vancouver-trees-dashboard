FROM rocker/r-ver:4.4.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    pkg-config \
    zlib1g-dev \
    libpng-dev \
    gdal-bin \
    libgdal-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install Shiny
RUN R -e "install.packages('shiny', repos = 'https://packagemanager.posit.co/all/latest')"

# Set working directory and copy app files
WORKDIR /app
COPY . .

# Install R packages using renv
RUN R -e "install.packages('renv', repos = 'https://packagemanager.posit.co/all/latest')"
RUN R -e "renv::restore()"

EXPOSE 8080

# Run the app using the port Render provides
CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 8080)))"]