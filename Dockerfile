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

# Set working directory
WORKDIR /app

# Copy lockfile and renv directory first to leverage Docker caching
COPY renv.lock renv.lock
COPY renv/ renv/

# Install R packages using renv
RUN R -e "install.packages('renv', repos = 'https://packagemanager.posit.co/all/latest')"
RUN R -e "renv::restore()"

# Copy the rest of the app
COPY . .

EXPOSE 8080

# Run the app
CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 8080)))"]