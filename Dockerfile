FROM rocker/r-ver:latest

# Force R to use pre-compiled Linux binaries so packages install instantly
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest'))" >> /usr/local/lib/R/etc/Rprofile.site

# Install all required system dependencies (including libuv1 for fs and zlib)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libuv1-dev \
    zlib1g-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

# Install all core R packages safely as binaries
RUN R -e "install.packages(c('shiny', 'sass', 'bslib', 'fs', 'httpuv'))"

# Copy your app files into the container
WORKDIR /app
COPY . /app

# Expose Render's default port
EXPOSE 3838

# Run your app directly
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"]