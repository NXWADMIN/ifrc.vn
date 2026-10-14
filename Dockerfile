FROM rocker/r-ver:latest

# Force R to use pre-compiled binaries to avoid compilation errors
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest'))" >> /usr/local/lib/R/etc/Rprofile.site

# Install system dependencies including zlib
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    zlib1g-dev

# Install Shiny, Sass, and Bslib packages
RUN R -e "install.packages(c('shiny', 'sass', 'bslib', 'httpuv'), repos='https://cloud.r-project.org/')"

# Copy your app files into the container
WORKDIR /app
COPY . /app

# Expose the default port for Render
EXPOSE 10000

# Run your app and bind it to Render's port
CMD ["Rscript", "app.R"]