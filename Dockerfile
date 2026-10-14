FROM rocker/r-ver:latest

# Install system dependencies required by R packages like curl, openssl, xml2, and sass
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
    libjpeg-dev

# Install Shiny and essential tidyverse/htmltools packages
RUN R -e "install.packages(c('shiny', 'sass'), repos='https://cloud.r-project.org/')"

# Copy your app files into the container
WORKDIR /app
COPY . /app

# Expose the default port for Render
EXPOSE 10000

# Run your app and bind it to Render's port
CMD ["Rscript", "app.R"]