FROM rocker/r-ver:latest

# Install system dependencies needed for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Install Shiny and any other required libraries
RUN R -e "install.packages('shiny', repos='https://cloud.r-project.org/')"

# Copy your project files into the container
WORKDIR /app
COPY . /app

# Expose the default port
EXPOSE 10000

# Command to run your app
CMD ["Rscript", "app.R"]