FROM rocker/shiny:latest

# Copy your app files directly into Shiny Server's folder
COPY . /srv/shiny-server/

# Expose Shiny Server's default port
EXPOSE 3838