FROM rocker/shiny:latest

# Install the missing system library required by the fs package
RUN apt-get update && apt-get install -y libuv1

# Copy app files into the Shiny server directory
COPY . /srv/shiny-server/

EXPOSE 3838