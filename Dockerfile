FROM rocker/r-ver:latest

# Use pre-compiled binaries so packages install instantly without errors
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest'))" >> /usr/local/lib/R/etc/Rprofile.site

# Explicitly install shiny and httpuv
RUN R -e "install.packages(c('shiny', 'httpuv'))"

# Copy your app files into the container
WORKDIR /app
COPY . /app

# Expose Render's port
EXPOSE 10000

# Run your app
CMD ["Rscript", "app.R"]