FROM rocker/r-ver:bookworm

# Force R to use pre-compiled Linux binaries from Posit
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest'))" >> /usr/local/lib/R/etc/Rprofile.site

# Explicitly install shiny and httpuv with dependencies
RUN R -e "install.packages(c('shiny', 'httpuv'), dependencies = TRUE)"

# Copy your app files into the container
WORKDIR /app
COPY . /app

# Expose Render's port
EXPOSE 10000

# Run your app
CMD ["Rscript", "app.R"]