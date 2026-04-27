# Use a stable, lean R base
FROM rocker/r-ver:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Install remotes first (Caching logic)
RUN R -e "install.packages('remotes')"

# Install the package from GitHub
# REPLACE 'yourusername/solar4r' with your actual GitHub handle
RUN R -e "remotes::install_github('yourusername/solar4r', upgrade='never', force=TRUE)"

# Copy ONLY the entrypoint and config (keeps the image light)
# We don't need the full source code in the image anymore!
COPY entrypoint.R /app/entrypoint.R
COPY config.json /app/config.json

# Make the CLI script executable
RUN chmod +x /app/entrypoint.R

# Set the entrypoint
ENTRYPOINT ["/app/entrypoint.R"]