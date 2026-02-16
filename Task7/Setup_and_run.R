# ==============================================================================
# Task 7: SETUP AND RUN - Complete Installation and Execution
# ==============================================================================
# This script will:
# 1. Check and install all required packages
# 2. Verify Rschach package is available
# 3. Run the automated test version
# ==============================================================================

cat("\n")
cat("==============================================================\n")
cat("  TASK 7: SETUP AND RUN - Installing Dependencies\n")
cat("==============================================================\n\n")

# ==============================================================================
# STEP 1: CHECK AND INSTALL CRAN PACKAGES
# ==============================================================================

cat("STEP 1: Checking CRAN packages...\n\n")

required_packages <- c(
  "dplyr",
  "tidyverse",
  "ggplot2",
  "rstan",
  "DiceKriging",
  "gridExtra",
  "Rcpp"
)

for (pkg in required_packages) {
  cat(sprintf("Checking %s... ", pkg))
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("NOT FOUND - Installing...\n")
    install.packages(pkg, repos = "https://cran.rstudio.com/")
    if (require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(sprintf("  ✓ %s installed successfully\n", pkg))
    } else {
      cat(sprintf("  ✗ ERROR: Failed to install %s\n", pkg))
      stop(sprintf("Could not install required package: %s", pkg))
    }
  } else {
    cat("✓ Installed\n")
  }
}

cat("\n")

# ==============================================================================
# STEP 2: CHECK RSCHACH PACKAGE
# ==============================================================================

cat("STEP 2: Checking Rschach package...\n\n")

if (!require("Rschach", quietly = TRUE)) {
  cat("✗ Rschach is NOT installed\n\n")
  cat("IMPORTANT: You need to install Rschach manually.\n")
  cat("Please follow these steps:\n\n")
  cat("1. Download Rschach_1.0.tar.gz from TUWEL (if you haven't already)\n")
  cat("2. In RStudio, go to: Tools → Install Packages...\n")
  cat("3. Change 'Install from:' to 'Package Archive File (.zip; .tar.gz)'\n")
  cat("4. Browse to and select: Rschach_1.0.tar.gz\n")
  cat("5. Click 'Install'\n\n")
  cat("OR run this command with the correct path:\n")
  cat('  install.packages("path/to/Rschach_1.0.tar.gz", repos = NULL, type = "source")\n\n')

  cat("After installing Rschach, run this script again.\n")
  stop("Rschach package not found")
} else {
  cat("✓ Rschach is installed\n\n")
}

# ==============================================================================
# STEP 3: VERIFY ALL PACKAGES LOAD
# ==============================================================================

cat("STEP 3: Loading all packages...\n\n")

library(Rschach)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rstan)
library(DiceKriging)
library(gridExtra)

cat("✓ All packages loaded successfully!\n\n")

# ==============================================================================
# STEP 4: VERIFY FILES EXIST
# ==============================================================================

cat("STEP 4: Checking required files...\n\n")

required_files <- c(
  "elo_model.stan",
  "8moves_v3.epd",
  "part7_analysis.R"
)

all_files_exist <- TRUE
for (file in required_files) {
  cat(sprintf("Checking %s... ", file))
  if (file.exists(file)) {
    cat("✓ Found\n")
  } else {
    cat("✗ NOT FOUND\n")
    all_files_exist <- FALSE
  }
}

if (!all_files_exist) {
  cat("\n✗ ERROR: Some required files are missing!\n")
  cat("Make sure you are in the Task7 directory.\n")
  cat("Current directory: ", getwd(), "\n")
  stop("Missing required files")
}

cat("\n✓ All required files found!\n\n")

# ==============================================================================
# STEP 5: CONFIGURE STAN
# ==============================================================================

cat("STEP 5: Configuring Stan...\n\n")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

cat(sprintf("✓ Stan configured with %d cores\n\n", parallel::detectCores()))

# ==============================================================================
# READY TO RUN
# ==============================================================================

cat("==============================================================\n")
cat("  ✓ SETUP COMPLETE - ALL DEPENDENCIES READY!\n")
cat("==============================================================\n\n")

cat("Ready to run Task 7!\n\n")

cat("Run FULL VERSION (2D Bayesian Optimization):\n")
cat("  - Takes ~3-4 hours\n")
cat("  - Interactive: asks for approval at each iteration\n")
cat("  - Creates visualizations and final results\n")
cat("  - Recommended: Run overnight or in background\n\n")
cat("  Command:\n")
cat('    source("part7_analysis.R")\n\n')

cat("NOTE: The script will ask for approval before each iteration.\n")
cat("      You can stop at any time by typing 'n' when prompted.\n\n")

cat("==============================================================\n\n")

