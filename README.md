# BaMANI.Package

`BaMANI.Package` ('BaMANI': 'Bayesian Multi-Algorithm Causal Network Inference') is an R package that provides tools and functionalities designed for Bayesian network Analysis, including a Shiny application for interactive data analysis. It leverages a variety of algorithms and incorporates user-defined constraints to improve the accuracy of inferring structure and causality within networks from observational data. 

## Installation

### Prerequisites

Ensure you have the following prerequisites installed:
- R (version 4.0 or higher)
- Rtools (for Windows users, required for building packages from source)

### From Local File

1. **Download the Package Archive:**
   Download the `BaMANI.Package_0.1.0.tar.gz` file from the repository.

2. **Install the Package:**
   Use the following R command to install the package from the downloaded file. Replace `path/to/` in the command below with the actual path to where you saved the downloaded file.

   ```r
   install.packages("path/to/BaMANI.Package_0.1.0.tar.gz", repos = NULL, type = "source")
   
Before running the BaMANI app provided by the package, install Rgraphviz package using "BiocManager" by the below command lines;
> install.packages("BiocManager")
> 
## Usage

To run the Shiny app provided by the package:

1. **Load the Package:**

   ```r
   library(BaMANI.Package)

2. **Run the Shiny App:**
   ```r
   BaMANI.Package::run_app()

## Development

### Setting Up the Development Environment

To set up the development environment and build the package from source, follow these steps:

1. **Clone the Repository:**
   ```sh
   git clone https://github.com/yourusername/BaMANI.Package.git
   cd BaMANI.Package

2. **Open R or RStudio:** 

Set the working directory to the package directory:
    ```r
    
    setwd("path/to/BaMANI.Package)   
 
3. **Load 'devtools' and Generate Documentation:** 

   ```r
   # Load devtools
   library(devtools)

   # Generate documentation using roxygen2
   document()

4. **Build the Package:**
      ```r
      build()

5. **Check the Package:**
      ```r
      check()

6. **Install the Package Locally:**

      ```r
      install.packages("BaMANI.Package_0.1.0.tar.gz", repos = NULL, type = "source")

## Troubleshooting

- Ensure Rtools is installed and properly configured. You can download it from CRAN Rtools.
- Installation of Rgraphviz package:
  ```r
  install.packages("BiocManager")
  BiocManager::install("Rgraphviz")
  # Load Rgraphviz package:
  library(Rgraphviz)  

- Ensure all package dependencies are installed. Use the following R command to install any missing packages:

  ```r
   required_packages <- c("shiny", "shinydashboard", "shinyjs", "shinyWidgets", "DT", "purrr", "parallel", "bnlearn", "visNetwork", "plotly", "shinyalert", "htmltools")
   install.packages(setdiff(required_packages, installed.packages()[,"Package"]))


