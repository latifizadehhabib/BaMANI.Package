# BaMANI.Package

`BaMANI.Package` is an R package that provides tools and functionalities for Bayesian Network Analysis, including a Shiny application for interactive data analysis.

## Installation

### Prerequisites

Ensure you have the following prerequisites installed:
- R (version 4.0 or higher)
- Rtools (for Windows users, required for building packages from source)

### From Local File

1. **Download the Package Archive:**
   Download the `BaMANI.Package_0.1.0.tar.gz` file from the repository.

2. **Install the Package:**
   Use the following R command to install the package from the downloaded file:

   ```r
   install.packages("path/to/BaMANI.Package_0.1.0.tar.gz", repos = NULL, type = "source")


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
     setwd("path/to/BaMANI.Package")

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
- Ensure all package dependencies are installed. Use the following R command to install any missing packages:

  ```r
   required_packages <- c("shiny", "shinydashboard", "shinyjs", "shinyWidgets", "DT", "purrr", "parallel", "bnlearn", "visNetwork", "plotly", "shinyalert", "htmltools")
   install.packages(setdiff(required_packages, installed.packages()[,"Package"]))


## Contributing

We welcome contributions to improve BaMANI.Package. If you have any suggestions or find any issues, please open an issue or submit a pull request.

