# SSSOM Mapping Dashboard

A comprehensive R Shiny dashboard for creating, analyzing, and visualizing SSSOM (Simple Standard for Sharing Ontological Mappings) compliant data mappings between different data standards.

## Features

- ✅ **Interactive Mapping Creation**: Upload variable lists and create mappings visually
- ✅ **SSSOM Compliance**: Full support for SSSOM standard format
- ✅ **Rich Visualizations**: Class-level analysis, confidence distributions, predicate charts
- ✅ **Export Capabilities**: Export as SSSOM TSV, Excel, or HTML reports
- ✅ **Variable Descriptions**: Support for detailed variable descriptions in mapping analysis

## Quick Start

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Installation

1. Clone this repository:
```bash
git clone https://github.com/.git
cd sssom-mapping-dashboard


install.packages(c("shiny", "shinydashboard", "shinyWidgets", "DT", 
                   "plotly", "dplyr", "readxl", "openxlsx", "stringr", 
                   "RColorBrewer"))

shiny::runApp("app.R")                  
                   
                   