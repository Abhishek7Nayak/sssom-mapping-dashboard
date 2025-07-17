# SSSOM Mapping Dashboard

A **comprehensive R Shiny dashboard** for **creating, analyzing, and visualizing SSSOM (Simple Standard for Sharing Ontological Mappings) compliant mappings** between different data models.  
Built to accelerate harmonization projects, the dashboard is ideal for ontologists, data modelers, and standards organizations.

---

## 🚀 Features

- **Interactive Mapping Creation:**  
  Upload variable lists and visually create new mappings between your source and target data models.

- **SSSOM Compliance:**  
  Full support for SSSOM standard format for maximum interoperability.

- **Rich Visualizations:**  
  Analyze your mappings with class-level overviews, confidence score distributions, and predicate-type charts.

- **Export Capabilities:**  
  Export mappings as SSSOM TSV, Excel, or HTML summary reports—ready for sharing or downstream processing.

- **Variable Descriptions:**  
  Supports and displays detailed descriptions for both source and target variables.

- **SQLite Integration:**  
  Optionally persists uploaded variables and mappings across sessions for easier collaboration.

---

## 🏁 Quick Start

### Prerequisites

- [R](https://cran.r-project.org/) (version 4.0 or higher)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended for easiest use)
- macOS, Windows, or Linux

### Installation

1. **Clone the repository:**
    ```bash
    git clone https://github.com/<your-username>/sssom-mapping-dashboard.git
    cd sssom-mapping-dashboard
    ```

2. **Install required R packages:**  
   (You only need to do this once)
    ```r
    install.packages(c(
      "shiny", "shinydashboard", "shinyWidgets", "DT", 
      "plotly", "dplyr", "readxl", "openxlsx", "stringr", 
      "RColorBrewer", "DBI", "RSQLite"
    ))
    ```

3. **Run the dashboard:**
    ```r
    shiny::runApp("app.R")
    ```
    Or, open `app.R` in RStudio and click **Run App**.

---

## 🖥️ Usage Guide

1. **Load Variable Lists or Existing Mappings**
   - **Upload variable lists:** Start from scratch by uploading CSV or Excel files describing your source and target variables.
   - **Upload existing mappings:** Import previously created SSSOM, legacy, or simple-format mapping files for visualization or further editing.

2. **Interactive Mapping (Variables mode)**
   - Select variables from each model, specify the mapping relationship, confidence, and justification, and add mappings one by one.

3. **Visualization & Analysis**
   - Use the Class Explorer and Mapping Analysis tabs to explore predicate distributions, class mapping coverage, and confidence scores.

4. **Export**
   - Download your completed mappings as SSSOM TSV (with full metadata), Excel, or as an HTML summary report for documentation and sharing.

---

## 📂 File Formats Supported

- **Variables:**  
  - CSV or Excel files with columns: `variable_name`, `description`, `class`, `data_type`
- **Mappings:**  
  - SSSOM TSV (with or without metadata header)
  - Excel or CSV in legacy or simplified column format

---

## 📝 Example Data

- Use the **Sample Data** button in the app to explore its features without uploading your own files.

---

## 🛠️ Troubleshooting

- If you get missing package errors, re-run the installation command above.
- For SQLite database issues, ensure you have write permission in the app directory.
- For any other issues, please open an [issue](https://github.com/<your-username>/sssom-mapping-dashboard/issues).

---

## 📜 License

This project is released under the **MIT License**.  
See [LICENSE](LICENSE) for details.

---

## 🤝 Contributing

Pull requests, bug reports, and feature suggestions are welcome!  
Please fork the repo, open a PR, or create an issue for discussion.

---

## 👤 Author

Created by **Abhishek Nayak**  
For questions, contact: nayak.abhishek@ul.ie

---
## 📖 Acknowledgements

This dashboard was built to support the [**Simple Standard for Sharing Ontological Mappings (SSSOM)**](https://mapping-commons.github.io/sssom/) standard, maintained by the [Mapping Commons](https://mapping-commons.github.io/) community.  
Special thanks to the contributors of the [SSSOM GitHub repository](https://github.com/mapping-commons/sssom) for their open science leadership and community resources.

## 📑 Citation

If you use this dashboard in your work, please cite the SSSOM specification:

> Matentzoglu, N., Balhoff, J. P., Bello, S. M., Brown, A. G., Brush, M. H., & others. (2022). Simple Standard for Sharing Ontological Mappings (SSSOM). *Zenodo*. https://doi.org/10.5281/zenodo.6407625

You can also cite the SSSOM GitHub repository:  
[https://github.com/mapping-commons/sssom](https://github.com/mapping-commons/sssom)


**Enjoy mapping!**

---

