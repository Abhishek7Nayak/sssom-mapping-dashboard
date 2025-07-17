library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(dplyr)
library(readxl)
library(openxlsx)
library(stringr)
library(RColorBrewer)
library(DBI)
library(RSQLite)
# Helper functions for SSSOM compliance
validate_sssom_mappings <- function(df) {
  required_cols <- c("subject_id", "predicate_id", "object_id")
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if(length(missing_cols) > 0) {
    return(list(valid = FALSE, message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))))
  }
  
  valid_predicates <- c("skos:exactMatch", "skos:closeMatch", "skos:broadMatch", 
                        "skos:narrowMatch", "skos:relatedMatch", "sssom:NoMatch")
  
  if("predicate_id" %in% colnames(df)) {
    invalid_predicates <- setdiff(unique(df$predicate_id), valid_predicates)
    if(length(invalid_predicates) > 0) {
      return(list(valid = FALSE, message = paste("Invalid predicates:", paste(invalid_predicates, collapse = ", "))))
    }
  }
  
  return(list(valid = TRUE, message = "SSSOM format validated successfully"))
}

convert_to_sssom <- function(df, format_type = "auto", source_prefix_value = "CLL:", target_prefix_value = "1MG:") {
  # Store original dataframe in case we need it
  df_converted <- df
  
  # Auto-detect format and convert to SSSOM
  if(format_type == "auto") {
    # Check column names to detect format
    col_names <- colnames(df)
    
    # Check if it's already SSSOM format
    if(all(c("subject_id", "predicate_id", "object_id") %in% col_names)) {
      format_type <- "sssom"
    }
    # Check for legacy format columns
    else if(all(c("Mapping Status", "CLL Variable", "1+MG Variables That Map to This") %in% col_names)) {
      format_type <- "legacy"
    }
    # Check for simple format columns
    else if(all(c("cll_variable", "relationship", "1mg_variable") %in% col_names) ||
            all(c("subject_variable", "relationship", "object_variable") %in% col_names)) {
      format_type <- "simple"
    }
    # Default to simple if can't detect
    else {
      format_type <- "simple"
    }
  }
  
  if(format_type == "legacy") {
    # Convert from legacy format
    # Check if required columns exist
    required_legacy_cols <- c("CLL Variable", "1+MG Variables That Map to This", "Mapping Status")
    if(!all(required_legacy_cols %in% colnames(df))) {
      stop("Legacy format requires columns: 'CLL Variable', '1+MG Variables That Map to This', 'Mapping Status'")
    }
    
    df_converted <- df %>%
      mutate(
        subject_id = paste0(source_prefix_value, str_replace_all(`CLL Variable`, " ", "_")),
        object_id = ifelse(`1+MG Variables That Map to This` == "None" | 
                             is.na(`1+MG Variables That Map to This`) | 
                             `1+MG Variables That Map to This` == "", 
                           NA, 
                           paste0(target_prefix_value, str_replace_all(`1+MG Variables That Map to This`, " ", "_"))),
        predicate_id = case_when(
          `Mapping Status` == "Complete" ~ "skos:closeMatch",
          `Mapping Status` == "Extra" ~ "sssom:NoMatch",
          `Mapping Status` == "Partial" ~ "skos:relatedMatch",
          TRUE ~ "skos:relatedMatch"
        ),
        confidence = case_when(
          `Mapping Status` == "Complete" ~ 0.85,
          `Mapping Status` == "Extra" ~ 0.95,
          `Mapping Status` == "Partial" ~ 0.60,
          TRUE ~ 0.50
        ),
        match_type = "HumanCurated",
        subject_class = ifelse("1+MG Class" %in% colnames(df), `1+MG Class`, ""),
        object_class = ifelse("1+MG Class" %in% colnames(df), `1+MG Class`, ""),
        mapping_justification = ifelse("Notes" %in% colnames(df), Notes, "")
      )
    
  } else if(format_type == "simple") {
    # Convert from simple CSV/Excel format
    # Handle different column naming conventions
    if("cll_variable" %in% colnames(df)) {
      subject_col <- "cll_variable"
      object_col <- "1mg_variable"
    } else if("subject_variable" %in% colnames(df)) {
      subject_col <- "subject_variable"
      object_col <- "object_variable"
    } else if("source_variable" %in% colnames(df)) {
      subject_col <- "source_variable"
      object_col <- "target_variable"
    } else {
      # Try to find columns containing these terms
      subject_col <- grep("subject|source|cll", colnames(df), ignore.case = TRUE, value = TRUE)[1]
      object_col <- grep("object|target|1mg|mg", colnames(df), ignore.case = TRUE, value = TRUE)[1]
    }
    
    if(is.na(subject_col) || is.na(object_col)) {
      stop("Cannot identify source and target variable columns in simple format")
    }
    
    # Check if relationship column exists
    rel_col <- grep("relationship|relation|predicate|mapping", colnames(df), ignore.case = TRUE, value = TRUE)[1]
    if(is.na(rel_col)) {
      # If no relationship column, default to closeMatch
      df$relationship <- "close_match"
      rel_col <- "relationship"
    }
    
    df_converted <- df %>%
      mutate(
        subject_id = paste0(source_prefix_value, str_replace_all(!!sym(subject_col), " ", "_")),
        object_id = ifelse(is.na(!!sym(object_col)) | !!sym(object_col) == "", 
                           NA, 
                           paste0(target_prefix_value, str_replace_all(!!sym(object_col), " ", "_"))),
        predicate_id = case_when(
          str_to_lower(!!sym(rel_col)) %in% c("exact", "exact_match", "exactmatch") ~ "skos:exactMatch",
          str_to_lower(!!sym(rel_col)) %in% c("close", "close_match", "closematch") ~ "skos:closeMatch",
          str_to_lower(!!sym(rel_col)) %in% c("broad", "broad_match", "broadmatch") ~ "skos:broadMatch",
          str_to_lower(!!sym(rel_col)) %in% c("narrow", "narrow_match", "narrowmatch") ~ "skos:narrowMatch",
          str_to_lower(!!sym(rel_col)) %in% c("related", "related_match", "relatedmatch") ~ "skos:relatedMatch",
          str_to_lower(!!sym(rel_col)) %in% c("no_match", "nomatch", "none", "na") ~ "sssom:NoMatch",
          TRUE ~ "skos:relatedMatch"
        )
      )
    
    # Check for confidence column
    conf_col <- grep("confidence|score|similarity", colnames(df), ignore.case = TRUE, value = TRUE)[1]
    if(!is.na(conf_col)) {
      df_converted$confidence <- as.numeric(df[[conf_col]])
    }
    
    # Check for class columns
    subj_class_col <- grep("subject_class|source_class|cll_class", colnames(df), ignore.case = TRUE, value = TRUE)[1]
    obj_class_col <- grep("object_class|target_class|mg_class|1mg_class", colnames(df), ignore.case = TRUE, value = TRUE)[1]
    
    if(!is.na(subj_class_col)) {
      df_converted$subject_class <- df[[subj_class_col]]
    }
    if(!is.na(obj_class_col)) {
      df_converted$object_class <- df[[obj_class_col]]
    }
    
    # Check for justification column
    just_col <- grep("justification|reason|notes|comment", colnames(df), ignore.case = TRUE, value = TRUE)[1]
    if(!is.na(just_col)) {
      df_converted$mapping_justification <- df[[just_col]]
    }
    
  } else if(format_type == "sssom") {
    # Already in SSSOM format, just ensure all required columns exist
    df_converted <- df
  }
  
  # Ensure all required SSSOM columns exist
  required_cols <- c("subject_id", "predicate_id", "object_id", "confidence", "match_type", 
                     "subject_class", "object_class", "mapping_justification")
  
  for(col in required_cols) {
    if(!col %in% colnames(df_converted)) {
      if(col == "confidence") {
        df_converted[[col]] <- 0.75
      } else if(col == "match_type") {
        df_converted[[col]] <- "HumanCurated"
      } else if(col %in% c("subject_class", "object_class", "mapping_justification")) {
        df_converted[[col]] <- ""
      }
    }
  }
  
  # Preserve description columns if they exist in original
  desc_cols <- c("subject_description", "object_description", "subject_label", "object_label")
  for(col in desc_cols) {
    if(col %in% colnames(df) && !col %in% colnames(df_converted)) {
      df_converted[[col]] <- df[[col]]
    }
  }
  
  # Select only the columns we need, preserving order
  final_cols <- c("subject_id", "predicate_id", "object_id", "confidence", "match_type", 
                  "subject_class", "object_class", "mapping_justification")
  
  # Add optional columns if they exist
  optional_cols <- c("subject_description", "object_description", "subject_label", "object_label",
                     "author_id", "mapping_date", "subject_source", "object_source")
  
  for(col in optional_cols) {
    if(col %in% colnames(df_converted)) {
      final_cols <- c(final_cols, col)
    }
  }
  
  # Return only the columns that exist
  df_converted <- df_converted[, intersect(final_cols, colnames(df_converted)), drop = FALSE]
  
  return(df_converted)
}
# Function to extract variable data from mapping descriptions
# Function to extract variable data from mapping descriptions
extract_variables_from_mappings <- function(mappings_df, source_prefix_value, target_prefix_value) {
  if(!all(c("subject_description", "object_description") %in% colnames(mappings_df))) {
    # Try alternative column names
    if("subject_label" %in% colnames(mappings_df)) {
      mappings_df$subject_description <- mappings_df$subject_label
    }
    if("object_label" %in% colnames(mappings_df)) {
      mappings_df$object_description <- mappings_df$object_label
    }
    
    # If still no description columns, return NULL
    if(!all(c("subject_description", "object_description") %in% colnames(mappings_df))) {
      return(NULL)
    }
  }
  
  # Extract source variables (e.g., CLL)
  source_vars <- mappings_df %>%
    select(subject_id, subject_class, subject_description) %>%
    distinct() %>%
    filter(!is.na(subject_id), !is.na(subject_description), subject_description != "") %>%
    mutate(
      variable_id = subject_id,
      variable_name = str_replace(subject_id, paste0("^", source_prefix_value), ""),
      description = subject_description,
      class = ifelse(is.na(subject_class) | subject_class == "", "Unclassified", subject_class),
      data_type = "unknown"
    ) %>%
    select(variable_id, variable_name, description, data_type, class)
  
  # Extract target variables (e.g., 1+MG)
  target_vars <- mappings_df %>%
    select(object_id, object_class, object_description) %>%
    distinct() %>%
    filter(!is.na(object_id), !is.na(object_description), object_description != "") %>%
    mutate(
      variable_id = object_id,
      variable_name = str_replace(object_id, paste0("^", target_prefix_value), ""),
      description = object_description,
      class = ifelse(is.na(object_class) | object_class == "", "Unclassified", object_class),
      data_type = "unknown"
    ) %>%
    select(variable_id, variable_name, description, data_type, class)
  
  # Return NULL if no variables were extracted
  if(nrow(source_vars) == 0 && nrow(target_vars) == 0) {
    return(NULL)
  }
  
  return(list(cll = source_vars, mg = target_vars))
}

create_sample_variables <- function() {
  # Sample CLL variables
  cll_vars <- data.frame(
    variable_id = paste0("CLL_", sprintf("%03d", 1:15)),
    variable_name = c("birth_date", "gender", "diagnosis_date", "stage_rai", "treatment_type",
                      "start_date", "end_date", "ctcae_grade", "dose_mg", "frequency",
                      "response", "test_date", "duration_weeks", "adverse_events", "follow_up_date"),
    description = c("Patient birth date", "Patient gender", "Date of diagnosis", "RAI staging",
                    "Type of treatment", "Treatment start date", "Treatment end date", 
                    "CTCAE toxicity grade", "Drug dose in mg", "Dosing frequency",
                    "Treatment response", "Date of test", "Treatment duration in weeks",
                    "Adverse event occurrence", "Follow-up visit date"),
    data_type = c("date", "categorical", "date", "categorical", "categorical",
                  "date", "date", "integer", "numeric", "categorical",
                  "categorical", "date", "numeric", "boolean", "date"),
    class = c(rep("Patient", 2), rep("Diagnosis", 2), rep("Treatment", 9), rep("Follow-up", 2)),
    stringsAsFactors = FALSE
  )
  
  # Sample 1+MG variables
  mg_vars <- data.frame(
    variable_id = paste0("1MG_", sprintf("%03d", 1:12)),
    variable_name = c("Birth_Date", "Administrative_gender", "Date_of_Diagnosis", "Stage",
                      "Treatment_Type", "Treatment_start", "Treatment_end", "adverseEvents",
                      "Treatment_dose", "Treatment_response", "Treatment_duration", "Sample_date"),
    description = c("Date of birth", "Administrative gender", "Diagnosis date", "Disease stage",
                    "Type of therapy", "Treatment start date", "Treatment end date",
                    "Adverse event indicator", "Treatment dose", "Response to treatment",
                    "Duration of treatment", "Sample collection date"),
    data_type = c("date", "categorical", "date", "categorical", "categorical",
                  "date", "date", "boolean", "numeric", "categorical", "numeric", "date"),
    class = c(rep("Subject", 2), rep("Diagnosis", 2), rep("Treatment", 7), "Sample"),
    stringsAsFactors = FALSE
  )
  
  return(list(cll = cll_vars, mg = mg_vars))
}

create_sample_mappings <- function() {
  data.frame(
    subject_id = c("CLL:birth_date", "CLL:gender", "CLL:diagnosis_date", "CLL:stage_rai",
                   "CLL:treatment_type", "CLL:start_date", "CLL:ctcae_grade", "CLL:dose_mg",
                   "CLL:response", "CLL:test_date", "CLL:duration_weeks"),
    predicate_id = c("skos:exactMatch", "skos:exactMatch", "skos:exactMatch", "skos:broadMatch",
                     "skos:exactMatch", "skos:exactMatch", "skos:closeMatch", "skos:closeMatch",
                     "skos:exactMatch", "sssom:NoMatch", "skos:closeMatch"),
    object_id = c("1MG:Birth_Date", "1MG:Administrative_gender", "1MG:Date_of_Diagnosis", "1MG:Stage",
                  "1MG:Treatment_Type", "1MG:Treatment_start", "1MG:adverseEvents", "1MG:Treatment_dose",
                  "1MG:Treatment_response", NA, "1MG:Treatment_duration"),
    confidence = c(0.98, 0.95, 0.97, 0.80, 0.92, 0.98, 0.75, 0.85, 0.90, 0.95, 0.80),
    match_type = rep("HumanCurated", 11),
    subject_class = c(rep("Patient", 2), rep("Diagnosis", 2), rep("Treatment", 7)),
    object_class = c(rep("Subject", 2), rep("Diagnosis", 2), rep("Treatment", 6), "Sample"),
    mapping_justification = c(
      "Direct equivalence", "Direct equivalence", "Direct equivalence", "CLL RAI staging broader than general stage",
      "Direct equivalence", "Direct equivalence", "CTCAE grade maps to boolean adverse event flag",
      "Same concept, may need unit conversion", "Direct equivalence", "No equivalent in 1+MG",
      "Same concept, different units (weeks vs days)"
    ),
    author_id = rep("orcid:0000-0000-0000-0000", 11),
    mapping_date = rep("2025-05-28", 11),
    subject_source = rep("CLL_Registry_v1.0", 11),
    object_source = rep("1MG_Standard_v1.0", 11),
    subject_description = c(
      "Patient birth date in YYYY-MM-DD format", "Patient biological gender", "Date when CLL diagnosis was confirmed",
      "RAI staging system classification", "Primary treatment modality", "Treatment initiation date",
      "CTCAE toxicity grade 1-5", "Drug dosage in milligrams", "Patient response to treatment",
      "Date when diagnostic test was performed", "Treatment duration in weeks"
    ),
    object_description = c(
      "Date of birth in ISO format", "Administrative gender classification", "Date of primary diagnosis",
      "Disease staging information", "Type of therapeutic intervention", "Treatment commencement date",
      "Boolean indicator of adverse event occurrence", "Therapeutic dose amount", "Treatment response assessment",
      "Not available in 1+MG standard", "Duration of treatment in days"
    ),
    stringsAsFactors = FALSE
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "SSSOM Mapping Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("upload")),
      menuItem("Interactive Mapping", tabName = "mapping", icon = icon("exchange-alt")),
      menuItem("Class Explorer", tabName = "explorer", icon = icon("search")),
      menuItem("Mapping Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-top-color: #3c8dbc;
        }
        .mapping-row:hover {
          background-color: #f0f8ff;
          cursor: pointer;
        }
        .confidence-bar {
          height: 20px;
          background: linear-gradient(to right, #f44336, #ff9800, #4caf50);
          border-radius: 10px;
        }
        .description-box {
          background-color: #f8f9fa;
          padding: 10px;
          border-radius: 5px;
          border-left: 4px solid #007bff;
          margin-bottom: 10px;
          max-height: 120px;
          overflow-y: auto;
        }
      "))
    ),
    
    tabItems(
      # Data Input Tab
      tabItem(tabName = "input",
              fluidRow(
                box(
                  title = "Choose Input Method", status = "primary", solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4,
                           textInput("sourceModelName", "Source Model Name:", value = "ModelA")
                    ),
                    column(4,
                           textInput("targetModelName", "Target Model Name:", value = "ModelB")
                    ),
                    column(4,
                           textInput("defaultAuthor", "Default Author (Name or ORCID):", 
                                     value = "", 
                                     placeholder = "e.g., John Doe or orcid:0000-0000-0000-0000")
                    )
                  ),
                  br(),
                  radioButtons("inputMode", "Select data input approach:",
                               choices = list(
                                 "Upload Variable Lists (Create New Mappings)" = "variables",
                                 "Upload Existing Mappings (Analyze Mappings)" = "mappings"
                               ),
                               selected = "variables"),
                  hr()
                )
              ),
              
              # Variables Upload Mode
              conditionalPanel(
                condition = "input.inputMode == 'variables'",
                fluidRow(
                  box(
                    title = "Upload Variable Lists", status = "info", solidHeader = TRUE,
                    width = 6,
                    h4(textOutput("sourceModelLabel")),
                    fileInput("cllVariables", "Choose CLL Variables File",
                              accept = c(".xlsx", ".csv")),
                    p("Required columns: variable_name, description, class, data_type"),
                    
                    h4(textOutput("targetModelLabel")),
                    fileInput("mgVariables", "Choose 1+MG Variables File",
                              accept = c(".xlsx", ".csv")),
                    p("Required columns: variable_name, description, class, data_type"),
                    
                    br(),
                    actionButton("loadVariables", "Load Variables", class = "btn-success"),
                    br(), br(),
                    downloadButton("downloadVarTemplate", "Download Variable Template", class = "btn-info")
                  ),
                  
                  box(
                    title = "Variable Upload Status", status = "warning", solidHeader = TRUE,
                    width = 6,
                    verbatimTextOutput("variableStatus"),
                    conditionalPanel(
                      condition = "output.variablesLoaded",
                      h4("Data Preview"),
                      DT::dataTableOutput("variablePreview")
                    )
                  )
                )
              ),
              
              # Mappings Upload Mode
              conditionalPanel(
                condition = "input.inputMode == 'mappings'",
                fluidRow(
                  box(
                    title = "Upload Existing Mappings", status = "info", solidHeader = TRUE,
                    width = 6,
                    fileInput("mappingFile", "Choose Mapping File",
                              accept = c(".tsv", ".csv", ".xlsx")),
                    
                    radioButtons("mappingFormat", "File Format:",
                                 choices = list(
                                   "SSSOM TSV (with metadata)" = "sssom",
                                   "Excel/CSV with mapping columns" = "simple",
                                   "Legacy format (auto-convert)" = "legacy",
                                   "Auto-detect format" = "auto"
                                 ),
                                 selected = "auto"),
                    
                    br(),
                    actionButton("loadMappings", "Load Mappings", class = "btn-success"),
                    br(), br(),
                    downloadButton("downloadMappingTemplate", "Download Mapping Template", class = "btn-info")
                  ),
                  
                  box(
                    title = "Mapping Upload Status", status = "warning", solidHeader = TRUE,
                    width = 6,
                    verbatimTextOutput("mappingStatus"),
                    conditionalPanel(
                      condition = "output.mappingsLoaded",
                      h4("Mapping Preview"),
                      DT::dataTableOutput("mappingPreview")
                    )
                  )
                )
              ),
              
              # Load Sample Data
              fluidRow(
                box(
                  title = "Sample Data", status = "success", solidHeader = TRUE,
                  width = 12,
                  p("Don't have data files? Load sample data to explore the dashboard:"),
                  actionButton("loadSampleData", "Load Sample Data", class = "btn-warning")
                )
              )
      ),
      
      # Interactive Mapping Tab
      tabItem(tabName = "mapping",
              conditionalPanel(
                condition = "input.inputMode == 'variables'",
                fluidRow(
                  box(
                    title = "Interactive Mapping Studio", status = "primary", solidHeader = TRUE,
                    width = 12,
                    conditionalPanel(
                      condition = "!output.variablesLoaded",
                      h4("Please upload variable lists in the Data Input tab first.")
                    ),
                    conditionalPanel(
                      condition = "output.variablesLoaded",
                      fluidRow(
                        column(4,
                               h4(textOutput("sourceModelLabel")),
                               selectInput("filterCLLClass", "Filter by Class:", choices = NULL),
                               div(style = "height: 400px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px;",
                                   DT::dataTableOutput("cllVariablesList"))
                        ),
                        column(4,
                               h4("Mapping Controls"),
                               br(),
                               h5("Selected CLL Variable:"),
                               div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 4px solid #007bff; max-height: 150px; overflow-y: auto;",
                                   verbatimTextOutput("selectedCLLVar")),
                               br(),
                               h5("Mapping Relationship:"),
                               selectInput("mappingPredicate", "Choose Relationship:",
                                           choices = list(
                                             "Exact Match" = "skos:exactMatch",
                                             "Close Match" = "skos:closeMatch",
                                             "Broad Match" = "skos:broadMatch",
                                             "Narrow Match" = "skos:narrowMatch",
                                             "Related Match" = "skos:relatedMatch",
                                             "No Match" = "sssom:NoMatch"
                                           )),
                               sliderInput("mappingConfidence", "Confidence:", 
                                           min = 0, max = 1, value = 0.8, step = 0.05),
                               textAreaInput("mappingJustification", "Justification:", 
                                             placeholder = "Explain the mapping relationship...",
                                             rows = 3),
                               textInput("mappingAuthor", "Author (Name or ORCID):", 
                                         value = "", 
                                         placeholder = "e.g., John Doe or orcid:0000-0000-0000-0000"),
                               br(),
                               actionButton("createMapping", "Create Mapping", class = "btn-success"),
                               actionButton("suggestJustification", "Auto-Suggest", class = "btn-info btn-sm"),
                               br(), br(),
                               h5("Selected 1+MG Variable:"),
                               div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 4px solid #28a745; max-height: 150px; overflow-y: auto;",
                                   verbatimTextOutput("selectedMGVar"))
                        ),
                        column(4,
                               h4(textOutput("targetModelLabel")),
                               selectInput("filterMGClass", "Filter by Class:", choices = NULL),
                               div(style = "height: 400px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px;",
                                   DT::dataTableOutput("mgVariablesList"))
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  box(
                    title = "Created Mappings", status = "info", solidHeader = TRUE,
                    width = 12,
                    DT::dataTableOutput("createdMappings"),
                    br(),
                    actionButton("clearMappings", "Clear All Mappings", class = "btn-danger")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.inputMode == 'mappings'",
                fluidRow(
                  box(
                    title = "Mapping Editor", status = "primary", solidHeader = TRUE,
                    width = 12,
                    h4("Interactive mapping creation is available when uploading variable lists."),
                    p("Since you uploaded existing mappings, use the Class Explorer tab to view and analyze your mappings.")
                  )
                )
              )
      ),
      
      # Class Explorer Tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(
                  title = "Class Mapping Explorer", status = "primary", solidHeader = TRUE,
                  width = 12,
                  conditionalPanel(
                    condition = "!output.mappingsLoaded && !output.variablesLoaded",
                    h4("Please load data in the Data Input tab first.")
                  ),
                  conditionalPanel(
                    condition = "output.mappingsLoaded || output.variablesLoaded",
                    fluidRow(
                      column(4,
                             selectInput("selectedClass", "Select Class to Explore:",
                                         choices = NULL)
                      ),
                      column(4,
                             uiOutput("classStatsUI")
                      ),
                      column(4,
                             actionButton("refreshClassView", "Refresh View", class = "btn-info")
                      )
                    )
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.mappingsLoaded || output.variablesLoaded",
                fluidRow(
                  box(
                    title = "Class Mappings", status = "info", solidHeader = TRUE,
                    width = 12,
                    DT::dataTableOutput("classMappingsTable")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Variable Details", status = "warning", solidHeader = TRUE,
                    width = 12,
                    uiOutput("variableDetailsUI")
                  )
                )
              )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
              conditionalPanel(
                condition = "!output.mappingsLoaded",
                fluidRow(
                  box(
                    title = "Analysis Dashboard", status = "primary", solidHeader = TRUE,
                    width = 12,
                    h4("Please load mapping data first to view analysis.")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.mappingsLoaded",
                fluidRow(
                  valueBoxOutput("totalMappings"),
                  valueBoxOutput("avgConfidence"),
                  valueBoxOutput("mappingCoverage")
                ),
                
                fluidRow(
                  box(
                    title = "Predicate Distribution", status = "primary", solidHeader = TRUE,
                    width = 6,
                    plotlyOutput("predicateChart")
                  ),
                  box(
                    title = "Confidence Distribution", status = "primary", solidHeader = TRUE,
                    width = 6,
                    plotlyOutput("confidenceChart")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Class Mapping Matrix", status = "info", solidHeader = TRUE,
                    width = 12,
                    DT::dataTableOutput("classMappingMatrix")
                  )
                )
              )
      ),
      
      # Export Tab
      tabItem(tabName = "export",
              fluidRow(
                box(
                  title = "Export Options", status = "primary", solidHeader = TRUE,
                  width = 12,
                  conditionalPanel(
                    condition = "!output.mappingsLoaded",
                    h4("Please load mapping data first to enable export options.")
                  ),
                  conditionalPanel(
                    condition = "output.mappingsLoaded",
                    h4("Export Your Mappings"),
                    br(),
                    downloadButton("exportSSSOMLTSV", "Export as SSSOM TSV", class = "btn-success"),
                    br(), br(),
                    downloadButton("exportExcel", "Export as Excel", class = "btn-info"),
                    br(), br(),
                    downloadButton("exportSummaryReport", "Generate Summary Report", class = "btn-warning"),
                    br(), br(),
                    h4("SSSOM Metadata Configuration"),
                    textInput("exportMappingSetId", "Mapping Set ID:", value = "CLL_to_1MG_mappings_v1.0"),
                    textInput("exportVersion", "Version:", value = "1.0"),
                    textInput("exportLicense", "License:", value = "CC0-1.0"),
                    textInput("exportCreator", "Creator ID:", value = "orcid:0000-0000-0000-0000")
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    cll_variables = NULL,
    mg_variables = NULL,
    mappings = NULL,
    selected_cll_var = NULL,
    selected_mg_var = NULL,
    variables_loaded = FALSE,
    mappings_loaded = FALSE,
    default_author = ""
  )
  # Connect to SQLite database (must come BEFORE any dbExistsTable/dbReadTable)
  db_path <- "sssom_mappings.sqlite"  # stored in the app directory
  con <- dbConnect(SQLite(), dbname = db_path)
  
  # Create tables if they don't exist
  dbExecute(con, "
CREATE TABLE IF NOT EXISTS mappings (
  subject_id TEXT,
  predicate_id TEXT,
  object_id TEXT,
  confidence REAL,
  match_type TEXT,
  subject_class TEXT,
  object_class TEXT,
  mapping_justification TEXT,
  author_id TEXT,
  mapping_date TEXT,
  subject_source TEXT,
  object_source TEXT,
  subject_description TEXT,
  object_description TEXT
)
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS cll_variables (
  variable_id TEXT,
  variable_name TEXT,
  description TEXT,
  data_type TEXT,
  class TEXT
)
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS mg_variables (
  variable_id TEXT,
  variable_name TEXT,
  description TEXT,
  data_type TEXT,
  class TEXT
)
")

# --------- THIS IS THE CORRECT WAY: PUT THIS INSIDE AN OBSERVE! ---------
observe({
  # Load CLL variables
  if (dbExistsTable(con, "cll_variables")) {
    cll_data <- dbReadTable(con, "cll_variables")
    if (nrow(cll_data) > 0) {
      values$cll_variables <- cll_data
    }
  }
  
  # Load MG variables
  if (dbExistsTable(con, "mg_variables")) {
    mg_data <- dbReadTable(con, "mg_variables")
    if (nrow(mg_data) > 0) {
      values$mg_variables <- mg_data
    }
  }
  
  # Load mappings
  if (dbExistsTable(con, "mappings")) {
    mappings_data <- dbReadTable(con, "mappings")
    if (nrow(mappings_data) > 0) {
      values$mappings <- mappings_data
      values$mappings_loaded <- TRUE
    }
  }
  
  # Update UI if data was loaded
  if (!is.null(values$cll_variables) && !is.null(values$mg_variables)) {
    values$variables_loaded <- TRUE
    
    # Update filters
    updateSelectInput(session, "filterCLLClass", 
                      choices = c("All", unique(values$cll_variables$class)))
    updateSelectInput(session, "filterMGClass", 
                      choices = c("All", unique(values$mg_variables$class)))
  }
  
  if (values$mappings_loaded && "subject_class" %in% colnames(values$mappings)) {
    updateSelectInput(session, "selectedClass",
                      choices = unique(values$mappings$subject_class))
  }
})

# Close database connection when app stops
onStop(function() {
  dbDisconnect(con)
})

source_prefix <- reactive({ paste0(input$sourceModelName, ":") })
target_prefix <- reactive({ paste0(input$targetModelName, ":") })
  
  
  output$sourceModelLabel <- renderText({
    paste(input$sourceModelName, "Variables")
  })
  
  output$targetModelLabel <- renderText({
    paste(input$targetModelName, "Variables")
  })
  
  # Add this observer to handle default author
  observeEvent(input$defaultAuthor, {
    if(input$defaultAuthor != "") {
      updateTextInput(session, "mappingAuthor", value = input$defaultAuthor)
    }
  })
  
  # Also update when variables are loaded
  observeEvent(values$variables_loaded, {
    if(values$variables_loaded && input$defaultAuthor != "") {
      updateTextInput(session, "mappingAuthor", value = input$defaultAuthor)
    }
  })
  
  # Load sample data
  observeEvent(input$loadSampleData, {
    sample_vars <- create_sample_variables()
    values$cll_variables <- sample_vars$cll
    values$mg_variables <- sample_vars$mg
    values$mappings <- create_sample_mappings()
    values$variables_loaded <- TRUE
    values$mappings_loaded <- TRUE
    
    updateSelectInput(session, "filterCLLClass", 
                      choices = c("All", unique(values$cll_variables$class)))
    updateSelectInput(session, "filterMGClass", 
                      choices = c("All", unique(values$mg_variables$class)))
    updateSelectInput(session, "selectedClass",
                      choices = unique(values$mappings$subject_class))
    
    showNotification("Sample data loaded successfully!", type = "message")
  })
  
  # Load variables
  observeEvent(input$loadVariables, {
    req(input$cllVariables, input$mgVariables)
    
    tryCatch({
      # Read CLL variables
      if(tools::file_ext(input$cllVariables$datapath) == "xlsx") {
        values$cll_variables <- read_excel(input$cllVariables$datapath)
      } else {
        values$cll_variables <- read.csv(input$cllVariables$datapath, stringsAsFactors = FALSE)
      }
      
      # Read 1+MG variables
      if(tools::file_ext(input$mgVariables$datapath) == "xlsx") {
        values$mg_variables <- read_excel(input$mgVariables$datapath)
      } else {
        values$mg_variables <- read.csv(input$mgVariables$datapath, stringsAsFactors = FALSE)
      }
      
      values$variables_loaded <- TRUE
      # Save uploaded variables to SQLite
      dbWriteTable(con, "cll_variables", values$cll_variables, overwrite = TRUE)
      dbWriteTable(con, "mg_variables", values$mg_variables, overwrite = TRUE)
      
      values$mappings <- data.frame()  # Initialize empty mappings
      
      # Update class filters
      updateSelectInput(session, "filterCLLClass", 
                        choices = c("All", unique(values$cll_variables$class)))
      updateSelectInput(session, "filterMGClass", 
                        choices = c("All", unique(values$mg_variables$class)))
      
      showNotification("Variables loaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error loading variables:", e$message), type = "error")
    })
  })
  
  # Load mappings
  # Load mappings - FIXED version
  observeEvent(input$loadMappings, {
    req(input$mappingFile)
    
    tryCatch({
      # Read mapping file
      if(tools::file_ext(input$mappingFile$datapath) == "xlsx") {
        raw_mappings <- read_excel(input$mappingFile$datapath)
      } else if(tools::file_ext(input$mappingFile$datapath) == "tsv") {
        # Handle SSSOM TSV with metadata header
        lines <- readLines(input$mappingFile$datapath)
        data_start <- which(!grepl("^#", lines))[1]
        raw_mappings <- read.table(input$mappingFile$datapath, sep = "\t", header = TRUE,
                                   skip = data_start - 1, stringsAsFactors = FALSE)
      } else {
        raw_mappings <- read.csv(input$mappingFile$datapath, stringsAsFactors = FALSE)
      }
      
      # Convert to SSSOM format if needed
      if(input$mappingFormat != "sssom") {
        values$mappings <- convert_to_sssom(raw_mappings, input$mappingFormat, source_prefix(), target_prefix())
      } else {
        values$mappings <- raw_mappings
      }
      
      values$mappings_loaded <- TRUE
      
      # Save mappings to database
      if(!is.null(values$mappings) && nrow(values$mappings) > 0) {
        dbWriteTable(con, "mappings", values$mappings, overwrite = TRUE, row.names = FALSE)
      }
      
      # Try to extract variable definitions from mapping descriptions
      if(all(c("subject_description", "object_description") %in% colnames(values$mappings))) {
        extracted_vars <- extract_variables_from_mappings(values$mappings, source_prefix(), target_prefix())
        if(!is.null(extracted_vars)) {
          values$cll_variables <- extracted_vars$cll
          values$mg_variables <- extracted_vars$mg
          values$variables_loaded <- TRUE
          
          # Save extracted variables to database
          if(nrow(extracted_vars$cll) > 0) {
            dbWriteTable(con, "cll_variables", extracted_vars$cll, overwrite = TRUE, row.names = FALSE)
          }
          if(nrow(extracted_vars$mg) > 0) {
            dbWriteTable(con, "mg_variables", extracted_vars$mg, overwrite = TRUE, row.names = FALSE)
          }
          
          # Update class filters for interactive mapping
          updateSelectInput(session, "filterCLLClass", 
                            choices = c("All", unique(values$cll_variables$class)))
          updateSelectInput(session, "filterMGClass", 
                            choices = c("All", unique(values$mg_variables$class)))
        }
      }
      
      # Update class selector
      if("subject_class" %in% colnames(values$mappings)) {
        updateSelectInput(session, "selectedClass",
                          choices = unique(values$mappings$subject_class))
      }
      
      showNotification("Mappings loaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error loading mappings:", e$message), type = "error")
    })
  })
  
  # Variable status output
  output$variableStatus <- renderText({
    if(values$variables_loaded) {
      paste0("✓ Variables loaded successfully\n",
             "CLL Variables: ", nrow(values$cll_variables), "\n",
             "1+MG Variables: ", nrow(values$mg_variables))
    } else {
      "Please upload variable files"
    }
  })
  
  # Mapping status output
  output$mappingStatus <- renderText({
    if(values$mappings_loaded) {
      validation <- validate_sssom_mappings(values$mappings)
      has_descriptions <- all(c("subject_description", "object_description") %in% colnames(values$mappings))
      desc_text <- if(has_descriptions) "\n✓ Variable descriptions included" else "\n⚠ No variable descriptions"
      
      paste0("✓ Mappings loaded successfully\n",
             "Total mappings: ", nrow(values$mappings), "\n",
             "Validation: ", validation$message,
             desc_text)
    } else {
      "Please upload mapping file"
    }
  })
  
  # Output reactive expressions
  output$variablesLoaded <- reactive({ values$variables_loaded })
  output$mappingsLoaded <- reactive({ values$mappings_loaded })
  outputOptions(output, "variablesLoaded", suspendWhenHidden = FALSE)
  outputOptions(output, "mappingsLoaded", suspendWhenHidden = FALSE)
  
  # Variable preview
  output$variablePreview <- DT::renderDataTable({
    req(values$variables_loaded)
    
    combined_vars <- rbind(
      values$cll_variables %>% mutate(source = "CLL"),
      values$mg_variables %>% mutate(source = "1+MG")
    )
    
    DT::datatable(combined_vars, options = list(pageLength = 5))
  })
  
  # Mapping preview
  output$mappingPreview <- DT::renderDataTable({
    req(values$mappings_loaded)
    
    # Show key columns for preview
    preview_cols <- c("subject_id", "predicate_id", "object_id", "confidence", "subject_class", "object_class")
    if("subject_description" %in% colnames(values$mappings)) {
      preview_cols <- c(preview_cols, "subject_description")
    }
    
    display_data <- values$mappings[preview_cols] %>% head(10)
    
    DT::datatable(display_data, 
                  options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # CLL Variables List
  output$cllVariablesList <- DT::renderDataTable({
    req(values$variables_loaded)
    
    df <- values$cll_variables
    if(input$filterCLLClass != "All") {
      df <- df %>% filter(class == input$filterCLLClass)
    }
    
    # Truncate descriptions for table display
    display_df <- df %>% 
      mutate(
        short_description = ifelse(nchar(description) > 50, 
                                   paste0(substr(description, 1, 47), "..."), 
                                   description)
      ) %>%
      select(variable_name, short_description, class)
    
    DT::datatable(display_df,
                  selection = 'single',
                  options = list(pageLength = 8, dom = 't', scrollX = TRUE),
                  rownames = FALSE,
                  colnames = c("Variable", "Description", "Class")) %>%
      DT::formatStyle('short_description', 
                      textAlign = 'left',
                      fontSize = '12px')
  })
  
  # 1+MG Variables List
  output$mgVariablesList <- DT::renderDataTable({
    req(values$variables_loaded)
    
    df <- values$mg_variables
    if(input$filterMGClass != "All") {
      df <- df %>% filter(class == input$filterMGClass)
    }
    
    # Truncate descriptions for table display
    display_df <- df %>% 
      mutate(
        short_description = ifelse(nchar(description) > 50, 
                                   paste0(substr(description, 1, 47), "..."), 
                                   description)
      ) %>%
      select(variable_name, short_description, class)
    
    DT::datatable(display_df,
                  selection = 'single',
                  options = list(pageLength = 8, dom = 't', scrollX = TRUE),
                  rownames = FALSE,
                  colnames = c("Variable", "Description", "Class")) %>%
      DT::formatStyle('short_description', 
                      textAlign = 'left',
                      fontSize = '12px')
  })
  
  # Handle variable selection
  observeEvent(input$cllVariablesList_rows_selected, {
    req(input$cllVariablesList_rows_selected)
    
    df <- values$cll_variables
    if(input$filterCLLClass != "All") {
      df <- df %>% filter(class == input$filterCLLClass)
    }
    
    row_idx <- input$cllVariablesList_rows_selected[1]
    values$selected_cll_var <- df[row_idx, ]
  })
  
  observeEvent(input$mgVariablesList_rows_selected, {
    req(input$mgVariablesList_rows_selected)
    
    df <- values$mg_variables
    if(input$filterMGClass != "All") {
      df <- df %>% filter(class == input$filterMGClass)
    }
    
    row_idx <- input$mgVariablesList_rows_selected[1]
    values$selected_mg_var <- df[row_idx, ]
  })
  
  # Selected variable outputs
  output$selectedCLLVar <- renderText({
    if(!is.null(values$selected_cll_var)) {
      paste0("Variable: ", values$selected_cll_var$variable_name, "\n",
             "Class: ", values$selected_cll_var$class, "\n",
             "Type: ", ifelse(is.null(values$selected_cll_var$data_type), "Not specified", values$selected_cll_var$data_type), "\n\n",
             "Description:\n", values$selected_cll_var$description)
    } else {
      "No variable selected"
    }
  })
  
  output$selectedMGVar <- renderText({
    if(!is.null(values$selected_mg_var)) {
      paste0("Variable: ", values$selected_mg_var$variable_name, "\n",
             "Class: ", values$selected_mg_var$class, "\n",
             "Type: ", ifelse(is.null(values$selected_mg_var$data_type), "Not specified", values$selected_mg_var$data_type), "\n\n",
             "Description:\n", values$selected_mg_var$description)
    } else {
      "No variable selected"
    }
  })
  
  # Auto-suggest justification based on descriptions
  observeEvent(input$suggestJustification, {
    req(values$selected_cll_var)
    
    suggestion <- ""
    
    if(!is.null(values$selected_mg_var)) {
      # Generate suggestion based on selected variables and relationship
      relationship_text <- case_when(
        input$mappingPredicate == "skos:exactMatch" ~ "Direct equivalence - both variables capture the same concept with identical data formats",
        input$mappingPredicate == "skos:closeMatch" ~ "Close match - similar concepts but may require data transformation",
        input$mappingPredicate == "skos:broadMatch" ~ "Broad match - source variable is more specific than target variable",
        input$mappingPredicate == "skos:narrowMatch" ~ "Narrow match - source variable is more general than target variable",
        input$mappingPredicate == "skos:relatedMatch" ~ "Related concepts but not directly equivalent",
        input$mappingPredicate == "sssom:NoMatch" ~ "No equivalent variable found in target system",
        TRUE ~ "Mapping relationship defined"
      )
      
      # Add variable-specific context
      if(input$mappingPredicate != "sssom:NoMatch") {
        suggestion <- paste0(relationship_text, ". ",
                             "CLL variable '", values$selected_cll_var$variable_name, "' ",
                             "maps to 1+MG variable '", values$selected_mg_var$variable_name, "'.")
      } else {
        suggestion <- paste0(relationship_text, ". ",
                             "CLL variable '", values$selected_cll_var$variable_name, "' ",
                             "has no direct equivalent in the 1+MG standard.")
      }
    } else {
      if(input$mappingPredicate == "sssom:NoMatch") {
        suggestion <- paste0("No equivalent variable found in 1+MG standard for CLL variable '", 
                             values$selected_cll_var$variable_name, "'.")
      } else {
        suggestion <- "Please select a 1+MG variable to generate a complete justification."
      }
    }
    
    updateTextAreaInput(session, "mappingJustification", value = suggestion)
  })
  
  # Create mapping
  # Create mapping - FIXED version
  observeEvent(input$createMapping, {
    req(values$selected_cll_var, input$mappingPredicate, input$mappingConfidence)
    
    # Get author value, use default if empty
    author_value <- if(input$mappingAuthor != "") {
      input$mappingAuthor
    } else {
      "dashboard_user"
    }
    
    # Format ORCID if it's just numbers
    if(grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{4}$", author_value)) {
      author_value <- paste0("orcid:", author_value)
    }
    
    new_mapping <- data.frame(
      subject_id = paste0(source_prefix(), values$selected_cll_var$variable_name),
      predicate_id = input$mappingPredicate,
      object_id = if(input$mappingPredicate == "sssom:NoMatch" || is.null(values$selected_mg_var)) {
        NA
      } else {
        paste0(target_prefix(), values$selected_mg_var$variable_name)
      },
      confidence = input$mappingConfidence,
      match_type = "HumanCurated",
      subject_class = values$selected_cll_var$class,
      object_class = if(is.null(values$selected_mg_var)) NA else values$selected_mg_var$class,
      mapping_justification = if(input$mappingJustification == "") "User created mapping" else input$mappingJustification,
      author_id = author_value,
      mapping_date = as.character(Sys.Date()),
      subject_source = input$sourceModelName,
      object_source = input$targetModelName,
      subject_description = values$selected_cll_var$description,
      object_description = if(is.null(values$selected_mg_var)) NA else values$selected_mg_var$description,
      stringsAsFactors = FALSE
    )
    
    if(is.null(values$mappings) || nrow(values$mappings) == 0) {
      values$mappings <- new_mapping
      # Write the first mapping to database (overwrite)
      dbWriteTable(con, "mappings", new_mapping, overwrite = TRUE, row.names = FALSE)
    } else {
      values$mappings <- rbind(values$mappings, new_mapping)
      # Append subsequent mappings to database
      dbWriteTable(con, "mappings", new_mapping, append = TRUE, row.names = FALSE)
    }
    
    values$mappings_loaded <- TRUE
    
    # Store the author for reuse if it's not empty
    if(input$mappingAuthor != "") {
      values$default_author <- input$mappingAuthor
    }
    
    # Clear selections
    values$selected_cll_var <- NULL
    values$selected_mg_var <- NULL
    updateTextAreaInput(session, "mappingJustification", value = "")
    
    # Update class selector
    updateSelectInput(session, "selectedClass",
                      choices = unique(values$mappings$subject_class))
    
    showNotification("Mapping created successfully!", type = "message")
  })
  
  # Clear mappings
  observeEvent(input$clearMappings, {
    values$mappings <- data.frame()
    values$mappings_loaded <- FALSE
    # Optional: clear DB as well
    dbExecute(con, "DELETE FROM mappings")
    dbExecute(con, "DELETE FROM cll_variables")
    dbExecute(con, "DELETE FROM mg_variables")
  
    showNotification("All mappings cleared", type = "warning")
  })
  
  # Created mappings table
  output$createdMappings <- DT::renderDataTable({
    req(values$mappings_loaded)
    
    display_data <- values$mappings %>%
      mutate(
        CLL_Variable = str_replace(subject_id, source_prefix(), ""),
        MG_Variable = ifelse(is.na(object_id), "-", str_replace(object_id, target_prefix(), "")),
        Relationship = str_replace(predicate_id, "skos:|sssom:", ""),
        Confidence = round(confidence, 2),
        Author = author_id  # Add author to display
      ) %>%
      select(CLL_Variable, Relationship, MG_Variable, Confidence, Author, subject_class, mapping_justification)
    
    DT::datatable(display_data,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle('Confidence',
                      backgroundColor = styleInterval(c(0.6, 0.8), c('#ffebee', '#fff3e0', '#e8f5e8')))
  })
  
  # Class statistics UI
  output$classStatsUI <- renderUI({
    req(values$mappings_loaded, input$selectedClass)
    
    class_mappings <- values$mappings %>%
      filter(subject_class == input$selectedClass)
    
    total_vars <- nrow(class_mappings)
    mapped_vars <- sum(!is.na(class_mappings$object_id))
    avg_conf <- round(mean(class_mappings$confidence, na.rm = TRUE), 2)
    
    tagList(
      h5(paste("Class:", input$selectedClass)),
      p(paste("Total variables:", total_vars)),
      p(paste("Mapped:", mapped_vars, "(", round(mapped_vars/total_vars*100), "%)")),
      p(paste("Avg confidence:", avg_conf))
    )
  })
  
  # Class mappings table
  output$classMappingsTable <- DT::renderDataTable({
    req(values$mappings_loaded, input$selectedClass)
    
    class_mappings <- values$mappings %>%
      filter(subject_class == input$selectedClass) %>%
      mutate(
        CLL_Variable = str_replace(subject_id, source_prefix(), ""),
        Relationship = case_when(
          predicate_id == "skos:exactMatch" ~ "Exact Match",
          predicate_id == "skos:closeMatch" ~ "Close Match",
          predicate_id == "skos:broadMatch" ~ "Broad Match",
          predicate_id == "skos:narrowMatch" ~ "Narrow Match",
          predicate_id == "skos:relatedMatch" ~ "Related Match",
          predicate_id == "sssom:NoMatch" ~ "No Match",
          TRUE ~ str_replace(predicate_id, "skos:|sssom:", "")
        ),
        MG_Variable = ifelse(is.na(object_id), "-", str_replace(object_id, target_prefix(), "")),
        Confidence = ifelse(is.na(confidence), "-", as.character(round(confidence, 2)))
      ) %>%
      select(CLL_Variable, Relationship, MG_Variable, Confidence, mapping_justification)
    
    DT::datatable(class_mappings,
                  selection = 'single',
                  options = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle('Relationship',
                      backgroundColor = styleEqual(
                        c('Exact Match', 'Close Match', 'Broad Match', 'Narrow Match', 'Related Match', 'No Match'),
                        c('#4CAF50', '#FFC107', '#FF9800', '#2196F3', '#9C27B0', '#F44336')
                      ),
                      color = styleEqual(
                        c('Exact Match', 'Close Match', 'Broad Match', 'Narrow Match', 'Related Match', 'No Match'),
                        c('white', 'black', 'white', 'white', 'white', 'white')
                      ))
  })
  
  # Variable details UI - UPDATED TO USE DESCRIPTIONS
  output$variableDetailsUI <- renderUI({
    req(input$classMappingsTable_rows_selected)
    
    class_mappings <- values$mappings %>%
      filter(subject_class == input$selectedClass)
    
    if(length(input$classMappingsTable_rows_selected) > 0) {
      row_idx <- input$classMappingsTable_rows_selected[1]
      selected_mapping <- class_mappings[row_idx, ]
      
      # Get descriptions from mapping data if available
      subject_desc <- if("subject_description" %in% colnames(selected_mapping) && 
                         !is.na(selected_mapping$subject_description) && 
                         selected_mapping$subject_description != "") {
        selected_mapping$subject_description
      } else {
        "No description available"
      }
      
      object_desc <- if("object_description" %in% colnames(selected_mapping) && 
                        !is.na(selected_mapping$object_description) && 
                        selected_mapping$object_description != "") {
        selected_mapping$object_description
      } else if(is.na(selected_mapping$object_id)) {
        "No target variable (no match)"
      } else {
        "No description available"
      }
      
      tagList(
        fluidRow(
          column(6,
                 h4("CLL Variable Details"),
                 div(class = "description-box",
                     p(strong("Variable ID:"), str_replace(selected_mapping$subject_id, source_prefix(), "")),
                     p(strong("Class:"), selected_mapping$subject_class),
                     p(strong("Description:"), subject_desc)
                 )
          ),
          column(6,
                 h4("1+MG Variable Details"),
                 div(class = "description-box", style = "border-left-color: #28a745;",
                     p(strong("Variable ID:"), ifelse(is.na(selected_mapping$object_id), "No match", str_replace(selected_mapping$object_id, target_prefix(), ""))),
                     p(strong("Class:"), ifelse(is.na(selected_mapping$object_class), "-", selected_mapping$object_class)),
                     p(strong("Description:"), object_desc)
                 )
          )
        ),
        hr(),
        fluidRow(
          column(12,
                 h4("Mapping Details"),
                 div(style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; border-left: 4px solid #ffc107;",
                     fluidRow(
                       column(6,
                              p(strong("Relationship:"), selected_mapping$predicate_id),
                              p(strong("Confidence:"), selected_mapping$confidence),
                              p(strong("Match Type:"), selected_mapping$match_type)
                       ),
                       column(6,
                              p(strong("Author:"), ifelse("author_id" %in% colnames(selected_mapping), selected_mapping$author_id, "Not specified")),
                              p(strong("Date:"), ifelse("mapping_date" %in% colnames(selected_mapping), selected_mapping$mapping_date, "Not specified")),
                              p(strong("Sources:"), paste(
                                ifelse("subject_source" %in% colnames(selected_mapping), selected_mapping$subject_source, "CLL"),
                                "→",
                                ifelse("object_source" %in% colnames(selected_mapping), selected_mapping$object_source, "1+MG")
                              ))
                       )
                     ),
                     hr(style = "margin: 10px 0;"),
                     p(strong("Justification:"), selected_mapping$mapping_justification)
                 )
          )
        )
      )
    } else {
      div(style = "text-align: center; padding: 20px; color: #6c757d;",
          icon("info-circle", style = "font-size: 24px; margin-bottom: 10px;"),
          p("Click on a mapping row above to see detailed variable information and mapping metadata.")
      )
    }
  })
  
  # Analysis Tab - Value boxes
  output$totalMappings <- renderValueBox({
    req(values$mappings_loaded)
    valueBox(
      value = nrow(values$mappings),
      subtitle = "Total Mappings",
      icon = icon("exchange-alt"),
      color = "blue"
    )
  })
  
  output$avgConfidence <- renderValueBox({
    req(values$mappings_loaded)
    avg_conf <- round(mean(values$mappings$confidence, na.rm = TRUE), 3)
    valueBox(
      value = avg_conf,
      subtitle = "Average Confidence",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$mappingCoverage <- renderValueBox({
    req(values$mappings_loaded)
    mapped_count <- sum(!is.na(values$mappings$object_id))
    total_count <- nrow(values$mappings)
    coverage <- round(mapped_count / total_count * 100)
    
    valueBox(
      value = paste0(coverage, "%"),
      subtitle = "Mapping Coverage",
      icon = icon("chart-pie"),
      color = if(coverage >= 80) "green" else if(coverage >= 60) "yellow" else "red"
    )
  })
  
  # Predicate chart
  output$predicateChart <- renderPlotly({
    req(values$mappings_loaded)
    
    predicate_counts <- values$mappings %>%
      count(predicate_id) %>%
      mutate(
        predicate_label = case_when(
          predicate_id == "skos:exactMatch" ~ "Exact Match",
          predicate_id == "skos:closeMatch" ~ "Close Match",
          predicate_id == "skos:broadMatch" ~ "Broad Match",
          predicate_id == "skos:narrowMatch" ~ "Narrow Match",
          predicate_id == "skos:relatedMatch" ~ "Related Match",
          predicate_id == "sssom:NoMatch" ~ "No Match",
          TRUE ~ predicate_id
        )
      )
    
    colors <- c('#4CAF50', '#FFC107', '#FF9800', '#2196F3', '#9C27B0', '#F44336')
    
    plot_ly(predicate_counts, x = ~predicate_label, y = ~n, type = "bar",
            marker = list(color = colors[1:nrow(predicate_counts)]),
            text = ~n, textposition = "outside") %>%
      layout(title = "Distribution of Mapping Types",
             xaxis = list(title = "Mapping Type"),
             yaxis = list(title = "Count"))
  })
  
  # Confidence chart
  output$confidenceChart <- renderPlotly({
    req(values$mappings_loaded)
    
    plot_ly(values$mappings, x = ~confidence, type = "histogram",
            nbinsx = 20, marker = list(color = '#2196F3', opacity = 0.7)) %>%
      layout(title = "Confidence Score Distribution",
             xaxis = list(title = "Confidence Score"),
             yaxis = list(title = "Frequency"))
  })
  
  # Class mapping matrix
  output$classMappingMatrix <- DT::renderDataTable({
    req(values$mappings_loaded)
    
    matrix_data <- values$mappings %>%
      filter(!is.na(object_class)) %>%
      count(subject_class, object_class) %>%
      tidyr::pivot_wider(names_from = object_class, values_from = n, values_fill = 0)
    
    DT::datatable(matrix_data,
                  options = list(pageLength = 15),
                  rownames = FALSE) %>%
      DT::formatStyle(columns = 2:ncol(matrix_data),
                      backgroundColor = styleInterval(c(1, 5, 10), c('#ffffff', '#e3f2fd', '#bbdefb', '#2196f3')))
  })
  
  # Download handlers
  output$downloadVarTemplate <- downloadHandler(
    filename = function() {
      "variable_template.xlsx"
    },
    content = function(file) {
      sample_vars <- create_sample_variables()
      
      wb <- createWorkbook()
      addWorksheet(wb, "CLL_Variables")
      writeData(wb, "CLL_Variables", sample_vars$cll)
      
      addWorksheet(wb, "1MG_Variables")
      writeData(wb, "1MG_Variables", sample_vars$mg)
      
      addWorksheet(wb, "Instructions")
      instructions <- data.frame(
        Field = c("variable_id", "variable_name", "description", "data_type", "class"),
        Description = c("Unique identifier", "Variable name", "Detailed description", 
                        "Data type (date, integer, categorical, etc.)", "Class/category of variable"),
        Required = c("Optional", "Required", "Required", "Required", "Required")
      )
      writeData(wb, "Instructions", instructions)
      
      saveWorkbook(wb, file)
    }
  )
  
  output$downloadMappingTemplate <- downloadHandler(
    filename = function() {
      "mapping_template.xlsx"
    },
    content = function(file) {
      sample_mappings <- create_sample_mappings()
      
      wb <- createWorkbook()
      addWorksheet(wb, "Mappings")
      writeData(wb, "Mappings", sample_mappings)
      
      addWorksheet(wb, "Instructions")
      instructions <- data.frame(
        Field = c("subject_id", "predicate_id", "object_id", "confidence", "match_type", 
                  "mapping_justification", "subject_description", "object_description"),
        Description = c("Source variable ID", "Relationship type", "Target variable ID", 
                        "Confidence score (0-1)", "How mapping was created", "Explanation of mapping",
                        "Description of source variable", "Description of target variable"),
        Required = c("Required", "Required", "Required", "Optional", "Optional", "Optional", "Recommended", "Recommended")
      )
      writeData(wb, "Instructions", instructions)
      
      saveWorkbook(wb, file)
    }
  )
  
  output$exportSSSOMLTSV <- downloadHandler(
    filename = function() {
      paste0("sssom_mappings_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      req(values$mappings_loaded)
      
      # Create SSSOM-compliant TSV with metadata header
      header_lines <- c(
        "# SSSOM Mapping Set Metadata",
        paste0("# mapping_set_id: ", input$exportMappingSetId),
        paste0("# mapping_set_version: ", input$exportVersion),
        paste0("# mapping_date: ", Sys.Date()),
        paste0("# license: ", input$exportLicense),
        paste0("# creator_id: ", input$exportCreator),
        paste0("# subject_source: ", input$sourceModelName),
        paste0("# object_source: ", input$targetModelName),
        ""
      )
      
      writeLines(header_lines, file)
      write.table(values$mappings, file, sep = "\t", row.names = FALSE,
                  col.names = TRUE, append = TRUE, quote = FALSE)
    }
  )
  
  output$exportExcel <- downloadHandler(
    filename = function() {
      paste0("mappings_export_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(values$mappings_loaded)
      
      wb <- createWorkbook()
      
      # Main mappings sheet
      addWorksheet(wb, "Mappings")
      writeData(wb, "Mappings", values$mappings)
      
      # Summary sheet
      summary_data <- values$mappings %>%
        group_by(subject_class) %>%
        summarise(
          total_variables = n(),
          mapped_variables = sum(!is.na(object_id)),
          avg_confidence = round(mean(confidence, na.rm = TRUE), 3),
          .groups = 'drop'
        ) %>%
        mutate(coverage_percent = round(mapped_variables / total_variables * 100, 1))
      
      addWorksheet(wb, "Summary")
      writeData(wb, "Summary", summary_data)
      
      saveWorkbook(wb, file)
    }
  )
  
  output$exportSummaryReport <- downloadHandler(
    filename = function() {
      paste0("mapping_summary_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(values$mappings_loaded)
      
      # Create a simple HTML report
      total_mappings <- nrow(values$mappings)
      mapped_count <- sum(!is.na(values$mappings$object_id))
      avg_confidence <- round(mean(values$mappings$confidence, na.rm = TRUE), 3)
      
      predicate_summary <- values$mappings %>%
        count(predicate_id) %>%
        arrange(desc(n))
      
      html_content <- paste0(
        "<html><head><title>SSSOM Mapping Summary Report</title></head><body>",
        "<h1>SSSOM Mapping Summary Report</h1>",
        "<p>Generated on: ", Sys.Date(), "</p>",
        "<h2>Overview Statistics</h2>",
        "<ul>",
        "<li>Total mappings: ", total_mappings, "</li>",
        "<li>Successfully mapped: ", mapped_count, " (", round(mapped_count/total_mappings*100, 1), "%)</li>",
        "<li>Average confidence: ", avg_confidence, "</li>",
        "</ul>",
        "<h2>Predicate Distribution</h2>",
        "<ul>",
        paste(sapply(1:nrow(predicate_summary), function(i) {
          paste0("<li>", predicate_summary$predicate_id[i], ": ", predicate_summary$n[i], "</li>")
        }), collapse = ""),
        "</ul>",
        "</body></html>"
      )
      
      writeLines(html_content, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)