library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(DT)

catalog <- read_csv("~/Documents/Claude_work/Biobank/biobank_tools_catalog.csv",
                    col_types = cols(.default = col_character()))
# helper for exact match
has_any_study_id_exact <- function(study_ids_cell, query_ids) {
  if (is.na(study_ids_cell) || study_ids_cell == "") return(FALSE)
  ids <- unlist(str_split(study_ids_cell, "\\s*;\\s*"))
  ids <- ids[ids != ""]
  any(ids %in% query_ids)
}

# helper for partial match (case-insensitive)
has_any_study_id_partial <- function(study_ids_cell, query_ids) {
  if (is.na(study_ids_cell) || study_ids_cell == "") return(FALSE)
  ids <- unlist(str_split(study_ids_cell, "\\s*;\\s*"))
  ids <- ids[ids != ""]
  ids_lower <- tolower(ids)
  q_lower   <- tolower(query_ids)
  any(vapply(q_lower,
             function(q) any(grepl(q, ids_lower, fixed = TRUE)),
             logical(1)))
}

# helper for biobank usage (exact or partial)
has_any_biobank_exact <- function(biobank_cell, query_ids) {
  if (is.na(biobank_cell) || biobank_cell == "") return(FALSE)
  ids <- unlist(str_split(biobank_cell, "\\s*;\\s*"))
  ids <- ids[ids != ""]
  any(ids %in% query_ids)
}

has_any_biobank_partial <- function(biobank_cell, query_ids) {
  if (is.na(biobank_cell) || biobank_cell == "") return(FALSE)
  ids <- unlist(str_split(biobank_cell, "\\s*;\\s*"))
  ids <- ids[ids != ""]
  ids_lower <- tolower(ids)
  q_lower   <- tolower(query_ids)
  any(vapply(q_lower,
             function(q) any(grepl(q, ids_lower, fixed = TRUE)),
             logical(1)))
}

ui <- fluidPage(
  titlePanel("Biobank-scale population genomics tools catalog"),
  sidebarLayout(
    sidebarPanel(
      h4("Search Options"),
      
      radioButtons(
        inputId = "search_type",
        label   = "Search by:",
        choices = c("Study IDs" = "study",
                    "Biobank Usage" = "biobank",
                    "Both" = "both"),
        selected = "study"
      ),
      
      conditionalPanel(
        condition = "input.search_type != 'biobank'",
        textInput(
          inputId   = "study_ids",
          label     = "Study IDs (comma/semicolon separated)",
          placeholder = "e.g. UKB_GWAS_2017, AoU_v1"
        )
      ),
      
      conditionalPanel(
        condition = "input.search_type != 'study'",
        textInput(
          inputId   = "biobank_ids", 
          label     = "Biobank names (comma/semicolon separated)",
          placeholder = "e.g. UKB, All_of_Us, FinnGen"
        )
      ),
      
      radioButtons(
        inputId = "match_mode",
        label   = "Match mode",
        choices = c("Exact" = "exact",
                    "Partial (contains text)" = "partial"),
        selected = "exact"
      ),
      
      actionButton("run", "Query catalog", class = "btn-primary"),
      
      tags$hr(),
      
      h4("Display Columns"),
      checkboxGroupInput(
        inputId = "display_cols",
        label = NULL,
        choices = list(
          "Tool Name" = "Tool_name",
          "Tool Class" = "Tool_class", 
          "Subclass" = "Subclass",
          "Biobank Usage" = "Biobank_usage",
          "Study IDs" = "Study_IDs",
          "Scale Reported" = "Scale_reported",
          "Compute Env" = "Compute_environment",
          "Primary Function" = "Primary_function",
          "Evidence Sources" = "Evidence_sources",
          "Last Version" = "Last_seen_version"
        ),
        selected = c("Tool_name", "Tool_class", "Subclass", 
                     "Biobank_usage", "Study_IDs")
      ),
      
      tags$hr(),
      downloadButton("download_filtered", "Download filtered tools (CSV)")
    ),
    mainPanel(
      h4("Tools used in selected studies/biobanks"),
      DTOutput("tools_table"),
      tags$hr(),
      h5("Showing columns:"), 
      verbatimTextOutput("selected_cols_info", placeholder = TRUE),
      verbatimTextOutput("search_summary", placeholder = TRUE)
    )
  )
)

server <- function(input, output, session) {
  
  filtered_base <- eventReactive(input$run, {
    # Check if both inputs are empty
    study_empty <- is.null(input$study_ids) || input$study_ids == ""
    biobank_empty <- is.null(input$biobank_ids) || input$biobank_ids == ""
    
    if (study_empty && biobank_empty) {
      return(catalog[0, ])
    }
    
    keep_vec <- rep(TRUE, nrow(catalog))
    
    # Filter by Study IDs if specified
    if (!study_empty && (input$search_type %in% c("study", "both"))) {
      query_ids <- unlist(str_split(input$study_ids, "[,;]"))
      query_ids <- str_trim(query_ids)
      query_ids <- query_ids[query_ids != ""]
      
      if (identical(input$match_mode, "partial")) {
        study_keep <- vapply(
          catalog$Study_IDs,
          has_any_study_id_partial,
          logical(1),
          query_ids = query_ids
        )
      } else {
        study_keep <- vapply(
          catalog$Study_IDs,
          has_any_study_id_exact,
          logical(1),
          query_ids = query_ids
        )
      }
      keep_vec <- keep_vec & study_keep
    }
    
    # Filter by Biobank usage if specified  
    if (!biobank_empty && (input$search_type %in% c("biobank", "both"))) {
      query_ids <- unlist(str_split(input$biobank_ids, "[,;]"))
      query_ids <- str_trim(query_ids)
      query_ids <- query_ids[query_ids != ""]
      
      if (identical(input$match_mode, "partial")) {
        biobank_keep <- vapply(
          catalog$Biobank_usage,
          has_any_biobank_partial,
          logical(1),
          query_ids = query_ids
        )
      } else {
        biobank_keep <- vapply(
          catalog$Biobank_usage,
          has_any_biobank_exact,
          logical(1),
          query_ids = query_ids
        )
      }
      keep_vec <- keep_vec & biobank_keep
    }
    
    out <- catalog[keep_vec, , drop = FALSE]
    out <- out[order(out$Tool_class, out$Tool_name), , drop = FALSE]
    out
  }, ignoreNULL = FALSE)
  
  filtered <- reactive({
    if (nrow(filtered_base()) == 0) return(filtered_base())
    
    if (is.null(input$display_cols) || length(input$display_cols) == 0) {
      return(filtered_base()[0, ])
    }
    
    filtered_base()[, input$display_cols, drop = FALSE]
  })
  
  output$tools_table <- renderDT({
    datatable(
      filtered(),
      options = list(
        pageLength = 25, 
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("biobank_tools_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_base(), file)
    }
  )
  
  output$selected_cols_info <- renderText({
    if (is.null(input$display_cols)) {
      "No columns selected"
    } else {
      paste("Selected:", paste(input$display_cols, collapse = ", "))
    }
  })
  
  output$search_summary <- renderText({
    search_type <- switch(input$search_type,
                          "study" = "Study IDs",
                          "biobank" = "Biobank Usage", 
                          "both" = "Study IDs + Biobank Usage")
    paste("Search type:", search_type, "| Match mode:", input$match_mode)
  })
}

shinyApp(ui = ui, server = server)
