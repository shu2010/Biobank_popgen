library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(DT)

# ---- load catalog ----
catalog <- read_csv("~/Documents/Claude_work/Biobank/biobank_tools_catalog.csv", col_types = NULL)

# helper: does a row's Study_IDs contain any of the query IDs?
has_any_study_id <- function(study_ids_cell, query_ids) {
  if (is.na(study_ids_cell) || study_ids_cell == "") return(FALSE)
  ids <- str_split(study_ids_cell, "\\s*;\\s*")[[1]]
  any(ids %in% query_ids)
}

ui <- fluidPage(
  titlePanel("Biobank-scale population genomics tools catalog"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "study_ids",
        label   = "Biobank study IDs (comma or semicolon separated)",
        placeholder = "e.g. UKB_GWAS_2017; AoU_v1; FinnGen_R6"
      ),
      actionButton("run", "Query catalog"),
      hr(),
      downloadButton("download_filtered", "Download filtered tools (CSV)")
    ),
    mainPanel(
      h4("Tools used in selected studies"),
      DTOutput("tools_table")
    )
  )
)

server <- function(input, output, session) {
  
  # reactive filtered data
  filtered <- eventReactive(input$run, {
    ids_raw <- input$study_ids
    if (is.null(ids_raw) || ids_raw == "") {
      return(catalog[0, ])  # empty
    }
    
    query_ids <- ids_raw %>%
      str_split("[,;]") %>%
      unlist() %>%
      str_trim() %>%
      discard(~ .x == "")
    
    if (length(query_ids) == 0) {
      return(catalog[0, ])
    }
    
    # rowwise filtering
    catalog %>%
      rowwise() %>%
      mutate(.keep_row = has_any_study_id(Study_IDs, query_ids)) %>%
      ungroup() %>%
      filter(.keep_row) %>%
      select(Tool_name, Tool_class, Subclass, Biobank_usage, Study_IDs) %>%
      arrange(Tool_class, Tool_name)
  }, ignoreNULL = FALSE)
  
  output$tools_table <- renderDT({
    datatable(
      filtered(),
      options = list(pageLength = 25, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("biobank_tools_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
