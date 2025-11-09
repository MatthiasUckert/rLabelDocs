# EXPORT MODULE
# Handles data export functionality and timeline navigation

#' Export Module UI
#'
#' Generates the user interface for the export/timeline tab.
#'
#' @param id Module namespace ID
#' @return Shiny UI
#' @export
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    load_app_css(),

    # ===== TIMELINE SLIDER SECTION =====
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          style = "background-color: #e8f4f8; border: 2px solid #17a2b8;",
          shiny::h4("Timeline View", style = "margin-top: 0; color: #2c3e50;"),
          shiny::fluidRow(
            shiny::column(
              8,
              shiny::p(
                "Use the slider to view the state of classifications at different points in time. Exports and the table below will reflect the selected timestamp.",
                style = "color: #666; margin-bottom: 15px;"
              ),
              shiny::div(
                style = "padding: 15px; background-color: white; border-radius: 5px;",
                shiny::uiOutput(ns("timeline_slider_ui")),
                shiny::div(
                  style = "margin-top: 10px; display: flex; gap: 10px;",
                  shiny::actionButton(
                    ns("reset_timeline"),
                    "Reset to Current",
                    icon = shiny::icon("undo"),
                    class = "btn-secondary"
                  )
                )
              )
            ),
            shiny::column(
              4,
              shiny::div(
                style = "background-color: white; padding: 15px; border-radius: 5px; border-left: 3px solid #17a2b8;",
                shiny::h5("Selected View", style = "margin-top: 0; color: #17a2b8;"),
                shiny::div(
                  style = "font-size: 14px;",
                  shiny::strong("Date: "), shiny::textOutput(ns("selected_date"), inline = TRUE)
                ),
                shiny::div(
                  style = "font-size: 14px; margin-top: 5px;",
                  shiny::strong("Time: "), shiny::textOutput(ns("selected_time"), inline = TRUE)
                ),
                shiny::div(
                  style = "font-size: 12px; margin-top: 10px; color: #666; font-style: italic;",
                  shiny::textOutput(ns("timeline_status"))
                )
              )
            )
          )
        )
      )
    ),

    # ===== EXPORT DATA SECTION =====
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          style = "background-color: #f8f9fa; border: 2px solid #3498db;",
          shiny::h4("Export Data", style = "margin-top: 0; color: #2c3e50;"),
          shiny::fluidRow(
            shiny::column(
              8,
              shiny::p(
                "Export your classification data and notes to CSV or Parquet format.",
                style = "color: #666; margin-bottom: 15px;"
              ),

              # Export buttons
              shiny::div(
                style = "display: flex; gap: 10px; flex-wrap: wrap;",
                shiny::actionButton(
                  ns("export_current_btn"),
                  "Export Current State",
                  icon = shiny::icon("download"),
                  class = "btn-primary",
                  style = "flex: 1; min-width: 200px;"
                ),
                shiny::actionButton(
                  ns("export_history_btn"),
                  "Export Full History",
                  icon = shiny::icon("history"),
                  class = "btn-info",
                  style = "flex: 1; min-width: 200px;"
                ),
                shiny::actionButton(
                  ns("view_exports_btn"),
                  "View Exports",
                  icon = shiny::icon("folder-open"),
                  class = "btn-secondary",
                  style = "flex: 1; min-width: 200px;"
                )
              )
            ),
            shiny::column(
              4,
              shiny::div(
                style = "background-color: white; padding: 15px; border-radius: 5px; border-left: 3px solid #3498db;",
                shiny::h5("Export Info", style = "margin-top: 0; color: #3498db;"),
                shiny::p(
                  style = "font-size: 12px; margin-bottom: 5px;",
                  shiny::strong("Current State:"), " Latest classifications and notes"
                ),
                shiny::p(
                  style = "font-size: 12px; margin-bottom: 5px;",
                  shiny::strong("Full History:"), " All versions with timestamps"
                ),
                shiny::p(
                  style = "font-size: 12px; margin-bottom: 0; color: #17a2b8;",
                  shiny::strong("Timeline:"), " Exports respect timeline selection"
                )
              )
            )
          ),

          # Status message area
          shiny::div(
            id = ns("export_status"),
            style = "margin-top: 15px;",
            shiny::uiOutput(ns("export_message"))
          )
        )
      )
    ),

    # ===== RECENT CLASSIFICATIONS TABLE =====
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
            shiny::h4("Recent Classifications", style = "margin: 0;"),
            shiny::div(
              style = "font-size: 13px; color: #666; font-style: italic;",
              shiny::textOutput(ns("table_timeline_info"), inline = TRUE)
            )
          ),
          DT::dataTableOutput(ns("recent_table"))
        )
      )
    )
  )
}

#' Export Module Server
#'
#' Server logic for the export module.
#'
#' @param id Module namespace ID
#' @param .dir Reactive or static path to project directory
#' @param schema Reactive or static schema list
#' @param marked_docs ReactiveValues object with marked document IDs
#' @export
mod_export_server <- function(id, .dir, schema, marked_docs = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # Make inputs reactive if they're not already
    dir_r <- if (shiny::is.reactive(.dir)) .dir else shiny::reactive(.dir)
    schema_r <- if (shiny::is.reactive(schema)) schema else shiny::reactive(schema)

    # ===== TIMELINE STATE =====
    timeline_state <- shiny::reactiveValues(
      timestamps = NULL,
      selected_timestamp = NULL,
      selected_index = NULL
    )

    # Load all timestamps
    shiny::observe({
      all_timestamps <- get_all_timestamps(dir_r())

      if (length(all_timestamps) > 0) {
        timeline_state$timestamps <- all_timestamps
        # Default to most recent (current state)
        timeline_state$selected_timestamp <- NULL
        timeline_state$selected_index <- length(all_timestamps)
      } else {
        timeline_state$timestamps <- NULL
        timeline_state$selected_timestamp <- NULL
        timeline_state$selected_index <- NULL
      }
    })

    # ===== TIMELINE SLIDER UI =====
    output$timeline_slider_ui <- shiny::renderUI({
      if (is.null(timeline_state$timestamps) || length(timeline_state$timestamps) == 0) {
        return(shiny::div(
          style = "text-align: center; padding: 20px; color: #999;",
          "No classification history yet"
        ))
      }

      n_timestamps <- length(timeline_state$timestamps)

      shiny::sliderInput(
        session$ns("timeline_slider"),
        label = "Select Point in Time:",
        min = 1,
        max = n_timestamps,
        value = timeline_state$selected_index %||% n_timestamps,
        step = 1,
        width = "100%",
        ticks = FALSE
      )
    })

    # Handle timeline slider changes
    shiny::observeEvent(input$timeline_slider, {
      if (!is.null(timeline_state$timestamps) && length(timeline_state$timestamps) > 0) {
        idx <- input$timeline_slider
        timeline_state$selected_index <- idx

        if (idx == length(timeline_state$timestamps)) {
          # At the end = current state
          timeline_state$selected_timestamp <- NULL
        } else {
          timeline_state$selected_timestamp <- timeline_state$timestamps[idx]
        }
      }
    })

    # Reset timeline button
    shiny::observeEvent(input$reset_timeline, {
      if (!is.null(timeline_state$timestamps)) {
        timeline_state$selected_index <- length(timeline_state$timestamps)
        timeline_state$selected_timestamp <- NULL
        shiny::showNotification("Reset to current state", type = "message", duration = 2)
      }
    })

    # Display selected date/time
    output$selected_date <- shiny::renderText({
      if (is.null(timeline_state$selected_timestamp)) {
        format(Sys.time(), "%Y-%m-%d")
      } else {
        format(timeline_state$selected_timestamp, "%Y-%m-%d")
      }
    })

    output$selected_time <- shiny::renderText({
      if (is.null(timeline_state$selected_timestamp)) {
        format(Sys.time(), "%H:%M:%S")
      } else {
        format(timeline_state$selected_timestamp, "%H:%M:%S")
      }
    })

    output$timeline_status <- shiny::renderText({
      if (is.null(timeline_state$selected_timestamp)) {
        "Viewing current state"
      } else {
        "Viewing historical state"
      }
    })

    output$table_timeline_info <- shiny::renderText({
      if (is.null(timeline_state$selected_timestamp)) {
        "Showing current classifications"
      } else {
        paste("Showing classifications as of", format_timestamp(timeline_state$selected_timestamp))
      }
    })

    # ===== LOAD DATA WITH TIMELINE SUPPORT =====
    classifications_data <- shiny::reactive({
      max_ts <- timeline_state$selected_timestamp

      # Get database connection
      con <- get_db_connection(dir_r())
      on.exit(DBI::dbDisconnect(con))

      # Use the helper function from data_io.R
      result <- get_current_classifications(con, .max_timestamp = max_ts)

      if (nrow(result) == 0) {
        return(data.frame(
          DocID = character(),
          UserID = character(),
          Timestamp = as.POSIXct(character()),
          Schema = integer(),
          stringsAsFactors = FALSE
        ))
      }

      # Standardize column names
      names(result) <- c("DocID", "UserID", "Timestamp", "Schema", "Class", "Value")
      result$Timestamp <- as.POSIXct(result$Timestamp, origin = "1970-01-01")

      result
    })

    # ===== RECENT CLASSIFICATIONS TABLE =====
    output$recent_table <- DT::renderDataTable({
      data <- classifications_data()
      sch <- schema_r()

      if (nrow(data) == 0) {
        return(data.frame(Message = "No classifications yet"))
      }

      # Get most recent classification per document (within timeline)
      recent <- data %>%
        dplyr::group_by(DocID) %>%
        dplyr::arrange(dplyr::desc(Timestamp)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(Timestamp)) %>%
        dplyr::select(DocID, UserID, Timestamp, Schema, Class, Value) %>%
        dplyr::mutate(
          Schema = sapply(Schema, function(sid) {
            sch[[as.character(sid)]]$name %||% paste("Schema", sid)
          }),
          Timestamp = format(Timestamp, "%Y-%m-%d %H:%M:%S")
        ) %>%
        dplyr::slice_head(n = 50)


      # Reorder columns
      recent <- recent[, c("DocID", "UserID", "Timestamp", "Schema", "Class", "Value")]

      DT::datatable(
        recent,
        options = list(
          pageLength = 10,
          dom = "tip",
          ordering = TRUE,
          order = list(list(3, "desc")),
          columnDefs = list(
            list(
              targets = 0,
              checkboxes = TRUE,
              orderable = FALSE
            )
          )
        ),
        selection = "none",
        rownames = FALSE,
        class = "cell-border stripe"
      )
    })

    # Handle marking changes
    shiny::observeEvent(input$recent_table_rows_selected, {
      if (!is.null(marked_docs)) {
        data <- classifications_data()

        if (nrow(data) > 0) {
          recent <- data %>%
            dplyr::group_by(DocID) %>%
            dplyr::arrange(dplyr::desc(Timestamp)) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(dplyr::desc(Timestamp)) %>%
            dplyr::slice_head(n = 50)

          selected_rows <- input$recent_table_rows_selected
          if (!is.null(selected_rows) && length(selected_rows) > 0) {
            marked_docs$ids <- unique(c(marked_docs$ids, recent$DocID[selected_rows]))
          }
        }
      }
    })

    # Handle checkbox clicks in datatable
    shiny::observeEvent(input$recent_table_cell_edit, {
      info <- input$recent_table_cell_edit
      if (!is.null(marked_docs) && !is.null(info)) {
        data <- classifications_data()

        if (nrow(data) > 0) {
          recent <- data %>%
            dplyr::group_by(DocID) %>%
            dplyr::arrange(dplyr::desc(Timestamp)) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(dplyr::desc(Timestamp)) %>%
            dplyr::slice_head(n = 50)

          row_idx <- info$row
          col_idx <- info$col
          new_value <- info$value

          if (col_idx == 0) { # Mark column
            doc_id <- recent$DocID[row_idx]

            if (new_value) {
              # Mark document
              marked_docs$ids <- unique(c(marked_docs$ids, doc_id))
              shiny::showNotification(
                paste("Marked:", doc_id),
                type = "message",
                duration = 2
              )
            } else {
              # Unmark document
              marked_docs$ids <- setdiff(marked_docs$ids, doc_id)
              shiny::showNotification(
                paste("Unmarked:", doc_id),
                type = "message",
                duration = 2
              )
            }
          }
        }
      }
    })


    # ===== EXPORT FUNCTIONALITY =====
    export_status <- shiny::reactiveVal(NULL)

    output$export_message <- shiny::renderUI({
      msg <- export_status()
      if (is.null(msg)) {
        return(NULL)
      }

      shiny::div(
        class = if (msg$type == "success") "alert alert-success" else "alert alert-info",
        style = "padding: 10px; margin: 0;",
        shiny::icon(if (msg$type == "success") "check-circle" else "info-circle"),
        " ", msg$text,
        if (!is.null(msg$link)) {
          shiny::tagList(
            shiny::br(),
            shiny::a(
              href = msg$link,
              download = basename(msg$link),
              "Download file",
              style = "color: #fff; text-decoration: underline; font-weight: bold;"
            )
          )
        }
      )
    })

    # Export Current State button
    shiny::observeEvent(input$export_current_btn, {
      show_export_modal("current")
    })

    # Export Full History button
    shiny::observeEvent(input$export_history_btn, {
      show_export_modal("history")
    })

    # Show export modal
    show_export_modal <- function(scope) {
      # Get timeline info
      timeline_info <- if (!is.null(timeline_state$selected_timestamp)) {
        paste("Up to:", format(timeline_state$selected_timestamp, "%Y-%m-%d %H:%M:%S"))
      } else {
        "Current state (latest)"
      }

      shiny::showModal(shiny::modalDialog(
        title = if (scope == "current") "Export Current State" else "Export Full History",
        size = "m",
        shiny::div(
          style = "padding: 10px;",

          # Format selection
          shiny::radioButtons(
            session$ns("export_format"),
            "Export Format:",
            choices = c(
              "CSV (Compatible with Excel)" = "csv",
              "Parquet (Efficient, compressed)" = "parquet"
            ),
            selected = "csv"
          ),

          # Content selection
          shiny::radioButtons(
            session$ns("export_content"),
            "What to export:",
            choices = c(
              "Classifications and Notes" = "both",
              "Classifications only" = "classifications",
              "Notes only" = "notes"
            ),
            selected = "both"
          ),

          # Info box
          shiny::div(
            style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin-top: 15px;",
            shiny::h5("Export Details:", style = "margin-top: 0; color: #1976d2;"),
            shiny::p(
              style = "font-size: 13px; margin-bottom: 5px;",
              shiny::strong("Scope: "),
              if (scope == "current") "Latest classifications and notes" else "All versions with complete history"
            ),
            shiny::p(
              style = "font-size: 13px; margin-bottom: 5px;",
              shiny::strong("Timeline: "), timeline_info
            ),
            shiny::p(
              style = "font-size: 13px; margin-bottom: 0;",
              shiny::strong("Location: "), "exports/ folder in project directory"
            )
          )
        ),
        footer = shiny::tagList(
          shiny::actionButton(
            session$ns("confirm_export"),
            "Export",
            class = "btn-primary",
            icon = shiny::icon("download"),
            onclick = sprintf("Shiny.setInputValue('%s', '%s')", session$ns("export_scope"), scope)
          ),
          shiny::modalButton("Cancel")
        )
      ))
    }

    # Confirm export
    shiny::observeEvent(input$confirm_export, {
      scope <- input$export_scope
      format <- input$export_format
      content <- input$export_content
      max_timestamp <- timeline_state$selected_timestamp

      shiny::removeModal()

      # Show progress
      export_status(list(
        type = "info",
        text = "Exporting data...",
        link = NULL
      ))

      tryCatch(
        {
          # Perform export
          filepath <- NULL

          if (content == "both") {
            if (scope == "current") {
              filepath <- export_current_state(dir_r(), format, .include_notes = TRUE, .max_timestamp = max_timestamp)
            } else {
              filepath <- export_full_history(dir_r(), format, .include_notes = TRUE, .max_timestamp = max_timestamp)
            }
          } else if (content == "classifications") {
            filepath <- export_classifications(dir_r(), format, scope, .max_timestamp = max_timestamp)
          } else {
            filepath <- export_notes(dir_r(), format, scope, .max_timestamp = max_timestamp)
          }

          # Get file info
          file_info <- file.info(filepath)
          file_size <- round(file_info$size / 1024, 1) # KB

          # Success message
          export_status(list(
            type = "success",
            text = paste0(
              "Export successful! ",
              "File: ", basename(filepath), " (",
              file_size, " KB)"
            ),
            link = filepath
          ))

          # Clear message after 10 seconds
          shiny::invalidateLater(10000)
          shiny::observe({
            export_status(NULL)
          })
        },
        error = function(e) {
          export_status(list(
            type = "error",
            text = paste("Export failed:", e$message),
            link = NULL
          ))
        }
      )
    })

    # View Exports button
    shiny::observeEvent(input$view_exports_btn, {
      show_exports_modal()
    })

    # Show exports list modal
    show_exports_modal <- function() {
      exports_list <- list_exports(dir_r())

      if (nrow(exports_list) == 0) {
        content <- shiny::div(
          style = "text-align: center; padding: 30px; color: #999;",
          shiny::icon("folder-open", style = "font-size: 48px; margin-bottom: 15px;"),
          shiny::h4("No exports yet"),
          shiny::p("Export data using the buttons above to see files here.")
        )
      } else {
        # Create table of exports
        content <- shiny::div(
          style = "max-height: 400px; overflow-y: auto;",
          shiny::tags$table(
            class = "table table-striped table-hover",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("Filename"),
                shiny::tags$th("Size"),
                shiny::tags$th("Modified"),
                shiny::tags$th("Actions")
              )
            ),
            shiny::tags$tbody(
              lapply(seq_len(nrow(exports_list)), function(i) {
                row <- exports_list[i, ]
                shiny::tags$tr(
                  shiny::tags$td(
                    shiny::tags$code(row$filename)
                  ),
                  shiny::tags$td(
                    paste0(row$size_mb, " MB")
                  ),
                  shiny::tags$td(
                    format(row$modified, "%Y-%m-%d %H:%M")
                  ),
                  shiny::tags$td(
                    shiny::div(
                      style = "display: flex; gap: 5px;",
                      shiny::a(
                        href = row$path,
                        download = row$filename,
                        class = "btn btn-sm btn-primary",
                        shiny::icon("download"),
                        " Download"
                      ),
                      shiny::actionButton(
                        session$ns(paste0("delete_", i)),
                        label = NULL,
                        icon = shiny::icon("trash"),
                        class = "btn btn-sm btn-danger",
                        onclick = sprintf(
                          "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                          session$ns("delete_export"),
                          row$filename
                        )
                      )
                    )
                  )
                )
              })
            )
          )
        )
      }

      shiny::showModal(shiny::modalDialog(
        title = "Exported Files",
        size = "l",
        content,
        footer = shiny::tagList(
          shiny::actionButton(
            session$ns("refresh_exports"),
            "Refresh",
            icon = shiny::icon("sync")
          ),
          shiny::modalButton("Close")
        )
      ))
    }

    # Delete export
    shiny::observeEvent(input$delete_export, {
      filename <- input$delete_export

      success <- delete_export(dir_r(), filename)

      if (success) {
        shiny::showNotification(
          paste("Deleted:", filename),
          type = "message",
          duration = 3
        )
        # Refresh modal
        show_exports_modal()
      } else {
        shiny::showNotification(
          paste("Failed to delete:", filename),
          type = "error",
          duration = 5
        )
      }
    })

    # Refresh exports list
    shiny::observeEvent(input$refresh_exports, {
      show_exports_modal()
    })
  })
}
