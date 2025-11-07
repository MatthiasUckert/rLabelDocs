# OVERVIEW MODULE
# Analytics dashboard for document classification progress

#' Overview Module UI
#'
#' Generates the user interface for the overview/analytics tab.
#'
#' @param id Module namespace ID
#' @return Shiny UI
#' @export
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    overview_css(),

    # Refresh button at top
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          style = "text-align: right; margin-bottom: 20px;",
          shiny::actionButton(
            ns("refresh_btn"),
            "Refresh Data",
            icon = shiny::icon("sync"),
            class = "btn-primary"
          )
        )
      )
    ),

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
                  style = "font-size: 12px; margin-bottom: 0;",
                  shiny::strong("Full History:"), " All versions with timestamps"
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


    # ===== KEY METRICS ROW =====
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::div(
          class = "metric-box metric-total",
          shiny::h3(shiny::textOutput(ns("metric_total")), style = "margin: 0;"),
          shiny::p("Total Documents", style = "margin: 5px 0 0 0; color: #666;")
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "metric-box metric-classified",
          shiny::h3(shiny::textOutput(ns("metric_classified")), style = "margin: 0;"),
          shiny::p("Classified", style = "margin: 5px 0 0 0; color: #666;")
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "metric-box metric-remaining",
          shiny::h3(shiny::textOutput(ns("metric_remaining")), style = "margin: 0;"),
          shiny::p("Remaining", style = "margin: 5px 0 0 0; color: #666;")
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "metric-box metric-percent",
          shiny::h3(shiny::textOutput(ns("metric_percent")), style = "margin: 0;"),
          shiny::p("% Complete", style = "margin: 5px 0 0 0; color: #666;")
        )
      )
    ),

    # ===== CHARTS ROW =====
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("Progress Overview"),
          shiny::plotOutput(ns("progress_chart"), height = "300px")
        )
      ),
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("User Activity"),
          shiny::plotOutput(ns("user_activity_chart"), height = "300px")
        )
      )
    ),

    # ===== DISTRIBUTION CHARTS ROW =====
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::h4("Classification Distributions by Schema"),
          shiny::uiOutput(ns("distribution_charts"))
        )
      )
    ),

    # ===== TIMELINE AND RECENT CLASSIFICATIONS =====
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::h4("Classification Timeline"),
          shiny::plotOutput(ns("timeline_chart"), height = "250px")
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::h4("Recent Classifications"),
          DT::dataTableOutput(ns("recent_table"))
        )
      )
    )
  )
}

#' Overview Module Server
#'
#' Server logic for the overview module.
#'
#' @param id Module namespace ID
#' @param .dir Reactive or static path to project directory
#' @param schema Reactive or static schema list
#' @export
mod_overview_server <- function(id, .dir, schema) {
  shiny::moduleServer(id, function(input, output, session) {

    # Make inputs reactive if they're not already
    dir_r <- if (shiny::is.reactive(.dir)) .dir else shiny::reactive(.dir)
    schema_r <- if (shiny::is.reactive(schema)) schema else shiny::reactive(schema)

    # Reactive trigger for refresh
    refresh_trigger <- shiny::reactiveVal(0)

    shiny::observeEvent(input$refresh_btn, {
      refresh_trigger(refresh_trigger() + 1)
      shiny::showNotification("Data refreshed", type = "message", duration = 2)
    })

    # ===== LOAD DATA =====
    overview_data <- shiny::reactive({
      refresh_trigger()  # Depend on refresh

      progress <- get_progress_stats(dir_r())
      all_class <- read_all_classifications(dir_r())

      list(
        progress = progress,
        classifications = all_class,
        schema = schema_r()
      )
    })

    # ===== KEY METRICS =====
    output$metric_total <- shiny::renderText({
      format(overview_data()$progress$total_documents, big.mark = ",")
    })

    output$metric_classified <- shiny::renderText({
      format(overview_data()$progress$classified_documents, big.mark = ",")
    })

    output$metric_remaining <- shiny::renderText({
      format(overview_data()$progress$unclassified_documents, big.mark = ",")
    })

    output$metric_percent <- shiny::renderText({
      paste0(overview_data()$progress$percentage_complete, "%")
    })

    # ===== PROGRESS CHART (Donut) =====
    output$progress_chart <- shiny::renderPlot({
      data <- overview_data()

      plot_df <- data.frame(
        category = c("Classified", "Remaining"),
        count = c(
          data$progress$classified_documents,
          data$progress$unclassified_documents
        ),
        stringsAsFactors = FALSE
      )

      if (sum(plot_df$count) == 0) {
        # Empty state
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0, label = "No documents", size = 6, color = "#999") +
          ggplot2::theme_void()
      } else {
        plot_df <- plot_df %>%
          dplyr::mutate(
            fraction = count / sum(count),
            ymax = cumsum(fraction),
            ymin = dplyr::lag(ymax, default = 0)
          )

        ggplot2::ggplot(plot_df, ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
          ggplot2::geom_rect() +
          ggplot2::coord_polar(theta = "y") +
          ggplot2::xlim(c(2, 4)) +
          ggplot2::scale_fill_manual(values = c("Classified" = "#27ae60", "Remaining" = "#e74c3c")) +
          ggplot2::theme_void() +
          ggplot2::theme(
            legend.position = "bottom",
            legend.title = ggplot2::element_blank()
          ) +
          ggplot2::labs(title = NULL)
      }
    })

    # ===== USER ACTIVITY CHART =====
    output$user_activity_chart <- shiny::renderPlot({
      data <- overview_data()

      if (nrow(data$classifications) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0, label = "No classifications yet", size = 6, color = "#999") +
          ggplot2::theme_void()
      } else {
        user_counts <- data$classifications %>%
          dplyr::group_by(UserID) %>%
          dplyr::summarise(
            classifications = dplyr::n_distinct(DocID),
            .groups = "drop"
          ) %>%
          dplyr::arrange(dplyr::desc(classifications))

        ggplot2::ggplot(user_counts, ggplot2::aes(x = stats::reorder(UserID, classifications), y = classifications)) +
          ggplot2::geom_col(fill = "#3498db") +
          ggplot2::coord_flip() +
          ggplot2::labs(
            x = NULL,
            y = "Documents Classified"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid.major.y = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(size = 11)
          )
      }
    })

    # ===== DISTRIBUTION CHARTS =====
    output$distribution_charts <- shiny::renderUI({
      data <- overview_data()

      if (nrow(data$classifications) == 0) {
        return(shiny::div(
          style = "text-align: center; color: #999; padding: 30px;",
          "No classifications to display"
        ))
      }

      # Create one chart per schema
      chart_outputs <- lapply(names(data$schema), function(schema_id) {
        output_id <- paste0("dist_chart_", schema_id)

        output[[output_id]] <- shiny::renderPlot({
          schema_data <- data$classifications %>%
            dplyr::filter(Schema == as.integer(schema_id))

          if (nrow(schema_data) == 0) {
            ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0, y = 0,
                                label = paste("No classifications for", data$schema[[schema_id]]$name),
                                size = 5, color = "#999") +
              ggplot2::theme_void()
          } else {
            value_counts <- schema_data %>%
              dplyr::group_by(Class, Value) %>%
              dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
              dplyr::arrange(Class, dplyr::desc(count))

            ggplot2::ggplot(value_counts, ggplot2::aes(x = stats::reorder(Value, count), y = count, fill = Class)) +
              ggplot2::geom_col() +
              ggplot2::coord_flip() +
              ggplot2::facet_wrap(~ Class, scales = "free_y", ncol = 2) +
              ggplot2::labs(
                title = data$schema[[schema_id]]$name,
                x = NULL,
                y = "Count"
              ) +
              ggplot2::theme_minimal() +
              ggplot2::theme(
                legend.position = "none",
                strip.text = ggplot2::element_text(face = "bold", size = 11),
                panel.grid.major.y = ggplot2::element_blank()
              )
          }
        })

        shiny::div(
          style = "margin-bottom: 20px;",
          shiny::plotOutput(session$ns(output_id), height = "300px")
        )
      })

      shiny::tagList(chart_outputs)
    })

    # ===== TIMELINE CHART =====
    output$timeline_chart <- shiny::renderPlot({
      data <- overview_data()

      if (nrow(data$classifications) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0, y = 0, label = "No classifications yet", size = 6, color = "#999") +
          ggplot2::theme_void()
      } else {
        timeline_data <- data$classifications %>%
          dplyr::mutate(Date = as.Date(Timestamp)) %>%
          dplyr::group_by(Date) %>%
          dplyr::summarise(
            documents = dplyr::n_distinct(DocID),
            .groups = "drop"
          ) %>%
          dplyr::arrange(Date)

        ggplot2::ggplot(timeline_data, ggplot2::aes(x = Date, y = documents)) +
          ggplot2::geom_line(color = "#3498db", size = 1) +
          ggplot2::geom_point(color = "#3498db", size = 2) +
          ggplot2::labs(
            x = NULL,
            y = "Documents Classified"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank()
          )
      }
    })

    # ===== RECENT CLASSIFICATIONS TABLE =====
    output$recent_table <- DT::renderDataTable({
      data <- overview_data()

      if (nrow(data$classifications) == 0) {
        return(data.frame(Message = "No classifications yet"))
      }

      # Get most recent classification per document
      recent <- data$classifications %>%
        dplyr::group_by(DocID) %>%
        dplyr::arrange(dplyr::desc(Timestamp)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(Timestamp)) %>%
        dplyr::select(DocID, UserID, Timestamp, Schema) %>%
        dplyr::mutate(
          Schema = sapply(Schema, function(sid) {
            data$schema[[as.character(sid)]]$name %||% paste("Schema", sid)
          }),
          Timestamp = format(Timestamp, "%Y-%m-%d %H:%M:%S")
        ) %>%
        dplyr::slice_head(n = 20)

      DT::datatable(
        recent,
        options = list(
          pageLength = 10,
          dom = 'tip',
          ordering = TRUE,
          order = list(list(2, 'desc'))
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
    })

    export_status <- shiny::reactiveVal(NULL)

    output$export_message <- shiny::renderUI({
      msg <- export_status()
      if (is.null(msg)) return(NULL)

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
              if (scope == "current") "Latest classifications and notes only" else "All versions with complete history"
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

      shiny::removeModal()

      # Show progress
      export_status(list(
        type = "info",
        text = "Exporting data...",
        link = NULL
      ))

      tryCatch({
        # Perform export
        filepath <- NULL

        if (content == "both") {
          if (scope == "current") {
            filepath <- export_current_state(dir_r(), format, .include_notes = TRUE)
          } else {
            filepath <- export_full_history(dir_r(), format, .include_notes = TRUE)
          }
        } else if (content == "classifications") {
          filepath <- export_classifications(dir_r(), format, scope)
        } else {
          filepath <- export_notes(dir_r(), format, scope)
        }

        # Get file info
        file_info <- file.info(filepath)
        file_size <- round(file_info$size / 1024, 1)  # KB

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

      }, error = function(e) {
        export_status(list(
          type = "error",
          text = paste("Export failed:", e$message),
          link = NULL
        ))
      })
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



    export_status <- shiny::reactiveVal(NULL)

    output$export_message <- shiny::renderUI({
      msg <- export_status()
      if (is.null(msg)) return(NULL)

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
              if (scope == "current") "Latest classifications and notes only" else "All versions with complete history"
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

      shiny::removeModal()

      # Show progress
      export_status(list(
        type = "info",
        text = "Exporting data...",
        link = NULL
      ))

      tryCatch({
        # Perform export
        filepath <- NULL

        if (content == "both") {
          if (scope == "current") {
            filepath <- export_current_state(dir_r(), format, .include_notes = TRUE)
          } else {
            filepath <- export_full_history(dir_r(), format, .include_notes = TRUE)
          }
        } else if (content == "classifications") {
          filepath <- export_classifications(dir_r(), format, scope)
        } else {
          filepath <- export_notes(dir_r(), format, scope)
        }

        # Get file info
        file_info <- file.info(filepath)
        file_size <- round(file_info$size / 1024, 1)  # KB

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

      }, error = function(e) {
        export_status(list(
          type = "error",
          text = paste("Export failed:", e$message),
          link = NULL
        ))
      })
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

#' CSS for Overview Interface
#'
#' Returns CSS styling for overview components.
#'
#' @return HTML head tag with CSS
#' @keywords internal
overview_css <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      /* Metric boxes */
      .metric-box {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        text-align: center;
        margin-bottom: 20px;
      }

      .metric-box h3 {
        font-size: 36px;
        font-weight: bold;
        color: #2c3e50;
      }

      .metric-total { border-left: 4px solid #3498db; }
      .metric-classified { border-left: 4px solid #27ae60; }
      .metric-remaining { border-left: 4px solid #e74c3c; }
      .metric-percent { border-left: 4px solid #f39c12; }

      /* Well panels */
      .well {
        background-color: white;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }

      .well h4 {
        margin-top: 0;
        margin-bottom: 15px;
        color: #2c3e50;
        font-weight: 600;
      }
    "))
  )
}
