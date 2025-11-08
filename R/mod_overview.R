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
    load_app_css(),

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

    # ===== TIMELINE CHART =====
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::h4("Classification Timeline"),
          shiny::plotOutput(ns("timeline_chart"), height = "250px")
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
#' @param marked_docs ReactiveValues object with marked document IDs
#' @export
mod_overview_server <- function(id, .dir, schema, marked_docs = NULL) {
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
      refresh_trigger() # Depend on refresh

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
              ggplot2::annotate("text",
                x = 0, y = 0,
                label = paste("No classifications for", data$schema[[schema_id]]$name),
                size = 5, color = "#999"
              ) +
              ggplot2::theme_void()
          } else {
            value_counts <- schema_data %>%
              dplyr::group_by(Class, Value) %>%
              dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
              dplyr::arrange(Class, dplyr::desc(count))

            ggplot2::ggplot(value_counts, ggplot2::aes(x = stats::reorder(Value, count), y = count, fill = Class)) +
              ggplot2::geom_col() +
              ggplot2::coord_flip() +
              ggplot2::facet_wrap(~Class, scales = "free_y", ncol = 2) +
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
  })
}
