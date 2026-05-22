library(shiny)
library(dplyr)
library(ggplot2)
library(here)
library(maps)
library(DT)

unimet <- read.delim(
  here("data", "UniMet.v1.tsv"),
  sep = "\t",
  quote = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
) %>%
  mutate(
    latitude = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    colexification_type = tolower(colexification_type)
  ) %>%
  filter(
    !is.na(source_domain),
    !is.na(target_domain),
    !is.na(source_translation_in_English),
    !is.na(target_translation_in_English),
    !is.na(language),
    !is.na(latitude),
    !is.na(longitude)
  )

world_map <- map_data("world")

domain_pair_lookup <- unimet %>%
  group_by(source_domain, target_domain) %>%
  summarise(
    n_realizations = n_distinct(glottocode, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_realizations), source_domain, target_domain) %>%
  mutate(
    pair_id = paste0("dp_", row_number()),
    pair_label_base = paste0(source_domain, " -- ", target_domain),
    pair_label = paste0(pair_label_base, " (", n_realizations, " languages)")
  )

concept_pair_lookup <- unimet %>%
  group_by(source_translation_in_English, target_translation_in_English) %>%
  summarise(
    n_realizations = n_distinct(glottocode, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_realizations), source_translation_in_English, target_translation_in_English) %>%
  mutate(
    pair_id = paste0("cp_", row_number()),
    pair_label_base = paste0(source_translation_in_English, " -- ", target_translation_in_English),
    pair_label = paste0(pair_label_base, " (", n_realizations, " languages)")
  )

n_all_langs <- n_distinct(unimet$glottocode, na.rm = TRUE)

domain_pair_choices <- c(
  setNames("dp_all", paste0("All pairs (", n_all_langs, " languages)")),
  setNames(domain_pair_lookup$pair_id, domain_pair_lookup$pair_label)
)
concept_pair_choices <- c(
  setNames("cp_all", paste0("All pairs (", n_all_langs, " languages)")),
  setNames(concept_pair_lookup$pair_id, concept_pair_lookup$pair_label)
)

point_colors <- c(
  full = "#2E6DB6",
  partial = "#E69F00",
  both = "#A52A6C",
  other = "#666666"
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .tight-row {
        margin-bottom: 6px;
      }
      .tight-row .form-group {
        margin-bottom: 6px;
      }
      .map-row {
        margin-top: 2px;
        margin-bottom: 4px;
      }
      .table-row {
        margin-top: 0;
      }
      .app-note-box {
        max-width: 900px;
        padding: 10px 12px;
        background: rgba(255, 255, 255, 0.95);
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        box-shadow: 0 1px 4px rgba(0, 0, 0, 0.08);
        font-size: 12px;
        line-height: 1.35;
        margin: 16px 0 0 0;
      }
    "))
  ),
  h2("Colexification Explorer", style = "text-align: center; margin-top: 10px;"),
  fluidRow(
    class = "tight-row",
    column(
      12,
      radioButtons(
        "pair_type",
        "Select what to visualize:",
        choices = c(
          "Source-target domain pair" = "domain",
          "Source-target concept pair" = "concept"
        ),
        selected = "domain",
        inline = TRUE
      )
    )
  ),
  conditionalPanel(
    condition = "input.pair_type == 'domain'",
    fluidRow(
      class = "tight-row",
      column(
        12,
        selectizeInput(
          "domain_pair",
          "Choose a source-target domain pair:",
          choices = domain_pair_choices,
          selected = unname(domain_pair_choices[1]),
          options = list(placeholder = "Select a domain pair")
        )
      )
    )
  ),
  conditionalPanel(
    condition = "input.pair_type == 'concept'",
    fluidRow(
      class = "tight-row",
      column(
        12,
        selectizeInput(
          "concept_pair",
          "Choose a source-target concept pair:",
          choices = concept_pair_choices,
          selected = unname(concept_pair_choices[1]),
          options = list(placeholder = "Select a concept pair")
        )
      )
    )
  ),
  fluidRow(
    class = "map-row",
    column(
      12,
      plotOutput(
        "world_map_plot",
        height = "650px"
      )
    )
  ),
  fluidRow(
    class = "table-row",
    column(
      12,
      DTOutput("pair_table")
    )
  ),
  div(
    class = "app-note-box",
    tags$strong("About this app"),
    tags$br(),
    "This app is released as part of a research project described by: ",
    "Khishigsuren, T., Bella, G., Brochhagen, T., Marav, D., Giunchiglia, F., & Batsuren, K. (2022). ",
    tags$em("Metonymy as a universal cognitive phenomenon: evidence from multilingual lexicons."),
    " In ",
    tags$em("Proceedings of the Annual Meeting of the Cognitive Science Society"),
    " (Vol. 44, No. 44).",
    tags$br(),
    tags$br(),
    "For correspondence, please contact Temuulen Khishigsuren at ",
    tags$a("kh.temulen@gmail.com", href = "mailto:kh.temulen@gmail.com"),
    "."
  )
)

server <- function(input, output, session) {
  selected_rows <- reactive({
    req(input$pair_type)

    if (identical(input$pair_type, "domain")) {
      req(input$domain_pair)
      if (identical(input$domain_pair, "dp_all")) return(unimet)

      selected_domain <- domain_pair_lookup %>%
        filter(pair_id == input$domain_pair) %>%
        slice(1)

      req(nrow(selected_domain) == 1)

      return(
        unimet %>%
          filter(
            source_domain == selected_domain$source_domain,
            target_domain == selected_domain$target_domain
          )
      )
    }

    req(input$concept_pair)
    if (identical(input$concept_pair, "cp_all")) return(unimet)

    selected_concept <- concept_pair_lookup %>%
      filter(pair_id == input$concept_pair) %>%
      slice(1)

    req(nrow(selected_concept) == 1)

    unimet %>%
      filter(
        source_translation_in_English == selected_concept$source_translation_in_English,
        target_translation_in_English == selected_concept$target_translation_in_English
      )
  })

  selected_pair_label <- reactive({
    req(input$pair_type)

    if (identical(input$pair_type, "domain")) {
      req(input$domain_pair)
      if (identical(input$domain_pair, "dp_all"))
        return(paste0("All domain pairs (", n_all_langs, " languages)"))
      selected_domain <- domain_pair_lookup %>%
        filter(pair_id == input$domain_pair) %>%
        slice(1)
      return(
        paste0(
          selected_domain$pair_label_base,
          " (",
          selected_domain$n_realizations,
          " languages)"
        )
      )
    }

    req(input$concept_pair)
    if (identical(input$concept_pair, "cp_all"))
      return(paste0("All concept pairs (", n_all_langs, " languages)"))
    selected_concept <- concept_pair_lookup %>%
      filter(pair_id == input$concept_pair) %>%
      slice(1)
    paste0(
      selected_concept$pair_label_base,
      " (",
      selected_concept$n_realizations,
      " languages)"
    )
  })

  map_points <- reactive({
    selected_rows() %>%
      mutate(
        longitude = ifelse(
          grepl("North America|South America", macro_area),
          longitude - 360,
          longitude
        )
      ) %>%
      group_by(language, glottocode, latitude, longitude) %>%
      summarise(
        has_full = any(colexification_type == "full"),
        has_partial = any(colexification_type == "partial"),
        word_forms = paste(sort(unique(word_forms)), collapse = " ; "),
        .groups = "drop"
      ) %>%
      mutate(
        marker_type = case_when(
          has_full & has_partial ~ "both",
          has_full ~ "full",
          has_partial ~ "partial",
          TRUE ~ "other"
        )
      )
  })

  output$world_map_plot <- renderPlot({
    points <- map_points()
    validate(
      need(nrow(points) > 0, "No language coordinates found for this selected pair.")
    )

    ggplot() +
      geom_polygon(
        data = world_map,
        aes(x = long, y = lat, group = group),
        fill = "gray95",
        color = "gray80",
        linewidth = 0.2
      ) +
      geom_point(
        data = points,
        aes(x = longitude, y = latitude, color = marker_type),
        size = 2.8,
        alpha = 0.9
      ) +
      scale_color_manual(
        values = point_colors,
        breaks = c("full", "partial", "both", "other"),
        labels = c("Full", "Partial", "Full + partial", "Other"),
        name = "Colexification type"
      ) +
      coord_quickmap(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)
      )
  })

  output$pair_table <- renderDT({
    table_data <- selected_rows() %>%
      transmute(
        language,
        glottocode,
        word_forms,
        colexification_type,
        source_domain,
        target_domain,
        source_concept = source_translation_in_English,
        target_concept = target_translation_in_English,
        source_definition = source_concept_definition,
        target_definition = target_concept_definition
      ) %>%
      arrange(language, word_forms)

    datatable(
      table_data,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 20,
        lengthMenu = c(20, 50, 100),
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv")
      )
    )
  })
}

shinyApp(ui, server)
