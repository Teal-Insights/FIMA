# -------------------------------------------------------------------------
# FIMA Explorer Shiny Application
# -------------------------------------------------------------------------

# Loading necessary libraries with suppressed messages
suppressPackageStartupMessages({
  library(tidyverse)
  library(shiny)
  library(bslib)
  library(shinyWidgets)
  library(DT)
  library(shinyjs)  # Added to the main imports since it's used in the UI
})

# components
source(file = "components/ui/ui_footer_component.R")
source(file = "components/ui/ui_documentation_component.R")

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
# Define UI
ui <- bslib::page_navbar(
  # Initialize shinyjs
  shinyjs::useShinyjs(),
  fillable = TRUE,
  # Navbar configuration
  bg = "#2c3e50",
  title = div(
    class = "header-container",
    tags$img(
      src = "ssdh_logo.svg",
      class = "header-logo",
      style = "border-right: 2px solid white; padding-right: 10px; height: 60px"
    ),
    span(
      class = "header-title",
      "Ruritania FIMA Explorer"
    )
  ),
  
  # Empty header div (keeping the original structure)
  header = div(),
  
  # CSS imports
  tags$head(
    # Favicon
    tags$link(rel = "icon", type = "image/x-icon", href = "ssdh_icon.png"),
    
    # CSS stylesheets
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/tab_data.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/navbar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/footer.css"),
    
    # Commented stylesheets - uncomment when files are available
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/integrated.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/documentation.css")
  ),
  
  # Add space between title and tabs
  bslib::nav_spacer(),
  
  # Theme configuration
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  
  # Home Panel
  bslib::nav_panel(
    title = "Home",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bg = "#2c3e50",
          
          # KPI selector
          tags$div(
            class = "guide-box p-3 mb-0 border rounded",
            h5("KPI"),
            selectInput(
              inputId = "id_kpi",
              label = NULL,
              choices = c("Protection Gap", "Regenerative Agriculture"),
              selected = "Protection Gap"
            )
          ),
          hr(),
          # Interventions selector
          tags$div(
            class = "guide-box p-3 mb-0 border rounded",
            h5("Interventions"),
            br(),
            # Conditional panels for interventions based on KPI selection
            conditionalPanel(
              condition = "input.id_kpi == 'Protection Gap'",
              checkboxGroupInput(
                inputId = "id_intervention_pg",
                label = NULL,
                choices = c(
                  "Catastrophe Bonds" = "catastrophe_bonds",
                  "Insurance Premium Subsidies" = "insurance_premium_subsidies",
                  "Microinsurance" = "microinsurance",
                  "Cross-border reinsurance" = "cross_border_reinsurance",
                  "Compulsory insurance coverage" = "compulsory_insurance_coverage",
                  "Insurance bundling" = "insurance_bundling",
                  "Risk-based solvency capital requirements" = "risk_based_solvency"
                )
              )
            ),
            # Land Use conditional panel (renamed from Regenerative Agriculture)
            conditionalPanel(
              condition = "input.id_kpi == 'Regenerative Agriculture'",
              checkboxGroupInput(
                inputId = "id_intervention_ra",
                label = NULL,
                choices = c(
                  "Silvopasture" = "Silvopasture",
                  "Reduced-Till Farming" = "reduced_till_farming",
                  "Climate-resilient seeds" = "climate_resilient_seeds",
                  "Managed Grazing" = "managed_grazing",
                  "Biological Fertilization" = "biological_fertilization",
                  "Polyculture & Crop Rotation" = "polyculture_crop_rotation",
                  "Organic Certification Practices" = "organic_certification",
                  "Integrated Pest Management" = "integrated_pest_management"
                )
              )
            )
          ),
          hr(),
          # Added: Show/Hide control for all tables
          tags$div(
            class = "guide-box p-3 mb-0 border rounded",
            h5("Display Indicators"),
            checkboxInput("show_tables", "Show Tables", value = TRUE)
          )
        ),
        # Main content area

        # -------------------------------------------------------------------------
        # interventions - shocking
        # -------------------------------------------------------------------------
        layout_column_wrap(
          width = 1,
          # interventions - shocking
          # -------------------------------------------------------------------------
          conditionalPanel(
            condition = "input.show_tables == true",
            # interventions - shocking
            # -------------------------------------------------------------------------
            # Nominal GDP growth
            card(
              full_screen = TRUE,
              card_header(
                # title
                div(
                  class = "d-flex justify-content-between align-items-center",
                  "Nominal GDP growth",
                  # Constant value controls (shown when interventions selected)
                  uiOutput("ngdp_constant_controls")
                ),
                # class
                class = "bg-primary text-white",
              ),
              # output
              card_body(
                # Table output with editable cells for interventions
                uiOutput("ngdp_inputs")
              )
            ),
            # Interest rate
            card(
              full_screen = TRUE,
              card_header(
                # title
                div(
                  class = "d-flex justify-content-between align-items-center",
                  "Interest rate",
                  # Constant value controls (shown when interventions selected)
                  uiOutput("interest_constant_controls")
                ),
                # class
                class = "bg-primary text-white",
              ),
              # output
              card_body(
                # Table output with editable cells for interventions
                uiOutput("interest_inputs")
              )
            ),
            # Primary balance
            card(
              full_screen = TRUE,
              card_header(
                # title
                div(
                  class = "d-flex justify-content-between align-items-center",
                  "Primary balance",
                  # Constant value controls (shown when interventions selected)
                  uiOutput("pb_constant_controls")
                ),
                # class
                class = "bg-primary text-white",
              ),
              # output
              card_body(
                # Table output with editable cells for interventions
                uiOutput("pb_inputs")
              )
            )
          )
        ),
        # -------------------------------------------------------------------------
        # Result output
        # -------------------------------------------------------------------------
        bslib::layout_column_wrap(
          width = NULL,
          heights_equal = "row",
          layout_column_wrap(
            width = 1 / 2,
            heights_equal = "row",
            # Debt-NGDP ratio, %
            card(
              full_screen = TRUE,
              card_header(
                # title
                "Debt-GDP ratio, %",
                # class
                class = "bg-primary text-white",
              ),
              plotOutput(outputId = "home_debt_ngdp")
            ),
            # Nominal GDP growth (%)
            card(
              full_screen = TRUE,
              card_header(
                # title
                "Nominal GDP growth (%)",
                # class
                class = "bg-primary text-white",
              ),
              plotOutput(outputId = "home_primary_balance")
            )
          )
        ),
        bslib::layout_column_wrap(
          width = NULL,
          heights_equal = "row",
          layout_column_wrap(
            width = 1 / 2,
            heights_equal = "row",
            # Interest % Revenue
            card(
              full_screen = TRUE,
              card_header(
                # title
                "Interest % Revenue",
                # class
                class = "bg-primary text-white",
              ),
              plotOutput(outputId = "home_debt_ngdp")
            ),
            # Primary Balance, % of Nominal GDP
            card(
              full_screen = TRUE,
              card_header(
                # title
                "Primary Balance, % of Nominal GDP",
                # class
                class = "bg-primary text-white",
              ),
              plotOutput(outputId = "home_primary_balance")
            )
          )
        ),
        
      )
    )
  ),
  # Data panel
  bslib::nav_panel(
    title = "Data",
    bslib::card(
      bslib::card_header("Data Explorer"),
      bslib::card_body(
        DT::dataTableOutput("data_table")
      )
    )
  ),
  
  # Documentation panel
  # Documentation Panel
  bslib::nav_panel(
    title = "Documentation",
    # Documentation component
    ui_documentation_component()
  ),
  
  # Add footer
  footer = ui_footer_component()
)
