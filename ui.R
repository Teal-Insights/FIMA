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
# Define the interventions for each KPI
intervention_data <- list(
  protection_gap = c("catastrophe_bonds", 
                     "insurance_premium_subsidies", 
                     "microinsurance", 
                     "cross_border_reinsurance", 
                     "compulsory_insurance_coverage", 
                     "insurance_bundling", 
                     "risk_based_solvency_capital_requirements"),
  
  land_use = c("silvopasture", 
               "reduced_till_farming", 
               "dams_and_seawalls", 
               "restoring_degraded_forest", 
               "precision_agriculture", 
               "agroforestry", 
               "large_and_medium_scale_irrigation", 
               "climate_resilient_seeds")
)

# Create nice display names for interventions
intervention_display_names <- list(
  catastrophe_bonds = "Catastrophe Bonds",
  insurance_premium_subsidies = "Insurance Premium Subsidies",
  microinsurance = "Microinsurance",
  cross_border_reinsurance = "Cross-border Reinsurance",
  compulsory_insurance_coverage = "Compulsory Insurance Coverage",
  insurance_bundling = "Insurance Bundling",
  risk_based_solvency_capital_requirements = "Risk-based Solvency Capital Requirements",
  silvopasture = "Silvopasture",
  reduced_till_farming = "Reduced-Till Farming",
  dams_and_seawalls = "Dams and Seawalls",
  restoring_degraded_forest = "Restoring Degraded Forest",
  precision_agriculture = "Precision Agriculture",
  agroforestry = "Agroforestry",
  large_and_medium_scale_irrigation = "Large and Medium Scale Irrigation",
  climate_resilient_seeds = "Climate-resilient Seeds"
)

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
      "FIMA Explorer"
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
  # -------------------------------------------------------------------------
  # Home Panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Home",
    bslib::layout_column_wrap(
      width = 1 / 3,
      heights_equal = "row",
      # About the FIMA Explorer App
      bslib::card(
        full_screen = TRUE,
        height = 400,
        bslib::card_header("About the FIMA Explorer App"),
        bslib::card_body(
          fillable = TRUE,
          fill = TRUE
        )
      ),
      # Risk Assessment
      bslib::card(
        full_screen = TRUE,
        height = 400,
        bslib::card_header("Risk Assessment"),
        bslib::card_body(
          fillable = TRUE,
          fill = TRUE,
          #---------------
          # select country
          #---------------
          shinyWidgets::pickerInput(
            inputId = "id_country",
            label = h5("Country"),
            choices = c("Aurelia", "Ruritania", "Xenon"),
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 3",
              liveSearch = TRUE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Select country...",
              title = "Select country..."
            ),
            multiple = FALSE,
            selected = NULL
          ),
          #---------------
          # Vulnerability
          #---------------
          # Conditional panel for vulnerabilities
          conditionalPanel(
            condition = "input.id_country == 'Ruritania'",
            tags$div(
              tags$h5("Vulnerabilities:"),
              tags$ul(
                tags$li("Vulnerability 1"),
                tags$li("Vulnerability 2"),
                tags$li("Vulnerability 3"),
                tags$li("Vulnerability 4")
              )
            )
          ),
          #---------------
          # KPIs
          #---------------
          conditionalPanel(
            condition = "input.id_country == 'Ruritania'",
            # KPIs section
            h5("KPIs"),
            checkboxGroupInput(
              inputId = "kpi_selection",
              label = NULL,
              choices = c("Protection Gap" = "protection_gap", 
                          "Land Use" = "land_use",
                          "KPI 3" = "kpi_3"),
              selected = NULL
            )
          )
        )
      ),
      # Interventions
      bslib::card(
        full_screen = TRUE,
        height = 400,
        bslib::card_header("Interventions"),
        bslib::card_body(
          fillable = TRUE,
          fill = TRUE,
          # Interventions selector
          conditionalPanel(
            condition = "input.kpi_selection.includes('protection_gap')",
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              h5("Protection Gap"),
              h6("(Interventions)"),
              br(),
              checkboxGroupInput(
                inputId = "protection_gap_interventions",
                label = NULL,
                choices = setNames(
                  intervention_data$protection_gap,
                  sapply(intervention_data$protection_gap, function(x) intervention_display_names[[x]])
                ),
                selected = intervention_data$protection_gap
              )
            )
            
          ),
          conditionalPanel(
            condition = "input.kpi_selection.includes('land_use')",
            hr(),
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              h5("Land Use"),
              h6("(Interventions)"),
              br(),
              checkboxGroupInput(
                inputId = "land_use_interventions",
                label = NULL,
                choices = setNames(
                  intervention_data$land_use,
                  sapply(intervention_data$land_use, function(x) intervention_display_names[[x]])
                ),
                selected = intervention_data$land_use
              )
            )
          )
        )
      ),
    )
  ),
  # -------------------------------------------------------------------------
  # Analysis Panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Analysis",
    bslib::card(
      bslib::layout_column_wrap(
        width = NULL,
        heights_equal = "row",
        layout_column_wrap(
          width = 1 / 3,
          heights_equal = "row",
          # credit rating
          card(
            full_screen = TRUE,
            card_header(
              # title
              "Credit rating",
              # class
              class = "bg-primary text-white",
            ),
            plotOutput(outputId = "home_credit_rating")
          ),
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
            plotOutput(outputId = "home_ngdp_growth")
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
            plotOutput(outputId = "home_ir_revenue")
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
            plotOutput(outputId = "home_pb")
          )
        )
      )
    )
  ),
  # -------------------------------------------------------------------------
  # Data panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Data",
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bg = "#2c3e50",
        ),
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          # Baseline Scenario data
          card(
            full_screen = TRUE,
            card_header(
              # title
              "Baseline Scenario data",
              # class
              class = "bg-primary text-white",
            ),
            bslib::card_body(
              reactable::reactableOutput("data_table_baseline")
            )
          ),
          # Alternative Scenario data
          card(
            full_screen = TRUE,
            card_header(
              # title
              "Alternative Scenario data",
              # class
              class = "bg-primary text-white",
            ),
            bslib::card_body(
              reactable::reactableOutput("data_table_alternative")
            )
          )
        )
      )
    )
  ),
  # -------------------------------------------------------------------------
  # Documentation panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Documentation",
    # Documentation component
    ui_documentation_component()
  ),
  
  # Add footer
  footer = ui_footer_component()
)
