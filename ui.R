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
source(file = "components/ui/ui_analysis_value_boxes.R")

# pick --------------------------------------------------------------------
# country
ui_data_countries <- readxl::read_excel(
  path = "data-raw/FIMA_APP.xlsx",
  sheet = "Instruments") %>% 
  pull(country) %>% 
  unique() %>% 
  sort()

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
# Define UI
ui <- bslib::page_navbar(
  id = "main_navbar",
  fillable = TRUE,
  # Navbar configuration
  bg = "#2c3e50",
  title = div(
    class = "header-container",
    tags$img(
      src = "ssdh_logo.svg",
      class = "header-logo"
    ),
    div(
      class = "header-title-container",
      style = "display: flex; flex-direction: column; justify-content: center;",
      span(
        class = "header-title",
        style = "font-size: 1.5rem; font-weight: 600; color: white; line-height: 1.2;",
        "FIMA Explorer"
      ),
      # span(
      #   class = "header-subtitle",
      #   style = "font-size: 0.85rem; font-weight: 600; color: white;",
      #   "Financial Materiality Assessment"
      #   # style = "font-size: 0.85rem; color: #ffcccc;",
      #   # "The app is under development"
      # )
    )
  ),
  # CSS imports
  tags$head(
    # Favicon
    tags$link(rel = "icon", type = "image/x-icon", href = "ssdh_icon.png"),
    
    # tabs
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/tab_home.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/tab_analysis.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/tab_data.css"),
    
    # CSS stylesheets
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
      width = 1 / 2,
      heights_equal = "row",
      # About the FIMA Explorer App
      bslib::card(
        full_screen = TRUE,
        height = 400,
        class = "border-0 shadow-none",
        bslib::card_header(h5("About the FIMA Explorer App")),
        bslib::card_body(
          fillable = TRUE,
          fill = TRUE,
          # About the app with title on border
          tags$div(
            class = "home-box-container", 
            style = "position: relative; margin-top: 10px; padding-top: 1px;",
            
            # Title that will appear on the border
            tags$div(class = "home-box-title", "About the app"),
            
            # Content box
            tags$div(
              class = "home-box p-2 mb-0 border rounded steps",
              p(
                "Financial Materiality Assessment (FIMA) Explorer App helps to 
                  analysis the impact of interventions based on their
                  Key Performance Indicators (KPIs) on key indicators such as:
                  Credit Rating, Debt-GDP ratio, %, Nominal GDP growth (%),
                  Interest Payments % Revenue and Primary Balance, % of Nominal 
                  GDP. For instance, does a country benefit from improved 
                  Protection Gap / Land use in terms of improved Credit Rating, 
                  lower Debt-GDP ratio, %, as well well increased Nominal GDP 
                  growth (%).
                "
              )
            )
          ),
          # home tab with title on border
          tags$div(
            class = "home-box-container", 
            style = "position: relative; margin-top: 10px; padding-top: 1px;",
            # Title that will appear on the border
            tags$div(class = "home-box-title", "Home tab"),
            
            # Content box
            tags$div(
              class = "home-box steps",
              # step 1
              h5("Step 1 : Select Country"),
              p(
                "Under select country dropdown, select country of your choice, 
                  it will display vulnerabilities it is exposed to."
              )
            )
          ),
          # Analysis tab box with title on border
          tags$div(
            class = "home-box-container", 
            style = "position: relative; margin-top: 10px; padding-top: 1px;",
            
            # Title that will appear on the border
            tags$div(class = "home-box-title", "Analysis tab"),
            
            # Content box
            tags$div(
              class = "home-box steps",
              # step 2
              h5("Step 2 : Check KPI(s) of choice"),
              p(
                "Under KPI checkboxes (on the Analysis tab), check KPI(s) of choice. 
                  This will enable respective intstruments and interventions to display thereafter."
              ),
              # step 3
              h5("Step 3 : Interact with Intstruments and Interventions"),
              p(
                "Once the intstruments and interventions are displayed, one can start checking them 
                  and then check the charts in 'Analysis' tab to see if there is any deviation."
              )
            )
          )
        )
      ),
      # Risk Assessment
      bslib::card(
        full_screen = TRUE,
        height = 400,
        class = "border-0 shadow-none",
        bslib::card_header(h5("Risk Assessment")),
        bslib::card_body(
          fillable = TRUE,
          fill = TRUE,
          #---------------
          # select country
          #---------------
          tags$div(
            class = "home-box-container", 
            style = "position: relative; margin-top: 10px; padding-top: 1px;",
            # Title that will appear on the border
            tags$div(class = "home-box-title", "Country"),
            # Content box
            tags$div(
              class = "home-box steps",
              style = "padding-top: 10px; padding-bottom: 0px;", 
              div(
                style = "margin-top: 5px; margin-bottom: 0px;", 
                shinyWidgets::pickerInput(
                  inputId = "id_country",
                  label = NULL,
                  choices = ui_data_countries,
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
                )
              )
            )
          ),
          #---------------
          # Vulnerability
          #---------------
          # Conditional panel for vulnerabilities
          conditionalPanel(
            condition = "input.id_country !== null && input.id_country !== ''",
            tags$div(
              class = "home-box-container", 
              style = "position: relative; margin-top: 10px; padding-top: 1px;",
              # Title that will appear on the border
              tags$div(class = "home-box-title", "Vulnerabilities"),
              
              # Content box
              tags$div(
                class = "home-box steps",
                tags$div(
                  uiOutput("vulnerability_list")
                )
              )
            )
          )
        )
      )
    )
  ),
  # -------------------------------------------------------------------------
  # Analysis Panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Analysis",
    value = "analysis",
    bslib::card(
      class = "border-0 shadow-none",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bg = "#2c3e50",
          id = "analysis_sidebar",
          class = "tab-analysis-sidebar",
          # KPIs checkbox
          conditionalPanel(
            condition = "input.id_country !== null && input.id_country !== ''",
            # country
            tags$span(
              class = "analysis-country",
              textOutput(outputId = "analysis_check_country")
            ),
            # KPIs section
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              h5("KPI(s)"),
              uiOutput("dynamic_kpi_checkboxes")
            )
          ),
          # Instruments
          # UI component
          conditionalPanel(
            "input.id_country !== null && input.id_country !== '' && 
              input.kpi_selection.length > 0 && 
              (
                input.kpi_selection.includes('protection_gap') || 
                input.kpi_selection.includes('land_use')
              )",
            tags$div(
              class = "instruments-panel",
              style = "background-color: #2c3e50; color: white; padding: 10px; border-radius: 5px;",
              h4("Instruments", style = "margin-top: 5px; margin-bottom: 15px;"),
              uiOutput("dynamic_instruments_checkboxes")
            )
          ),
          # land use intervetnions checkbox
          conditionalPanel(
            condition = "input.kpi_selection.includes('land_use')",
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              h5("Land Use"),
              h6("(Interventions)"),
              uiOutput("dynamic_land_use_interventions_checkboxes")
            )
          ),
          # protection gap interventions checkbox
          conditionalPanel(
            condition = "input.kpi_selection.includes('protection_gap')",
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              h5("Protection Gap"),
              h6("(Interventions)"),
              uiOutput("dynamic_protection_gap_interventions_checkboxes")  # Dynamic checkboxes will be rendered here
            )
          )
        ),
        # -------------------------------------------------------------------------
        # main content in analysis tab
        # -------------------------------------------------------------------------
        conditionalPanel(
          condition = "input.id_country !== null &&  input.id_country !== ''",
          # main output
          # Value boxes at the top
          layout_column_wrap(
            width = 1 / 5,
            !!!vbs
          ),
          # row one of charts
          bslib::layout_column_wrap(
            width = NULL,
            heights_equal = "row",
            layout_column_wrap(
              width = 1 / 3,
              heights_equal = "row",
              # credit rating
              card(
                full_screen = TRUE,
                height = "400px", 
                card_header(
                  # title
                  "Credit rating",
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(outputId = "home_credit_rating", height = "230px") 
              ),
              # General government gross debt (% GDP)
              card(
                full_screen = TRUE,
                height = "400px", 
                card_header(
                  # title
                  "General government gross debt (% GDP)",
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(outputId = "home_debt_ngdp", height = "230px") 
              ),
              # Nominal GDP growth (%)
              card(
                full_screen = TRUE,
                height = "400px", 
                card_header(
                  # title
                  "Nominal GDP growth (%)",
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(outputId = "home_ngdp_growth", height = "230px") 
              )
            )
          ),
          # row two of charts
          bslib::layout_column_wrap(
            width = NULL,
            heights_equal = "row",
            layout_column_wrap(
              width = 1 / 2,
              heights_equal = "row",
              # General government interest payments (% Revenue)
              card(
                full_screen = TRUE,
                height = "400px", # Add this line to set a fixed height
                card_header(
                  # title
                  "General government interest payments (% Revenue)",
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(outputId = "home_ir_revenue", height = "230px") # Adjust chart height
              ),
              # Primary Balance, % of Nominal GDP
              card(
                full_screen = TRUE,
                height = "400px", # Add this line to set a fixed height
                card_header(
                  # title
                  "Primary Balance, % of Nominal GDP",
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(outputId = "home_pb", height = "230px") # Adjust chart height
              )
            )
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
    value = "data",
    bslib::card(
      class = "border-0 shadow-none",
      bslib::layout_sidebar(
        # -------------------------------------------------------------------------
        # sidebar content in data tab
        # -------------------------------------------------------------------------
        sidebar = bslib::sidebar(
          bg = "#2c3e50",
          # overview note
          conditionalPanel(
            condition = "input.id_country !== null && 
               input.id_country !== '' && 
               input.kpi_selection.length > 0 && 
               (input.kpi_selection.includes('protection_gap') || 
                input.kpi_selection.includes('land_use')) &&
               (input.protection_gap_interventions.length > 0 || 
                input.land_use_interventions.length > 0)",
            # country
            tags$span(
              class = "data-country",
              textOutput(outputId = "data_check_country")
            ),
            # note
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              tags$p(h5(icon("info-circle"), "Overview:"), class = "fw-bold"),
              tags$p(
                "In Alternative Scenario, the projections start from 2024. 
                The values before 2024 in Alternative Scenarios are baseline 
                values."
              )
            )
          ),
          # download data
          conditionalPanel(
            condition = "input.id_country !== null && 
               input.id_country !== '' && 
               input.kpi_selection.length > 0 && 
               (input.kpi_selection.includes('protection_gap') || 
                input.kpi_selection.includes('land_use')) &&
               (input.protection_gap_interventions.length > 0 || 
                input.land_use_interventions.length > 0)",
            tags$div(
              class = "guide-box p-2 mb-0",
              downloadButton(
                "download_data", 
                "Download Data",
                class = "data-button",
                icon = icon("download")
              )
            )
          )
        ),
        # -------------------------------------------------------------------------
        # main content in data tab
        # -------------------------------------------------------------------------
        # Baseline Scenario data
        layout_column_wrap(
          width = 1,
          heights_equal = "row",
          # Baseline Scenario data
          conditionalPanel(
            condition = "input.id_country !== null &&  input.id_country !== ''",
            # table output
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
            )
          ),
          # Alternative Scenario data
          conditionalPanel(
            condition = "input.id_country !== null && 
               input.id_country !== '' && 
               input.kpi_selection.length > 0 && 
               (input.kpi_selection.includes('protection_gap') || 
                input.kpi_selection.includes('land_use')) &&
               (input.protection_gap_interventions.length > 0 || 
                input.land_use_interventions.length > 0)",
            
            # table output
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
    )
  ),
  # -------------------------------------------------------------------------
  # Documentation panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Documentation",
    value = "docs",
    # Documentation component
    ui_documentation_component()
  ),
  
  # Add footer
  footer = ui_footer_component()
)
