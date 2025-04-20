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
  library(shinyjs)
})

# components
source(file = "components/ui/ui_footer_component.R")
source(file = "components/ui/ui_documentation_component.R")
source(file = "components/ui/ui_analysis_value_boxes.R")
source(file = "components/ui/ui_contact_component.R")

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
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/documentation.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/tab_contact.css"),
    
    # navbar and footer
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/navbar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/footer.css"),
    
    # integrated
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/integrated.css"),
    
    # bootstrap
    tags$link(
      rel = "stylesheet", 
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
    )
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
          navset_tab(
            nav_panel(
              title = "About the app", 
              tags$div(
                class = "home-box-container", 
                style = "position: relative; margin-top: 10px; padding-top: 1px;",
                
                # Title that will appear on the border
                tags$div(class = "home-box-title", "About the app"),
                
                # Content box
                tags$div(
                  class = "home-box p-2 mb-0 border rounded steps",
                  tags$h6("Welcome to the FIMA Explorer (PROTOTYPE)", class = "top-header"),
                  p("This web application demonstrates how the Financial Materiality Assessment (FIMA) framework can help countries identify and prioritize nature-based interventions with potential fiscal and economic benefits."),
                  tags$h6("Important Note on Data"),
                  p("The countries, scenarios, and numerical impacts shown in this prototype are ", tags$strong("illustrative only"), ". They are designed to demonstrate the workflow and potential of the FIMA approach rather than provide precise forecasts."),
                  tags$h6("Purpose of This Tool"),
                  tags$ul(
                    class = "green-bullets",
                    tags$li(
                      span("Provide a standardized framework for assessing intervention impacts")
                    ),
                    tags$li(
                      span("Demonstrate how climate and nature KPIs can be linked to sovereign financial metrics")
                    ),
                    tags$li(
                      span("Offer a starting point for deeper country-specific analysis")
                    ),
                    tags$li(
                      span("Help prioritize interventions based on credit relevance")
                    )
                  ),
                  p(
                    "Even as this tool evolves, it will continue to provide 
                order-of-magnitude approximations as a first step before 
                detailed economic analysis on a country-specific basis.
                " 
                  ),
                  p(
                    "We welcome your feedback on how this assessment framework 
                could better support your planning and decision-making 
                processes."
                  )
                )
              )
            ),
            nav_panel(
              title = "Guide on using the app", 
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
                  h6("Step 1 : Select Country", class = "top-header"),
                  p(
                    "
                  Under select country dropdown, select country of your choice, 
                  it will display vulnerabilities it is exposed to.
                "
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
                  h6("Step 2 : Check KPI(s) of choice", class = "top-header"),
                  p(
                    "
                      Under KPI checkboxes on the ", tags$strong("Analysis"),
                      " tab, check KPI(s) of choice. This will enable 
                      respective intstruments and interventions to display 
                      thereafter. Each instrument and intervention is color 
                      code.
                    "
                  ),
                  # about colors
                  h6("Color Legend", class = "top-header"),
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 10px;",
                    div(
                      class = "color-legend",
                      style = "background-color: #006400; color: white;",
                      tags$strong("Dark green")
                    ),
                    p("Most effective instrument(s) or intervention(s)")
                  ),
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 10px;",
                    div(
                      class = "color-legend",
                      style = "background-color: #0a830a; color: white;",
                      tags$strong("Medium green")
                    ),
                    p("Medium effective instrument(s) or intervention(s)")
                  ),
                  div(
                    style = "display: flex; align-items: center; margin-bottom: 10px;",
                    div(
                      class = "color-legend",
                      style = "background-color: #e8f5e8; color: black;",
                      tags$strong("Light green")
                    ),
                    p("Least effective instrument(s) or intervention(s)")
                  ),
                  
                  # step 3
                  h6(
                    style = "padding-top: 12px;",
                    "Step 3 : Interact with Intstruments and Intervention(s)"
                    ),
                  p(
                    "
                  Once the intstruments and interventions are displayed, one 
                  can start checking them and then check the charts on", 
                    tags$strong("Analysis"), "tab to see if there is any 
                  deviation.
                "
                  )
                )
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
                style = "margin-top: 5px; margin-bottom: 5px;", 
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
                  selected = NULL,
                  width = "300px"
                )
              )
            )
          ),
          #---------------
          # about country
          #---------------
          conditionalPanel(
            condition = "input.id_country !== null && input.id_country !== ''",
            tags$div(
              class = "home-box-container", 
              style = "position: relative; margin-top: 10px; padding-top: 1px;",
              
              # Title that will appear on the border
              tags$div(
                class = "home-box-title", 
                textOutput(outputId = "about_country_header")
              ),
              
              # Content box
              tags$div(
                class = "home-box steps",
                tags$div(
                  uiOutput("about_country_content")
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
              style = "padding: 0 !important; overflow: hidden;",
              tags$div(
                style = "background-color: #2c3e50; color: white; padding: 10px 15px; font-size: 18px; font-weight: bold;",
                "KPI Selection"
              ),
              tags$div(
                style = "background-color: white; color: black; padding: 0;",
                uiOutput("dynamic_kpi_checkboxes")
              )
            )
            
          ),
          # Instruments
          conditionalPanel(
            condition = "(input.kpi_selection.length > 0)",
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              style = "padding: 0 !important; overflow: hidden;",
              tags$div(
                style = "background-color: #2c3e50; color: white; padding: 10px 15px; font-size: 18px; font-weight: bold;",
                "Instruments"
              ),
              tags$div(
                style = "background-color: white; color: black; padding: 0;",
                uiOutput("dynamic_instruments_checkboxes")
              )
            )
          ),
          # land use intervetnions checkbox
          conditionalPanel(
            condition = 
              "(
              input.kpi_selection.length > 0 && 
              input.kpi_selection.includes('land_use')
             )
            ",
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              style = "padding: 0 !important; overflow: hidden; background-color: #2c3e50;",
              # Header
              tags$div(
                style = "color: white; padding: 10px 15px; font-size: 18px; font-weight: bold;",
                "Land Use (Interventions)"
              ),
              # Dynamic land use interventions content
              uiOutput("dynamic_land_use_interventions_checkboxes")
            )
          ),
          # protection gap interventions checkbox
          conditionalPanel(
            condition = 
              "(
                input.kpi_selection.length > 0 && 
                input.kpi_selection.includes('protection_gap')
              )",
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              style = "padding: 0 !important; overflow: hidden; background-color: #2c3e50;",
              # Header
              tags$div(
                style = "color: white; padding: 10px 15px; font-size: 18px; font-weight: bold;",
                "Protection Gap (Interventions)"
              ),
              # Dynamic protection gap interventions content
              uiOutput("dynamic_protection_gap_interventions_checkboxes")
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
                  h6("Credit Rating"),
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(
                  outputId = "home_credit_rating", height = "230px"
                ) 
              ),
              # General government gross debt (% GDP)
              card(
                full_screen = TRUE,
                height = "400px", 
                card_header(
                  # title
                  h6("General Government Gross Debt (% GDP)"),
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(
                  outputId = "home_debt_ngdp", height = "230px"
                ) 
              ),
              # Nominal GDP growth (%)
              card(
                full_screen = TRUE,
                height = "400px", 
                card_header(
                  # title
                  h6("Nominal GDP Growth (%)"),
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(
                  outputId = "home_ngdp_growth", height = "230px"
                ) 
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
                height = "400px", 
                card_header(
                  # title
                  h6("General Government Interest Payments (% Revenue)"),
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(
                  outputId = "home_ir_revenue", height = "230px"
                )
              ),
              # Primary Balance, % of Nominal GDP
              card(
                full_screen = TRUE,
                height = "400px",
                card_header(
                  # title
                  h6("Primary Balance, % of Nominal GDP"),
                  # class
                  class = "bg-primary text-white",
                ),
                echarts4r::echarts4rOutput(
                  outputId = "home_pb", height = "230px"
                )
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
          id = "data_sidebar",
          class = "tab-data-sidebar",
          
          # overview note
          
          conditionalPanel(
            condition = "
              (
               input.id_country !== null && input.id_country !== ''
              )",
            
            # country
            tags$span(
              class = "data-country",
              textOutput(outputId = "data_check_country")
            )
          ),
          # download data
          conditionalPanel(
            condition = "
              (
               input.kpi_selection.length > 0
              ) &&
              (
               input.protection_gap_interventions.length > 0 || 
               input.land_use_interventions.length > 0 ||
               input.id_instruments.length > 0
              )",
            
            # note
            tags$div(
              class = "guide-box p-2 mb-0 border rounded",
              style = "background-color: #e8f5e8; color: black;",
              tags$p(h5(icon("info-circle"), "Overview:"), class = "fw-bold"),
              tags$p(
                "In Alternative Scenario, the projections start from 2024. 
                The values before 2024 in Alternative Scenarios are baseline 
                values."
              )
            ),
            # download button
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
                h6("Baseline Scenario data"),
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
            condition = "
              (
               input.kpi_selection.length > 0
              ) &&
              (
               input.protection_gap_interventions.length > 0 || 
               input.land_use_interventions.length > 0 ||
               input.id_instruments.length > 0
              )",
            
            # alternative scenario table output
            card(
              full_screen = TRUE,
              card_header(
                # title
                h6("Alternative Scenario data"),
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
  # -------------------------------------------------------------------------
  # Contact panel
  # -------------------------------------------------------------------------
  bslib::nav_panel(
    title = "Contact",
    value = "contact",
    # contact component
    ui_contact_component()
  ),
  # -------------------------------------------------------------------------
  # general footer
  # -------------------------------------------------------------------------
  footer = ui_footer_component()
)

# ends: -------------------------------------------------------------------

