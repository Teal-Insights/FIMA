# starts: -----------------------------------------------------------------
ui_footer_component <- function() {
  tags$footer(
    class = "custom-footer",
    tags$div(
      class = "footer-content",
      tags$a(
        class = "footer-link logo-link",
        href = "https://www.ssdh.net/",
        target = "_blank",
        tags$img(
          src = "ssdh_logo.svg",
          class = "header-logo"
        )
      ),
      tags$a(
        class = "footer-link text-link",
        href = "#",
        "Teal Insights"
      ),
      span(
        class = "footer-divider",
        lubridate::year(Sys.Date())
      )
    )
  )
}
# ends: -------------------------------------------------------------------
