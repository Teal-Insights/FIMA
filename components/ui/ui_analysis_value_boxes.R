
# start: ------------------------------------------------------------------
create_custom_value_box <- function(title, output_id, output_text, bg_color = "#524f4e") {
  div(
    class = "custom-value-box",
    style = paste0(
      "background-color: ", 
      bg_color, 
      "; color: white; border-radius: 8px; overflow: hidden; height: 100%; width: 100%; ",
      "display: flex; flex-direction: column;"
    ),
    # Header
    div(
      class = "custom-value-box-header",
      style = "padding: 10px; background-color: rgba(0,0,0,0.1); font-weight: bold; font-size: 15px;",
      HTML(gsub("\n", "<br>", title))
    ),
    # Reactable output as its own div
    if (!is.null(output_id)) {
      div(
        class = "reactable-container",
        style = "flex-grow: 1; overflow: auto; width: 100%;",
        reactable::reactableOutput(output_id)
      )
    },
    # Text output as its own div, directly at the box bottom
    div(
      class = "text-output-container",
      style = "padding: 2px 10px; font-weight: bold; text-align: left;",
      textOutput(outputId = output_text)
    )
  )
}

# Value boxes list for UI
vbs <- list(
  create_custom_value_box("Credit Rating", "vb_cra", "vb_cra_text"),
  create_custom_value_box("Debt, % of GDP", "vb_debt", "vb_debt_text"),
  create_custom_value_box("Nominal GDP Growth (%)", "vb_ngdp_growth", "vb_ngdp_growth_text"),
  create_custom_value_box("Interest % of Revenue", "vb_interest", "vb_interest_text"),
  create_custom_value_box("Primary Balance, % of GDP", "vb_pb", "vb_pb_text")
)
# end: --------------------------------------------------------------------


