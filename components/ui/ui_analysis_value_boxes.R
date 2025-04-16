
# start: ------------------------------------------------------------------
# Custom value box function
create_custom_value_box <- function(title, output_id, output_text, bg_color = "#524f4e") {
  div(
    class = "custom-value-box",
    style = paste0(
      "background-color: ", 
      bg_color, 
      "; color: white; border-radius: 8px; overflow: hidden; height: 100%; width: 100%;"
    ),
    div(
      class = "custom-value-box-header",
      style = "padding: 10px; background-color: rgba(0,0,0,0.1); font-weight: bold; font-size: 15px;",
      HTML(gsub("\n", "<br>", title))
    ),
    div(
      class = "custom-value-box-content",
      style = "padding: 0; margin: 0; height: calc(100% - 40px); display: flex; flex-direction: column;",
      # Include the reactable output if it's provided (with no padding)
      if (!is.null(output_id)) {
        div(
          style = "flex-grow: 1; overflow: hidden; width: 100%;",
          reactable::reactableOutput(output_id)
        )
      },
      # Include the text output below
      div(
        style = "padding: 10px; font-weight: bold; text-align: left;",
        textOutput(outputId = output_text)
      )
    )
  )
}

# Value boxes list for UI
vbs <- list(
  create_custom_value_box("Credit Rating", "vb_cra", "vb_cra_text"),
  create_custom_value_box("Debt, % of NGDP", "vb_debt", "vb_debt_text"),
  create_custom_value_box("NGDP Growth (%)", "vb_ngdp_growth", "vb_ngdp_growth_text"),
  create_custom_value_box("Interest % of Revenue", "vb_interest", "vb_interest_text"),
  create_custom_value_box("Primary Balance, % of NGDP", "vb_pb", "vb_pb_text")
)
# end: --------------------------------------------------------------------


