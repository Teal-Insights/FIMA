
# start: ------------------------------------------------------------------
create_custom_value_box <- function(title, output_id, bg_color = "#524f4e") {
  div(
    class = "custom-value-box",
    style = paste0("background-color: ", bg_color, "; color: white; border-radius: 8px; overflow: hidden; height: 100%; width: 100%;"),
    div(
      class = "custom-value-box-header",
      style = "padding: 10px 15px; background-color: rgba(0,0,0,0.1); font-weight: bold; font-size: 16px;",
      HTML(gsub("\n", "<br>", title))
    ),
    div(
      class = "custom-value-box-content",
      style = "padding: 0; margin: 0; height: calc(100% - 40px);",
      reactable::reactableOutput(output_id)
    )
  )
}

# Then you can create your list like this:
vbs <- list(
  create_custom_value_box("Credit Rating", "vb_cra"),
  create_custom_value_box("Debt, % of NGDP", "vb_debt"),
  create_custom_value_box("NGDP Growth (%)", "vb_ngdp_growth"),
  create_custom_value_box("Interest % of Revenue", "vb_interest"),
  create_custom_value_box("Primary Balance, % of NGDP", "vb_pb")
)

# end: --------------------------------------------------------------------


