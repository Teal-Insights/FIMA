# starts: -----------------------------------------------------------------
# Helper function to create a section card
create_section_card <- function(title, content) {
  card(
    class = "mt-4",
    card_header(class = "bg-light", h4(title)),
    card_body(
      withMathJax(
        HTML(sprintf("<div class='math-section'>%s</div>", content))
      )
    )
  )
}

# Content sections as separate variables for better maintainability

# -------------------------------------------------------------------------
# Overview content
# -------------------------------------------------------------------------
overview_content <- "
  <p>
    The FIMA Explorer helps analyze how innovative policy interventions can 
    affect a country's debt sustainability. It builds upon the IMF's debt dynamics 
    framework to project how debt-to-GDP ratios evolve under different scenarios, 
    with a special focus on climate finance mechanisms and sustainable agriculture 
    initiatives.
  </p>
  
  <h5>How It Works</h5>
  
  <h6><b>1. Starting Point: Baseline</b></h6>
  <ul>
    <li>Nominal GDP growth (%)</li>
    <li>Primary fiscal balance(%)</li>
    <li>Government debt levels (%)</li>
    <li>Nominal Interest rate (%)</li>
  </ul>
  
  <h6><b>2. Policy Adjustments</b></h6>
  <p>
    The main variables: Nominal GDP growth (%), Primary fiscal balance(%) and 
    Nominal Interest rate (%) are adjusted to obtain Policy shock values which 
    are added to the Baseline Scenario values to obtain final values
    (Policy adjusted values).
  </p>
  <ul>
    <li>Nominal GDP growth (%)</li>
    <li>Primary fiscal balance(%)</li>
    <li>Nominal Interest rate (%)</li>
  </ul>
  
  <p>
    The tool allows users to test how implementing specific interventions across 
    Key Performing Indicators might impact these variables:
  </p>
  
  <h6><b>Protection Gap Interventions</b></h6>
  <ul>
    <li><b>Catastrophe Bonds</b>: Model how issuing Catastrophe bonds affects borrowing costs and fiscal resilience</li>
    <li><b>Insurance Premium Subsidies</b>: Analyze fiscal impacts of subsidizing climate risk insurance</li>
    <li><b>Microinsurance</b>: Estimate how expanding coverage reduces contingent liabilities</li>
    <li><b>Cross-border Reinsurance</b>: Evaluate effects of international risk-pooling mechanisms</li>
    <li><b>Compulsory Insurance Coverage</b>: Project impacts of mandatory climate risk coverage</li>
    <li><b>Insurance Bundling</b>: Model efficiency gains from integrated insurance products</li>
    <li><b>Risk-based Solvency Capital Requirements</b>: Assess financial stability improvements</li>
  </ul>
  
  <h6><b>Land Use / Regenerative Agriculture Initiatives</b></h6>
  <ul>
    <li><b>Silvopasture</b>: Calculate productivity gains from integrated forestry-livestock systems</li>
    <li><b>Reduced-Till Farming</b>: Estimate cost savings and yield improvements</li>
    <li><b>Climate-resilient Seeds</b>: Project agricultural output stability during extreme weather</li>
    <li><b>Managed Grazing</b>: Model sustainable livestock production improvements</li>
    <li><b>Biological Fertilization</b>: Analyze cost reductions and export value improvements</li>
    <li><b>Polyculture & Crop Rotation</b>: Assess productivity and input cost impacts</li>
    <li><b>Organic Certification Practices</b>: Evaluate premium pricing and market access benefits</li>
    <li><b>Integrated Pest Management</b>: Calculate input cost reductions and yield stability</li>
  </ul>
  
  <h6><b>3. Resulting Projections</b></h6>
  <p>
    The tool combines baseline values with policy adjustments to show how debt 
    trajectories might change. This helps answer questions like:
  </p>
  <ul>
    <li>How might catastrophe bonds reduce fiscal volatility following natural disasters?</li>
    <li>What debt sustainability improvements could result from agricultural transformation programs?</li>
    <li>How do climate-resilient investments affect long-term growth and borrowing costs?</li>
    <li>Which combination of protection mechanisms and agricultural innovations creates optimal debt sustainability?</li>
  </ul>
  
  <h5>Important Notes</h5>
  <ul>
    <li>This tool provides a simplified but rigorous starting point for analyzing climate finance interventions</li>
    <li>Results should be interpreted as illustrative projections, not precise forecasts</li>
    <li>The model captures direct fiscal effects but may not fully account for all socioeconomic co-benefits</li>
    <li>For detailed country analysis, this tool should complement rather than replace full DSA frameworks</li>
  </ul>
"

# -------------------------------------------------------------------------
# Key equation content
# -------------------------------------------------------------------------
key_equation_content <- "
  <h5>Debt Dynamics Equation:</h5>
  \\[ d_t = \\frac{(1 + r_t)}{(1 + g_t)}d_{t-1} - pb_t \\]

  <h5>Where:</h5>
  <ul>
    <li>\\( d_t \\) = Debt as % of GDP at time t</li>
    <li>\\( d_{t-1} \\) = Debt as % of GDP at time t - 1</li>
    <li>\\( r_t \\) = Nominal interest rate (%) for time t</li>
    <li>\\( g_t \\) = Nominal GDP growth (%) for time t</li>
    <li>\\( pb_t \\) = Primary balance as % of GDP at time t</li>
    <li>\\( t \\) = time period</li>
  </ul>"

# -------------------------------------------------------------------------
# Methodology content
# -------------------------------------------------------------------------
methodology_content <- "
  <p>The computation process for debt dynamics analysis is fundamentally grounded 
  in three essential variables: Gross domestic product, current prices, General 
  government primary net lending/borrowing (primary balance), Nominal interest rate (%) and General 
  government gross debt. This comprehensive framework enables researchers and 
  policymakers to analyze the evolution of public debt over time, taking into 
  account the complex interplay between economic growth, interest rates, and 
  fiscal policy decisions. The methodology's strength lies in its ability to 
  decompose debt dynamics into its constituent components, allowing for a 
  detailed understanding of how different macroeconomic factors contribute to 
  changes in the debt-to-GDP ratio.</p>

  <h5>Key Equations:</h5>
  <h6>1. Main Debt Dynamics Equation:</h6>
  \\[ \\tag{1} d_t = \\frac{1 + r_t}{1 + g_t}d_{t-1} - pb_t \\]

  <h6>3. Policy-Adjusted Forecast (%) Calculations:</h6>
  \\[ \\tag{3a} r_t^{\\text{Policy-Adjusted Forecast (%)}} = r_t^{\\text{Baseline (%)}} + r_t^{\\text{Policy shock (%)}} \\]
  \\[ \\tag{3b} g_t^{\\text{Policy-Adjusted Forecast (%)}} = g_t^{\\text{Baseline (%)}} + g_t^{\\text{Policy shock (%)}} \\]
  \\[ \\tag{3c} pb_t^{\\text{Policy-Adjusted Forecast (%)}} = pb_t^{\\text{Baseline (%)}} + pb_t^{\\text{Policy shock (%)}} \\]

  <h5>Where:</h5>
  <ul>
    <li>\\( d_t \\) = Public-debt-to-GDP ratio at time t</li>
    <li>\\( r_t \\) = Nominal interest rate (%)</li>
    <li>\\( g_t \\) = Nominal GDP growth (yoy%)</li>
    <li>\\( pb_t \\) = Primary-Balance-to-GDP ratio</li>
    <li>\\( r_t^{\\text{Policy-Adjusted Forecast (%)}}, g_t^{\\text{Policy-Adjusted Forecast (%)}}, pb_t^{\\text{Policy-Adjusted Forecast (%)}} \\) = Policy-Adjusted Forecast (%) values</li>
    <li>\\( r_t^{\\text{Baseline (%)}}, g_t^{\\text{Baseline (%)}}, pb_t^{\\text{Baseline (%)}} \\) = Baseline (%) values</li>
    <li>\\( r_t^{\\text{Policy shock (%)}}, g_t^{\\text{Policy shock (%)}}, pb_t^{\\text{Policy shock (%)}} \\) = Policy shock (%) values</li>
    <li>Policy shocks for Nominal interest rate (%),Nominal GDP growth (yoy%) and primary balance 
      are obtained by summing up all the interventions that affect a give 
      variable. For instance, Nominal interest will be impacted by financial
      related interventions from Protection GAP KPI.
    </li>
  </ul>

  <h5>Debt Projection:</h5>
  <p>The final projection methodology incorporates policy shocks through 
  equations (3a), (3b), and (3c), where Baseline (%) values for Nominal interest 
  rate (%),Nominal GDP growth (yoy%) and primary balance are adjusted by 
  shock values. These shock calculations are performed in percentage terms, 
  with the final shocked values representing the sum of Baseline (%) and policy 
  shock values for each respective variable. The resulting final 
  values (\\(r_t^{\\text{Policy-Adjusted Forecast 
  (%)}}, g_t^{\\text{Policy-Adjusted Forecast (%)}}, 
  pb_t^{\\text{Policy-Adjusted Forecast (%)}}\\)) are then input into the main 
  debt dynamics equation (1) to generate the debt projection under the specified 
  Alternative Scenario.</p>"

# Main component function
ui_documentation_component <- function() {
  card_body(
    div(
      class = "container",
      create_section_card("Overview", overview_content),
      create_section_card("Key Equation", key_equation_content),
      create_section_card("Methodology", methodology_content)
    )
  )
}

# ends: -------------------------------------------------------------------
