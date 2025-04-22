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

# -------------------------------------------------------------------------
# Overview content
# -------------------------------------------------------------------------
overview_content <- "
  <h5>Development Status</h5>
  <p>
    The FIMA Explorer is currently a <strong>prototype stage</strong>. This means:
  </p>
  <ol>
    <li><strong>Illustrative Data</strong>: All countries, vulnerabilities, and impact values are for illustrative purposes only. The three demo countries are fictional and their data was modeled according to the description provided in the About box (Home tab).</li>
    <li><strong>Simplified Modeling</strong>: The relationships between interventions and macroeconomic outcomes are based on generalized assumptions rather than rigorously calibrated models.</li>
    <li><strong>Conceptual Workflow</strong>: We want to test whether this is the best design for our users. We are interested in getting feedback on how to improve this tool. </li>
  </ol>

  <h5>Assessment Framework</h5>
  <p>
    The FIMA Explorer is designed as a first-step assessment tool that:
  </p>
  <ol>
    <li><strong>Maps vulnerabilities</strong> to identify relevant climate and nature KPIs</li>
    <li><strong>Links interventions</strong> to these KPIs</li>
    <li><strong>Estimates potential impacts</strong> on key macrofiscal variables</li>
    <li><strong>Projects effects</strong> on debt sustainability and credit metrics</li>
  </ol>

  <h5>Development Roadmap</h5>
  <p>
    This tool represents the first step in a multi-phase approach:
  </p>
  <ol>
    <li><strong>Current phase</strong>: Illustrative prototype with representative examples</li>
    <li><strong>Next phase</strong>: Integration of generalized and empirically-based intervention impact models</li>
    <li><strong>Future phase</strong>: Country-specific customization and calibration</li>
    <li><strong>Final phase</strong>: Full integration with DSA frameworks and credit rating methodologies</li>
  </ol>

  <h5>Understanding the Results</h5>
  <p>
    Even in its final form, results from this tool should be interpreted as:
  </p>
  <ul>
    <li><strong>Directional guidance</strong> on which interventions may offer the greatest credit-relevant benefits</li>
    <li><strong>Starting points</strong> for more detailed economic modeling</li>
    <li><strong>Order-of-magnitude estimates</strong> rather than precise predictions</li>
  </ul>

  <h5>Providing Feedback</h5>
  <p>
    Your insights are crucial to ensuring this tool evolves to meet real analytical needs. We're particularly interested in:
  </p>
  <ul>
    <li>Is the conceptual workflow intuitive and valuable?</li>
    <li>What additional features would make this tool more useful for your work?</li>
    <li>What metrics and interventions should be prioritized in future development?</li>
    <li>How could this tool complement your existing analytical processes?</li>
  </ul>
"

# -------------------------------------------------------------------------
# Key equation content
# -------------------------------------------------------------------------
key_equation_content <- "
  <h5>Debt Dynamics Equation:</h5>
  <div class='equation-container'>
    \\[ b_{t+1} = \\frac{(1 + r_t)}{(1 + g_t)}b_{t} - S_{t+1} \\]
  </div>

  <h5>Where:</h5>
  <ul>
    <li>\\( b_t \\) = Debt as % of GDP at time t</li>
    <li>\\( b_{t+1} \\) = Debt as % of GDP at time t + 1</li>
    <li>\\( r_t \\) = Nominal interest rate (%) at time t</li>
    <li>\\( g_t \\) = Nominal GDP growth (%) at time t</li>
    <li>\\( S_{t+1} \\) = Primary balance as % of GDP at time t + 1</li>
  </ul>"

# -------------------------------------------------------------------------
# Methodology content
# -------------------------------------------------------------------------
methodology_content <- "
  <p>
    The computation process for debt dynamics analysis is grounded on four 
    variables: nominal GDP growth, general government primary balance, 
    nominal interest rate and general government gross debt. This framework 
    enables researchers and policymakers to analyze the evolution of public 
    debt over time, taking into account the complex interplay between economic 
    growth, interest rates and fiscal policy decisions.
  </p>

  <h5>Key Equations:</h5>
  <h6>1. Main Debt Dynamics Equation:</h6>
  <div class='equation-container'>
    \\[ \\tag{1} b_{t+1} = \\frac{(1 + r_t)}{(1 + g_t)}b_{t} - S_{t+1} \\]
  </div>

  <h6>3. Policy-Adjusted Forecast (%) Calculations:</h6>
  <div class='equation-container policy-eq-fix'>
    \\[ \\tag{3a} r_t^{\\text{Adj}} = r_t^{\\text{Base}} + r_t^{\\text{Shock}} \\]
    \\[ \\tag{3b} g_t^{\\text{Adj}} = g_t^{\\text{Base}} + g_t^{\\text{Shock}} \\]
    \\[ \\tag{3c} S_{t+1}^{\\text{Adj}} = S_{t+1}^{\\text{Base}} + S_{t+1}^{\\text{Shock}} \\]
  </div>

  <h5>Where:</h5>
  <ul>
    <li><strong>Base</strong> = Baseline (%) is the trajectory of the variable without any shocks</li>
    <li><strong>Shock</strong> = Policy shock (%) represents the impact of a specific intervention or instrument</li>
    <li><strong>Adj</strong> = Policy-adjusted forecast (%) after applying policy shocks</li>
    <li>\\( b_t \\) = Debt as % of GDP at time t</li>
    <li>\\( b_{t+1} \\) = Debt as % of GDP at time t + 1</li>
    <li>\\( r_t \\) = Nominal interest rate (%) at time t</li>
    <li>\\( g_t \\) = Nominal GDP growth (%) at time t</li>
    <li>\\( S_{t+1} \\) = Primary balance as % of GDP at time t + 1</li>
    <li>Policy-Adjusted Forecast (%) values are the final values after applying policy shocks</li>
    <li>Baseline (%) values are the initial projected values</li>
    <li>Policy shock (%) values represent the impact of specific interventions</li>
    <li>
      Policy-adjusted forecast for nominal interest rate (%), nominal GDP 
      growth (yoy%) and primary balance are obtained by summing up all the 
      interventions and instruments (shocks) that affect the variable. For 
      instance, nominal interest will be impacted by financial related 
      interventions from Protection GAP KPI.
    </li>
  </ul>

  <h5>Debt Projection:</h5>
  <p>
    The final projection methodology incorporates policy shocks through 
    equations (3a), (3b), and (3c), where the baseline (%) is adjusted by 
    shocks. These calculations are performed in percentage terms, with the 
    policy-adjusted values representing the sum of baseline (%) plus the policy 
    shocks. The policy-adjusted values are then put into the main debt dynamics 
    equation (1) to generate the debt projection under the Alternative Scenario.
  </p>
"

# Main component function
ui_documentation_component <- function() {
  card_body(
    div(
      class = "container",
      create_section_card("About This Prototype", overview_content),
      create_section_card("Key Equation", key_equation_content),
      create_section_card("Methodology", methodology_content)
    )
  )
}
# ends: -------------------------------------------------------------------