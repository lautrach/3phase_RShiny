# Optimal Trap Placement (3 Phases)

## Purpose

::: {style="padding: 15px; border-radius: 5px;"}
The primary objective of this application is to provide a comprehensive analysis of mosquito trap placements and identify locations with a high probability of predicting human cases of West Nile Virus (WNV). The project is divided into three distinct phases, each focusing on a unique aspect of trap analysis and optimization.
:::

## Application Usage Guide

This application is structured into three interactive phases, each accessible through its designated tab in the navigation panel on the left. Below is a detailed description of the purpose and methodology of each phase. Visit the respective tabs for more in-depth usage instructions and to explore interactive visualizations.

### **Phase 1: Modeling Historic Trap Performance**

::: {style="padding: 10px;border-radius: 8px;"}
In this phase, a comprehensive analysis of historic trap performance is conducted. A **location map** visualizes the placement of traps, providing a clear spatial context. The performance of each trap is assessed using **sensitivity** and **specificity** values as key metrics, displayed through histograms:

-   **Sensitivity** captures the *true positive rate*, indicating how well the traps detect cases when present.
-   **Specificity** measures the *true negative rate*, reflecting the ability to correctly identify areas without cases.

These metrics help gauge the historical reliability of traps in detecting WNV.
:::

### **Phase 2: Defining Scores for Individual Traps**

::: {style="padding: 10px;  border-radius: 8px;"}
This phase calculates a **performance score** for each trap, which is visualized through an interactive heatmap. The heatmap provides a clear representation of the score distribution across all trap locations, making it easy to identify high and low-performing traps.
:::

### **Phase 3: Identifying Location Characteristics**

::: {style="padding: 10px;  border-radius: 8px;"}
In the final phase, a **Generalized Additive Model (GAM)** is employed to analyze which specific location characteristics are associated with higher trap scores. The following variables are considered in the analysis:

-   **Total Population (TPOP2020)**: Population density around trap locations.
-   **Average Impervious Surface (AVG_IMP)**: Percentage of impervious surfaces like roads and pavements.
-   **Average Canopy Coverage (AVG_CANPY)**: Tree canopy coverage in the vicinity.
-   **HSgradPct**: Percentage of high school graduates.
-   **Family Poverty Rate (FamPovPct)**: Socio-economic indicator of poverty levels.
-   **Land Classifications (Class_21Pct, Class_22Pct, Class_23Pct, Class_24Pct)**: Different categories of land use around the trap.

The resulting plots from this model illustrate the relationships between these factors and trap performance, providing valuable insights into the environmental and socio-economic determinants of high-performing trap locations.
:::

------------------------------------------------------------------------

## **Acknowledgements**

### Citation

::: {style="padding: 10px; border-radius: 5px;"}
<em>Where to place a mosquito trap for West Nile Virus surveillance?</em>\
Anwesha Chakravarti, Bo Li, Dan Bartlett, Patrick Irwin, Rebecca Smith\
<a href="https://arxiv.org/abs/2406.06920" target="_blank">Under Submission</a>
:::

### Funding

::: {style="padding: 10px;  border-radius: 5px;"}
This work was supported by Cooperative Agreement Number **U01CK000651** from the Centers for Disease Control and Prevention. Its contents are solely the responsibility of the authors and do not necessarily represent the official views of the Centers for Disease Control and Prevention or the Department of Health and Human Services.
:::
