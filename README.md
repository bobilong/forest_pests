# Forest Pests Impact on Ecosystem Services

This repository contains R scripts for analyzing the spatial distribution and impact of forest pests on key ecosystem services: timber production, biodiversity (diversity), and carbon sequestration. The analysis includes mapping pest enrichment patterns, identifying hotspots for native and non-native species, assessing spatial autocorrelation, and interpreting model predictions using SHAP (SHapley Additive exPlanations) values.

## Overview

This project uses spatial data and statistical modeling to:

- Visualize pest species richness and the proportion of native vs. non-native species across global forests.
- Identify hotspots where pest impacts overlap with ecosystem service provision.
- Analyze spatial patterns and correlations using Moran I statistics.
- Explain machine learning model predictions for pest impacts on ecosystem services.

The scripts generate various figures, including maps, Venn diagrams, heatmaps, and SHAP plots, as presented in the associated research paper.

## Files

- `fig1.R`: Generates maps showing pest species enrichment and native/non-native composition differences.
- `fig2.R`: Creates Venn diagrams illustrating overlaps between native and non-native pest hotspots across ecosystem services, along with bar plots of exposure types by country.
- `fig3.R`: Produces heatmaps of Moran I spatial autocorrelation values categorized by country, ecosystem service, and native/non-native status.
- `fig4.R`: Plots SHAP explanations for model predictions, including curves and boxplots for continuous and categorical variables across timber, diversity, and carbon services.
- `supFig.R`: Contains supplementary figures, including percentile maps of enrichment, standardized reporting rate maps, residual boxplots by income group, forest cover plots, exposure area bar plots, and additional SHAP visualizations.

## Dependencies

The scripts require the following R packages:

- `sf`
- `ggplot2`
- `viridis`
- `patchwork`
- `ggvenn`
- `raster`
- `terra`
- `dplyr`
- `tidyr`
- `purrr`
- `readr`
- `stringr`
- `forcats`
- `scales`
- `ggtext`
- `cowplot`
- `ggrepel`
- `shapviz`
- `pheatmap`
- `RColorBrewer`
- `gridExtra`

Install them using:

```r
install.packages(c("sf", "ggplot2", "viridis", "patchwork", "ggvenn", "raster", "terra", "dplyr", "tidyr", "purrr", "readr", "stringr", "forcats", "scales", "ggtext", "cowplot", "ggrepel", "shapviz", "pheatmap", "RColorBrewer", "gridExtra"))
```

## Data

The scripts assume the presence of specific data files in the working directory or specified paths:

- Raster files: `native_hotspot.tif`, `non_native_hotspot.tif`, `enrichment_raster.tif`, etc.
- CSV files: SHAP results (e.g., `shap_timber.csv`, `shap_diversity.csv`, `shap_carbon.csv`), Moran I data, and other tabular data.
- Shapefiles: Global administrative boundaries and forest cover data.

Ensure all required data files are available before running the scripts.

## Usage

1. Set your working directory to the project folder.
2. Load the necessary packages.
3. Run the scripts in order (e.g., start with `fig1.R` for base maps).

## Results

- **Figure 1**: Maps of pest enrichment and native/non-native proportions.
- **Figure 2**: Venn diagrams and bar plots of hotspot overlaps and country-level exposures.
- **Figure 3**: Heatmaps of spatial autocorrelation.
- **Figure 4**: SHAP plots explaining model predictions.
- **Supplementary Figures**: Additional visualizations for enrichment percentiles, reporting rates, residuals, forest cover, and exposures.


## Citation


## Contact

For questions or issues, please open an issue on GitHub.