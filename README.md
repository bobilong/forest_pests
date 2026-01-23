Forest Pest Risk Analysis
This repository contains an R script for analyzing global forest pest distribution patterns, assessing risks to forest services (timber, biodiversity, carbon), and evaluating spatial associations and drivers of pest exposure.

📌 Overview
The code performs:

Global mapping of pest richness (native vs. non-native)

Identification of pest hotspots

Overlap analysis between pest exposure and forest services (timber, biodiversity, carbon)

Spatial autocorrelation analysis (Moran’s I) for pest–service relationships

Machine learning modeling to identify key drivers of pest exposure

Visualization of results using ggplot2, terra, and tidyterra

📦 Dependencies
Ensure the following R packages are installed:

r
library(terra)
library(tidyterra)
library(data.table)
library(ggplot2)
library(paletteer)
library(scales)
library(patchwork)
library(rnaturalearth)
library(spdep)
library(gstat)
library(ggforce)
library(ggrepel)
library(ggbeeswarm)
📁 File Structure
The script expects the following directory structure (adjust paths as needed):

text
/root/autodl-tmp/pests/
├── GBIF/native_nonNative2/          # Pest richness rasters (native/non-native)
├── Curtis_Forestry_10Percent_10km2.tif  # Timber production zones
├── treeBiodiversity.tif             # Tree biodiversity raster
├── restoration/carbon_resample.tif  # Carbon stock raster
├── hasenForestCover_resample.tif    # Forest cover mask
├── overlapResult/                   # Overlap analysis outputs
├── result/                          # Model results (Moran's I, ML)
└── WAEdata_new_y/result779/otherdata/Con_EU_dis.shp  # Country boundaries
🔧 Key Functions
1. patternMap()
Generates pest richness maps and native/non-native ratio maps for a given pest type (all, animals, plants, diseases).

2. addplot()
Creates binary overlap maps between pest hotspots and forest services.

3. addPiePlot()
Produces pie charts showing the proportion of forest service area exposed to different pest risk categories.

4. get_k_moran()
Finds the optimal k-nearest neighbors for Moran’s I bivariate spatial autocorrelation analysis.

5. addPlot() & addShapPlot()
Visualize machine learning feature importance and SHAP values for pest exposure models.

📈 Outputs
The script generates several multi-panel figures:

Fig. 1: Pest richness and native/non-native ratio for all pests, animals, plants, and diseases.

Fig. 2: Overlap between pest hotspots and forest services + pie charts of exposed areas.

Fig. 3: Spatial autocorrelation (Moran’s I) between pests and forest services across selected countries.

Fig. 4: Machine learning feature importance and SHAP dependence plots.

🚀 Usage
Update file paths in the script to match your local directory structure.

Run the script sequentially to generate figures and intermediate files.

Adjust parameters (e.g., color palettes, thresholds) as needed for your analysis.

📝 Notes
The code assumes a global raster resolution of 0.5° (720×360 cells).

Forest cover mask (hasenForestcover) is used to restrict analysis to forested areas.

The Moran’s I analysis is performed for six focal countries/regions: USA, EU, Australia, Canada, China, UK.

Machine learning models are trained separately for native and non-native pests across three forest services.

📄 License
This code is provided for research purposes. Please cite "Global exposure risk of timber production, carbon stocking, and biodiversity to forest pests" if used in publications.
