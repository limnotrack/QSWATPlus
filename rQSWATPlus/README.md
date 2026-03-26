# rQSWATPlus

R interface to the [QSWATPlus](https://github.com/limnotrack/QSWATPlus) workflow for setting up [SWAT+](https://swat.tamu.edu/software/plus/) hydrological models.

## Overview

`rQSWATPlus` provides R functions to replicate the QSWATPlus QGIS plugin workflow for SWAT+ model setup. It handles:

- **Watershed delineation** from DEMs using TauDEM (via [`traudem`](https://github.com/lucarraro/traudem/))
- **HRU creation** from land use, soil, and slope overlays
- **Stream network** topology extraction
- **Database output** (SQLite) compatible with the SWAT+ Editor

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("limnotrack/QSWATPlus", subdir = "rQSWATPlus")
```

### Prerequisites

TauDEM must be installed on your system. See [`traudem` installation guide](https://lucarraro.github.io/traudem/articles/taudem-installation.html).

```r
# Check TauDEM installation
traudem::taudem_sitrep()
```

## Quick Start

```r
library(rQSWATPlus)

# Complete workflow in one call
project <- qswat_run(
  project_dir = "my_project",
  dem_file = "dem.tif",
  landuse_file = "landuse.tif",
  soil_file = "soil.tif",
  landuse_lookup = "landuse_lookup.csv",
  soil_lookup = "soil_lookup.csv",
  outlet_file = "outlets.shp",
  threshold = 500,
  slope_breaks = c(0, 5, 15, 9999)
)
```

Or step by step:

```r
# 1. Setup
project <- qswat_setup(project_dir, dem_file, landuse_file,
                        soil_file, landuse_lookup, soil_lookup)

# 2. Delineate watershed
project <- qswat_delineate(project, threshold = 500)

# 3. Extract stream network
project <- qswat_create_streams(project)

# 4. Create HRUs
lu <- qswat_read_landuse_lookup("landuse_lookup.csv")
soil <- qswat_read_soil_lookup("soil_lookup.csv")
slopes <- qswat_create_slope_classes(c(0, 5, 15, 9999))
project <- qswat_create_hrus(project, lu, soil, slopes)

# 5. Write database
qswat_write_database(project)
```

## Example Data

The package includes example data from the Ravn watershed (Denmark):

```r
dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
outlet <- system.file("extdata", "ravn_outlet.shp", package = "rQSWATPlus")
```

## Input Data Requirements

| Input | Format | Description |
|-------|--------|-------------|
| DEM | GeoTIFF | Digital elevation model (projected CRS) |
| Land use | GeoTIFF | Integer land use classification raster |
| Soil | GeoTIFF | Integer soil map unit raster |
| Land use lookup | CSV | Maps raster values → SWAT+ land use codes |
| Soil lookup | CSV | Maps raster values → soil names |
| Outlets | Shapefile | Optional watershed outlet points |

See `vignette("data-requirements", package = "rQSWATPlus")` for detailed specifications.

## Vignettes

- `vignette("introduction")` - Getting started guide
- `vignette("data-requirements")` - Input data format specifications
- `vignette("workflow")` - Complete worked example with the Ravn dataset

## Dependencies

- [terra](https://rspatial.github.io/terra/) - Raster data handling
- [sf](https://r-spatial.github.io/sf/) - Vector/shapefile processing
- [RSQLite](https://rsqlite.r-dbi.org/) - SQLite database output
- [traudem](https://github.com/lucarraro/traudem/) - TauDEM interface

## License

GPL (>= 3)
