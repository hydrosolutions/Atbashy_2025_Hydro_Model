# Atbashy River Basin Hydrological Modeling

![Hydrological Modeling](https://github.com/hydrosolutions/atbashy-model/workflows/header_image.png)

## Project Overview

This repository contains a comprehensive hydrological modeling framework for simulating discharge in the Atbashy River Basin, Kyrgyzstan. The project focuses on using the airGR package to model hydrological processes, with specific consideration for the mountainous Central Asian environment including snow and glacier melt contributions.

### Key Features

- **Elevation Band Modeling**: Accounts for orographic effects on precipitation and temperature using multiple elevation bands
- **Snow Dynamics**: Implements the CemaNeige snow model for snow accumulation and melt processes
- **Glacier Contribution**: Incorporates glacier melt data based on Rounce et al. (2023) projections
- **Climate Scenarios**: Supports running multiple climate change scenarios for future projections
- **Monthly Calibration**: Specialized functions for calibration with monthly discharge observations

## Project Structure

```
.
├── 01_code/
│   ├── functions/            # Core modeling and utility functions
│   ├── config_param.R        # Centralized configuration settings
│   └── hydro_runs_model_airGR.qmd  # Main modeling script
├── 02_data/
│   ├── CHELSA_V21/           # Historical climate observations
│   ├── Central_Asia_Domain/  # Climate model data (historical and future)
│   ├── GIS/                  # Spatial data files
│   ├── Glaciers/             # Glacier data
│   └── Discharge/            # River discharge measurements
├── 05_models/                # Model output storage
├── 06_figures/               # Generated figures and visualizations
└── README.md                 # This file
```

## Getting Started

### Prerequisites

- R (>= 4.0.0)
- Required R packages:
  - airGR (for hydrological modeling)
  - tidyverse (for data manipulation)
  - lubridate (for date handling)
  - sf and terra (for spatial data processing)
  - timetk and ggplot2 (for visualization)

### Installation

1. Clone this repository:
```
git clone https://github.com/[username]/atbashy-river-basin-model.git
cd atbashy-river-basin-model
```

2. Install required R packages:
```R
install.packages(c("pacman", "here", "tidyverse", "lubridate", "airGR", "sf", "terra", "timetk", "ggplot2"))
```

3. Setup data directories:
```
mkdir -p 02_data/CHELSA_V21 02_data/Central_Asia_Domain 02_data/GIS 02_data/Glaciers 02_data/Discharge 05_models 06_figures
```

## Core Functions Documentation

The framework includes specialized functions for different aspects of the hydrological modeling process:

### Data Processing Functions

- `process_rsm_data.R`: Processes Regional Statistical Model data by separating temperature and precipitation measurements
- `loadTabularData.R`: Loads csv files with tabular hydrometeorological data where years are in rows and decades or months are in columns
- `process_glacier_data.R`: Processes glacier melt data for multiple climate models and scenarios
- `process_glacier_runoff.R`: Processes glacier runoff data for RS MINERVE format
- `disaggregate_glacier_melt.R`: Converts monthly glacier melt values to daily values

### Modeling Functions

- `solid_fraction_elevation_layer.R`: Calculates solid precipitation fraction for elevation bands using the Glazirin method
- `run_climate_scenarios.R`: Executes hydrological model simulations for multiple climate scenarios and models
- `calibrate_with_monthly_data.R`: Calibrates hydrological models using monthly observation data
- `ErrorCrit_NSE_Monthly.R`: Custom Nash-Sutcliffe Efficiency criterion for monthly data

### Visualization Functions

- `plot_summary_stats.R`: Creates summary statistics plots for hydrological data
- `plot_seasonal_diagnostics.R`: Creates multi-panel visualization of river discharge data
- `plot_exceedance.R`: Creates exceedance probability plots for discharge data
- `create_enhanced_subseries_plot.R`: Creates multi-panel subseries plot showing monthly discharge patterns
- `plot_discharge_timeseries.R`: Creates professional time series plots for river discharge

## Workflow

1. **Configure Project Settings**:
   - Edit `01_code/config_param.R` to set project-specific parameters

2. **Prepare Input Data**:
   - Process climate data (temperature and precipitation)
   - Format discharge observations
   - Process glacier melt data

3. **Run the Model**:
   - Execute `01_code/hydro_runs_model_airGR.qmd` using Quarto or RStudio
   - The script guides through model setup, calibration, validation, and scenario runs

4. **Analyze Results**:
   - Use visualization functions to create figures
   - Export data for further analysis

## Climate Scenarios

The framework supports running simulations for different climate scenarios:

- **Historical Observations**: 1979-1995
- **Future Scenarios**: 2000-2024
- **Calibration Period**: 1979-1990
- **Validation Period**: 1991-1996

Climate models supported:
- ERA5 (reanalysis)

SSP scenarios:
- No SSP scenarios are investigated here.

## Unique Features

### Snow/Rain Partitioning

The model implements the Glazirin method for snow/rain partitioning, calibrated specifically for Central Asian mountain environments:

```R
snow_fraction = solid_fraction_elevation_layer(
  Basin_Info,
  temperature_bands, 
  tas_min = 8.56, 
  tas_max = 9.97
)
```

### Glacier Melt Handling

Glacier melt is integrated through temperature-based disaggregation:

```R
daily_melt = disaggregate_glacier_melt(
  monthly_melt,
  daily_temp,
  method = "temperature"
)
```

### Monthly Calibration

The framework includes a specialized NSE criterion for monthly calibration:

```R
ErrorCrit_NSE_Monthly(
  InputsCrit,
  OutputsModel,
  verbose = TRUE
)
```

## Contributing

Contributions to this project are welcome. Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/new-feature`)
3. Commit your changes (`git commit -am 'Add new feature'`)
4. Push to the branch (`git push origin feature/new-feature`)
5. Create a new Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- The airGR development team for their robust modeling framework

## Contact

For questions or support, please contact hydrosolutions GmbH at siegfried@hydrosolutions.ch
