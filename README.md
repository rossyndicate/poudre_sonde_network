# Poudre Sonde Network

This repository contains the data processing, calibration, verification, and reporting workflows for the Poudre Sonde Network (PSN). The PSN of water quality sensors ("sondes") deployed along the Poudre River and related sites. This repo focuses on all sonde data collected across three projects:

- **Poudre Water Quality Network (PWQN)**: The PWQN is a collection of river monitoring sites strategically located throughout and directly upstream of the City of Fort Collins. Established in 2019, this network represents a unique public-private partnership between the City’s storm water and water quality divisions, the ROSS lab, and In-Situ Inc., a water monitoring equipment manufacturer based in Fort Collins. This monitoring partnership was spurred by the 2018 fish kill through the city, whose source was never determined. With a network of sensors now deployed, we can better track changes in water quality like those experienced in 2018 and understand how the river is influenced by its myriad of urban and agricultural inputs. [Project Page](https://www.rossyndicate.com/projects/poudre-water-quality-network)
- **Upper CLP DSS**: ROSS work supporting real-time water treatment decision support in the Cache la Poudre River. [GitHub](https://github.com/rossyndicate/upper_clp_dss)
- **ROSS CPF Reservoir Study**: Investigation of post-fire water quality impacts from the Cameron Peak Fire. [GitHub](https://github.com/rossyndicate/cameron_peak_fire) \| [Project Page](https://www.rossyndicate.com/projects/post-fire-water-quality-monitoring)

## Repository Structure

```         
poudre_sonde_network/
├── operations/                   # Day-to-day operational scripts, budgeting, and reporting
│   ├── manager_tools.Rmd           # Scripts for managing the network (sensor tracking, file name generation, mWater photo downloads)
│   ├── reporting/                  # Annual reports, flagging documentation, budget files, presentation materials
│   ├── annual_reports/             # Year-by-year report figure generation (2022–2026)
│   ├── budget/                     # Budget prep and report generation (PWQN, UCLP/FC)
│   └── flagging_descriptions/      # Documentation for each QA/QC flag type
├── data_organization/            # Yearly data processing scripts (2019–2026) and drift correction. Tracks file read in, QAQC, and final saving to HydroShare
├── calibration_correction_tool/   # Shiny app for reviewing and correcting sensor calibration
│   ├── creds/                      # Symlink to `creds`  
│   ├── data/                       # Symlink to `data`
│   └── ui.R, server.R, global.R, R/  # Shiny files
├── manual_verification_tool/     # Shiny app for manual QA/QC verification of sonde data
│   ├── data/                       # Symlink to `data/raw/sensor/manual_data_verification/YEAR_CYCLE/in_progress`
│   ├── field_note_check.R          # Helper for checking field notes against sonde data
│   ├── plot_usgs_flow.R            # Helper for plotting USGS/CDWR flow data
│   └── ui.R, server.R, global.R, R/  # Shiny files
├── src/                            # Core pipeline functions: flagging, plotting, data retrieval, etc
├── scratch/                        # Scripts generated for specific tasks, works-in-progress, or archived workflows. 
├── archive/                       # Older/superseded analysis scripts, kept for reference
│   └── manual_post_verification_v0/   # 2023 manual verification workflow 
└── timelapse/                      # Scripts for generating and organizing site timelapses from cameras
```

## Key Components

- **`src/`** — Core, reusable pipeline functions used across the project: data retrieval from APIs, flagging logic, plot generation, photo/timelapse compilation, and station metadata management. NOTE: Many of these have been migrated to `rossyndicate/ross.wq.tools` for broader use across the ROSSyndicate projects
- **`data_organization/`** — Year-specific scripts for importing, tidying, and drift-correcting raw sonde data.
- **`calibration_correction_tool/`, `manual_verification_tool/`** — Interactive Shiny apps supporting different stages of the QA/QC workflow (calibration review, manual verification).
- **`operations/`** — Reporting, and administrative workflows, including annual report generation and flag documentation.
- **`archive/`** — Deprecated workflows retained for reference.

## License:

The code in this repository is covered by the MIT use license ( [LICENSE](LICENSE) ). We request that all downstream uses of this work be available to the public when possible.
