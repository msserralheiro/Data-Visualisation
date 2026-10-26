# Data Visualisation — Group Project

Visual analysis of NYPD complaint data (2020–2026).

## Repository layout

| Folder | Contents |
|---|---|
| `R_Scripts/` | R scripts (data loading, cleaning, analysis) |
| `Data/` | Data files — **not tracked by git**, see below |
| `Figures/` | Exported plots |
| `Dashboard/` | Dashboard code |
| `Documents/` | Report, slides, notes |

## Getting started

1. Clone the repo:
   ```bash
   git clone https://github.com/msserralheiro/Data-Visualisation.git
   ```
2. Download `nypd_2020_2026.rds` from the course page on Moodle
   and put it in `Data/` (so the path is `Data/nypd_2020_2026.rds`).
3. Open `DV_Group_Project.Rproj` in RStudio (this sets the working directory
   so the relative paths in the scripts work).
4. Install the packages used: `install.packages("dplyr")`
5. Run `R_Scripts/DV_Data_Loading.R`.

## Working together

- Pull before you start: `git pull`
- Commit small, focused changes with clear messages.
- Don't commit data files or RStudio session files — `.gitignore` handles this.
