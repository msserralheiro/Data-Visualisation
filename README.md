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

`main` is protected: nobody pushes to it directly. Every change goes through a
pull request (PR) that one teammate approves before it is merged.

1. Get the latest `main` and create a branch for your task:
   ```bash
   git checkout main
   git pull
   git checkout -b your-name/short-description   # e.g. ana/borough-map
   ```
2. Work, then commit small, focused changes with clear messages:
   ```bash
   git add R_Scripts/my_script.R
   git commit -m "Add monthly complaints by borough plot"
   ```
3. Push your branch and open a PR on GitHub:
   ```bash
   git push -u origin your-name/short-description
   ```
   GitHub shows a **Compare & pull request** button — click it, describe the change.
4. A teammate reviews and approves; then merge the PR on GitHub
   (the branch is deleted automatically).
5. Back on your machine, update `main` before starting the next task:
   ```bash
   git checkout main
   git pull
   ```

Tips:
- Pull `main` often and keep branches short-lived to avoid merge conflicts.
- Avoid two people editing the same script at the same time.
- Don't commit data files or RStudio session files — `.gitignore` handles this.
