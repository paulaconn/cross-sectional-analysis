# cross-sectional-analysis
This script tests pre and post quantitative data for NSF Ethics.

## Files:
* cross-sectional-analysis.r: automates the following tasks:
1. Filters conditions that are not tested in the grant
2. Formats data for multiple types of analyses, collecting the appropriate between or within subjects data.
3. Checks the distribution of the measures through 36 Shapiro-Wilk tests.
4. Plots Kernel Density Plots to show the distribution of the measures.
5. Runs 9 Kruskal-Wallis tests to test the differences between the conditions for each measure.
6. Runs 36 Wilcoxon Signed Rank Tests to identify the short term within-subjects changes.
7. Runs additional custom tests to make the results more intuitive for the reader: 1 Kruskal Wallis test not including non-significant conditions from item 6 above, and 1 Mann-Whitney U test.

## Requirements:
* R with libraries dplyr, tydyr, and ggplot installed.
* Data_2016_2018.csv: Contains all composite score data per participant (omitted for participant's privacy)

## Outputs:
* 9 Kernel density plots
* Statistical analysis results are written to console.

## Notes:
Please do not upload any participant data to Github.