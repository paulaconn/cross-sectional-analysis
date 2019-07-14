#
# Analysis of pre/post survey responses comparing between and within subjects.
# Requires libraries below, and Data_2016_2018.csv
#
# By Paula 
#

library(dplyr)
library(tidyr) #for drop_na() function
library(ggplot2) #for Kernel density plots

formatConditions <- function(filename, lst_analysis) {
  # Reads data and selects conditions needed for analysis.
  # 
  df <- read.csv(filename, header = TRUE, sep = ",")
  df <- df %>% dplyr::filter(MATCHED.PRE...POST == "YES", 
                             (!Final.Condition %in% c("**REVIEW**", "Baseline")))
  return(df)
}

formatData <- function(input_df, lst_analysis, isPaired) {
  # Formats data for analysis by outputting the results in a list, containing
  # dataframes for each measure (voting, etc.) with the conditions outlined
  # in each row. To print, type: 'lst_data[[1]]' into the console.
  iter_results <- list()
  lst_formatted_data <- lapply(lst_analysis, function(x) {
    if(!isPaired){
      x <- paste0(x, ".POST.PRE")
    }
    else {
      x <- c(paste0(x, ".PRE"), paste0(x, ".POST"))
    }
    lst_formatted_data <- input_df %>% dplyr::select(Final.Condition, x) %>% drop_na()
    })
  return(lst_formatted_data)
}

checkDistribution <- function(lst_formatted) {
  # Tests distribution with the Shapiro-Wilk test and outputs a Kernel Density plot
  # Null hypothesis: Sample comes from a normal distribution
  # 
  colorblind_colors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                         "#0072B2", "#D55E00", "#CC79A7")
  lapply(lst_formatted, function(x){
    score_id <- as.symbol(colnames(x[2])) #For tidy Quasiquotation
    cat("\nShapiro-Wilk ", score_id, " \n")
    result <- x %>% group_by(Final.Condition)  %>% summarize(
      count = n(),
      #tidy Quasiquotation to send string not variable name
      average := mean(!!score_id),
      statistic := ifelse(sd(!!score_id) != 0, shapiro.test(!!score_id)$statistic, NA),
      p.value = ifelse(sd(!!score_id) != 0, shapiro.test(!!score_id)$p.value, NA)
    )
    print(result)
    plotDistribution(x, colorblind_colors)
  })
}

plotDistribution <- function(df, colorblind_colors){
  # Plots a Kernel Density plot, called by checkDistribution()
  # 
  score_label <- sub("[[:punct:]](.*)","", colnames(df[2]))
  png(paste0(score_label, "_distribution.png"))
  label <- paste(score_label, "Change Score")
  density_plot <- plot(ggplot(df, aes(x = df[[2]], fill = df[[1]])) 
       + geom_density(alpha = 0.5) 
       + xlab(label)
       + scale_fill_manual(values = colorblind_colors)
       + theme_classic()
       + theme(legend.title=element_blank()))
  print(density_plot)
  dev.off()
}

runKruskalWallis <- function(lst_formatted) {
  # Runs Kruskal-Wallis for all the measures
  # Null hypothesis: Samples are from identical distributions
  # 
  lapply(lst_formatted, function(x){
    print(kruskal.test(formula(paste(colnames(x[2]), "~ as.factor(Final.Condition)")),
                       data = x))
  })
}

runWilcox <- function(lst_formatted) {
  # Runs Wilcoxon Signed Rank tests for all the measures. Uses tidy's Quasiquotation
  # to send strings not variable names (e.g., '!!pre' and summarize() vs. dplyr's
  # summarise() function)
  # Null hypothesis: Samples have (approximately) equal medians 
  #
  lapply(lst_formatted, function(x){
    pre <- as.symbol(colnames(x[2])) #For tidy Quasiquotation
    post <- as.symbol(colnames(x[3])) #For tidy Quasiquotation
    cat("\nWilcoxon ", pre, "and POST \n")
    result <- x %>% group_by(Final.Condition)  %>% summarize(
      count = n(),
      mdn_pre := median(!!pre),
      IQR_pre := IQR(!!pre),
      mdn_post := median(!!post),
      IQR_post := IQR(!!post),
      statistic := wilcox.test(!!pre, !!post, paired = TRUE, exact = FALSE)$statistic,
      p.value = wilcox.test(!!pre, !!post, paired = TRUE, exact = FALSE)$p.value,
      z.score = qnorm(p.value),
      effect := abs(z.score)/sqrt(count)
    )
    print(result)
  })
}

######################
# MAIN EXECUTION
######################

lst_col_analysis <- c("Voting.composite", "Sympathy.composite",
                      "Vulnerability.composite", "Fear.composite",
                      "Uncertainty.composite", "Coping.composite",
                      "Discomfort.composite", "Awareness.composite",
                      "Knowledge.composite")
original <- formatConditions("Data_2016_2018.csv", lst_col_analysis)
lst_data <- formatData(original, lst_col_analysis, FALSE)
wilcox_data <- formatData(original, lst_col_analysis, TRUE)
checkDistribution(lst_data)
runKruskalWallis(lst_data)
runWilcox(wilcox_data)

######################
# ADDT CUSTOM TESTS
######################
" Below are additional tests that were requested to make the writing of the results more
  intuitive and easy to follow for the reader. Kruskal-Wallis for voting is conducted
  on the measures that showed significance in paired testing (all except Team Member),
  and a Mann-Whitney U test is conducted on Project and End User for Awareness scores." 

cat('\n========= CUSTOM TESTS ============')
custom_kw_data <- lst_data[[1]] %>% filter(!stringr::str_detect(Final.Condition, 
                                                                'Team Member'))
print(kruskal.test(Voting.composite.POST.PRE ~ Final.Condition, data = custom_kw_data))

# Null hypothesis for the Mann-Whitney U Test is that the sample populations
# have (approximately) equal distributions.
custom_mw_data_x <- lst_data[[8]] %>% filter(stringr::str_detect(
  Final.Condition, 'Project')) %>% select(Awareness.composite.POST.PRE)
custom_mw_data_y <- lst_data[[8]] %>% filter(stringr::str_detect(
  Final.Condition, 'End User')) %>% select(Awareness.composite.POST.PRE)
custom_mw_data_x <- as.numeric(as.character(unlist(custom_mw_data_x)))
custom_mw_data_y <- as.numeric(as.character(unlist(custom_mw_data_y)))
N <- length(custom_mw_data_x) + length(custom_mw_data_y)
  
mw_result <- wilcox.test(custom_mw_data_x, custom_mw_data_y, paired = FALSE)
print(mw_result)
cat('Effect Size:', abs(qnorm(mw_result$p.value))/sqrt(N))
