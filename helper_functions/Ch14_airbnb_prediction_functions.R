price_diff_by_variables <- function(df, factor_var, dummy_var){
  # Looking for interactions.
  # It is a function it takes 3 arguments: 1) Your dataframe, 
  # 2) the factor variable (like room_type) 
  # 3)the dummy variable you are interested in (like TV)
  
  # Process your data frame and make a new dataframe which contains the stats
  require("dplyr")
  require("ggplot2")
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)
  
  stats <- df %>%
    group_by_(factor_var, dummy_var) %>%
    summarize(Mean = mean(price, na.rm=TRUE),
              se = sd(price)/sqrt(n()))
  
  stats[,2] <- lapply(stats[,2], factor)
  
  ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position = position_dodge(width=0.9))+
    geom_errorbar(aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)), 
                  position=position_dodge(width = 0.9), width = 0.25)+
    ylab('Mean Price')+
    theme_bw()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line()) +
    scale_fill_grey()  
}