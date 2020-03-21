#' @title rdpi
#' 
#' @description Function to compute the RDPI (Relative Distance Plasticity Index,
#' Valladares et al, (2006) Quantitative estimation of phenotypic plasticity:
#'  bridging the gap between the evolutionary concept and its ecological applications,
#'  Journal of Ecology, 94(6):1103-1116.
#'
#' @param dataframe The dataframe that contains the data
#' @param sp The bare (unquoted) name of the column whose values will be used as independent variable.
#'The function will compare RDPI values among values of this variable. It can be species, provenances, etc.
#' @param trait The bare (unquoted) name of the column that holds the trait for which to calculate RDPI. Must be numeric
#' @param factor the bare (unquoted) name of the column that holds the environmental factor for which we will calculate RDPI. 
#' By definition, RDPI computes distances between pairs of observations that are at different levels of this factor.
#' @return This function computes RDPI to the environmental factor for each species of the dataset(or any other identifying variable defined in `sp`)
#' Then it makes an ANOVA or t-test of the values of RDPI across species
#' and plots the boxplot
#' @examples
#' data(ecophysio)
#' rdpi(ecophysio,sp,SB, Piso)
#' 
#' # if we want to store the values
#' 
#' foo <- rdpi(ecophysio,sp,SB, Piso)
#' @export

rdpi <- function(dataframe, sp, trait, factor) {
    
    # Load the required libraries
    
    library(agricolae)
    library(psych)
    library(dplyr)
    library(ggplot2)
    library(sciplot)
    
    # Create the object (an empty data frame) that will store the results of RDPI
    RDPI <- data.frame(sp = character(0), value = numeric(0))
    
    # Since we need to compute everything per species, we make a 'for' loop
    levels_dataframe <- levels(dataframe %>% pull({{sp}}) %>% droplevels())
    
    for (a in levels_dataframe) {
        
        # subset the data for a given species
        data_sp <- dataframe %>% filter({{sp}} == a)
        
        RDPI_temp <- rdpi_matrix(data_sp, {{trait}}, {{factor}})
    
        RDPI_sp <- data.frame(sp = as.character(a),
                              rdpi = RDPI_temp)
        
        RDPI <- rbind(RDPI, RDPI_sp)
     }
    
    # Once everything calculated, let's produce nice outputs
    
    # A table with summary statistics for each sp
    summary <- RDPI %>%
        group_by(sp) %>%
        summarise(mean = mean(rdpi,na.rm=T),
                  sd = sd(rdpi,na.rm=T),
                  se = se(rdpi, na.rm=T))
    
    # A boxplot
    boxplot_rdpi <- ggplot(RDPI) +
        geom_boxplot(aes(sp, rdpi)) +
        ylab("RDPI") +
        xlab(deparse(substitute(sp)))
    
    print(boxplot_rdpi)
    
    if (nlevels(RDPI$sp) < 3) {
        fit <- t.test(RDPI$rdpi ~ RDPI$sp)
        print(summary)
        print("t-test")
        print(fit)
        
    } else {
        fit <- aov(RDPI$rdpi ~ RDPI$sp)
        
        print("ANOVA test")
        print(summary(fit))
        Tuk <- HSD.test (fit, trt='RDPI$sp')
        Tuk$groups$sp <- as.factor(row.names(Tuk$groups))
        summary <- left_join(summary, Tuk$groups) %>%
            select(-`RDPI$rdpi`)
        print("Tukey HSD test for differences accross groups")
        print(summary)
    }
    
    invisible(RDPI)
    # if(verbose == T) {
    #     return(RDPI)
    # } 
    # 
}

