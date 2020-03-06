#' @title RDPI

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
#' @param verbose defines if we want to get a data frame with all the individual RDPI values calculated.
#'  By default is set to `TRUE`; indicating that we will get the data frame. If set to `FALSE`, we only get a summary table and a bocplot
#' @return This function computes RDPI to the environmental factor for each species of the dataset(or any other identifying variable defined in `sp`)
#' Then it makes an ANOVA or t-test of the values of RDPI across species
#' and plots the boxplot
#' @examples
#' data(ecophysio)
#' rdpi(ecophysio,sp,SB, Piso, verbose = F)
#' 
#' # if we want to store the values
#' 
#' foo <- rdpi(ecophysio,sp,SB, Piso, verbose = T)
#' @export

rdpi <- function(dataframe, sp, trait, factor, verbose = T) {
    
    # Load the required libraries
    
    library(agricolae)
    library(psych)
    library(dplyr)
    library(ggplot2)
    library(sciplot)
    
    # Modify the parameters to add quotes (")
    # sp <- deparse(substitute(sp))
    # trait <- deparse(substitute(trait))
    # factor <- deparse(substitute(factor))
    
    # Create the object (an empty data frame) that will store the results of RDPI
    RDPI <- data.frame(sp = character(0), value = numeric(0))
    
    # Since we need to compute everything per species, we make a 'for' loop
    
    levels_dataframe <- levels(dataframe %>% pull({{sp}}) %>% droplevels())
    
    for (a in levels_dataframe) {
        
        # subset the data for a given species
        data_sp <- dataframe %>% filter({{sp}} == a)
        
        # NOTE: RDPI is based on pairwise distances (Canberra distance) between individuals that
        # belong to different levels of an environmental variable. We perform this in three steps:
        
        # Step1: Compute pairwise Canberra distance (aka RDPI) for all individuals in the dataset
        RDPI_temp <- as.matrix(dist(x = data_sp %>% pull({{trait}}), method="canberra"))
        
        # Step 2: Generate a matrix where value is "TRUE" only if observation i and observation j
        # belong to different levels of the factor
        filter_frame <- data.frame(matrix(NA, nrow(data_sp), nrow(data_sp)))
        
        for (i in 1:nrow(filter_frame)) {
            for (j in 1:ncol(filter_frame)) {
                
                ifelse(pull(data_sp, {{factor}})[i] == pull(data_sp, {{factor}})[j],
                       filter_frame[i,j] <- FALSE,
                       filter_frame[i,j] <- TRUE)
            }
        }
        
        
        
        filter_frame[upper.tri(filter_frame, diag = T)] <- FALSE         #only keep lower triangle
        
        # Step 3: Subset RDPI so that it only includes comparisons between individuals that
        # belong to different levels of an environmental variable
        RDPI_temp <- RDPI_temp[filter_frame == TRUE]
        
        
        RDPI_sp <- data.frame(sp = a,
                              value = RDPI_temp)
        
        RDPI <- rbind(RDPI, RDPI_sp)
    }
    
    # Once everything calculated, let's produce nice outputs
    
    # A table with summary statistics for each sp
    summary <- RDPI %>%
        group_by(sp) %>%
        summarise(mean = mean(value,na.rm=T),
                  sd = sd(value,na.rm=T),
                  se = se(value, na.rm=T))
    
    # A boxplot
    boxplot_rdpi <- ggplot(RDPI) +
        geom_boxplot(aes(sp, value)) +
        ylab("RDPI") +
        xlab(deparse(substitute(sp)))
    
    print(boxplot_rdpi)
    
    if (nlevels(RDPI$sp) < 3) {
        fit <- t.test(RDPI$value ~ RDPI$sp)
        print(summary)
        print("t-test")
        print(fit)
        
    } else {
        fit <- aov(RDPI$value ~ RDPI$sp)
        
        print("ANOVA test")
        print(summary(fit))
        Tuk <- HSD.test (fit, trt='RDPI$sp')
        Tuk$groups$sp <- row.names(Tuk$groups)
        summary <- left_join(summary, Tuk$groups) %>%
            select(-`RDPI$value`)
        print("Tukey HSD test for differences accross groups")
        print(summary)
    }
    
    
    if(verbose == T) {
        return(RDPI)
    } 
    
}

