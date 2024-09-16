#' @title rdpi_matrix

#' @description Function to compute a vector containing the relative distance plasticity index values (Valladares et al. 2006) of a given
#' trait (trait) for a given categorical environmental variable (factor). It calculates rdpi for each pair of observation that does
#' not belong to the same level of "factor", and returns a vector containing all the calculated rdpi values.
#' @param data The dataframe that contains the data
#' @param trait The bare (unquoted) name of the column that holds the trait for which to calculate RDPI. Must be numeric
#' @param factor the bare (unquoted) name of the column that holds the environmental factor for which we will calculate RDPI.
#' By definition, RDPI computes distances between pairs of observations that are at different levels of this factor.
#' @return a vector containing all the calculated rdpi values for each pair of observations that do not belong to the same level of "factor"
#' @examples
#' data(ecophysio)
#' compute_rdpi(ecophysio,SB, Piso)
#' @export
#'
    rdpi_matrix <- function (data, trait, factor) {

        # Step1: Compute pairwise Canberra distance (aka RDPI) for all individuals in the dataset
        RDPI_temp <- as.matrix(dist(x = data |> |>  pull({{trait}}), method="canberra"))

        # Step 2: Generate a matrix where value is "TRUE" only if observation i and observation j
        # belong to different levels of the factor
        filter_frame <- data.frame(matrix(NA, nrow(data), nrow(data)))

        for (i in 1:nrow(filter_frame)) {
            for (j in 1:ncol(filter_frame)) {

                ifelse(pull(data, {{factor}})[i] == pull(data, {{factor}})[j],
                       filter_frame[i,j] <- FALSE,
                       filter_frame[i,j] <- TRUE)
            }
        }

        filter_frame[upper.tri(filter_frame, diag = T)] <- FALSE         #only keep lower triangle

        # Step 3: Subset RDPI so that it only includes comparisons between individuals that
        # belong to different levels of an environmental variable
        RDPI_temp[filter_frame == TRUE]

    }
