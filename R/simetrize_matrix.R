#' Get the full symetric matrix from a upper triangular contact matrix
#'
#' This function takes a upper triangular HiC contact matrix and returns the full symetric matrix.
#' @import magrittr
#' @import Matrix
#' @importFrom dplyr summarize
#' @importFrom dplyr slice
#' @param mat HiC contact matrix (should be upper triangular)
#' @return The full symetric HiC contact matrix corresponding to the input one
#' @export
#' @examples
#' plot(0)

simetrize_matrix <- function(mat){

    idx <- min(100, nrow(mat))
    
    check <- summary(mat[1:idx, 1:idx]) %>%
        summarize(upper = sum(i > j),
                  lower = sum(j > i)) %>%
        slice(1) %>%
        unlist

    if(length(unique(check)) == 1) {
        
        message("Matrix seems to be already symetric, doing nothing ...")
        
        return(mat)

    }

    mat <- mat + t(mat)

    mat

}
