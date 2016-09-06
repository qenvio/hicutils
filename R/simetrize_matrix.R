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

    check <- c(triu(mat, -1) %>% sum(na.rm = T), tril(mat, -1) %>% sum(na.rm = T))

    if(min(check) > 0) {
        
        message("Matrix seems to be already symetric, doing nothing ...")
        
        return(mat)

    }

    mat <- mat + t(mat)

    mat

}
