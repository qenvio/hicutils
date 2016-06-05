#' Get upper triangular matrix from a HiC contact matrix
#'
#' This function takes a HiC contact matrix and returns the upper triangular matrix. It divides the diagonal counts by 2.
#' @import magrittr
#' @import Matrix
#' @param mat HiC contact matrix
#' @return The upper triangular matrix of the original one
#' @export
#' @examples
#' plot(0)

upper_tri <- function(mat){

    idx <- min(100, nrow(mat))
    
    check <- summary(mat[1:idx, 1:idx]) %>%
        summarize(upper = sum(i > j),
                  lower = sum(j > i)) %>%
        slice(1) %>%
        unlist

    if(min(check) == 0) {
        
        message("Matrix seems to be already upper diagonal, doing nothing ...")
        
        return(mat)

    }

    out <- summary(mat) %>%
        filter(j >= i) %>%
        (function(x) sparseMatrix(i = x$i,
                                  j = x$j,
                                  x = x$x,
                                  dims = dim(mat),
                                  dimnames = dimnames(mat)))

    diag(out) <- diag(out) / 2
    
    out

    }
