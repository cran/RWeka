parse_Weka_digraph <-
function(x, plainleaf = TRUE)
{
    ## Use the individual lines apart from the first and last ones.
    x <- strsplit(x, "\n")[[1]]
    x <- x[-c(1, length(x))]
    
    edgeIndex <- grep("->", x, extended = FALSE)
    edges <- x[edgeIndex]
    nodes <- x[-edgeIndex]
    
    nval <- matrix(rep("", 2 * length(nodes)), ncol = 2)
    colnames(nval) <- c("name", "splitvar")
    nval[, 1] <- sapply(strsplit(nodes, " "), "[", 1)
    nval[, 2] <- sapply(strsplit(nodes, "\""), "[", 2)
    if(plainleaf)
        nval[grep("(", nval[, 2], extended = FALSE), 2] <- ""
    
    eval <- matrix(rep("", 3 * length(edges)), ncol = 3)
    colnames(eval) <- c("from", "to", "label")
    eval[, 1] <- sapply(strsplit(edges, "->"), "[", 1)
    eval[, 2] <- sapply(strsplit(sapply(strsplit(edges, "->"), "[", 2),
                                 " "),
                        "[", 1)
    eval[, 3] <- sapply(strsplit(edges, "\""), "[", 2)
    
    return(list(nodes = nval, edges = eval))
}
