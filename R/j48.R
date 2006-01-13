if(FALSE) {
setGeneric("j48", function(x, ...) standardGeneric("j48"))
setMethod("j48",signature(x="formula"),
          function (x, data=NULL, unpruned = FALSE, confidence = 0.25,
                    binary = TRUE,
                    minum = 2, reducederrpr = FALSE, folds = 3,
                    subtreeraising = FALSE, seed = NULL,
                    ..., subset, na.action = na.omit) {
              call <- match.call()
              m <- match.call(expand.dots = FALSE)
              if (is.matrix(eval(m$data, parent.frame())))
                  m$data <- as.data.frame(data)
              m$... <- NULL
              m$formula <- m$x
              m$x <- NULL
              m$scaled <- m$unpruned <- m$confidence <- m$binary <- NULL
              m$minum <- m$reducederrpr <- m$folds <- m$subtreeraising<- seed <- NULL
              m$subset <- m$na.action <- NULL
              m[[1]] <- as.name("model.frame")
              m <- eval(m, parent.frame())
              Terms <- attr(m, "terms")
              attr(Terms, "intercept") <- 0
              x <- model.frame(Terms, m)
              y <- model.extract(m, response)
              ## call(ret) <- call
              ## terms(ret) <- Terms
              ##if (!is.null(attr(m, "na.action")))
              ## n.action(ret) <- attr(m, "na.action")
            
              if (!missing(subset)) x <- x[subset,]
              yn <- names(x)
              df <- na.action(data.frame(y, x[,-1]))
              y <- df[,1]
              x <- df[,-1]
              
              ## Create and save weka data file 
              xdata <-  cbind(x, y)
              names(xdata) <- c(yn[-1], yn[1])
              tmp <- tempfile()
              write.arff(xdata,file=tmp)

              reader <- .jnew("java/io/FileReader", tmp)
              instances <- .jnew("weka/core/Instances",
                                 .jcast(reader, "java/io/Reader"))
              J48 <- .jnew("weka/classifiers/trees/J48")
              
              if(unpruned)
                  .jcall(J48, "V", "setUnpruned", TRUE)
              else
                  .jcall(J48, "V", "setUnpruned", FALSE)
              
              ## .jcall(J48, "V", "setConfidenceFactor", as.single(confidence))
         
              .jcall(J48, "V", "setMinNumObj",as.integer(minum))

              if(reducederrpr)
                  .jcall(J48, "V", "setReducedErrorPruning", TRUE)
              else
                  .jcall(J48, "V", "setReducedErrorPruning", FALSE)
           
              .jcall(J48, "V", "setNumFolds", as.integer(folds))
           
              if(binary)
                  .jcall(J48, "V", "setBinarySplits", TRUE)
              else
                  .jcall(J48, "V", "setBinarySplits", FALSE)
           
              if(subtreeraising)
                  .jcall(J48, "V", "setSubtreeRaising", TRUE)
              else
                  .jcall(J48, "V", "setSubtreeRaising", FALSE)
           
              if(!is.null(seed))
                  .jcall(J48, "V", "setSeed", as.integer(seed))

              .jcall(instances, "V", "setClassIndex",
                     as.integer(.jcall(instances, "I", "numAttributes") - 1))
              .jcall(J48, "V", "buildClassifier", instances)
              graph  <- .jcall(J48, "S", "graph")

              spgraph <- strsplit(graph,split="\n")
              graph <- spgraph[[1]][c(-1,-length(spgraph))]

              ctree <- classifierTree(graph)

              resp <- as.data.frame(y)
              xda <- as.data.frame(x)
              names(xda) <- yn[-1]
              names(resp) <- yn[1]
              
              return(new("j48", .Data=ctree, data = xda, response =
                         resp, terms = Terms, n.action = na.action))
          })


setGeneric("classifierTree", function(x) standardGeneric("classifierTree"))
setMethod("classifierTree",signature(x="vector"),
          function(x) {
            
            nodes_and_edges <- parse_Weka_digraph(x, FALSE)
            nodes <- nodes_and_edges$nodes
            edges <- nodes_and_edges$edges
            
            max_depth <- 0
            
            get_subtree <- function(node, depth = 0) {
              if(depth > max_depth) {
                max_depth <<- depth
              }
              ind <- which(nodes[, "name"] == node)
              ## message(ind, "\n")
              ## Sanitize ...
              label <- nodes[ind, "splitvar"]
              ## message(node, label, "\n")
              ind <- which(edges[, "to"] == node)
              edgetext <- if(any(ind))
                edges[ind, "label"]
              else
                ""
              ind <- which(edges[, "from"] == node)
              if(!length(ind)) {
                out <- 1
                attributes(out) <-
                  list(members = 1, leaf = TRUE, depth = depth,
                       label = label, edgetext = edgetext)
                return(out)
              }
              out <- vector("list", length = length(ind))
              for(i in seq(along = out))
                out[[i]] <- Recall(edges[ind[i], "to"], depth + 1)
              attributes(out) <-
                list(members = sum(sapply(out, attr, "members")),
                     leaf = FALSE, depth = depth, label = label,
                     edgetext = edgetext)
              out
            }
            
            out <- get_subtree("N0")
            attr(out, "max_depth") <- max_depth
            out
          })
        
setGeneric("as.dendrogram", function(object, ...) standardGeneric("as.dendrogram"))
setMethod("as.dendrogram",signature(object="j48"),  
          function(object , ...) {
            max_depth <- attr(object, "max_depth")
            attr(object, "max_depth") <- NULL

            use_me <- function(x) {
              y <- x
              class(y) <- "dendrogram"
              attr(y, "height") <- max_depth - attr(x, "depth")
              if(is.leaf(x))
            return(y)
              for(i in seq(along = x))
                y[[i]] <- Recall(x[[i]])
              y
            }
            
            use_me(object)
          })

setGeneric("parse_Weka_digraph",
           function(x, plainleaf = TRUE)
           standardGeneric("parse_Weka_digraph"))
setMethod("parse_Weka_digraph",
          signature(x="vector"),
          function(x, plainleaf = TRUE) {
            edgeIndex <- grep("->", x, extended = FALSE)
            edges <- x[edgeIndex]
            nodes <- x[-edgeIndex]
            
            get1 <- function(x) x[1]
            get2 <- function(x) x[2]
            
            nval <- matrix(rep("", 2*length(nodes)), ncol = 2)
            colnames(nval) <- c("name", "splitvar")
            nval[,1] <- sapply(strsplit(nodes, " "), get1)
            nval[,2] <- sapply(strsplit(nodes, "\""), get2)
            if(plainleaf) nval[grep("(", nval[,2], extended = FALSE), 2] <- ""
            
            eval <- matrix(rep("", 3*length(edges)), ncol = 3)
            colnames(eval) <- c("from", "to", "label")
            eval[,1] <- sapply(strsplit(edges, "->"), get1)
            eval[,2] <- sapply(strsplit(sapply(strsplit(edges, "->"), get2), " "), get1)
            eval[,3] <- sapply(strsplit(edges, "\""), get2)
            
            return(list(nodes = nval, edges = eval))
          })


nodeApply <- function(x, FUN, ...) {
    y <- FUN(x, ...)
    if(is.leaf(x))
        return(y)
    for(i in seq(along = x))
        y[[i]] <- Recall(x[[i]], FUN, ...)
    y
}

## construct a BinaryTree object
## for numeric binary splits only
makeBinaryTree <- function(x, data, weights = rep(1, nrow(data))) {

    terminal <- attr(x, "leaf")

    if (terminal) {
        node <- list(nodeID = nr,
                     weights = weights,
                     criterion = list(statistic = 1,
                                      criterion = 1,
                                      maxcriterion = 1),
                     terminal = TRUE,
                     psplit = NULL,
                     ssplit = NULL,
                     prediction = 0,
                     left = NULL,
                     right = NULL)
       class(node) <- "TerminalNode"
    } else {
        var <- attr(x, "label")
        ordered <- is.numeric(data[[var]]) || is.ordered(data[[var]])
        split <- as.numeric(gsub("[=< ]", "", attr(x[[1]], "edgetext")))

        node <- list(nodeID = nr,
                     weights = weights,
                     criterion = list(statistic = 1,
                                      criterion = 1,
                                      maxcriterion = 1),
                     terminal = FALSE,
                     psplit = list(variableID = which(colnames(data) == var),
                                   variableName = var,
                                   ordered = ordered,
                                   splitpoint = split,
                                   splitstatistics = NULL),
                     ssplit = NULL,
                     prediction = 0,
                     left = makeBinaryTree(x[[1]], data = data, weights *
                                           (data[[var]] <= split)),
                     right = makeBinaryTree(x[[2]], data = data, weights *
                                            (data[[var]] > split)))
        class(node$psplit) <- "orderedSplit"
        class(node) <- "SplittingNode"
    }
    nr <<- nr + 1
    return(node)
}


## setGeneric("as.BinaryTree", function(object, ...) standardGeneric("as.BinaryTree"))
## setMethod("as.BinaryTree",signature(object = "j48"),
as.BinaryTree <- function(object, ...) {
    
    nr <<- 1
    tree <- makeBinaryTree(object, object@data)
    RET <- new("BinaryTree")
    RET@tree <- tree
    RET@responses <- party:::initVariableFrame(object@response, NULL)
    RET@get_where <- function(...) NULL
    RET
}
## )
}
