write_to_dot <-
function(x, con = stdout(), ...)
    UseMethod("write_to_dot")

write_to_dot.Weka_classifier <-
function(x, con = stdout(), ...)
{
    ## Most Weka classifiers do not implement the 'Drawable' interface
    ## and hence have no graph() method.
    if(!.has_method(x$classifier, "graph"))
        stop("Cannot create dot description from 'x'.")
    writeLines(.jcall(x$classifier, "S", "graph"), con)
}

parse_Weka_digraph <-
function(x, plainleaf = TRUE)
{
    ## Create a simple node/edge representation of digraphs obtained
    ## from Weka's graph() methods.  Note that this could easily be
    ## turned into a full-featured dot parser possibly providing a
    ## graphNEL representation, but this is already done by package
    ## 'graph' ...
    
    ## Use the individual lines apart from the first and last ones.
    x <- strsplit(x, "\n")[[1L]]
    x <- x[-c(1L, length(x))]
    
    ind <- regexpr("->", x, fixed = TRUE)
    nodes <- x[ind == -1L]
    edges <- x[ind != -1L]
    
    nval <- matrix(rep("", 2L * length(nodes)), ncol = 2L)
    colnames(nval) <- c("name", "splitvar")
    nval[, 1L] <- sapply(strsplit(nodes, " "), "[", 1L)
    nval[, 2L] <- sapply(strsplit(nodes, "\""), "[", 2L)
    if(plainleaf)
        nval[grep("(", nval[, 2L], fixed = TRUE), 2L] <- ""
    
    eval <- matrix(rep("", 3L * length(edges)), ncol = 3L)
    colnames(eval) <- c("from", "to", "label")
    eval[, 1L] <- sapply(strsplit(edges, "->"), "[", 1L)
    eval[, 2L] <-
        sapply(strsplit(as.character(sapply(strsplit(edges, "->"),
                                            "[", 2L)),
                        " "),
               "[", 1L)
    eval[, 3L] <- sapply(strsplit(edges, "\""), "[", 2L)
    
    return(list(nodes = nval, edges = eval))
}

make_Weka_classifier_tree <-
function(x)
{
    ## For a fitted Weka classifier tree from a class which implements
    ## the Drawable interface and hence has a graph() method creating a
    ## dot representation, create an intermediate representation of the
    ## graph which can then be coerced to e.g. a dendrogram object (note
    ## that the plot method for dendrograms really is not good enough
    ## for our purposes), or a BinaryTree object (provided that the tree
    ## is binary, of course).

    x <- .jcall(x$classifier, "S", "graph")
    nodes_and_edges <- parse_Weka_digraph(x, FALSE)
    nodes <- nodes_and_edges$nodes
    edges <- nodes_and_edges$edges

    max_n_of_children <- if(NROW(edges) > 0L) max(table(edges[, "from"])) else 0
    max_depth <- 0
            
    get_subtree <- function(node, depth = 0) {
        if (depth > max_depth)
            max_depth <<- depth

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
        if (!length(ind)) {
            out <- 1
            attributes(out) <-
                list(members = 1, leaf = TRUE, depth = depth,
                     label = label, edgetext = edgetext, nodeID = node)
            return(out)
        }
        out <- vector("list", length = length(ind))
        for (i in seq_along(out))
            out[[i]] <- Recall(edges[ind[i], "to"], depth + 1)
        attributes(out) <-
            list(members = sum(sapply(out, attr, "members")),
                 leaf = FALSE, depth = depth, label = label,
                 edgetext = edgetext, nodeID = node)
        out
    }

    out <- get_subtree("N0")
    attr(out, "max_depth") <- max_depth
    attr(out, "max_n_of_children") <- max_n_of_children
    
    out
}

as.dendrogram.Rweka_classifier_tree <-
function(object, ...)
{
    max_depth <- attr(object, "max_depth")
    attr(object, "max_depth") <- NULL

    convert <- function(x) {
        y <- x
        class(y) <- "dendrogram"
        attr(y, "height") <- max_depth - attr(x, "depth")
        if(is.leaf(x))
            return(y)
        for(i in seq_along(x))
            y[[i]] <- Recall(x[[i]])
        y
    }

    convert(object)
}

as.BinaryTree <-
function(object, data, response, ...)
{
    ## Hmm, shouldn't this be a generic function, and this be "just" the
    ## method for Rweka_classifier_tree objects?

    ## Probably yes, but currently only used for plotting. It should
    ## not be expected to return a fully valid BinaryTree object.

    if(attr(object, "max_n_of_children") > 2)
        stop("Cannot coerce n-ary trees.")

    ## construct a BinaryTree object    
    ## for numeric binary splits only
    make_BinaryTree <-
    function(x, data, weights = rep(1, nrow(data)))
    {
        terminal <- attr(x, "leaf")

        if(terminal) {
            node <- list(nodeID = attr(x, "nodeID"),
	                 ## for a more generally valid BinaryTree object,			 
	                 ## these would need to be integer IDs
                         weights = weights,
                         criterion = list(statistic = 1,
                                          criterion = 1,
                                          maxcriterion = 1),
                         terminal = TRUE,
                         psplit = NULL,
                         ssplit = NULL,
                         prediction = 0,
                         left = NULL,
                         right = NULL,
                         label = attr(x, "label"))
           class(node) <- "TerminalNode"
        } else {
            var <- attr(x, "label")
            ordered <- is.numeric(data[[var]]) || is.ordered(data[[var]])
            if (ordered) {
                split <- as.numeric(gsub("[=< ]", "",
                                         attr(x[[1L]], "edgetext")))
                leftweights <- weights * (data[[var]] <= split)
                rightweights <- weights * (data[[var]] > split)
            } else {
                split <- gsub("[!= ]", "", attr(x[[1L]], "edgetext"))
                split <- as.integer(levels(data[[var]]) %in% split)
                if (length(grep("!", attr(x[[1L]], "edgetext"))) > 0)
                    split <- !split
                leftweights <- weights * (data[[var]] %in% levels(data[[var]])[split])
                rightweights <- weights * (data[[var]] %in% levels(data[[var]])[!split])
                attr(split, "levels") <- levels(data[[var]])
            }

            node <- list(nodeID = attr(x, "nodeID"),
                         weights = weights,
                         criterion = list(statistic = 1,
                                          criterion = 1,
                                          maxcriterion = 1),
                         terminal = FALSE,
                         psplit = list(variableID = which(colnames(data) == var),
                                       variableName = var,
                                       ordered = ordered,
                                       splitpoint = split,
                                       splitstatistics = NULL,
                                       table = ifelse(!ordered, table(data[[var]]), 0)),
                         ssplit = NULL,
                         prediction = 0,
                         left = Recall(x[[1L]], data = data, leftweights),
                         right = Recall(x[[2L]], data = data, rightweights))
            class(node$psplit) <-
                ifelse(ordered, "orderedSplit", "nominalSplit")
            class(node) <- "SplittingNode"
        }
        return(node)
    }

    out <- new("BinaryTree")
    out@tree <- make_BinaryTree(object, data)
    out@responses <- party::initVariableFrame(data.frame(response), NULL)
    out@get_where <- function(...) NULL
    out
}

node_Weka <-
function(treeobj, digits = 3, abbreviate = FALSE,
         fill = c("lightgray", "white"), linebreaks = TRUE, id = FALSE)
{
    fill <- rep(fill, length.out = 2L)

    rval <- function(node) {

        ### this is a character node label from Weka, we just print
        lab <- node$label
        #Z# well, we could try to prettify:
        if(linebreaks) {
          lab1 <- strsplit(lab, " (", fixed = TRUE)[[1L]]
          lab <- paste(paste(lab1[-length(lab1)], collapse = " ("), "\n(", lab1[length(lab1)], sep = "")
        }

        node_vp <- viewport(x = unit(0.5, "npc"),
            y = unit(0.5, "npc"),   
            width = unit(1, "strwidth", lab) + unit(1, "lines"),
            height = unit(1, "strheight", lab) + unit(2, "lines"),
            name = sprintf("node_terminal%s", node$nodeID))
        pushViewport(node_vp)

        grid.rect(gp = gpar(fill = fill[1L]))   
        grid.text(y = unit(1.5 + 0.5 * linebreaks, "lines"), lab)

        if(id) {
            nodeIDvp <- viewport(x = unit(0.5, "npc"),
            	y = unit(1, "npc"),
            	width = max(unit(1, "lines"), unit(1.3, "strwidth", as.character(node$nodeID))), 
            	height = max(unit(1, "lines"), unit(1.3, "strheight", as.character(node$nodeID))))
            pushViewport(nodeIDvp)
            grid.rect(gp = gpar(fill = fill[2L], lty = "solid"))
            grid.text(node$nodeID)
            popViewport()
        }
        upViewport()
    }
    return(rval)
}
class(node_Weka) <- "grapcon_generator"

plot.Weka_tree <-
function(x, type = "simple", terminal_panel = node_Weka,
         ip_args = list(pval = FALSE, id = FALSE), ...)
{
    if(!require("party"))
        stop("Plotting Weka trees requires package 'party'.")
    x$call[[1]] <- as.name("ModelEnvFormula")
    data <- eval(x$call, environment(formula(x)))@get("input")
    response <- eval(x$call, environment(formula(x)))@get("response")
    x <- make_Weka_classifier_tree(x)
    if(attr(x, "max_n_of_children") > 2)
        stop("Plotting of trees with multi-way splits is currently not implemented.")
    
    plot(as.BinaryTree(x, data, response), type = type,
         terminal_panel = terminal_panel, ip_args = ip_args, ...)
}
