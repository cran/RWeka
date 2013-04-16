### R code from vignette source 'RWeka.Rnw'

###################################################
### code chunk number 1: RWeka.Rnw:15-16
###################################################
require("RWeka")


###################################################
### code chunk number 2: RWeka.Rnw:62-63 (eval = FALSE)
###################################################
## WPM("refresh-cache")


###################################################
### code chunk number 3: RWeka.Rnw:66-67 (eval = FALSE)
###################################################
## WPM("list-packages", "installed")


###################################################
### code chunk number 4: RWeka.Rnw:70-71 (eval = FALSE)
###################################################
## WPM("list-packages", "available")


###################################################
### code chunk number 5: RWeka.Rnw:93-97 (eval = FALSE)
###################################################
## LBR <-
## make_Weka_classifier("weka/classifiers/lazy/LBR",
##                      c("LBR", "Weka_lazy"),
##                      init = make_Weka_package_loader("lazyBayesianRules"))


###################################################
### code chunk number 6: RWeka.Rnw:111-116
###################################################
m1 <- J48(Species ~ ., data = iris)
writeLines(rJava::.jstrVal(m1$classifier))
save(m1, file = "m1.rda")
load("m1.rda")
rJava::.jstrVal(m1$classifier)


###################################################
### code chunk number 7: RWeka.Rnw:130-135
###################################################
m1 <- J48(Species ~ ., data = iris)
rJava::.jcache(m1$classifier)
save(m1, file = "m1.rda")
load("m1.rda")
writeLines(rJava::.jstrVal(m1$classifier))


###################################################
### code chunk number 8: RWeka.Rnw:140-141
###################################################
unlink("m1.rda")


###################################################
### code chunk number 9: RWeka.Rnw:151-170
###################################################
graphVisualizer <-
function(file, width = 400, height = 400,
         title = substitute(file), ...)
{
    ## Build the graph visualizer
    visualizer <- .jnew("weka/gui/graphvisualizer/GraphVisualizer")
    reader <- .jnew("java/io/FileReader", file)
    .jcall(visualizer, "V", "readDOT",
          .jcast(reader, "java/io/Reader"))
    .jcall(visualizer, "V", "layoutGraph")
    ## and put it into a frame.
    frame <- .jnew("javax/swing/JFrame",
                   paste("graphVisualizer:", title))
    container <- .jcall(frame, "Ljava/awt/Container;", "getContentPane")
    .jcall(container, "Ljava/awt/Component;", "add", 
           .jcast(visualizer, "java/awt/Component"))
    .jcall(frame, "V", "setSize", as.integer(width), as.integer(height))
    .jcall(frame, "V", "setVisible", TRUE)
}


###################################################
### code chunk number 10: RWeka.Rnw:173-175 (eval = FALSE)
###################################################
## write_to_dot(m1, "m1.dot")
## graphVisualizer("m1.dot")


###################################################
### code chunk number 11: RWeka.Rnw:202-203
###################################################
c("-W", "weka.classifiers.trees.J48", "--", "-M", 30)


###################################################
### code chunk number 12: RWeka.Rnw:207-208
###################################################
Weka_control(W = J48, "--", M = 30)


###################################################
### code chunk number 13: RWeka.Rnw:212-217
###################################################
myAB <- make_Weka_classifier("weka/classifiers/meta/AdaBoostM1")
myAB(Species ~ ., data = iris,
     control = c("-W", "weka.classifiers.trees.J48", "--", "-M", 30))
myAB(Species ~ ., data = iris,
     control = Weka_control(W = J48, "--", M = 30))


###################################################
### code chunk number 14: RWeka.Rnw:223-227
###################################################
AdaBoostM1(Species ~ ., data = iris,
           control = Weka_control(W = list(J48, "--", M = 30)))
AdaBoostM1(Species ~ ., data = iris,
           control = Weka_control(W = list(J48, M = 30)))


