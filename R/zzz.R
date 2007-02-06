### * Associators
Apriori <-
    make_Weka_associator("weka/associations/Apriori", "Apriori")
Tertius <-
    make_Weka_associator("weka/associations/Tertius", "Tertius")

### * Classifiers

### ** Functions
LinearRegression <-
    make_Weka_classifier("weka/classifiers/functions/LinearRegression",
                         c("LinearRegression", "Weka_functions"))
Logistic <-
    make_Weka_classifier("weka/classifiers/functions/Logistic",
                         c("Logistic", "Weka_functions"))
SMO <-
    make_Weka_classifier("weka/classifiers/functions/SMO",
                         c("SMO", "Weka_functions"))

### ** Lazy
IBk <- make_Weka_classifier("weka/classifiers/rules/IBk",
                            c("IBk", "Weka_lazy"))
LBR <- make_Weka_classifier("weka/classifiers/rules/LBR",
                            c("LBR", "Weka_lazy"))

### ** Rules
JRip <- make_Weka_classifier("weka/classifiers/rules/JRip",
                             c("JRip", "Weka_rules"))
M5Rules <- make_Weka_classifier("weka/classifiers/rules/M5Rules",
                                c("M5Rules", "Weka_rules"))
OneR <- make_Weka_classifier("weka/classifiers/rules/OneR",
                             c("OneR", "Weka_rules"))
PART <- make_Weka_classifier("weka/classifiers/rules/PART",
                             c("PART", "Weka_rules"))

### ** Trees
J48 <- make_Weka_classifier("weka/classifiers/trees/J48",
                            c("J48", "Weka_tree"))
M5P <- make_Weka_classifier("weka/classifiers/trees/M5P",
                            c("M5P", "Weka_tree"))
LMT <- make_Weka_classifier("weka/classifiers/trees/LMT",
                            c("LMT", "Weka_tree"))
DecisionStump <-
    make_Weka_classifier("weka/classifiers/trees/DecisionStump",
                         c("DecisionStump", "Weka_tree"))

### ** Meta learners

.get_qualified_Java_class_name <-
function(o)
    as_qualified_name(get_Java_class(o))
.Weka_meta_classifier_handlers <-
    list(control =
         make_Weka_control_handler("-W", .get_qualified_Java_class_name))
AdaBoostM1 <-
    make_Weka_classifier("weka/classifiers/meta/AdaBoostM1",
                         c("AdaBoostM1", "Weka_meta"),
                         .Weka_meta_classifier_handlers)
Bagging <-
    make_Weka_classifier("weka/classifiers/meta/Bagging",
                         c("Bagging", "Weka_meta"),
                         .Weka_meta_classifier_handlers)
LogitBoost <-
    make_Weka_classifier("weka/classifiers/meta/LogitBoost",
                         c("LogitBoost", "Weka_meta"),
                         .Weka_meta_classifier_handlers)
MultiBoostAB <-
    make_Weka_classifier("weka/classifiers/meta/MultiBoostAB",
                         c("MultiBoostAB", "Weka_meta"),
                         .Weka_meta_classifier_handlers)
Stacking <-
    make_Weka_classifier("weka/classifiers/meta/Stacking",
                         c("Stacking", "Weka_meta"),
                         .Weka_meta_classifier_handlers)

### * Clusterers

Cobweb <-
    make_Weka_clusterer("weka/clusterers/Cobweb", "Cobweb")
FarthestFirst <-
    make_Weka_clusterer("weka/clusterers/FarthestFirst", "FarthestFirst")
SimpleKMeans <-
    make_Weka_clusterer("weka/clusterers/SimpleKMeans", "SimpleKMeans")
XMeans <-
    make_Weka_clusterer("weka/clusterers/XMeans", "XMeans")
DBScan <-
    make_Weka_clusterer("weka/clusterers/DBScan", "DBScan")

### * Converters

### ** Savers

## <FIXME>
## Shouldn't *all* file saver interfaces use the index decrementer as
## control handler?
## If yes, maybe include this in the make_Weka_file_saver() defaults?
.decrement_number_by_one <-
function(o)
{
    ## Weka_control(c = 2)
    if(is.numeric(o))
        o <- o - 1
    else if(is.character(o)
            && (regexpr("^[[:digit:]]*$", o) > -1))
        o <- as.numeric(o) - 1
    o
}
.Weka_file_saver_handlers <-
    list(control =
         make_Weka_control_handler("-c", .decrement_number_by_one))
C45Saver <- make_Weka_file_saver("weka/core/converters/C45Saver",
                                 .Weka_file_saver_handlers)
XRFFSaver <- make_Weka_file_saver("weka/core/converters/XRFFSaver",
                                  .Weka_file_saver_handlers)
## </FIXME>
## Could also provide interfaces to ArffSaver, CSVSaver, and
## LibSVMSaver.

### ** Loaders

C45Loader <- make_Weka_file_loader("weka/core/converters/C45Loader")
XRFFLoader <- make_Weka_file_loader("weka/core/converters/XRFFLoader")

### * Filters

Normalize <-
    make_Weka_filter("weka/filters/unsupervised/attribute/Normalize", 
                     "Normalize")
Discretize <-
    make_Weka_filter("weka/filters/supervised/attribute/Discretize", 
                     "Discretize")

### * Stemmers

IteratedLovinsStemmer <-
    make_Weka_stemmer("weka/core/stemmers/IteratedLovinsStemmer")
LovinsStemmer <-
    make_Weka_stemmer("weka/core/stemmers/LovinsStemmer")


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
