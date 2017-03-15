### Weka tokenizers.

## Note that class Tokenizer provides methods
##   getOptions()
##   listOptions()
##   setOptions()
## (in fact, Weka's OptionHandler interface) so that we should be able
## to safely call these methods.

make_Weka_tokenizer <-
function(name, package = NULL)
{
    ## Create an interface to a Weka class for tokenizing.

    kind <- "R_Weka_tokenizer_interface"
    name <- as_JNI_name(name)
    meta <- make_R_Weka_interface_metadata(name, kind, "character",
                                           package = package)
    Weka_interfaces[[Java_class_base_name(name)]] <- meta

    out <- function(x, control = NULL) {
        tokenizer <- Weka_object_for_name(name, package)
        x <- Filter(nzchar, as.character(x))
        if(!length(x)) return(character())
        .jcall("RWekaInterfaces", "[S", "tokenize",
               .jcast(tokenizer, "weka/core/tokenizers/Tokenizer"),
               .jarray(as.character(control)),
               .jarray(as.character(x)))
    }

    make_R_Weka_interface(out, meta)
}
