read_data_into_Weka <-
function(x)
{
    ## Write the data to a temporary ARFF file.
    arfff <- tempfile()
    on.exit(unlink(arfff))
    write.arff(x, arfff)

    ## Read the temporary ARFF file into Weka.
    reader <- .jnew("java/io/FileReader", arfff)
    instances <- .jnew("weka/core/Instances",
                       .jcast(reader, "java/io/Reader"))

    instances
}

read_model_frame_into_Weka <-
function(mf)
{
    ## For Weka, always have the response *last* in the ARFF file.
    instances <- read_data_into_Weka(mf[c(seq(along = mf)[-1], 1)])
    .jcall(instances, "V", "setClassIndex",
           as.integer(.jcall(instances, "I", "numAttributes") - 1))
    instances
}
