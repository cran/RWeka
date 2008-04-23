import weka.classifiers.*;
import weka.clusterers.*;
import weka.core.*;
import weka.core.stemmers.*;
import java.lang.*;
import java.io.*;

// changed to abstract class and class methods. (C)

public abstract class RWekaInterfaces {

    public static double[] classifyInstances(Classifier C, Instances I)
	throws Exception
    {
	int n = I.numInstances();
	double[] out = new double[n];
	
	int k = 0;
	for(int i = 0; i < n; i++) {
	    try {
		out[i] = C.classifyInstance(I.instance(i));
	    } catch(Exception e) {
		k++;
		out[i] = Double.NaN;	// see weke.core.Instance
	    }
	}
	if (k > 0)
	   System.out.println(k+" instances not classified");

	return(out);
    }

    public static double[] clusterInstances(Clusterer C, Instances I)
	throws Exception
    {
	int n = I.numInstances();
	double[] out = new double[n];

	int k = 0;
	for(int i = 0; i < n; i++) {
	    try {
		out[i] = C.clusterInstance(I.instance(i));
		if (out[i] < 0)
		    out[i] = Double.NaN;
	    } catch (Exception e) {
		k++;
		out[i] = Double.NaN;	// as above
	    }
	}
	if (k > 0)
	   System.out.println(k+" instances not classified");
	
	return(out);
    }

    public static double[] distributionForInstances(Classifier C, Instances I)
	throws Exception
    {
	// We could more elegantly have this as double[][] with row i
	// giving the class probabilities for instance i, but it seems
	// rather costly to read this back into R (as we get a "long"
	// (numInstances) list of "short" (numClasses) array references).
	
	int n = I.numInstances();
	int m = I.numClasses();
	double[] out = new double[n * m];
	double[] tmp = new double[m];
	int k = 0;

	for(int i = 0; i < n; i++) {
	    tmp = C.distributionForInstance(I.instance(i));
	    for(int j = 0; j < m; j++, k++)
		out[k] = tmp[j];
	}
	return(out);
    }

    public static double[] distributionForInstances(Clusterer C, Instances I)
	throws Exception
    {
	int n = I.numInstances();
	int m = C.numberOfClusters();
	double[] out = new double[n * m];
	double[] tmp = new double[m];
	int k = 0;

	for(int i = 0; i < n; i++) {
	    tmp = C.distributionForInstance(I.instance(i));
	    for(int j = 0; j < m; j++, k++)
		out[k] = tmp[j];
	}
	return(out);
    }

    // Populate Instances with data in column-major format. Use
    // with care if called more than once with the same Instances
    // object. (C)
    
    public static void addInstances(Instances instances, double[] data, 
				    int nrow)
	throws Exception
    {
	int i, j, ncol = instances.numAttributes();

	if (data.length / ncol != nrow) {
	    throw new Exception("invalid number of rows 'nrow'");
	}

	for (i = 0; i < nrow; i++) {
	    Instance instance = new Instance(ncol);
	    for (j = 0; j < ncol; j++) 
		instance.setValue(j, data[i+j*nrow]);
	    instances.add(instance);
	}
    }

    public static double[] getAttributeWeights(Instances I)
	throws Exception
    {
	int i, n = I.numAttributes();
	double[] out = new double[n];
	for(i = 0; i < n; i++) {
	    out[i] = I.attribute(i).weight();
	}
	return(out);
    }	
    
    public static double[] getInstanceWeights(Instances I)
	throws Exception
    {
	int i, n = I.numInstances();
	double[] out = new double[n];
	for(i = 0; i < n; i++) {
	    out[i] = I.instance(i).weight();
	}
	return(out);
    }

    public static String[] stem(Stemmer S, String[] words)
	throws Exception
    {
	int i, n = words.length;
	String[] out = new String[n];
	for(i = 0; i < n; i++) {
	    out[i] = S.stem(words[i]);
	}
	return(out);
    }

    // Format or parse the data of a Weka 'date' attribute.
    // Note that NA_character is assumed to be R's missing
    // value code for character. (C)
    public static String[] formatDate(Attribute A, double[] data,
	String NA_character)
    {
	String[] out = new String[data.length];
	for (int i = 0; i < data.length; i++) {
	    if (Double.isNaN(data[i])) {
		out[i] = NA_character;
	    } else {
		out[i] = A.formatDate(data[i]);
	    }
	}
	return out;
    }

    public static double[] parseDate(Attribute A, String[] data,
	String NA_character)
    {
	double[] out = new double[data.length];
	for (int i = 0; i < data.length; i++) {
	    try {
		if (data[i].equals(NA_character)) {
		    out[i] = Double.NaN;
		} else {
		    out[i] = A.parseDate(data[i]);
		}
	    }
	    catch (Exception e) {
		out[i] = Double.NaN;
	    }
	}
	return out;
    }
    
}

//
