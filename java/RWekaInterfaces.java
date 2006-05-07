import weka.classifiers.*;
import weka.clusterers.*;
import weka.core.*;

public class RWekaInterfaces {

    public double[] classifyInstances(Classifier C, Instances I)
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

    public double[] clusterInstances(Clusterer C, Instances I)
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

//     public double[][] distributionForInstances(Classifier C, Instances I)
// 	throws Exception
//     {
// 	int n = I.numInstances();
// 	double[][] out = new double[n][I.numClasses()];
// 	for(int i = 0; i < n; i++) {
// 	    out[i] = C.distributionForInstance(I.instance(i));
// 	}
// 	return(out);
//     }

    public double[] distributionForInstances(Classifier C, Instances I)
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

    public double[] distributionForInstances(Clusterer C, Instances I)
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
    
}
