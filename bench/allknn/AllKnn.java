/**
 * This file is modified from https://github.com/zoq/benchmarks/blob/master/methods/weka/allknn.py
 */

import java.io.*;
import weka.core.*;
import weka.core.neighboursearch.KDTree;
import weka.core.converters.ConverterUtils.DataSource;

/**
 * This class use the weka libary to implement All K-Nearest-Neighbors.
 */
public class AllKnn {
	private static final String USAGE = String
	.format(" This program will calculate the all k-nearest-neighbors of a set\n"
	    + "of points using kd-trees. You may specify a separate set of\n"
	    + "reference points and query points, or just a reference set which\n"
	    + "will be used as both the reference and query set.\n\n"
			+ "Required options:\n"
			+ "-r [string]     File containing the reference dataset.\n"
			+ "-k [int]        Number of furthest neighbors to find.\n\n"
			+ "Options:\n"
			+ "-l [int]        Leaf size for tree building.  Default value 20.\n"
			+ "-q [string]     File containing query points (optional).\n"
			+ "                Default value ''.\n");

	public static void main(String args[]) {
        //Timers timer = new Timers();
        try {
			// Get the data set path.
			String referenceFile = Utils.getOption('r', args);
			String queryFile = Utils.getOption('q', args);
			if (referenceFile.length() == 0)
				throw new IllegalArgumentException("Required option: File containing" +
						"the reference dataset.");

			// Load input dataset.
			DataSource source = new DataSource(referenceFile);
			Instances referenceData = source.getDataSet();

			Instances queryData = null;
			if (queryFile.length() != 0)
			{
				source = new DataSource(queryFile);
				queryData = source.getDataSet();
			}

            //timer.StartTimer("total_time");

			// Get all the parameters.
			String leafSize = Utils.getOption('l', args);
			String neighbors = Utils.getOption('k', args);

			// Validate options.
			int k = 0;
			if (neighbors.length() == 0)
			{
				throw new IllegalArgumentException("Required option: Number of " +
						"furthest neighbors to find.");
			}
			else
			{
				k = Integer.parseInt(neighbors);
				if (k < 1 || k > referenceData.numInstances())
					throw new IllegalArgumentException("[Fatal] Invalid k");
			}

			int l = 20;
			if (leafSize.length() != 0)
				l = Integer.parseInt(leafSize);

			// Create KDTree.
			KDTree tree = new KDTree();
			tree.setMaxInstInLeaf(l);
			tree.setInstances(referenceData);

			// Perform All K-Nearest-Neighbors.
			if (queryFile.length() != 0)
			{
				for (int i = 0; i < queryData.numInstances(); i++)
				{
					Instances out = tree.kNearestNeighbours(queryData.instance(i), k);
				}
			}
			else
			{
				for (int i = 0; i < referenceData.numInstances(); i++)
				{
					Instances out = tree.kNearestNeighbours(referenceData.instance(i), k);
				}
			}

            //timer.StopTimer("total_time");
            //timer.PrintTimer("total_time");
        } catch (IOException e) {
            System.err.println(USAGE);
        } catch (Exception e) {
            e.printStackTrace();
        }
	}
}
