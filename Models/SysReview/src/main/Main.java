/**
 * 
 */
package main;

import utils.Log;

/**
 * @author Raimbault Juste <br/> <a href="mailto:juste.raimbault@polytechnique.edu">juste.raimbault@polytechnique.edu</a>
 *
 */
public class Main {

	/**
	 * Tab of all references, stored as a list of their words
	 * 
	 * tab is dirty but ok since need to id !
	 */
	public static String[][] corpus;
	
	
	/**
	 * list of keywords to be searched in abstracts
	 */
	public static String[] keywords;
	
	/**
	 * Adjacency matrix of graph
	 */
	public static int[][] ocMatrix;
	
	
	
	public static void proceed(){
		//log
		Log.initLog();
		//parse bib file
		BibParser.parse("/Users/Juste/Documents/Cours/HSSVTT/Project/Data/corpus/bibtex/corpus.bib");
		//load keywords
		Processer.loadKeywords("/Users/Juste/Documents/Cours/HSSVTT/Project/Data/keywords/lastKeywords.csv");
		//compute matrix
		Processer.computeAdjacencyMatrix();
		//export graph
		//GraphProcesser.exportGraph("res/graph.gml",5);
		
		for(int i = 0 ; i< ocMatrix.length;i++){
			for(int j=0;j<ocMatrix[i].length;j++){
				System.out.print(ocMatrix[i][j]);
			}
			System.out.println();
		}
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		proceed();

	}

}
