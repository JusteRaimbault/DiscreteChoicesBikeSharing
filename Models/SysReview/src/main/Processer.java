/**
 * 
 */
package main;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.util.LinkedList;

import org.apache.commons.lang3.StringUtils;

import utils.Log;

/**
 * @author Raimbault Juste <br/> <a href="mailto:juste.raimbault@polytechnique.edu">juste.raimbault@polytechnique.edu</a>
 *
 */
public class Processer {

	
	/**
	 * Computation of adjacency matrix.
	 * 
	 * Beware of complexity -> O(n2) is too much.
	 * Should buy set of occuring keywords for each text, then compute the matrix.
	 */
	public static void computeAdjacencyMatrix(){
		int n = Main.keywords.length;
		Main.ocMatrix = new int[n][n];
		for(int i = 0;i<n;i++){for(int j=0;j<n;j++){Main.ocMatrix[i][j]=0;}}
		//build keyword matrix for the corpus ; remember ids
		int[][] keywords = new int[Main.corpus.length][];
		for(int t=0;t<Main.corpus.length;t++){
			Log.output("Calculating occurences for text "+t);
			LinkedList<Integer> l = new LinkedList<Integer>();
			for(int i = 0;i<n;i++){
				if(occurs(Main.keywords[i],Main.corpus[t])){l.add(new Integer(i));}
			}
			keywords[t]=new int[l.size()];int i=0;for(Integer k:l){keywords[t][i]=k.intValue();i++;}
		}
		
		for(int t=0;t<Main.corpus.length;t++){
			int[] k = keywords[t];
			for(int i=0;i<k.length;i++){
				for(int j=i+1;j<k.length;j++){Main.ocMatrix[k[i]][k[j]]=Main.ocMatrix[k[i]][k[j]]+1;}
			}
		}
		
	}
	
	
	public static void loadKeywords(String file){
		Log.output("Loading Keywords from file "+file);
		LinkedList<String> keywords = new LinkedList<String>();
		try {
			BufferedReader r = new BufferedReader( new FileReader(new File(file)));
			String currentLine = r.readLine();
			while(currentLine != null){keywords.add(currentLine);currentLine=r.readLine();}
		} catch (Exception e) {
			e.printStackTrace();
		}
		Main.keywords=keywords.toArray(new String[0]);
	}
	
	

	/**
	 * Number of occurence of a stem in text.
	 * 
	 * needs an heuristic against garbages words --> take length + cste and look at levenstein distance ?
	 * take successives words ; test against length
	 * 
	 * @param word
	 * @param text
	 * @return
	 */
	public static boolean occurs(String word, String[] text){
		boolean res = false;
		String[] wt = word.split(" "); String w = "";
		for(int j =0 ; j<wt.length;j++){w+=wt[j];}
		String currentTextWord;
		for(int i=0;i<text.length - (wt.length + 2);i++){
			currentTextWord = "";
			for(int j = 0 ; j <wt.length + 2;j++){currentTextWord+=(text[i+j]);}
			int d = StringUtils.getLevenshteinDistance(StringUtils.lowerCase(currentTextWord) , StringUtils.lowerCase(w));
			res = res || (d <= Math.abs(w.length()-currentTextWord.length()))  ;
		}
		return res;
	}
	
	
	
}
