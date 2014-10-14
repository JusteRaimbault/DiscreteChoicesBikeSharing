/**
 * 
 */
package test;

import main.Processer;
import main.BibParser;

/**
 * @author Raimbault Juste <br/> <a href="mailto:juste.raimbault@polytechnique.edu">juste.raimbault@polytechnique.edu</a>
 *
 */
public class Test {

	
	/**
	 * occurecnes seems quite ok.
	 */
	public static void testoccurences(){
		String w = "socio economic";
		String t = "This paper revisits the City Profile for Delhi, the first article ever published in Cities in 1983 (Datta, 1983). Thirty years later and following the centennial anniversary year of Delhi's establishment as the capital of India in 2012, this article makes a wide-ranging survey of Delhi in the administrative, socio-economic and environmental arenas. By tracing the history of urban planning in the city to the present and examining the issues facing Delhi, we then critically examine its institutional arrangements with respect to the outcomes of recent developments that have occurred in the city. These aspects are then evaluated in the context of the future development of the city; a city which still faces numerous local challenges but also houses the government of an emerging superpower that will play an increasing role both regionally and globally. (c) 2013 Elsevier Ltd. All rights reserved.";
		System.out.println(Processer.occurs(w, t.split(" ")));
	}
	
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		testoccurences();

	}

}
