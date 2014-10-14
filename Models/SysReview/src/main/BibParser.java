/**
 * 
 */
package main;

import java.io.FileReader;
import java.io.Reader;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

import utils.Log;

/**
 * @author Raimbault Juste <br/> <a href="mailto:juste.raimbault@polytechnique.edu">juste.raimbault@polytechnique.edu</a>
 *
 * Class for Parsing the bibtex file and put titles and abstracts into static set.
 */
public class BibParser {
	
	/**
	 * Main parser : reads file and fill table.
	 * 
	 * @param file
	 */
	public static void parse(String file){
		Log.output("Parsing Bibtex file : "+file);
		try {
			Reader reader = new FileReader(file);
			org.jbibtex.BibTeXParser bibtexParser = new org.jbibtex.BibTeXParser();
			org.jbibtex.BibTeXDatabase database = bibtexParser.parse(reader);
			Map<org.jbibtex.Key, org.jbibtex.BibTeXEntry> entryMap = database.getEntries();

			Collection<org.jbibtex.BibTeXEntry> entries = entryMap.values();
			
			
			//org.jbibtex.LaTeXParser latexParser = new org.jbibtex.LaTeXParser();
			//org.jbibtex.LaTeXPrinter latexPrinter = new org.jbibtex.LaTeXPrinter();

			//initialize corpus
			Main.corpus = new String[entries.size()][];
			
			int i =0;
			for(org.jbibtex.BibTeXEntry entry : entries){
				
				try{
					String[] t = entry.getField(new org.jbibtex.Key("abstract")).toUserString().split(" ");
					Main.corpus[i] = t ;
					//transate better if LateX special chars ?
					//System.out.println(latexPrinter.print(latexParser.parse(entry.getField(org.jbibtex.BibTeXEntry.KEY_TITLE).toUserString())));
				}catch(Exception e){Main.corpus[i]=new String[0];}
				i++;
			}
			
			
		} catch (Exception e) {
			Log.fail(e.toString());
		}
	}
	
	
	
	
	
	
}
