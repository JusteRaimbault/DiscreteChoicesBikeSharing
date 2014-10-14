package test;

import java.io.FileReader;
import java.io.Reader;
import java.util.Collection;
import java.util.Map;

/**
 * 
 */

/**
 * @author Raimbault Juste <br/> <a href="mailto:juste.raimbault@polytechnique.edu">juste.raimbault@polytechnique.edu</a>
 *
 */
public class TestJBibtex {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			Reader reader = new FileReader("data/exportlist.bib");
			org.jbibtex.BibTeXParser bibtexParser = new org.jbibtex.BibTeXParser();
			org.jbibtex.BibTeXDatabase database = bibtexParser.parse(reader);
			Map<org.jbibtex.Key, org.jbibtex.BibTeXEntry> entryMap = database.getEntries();

			Collection<org.jbibtex.BibTeXEntry> entries = entryMap.values();
			for(org.jbibtex.BibTeXEntry entry : entries){
				try{
					
			    System.out.println(entry.getField(org.jbibtex.BibTeXEntry.KEY_AUTHOR).toUserString());
				}catch(Exception e){e.printStackTrace();}
			}
			
			org.jbibtex.LaTeXParser latexParser = new org.jbibtex.LaTeXParser();
			org.jbibtex.LaTeXPrinter latexPrinter = new org.jbibtex.LaTeXPrinter();

			for(org.jbibtex.BibTeXEntry entry : entries){
			    System.out.println(latexPrinter.print(latexParser.parse(entry.getField(org.jbibtex.BibTeXEntry.KEY_AUTHOR).toUserString())));
			}
			
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
