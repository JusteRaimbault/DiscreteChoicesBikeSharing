/**
 * 
 */
package main;

import java.io.IOException;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Graph;
import org.graphstream.graph.implementations.SingleGraph;
import org.graphstream.stream.file.FileSinkGML;

/**
 * @author Raimbault Juste <br/> <a href="mailto:juste.raimbault@polytechnique.edu">juste.raimbault@polytechnique.edu</a>
 *
 */
public class GraphProcesser {

	
	/**
	 * Creation and export of graph.
	 * 
	 * Needs to be weighted.
	 * 
	 * @param out
	 */
	public static void exportGraph(String out, int minWeight){
		//graph creation
		Graph graph = new SingleGraph("Graph");
		for(int t=0;t<Main.ocMatrix.length;t++){
			graph.addNode(t+"");
		}
		for(int i=0;i<Main.ocMatrix.length;i++){
			for(int j=i+1;j<Main.ocMatrix.length;j++){
				if(Main.ocMatrix[i][j]>minWeight)graph.addEdge(""+i+"_"+j,""+i,""+j);
			}
		}
		for(Edge e:graph.getEdgeSet()){e.setAttribute("weight", new Double(1+Main.ocMatrix[Integer.parseInt(e.getSourceNode().getId())][Integer.parseInt(e.getTargetNode().getId())]));}
		try {
			new FileSinkGML().writeAll(graph,out);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
}
