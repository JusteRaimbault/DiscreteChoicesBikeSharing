/**
 * 
 */
package main;

import java.util.HashSet;

import org.graphstream.graph.Graph;
import org.graphstream.graph.Edge;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.SingleGraph;
import org.graphstream.stream.Sink;
import org.graphstream.stream.file.FileSinkGML;
import org.graphstream.stream.file.FileSource;
import org.graphstream.stream.file.FileSourceFactory;
import org.graphstream.ui.layout.Layout;
import org.graphstream.ui.layout.springbox.BarnesHutLayout;
import org.graphstream.ui.layout.springbox.implementations.SpringBox;
import org.graphstream.ui.swingViewer.View;
import org.graphstream.ui.swingViewer.Viewer;

/**
 * @author Raimbault Juste <br/> <a href="mailto:juste.raimbault@polytechnique.edu">juste.raimbault@polytechnique.edu</a>
 *
 */
public class Visualization {
	
	public static void visualize(String graphFile,double threshold){
		Graph graph = new SingleGraph("Graph");
		try{
		FileSource fs = FileSourceFactory.sourceFor(graphFile);
		fs.addSink(graph);
		try {
			
		  fs.readAll(graphFile);
		} catch( Exception e) {e.printStackTrace();}
		finally {
		  fs.removeSink(graph);
		  System.out.println("Total graph : "+graph.getEdgeCount()+" edges");
		  
		  int nEdges = graph.getEdgeCount();

		  //normalize graph
		  int maxWeight = 0;
		  for(Edge e:graph.getEdgeSet()){maxWeight = Math.max(((Integer)e.getAttribute("weight")).intValue(),maxWeight);}
		  

		  System.out.println("Max weight : "+maxWeight);

		  for(Edge e:graph.getEdgeSet()){e.setAttribute("weight", new Double((double)((Integer)e.getAttribute("weight")).intValue() / (double)maxWeight));}
		  		  
		  int c=0;
		  while(graph.getEdgeIterator().hasNext()&&c<nEdges){
			  Edge e = graph.getEdgeIterator().next();
			  if(((Double)e.getAttribute("weight")).doubleValue()<threshold){graph.removeEdge(e);}
			  c++;
		 }
		  	  
		  System.out.println("Reduced : "+graph.getEdgeCount()+" edges");
		  
		  
		  //remove unlinked edges
		  while(graph.getNodeIterator().hasNext()){
			  Node n = graph.getNodeIterator().next();
			  if(n.getEdgeSet().size()==0)graph.removeNode(n);
		  }
		  
		  System.out.println("New Nodes number : "+graph.getNodeCount());
		  
		  new FileSinkGML().writeAll(graph,"res/reduced.gml");
		  
		  Viewer viewer = graph.display(false);
		  //View view = viewer.getDefaultView();
		  //Layout lay = new SpringBox();System.out.println(lay.getStabilizationLimit());
		  //lay.setStabilizationLimit(0);
		  //graph.addSink(lay);
		  //lay.compute();
		  
		  //System.out.println(view.getBounds().toString());
		  //view.getCamera().setAutoFitView(false);
		  //System.out.println(view.getBounds().toString());

		}
		}catch(Exception e){e.printStackTrace();}
	}
	
	public static void main(String[] args){
		visualize("res/graph.gml",1);
	}
	
}
