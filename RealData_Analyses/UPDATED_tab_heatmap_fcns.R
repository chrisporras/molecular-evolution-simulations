# Some functions for making figures
# Loretta Au, May 18, 2017

# Download this file and keep it in the same directory as where
# you run your R code.
# To tell your other source code to read this file ("load" in the functions)
# we will need to add in this line:
# source("fns_keap1.R");  # Change the file name argument if needed



# Create your own color palettes! :)
black.colors = colorRampPalette(c("gray50", "black"))(10);
	    
   # The colorRampPalette(); is a function in R that will create
   # a vector of colors from however many you provide (here, from 50% gray to black)
   # The number in the argument tells R how many colors to create
   # in the vector, which is 5 in this case

bluered.gradient = colorRampPalette(c("blue","white","red"))(20);

BlackoutMatrix = function(matrix){
   # "Black out" the matrix entries that have no data
   # Assign 0 if there is no data; assign 1 if there is data
   new.matrix = matrix(data=0, nrow=nrow(matrix), ncol=ncol(matrix));
   
   for(i in 1:nrow(matrix)){
   	 for(j in 1:ncol(matrix)){
	    if(is.na(matrix[i,j]) == FALSE){
	        new.matrix[i,j] = 1;   # If we have data, set to 1; otherwise leave as 0.
	    }
	 }
   }
   return(new.matrix);
}

# Example Usage:

## Use the function on our AA proportion matrix (assign it to something)

#  blackout.aaprop = BlackoutMatrix(aa.prop.matrix);  # this calls the function to work on our matrix
  
## Create a figure
#  image(t(aa.prop.matrix), breaks=seq(0,1,0.1), col = black.colors, axes=FALSE);  
    # NOTE: "breaks" sets the threshold for each color. The number of colors is always
    # one fewer than the number of breaks
#  image(t(blackout.aaprop), zlim=c(0,1), col=c("white", "black"), add=TRUE);  # Plot the 0-and-1 matrix on top




AA.Axis = function(my.side = 2, my.labels = aminoacids){
  # Default is to label the y-axis with aminoacids
  my.spacing = seq(0,1,length.out = length(my.labels));
  axis(side = my.side, labels=aminoacids, at = my.spacing);
}


Pos.axis = function(my.side = 1, my.labels){
  #x-axis
  my.spacing = seq(0,1,length=length(my.labels));
  axis(side = my.side, labels  = my.labels, at = my.spacing);
}

## Create a colorbar (separate image file, so will have to post-process
 #in Inkscape or Gimp)
#pdf("Figures/sample_colorbar.pdf",height=1.5,width=3);
#par(mar=c(5,1,1,1));
 #  image(as.matrix(seq(0,0.2,0.01), nrow=1), col=my.blues(20), axes=F);
  #  axis(1, at=c(0,1), labels=NA, tck=-0.01);
   # axis(1, at=seq(0,1,length=11), labels=c(seq(0,0.18,0.02), ">=0.2"), lwd=0, tck=-0.01, line=-0.8, las=2);
#box();
#dev.off();




# Get organism names from tab-delimited file and use them
# in FASTA file

# Example:
#    tabulated.filename = "Keap1_tab_OrthoDB.txt";
#    fasta.filename = "Keap1_fasta_OrthoDB.fa";

TabOrganisms = function(tabulated.filename, fasta.filename){
# Be sure that the 'seqinr' library has been loaded!
    tabulated.file = read.table(tabulated.filename, sep="\t", header=TRUE);
    fasta.file = read.fasta(fasta.filename, seqtype="AA");
    my.sequences = getSequence(fasta.file);
    my.organisms = tabulated.file$organism_name;

    new.fasta.filename = paste(fasta.filename, "new.fa", sep="_");
    write.fasta(file.out = new.fasta.filename, 
                sequences = my.sequences, names = my.organisms);

}


# Color scale example
postscript(file="colscale.eps", horizontal=FALSE,
         #height=3, width=1.4,  
	 height=1.4, width=3,
         paper="special");
  default.blues = colorRampPalette(c("white", blues9))
  
  par(mar=c(4,2,2,2)+0.1);
     dense = seq(0,35);  # How many colors are we representing?
   image(as.matrix(dense,ncol=1), col=c(default.blues(35)), breaks=dense, axes=F);
      axis(1, at = seq(0,1, length=length(seq(0,35,5))), # The 5 is for the spacing
              labels = seq(0, 35, 5),
	      las=2, cex.axis=1.2);
   box();
   dev.off();
