###install pasilla package
source("https://bioconductor.org/biocLite.R") 
biocLite("pasilla")

###Load count table
library(pasilla)
datafile = system.file( "extdata/pasilla_gene_counts.tsv", package="pasilla" )
pasillaCountTable = read.table( datafile, header=TRUE, row.names=1 )
head( pasillaCountTable )


###a description of the samples, a design matrix
pasillaDesign = data.frame(
   row.names = colnames( pasillaCountTable ),
   condition = c( "untreated", "untreated", "untreated",
                 "untreated", "treated", "treated", "treated" ),
   libType = c( "single-end", "single-end", "paired-end",
                 "paired-end", "single-end", "paired-end", "paired-end" ) )
pasillaDesign

###Now we use only paired-end data
pairedSamples = pasillaDesign$libType == "paired-end"
countTable = pasillaCountTable[ , pairedSamples ]
condition = pasillaDesign$condition[ pairedSamples ]
head(countTable)
condition
###instantiate a CountDataSet
library( "DESeq" )
cds = newCountDataSet( countTable, condition )


#####Normalisation
cds = estimateSizeFactors( cds )
sizeFactors( cds )
#(Genes with large counts may distort the result)
#(only holds when expression of most genes stay the same)

####Variance estimation
cds = estimateDispersions( cds )
plotDispEsts( cds )

###that the variance seen between counts is 
###the sum of two components: the sample-to-sample 
###variation, and the uncertainty in measuring a
###concentration by counting reads.

####Inference: Calling dierential expression
res = nbinomTest( cds, "untreated", "treated" )
head(res)
plotMA(res)
ordered_res=res[order(res$padj),]


####save the analysis results
ordered_res=ordered_res[ordered_res$padj<0.01,]
write.csv(ordered_res, file="My Pasilla Analysis Result Table.csv" )

###visulation-heatmap of differentially expressed genes
library(RColorBrewer)
hmcol = colorRampPalette(brewer.pal(9, "GnBu"))(100)
diff_counts=pasillaCountTable[res$padj<=0.001 & !is.na(res$padj),]
pheatmap(log2(diff_counts+1), show_rownames = FALSE, color=hmcol, 
         cellwidth = 20)


###visulation-sample to sample distance
dists = dist( t(log(pasillaCountTable+1)))
mat = as.matrix( dists )
pheatmap(mat, col=hmcol )
pasillaDesign

###PCA plot
sample_PCA=prcomp(t(log(pasillaCountTable+1)))
head(sample_PCA)
library(ggplot2)
library(reshape2)
df=sample_PCA$x
df=data.frame(df[,1:2])
df$sample=substr(rownames(df),1,nchar(rownames(df))-1)
ggplot(df, aes(PC1, PC2, col=sample))+
  geom_point()
