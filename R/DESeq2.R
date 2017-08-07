source("https://bioconductor.org/biocLite.R")
biocLite("DESeq2")
###install pasilla package
# source("https://bioconductor.org/biocLite.R") 
# biocLite("pasilla")


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

condition = pasillaDesign$condition
condition
###instantiate a CountDataSet
# source("https://bioconductor.org/biocLite.R")
# biocLite("DESeq2")
library( "DESeq2" )
dds <- DESeqDataSetFromMatrix(countData = pasillaCountTable,
                              colData =pasillaDesign,
                              design = ~ condition)
dds <- DESeq(dds)

res <- results(dds)

resOrdered <- res[order(res$padj),]
summary(res)
sum(res$padj < 0.01, na.rm=TRUE)
plotMA(res, ylim=c(-2,2))
####save the analysis results
write.csv(as.data.frame(resOrdered), 
          file="condition_treated_results.csv")
###save significant results only
resSig <- subset(resOrdered, padj < 0.1)
write.csv(as.data.frame(resSig), 
          file="sig_results.csv")



####Multi-factor designs
colData(dds)
ddsMF <- dds
design(ddsMF) <- formula(~ libType + condition)
ddsMF <- DESeq(ddsMF)
resMF <- results(ddsMF)

write.csv(resMF[order(resMF$padj),], 'condition_libtype.csv')


###visulation-heatmap of differentially expressed genes
library("pheatmap")
select <- order(rowMeans(counts(dds,normalized=TRUE)),
                decreasing=TRUE)[1:20]

select <- order(res$padj)[1:20]


df <- as.data.frame(colData(dds)[,c("condition","libType")])
pheatmap(log2(assay(dds)+1)[select,], cluster_rows=TRUE, show_rownames=TRUE,
         cluster_cols=FALSE, annotation_col=df, cellwidth = 20)

###visulation-sample to sample distance
rld <- rlog(dds, blind=FALSE)
sampleDists <- dist(t(assay(rld)))

library("RColorBrewer")
sampleDistMatrix <- as.matrix(sampleDists)
rownames(sampleDistMatrix) <- paste(rld$condition, rld$type, sep="-")
colnames(sampleDistMatrix) <- NULL
colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
pheatmap(sampleDistMatrix,
         clustering_distance_rows=sampleDists,
         clustering_distance_cols=sampleDists,
         col=colors, cellheight = 20, cellwidth = 20)

###PCA plot
plot(
prcomp(t(head(assay(rld))))$x[,2],
prcomp(t(head(assay(rld))))$x[,1]
)

plotPCA(rld, intgroup=c("condition", "libType"))


####DEATILS 
#####Normalisation
dds= estimateSizeFactors(dds)
sizeFactors(dds)
#(Genes with large counts may distort the result)
#(only holds when expression of most genes stay the same)
####Variance estimation
dds = estimateDispersions( dds )
plotDispEsts( dds )

###that the variance seen between counts is 
###the sum of two components: the sample-to-sample 
###variation, and the uncertainty in measuring a
###concentration by counting reads.

####Inference: Calling dierential expression
dds <- nbinomWaldTest(dds)
results(dds, contrast=c("condition","treated","untreated"))

