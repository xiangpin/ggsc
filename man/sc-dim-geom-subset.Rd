% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sc-dim-utilities.R
\name{sc_dim_geom_sub}
\alias{sc_dim_geom_sub}
\title{sc_dim_geom_subset}
\usage{
sc_dim_geom_sub(mapping = NULL, subset, .column = "ident", ...)
}
\arguments{
\item{mapping}{aesthetic mapping}

\item{subset}{subset of clusters to be displayed}

\item{.column}{which column represents cluster (e.g., 'ident')}

\item{...}{additional parameters pass to sc_geom_point}
}
\value{
plot with a layer of specified clusters
}
\description{
sc_dim_geom_subset
}
\examples{
library(scuttle)
library(scater)
library(scran)
library(ggplot2)
sce <- mockSCE()
sce <- logNormCounts(sce)
clusters <- clusterCells(sce, assay.type = 'logcounts')
colLabels(sce) <- clusters
sce <- runUMAP(sce, assay.type = 'logcounts')
p1 <- sc_dim(sce, reduction = 'UMAP')
f1 <- p1 + sc_dim_geom_sub(subset = c(1, 2), .column = 'label', bg_colour='black')
}
\seealso{
\link{sc_dim_geom_sub}
}
