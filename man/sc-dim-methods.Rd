% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sc-dim.R
\name{sc_dim}
\alias{sc_dim}
\alias{sc_dim,Seurat-method}
\alias{sc_dim,Seurat}
\alias{sc_dim,SingleCellExperiment-method}
\alias{sc_dim,SingleCellExperiment}
\title{sc_dim}
\usage{
sc_dim(
  object,
  dims = c(1, 2),
  reduction = NULL,
  cells = NULL,
  slot = "data",
  mapping = NULL,
  geom = sc_geom_point,
  ...
)

\S4method{sc_dim}{Seurat}(
  object,
  dims = c(1, 2),
  reduction = NULL,
  cells = NULL,
  slot = "data",
  mapping = NULL,
  geom = sc_geom_point,
  ...
)

\S4method{sc_dim}{SingleCellExperiment}(
  object,
  dims = c(1, 2),
  reduction = NULL,
  cells = NULL,
  slot = "data",
  mapping = NULL,
  geom = sc_geom_point,
  ...
)
}
\arguments{
\item{object}{Seurat object or SingleCellExperiment object}

\item{dims}{selected dimensions (must be a two-length vector) that
are used in visualization}

\item{reduction}{reduction method, default is NULL and will use the
default setting store in the object}

\item{cells}{selected cells to plot (default is all cells)}

\item{slot}{slot to pull expression data from (e.g., 'count' or 'data')}

\item{mapping}{aesthetic mapping, the \code{x} and \code{y} is set internally,
other character of geometric layer, such as \code{color}, \code{size},
\code{alpha} or (\code{shape} when geom = geom_point) can be set manually.}

\item{geom}{the function of geometric layer, default is sc_geom_point,
other geometric layer, such as \code{geom_bgpoint} or \code{geom_point} also works.}

\item{...}{additional parameters pass to \code{geom_scattermore2()}.
\itemize{
\item \code{bg_colour} the colour of background point, default is \code{NA}.
this character also can be set in \code{mappint}.
\item \code{gap_colour} the colour of gap background, default is \code{'white'}.
\item \code{bg_line_width} the line width of background point,
default is \code{.3}.
\item \code{gap_line_width} the gap line width of background point,
default is \code{.1}.
\item \code{alpha} the transparency of colour, default is 1.
\item \code{subset} subset the data frame which meet conditions to display.
this should be set in \code{mapping}.
}}
}
\value{
dimension reduction plot
}
\description{
sc_dim
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
p1 <- sc_dim(sce, reduction = 'UMAP', mapping = aes(colour = Cell_Cycle))
p2 <- sc_dim(sce, reduction = 'UMAP')
f1 <- p1 + sc_dim_geom_label()
f2 <- p2 + 
      sc_dim_geom_label(
        geom = shadowtext::geom_shadowtext,
        color='black',
        bg.color='white'
      )
}
\seealso{
\link[scattermore:geom_scattermore]{geom_scattermore};
}
