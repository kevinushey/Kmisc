\name{manhattan_plot}
\alias{manhattan_plot}
\title{Make a Manhattan Plot}
\usage{
  manhattan_plot(pval, bp, chr, groups = NULL,
    cutoff = NULL,
    xlab = "Chromosome (base-pair position)",
    ylab = expression(paste(-log[10](italic(p)))),
    transform = TRUE, ...)
}
\arguments{
  \item{pval}{A vector of p-values.}

  \item{bp}{A vector of base-pair positions, corresponding
  to the genomic location for which that p-value is
  associated (typically, the SNP location).}

  \item{chr}{The chromosomal location associated with the
  p-value.}

  \item{groups}{A groups vector: used if you want to
  overlay multiple manhattan plots.}

  \item{cutoff}{optional. By default, a Bonferroni cutoff
  line is drawn on the plot; if you want to plot a custom
  cut-off line you can specify the cutoff here.}

  \item{xlab}{The label to use for the x axis.}

  \item{ylab}{The label to use for the y axis.}

  \item{transform}{boolean; if \code{TRUE}, we compute
  \code{-log10(pval)}; otherwise, we use \code{pval} as-is,
  assuming that \code{pval} has been transformed
  accordingly.}

  \item{...}{Optional arguments passed to \code{xyplot}.}
}
\description{
  Generates a manhattan plot (a plot of
  \code{-log10(p-val)}) for a set of markers by chromosome
  and base-pair position.
}
\examples{
pval <- runif(1E4)
bp <- c(1:5E3, 1:5E3)
chr <- rep(1:22, length.out=1E4)
groups=rep( c("Phenotype 1", "Phenotype 2"), each=5E3 )
manhattan_plot( pval, bp, chr, groups )
manhattan_plot( pval, bp, chr )
}

