\name{plotTensor3D}
\docType{methods}
\alias{plotTensor3D}

\title{
Plot function for visualization of tensor data structure
}
\description{
Combined with recTensor function and the result of NTF or NTD, the reconstructed tensor structure can be visullized.
}
\usage{
plotTensor3D(X = NULL, method=c("sd", "mad"),
  sign=c("positive", "negative", "both"), thr=2)
}

\arguments{
  \item{X}{
Tensor object, which is defined as S4 class of rTensor package.
  }
  \item{method}{
Cutoff method to focus on large/small value in the tensor data (Default: "sd").
  }
  \item{sign}{
Direction to cutoff the large/small value in the tensor data (Default: "positive").
  }
  \item{thr}{
Threshold of cutoff method (Default: 2).
  }
}
\author{Koki Tsuyuzaki}

\examples{
tensordata <- toyModel(model = "CP")

out <- NTF(tensordata, rank=3, algorithm="Beta-HALS", num.iter=2)

tmp <- tempdir()

png(filename=paste0(tmp, "/NTF.png"))
plotTensor3D(recTensor(out$S, out$A))
dev.off()
}

\keyword{methods}