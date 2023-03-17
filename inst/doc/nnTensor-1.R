## ----data, echo=TRUE----------------------------------------------------------
library("nnTensor")
X <- toyModel("NMF")

## ----data2, echo=TRUE, fig.height=4, fig.width=4------------------------------
image(X, main="Original Data")

## ----nmf, echo=TRUE-----------------------------------------------------------
set.seed(123456)
out_NMF <- NMF(X, J=5)
str(out_NMF, 2)

## ----conv_nmf, echo=TRUE, fig.height=4, fig.width=8---------------------------
layout(t(1:2))
plot(log10(out_NMF$RecError[-1]), type="b", main="Reconstruction Error")
plot(log10(out_NMF$RelChange[-1]), type="b", main="Relative Change")

## ----rec_nmf, echo=TRUE, fig.height=4, fig.width=8----------------------------
recX <- out_NMF$U %*% t(out_NMF$V)
layout(t(1:2))
image(X, main="Original Data")
image(recX, main="Reconstructed Data (NMF)")

## ----nmf2, echo=TRUE----------------------------------------------------------
set.seed(123456)
out_NMF2 <- NMF(X, J=1:10, num.iter=1)
str(out_NMF2, 2)

## ----plot_nmf2, echo=TRUE, fig.height=4, fig.width=12-------------------------
plot(out_NMF2)

## ----nmtf, echo=TRUE----------------------------------------------------------
set.seed(123456)
out_NMTF <- NMTF(X, rank=c(4,5))
str(out_NMTF, 2)

## ----conv_nmtf, echo=TRUE, fig.height=4, fig.width=8--------------------------
layout(t(1:2))
plot(log10(out_NMTF$RecError[-1]), type="b", main="Reconstruction Error")
plot(log10(out_NMTF$RelChange[-1]), type="b", main="Relative Change")

## ----rec_nmtf, echo=TRUE, fig.height=4, fig.width=8---------------------------
recX2 <- out_NMTF$U %*% out_NMTF$S %*% t(out_NMTF$V)
layout(t(1:2))
image(X, main="Original Data")
image(recX2, main="Reconstructed Data (NMTF)")

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

