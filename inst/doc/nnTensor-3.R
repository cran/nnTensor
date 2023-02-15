## ----data, echo=TRUE----------------------------------------------------------
library("nnTensor")
X_CP <- toyModel("CP")
X_Tucker <- toyModel("Tucker")

## ----plot_data, echo=TRUE, fig.height=4, fig.width=4--------------------------
plotTensor3D(X_CP)

## ----plot_data2, echo=TRUE, fig.height=4, fig.width=4-------------------------
plotTensor3D(X_Tucker)

## ----ntf_cp, echo=TRUE--------------------------------------------------------
set.seed(123456)
out_NTF_CP <- NTF(X_CP, algorithm="KL", rank=4)
str(out_NTF_CP, 2)

## ----conv_ntf_cp, echo=TRUE, fig.height=4, fig.width=8------------------------
layout(t(1:2))
plot(log10(out_NTF_CP$RecError[2:101]), type="b", main="Reconstruction Error")
plot(log10(out_NTF_CP$RelChange[2:101]), type="b", main="Relative Change")

## ----rec_ntf_cp, echo=TRUE, fig.height=4, fig.width=8-------------------------
recX_CP <- recTensor(out_NTF_CP$S, out_NTF_CP$A, idx=1:3)
layout(t(1:2))
plotTensor3D(X_CP)
plotTensor3D(recX_CP)

## ----ntf_tucker, echo=TRUE----------------------------------------------------
set.seed(123456)
out_NTF_Tucker <- NTF(X_Tucker, algorithm="KL", rank=4)
str(out_NTF_Tucker, 2)

## ----conv_ntf_tucker, echo=TRUE, fig.height=4, fig.width=8--------------------
layout(t(1:2))
plot(log10(out_NTF_Tucker$RecError[2:101]), type="b", main="Reconstruction Error")
plot(log10(out_NTF_Tucker$RelChange[2:101]), type="b", main="Relative Change")

## ----rec_ntf_tucker, echo=TRUE, fig.height=4, fig.width=8---------------------
recX_Tucker <- recTensor(out_NTF_Tucker$S, out_NTF_Tucker$A, idx=1:3)
layout(t(1:2))
plotTensor3D(X_Tucker)
plotTensor3D(recX_Tucker)

## ----ntd, echo=TRUE-----------------------------------------------------------
set.seed(123456)
out_NTD <- NTD(X_Tucker, rank=c(3,4,5))
str(out_NTD, 2)

## ----conv_ntd, echo=TRUE, fig.height=4, fig.width=8---------------------------
layout(t(1:2))
plot(out_NTD$RecError[2:101], type="b", main="Reconstruction Error")
plot(out_NTD$RelChange[2:101], type="b", main="Relative Change")

## ----rec_ntd_tucker, echo=TRUE, fig.height=4, fig.width=8---------------------
recX_Tucker2 <- recTensor(out_NTD$S, out_NTD$A, idx=1:3)
layout(t(1:2))
plotTensor3D(X_Tucker)
plotTensor3D(recX_Tucker2)

## ----ntd2, echo=TRUE----------------------------------------------------------
set.seed(123456)
out_NTD2_1 <- NTD(X_Tucker, rank=c(3,4), modes=1:2)
out_NTD2_2 <- NTD(X_Tucker, rank=c(3,5), modes=c(1,3))
out_NTD2_3 <- NTD(X_Tucker, rank=c(4,5), modes=2:3)

## ----conv_ntd2, echo=TRUE, fig.height=12, fig.width=8-------------------------
layout(rbind(1:2, 3:4, 5:6))
plot(out_NTD2_1$RecError[2:101], type="b", main="Reconstruction Error\nNTD-2 (mode=1:2)")
plot(out_NTD2_1$RelChange[2:101], type="b", main="Relative Change\nNTD-2 (mode=1:2)")
plot(out_NTD2_2$RecError[2:101], type="b", main="Reconstruction Error\nNTD-2 (mode=c(1,3))")
plot(out_NTD2_2$RelChange[2:101], type="b", main="Relative Change\nNTD-2 (mode=c(1,3))")
plot(out_NTD2_3$RecError[2:101], type="b", main="Reconstruction Error\nNTD-2 (mode=2:3)")
plot(out_NTD2_3$RelChange[2:101], type="b", main="Relative Change\nNTD-2 (mode=2:3)")

## ----rec_ntd_tucker2, echo=TRUE, fig.height=8, fig.width=8--------------------
recX_Tucker2_1 <- recTensor(out_NTD2_1$S, out_NTD2_1$A, idx=1:3)
recX_Tucker2_2 <- recTensor(out_NTD2_2$S, out_NTD2_2$A, idx=1:3)
recX_Tucker2_3 <- recTensor(out_NTD2_3$S, out_NTD2_3$A, idx=1:3)
layout(rbind(1:2, 3:4))
plotTensor3D(X_Tucker)
plotTensor3D(recX_Tucker2_1)
plotTensor3D(recX_Tucker2_2)
plotTensor3D(recX_Tucker2_3)

## ----ntd1, echo=TRUE----------------------------------------------------------
set.seed(123456)
out_NTD1_1 <- NTD(X_Tucker, rank=3, modes=1)
out_NTD1_2 <- NTD(X_Tucker, rank=4, modes=2)
out_NTD1_3 <- NTD(X_Tucker, rank=5, modes=3)

## ----conv_ntd1, echo=TRUE, fig.height=12, fig.width=8-------------------------
layout(rbind(1:2, 3:4, 5:6))
plot(out_NTD1_1$RecError[2:101], type="b", main="Reconstruction Error\nNTD-1 (mode=1:2)")
plot(out_NTD1_1$RelChange[2:101], type="b", main="Relative Change\nNTD-1 (mode=1:2)")
plot(out_NTD1_2$RecError[2:101], type="b", main="Reconstruction Error\nNTD-1 (mode=c(1,3))")
plot(out_NTD1_2$RelChange[2:101], type="b", main="Relative Change\nNTD-1 (mode=c(1,3))")
plot(out_NTD1_3$RecError[2:101], type="b", main="Reconstruction Error\nNTD-1 (mode=2:3)")
plot(out_NTD1_3$RelChange[2:101], type="b", main="Relative Change\nNTD-1 (mode=2:3)")

## ----rec_ntd_tucker1, echo=TRUE, fig.height=8, fig.width=8--------------------
recX_Tucker2_1 <- recTensor(out_NTD1_1$S, out_NTD1_1$A, idx=1:3)
recX_Tucker2_2 <- recTensor(out_NTD1_2$S, out_NTD1_2$A, idx=1:3)
recX_Tucker2_3 <- recTensor(out_NTD1_3$S, out_NTD1_3$A, idx=1:3)
layout(rbind(1:2, 3:4))
plotTensor3D(X_Tucker)
plotTensor3D(recX_Tucker2_1)
plotTensor3D(recX_Tucker2_2)
plotTensor3D(recX_Tucker2_3)

## ----data2, echo=TRUE---------------------------------------------------------
library("nnTensor")
library("rTensor")
X_Higher <- as.tensor(array(runif(3*4*5*6*7), dim=c(3,4,5,6,7)))
out_NTF_Higher <- NTF(X_Higher, rank=4)
out_NTD_Higher <- NTD(X_Higher, rank=c(1,2,1,2,3))

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

