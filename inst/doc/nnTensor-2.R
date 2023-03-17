## ----data, echo=TRUE----------------------------------------------------------
library("nnTensor")
X_Easy <- toyModel("siNMF_Easy")
X_Hard <- toyModel("siNMF_Hard")

## ----plot_data, echo=TRUE, fig.height=2.7, fig.width=8------------------------
layout(t(1:3))
image(X_Easy$X1, main="X1")
image(X_Easy$X2, main="X2")
image(X_Easy$X3, main="X3")

## ----plot_data2, echo=TRUE, fig.height=2.7, fig.width=8-----------------------
layout(t(1:3))
image(X_Hard$X1, main="X1")
image(X_Hard$X2, main="X2")
image(X_Hard$X3, main="X3")

## ----sinmf_easy, echo=TRUE----------------------------------------------------
set.seed(123456)
out_siNMF_Easy <- siNMF(X_Easy, algorithm="KL", J=3)
str(out_siNMF_Easy, 2)

## ----conv_sinmf_easy, echo=TRUE, fig.height=4, fig.width=8--------------------
layout(t(1:2))
plot(log10(out_siNMF_Easy$RecError[-1]), type="b", main="Reconstruction Error")
plot(log10(out_siNMF_Easy$RelChange[-1]), type="b", main="Relative Change")

## ----rec_sinmf_easy, echo=TRUE, fig.height=5.3, fig.width=8-------------------
recX1 <- out_siNMF_Easy$W %*% t(out_siNMF_Easy$H[[1]])
recX2 <- out_siNMF_Easy$W %*% t(out_siNMF_Easy$H[[2]])
recX3 <- out_siNMF_Easy$W %*% t(out_siNMF_Easy$H[[3]])

layout(rbind(1:3, 4:6))
image(X_Easy$X1, main="Original Data\n(X1)")
image(X_Easy$X2, main="Original Data\n(X2)")
image(X_Easy$X3, main="Original Data\n(X3)")
image(recX1, main="Reconstructed Data\n(X1, siNMF)")
image(recX2, main="Reconstructed Data\n(X2, siNMF)")
image(recX3, main="Reconstructed Data\n(X3, siNMF)")

## ----sinmf_hard, echo=TRUE----------------------------------------------------
set.seed(123456)
out_siNMF_Hard <- siNMF(X_Hard, algorithm="KL", J=3)
str(out_siNMF_Hard, 2)

## ----rec_sinmf_hard, echo=TRUE, fig.height=5.3, fig.width=8-------------------
recX1 <- out_siNMF_Hard$W %*% t(out_siNMF_Hard$H[[1]])
recX2 <- out_siNMF_Hard$W %*% t(out_siNMF_Hard$H[[2]])
recX3 <- out_siNMF_Hard$W %*% t(out_siNMF_Hard$H[[3]])
layout(rbind(1:3, 4:6))
image(X_Hard$X1, main="Original Data\n(X1)")
image(X_Hard$X2, main="Original Data\n(X2)")
image(X_Hard$X3, main="Original Data\n(X3)")
image(recX1, main="Reconstructed Data\n(X1, siNMF)")
image(recX2, main="Reconstructed Data\n(X2, siNMF)")
image(recX3, main="Reconstructed Data\n(X3, siNMF)")

## ----jnmf_hard, echo=TRUE-----------------------------------------------------
out_jNMF_Hard <- jNMF(X_Hard, algorithm="KL", J=3)
str(out_jNMF_Hard, 2)

## ----rec_jnmf_hard, echo=TRUE, fig.height=8, fig.width=8----------------------
recX1_common <- out_jNMF_Hard$W %*% t(out_jNMF_Hard$H[[1]])
recX2_common <- out_jNMF_Hard$W %*% t(out_jNMF_Hard$H[[2]])
recX3_common <- out_jNMF_Hard$W %*% t(out_jNMF_Hard$H[[3]])

recX1_specific <- out_jNMF_Hard$V[[1]] %*% t(out_jNMF_Hard$H[[1]])
recX2_specific <- out_jNMF_Hard$V[[2]] %*% t(out_jNMF_Hard$H[[2]])
recX3_specific <- out_jNMF_Hard$V[[3]] %*% t(out_jNMF_Hard$H[[3]])

layout(rbind(1:3, 4:6, 7:9))
image(X_Hard$X1, main="Original Data\n(X1)")
image(X_Hard$X2, main="Original Data\n(X2)")
image(X_Hard$X3, main="Original Data\n(X3)")
image(recX1_common, main="Reconstructed Data\n(Common Pattern in X1, jNMF)")
image(recX2_common, main="Reconstructed Data\n(Common Pattern in X2, jNMF)")
image(recX3_common, main="Reconstructed Data\n(Common Pattern in X3, jNMF)")
image(recX1_specific, main="Reconstructed Data\n(Specific Pattern in X1, jNMF)")
image(recX2_specific, main="Reconstructed Data\n(Specific Pattern in X2, jNMF)")
image(recX3_specific, main="Reconstructed Data\n(Specific Pattern in X3, jNMF)")

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

