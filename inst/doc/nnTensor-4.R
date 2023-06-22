## ----package, echo=TRUE-------------------------------------------------------
library("nnTensor")
library("ggplot2")
library("dplyr")

## ----data, echo=TRUE----------------------------------------------------------
data_matrix <- toyModel("NMF")
data_matrices <- toyModel("siNMF_Easy")
data_tensor <- toyModel("CP")

## ----nmf_mask, echo=TRUE------------------------------------------------------
out_NMF <- expand.grid(replicate=1:3, rank=factor(1:10), value=0)
count <- 1
for(i in 1:10){
  masks_NMF <- kFoldMaskTensor(data_matrix, k=3)
  for(j in 1:3){
    out_NMF[count, 3] <- rev(
      NMF(data_matrix,
        M = masks_NMF[[j]],
        J = i)$TestRecError)[1]
    count <- count + 1
  }
}

## ----nmf_plot, echo=TRUE, fig.width=10, fig.height=4--------------------------
ggplot(out_NMF, aes(x=rank, y=value)) +
geom_point() +
stat_summary(fun = mean, geom = "point", shape=21, size=3, fill="blue") +
stat_summary(fun = mean, geom = "line", colour = "blue", aes(group=1)) +
xlab("Rank") +
ylab("Test Reconstruction Error")

## ----nmf_min, echo=TRUE-------------------------------------------------------
(group_by(out_NMF, rank) |>
  summarize(Avg = mean(value)) -> avg_test_error_NMF)
avg_test_error_NMF[which(avg_test_error_NMF$Avg == min(avg_test_error_NMF$Avg))[1], ]

## ----nmtf_mask, echo=TRUE, eval=FALSE-----------------------------------------
#  out_NMTF <- expand.grid(replicate=1:3, rank2=1:10, rank1=1:10, value=0)
#  rank_NMTF <- paste0(out_NMTF$rank1, "-", out_NMTF$rank2)
#  out_NMTF <- cbind(out_NMTF, rank=factor(rank_NMTF, levels=unique(rank_NMTF)))
#  count <- 1
#  for(i in 1:10){
#    for(j in 1:10){
#      masks_NMTF <- kFoldMaskTensor(data_matrix, k=3)
#      for(k in 1:3){
#        out_NMTF[count, 4] <- rev(
#          NMTF(data_matrix,
#            M = masks_NMTF[[k]],
#            rank = c(i, j))$TestRecError)[1]
#        count <- count + 1
#      }
#    }
#  }

## ----nmtf_plot, echo=TRUE, eval=FALSE, fig.width=10, fig.height=4-------------
#  ggplot(out_NMTF, aes(x=rank, y=value)) +
#  geom_point() +
#  stat_summary(fun = mean, geom = "point", shape=21, size=3, fill="blue") +
#  stat_summary(fun = mean, geom = "line", colour = "blue", aes(group=1)) +
#  xlab("Rank") +
#  ylab("Test Reconstruction Error") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----nmtf_min, echo=TRUE, eval=FALSE------------------------------------------
#  (out_NMTF |>
#    group_by(rank1, rank2) |>
#    summarize(Avg = mean(value)) -> avg_test_error_NMTF)
#  avg_test_error_NMTF[which(avg_test_error_NMTF$Avg == min(avg_test_error_NMTF$Avg))[1], ]

## ----sinmf_mask, echo=TRUE, eval=FALSE----------------------------------------
#  out_siNMF <- expand.grid(replicate=1:3, rank=factor(1:10), value=0)
#  count <- 1
#  for(i in 1:10){
#    masks_siNMF <- lapply(1:3, function(x){
#      list(
#        kFoldMaskTensor(data_matrices[[1]], k=3)[[x]],
#        kFoldMaskTensor(data_matrices[[2]], k=3)[[x]],
#        kFoldMaskTensor(data_matrices[[3]], k=3)[[x]])
#    })
#    for(j in 1:3){
#      out_siNMF[count, 3] <- rev(
#        siNMF(data_matrices,
#          M = masks_siNMF[[j]],
#          J = i)$TestRecError)[1]
#      count <- count + 1
#    }
#  }

## ----sinmf_plot, echo=TRUE, eval=FALSE, fig.width=10, fig.height=4------------
#  ggplot(out_siNMF, aes(x=rank, y=value)) +
#  geom_point() +
#  stat_summary(fun = mean, geom = "point", shape=21, size=3, fill="blue") +
#  stat_summary(fun = mean, geom = "line", colour = "blue", aes(group=1)) +
#  xlab("Rank") +
#  ylab("Test Reconstruction Error")

## ----sinmf_min, echo=TRUE, eval=FALSE-----------------------------------------
#  (out_siNMF |>
#    group_by(rank) |>
#    summarize(Avg = mean(value)) -> avg_test_error_siNMF)
#  avg_test_error_siNMF[which(avg_test_error_siNMF$Avg == min(avg_test_error_siNMF$Avg))[1], ]

## ----jnmf_mask, echo=TRUE, eval=FALSE-----------------------------------------
#  out_jNMF <- expand.grid(replicate=1:3, rank=factor(1:10), value=0)
#  count <- 1
#  for(i in 1:10){
#    masks_jNMF <- lapply(1:3, function(x){
#      list(
#        kFoldMaskTensor(data_matrices[[1]], k=3)[[x]],
#        kFoldMaskTensor(data_matrices[[2]], k=3)[[x]],
#        kFoldMaskTensor(data_matrices[[3]], k=3)[[x]])
#    })
#    for(j in 1:3){
#      out_jNMF[count, 3] <- rev(
#        jNMF(data_matrices,
#          M = masks_jNMF[[j]],
#          J = i)$TestRecError)[1]
#      count <- count + 1
#    }
#  }

## ----jnmf_plot, echo=TRUE, eval=FALSE, fig.width=10, fig.height=4-------------
#  ggplot(out_jNMF, aes(x=rank, y=value)) +
#  geom_point() +
#  stat_summary(fun = mean, geom = "point", shape=21, size=3, fill="blue") +
#  stat_summary(fun = mean, geom = "line", colour = "blue", aes(group=1)) +
#  xlab("Rank") +
#  ylab("Test Reconstruction Error")

## ----jnmf_min, echo=TRUE, eval=FALSE------------------------------------------
#  (out_jNMF |>
#    group_by(rank) |>
#    summarize(Avg = mean(value)) -> avg_test_error_jNMF)
#  avg_test_error_jNMF[which(avg_test_error_jNMF$Avg == min(avg_test_error_jNMF$Avg))[1], ]

## ----ntf_mask, echo=TRUE, eval=FALSE------------------------------------------
#  out_NTF <- expand.grid(replicate=1:3, rank=factor(1:10), value=0)
#  count <- 1
#  for(i in 1:10){
#    masks_NTF <- kFoldMaskTensor(data_tensor, k=3)
#    for(j in 1:3){
#      out_NTF[count, 3] <- rev(
#        NTF(data_tensor,
#          M = masks_NTF[[j]],
#          rank = i)$TestRecError)[1]
#      count <- count + 1
#    }
#  }

## ----ntf_plot, echo=TRUE, eval=FALSE, fig.width=10, fig.height=4--------------
#  ggplot(out_NTF, aes(x=rank, y=value)) +
#  geom_point() +
#  stat_summary(fun = mean, geom = "point", shape=21, size=3, fill="blue") +
#  stat_summary(fun = mean, geom = "line", colour = "blue", aes(group=1)) +
#  xlab("Rank") +
#  ylab("Test Reconstruction Error")

## ----ntf_min, echo=TRUE, eval=FALSE-------------------------------------------
#  (group_by(out_NTF, rank) |>
#    summarize(Avg = mean(value)) -> avg_test_error_NTF)
#  avg_test_error_NTF[which(avg_test_error_NTF$Avg == min(avg_test_error_NTF$Avg))[1], ]

## ----ntd_mask, echo=TRUE, eval=FALSE------------------------------------------
#  out_NTD <- expand.grid(replicate=1:3, rank3=1:5, rank2=1:5, rank1=1:5, value=0)
#  rank_NTD <- paste0(out_NTD$rank1, "-", out_NTD$rank2,
#    "-", out_NTD$rank3)
#  out_NTD <- cbind(out_NTD, rank=factor(rank_NTD, levels=unique(rank_NTD)))
#  count <- 1
#  for(i in 1:5){
#    for(j in 1:5){
#      for(k in 1:5){
#        masks_NTD <- kFoldMaskTensor(data_tensor, k=3)
#        for(k in 1:3){
#          out_NTD[count, 5] <- rev(
#            NTD(data_tensor,
#              M = masks_NTD[[k]],
#              rank = c(i, j, k))$TestRecError)[1]
#          count <- count + 1
#        }
#      }
#    }
#  }

## ----ntd_plot, echo=TRUE, eval=FALSE, fig.width=10, fig.height=4--------------
#  ggplot(out_NTD, aes(x=rank, y=value)) +
#  geom_point() +
#  stat_summary(fun = mean, geom = "point", shape=21, size=3, fill="blue") +
#  stat_summary(fun = mean, geom = "line", colour = "blue", aes(group=1)) +
#  xlab("Rank") +
#  ylab("Test Reconstruction Error") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----ntd_min, echo=TRUE, eval=FALSE-------------------------------------------
#  (out_NTD |>
#    group_by(rank1, rank2, rank3) |>
#    summarize(Avg = mean(value)) -> avg_test_error_NTD)
#  avg_test_error_NTD[which(avg_test_error_NTD$Avg == min(avg_test_error_NTD$Avg))[1], ]

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

