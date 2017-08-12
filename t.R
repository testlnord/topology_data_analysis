library(TDA)
# sudo apt install libmpfr-dev
# sudo apt install libgmp-dev


data("iris")

sl.lim <- c(min(iris$Sepal.Length), max(iris$Sepal.Length))
sw.lim <- c(min(iris$Sepal.Width), max(iris$Sepal.Width))
pl.lim <- c(min(iris$Petal.Length), max(iris$Petal.Length))
pw.lim <- c(min(iris$Petal.Width), max(iris$Petal.Width))
by <- 0.1

sl.seq <- seq(sl.lim[1], sl.lim[2], by=by)
sw.seq <- seq(sw.lim[1], sw.lim[2], by=by)
pl.seq <- seq(pl.lim[1], pl.lim[2], by=by)
pw.seq <- seq(pw.lim[1], pw.lim[2], by=by)
Grid <- expand.grid(sl.seq, sw.seq, pl.seq, pw.seq)

# grid.diag <- gridDiag(iris[,1:4], FUN=kde, h=0.3, lim=cbind(sl.lim, sw.lim, pl.lim, pw.lim)
#                     by = by, sublevel = FALSE, library = "Dionysus",
#                     printProgress = FALSE)
spc <- iris$Species
iris$Species <- as.integer(spc)
rips.diag <- ripsDiag(iris, maxdimension = 2, maxscale = 5)
plot(rips.diag[['diagram']])
# получаем, что есть устойчивые два кластера одномерных. А все циклы быстро зарастают.
library(ggfortify)
autoplot(prcomp(iris))
#видим те же два кластера на картинке
data("diamonds")
diamonds$color <- as.integer(diamonds$color)
diamonds$cut <- as.integer(diamonds$cut)
diamonds$clarity <- as.integer(diamonds$clarity)
#c("carat", "price", 'x','y','z', 'color', 'cut')
df <- diamonds[sample(1:nrow(diamonds), 500),]
autoplot(prcomp(df))

r.diag <- ripsDiag(df, maxdimension = 2, maxscale = 100)
plot(r.diag[['diagram']])

# https://cran.r-project.org/web/packages/TDA/vignettes/article.pdf
m <- 80     # subsample size
n <- 50     # we will compute n landscapes using subsamples of size m
maxscale <- 10
tseq <- seq(0, maxscale, length = 500)          #domain of landscapes
#here we store n Rips diags
Diags <- list()
#here we store n landscapes
Lands <- matrix(0, nrow = n, ncol = length(tseq))

for (i in seq_len(n)) {
    subX <- diamonds[sample(seq_len(nrow(diamonds)), m), ]
    Diags[[i]] <- ripsDiag(subX, maxdimension = 1, maxscale = maxscale)
    Lands[i, ] <- landscape(Diags[[i]][["diagram"]], dimension = 0, KK = 1, tseq)
}
bootLand <- multipBootstrap(Lands, B = 100, alpha = 0.05,
                            parallel = FALSE)

plot(tseq, bootLand[["mean"]], main = "Mean Landscape with 95% band")
polygon(c(tseq, rev(tseq)), 
        c(bootLand[["band"]][, 1], rev(bootLand[["band"]][, 2])),
        col = "pink")
lines(tseq, bootLand[["mean"]], lwd = 2, col = 2)

# так себе история. Мы получаем одну компоненту связности :(
# даже с ирисами интереснее
mscale = 3
rips.diag <- ripsDiag(iris, maxdimension = 1, maxscale = mscale)
tseq <- seq(0, mscale, length = 1000)   #domain
iris.land <- landscape(rips.diag[["diagram"]], dimension = 0, KK = 1, tseq)
iris.sil <- silhouette(rips.diag[["diagram"]], p = 1, dimension = 0, tseq)
plot(tseq, iris.land, type = "l"); plot(tseq, iris.sil, type = "l")
