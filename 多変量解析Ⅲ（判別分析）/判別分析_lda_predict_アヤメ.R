par(mfrow=c(1,2)) 						# Divide R Graphics window to 1×2

x <- iris								# irisデータの読み込み

x <- subset(x, as.integer(x[,5]) > 1)			#　Versicolor と Virginica の抽出
x

plot(x[,3],x[,4],col=as.integer(x$Species), pch=as.integer(x$Species))

library(MASS)
lda_out <- lda(as.integer(x$Species) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, x)	#判別分析の実行
lda_out

iris_predict = predict(lda_out)				#線形判別分析の結果を基にアヤメのデータの再判別
iris_predict

n <- 100								#データ数
col_num <- n*3							#出力行列の列数
result <- matrix(0, nrow=n,ncol=3)		#結果の出力用行列の定義

for(i in 1:n){

	xu <- as.matrix(x[i,-5])				#　i行目のデータ（5列目は除く）を抽出

	result[i,1] <- xu[1,3]
	result[i,2] <- xu[1,4]
	
	if (iris_predict$x[i] < 0){
		result[i,3] <- 2 				     	#verグループ　　2:△
	}
	else{
		result[i,3] <- 3					#virグループ　　3:＋
	}
}

plot(result[,1],result[,2],col=result[,3], pch = result[,3])　　#判別結果のプロット

