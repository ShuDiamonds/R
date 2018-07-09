# 一日目
 # 正規線形モデリング

 # 2013 年 8 月 28 最終改訂
 # 執筆者 馬場真哉（北大水産 M2）
 # ウェブサイト http://logics-of-blue.com/
 # ミスなどのご連絡は logics.of.blue★gmail.com までお願いします。
 # （星を＠に変更）

 #===================================
 # R による統計モデリング
 # この章は全てのプログラムを記述～実行する
 #===============================

 # ★★★★★★★★★★★★★★★★★
 # ★ 写経はじめ ★
 # ★★★★★★★★★★★★★★★★★
 # こんにちは世界
 print("Hallow World")

 # print は省略可能
 "Hallow World"

 # ""を抜かすとエラー
 Hallow World

 # 簡単な計算
 1 + 2
 5*6
 3 + 4/2 - 1
 2^10

 # コメント部の説明
 # 1 + 237

 # 変数の定義
 # 「<-」の左右にはスペースを必ずあけること。
 # 見やすいプログラムを書く癖をつけた方がよい
 x
 x <- 3
 x
 x*2


 # データの読み込みと表示
 # データをコピーしてから実行する
 data.0.clip <- read.delim("clipboard")
 data.0.clip
 head(data.0.clip)
 pairs(data.0.clip)

# 読み込みはこの一行で OK
 data.0 <- read.csv("data0.csv")
 data.0
 head(data.0)
 pairs(data.0)

# データの中身の取出し73 # attach()関数は"絶対に"使わないこと。
 food # エラーが出る
 names(data.0) # data.0 に格納されているモノの名前を調べる
 data.0$food # OK

# プロット
 # 興味の対象を「～」の左側におく
 # 興味の対象 length はえさの量によってどう変わるか？
 plot(data.0$length ~ data.0$food)

 # 興味の対象 length は薬の有無によってどう変わるか？
 plot(data.0$length ~ data.0$medicine)

# ちょっと複雑なプロット
 # 長いプログラムは分かりやすく改行するとよい
 # インデント（左端にスペースを空ける）するとなお見やすい。
 # Google 基準ではインデントはスペース 2 つ分
 # ただし丸かっこで囲まれている場合は、一行目に合わせてインデントを付ける
 # ↑ただし、これはめんどくさいので私は使っていない。スペース 2 つで十分と思う。
 # 無理に Google に合わせる必要はないが、改行&インデントは実務上必須。
 # 私はタブを使うこともよくあるが、Google 基準では使ってはいけないらしい？

 # 見づらいコードを書き続けると、いつか"必ず"後悔することになるので注意。
 # Google に合わせる必要は皆無だが、自分なりにルールを決める必要はある。

 plot(
 data.0$length ~ data.0$food, 
 col = c(2,3)[data.0$medicine], 
 pch=16,
 ylab = "Length",
 xlab = "food",
 main = "薬の有無別、体長と餌の量の関係",
 cex.main = 1.5,
 font.lab = 2109 )


# 凡例
 legend(
 "topleft",
 legend = c("薬あり", "薬なし"), 
 col = c(2,3), 
 bty = "n", 
 pch = 16
 )


 # モデルの作成
 # 応答変数：length
 # 説明変数：food & medicine
 # 興味のある対象（応答変数 length）は説明変数によってどれくらい変化するか？ の
 モデリング
 lm.model.0 <- lm(length ~ food + medicine, data = data.0)
 lm.model.0

 # ANOVA による検定結果
 # 説明は後程
 anova(lm.model.0)

 # より詳しい結果の表示→最初は飛ばす
 summary(lm.model.0)

 # 予測
 predict(
 lm.model.0,
 newdata = data.frame(food = 50, medicine = "medicine"))

 # 予測区間つき
 predict(145 lm.model.0, 
 newdata = data.frame(food = 50, medicine = "medicine"),
 interval = "prediction", level = 0.95)

 # newdata の指定なし
 predict(lm.model.0)

 # データの型の確認
 class(data.0)

# ベクトルデータ
 vec <- c(1, 2, 3, 4.5, 5, 6, 7.2, 8, 9, 9.9)
 vec
 int <- 1:10
 int

# 等差数列
 seq(from = 0.1, to = 1, by = 0.1)

# データフレームに格納する
 d <- data.frame(
 vec = vec,
 int = int,
 seq = seq(from = 0.1, to = 1, by = 0.1)
 )


# データの取り出し方
 d
 d[1,]
 d[,1]
 d[2,3]
 names(d)
 d$vec
 d[,c("vec")]
 d[,c("vec", "seq")]


# 予測の図示
 # newdata の作成
 newfood <- seq(from = min(data.0$food), to = max(data.0$food), by = 1)
 newfood

 # 薬があるときのえさと体長の関係の予測のためのデータセット
 new.1 <- data.frame(
 food = newfood,
 medicine = "medicine")

 # 薬がない時のえさと体長の関係の予測のためのデータセット
 new.2 <- data.frame(
 food = newfood,
 medicine = "na")

 # 薬の有無別に予測
 pred.1 <- predict(
 lm.model.0, newdata = new.1,
 interval = "prediction", level = 0.95)
 pred.2 <- predict(
 lm.model.0, newdata = new.2,
 interval = "prediction", level = 0.95)

 # 図示
 # ちょっと複雑なプロット
 # これは以前作ったものと同じ。
 # コピペする方が早いし、正確。
 # 意地を張ると遅くなるうえに不正確になるので注意。ぜひコピペしてください。
 plot(data.0$length ~ data.0$food, 
 col = c(2,3)[data.0$medicine], 
 pch=16,
 ylab = "Length",
 xlab = "food",
 main = "薬の有無別、体長と餌の量の関係",
 cex.main = 1.5,
 font.lab = 2217 )
 # 予測値
 lines(pred.1[,1] ~ newfood, col = 2, lwd = 2)
 lines(pred.1[,2] ~ newfood, col = 2, lwd = 1, lty = 2)
 lines(pred.1[,3] ~ newfood, col = 2, lwd = 1, lty = 2)
 lines(pred.2[,1] ~ newfood, col = 3, lwd = 2)
 lines(pred.2[,2] ~ newfood, col = 3, lwd = 1, lty = 2)
 lines(pred.2[,3] ~ newfood, col = 3, lwd = 1, lty = 2)

 # 凡例
 legend("topleft",
 legend = c("薬あり", "薬なし"), 
 col = c(2,3), 
 bty = "n", 
 pch = 16)


 # ♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪
 # ♪ 写経終わり ♪
 # ♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪


 #======================================
 # 統計の基本と t 検定
 #======================================

 # 大阪データ
 osaka <- c(19,19,20,20,20,20,20,21,21,21)
 # 期待値
 19*(2/10) + 20*(5/10) + 21*(3/10)
 mean(osaka)
 # 標本分散
 ((19-20.1)^2)*(2/10) + ((20-20.1)^2) *(5/10) + 
 ((21-20.1)^2) *(3/10)

 # 不偏分散
 length(osaka)
 ((19-20.1)^2)*(2/9) + ((20-20.1)^2) *(5/9) + ((21-20.1)^2) *(3/9)
 var(osaka)


 #東京データ
 tokyo <- c(50,0,0,20,20,20,20,20,70,70)
 #期待値
 mean(tokyo)
 #標本分散
 (-50-19)^2 * (1/10) + (0-19)^2 * (2/10) + 
 (20-19)^2 *(5/10) + (70-19)^2 * (2/10)
 #不偏分散
 var(tokyo)


 # ★★★★★★★★★★★★★★★★★
 # ★ 写経はじめ ★
 # ★★★★★★★★★★★★★★★★★

 # サンプルデータの作成
 d <- c(-1, -1, 0, 0, 1, 3, 5, 6, 7, 7)
 
 #平均値
 mean(d)

 #標準偏差
 sd(d)
 
 #サンプルサイズ
 length(d)
 
 #標準誤差
 sd(d)/sqrt(length(d))
 
 #ｔ検定
 mean <- mean(d)
 std.error <- sd(d) / sqrt(length(d))
 t.value <- mean / std.error
 t.value
 (1-pt(t.value, df=length(d)-1))*2

 # R に入っている関数を使う
 t.test(d)

 #====================
 # 統計モデルな t 検定
 #====================

 # データの読み込みと表示
 # 作業ディレクトリを変更してから以下を実行

 # NOT RUN
 # setwd()

 data.0 <- read.csv("data0.csv")
 lm.model.0 <- lm(length ~ food + medicine, data = data.0)
 lm.model.0


 # より詳しい結果の表示
 # ここに t value と t 検定の結果が出ている。
 summary(lm.model.0)


 # ♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪
 # ♪ 写経終わり ♪
 # ♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪♪


 #===========================================
 # 分散分析 ANOVA
 #===========================================

 # グラフを描くだけ

 # サンプルデータの作成
 d2 <- data.frame(
 Y = c(c(1,2,3,4,5), c(4,5,6,7,8), c(7,8,9,10,11)),
 option = rep(c("A", "B", "C"), each=5)
 )

 d2

 # データの可視化
 plot(d2$Y ~ d2$option)

 # スライドに載せたような散布図形式のグラフを作る
 par(mar=c(5, 6, 3, 3))
 plot.default(
 d2$Y ~ d2$option, 
 ylim=c(0,12), xlim=c(0.5,3.5),
 ylab="結果", xlab="選択肢",
 cex=2,
 cex.lab=2, cex.main=3,
 xaxt="n")
 axis(side=1, 1:3, LETTERS[1:3])


 # ★★★★★★★★★★★★★★★★★
 # ★ 写経はじめ ★
 # ★★★★★★★★★★★★★★★★★

 # サンプルデータの作成
 d2 <- data.frame(
 Y = c(c(1,2,3,4,5), c(4,5,6,7,8), c(7,8,9,10,11)),372 option = rep(c("A", "B", "C"), each=5)
 )

 d2

 # データの可視化
 plot(d2$Y ~ d2$option)

 # Y の総平均
 mean(d2$Y)

 # option ごとの Y の期待値の算出
 tapply(d2$Y, d2$option, mean)

 # R で分散分析を実装する
 lm.model.anova <- lm(Y ~ option, data=d2)

 # 係数の確認
 lm.model.anova
 summary(lm.model.anova)

 # 分散分析による予測
 predict(lm.model.anova, data.frame(option=c("A", "B", "C")))

 # 予測値は option 別の期待値に等しい
 tapply(d2$Y, d2$option, mean)









