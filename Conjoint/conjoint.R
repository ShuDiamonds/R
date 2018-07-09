#�Q�l�t�q�k: http://www.trifields.jp/research-conjoint-analysis-344

#conjoint�p�b�P�[�W�̃C���X�g�[��
install.packages("conjoint")
#conjoint�p�b�P�[�W�̓ǂݍ���
library("conjoint")
#tea�f�[�^�̓ǂݍ���
data(tea)

#���q�̑g�ݍ��킹���쐬

experiment<-expand.grid(
price=c("low","medium","high"),
variety=c("black","green","red"),
kind=c("bags","granulated","leafy"),
aroma=c("yes","no"))

#���q�̒���\���쐬
design<-caFactorialDesign(data=experiment,type="orthogonal")
#���q�̒���\��\��
print(design)
#���q�̒���\���R�[�h��
code<-caEncodedDesign(design)
#�R�[�h�����ꂽ����\�̕\��
print(code)
#���֌W���̕\��
print(cor(code))

#���i�v���t�@�C��-�A���P�[�g�ɋL�ڂ���13�̏��i
print(tprof)

#�e�����̃��x��
print(tlevn)
#�A���P�[�g�f�[�^�̕\��
#���̃f�[�^�͒����v�悩��13�̑g�ݍ��킹�����10�_���_�ŕ]��
head(tprefm)

Conjoint(tprefm, tprof, tlevn)

#5�̃N���X�^�[�ɕ���
segments<-caSegmentation(tprefm,tprof,5)

newdata<-merge(tprefm,segments$cluster)
head(newdata)


hist(newdata[newdata$y==5,]$profil1)

