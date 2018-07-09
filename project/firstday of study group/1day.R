# �����
 # ���K���`���f�����O

 # 2013 �N 8 �� 28 �ŏI����
 # ���M�� �n��^�Ɓi�k�吅�Y M2�j
 # �E�F�u�T�C�g http://logics-of-blue.com/
 # �~�X�Ȃǂ̂��A���� logics.of.blue��gmail.com �܂ł��肢���܂��B
 # �i�������ɕύX�j

 #===================================
 # R �ɂ�铝�v���f�����O
 # ���̏͂͑S�Ẵv���O�������L�q�`���s����
 #===============================

 # ����������������������������������
 # �� �ʌo�͂��� ��
 # ����������������������������������
 # ����ɂ��͐��E
 print("Hallow World")

 # print �͏ȗ��\
 "Hallow World"

 # ""�𔲂����ƃG���[
 Hallow World

 # �ȒP�Ȍv�Z
 1 + 2
 5*6
 3 + 4/2 - 1
 2^10

 # �R�����g���̐���
 # 1 + 237

 # �ϐ��̒�`
 # �u<-�v�̍��E�ɂ̓X�y�[�X��K�������邱�ƁB
 # ���₷���v���O�����������Ȃ����������悢
 x
 x <- 3
 x
 x*2


 # �f�[�^�̓ǂݍ��݂ƕ\��
 # �f�[�^���R�s�[���Ă�����s����
 data.0.clip <- read.delim("clipboard")
 data.0.clip
 head(data.0.clip)
 pairs(data.0.clip)

# �ǂݍ��݂͂��̈�s�� OK
 data.0 <- read.csv("data0.csv")
 data.0
 head(data.0)
 pairs(data.0)

# �f�[�^�̒��g�̎�o��73 # attach()�֐���"��΂�"�g��Ȃ����ƁB
 food # �G���[���o��
 names(data.0) # data.0 �Ɋi�[����Ă��郂�m�̖��O�𒲂ׂ�
 data.0$food # OK

# �v���b�g
 # �����̑Ώۂ��u�`�v�̍����ɂ���
 # �����̑Ώ� length �͂����̗ʂɂ���Ăǂ��ς�邩�H
 plot(data.0$length ~ data.0$food)

 # �����̑Ώ� length �͖�̗L���ɂ���Ăǂ��ς�邩�H
 plot(data.0$length ~ data.0$medicine)

# ������ƕ��G�ȃv���b�g
 # �����v���O�����͕�����₷�����s����Ƃ悢
 # �C���f���g�i���[�ɃX�y�[�X���󂯂�j����ƂȂ����₷���B
 # Google ��ł̓C���f���g�̓X�y�[�X 2 ��
 # �������ۂ������ň͂܂�Ă���ꍇ�́A��s�ڂɍ��킹�ăC���f���g��t����
 # ���������A����͂߂�ǂ������̂Ŏ��͎g���Ă��Ȃ��B�X�y�[�X 2 �ŏ\���Ǝv���B
 # ������ Google �ɍ��킹��K�v�͂Ȃ����A���s&�C���f���g�͎�����K�{�B
 # ���̓^�u���g�����Ƃ��悭���邪�AGoogle ��ł͎g���Ă͂����Ȃ��炵���H

 # ���Â炢�R�[�h������������ƁA����"�K��"������邱�ƂɂȂ�̂Œ��ӁB
 # Google �ɍ��킹��K�v�͊F�������A�����Ȃ�Ƀ��[�������߂�K�v�͂���B

 plot(
 data.0$length ~ data.0$food, 
 col = c(2,3)[data.0$medicine], 
 pch=16,
 ylab = "Length",
 xlab = "food",
 main = "��̗L���ʁA�̒��Ɖa�̗ʂ̊֌W",
 cex.main = 1.5,
 font.lab = 2109 )


# �}��
 legend(
 "topleft",
 legend = c("�򂠂�", "��Ȃ�"), 
 col = c(2,3), 
 bty = "n", 
 pch = 16
 )


 # ���f���̍쐬
 # �����ϐ��Flength
 # �����ϐ��Ffood & medicine
 # �����̂���Ώہi�����ϐ� length�j�͐����ϐ��ɂ���Ăǂꂭ�炢�ω����邩�H ��
 ���f�����O
 lm.model.0 <- lm(length ~ food + medicine, data = data.0)
 lm.model.0

 # ANOVA �ɂ�錟�茋��
 # �����͌��
 anova(lm.model.0)

 # ���ڂ������ʂ̕\�����ŏ��͔�΂�
 summary(lm.model.0)

 # �\��
 predict(
 lm.model.0,
 newdata = data.frame(food = 50, medicine = "medicine"))

 # �\����Ԃ�
 predict(145 lm.model.0, 
 newdata = data.frame(food = 50, medicine = "medicine"),
 interval = "prediction", level = 0.95)

 # newdata �̎w��Ȃ�
 predict(lm.model.0)

 # �f�[�^�̌^�̊m�F
 class(data.0)

# �x�N�g���f�[�^
 vec <- c(1, 2, 3, 4.5, 5, 6, 7.2, 8, 9, 9.9)
 vec
 int <- 1:10
 int

# ��������
 seq(from = 0.1, to = 1, by = 0.1)

# �f�[�^�t���[���Ɋi�[����
 d <- data.frame(
 vec = vec,
 int = int,
 seq = seq(from = 0.1, to = 1, by = 0.1)
 )


# �f�[�^�̎��o����
 d
 d[1,]
 d[,1]
 d[2,3]
 names(d)
 d$vec
 d[,c("vec")]
 d[,c("vec", "seq")]


# �\���̐}��
 # newdata �̍쐬
 newfood <- seq(from = min(data.0$food), to = max(data.0$food), by = 1)
 newfood

 # �򂪂���Ƃ��̂����Ƒ̒��̊֌W�̗\���̂��߂̃f�[�^�Z�b�g
 new.1 <- data.frame(
 food = newfood,
 medicine = "medicine")

 # �򂪂Ȃ����̂����Ƒ̒��̊֌W�̗\���̂��߂̃f�[�^�Z�b�g
 new.2 <- data.frame(
 food = newfood,
 medicine = "na")

 # ��̗L���ʂɗ\��
 pred.1 <- predict(
 lm.model.0, newdata = new.1,
 interval = "prediction", level = 0.95)
 pred.2 <- predict(
 lm.model.0, newdata = new.2,
 interval = "prediction", level = 0.95)

 # �}��
 # ������ƕ��G�ȃv���b�g
 # ����͈ȑO��������̂Ɠ����B
 # �R�s�y��������������A���m�B
 # �Ӓn�𒣂�ƒx���Ȃ邤���ɕs���m�ɂȂ�̂Œ��ӁB���ЃR�s�y���Ă��������B
 plot(data.0$length ~ data.0$food, 
 col = c(2,3)[data.0$medicine], 
 pch=16,
 ylab = "Length",
 xlab = "food",
 main = "��̗L���ʁA�̒��Ɖa�̗ʂ̊֌W",
 cex.main = 1.5,
 font.lab = 2217 )
 # �\���l
 lines(pred.1[,1] ~ newfood, col = 2, lwd = 2)
 lines(pred.1[,2] ~ newfood, col = 2, lwd = 1, lty = 2)
 lines(pred.1[,3] ~ newfood, col = 2, lwd = 1, lty = 2)
 lines(pred.2[,1] ~ newfood, col = 3, lwd = 2)
 lines(pred.2[,2] ~ newfood, col = 3, lwd = 1, lty = 2)
 lines(pred.2[,3] ~ newfood, col = 3, lwd = 1, lty = 2)

 # �}��
 legend("topleft",
 legend = c("�򂠂�", "��Ȃ�"), 
 col = c(2,3), 
 bty = "n", 
 pch = 16)


 # ������������������
 # �� �ʌo�I��� ��
 # ������������������


 #======================================
 # ���v�̊�{�� t ����
 #======================================

 # ���f�[�^
 osaka <- c(19,19,20,20,20,20,20,21,21,21)
 # ���Ғl
 19*(2/10) + 20*(5/10) + 21*(3/10)
 mean(osaka)
 # �W�{���U
 ((19-20.1)^2)*(2/10) + ((20-20.1)^2) *(5/10) + 
 ((21-20.1)^2) *(3/10)

 # �s�Ε��U
 length(osaka)
 ((19-20.1)^2)*(2/9) + ((20-20.1)^2) *(5/9) + ((21-20.1)^2) *(3/9)
 var(osaka)


 #�����f�[�^
 tokyo <- c(50,0,0,20,20,20,20,20,70,70)
 #���Ғl
 mean(tokyo)
 #�W�{���U
 (-50-19)^2 * (1/10) + (0-19)^2 * (2/10) + 
 (20-19)^2 *(5/10) + (70-19)^2 * (2/10)
 #�s�Ε��U
 var(tokyo)


 # ����������������������������������
 # �� �ʌo�͂��� ��
 # ����������������������������������

 # �T���v���f�[�^�̍쐬
 d <- c(-1, -1, 0, 0, 1, 3, 5, 6, 7, 7)
 
 #���ϒl
 mean(d)

 #�W���΍�
 sd(d)
 
 #�T���v���T�C�Y
 length(d)
 
 #�W���덷
 sd(d)/sqrt(length(d))
 
 #������
 mean <- mean(d)
 std.error <- sd(d) / sqrt(length(d))
 t.value <- mean / std.error
 t.value
 (1-pt(t.value, df=length(d)-1))*2

 # R �ɓ����Ă���֐����g��
 t.test(d)

 #====================
 # ���v���f���� t ����
 #====================

 # �f�[�^�̓ǂݍ��݂ƕ\��
 # ��ƃf�B���N�g����ύX���Ă���ȉ������s

 # NOT RUN
 # setwd()

 data.0 <- read.csv("data0.csv")
 lm.model.0 <- lm(length ~ food + medicine, data = data.0)
 lm.model.0


 # ���ڂ������ʂ̕\��
 # ������ t value �� t ����̌��ʂ��o�Ă���B
 summary(lm.model.0)


 # ������������������
 # �� �ʌo�I��� ��
 # ������������������


 #===========================================
 # ���U���� ANOVA
 #===========================================

 # �O���t��`������

 # �T���v���f�[�^�̍쐬
 d2 <- data.frame(
 Y = c(c(1,2,3,4,5), c(4,5,6,7,8), c(7,8,9,10,11)),
 option = rep(c("A", "B", "C"), each=5)
 )

 d2

 # �f�[�^�̉���
 plot(d2$Y ~ d2$option)

 # �X���C�h�ɍڂ����悤�ȎU�z�}�`���̃O���t�����
 par(mar=c(5, 6, 3, 3))
 plot.default(
 d2$Y ~ d2$option, 
 ylim=c(0,12), xlim=c(0.5,3.5),
 ylab="����", xlab="�I����",
 cex=2,
 cex.lab=2, cex.main=3,
 xaxt="n")
 axis(side=1, 1:3, LETTERS[1:3])


 # ����������������������������������
 # �� �ʌo�͂��� ��
 # ����������������������������������

 # �T���v���f�[�^�̍쐬
 d2 <- data.frame(
 Y = c(c(1,2,3,4,5), c(4,5,6,7,8), c(7,8,9,10,11)),372 option = rep(c("A", "B", "C"), each=5)
 )

 d2

 # �f�[�^�̉���
 plot(d2$Y ~ d2$option)

 # Y �̑�����
 mean(d2$Y)

 # option ���Ƃ� Y �̊��Ғl�̎Z�o
 tapply(d2$Y, d2$option, mean)

 # R �ŕ��U���͂���������
 lm.model.anova <- lm(Y ~ option, data=d2)

 # �W���̊m�F
 lm.model.anova
 summary(lm.model.anova)

 # ���U���͂ɂ��\��
 predict(lm.model.anova, data.frame(option=c("A", "B", "C")))

 # �\���l�� option �ʂ̊��Ғl�ɓ�����
 tapply(d2$Y, d2$option, mean)








