mtcars
str(mtcars)

# ������
data(mtcars)
attach(mtcars)
table(cyl) #�Q�ΨT���Ʋ��ͦ��Ƥ��t
T_cyl = table(cyl)
barplot(T_cyl , main="cyl �T���Ʀ��Ƥ��t��", 
        xlab="�T����", col=c("red", "blue", "green"), 
        names.arg=c("4 �T��", "6 �T��", "8 �T��"), border = "cyan")
# col �����������C��
#��r "black"   "red"     "green3"  "blue"    "cyan"    "magenta" "yellow" "gray"
#
#�Ʀr
#col=1��, col=2��, col=3��, col=4�`��, col=5����, col=6������, col=7��
# names.arg����X�b����

barplot(T_cyl , 
        main="cyl �T���Ʀ��Ƥ��t��", 
        xlab="�T����", 
        col=c("red", "blue", "green"), 
        names.arg=c("4 �T��", "6 �T��", "8 �T��"), border = "cyan",
        horiz=TRUE)
# horiz=TRUE �e�������

library("vcd")
prop.table( table(cyl) )
T_cyl1 = prop.table( table(cyl) )
barplot(T_cyl1 , main="cyl �T���Ʀ��Ƥ��t��", xlab="�T����", 
         col=c("red", "blue", "green"), 
         names.arg=c("4 �T��", "6 �T��", "8 �T��"), border = "cyan", 
         horiz=TRUE)
# ���ժ�����----
T_cyl2 = table(am,cyl) #�إ� �ܳt���P�T���ƥ�e��
T_cyl2
barplot(T_cyl2 , 
        main="cyl �T���Ʀ��Ƥ��t��", 
        xlab="�T����", 
        col=c("red", "blue"), 
        names.arg=c("4 �T��", "6 �T��", "8 �T��"), 
        border = "cyan",
        horiz=FALSE, 
        legend = rownames(T_cyl2), beside=TRUE)

# legend �O�Ϩ�
# beside�O���չ��٬O���|��

# ������(�ʤ�����|��)
T_cyl3 = prop.table( table(am,cyl) ,2)
par(las=1) #����=1�A���ܼ��Ҥ�r�������C ����=2�A���ܼ��Ҥ�r�������C
barplot(T_cyl3 , main="cyl �T���Ʀ��Ʀʤ�����|��", xlab="�T����", col=c("red", "blue"), 
        names.arg=c("4 �T��", "6 �T��", "8 �T��"), border = "cyan",
        horiz=FALSE, legend = c('�۰�','���'), beside=FALSE, cex.names=2)

#cex.names=2 ���ܼ��Ҥ�r�j�p����Ӫ��⭿

# ������(�ʤ�����|��)
barplot(T_cyl3 , main="cyl �T���Ʀ��Ʀʤ�����|��", xlab="�T����", 
        col=c("red", "blue"), names.arg=c("4 �T��", "6 �T��", "8 �T��"), border = "cyan",
        horiz=FALSE, legend = c('�۰�','���'), beside=FALSE, cex.names=2, space=2)
#space=2 ���ܪ��������Z��
