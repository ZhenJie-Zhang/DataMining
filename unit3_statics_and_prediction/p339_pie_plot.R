# p338_²����� ----
pieces <- c(8,8,2,4,2)
pie(pieces , labels = c('�u�@','��ı','����','���q��','���'), main="�ͬ��ɶ����t��") 

# p339_²����� �[�W�ʤ���----
pieces <- c(8,8,2,4,2)
pct <- round(pieces/sum(pieces)*100)  # �p��U�����O�ʤ���
lbls <- paste(c('�u�@','��ı','����','���q��','���'),pct,'%', sep='')
pie(pieces , labels = lbls, main="�ͬ��ɶ����t��") 

# p340_²����� �[�W�ʤ���----
pieces <- c(8,8,2,4,2)
pct <- round(pieces/sum(pieces)*100)  # �p��U�����O�ʤ���
lbls <- paste(c('�u�@','��ı','����','���q��','���'),'\n',pct,'%', sep='')
pie(pieces , labels = lbls, main="�ͬ��ɶ����t��", cex=1.5, cex.main=2) 
#cex=1.5���Ҧr��j1.5���Fcex.main=2���D��j�⭿