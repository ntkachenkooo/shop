h <- list(
  list(
    name = '������, ��.',
    min = 100,
    max = 150,
    price = 129
  ),
  list(
    name = '�������, ��.',
    min = 80,
    max = 120,
    price = 89
  ),
  list(
    name = '�������, ����',
    min = 35,
    max = 70,
    price = 39
  ),
  list(
    name = '����������� ����, ��������',
    min = 170,
    max = 200,
    price = 199
  ),
  list(
    name = '���, �����',
    min = 100,
    max = 120,
    price = 79
  ),
  list(
    name = '�������, �����',
    min = 50,
    max = 60,
    price = 89
  ),
  list(
    name = '�������� ������, �����',
    min = 20,
    max = 30,
    price = 84
  )
)


FILE_SUPPLY <- '.in'
FILE_SALE <- '.out'


generate.data <-
  function(file.name = "�������",
           path = "C:/Users/tkach/�������� ����",
           dataType = 'SUPPLY',
           days = 7,
           goods = h) {
    #�������� ������������� �����
    if (!dir.exists(path)) {
      dir.create(path)
    }
    #������� � �����
    if (dir.exists(path)) {
      setwd(path) #������� � �����
      
      #�������� ������� �� ������ ������� ������ days
      tabl <- data.frame('����' = 1:days)
      #��������
      if (dataType == 'SUPPLY') {
        for (i in 1:length(goods)) {
          tabl[i + 1] <-
            sample(
              x = goods[[i]]$min:goods[[i]]$max,
              size = days,
              replace = TRUE
            )
          colnames(x = tabl)[i + 1] = unlist(goods[i])[1]
        }
        write.table(
          tabl,
          file = paste0(file.name, FILE_SUPPLY),
          row.names = FALSE,
          sep = ' ',
          #encoding = "UTF-8"
        )
      } else{
        #�������
        for (i in 1:length(goods)) {
          data.in <-
            read.table(file = paste0(file.name, FILE_SUPPLY),
                       head = TRUE)
          data.out <- vector()
          for (j in 1:days) {
            data.out[j] <-
              as.integer(runif(
                n = 1,
                min = goods[[i]]$min,
                max = goods[[i]]$max
              ))
            data.out[j] <-
              ifelse(data.out[j] > data.in[j, i + 1],
                     data.in[j, i + 1],
                     data.out[j])
          }
          tabl[i + 1] <- data.out
          colnames(x = tabl)[i + 1] <- unlist(goods[i])['name']
        }
        write.table(
          tabl,
          file = paste0(file.name, FILE_SALE),
          row.names = FALSE,
          sep = ' ',
          #encoding = "UTF-8"
        )
      }
      return(tabl)
    }
  }


h1 <- list(
  list(
    file.name = "����",
    name = '������, ��.',
    price_min = 89,
    price_max = 129
  ),
  list(
    file.name = "����",
    name = '�������, ��.',
    price_min = 59,
    price_max = 89
  ),
  list(
    file.name = "����",
    name = '�������, ����',
    price_min = 25,
    price_max = 39
  ),
  list(
    file.name = "����",
    name = '����������� ����, ��������',
    price_min = 149,
    price_max = 199
  ),
  list(
    file.name = "����",
    name = '���, �����',
    price_min = 49,
    price_max = 79
  ),
  list(
    file.name = "����",
    name = '�������, �����',
    price_min = 69,
    price_max = 89
  ),
  list(
    file.name = "����",
    name = '�������� ������, �����',
    price_min = 70,
    price_max = 84
  )
)


h2 = list(list(name = '������� ����'),
          list(name = '���� �������'),
          list(name = '���� ����������'))


generate.price <- function(file.name = "����",
                           path = "C:/Users/tkach/�������� ����",
                           days = 7,
                           goods1 = h1,
                           goods2 = h2) {
  #�������� ������� �� ������ ������� ������ day
  tabl1 <- data.frame('��������' = 1:7)
  for (i in 1:length(goods1)) {
    tabl1[i, 1] <- goods1[[i]]$name
  }
  
  for (i in 1:days) {
    v <-
      as.integer(runif(
        n = 1,
        min = goods1[[i]]$price_min,
        max = goods1[[i]]$price_max
      ))
    tabl1[i, 2] <- v
  }
  
  colnames(x = tabl1)[2] = goods2[[1]]$name
  for (i in 1:days) {
    tabl1[i, 3] <- tabl1[i, 2] * 1.2
    tabl1[i, 4] <- tabl1[i, 2] * 0.65
  }
  colnames(x = tabl1)[3] = goods2[[2]]$name
  colnames(x = tabl1)[4] = goods2[[3]]$name
  
  write.table(
    tabl1,
    file = paste0(path, '/', file.name, '.txt'),
    row.names = FALSE,
    sep = ' '
  )
  return(tabl1)
}



generate.data(path = 'C:/Users/tkach/�������� ����/������� 1', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 1', dataType = 'SALE')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 2', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 2', dataType = 'SALE')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 3', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 3', dataType = 'SALE')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 4', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 4', dataType = 'SALE')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 5', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 5', dataType = 'SALE')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 6', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 6', dataType = 'SALE')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 7', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 7', dataType = 'SALE')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 8', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 8', dataType = 'SALE')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 9', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 9', dataType = 'SALE')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 10', dataType = 'SUPPLY')
generate.data(path = 'C:/Users/tkach/�������� ����/������� 10', dataType = 'SALE')

generate.price(path = 'C:/Users/tkach/�������� ����/������� 1')
generate.price(path = 'C:/Users/tkach/�������� ����/������� 2')
generate.price(path = 'C:/Users/tkach/�������� ����/������� 3')
generate.price(path = 'C:/Users/tkach/�������� ����/������� 4')
generate.price(path = 'C:/Users/tkach/�������� ����/������� 5')
generate.price(path = 'C:/Users/tkach/�������� ����/������� 6')
generate.price(path = 'C:/Users/tkach/�������� ����/������� 7')
generate.price(path = 'C:/Users/tkach/�������� ����/������� 8')
generate.price(path = 'C:/Users/tkach/�������� ����/������� 9')
generate.price(path = 'C:/Users/tkach/�������� ����/������� 10')



#�������� �������
rev <- rep(0, 12)
profit <- rep(0, length(rev))

res.tab1 <- data.frame(�������     = rev, "�������" = profit)
res.tab2 <- data.frame(�������     = rev, "�������" = profit)
res.tab3 <- data.frame(�������     = rev, "�������" = profit)
res.tab4 <- data.frame(�������     = rev, "�������" = profit)
res.tab5 <- data.frame(�������     = rev, "�������" = profit)
res.tab6 <- data.frame(�������     = rev, "�������" = profit)
res.tab7 <- data.frame(�������     = rev, "�������" = profit)



sale1 <- rep(0, nrow(res.tab1))
sale2 <- rep(0, nrow(res.tab2))
sale3 <- rep(0, nrow(res.tab3))
sale4 <- rep(0, nrow(res.tab4))
sale5 <- rep(0, nrow(res.tab5))
sale6 <- rep(0, nrow(res.tab6))
sale7 <- rep(0, nrow(res.tab7))
res.tab1$"����������, ����." <- sale1
res.tab1$"��������,��" <- 0
res.tab1$"������������� ������" <- 0
res.tab1$"������� ����" <- 0
res.tab1$"����" <- 0
res.tab1$"������� ���" <- 0
res.tab1$"���� " <- 0
res.tab1$"�������� ����" <- 0
res.tab1$" ����" <- 0

res.tab2$"����������, ����." <- sale2
res.tab2$"��������,��" <- 0
res.tab2$"������������� ������" <- 0
res.tab2$"������� ����" <- 0
res.tab2$"����" <- 0
res.tab2$"������� ���" <- 0
res.tab2$"���� " <- 0
res.tab2$"�������� ����" <- 0
res.tab2$" ����" <- 0

res.tab3$"����������, ����." <- sale3
res.tab3$"��������,��" <- 0
res.tab3$"������������� ������" <- 0
res.tab3$"������� ����" <- 0
res.tab3$"����" <- 0
res.tab3$"������� ���" <- 0
res.tab3$"���� " <- 0
res.tab3$"�������� ����" <- 0
res.tab3$" ����" <- 0

res.tab4$"����������, ����." <- sale4
res.tab4$"��������,��" <- 0
res.tab4$"������������� ������" <- 0
res.tab4$"������� ����" <- 0
res.tab4$"����" <- 0
res.tab4$"������� ���" <- 0
res.tab4$"���� " <- 0
res.tab4$"�������� ����" <- 0
res.tab4$" ����" <- 0

res.tab5$"����������, ����." <- sale5
res.tab5$"��������,��" <- 0
res.tab5$"������������� ������" <- 0
res.tab5$"������� ����" <- 0
res.tab5$"����" <- 0
res.tab5$"������� ���" <- 0
res.tab5$"���� " <- 0
res.tab5$"�������� ����" <- 0
res.tab5$" ����" <- 0

res.tab6$"����������, ����." <- sale6
res.tab6$"��������,��" <- 0
res.tab6$"������������� ������" <- 0
res.tab6$"������� ����" <- 0
res.tab6$"����" <- 0
res.tab6$"������� ���" <- 0
res.tab6$"���� " <- 0
res.tab6$"�������� ����" <- 0
res.tab6$" ����" <- 0

res.tab7$"����������, ����." <- sale7
res.tab7$"��������,��" <- 0
res.tab7$"������������� ������" <- 0
res.tab7$"������� ����" <- 0
res.tab7$"����" <- 0
res.tab7$"������� ���" <- 0
res.tab7$"���� " <- 0
res.tab7$"�������� ����" <- 0
res.tab7$" ����" <- 0

res.tab1$"�������"[1] <- "�������_1"
res.tab1$"�������"[2] <- "�������_2"
res.tab1$"�������"[3] <- "�������_3"
res.tab1$"�������"[4] <- "�������_4"
res.tab1$"�������"[5] <- "�������_5"
res.tab1$"�������"[6] <- "�������_6"
res.tab1$"�������"[7] <- "�������_7"
res.tab1$"�������"[8] <- "�������_8"
res.tab1$"�������"[9] <- "�������_9"
res.tab1$"�������"[10] <- "�������_10"
res.tab1$"�������"[11] <- "�����"
res.tab1$"�������"[12] <- "�������"


res.tab2$"�������"[1] <- "�������_1"
res.tab2$"�������"[2] <- "�������_2"
res.tab2$"�������"[3] <- "�������_3"
res.tab2$"�������"[4] <- "�������_4"
res.tab2$"�������"[5] <- "�������_5"
res.tab2$"�������"[6] <- "�������_6"
res.tab2$"�������"[7] <- "�������_7"
res.tab2$"�������"[8] <- "�������_8"
res.tab2$"�������"[9] <- "�������_9"
res.tab2$"�������"[10] <- "�������_10"
res.tab2$"�������"[11] <- "�����"
res.tab2$"�������"[12] <- "�������"

res.tab3$"�������"[1] <- "�������_1"
res.tab3$"�������"[2] <- "�������_2"
res.tab3$"�������"[3] <- "�������_3"
res.tab3$"�������"[4] <- "�������_4"
res.tab3$"�������"[5] <- "�������_5"
res.tab3$"�������"[6] <- "�������_6"
res.tab3$"�������"[7] <- "�������_7"
res.tab3$"�������"[8] <- "�������_8"
res.tab3$"�������"[9] <- "�������_9"
res.tab3$"�������"[10] <- "�������_10"
res.tab3$"�������"[11] <- "�����"
res.tab3$"�������"[12] <- "�������"

res.tab4$"�������"[1] <- "�������_1"
res.tab4$"�������"[2] <- "�������_2"
res.tab4$"�������"[3] <- "�������_3"
res.tab4$"�������"[4] <- "�������_4"
res.tab4$"�������"[5] <- "�������_5"
res.tab4$"�������"[6] <- "�������_6"
res.tab4$"�������"[7] <- "�������_7"
res.tab4$"�������"[8] <- "�������_8"
res.tab4$"�������"[9] <- "�������_9"
res.tab4$"�������"[10] <- "�������_10"
res.tab4$"�������"[11] <- "�����"
res.tab4$"�������"[12] <- "�������"

res.tab5$"�������"[1] <- "�������_1"
res.tab5$"�������"[2] <- "�������_2"
res.tab5$"�������"[3] <- "�������_3"
res.tab5$"�������"[4] <- "�������_4"
res.tab5$"�������"[5] <- "�������_5"
res.tab5$"�������"[6] <- "�������_6"
res.tab5$"�������"[7] <- "�������_7"
res.tab5$"�������"[8] <- "�������_8"
res.tab5$"�������"[9] <- "�������_9"
res.tab5$"�������"[10] <- "�������_10"
res.tab5$"�������"[11] <- "�����"
res.tab5$"�������"[12] <- "�������"

res.tab6$"�������"[1] <- "�������_1"
res.tab6$"�������"[2] <- "�������_2"
res.tab6$"�������"[3] <- "�������_3"
res.tab6$"�������"[4] <- "�������_4"
res.tab6$"�������"[5] <- "�������_5"
res.tab6$"�������"[6] <- "�������_6"
res.tab6$"�������"[7] <- "�������_7"
res.tab6$"�������"[8] <- "�������_8"
res.tab6$"�������"[9] <- "�������_9"
res.tab6$"�������"[10] <- "�������_10"
res.tab6$"�������"[11] <- "�����"
res.tab6$"�������"[12] <- "�������"

res.tab7$"�������"[1] <- "�������_1"
res.tab7$"�������"[2] <- "�������_2"
res.tab7$"�������"[3] <- "�������_3"
res.tab7$"�������"[4] <- "�������_4"
res.tab7$"�������"[5] <- "�������_5"
res.tab7$"�������"[6] <- "�������_6"
res.tab7$"�������"[7] <- "�������_7"
res.tab7$"�������"[8] <- "�������_8"
res.tab7$"�������"[9] <- "�������_9"
res.tab7$"�������"[10] <- "�������_10"
res.tab7$"�������"[11] <- "�����"
res.tab7$"�������"[12] <- "�������"



p <-
  c(
    'C:/Users/tkach/�������� ����/������� 1',
    'C:/Users/tkach/�������� ����/������� 2',
    'C:/Users/tkach/�������� ����/������� 3',
    'C:/Users/tkach/�������� ����/������� 4',
    'C:/Users/tkach/�������� ����/������� 5',
    'C:/Users/tkach/�������� ����/������� 6',
    'C:/Users/tkach/�������� ����/������� 7',
    'C:/Users/tkach/�������� ����/������� 8',
    'C:/Users/tkach/�������� ����/������� 9',
    'C:/Users/tkach/�������� ����/������� 10'
  )
sum_out1 <- vector()
sum_out2 <- vector()
sum_out3 <- vector()
sum_out4 <- vector()
sum_out5 <- vector()
sum_out6 <- vector()
sum_out7 <- vector()
sum_in1 <- vector()
sum_in2 <- vector()
sum_in3 <- vector()
sum_in4 <- vector()
sum_in5 <- vector()
sum_in6 <- vector()
sum_in7 <- vector()


for (i in 1:10) {
  #���������� �������� ������ � ����������
  setwd(p[i])
  in1 <- read.table("�������.in", head = TRUE, encoding = "UTF-8")
  out1 <- read.table("�������.out", head = TRUE, encoding = "UTF-8")
  price <-
    read.table("����.txt", head = TRUE, encoding = "UTF-8")
  sum_out1 <- c(sum_out1, sum(out1[2]))
  sum_out2 <- c(sum_out2, sum(out1[3]))
  sum_out3 <- c(sum_out3, sum(out1[4]))
  sum_out4 <- c(sum_out4, sum(out1[5]))
  sum_out5 <- c(sum_out5, sum(out1[6]))
  sum_out6 <- c(sum_out6, sum(out1[7]))
  sum_out7 <- c(sum_out7, sum(out1[8]))
  sum_in1 <- c(sum_in1, sum(in1[2]))
  sum_in2 <- c(sum_in2, sum(in1[3]))
  sum_in3 <- c(sum_in3, sum(in1[4]))
  sum_in4 <- c(sum_in4, sum(in1[5]))
  sum_in5 <- c(sum_in5, sum(in1[6]))
  sum_in6 <- c(sum_in6, sum(in1[7]))
  sum_in7 <- c(sum_in7, sum(in1[8]))
  #������� ����������� ��� ��������� ���������
  setwd('C:/Users/tkach/�������� ����/������')
  #____________1_______________
  p_supply <- price[1 , 2] #���� �������
  p_sale <- price[1 , 3] #���� �������
  p_util <- price[1 , 4] #���� ����������
  Q_supply = sum_in1[i] #SUM_IN2...
  sales = sum_out1[i]#SUM_out2...
  Q_util = Q_supply - sales
  
  TC = Q_supply * p_supply + Q_util * p_util #
  res.tab1[i, 1] = sales * p_sale#
  res.tab1[i, 2] = res.tab1[i, 1] - TC#�������
  res.tab1[i, 3] = sales#
  res.tab1[i, 4] = Q_util#
  
  res.tab1$"�������"[11] <- sum(res.tab1$"�������"[1:10])#
  res.tab1$"�������"[12] <- mean(res.tab1$"�������"[1:10])#
  res.tab1$"�������"[11] <- sum(res.tab1$"�������"[1:10])#
  res.tab1$"�������"[12] <- mean(res.tab1$"�������"[1:10])#
  
  
  res.tab1$"������������� ������"[i] <-
    round(sd(out1[1:7, 2]), 2) #[1:7,3]
  
  res.tab1$"������������� ������"[11] <- #
    sum(res.tab1$"������������� ������"[1:10])#
  res.tab1$"������������� ������"[12] <- #
    mean(res.tab1$"������������� ������"[1:10])#
  res.tab1$"����������, ����."[i] <- sum_in1[i]#
  res.tab1$"��������,��"[i] <- sum_in1[i] - sum_out1[i]#
  res.tab1$"����������, ����."[11] <- #
    sum(res.tab1$"����������, ����."[1:10])#
  res.tab1$"����������, ����."[12] <- #
    mean(res.tab1$"����������, ����."[1:10])#
  
  res.tab1$"��������,��"[11] <- sum(res.tab1$"��������,��"[1:10])#
  res.tab1$"��������,��"[12] <- mean(res.tab1$"��������,��"[1:10])#
  
  sp1 <-
    c((in1[1, 2] - out1[1, 2]),
      (in1[2, 2] - out1[2, 2]),
      (in1[3, 2] - out1[3, 2]) ,
      (in1[4, 2] - out1[4, 2]),
      (in1[5, 2] - out1[5, 2]),
      (in1[6, 2] - out1[6, 2]),
      (in1[7, 2] - out1[7, 2])
    ) #������ �� [1,3]...[7,3], [1,4]...[7..4]
  res.tab1$"������� ����"[i] <- max(out1[2]) #out1[3] out1[4]
  res.tab1$"����"[i] <- which.max(out1[1:7, 2]) #out1[3] out1[4]
  res.tab1$"������� ���"[i] <- min(out1[2]) #out1[3] out1[4]
  res.tab1$"���� "[i] <- which.min(out1[1:7, 2]) #out1[3] out1[4]
  res.tab1$"�������� ����"[i] <- max(sp1)#
  res.tab1$" ����"[i] <- which.max(sp1)#
  for (j in 11:12) {
    #
    res.tab1$"������� ����"[j] <- ' ' #
    res.tab1$"����"[j] <- '' #
    res.tab1$"������� ���"[j] <- ' ' #
    res.tab1$"���� "[j] <- ' ' #
    res.tab1$"�������� ����"[j] <- ' ' #
    res.tab1$" ����"[j] <- ' ' #
    write.table(
      res.tab1,
      file = paste0(h[[1]]$name, '.txt'),
      # h[[2]]
      row.names = FALSE,
      sep = ' ',
      #encoding = "UTF-8"
    )
  }
  
  #____________2_______________
  p_supply <- price[2 , 2] #���� �������
  p_sale <- price[2 , 3] #���� �������
  p_util <- price[2 , 4] #���� ����������
  Q_supply = sum_in2[i] #SUM_IN2...
  sales = sum_out2[i]#SUM_out2...
  Q_util = Q_supply - sales
  
  TC = Q_supply * p_supply + Q_util * p_util #
  res.tab2[i, 1] = sales * p_sale#
  res.tab2[i, 2] = res.tab2[i, 1] - TC#
  res.tab2[i, 3] = sales#
  res.tab2[i, 4] = Q_util#
  
  res.tab2$"�������"[11] <- sum(res.tab2$"�������"[1:10])#
  res.tab2$"�������"[12] <- mean(res.tab2$"�������"[1:10])#
  res.tab2$"�������"[11] <- sum(res.tab2$"�������"[1:10])#
  res.tab2$"�������"[12] <- mean(res.tab2$"�������"[1:10])#
  
  
  res.tab2$"������������� ������"[i] <-
    round(sd(out1[1:7, 3]), 2) #[1:7,3]
  
  res.tab2$"������������� ������"[11] <- #
    sum(res.tab2$"������������� ������"[1:10])#
  res.tab2$"������������� ������"[12] <- #
    mean(res.tab2$"������������� ������"[1:10])#
  res.tab2$"����������, ����."[i] <- sum_in2[i]##SUM_IN2...
  res.tab2$"��������,��"[i] <-
    sum_in2[i] - sum_out2[i]##SUM_IN2...
  res.tab2$"����������, ����."[11] <- #
    sum(res.tab2$"����������, ����."[1:10])#
  res.tab2$"����������, ����."[12] <- #
    mean(res.tab2$"����������, ����."[1:10])#
  
  res.tab2$"��������,��"[11] <- sum(res.tab2$"��������,��"[1:10])#
  res.tab2$"��������,��"[12] <-
    mean(res.tab2$"��������,��"[1:10])#
  
  sp1 <-
    c((in1[1, 3] - out1[1, 3]),
      (in1[2, 3] - out1[2, 3]),
      (in1[3, 3] - out1[3, 3]) ,
      (in1[4, 3] - out1[4, 3]),
      (in1[5, 3] - out1[5, 3]),
      (in1[6, 3] - out1[6, 3]),
      (in1[7, 3] - out1[7, 3])
    ) #������ �� [1,3]...[7,3], [1,4]...[7..4]
  res.tab2$"������� ����"[i] <- max(out1[3]) #out1[3] out1[4]
  res.tab2$"����"[i] <- which.max(out1[1:7, 3]) #out1[3] out1[4]
  res.tab2$"������� ���"[i] <- min(out1[3]) #out1[3] out1[4]
  res.tab2$"���� "[i] <- which.min(out1[1:7, 3]) #out1[3] out1[4]
  res.tab2$"�������� ����"[i] <- max(sp1)#
  res.tab2$" ����"[i] <- which.max(sp1)#
  
  write.table(
    res.tab2,
    file = paste0(h[[2]]$name, '.txt'),
    # h[[2]]
    row.names = FALSE,
    sep = ' ',
    
  )
  
  
  #____________3_______________
  p_supply <- price[3 , 2] #���� �������
  p_sale <- price[3 , 3] #���� �������
  p_util <- price[3 , 4] #���� ����������
  Q_supply = sum_in3[i] #SUM_IN2...
  sales = sum_out3[i]#SUM_out2...
  Q_util = Q_supply - sales
  
  TC = Q_supply * p_supply + Q_util * p_util #
  res.tab3[i, 1] = sales * p_sale#
  res.tab3[i, 2] = res.tab3[i, 1] - TC#
  res.tab3[i, 3] = sales#
  res.tab3[i, 4] = Q_util#
  
  res.tab3$"�������"[11] <- sum(res.tab3$"�������"[1:10])#
  res.tab3$"�������"[12] <- mean(res.tab3$"�������"[1:10])#
  res.tab3$"�������"[11] <- sum(res.tab3$"�������"[1:10])#
  res.tab3$"�������"[12] <- mean(res.tab3$"�������"[1:10])#
  
  
  res.tab3$"������������� ������"[i] <-
    round(sd(out1[1:7, 4]), 2) #[1:7,3]
  
  res.tab3$"������������� ������"[11] <- #
    sum(res.tab3$"������������� ������"[1:10])#
  res.tab3$"������������� ������"[12] <- #
    mean(res.tab3$"������������� ������"[1:10])#
  res.tab3$"����������, ����."[i] <- sum_in3[i]#SUM_IN2...
  res.tab3$"��������,��"[i] <-
    sum_in3[i] - sum_out3[i]##SUM_IN2...
  res.tab3$"����������, ����."[11] <- #
    sum(res.tab3$"����������, ����."[1:10])#
  res.tab3$"����������, ����."[12] <- #
    mean(res.tab3$"����������, ����."[1:10])#
  
  res.tab3$"��������,��"[11] <- sum(res.tab3$"��������,��"[1:10])#
  res.tab3$"��������,��"[12] <-
    mean(res.tab3$"��������,��"[1:10])#
  
  sp1 <-
    c((in1[1, 4] - out1[1, 4]),
      (in1[2, 4] - out1[2, 4]),
      (in1[3, 4] - out1[3, 4]) ,
      (in1[4, 4] - out1[4, 4]),
      (in1[5, 4] - out1[5, 4]),
      (in1[6, 4] - out1[6, 4]),
      (in1[7, 4] - out1[7, 4])
    ) #������ �� [1,3]...[7,3], [1,4]...[7..4]
  res.tab3$"������� ����"[i] <- max(out1[4]) #out1[3] out1[4]
  res.tab3$"����"[i] <- which.max(out1[1:7, 4]) #out1[3] out1[4]
  res.tab3$"������� ���"[i] <- min(out1[4]) #out1[3] out1[4]
  res.tab3$"���� "[i] <- which.min(out1[1:7, 4]) #out1[3] out1[4]
  res.tab3$"�������� ����"[i] <- max(sp1)#
  res.tab3$" ����"[i] <- which.max(sp1)#
  
  
  write.table(
    res.tab3,
    file = paste0(h[[3]]$name, '.txt'),
    # h[[2]]
    row.names = FALSE,
    sep = ' ',
  )
  #____________4_______________
  p_supply <- price[4 , 2] #���� �������
  p_sale <- price[4 , 3] #���� �������
  p_util <- price[4 , 4] #���� ����������
  Q_supply = sum_in4[i] #SUM_IN2...
  sales = sum_out4[i]#SUM_out2...
  Q_util = Q_supply - sales
  
  TC = Q_supply * p_supply + Q_util * p_util #
  res.tab4[i, 1] = sales * p_sale#
  res.tab4[i, 2] = res.tab4[i, 1] - TC#
  res.tab4[i, 3] = sales#
  res.tab4[i, 4] = Q_util#
  
  res.tab4$"�������"[11] <- sum(res.tab4$"�������"[1:10])#
  res.tab4$"�������"[12] <- mean(res.tab4$"�������"[1:10])#
  res.tab4$"�������"[11] <- sum(res.tab4$"�������"[1:10])#
  res.tab4$"�������"[12] <- mean(res.tab4$"�������"[1:10])#
  
  
  res.tab4$"������������� ������"[i] <-
    round(sd(out1[1:7, 5]), 2) #[1:7,3]
  
  res.tab4$"������������� ������"[11] <- #
    sum(res.tab4$"������������� ������"[1:10])#
  res.tab4$"������������� ������"[12] <- #
    mean(res.tab4$"������������� ������"[1:10])#
  res.tab4$"����������, ����."[i] <- sum_in4[i]#SUM_IN2...
  res.tab4$"��������,��"[i] <- sum_in4[i] - sum_out4[i]##SUM_IN2...
  res.tab4$"����������, ����."[11] <- #
    sum(res.tab4$"����������, ����."[1:10])#
  res.tab4$"����������, ����."[12] <- #
    mean(res.tab4$"����������, ����."[1:10])#
  
  res.tab4$"��������,��"[11] <- sum(res.tab4$"��������,��"[1:10])#
  res.tab4$"��������,��"[12] <- mean(res.tab4$"��������,��"[1:10])#
  
  sp1 <-
    c((in1[1, 5] - out1[1, 5]),
      (in1[2, 5] - out1[2, 5]),
      (in1[3, 5] - out1[3, 5]) ,
      (in1[4, 5] - out1[4, 5]),
      (in1[5, 5] - out1[5, 5]),
      (in1[6, 5] - out1[6, 5]),
      (in1[7, 5] - out1[7, 5])
    ) #������ �� [1,3]...[7,3], [1,4]...[7..4]
  res.tab4$"������� ����"[i] <- max(out1[5]) #out1[3] out1[4]
  res.tab4$"����"[i] <- which.max(out1[1:7, 5]) #out1[3] out1[4]
  res.tab4$"������� ���"[i] <- min(out1[5]) #out1[3] out1[4]
  res.tab4$"���� "[i] <- which.min(out1[1:7, 5]) #out1[3] out1[4]
  res.tab4$"�������� ����"[i] <- max(sp1)#
  res.tab4$" ����"[i] <- which.max(sp1)#
  
  
  write.table(
    res.tab4,
    file = paste0(h[[4]]$name, '.txt'),
    # h[[2]]
    row.names = FALSE,
    sep = ' ',
  )
  #____________5_______________
  p_supply <- price[5 , 2] #���� �������
  p_sale <- price[5 , 3] #���� �������
  p_util <- price[5 , 4] #���� ����������
  Q_supply = sum_in5[i] #SUM_IN2...
  sales = sum_out5[i]#SUM_out2...
  Q_util = Q_supply - sales
  
  TC = Q_supply * p_supply + Q_util * p_util #
  res.tab5[i, 1] = sales * p_sale#
  res.tab5[i, 2] = res.tab5[i, 1] - TC#
  res.tab5[i, 3] = sales#
  res.tab5[i, 4] = Q_util#
  
  res.tab5$"�������"[11] <- sum(res.tab5$"�������"[1:10])#
  res.tab5$"�������"[12] <- mean(res.tab5$"�������"[1:10])#
  res.tab5$"�������"[11] <- sum(res.tab5$"�������"[1:10])#
  res.tab5$"�������"[12] <- mean(res.tab5$"�������"[1:10])#
  
  
  res.tab5$"������������� ������"[i] <-
    round(sd(out1[1:7, 6]), 2) #[1:7,3]
  
  res.tab5$"������������� ������"[11] <- #
    sum(res.tab5$"������������� ������"[1:10])#
  res.tab5$"������������� ������"[12] <- #
    mean(res.tab5$"������������� ������"[1:10])#
  res.tab5$"����������, ����."[i] <- sum_in5[i]#SUM_IN2...
  res.tab5$"��������,��"[i] <- sum_in5[i] - sum_out5[i]##SUM_IN2...
  res.tab5$"����������, ����."[11] <- #
    sum(res.tab5$"����������, ����."[1:10])#
  res.tab5$"����������, ����."[12] <- #
    mean(res.tab5$"����������, ����."[1:10])#
  
  res.tab5$"��������,��"[11] <- sum(res.tab5$"��������,��"[1:10])#
  res.tab5$"��������,��"[12] <- mean(res.tab5$"��������,��"[1:10])#
  
  sp1 <-
    c((in1[1, 6] - out1[1, 6]),
      (in1[2, 6] - out1[2, 6]),
      (in1[3, 6] - out1[3, 6]) ,
      (in1[4, 6] - out1[4, 6]),
      (in1[5, 6] - out1[5, 6]),
      (in1[6, 6] - out1[6, 6]),
      (in1[7, 6] - out1[7, 6])
    ) #������ �� [1,3]...[7,3], [1,4]...[7..4]
  res.tab5$"������� ����"[i] <- max(out1[6]) #out1[3] out1[4]
  res.tab5$"����"[i] <- which.max(out1[1:7, 6]) #out1[3] out1[4]
  res.tab5$"������� ���"[i] <- min(out1[6]) #out1[3] out1[4]
  res.tab5$"���� "[i] <- which.min(out1[1:7, 6]) #out1[3] out1[4]
  res.tab5$"�������� ����"[i] <- max(sp1)#
  res.tab5$" ����"[i] <- which.max(sp1)#
  
  
  write.table(
    res.tab5,
    file = paste0(h[[5]]$name, '.txt'),
    # h[[2]]
    row.names = FALSE,
    sep = ' ',
  )
  
  #____________6_______________
  p_supply <- price[6 , 2] #���� �������
  p_sale <- price[6 , 3] #���� �������
  p_util <- price[6 , 4] #���� ����������
  Q_supply = sum_in6[i]
  sales = sum_out6[i]
  Q_util = Q_supply - sales
  
  TC = Q_supply * p_supply + Q_util * p_util
  res.tab6[i, 1] = sales * p_sale
  res.tab6[i, 2] = res.tab6[i, 1] - TC
  res.tab6[i, 3] = sales
  res.tab6[i, 4] = Q_util
  
  res.tab6$"�������"[11] <- sum(res.tab6$"�������"[1:10])
  res.tab6$"�������"[12] <- mean(res.tab6$"�������"[1:10])
  res.tab6$"�������"[11] <- sum(res.tab6$"�������"[1:10])
  res.tab6$"�������"[12] <- mean(res.tab6$"�������"[1:10])
  
  
  res.tab6$"������������� ������"[i] <-
    round(sd(out1[1:7, 7]), 2)
  
  res.tab6$"������������� ������"[11] <-
    sum(res.tab6$"������������� ������"[1:10])
  res.tab6$"������������� ������"[12] <-
    mean(res.tab6$"������������� ������"[1:10])
  res.tab6$"����������, ����."[i] <- sum_in6[i]
  res.tab6$"��������,��"[i] <- sum_in6[i] - sum_out6[i]
  res.tab6$"����������, ����."[11] <-
    sum(res.tab6$"����������, ����."[1:10])
  res.tab6$"����������, ����."[12] <-
    mean(res.tab6$"����������, ����."[1:10])
  
  res.tab6$"��������,��"[11] <- sum(res.tab6$"��������,��"[1:10])
  res.tab6$"��������,��"[12] <- mean(res.tab6$"��������,��"[1:10])
  
  sp1 <-
    c((in1[1, 7] - out1[1, 7]),
      (in1[2, 7] - out1[2, 7]),
      (in1[3, 7] - out1[3, 7]) ,
      (in1[4, 7] - out1[4, 7]),
      (in1[5, 7] - out1[5, 7]),
      (in1[6, 7] - out1[6, 7]),
      (in1[7, 7] - out1[7, 7])
    )
  res.tab6$"������� ����"[i] <- max(out1[7])
  res.tab6$"����"[i] <- which.max(out1[1:7, 7])
  res.tab6$"������� ���"[i] <- min(out1[7])
  res.tab6$"���� "[i] <- which.min(out1[1:7, 7])
  res.tab6$"�������� ����"[i] <- max(sp1)
  res.tab6$" ����"[i] <- which.max(sp1)
  
  
  write.table(
    res.tab6,
    file = paste0(h[[6]]$name, '.txt'),
    # h[[2]]
    row.names = FALSE,
    sep = ' ',
  )
  
  #____________7_______________
  p_supply <- price[7 , 2] #���� �������
  p_sale <- price[7 , 3] #���� �������
  p_util <- price[7 , 4] #���� ����������
  Q_supply = sum_in7[i] #SUM_IN2...
  sales = sum_out7[i]#SUM_out2...
  Q_util = Q_supply - sales
  
  TC = Q_supply * p_supply + Q_util * p_util
  res.tab7[i, 1] = sales * p_sale
  res.tab7[i, 2] = res.tab7[i, 1] - TC
  res.tab7[i, 3] = sales
  res.tab7[i, 4] = Q_util
  
  res.tab7$"�������"[11] <- sum(res.tab7$"�������"[1:10])
  res.tab7$"�������"[12] <- mean(res.tab7$"�������"[1:10])
  res.tab7$"�������"[11] <- sum(res.tab7$"�������"[1:10])
  res.tab7$"�������"[12] <- mean(res.tab7$"�������"[1:10])
  
  
  res.tab7$"������������� ������"[i] <-
    round(sd(out1[1:7, 8]), 2)
  
  res.tab7$"������������� ������"[11] <-
    sum(res.tab7$"������������� ������"[1:10])
  res.tab7$"������������� ������"[12] <-
    mean(res.tab7$"������������� ������"[1:10])
  res.tab7$"����������, ����."[i] <- sum_in7[i]
  res.tab7$"��������,��"[i] <- sum_in7[i] - sum_out7[i]
  res.tab7$"����������, ����."[11] <-
    sum(res.tab7$"����������, ����."[1:10])
  res.tab7$"����������, ����."[12] <-
    mean(res.tab7$"����������, ����."[1:10])
  
  res.tab7$"��������,��"[11] <- sum(res.tab7$"��������,��"[1:10])
  res.tab7$"��������,��"[12] <- mean(res.tab7$"��������,��"[1:10])
  
  sp1 <-
    c((in1[1, 8] - out1[1, 8]),
      (in1[2, 8] - out1[2, 8]),
      (in1[3, 8] - out1[3, 8]) ,
      (in1[4, 8] - out1[4, 8]),
      (in1[5, 8] - out1[5, 8]),
      (in1[6, 8] - out1[6, 8]),
      (in1[7, 8] - out1[7, 8])
    )
  res.tab7$"������� ����"[i] <- max(out1[8])
  res.tab7$"����"[i] <- which.max(out1[1:7, 8])
  res.tab7$"������� ���"[i] <- min(out1[8])
  res.tab7$"���� "[i] <- which.min(out1[1:7, 8])
  res.tab7$"�������� ����"[i] <- max(sp1)
  res.tab7$" ����"[i] <- which.max(sp1)
  
  
  write.table(
    res.tab7,
    file = paste0(h[[7]]$name, '.txt'),
    row.names = FALSE,
    sep = ' ',
  )
}



#�������
#1.1
{
  setwd('C:/Users/tkach/�������� ����/������� 1')
  in1 <- read.table("�������.in", head = TRUE, encoding = "UTF-8")
  out1 <- read.table("�������.out", head = TRUE, encoding = "UTF-8")
  data <- out1[1:7, 2]
  dev.new()
  plot(
    x = data,
    type = 'b',
    main = '������� ������� �� ���� ������ � ������ ��������',
    xlab = '���� ������',
    ylab = "������, ��.",
    col = 'blue',
    lwd = 2
  )
}
#2.1

#2.1
{
  setwd('C:/Users/tkach/�������� ����/������� 2')
  price <- read.table("����.txt", head = TRUE, encoding = "UTF-8")
  out2 <- read.table("�������.out", head = TRUE, encoding = "UTF-8")
  data1 <- out2[1:7, 2] * price[1, 3]
  data2 <- out2[1:7, 3] * price[2, 3]
  data3 <- out2[1:7, 4] * price[3, 3]
  dev.new()
  plot(
    x = data1,
    col = 'orange',
    type = 'b',
    fg = 'black',
    lwd = 2,
    main = '�������� ������ ������� �� ������ ��������',
    xlab = '���� ������',
    ylab = '������� �� ������, ���.',
    ylim = c(0, max(data1, data2, data3))
  )
  
  lines(x = data2, col = 'green')
  points(x = data2, col = 'green', pch = 20)
  
  lines(x = data3, col = 'blue')
  points(x = data3, col = 'blue', pch = 20)
  
  
  legend(
    'topleft',
    legend = c('������', '�������', '�������'),
    col = c('orange', 'green', 'blue'),
    pch = 20,
    cex = 0.75,
  )
}

#2.2
{
  setwd('C:/Users/tkach/�������� ����/������� 3')
  price <- read.table("����.txt", head = TRUE, encoding = "UTF-8")
  out3 <- read.table("�������.out", head = TRUE, encoding = "UTF-8")
  data1 <- out3[1:7, 2] * price[1, 3]
  data2 <- out3[1:7, 3] * price[2, 3]
  data3 <- out3[1:7, 4] * price[3, 3]
  dev.new()
  plot(
    x = data1,
    col = 'orange',
    type = 'b',
    fg = 'black',
    lwd = 2,
    main = '�������� ������ ������� � ������� ��������',
    xlab = '���� ������',
    ylab = '������� �� ������, ���.',
    ylim = c(0, max(data1, data2, data3))
  )
  
  lines(x = data2, col = 'green')
  points(x = data2, col = 'green', pch = 20)
  
  lines(x = data3, col = 'blue')
  points(x = data3, col = 'blue', pch = 20)
  
  
  legend(
    'topleft',
    legend = c('������', '�������', '�������'),
    col = c('orange', 'green', 'blue'),
    pch = 20,
    cex = 0.75,
  )
}

#3.1
{
  setwd('C:/Users/tkach/�������� ����/������� 2')
  in1 <-
    out1 <- read.table("�������.in", head = TRUE, encoding = "UTF-8")
  out1 <- read.table("�������.out", head = TRUE, encoding = "UTF-8")
  sale <- read.table("����.txt", head = TRUE, encoding = "UTF-8")
  Pr1 <- vector()
  Pr2 <- vector()
  Pr3 <- vector()
  for (i in 1:7) {
    a <-
      (out1[i, 5] * sale[4, 3] - in1[i, 5] * sale[4, 2] - sale[4, 4] * (in1[i, 5] - out1[i, 5]))
    Pr1 <- c(Pr1, a)
    b <-
      (out1[i, 6] * sale[4, 3] - in1[i, 6] * sale[5, 2] - sale[5, 4] * (in1[i, 6] - out1[i, 6]))
    Pr2 <- c(Pr2, b)
    c <-
      (out1[i, 8] * sale[7, 3] - in1[i, 8] * sale[7, 2] - sale[5, 4] * (in1[i, 8] - out1[i, 8]))
    Pr3 <- c(Pr3, c)
  }
  dev.new()
  plot(
    x = Pr1,
    col = 'orange',
    type = 'b',
    fg = 'black',
    lwd = 2,
    main = '�������� ������� ������� �� ������ ��������',
    xlab = '���� ������',
    ylab = '������� �� ������, ���.',
    ylim = c(min(Pr1, Pr2, Pr3), max(Pr1, Pr2, Pr3))
  )
  
  lines(x = Pr2, col = 'green')
  points(x = Pr2, col = 'green', pch = 20)
  
  lines(x = Pr3, col = 'blue')
  points(x = Pr3, col = 'blue', pch = 20)
  
  
  legend(
    'topleft',
    legend = c('����������� ����', '���', '�������� ������'),
    col = c('orange', 'green', 'blue'),
    pch = 20,
    cex = 0.75,
  )
}

#3.2
{
  setwd('C:/Users/tkach/�������� ����/������� 3')
  in1 <-
    out1 <- read.table("�������.in", head = TRUE, encoding = "UTF-8")
  out1 <- read.table("�������.out", head = TRUE, encoding = "UTF-8")
  sale <- read.table("����.txt", head = TRUE, encoding = "UTF-8")
  Pr1 <- vector()
  Pr2 <- vector()
  Pr3 <- vector()
  for (i in 1:7) {
    a <-
      (out1[i, 5] * sale[4, 3] - in1[i, 5] * sale[4, 2] - sale[4, 4] * (in1[i, 5] - out1[i, 5]))
    Pr1 <- c(Pr1, a)
    b <-
      (out1[i, 6] * sale[4, 3] - in1[i, 6] * sale[5, 2] - sale[5, 4] * (in1[i, 6] - out1[i, 6]))
    Pr2 <- c(Pr2, b)
    c <-
      (out1[i, 8] * sale[7, 3] - in1[i, 8] * sale[7, 2] - sale[5, 4] * (in1[i, 8] - out1[i, 8]))
    Pr3 <- c(Pr3, c)
  }
  dev.new()
  plot(
    x = Pr1,
    col = 'orange',
    type = 'b',
    fg = 'black',
    lwd = 2,
    main = '�������� ������� ������� �� ������ ��������',
    xlab = '���� ������',
    ylab = '������� �� ������, ���.',
    ylim = c(min(Pr1, Pr2, Pr3), max(Pr1, Pr2, Pr3))
  )
  
  lines(x = Pr2, col = 'green')
  points(x = Pr2, col = 'green', pch = 20)
  
  lines(x = Pr3, col = 'blue')
  points(x = Pr3, col = 'blue', pch = 20)
  
  
  legend(
    'topleft',
    legend = c('����������� ����', '���', '�������� ������'),
    col = c('orange', 'green', 'blue'),
    pch = 20,
    cex = 0.75,
  )
}

#4.1
{
  dev.new()
  setwd('C:/Users/tkach/�������� ����/������')
  in1 <-
    read.table("�������, ��..txt", head = TRUE, encoding = "UTF-8")
  P <- as.integer(in1[1:10, 2])
  barplot(
    height = P,
    names.arg = 1:10,
    main = '������� �������� �� ���� ���������',
    col = 'pink',
    xlab = "�������",
    ylab = "�������, ���.",
    ylim = c(min(P), max(P))
  )
}
#5
{
  dev.new()
  in1 <-
    read.table("�������, ��..txt", head = TRUE, encoding = "UTF-8")
  in2 <-
    read.table("������, ��..txt", head = TRUE, encoding = "UTF-8")
  in3 <-
    read.table("�������, �����.txt", head = TRUE, encoding = "UTF-8")
  P1 <- as.integer(in1[1:10, 2])
  P2 <- as.integer(in2[1:10, 2])
  P3 <- as.integer(in3[1:10, 2])
  tabl <- data.frame('�' = 1:10)
  tabl$"�������" <- P1
  tabl$"������" <- P2
  tabl$"�������" <- P3
  P <- as.matrix(tabl[1:10, 2:4])
  Px <- t(P)
  barplot(
    height = Px,
    names.arg = 1:10,
    main = '������� �� ���� ��������� ����',
    ylab = '�������, ���.',
    xlab = "�������",
    col = c('beige', 'pink', 'purple'),
    beside = TRUE,
    ylim = c(min(P), max(P))
  )
  legend(
    'topleft',
    legend = c('�������', '������', '�������'),
    col = c('beige', 'pink', 'purple'),
    pch = 20,
    cex = 0.75,
  )
}

#6
{
  dev.new()
  shops = c(1:10)
  data <- data.frame('shop' = shops, 'profit' = tabl[1:10, 4])
  boxplot(
    profit ~ shops,
    data = data,
    main = "������� ������� �� ���� ���������",
    xlab = "�������",
    ylab = "�������, ���.",
    col = 'coral'
    
  )
}
