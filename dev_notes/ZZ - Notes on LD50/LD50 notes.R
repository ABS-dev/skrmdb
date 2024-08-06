a = dcast(challenge, TestID ~ log_dil, value.var = "Survivors")
setkey(a, TestID)
setkey(LD, TestID)
a = a[LD]
setorder(a, `11`, `10`, `9`, `8`, `7`)
a[, LD50_SK := round(LD50_SK, 2)]
a[, LD50_RM := round(LD50_RM, 2)]
a[, LD50_DB := round(LD50_DB, 2)]

print(a, nrow = a)


l5 = c()
l6 = c()
l7 = c()
j5 = 0
j6 = 0
j7 = 0
for (i7 in 0:3) {
  for (i8 in i7:3) {
    for (i9 in i8:3) {
      for (i10 in i9:3) {
        for (i11 in i10:3) {
          j5 <<- j5 + 1
          l5 = c(l5, i7, i8, i9, i10, i11)
          for (i12 in i11:3) {
            j6 <<- j6 + 1
            l6 = c(l6, i7, i8, i9, i10, i11, i12)
            for (i13 in i12:3) {
              j7 <<- j7 + 1
              l7 = c(l7, i7, i8, i9, i10, i11, i12, i13)
            }
          }
        }
      }
    }
  }
}

dt5 = data.table(
  TestID = rep(1:j5, each = 5),
  Survivors = l5,
  Total = 3,
  log_dil = 7:11
)

dt6 = data.table(
  TestID = rep(1:j6, each = 6),
  Survivors = l6,
  Total = 3,
  log_dil = 7:12
)


dt7 = data.table(
  TestID = rep(1:j7, each = 7),
  Survivors = l7,
  Total = 3,
  log_dil = 7:13
)

LDtemp5 = dt5[, .(LD50_SK = SpearKarb(y = Survivors, 
                                      n = Total, 
                                      x = log_dil)$ed,
                  LD50_RM = ReedMuench(y = Survivors, 
                                       n = Total, 
                                       x = log_dil)$ed,
                  LD50_DB = DragBehr(y = Survivors, 
                                     n = Total, 
                                     x = log_dil)$ed), 
              TestID]

LDtemp6 = dt6[, .(LD50_SK = SpearKarb(y = Survivors, 
                                      n = Total, 
                                      x = log_dil)$ed,
                  LD50_RM = ReedMuench(y = Survivors, 
                                       n = Total, 
                                       x = log_dil)$ed,
                  LD50_DB = DragBehr(y = Survivors, 
                                     n = Total, 
                                     x = log_dil)$ed), 
              TestID]
LDtemp7 = dt7[, .(LD50_SK = SpearKarb(y = Survivors, 
                                      n = Total, 
                                      x = log_dil)$ed,
                  LD50_RM = ReedMuench(y = Survivors, 
                                       n = Total, 
                                       x = log_dil)$ed,
                  LD50_DB = DragBehr(y = Survivors, 
                                     n = Total, 
                                     x = log_dil)$ed), 
              TestID]

LDtemp5[, LD50_SK := round(LD50_SK, 2)]
LDtemp5[, LD50_RM := round(LD50_RM, 2)]
LDtemp5[, LD50_DB := round(LD50_DB, 2)]
LDtemp6[, LD50_SK := round(LD50_SK, 2)]
LDtemp6[, LD50_RM := round(LD50_RM, 2)]
LDtemp6[, LD50_DB := round(LD50_DB, 2)]
LDtemp7[, LD50_SK := round(LD50_SK, 2)]
LDtemp7[, LD50_RM := round(LD50_RM, 2)]
LDtemp7[, LD50_DB := round(LD50_DB, 2)]

b5 = dcast(dt5, TestID ~ log_dil, value.var = "Survivors")
setkey(b5, TestID)
setkey(LDtemp5, TestID)
b5 = b5[LDtemp5]

b6 = dcast(dt6, TestID ~ log_dil, value.var = "Survivors")
setkey(b6, TestID)
setkey(LDtemp6, TestID)
b6 = b6[LDtemp6]

b7 = dcast(dt7, TestID ~ log_dil, value.var = "Survivors")
setkey(b7, TestID)
setkey(LDtemp7, TestID)
b7 = b7[LDtemp7]


temp5 = b5[`9` == 0 & `10` == 1 & `11` == 1]
temp6 = b6[`9` == 0 & `10` == 1 & `11` == 1]
temp7 = b7[`9` == 0 & `10` == 1 & `11` == 1]

setorder(temp5, -LD50_DB)
setorder(temp6, -LD50_DB)
setorder(temp7, -LD50_DB)

temp5
temp6
temp7
