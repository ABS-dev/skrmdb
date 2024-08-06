library(data.table)
library(ggplot2)

data = data.table(dil = as.numeric(1:5),
                  num.pos = as.numeric(c(2, 5, 1, 2, 3)),
                  num.neg = as.numeric(0),
                  total   = as.numeric(c(5, 5, 4, 4, 4)))
data[, num.neg := total - num.pos]

p = ggplot(data, aes(x = dil)) +
  geom_bar(aes(y = total), stat = "identity", fill = "darkgreen") +
  geom_bar(aes(y = num.pos), stat = "identity", fill = "orange") +
  # geom_line(aes(y = num.pos)) +
  scale_y_continuous(limits = c(0, 5))

plot(p)

p = ggplot(data, aes(x = dil)) +
  geom_bar(aes(y = 4 * total / total), stat = "identity", fill = "darkgreen") +
  geom_bar(aes(y = 4 * num.pos / total), stat = "identity", fill = "orange") +
  # geom_line(aes(y = num.pos)) +
  scale_y_continuous(limits = c(0, 4))

plot(p)

# Correct way to handle this:

data1 = copy(data)
data1[, scale.pos := num.pos * 20 / total / 5]
data1[, scale.neg := num.neg * 20 / total / 5]

data1[, cum.pos := cumsum(scale.pos)]
data1[, cum.neg := rev(cumsum(rev(scale.pos)))]

temp1 = melt(data1, id.vars = "dil", measure.vars = c("cum.pos", "cum.neg"))

colors = c("orange", "darkgreen")
names(colors) = c("cum.pos", "cum.neg")

fit1 = lm(c(5.6, 6.6) ~ c(2, 3))
fit2 = lm(c(10.0, 6.0) ~ c(2, 3))
M = matrix(c(1, 1, -fit1$coefficients[2], -fit2$coefficients[2]), nrow = 2)
B = matrix(c(fit1$coefficients[1], fit2$coefficients[1]), nrow = 2)
pt1 = as.data.table(t(solve(M) %*% B))
pt1 = rbind(pt1, pt1)
setnames(pt1, c("value", "dil"))
pt1[2, value := 0]
p0 = ggplot() +
  geom_point(data = temp1, aes(x = dil, y = value, color = variable), size = 3) +
  geom_line(data = temp1, aes(x = dil, y = value, color = variable),  size = 1.5) +
  scale_y_continuous(limits = c(0, 15)) + 
  scale_color_manual(values = colors)

p1 = p0 + 
  geom_point(data = pt1, aes(x = dil, y = value), size = 2) +
  geom_line(data = pt1, aes(x = dil, y = value), linetype = "dotted", size = 1.5)
  
plot(p1)


data2 = copy(data)
data2[, cum.pos := cumsum(num.pos)]
data2[, cum.neg := rev(cumsum(rev(num.neg)))]

temp2 = melt(data2, id.vars = "dil", measure.vars = c("cum.pos", "cum.neg"))

fit1 = lm(c(2, 7) ~ c(1, 2))
fit2 = lm(c(9, 6) ~ c(1, 2))
M = matrix(c(1, 1, -fit1$coefficients[2], -fit2$coefficients[2]), nrow = 2)
B = matrix(c(fit1$coefficients[1], fit2$coefficients[1]), nrow = 2)
pt2 = as.data.table(t(solve(M) %*% B))
setnames(pt2, c("value", "dil"))


p2 = p0 +  
  geom_point(data = temp2, aes(x = dil, y = value, color = variable), size = 1.5) +
  geom_line(data = temp2, aes(x = dil, y = value, color = variable),  size = 1, linetype = "dashed") +
  geom_point(data = pt2, aes(x = dil, y = value), size = 2)
  

print(p2)


fit1 = lm(c(1.6, 5.6) ~ c(1, 2))
fit2 = lm(c(11.6, 10) ~ c(1, 2))
M = matrix(c(1, 1, -fit1$coefficients[2], -fit2$coefficients[2]), nrow = 2)
B = matrix(c(fit1$coefficients[1], fit2$coefficients[1]), nrow = 2)
pt3 = as.data.table(t(solve(M) %*% B))
setnames(pt3, c("value", "dil"))

temp3 = temp1[dil <= 2]
temp3[, dil := as.numeric(dil)]
temp3[dil == max(dil), dil := pt3$dil]
temp3[dil == max(dil), value := pt3$value]

pt3a = rbind(pt3, pt3)
pt3a[2, value := 0]
p3 = p1 + 
  geom_line(data = temp3, aes(x = dil, y = value, color = variable), size = 1.25, linetype = "dotted") +
  geom_point(data = pt3a, aes(x = dil, y = value), size = 2) +
  geom_line(data = pt3a, aes(x = dil, y = value), linetype = "dotted", size = 1.5)

plot(p3)

