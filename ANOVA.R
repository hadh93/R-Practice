T1 <- c(8, 9, 6, 8, 5)
T2 <- c(5, 4, 7, 6, 6)
T3 <- c(9, 3, 2, 4)

#Easy way
data <- data.frame(group = rep(c("A", "B", "C"), each = 5),
                   value = c(T1,T2,T3))

fit <- aov(value ~ group, data = data)
summary(fit)

#Hard way

length(T3) <- length(T1)

df <- data.frame(T1=T1, T2=T2, T3=T3)


Y_Bar = sum(df,na.rm=TRUE) / sum(!is.na(df))

SST = 0
for(i in 1:ncol(df)) {
  for(j in 1:length(df[,i]))
    if (!is.na(df[j,i])){
      SST = SST+ (df[ j, i] - Y_Bar)^2
    }
}

n1 = sum(!is.na(df$T1))
n2 = sum(!is.na(df$T2))
n3 = sum(!is.na(df$T3))

T1_mean = sum(df$T1, na.rm=TRUE) / n1
T2_mean = sum(df$T2, na.rm=TRUE) / n2
T3_mean = sum(df$T3, na.rm=TRUE) / n3

SSB = n1*(T1_mean - Y_Bar)^2 +n2*(T2_mean - Y_Bar)^2 +n3*(T3_mean - Y_Bar)^2

SSW = 0

means = c(T1_mean, T2_mean, T3_mean)

for(i in 1:ncol(df)) {
  mean = means[i]
  for(j in 1:length(df[,i]))
    if (!is.na(df[j,i])){
      SSW = SSW + (df[j,i]-mean)^2
    }
}

N = 14
k = 3

df_between = k-1
df_within = N-k

MSB = SSB / df_between

MSW = SSW / df_within

F_statistics = MSB / MSW

p_value <- pf(F_statistics, df_between, df_within, lower.tail = FALSE)
