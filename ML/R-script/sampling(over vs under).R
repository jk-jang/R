#sampling(over vs under)
library(ggplot2)
set.seed(0)
A1 <- rnorm(1000, 0, 1)
A2 <- rnorm(1000, 0, 1)

data1 <- data.frame(x1=A1, x2=A2)
data1$label <- 'false'
B1 <- rnorm(50, 1.7, 0.5)
B2 <- rnorm(50, 1.7, 0.5)
data2 <- data.frame(x1=B1, x2=B2)
data2$label <- 'true'
library(reshape2)

df <- rbind(data1,data2)

p <- ggplot(data=df, aes(x=x1, y=x2, color=label))+
  geom_point()+
  theme_bw()+
  xlim(-3,3)+
  ylim(-3,3)+
  theme(legend.position='none')

data <- melt(df)
df <- rbind(data1,data2)
data <- melt(df)

p <- ggplot(data=data, aes(x=value, fill=label, ..count..))+
  xlim(-3,3)+
  geom_density()+
  theme_bw()+
  theme(legend.position='none')
ggsave('C:/Users/JeanG/Dropbox/dens_origin.png', width=4, height=1.5)

p <- ggplot(data=df, aes(x=x1, y=x2, color=label))+
  geom_point()+
  theme_bw()+
  xlim(-3,3)+
  ylim(-3,3)+
  theme(legend.position='none')
ggsave('C:/Users/JeanG/Dropbox/scatter_origin.png', width=4, height=4)

df <- rbind(data1,data2[sample(1:nrow(data2), 1000, replace=T),])
data <- melt(df)
p <- ggplot(data=data, aes(x=value, fill=label, ..count..))+
  geom_density()+
  xlim(-3,3)+
  theme_bw()+
  theme(legend.position='none')
ggsave('C:/Users/JeanG/Dropbox/dens_over.png', width=4, height=2)
p <- ggplot(data=df, aes(x=x1, y=x2, color=label))+
  geom_point()+
  theme_bw()+
  xlim(-3,3)+
  ylim(-3,3)+
  theme(legend.position='none')
ggsave('C:/Users/JeanG/Dropbox/scatter_over.png', width=4, height=4)

df <- rbind(data1[sample(1:nrow(data1), 100),],data2)
data <- melt(df)
p <- ggplot(data=data, aes(x=value, fill=label, ..count..))+
  geom_density()+
  xlim(-3,3)+
  theme_bw()+
  theme(legend.position='none')
ggsave('C:/Users/JeanG/Dropbox/dens_under.png', width=4, height=1)
p <- ggplot(data=df, aes(x=x1, y=x2, color=label))+
  geom_point()+
  theme_bw()+
  xlim(-3,3)+
  ylim(-3,3)+
  theme(legend.position='none')
ggsave('C:/Users/JeanG/Dropbox/scatter_origin.png', width=4, height=4)
