mu<-30
sig<-4

a<-pnorm(40,mean =mu, sd = sig)

writeLines(as.character(round(a,3)))

for (i in 1:2) {
  inp<-readline()
}

writeLines(inp)

Phys<-as.numeric(strsplit('15  12  8   8   7   7   7   6   5   3',"\\s+",perl = TRUE)[[1]])
Hist<-as.numeric(strsplit('10  25  17  11  13  17  20  13  9   15',"\\s+",perl = TRUE)[[1]])

c = cor(Phys,Hist)

writeLines(as.character(round(c,3)))

str <-"2 7
0.18 0.89 109.85
1.0 0.26 155.72
0.92 0.11 137.66
0.07 0.37 76.17
0.85 0.16 139.75
0.99 0.41 162.6
0.87 0.47 151.77
4
0.49 0.18
0.57 0.83
0.56 0.64
0.76 0.18"

(F,N)<-as.numeric(strsplit(str,,"\\s+",perl = TRUE))