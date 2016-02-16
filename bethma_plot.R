
require(dplyr)
require(gdata)
require(ggplot2)
require(lme4)
require(rjags)
require(ggmcmc)
require(string)
require(BEST)
require(foreign)

#write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
#  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
#}

#data
y <- c1$FAR1D_20
vname <- "OFC planted in Yala?"
it <- c("Minor", "Major")

#histograms
G1_1 <- c1 %>% group_by(irrigtype, FAR1D_20) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))

G1_1$irrigtype[G1_1$irrigtype==0] <- "Minor"
G1_1$irrigtype[G1_1$irrigtype == 1] <- "Major"

#G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 1] <- "Not at all"
#G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 2] <- "A little"
#G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 3] <- "Moderately"
#G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 4] <- "Very"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 1] <- "Time"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 2] <- "Money"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 3] <- "Labor"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 4] <- "Not effective"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 5] <- "Low yield"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 6] <- "Knowledge"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 7] <- "Other"


G1_1$FAR1D_20[G1_1$FAR1D_20 == 1] <- "Yes"
G1_1$FAR1D_20[G1_1$FAR1D_20 == 2] <- "No"
G1_1$FAR1D_20[G1_1$FAR1D_20 == 997] <- "Yes, annual"
G1_1$FAR1D_20[G1_1$FAR1D_20 == 9997] <- "Yes, depends on season"


# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 1] <- "Income"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 2] <- "Water scarcity"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 3] <- "Expert advice"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 4] <- "Common"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 5] <- "Custom"
# G1_1$ADP1_G1_1[G1_1$ADP1_G1_1 == 6] <- "Other"


bar1 <- ggplot(G1_1, aes(x = c1$FAR1D_20, y = freq, fill = irrigtype)) +
  labs(x = "", y = "Percent", title = vname) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.title = element_blank())
#geom_text(aes(label = irrigtype), size = 3, hjust = 1 , vjust = 1, angle = 90)

print(bar1)