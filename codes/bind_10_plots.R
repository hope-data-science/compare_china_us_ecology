
for(i in 1:10){
  get(str_c("p",i))+
    ggtitle(str_c(LETTERS[i],")")) +
    theme(legend.position = "none")->np
  assign(str_c("pp",i),np)
}

p2   #extract lengend from this page

grid.arrange(pp1,pp2,pp3,pp4,pp5,pp6,pp7,pp8,pp9,pp10,ncol=2)

####

library(gridExtra)
grid.arrange(p1 + ggtitle("A)"),
             p3 + ggtitle("B)"),
             p5 + ggtitle("C)"),
             p7 + ggtitle("D)"),
             p9 + ggtitle("E)"),ncol = 1)

grid.arrange(p2 + ggtitle("A)"),
             p4 + ggtitle("B)"),
             p6 + ggtitle("C)"),
             p8 + ggtitle("D)"),
             p10 + ggtitle("E)"),ncol = 1)
