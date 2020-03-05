# Question 1

sex = c(1,1,1,1,1,2,2,2,2,2); smoking = c(1,0,1,0,1,0,0,0,1,1); age=c(21:30);
zz = data.frame(sex, smoking, age);

# apply(zz[-1,], 2, min)
# apply(X, MARGIN (1 is rows and 2 is columns), FUN, ...)
print(apply(zz[-1,], 2, min))

# zz[zz[,3]>24,]
print(zz[zz[,3]>24,])

# zz[order(zz["smoking"], zz["age"]), ]
print(zz[order(zz["smoking"], zz["age"]), ])

# subset(zz, zz["sex"]==2)
print(subset(zz, zz["sex"]==2))

# apply(zz[,-3], 1, function(x){ sum(x) })
print(apply(zz[,-3], 1, function(x){ sum(x) }))

# Question 2

mylist=list(sex = c(1,1,1,1,2,2,2,2,2,2), smoking = c(1,0,1,0,1,0,1,0,1,1), age=c(21:30));

# x = lapply(mylist, max); print(x); y = sapply(mylist, min); print(y)
x = lapply(mylist, max); 
print(x); 
y = sapply(mylist, min); 
print(y)

# z = mapply(function(p1,p2,p3){c(p1,p2,p3)}, mylist$sex, mylist[[2]], mylist[['age']]); print(z)
z = mapply(function(p1,p2,p3){c(p1,p2,p3)}, mylist$sex, mylist[[2]], mylist[['age']]); 
print(z)

# class(x); class(y); class(z)
print(class(x)); 
print(class(y));
print(class(z));


# Question 3

geneExpr=data.frame(gene=LETTERS[1:10], 
                    expr1=c(2.1, 4.5, 6.8, 7.9, 8.1, 5.0, 4.6, 3.2, 3.5, 7.8), 
                    expr2=c(6.1, 4.2, 2.8, 0.9, 0.1, 3.0, 2.6, 8.2, 3.4, 6.8));

# a)
counter<-0
for (i in 1:length(geneExpr)) {
  if (geneExpr[i,2]<=7){
    counter<-counter+1
  }
}
print(counter)

# b)
myMin<-function(x, cut) { min(x[x>cut]) }
print(apply(geneExpr[,-1],2, myMin, cut=4))


# Question 4


# a)

# * Quantifier - Matches between zero and unlimited times, 
# as many times as possible, giving back as needed (greedy)

strings <- c("abababa", "aaba", "aabbaa", "aba")
print(grep("a(ab)*a", strings, value = TRUE))


# b)

# + Quantifier - Matches between one and unlimited times, as many times as possible, 
# back as needed (greedy)

# ? Quantifier - Matches between zero and one times, 
# as many times as possible, giving back as needed (greedy)

strings <- c("abc", "ac", "abbb", "bbc")
print(grep("ab+c?", strings, value = TRUE))

# c)

strings <- c("abc", "azc", "abcbcbcbc", "ac")
print(grep("a.[bc]+", strings, value = TRUE))


# Quesiton 4
library(stringr)
phones <- c("+1 (123) 904 6517", "(212) 323 2218", " 2197338965", 
            "329-293-8753 ", "595 794 7569", "+1 387 287 6718", 
            "233.398.9187 ", "482 952 3315", "Work: 579-499-7527", 
            "Home: 543.355.3679")

# (^ |^[\(]?\d\d\d[\)]?|^(\+1 [\(]?\d\d\d[\)]?)|: (.?\d\d\d))(\.|-| |)([\(]?\d\d\d[\)]?)(\.|-| |)(\d\d\d\d)

print(str_detect(phones, 
                 "(^ |^[\\(]?\\d\\d\\d[\\)]?|^(\\+1 [\\(]?\\d\\d\\d[\\)]?)|: 
                 (.?\\d\\d\\d))(\\.|-| |)([\\(]?\\d\\d\\d[\\)]?)(\\.|-| |)(\\d\\d\\d\\d)"))