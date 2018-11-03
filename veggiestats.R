# ATTENTION:
# - Umlaute in "Gemuesegewichtneu.csv" have been replaced!
# - Make sure "Gemuesegewichtneu.csv" is in same directory as script!
#   --> Session --> Set working directory --> To Source File Locations

# Read data
x = read.table("Gemuesegewichtneu.csv", sep=";", header=TRUE, dec=",")

# Which vegetables?
levels(x$Name)

# --> There is an error ("Mairueueben")
# Fixing error:
x$Name[x$Name == 'Mairueueben'] = 'Mairueben'
x$Name = droplevels(x$Name)

# Default summary... not really helpful
summary(x)

# Default boxplot
par(mar=c(7,5,1,1))
plot(x = x$Name, y = x$Gewicht, las=2, ylab="Gewicht (g)")

# Some basic statistics for "Gewicht" per vegetable (i.e. per factor "Name")
tapply(x$Gewicht, x$Name, summary)

# Which supermarkets?
levels(x$Supermarkt)

# Does the mean depend on supermarket?
# --> One Way ANOVA?
aov_markets = aov(Gewicht~Supermarkt, x)
summary(aov_markets)

# BUT THAT DOES NOT MAKE ANY SENSE, OF COURSE... 
# --> need to apply ANOVA per vegetable!
aov1 = list()
noaov1 = c()
for (veggie in levels(x$Name)) {
  # Save result in list
  tryCatch(
    {
      aov1[[veggie]] = aov(Gewicht~Supermarkt, x[x$Name==veggie,])
    },
    # Need to catch errors because for some vegetables there are just not enough data
    error=function(cond) {
    message(paste("Error with vegetable:", veggie))
    #message("\tHere's the original error message:")
    #message(paste("\t", cond, sep=""))
    }
  )
  if (is.null(aov1[[veggie]])) noaov1 = c(noaov1, veggie)
}
# No ANOVA
noaov1

# Now get aov summary from our result list
(aov1_summary = lapply(aov1, summary) )
significant = c()
for (veggie in names(aov1_summary)) {
  if (aov1_summary[[veggie]][[1]][["Pr(>F)"]][1] < 0.001) {
    significant=c(significant, veggie)
  }
}
print("Supermarket influences mass for:")
print(significant)
                      
                      
# Mean mass appears to depend on supermarket for Bundmoehren, Lauchzwiebeln, Radieschen, and Rettich
# But can we tell which supermarkets, or rather pairs of supermarkets, actually make the difference?
# --> Tukey's HSD test?
# Attention: This outputs pair-wise comparisions for EACH vegetable!
# Adapted from https://datascienceplus.com/one-way-anova-in-r/
lapply(aov1[significant], TukeyHSD, conf.level = 0.95)

# Diagnostic plot to check underlying assumptions
op = par(mfrow=c(2,2))
plot(aov1[["Bundmoehren"]])
par(op)

# Another diagnostic plot from gplots
# install.packages("gplots") 
library(gplots)
plotveggies = setdiff(levels(x$Name), noaov1)
par(mfrow=c(4,2), mar=c(2,4,2,1))
for (veggie in plotveggies) {
  plotmeans(Gewicht~Supermarkt, x[x$Name==veggie,], main=veggie, ylab = "Gewicht (g)")
}

