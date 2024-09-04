arab_data = read.csv("arab.csv")

values = data.frame(mean = numeric(0),
varianz = numeric(0),
diff = numeric(0))

for (gene in arab_data[, 4:ncol(arab_data)]) {

    mean = mean(gene)
    var = var(gene)
    diff = var - mean

    values[nrow(values)+1,]=c(mean,var,diff)
}

png("plots/mean_comparison.png", width = 800, height = 600)
plot(values$mean,values$varianz,
xlab="mean",ylab="var",
xlim=c(0,1000000),ylim=c(0,1000000))
abline(a = 0, b = 1, col = "red", lwd = 2)
dev.off() 

png("plots/mean_var_diff.png", width = 800, height = 600)
hist(values$diff,
breaks = 100,
xlim = c(min(values$diff), max(values$diff)))
dev.off() 

filtered_values = values[values$mean <= 1000, ]
png("plots/mean.png", width = 800, height = 600)
hist(filtered_values$mean,
breaks=100,
xlim = c(-1000,1000))
dev.off() 

print(max(values$mean))