library("caTools")

# Clean-looking ECG image (source: https://pcs12.azureedge.net/ekgtracings/70.gif)
ecg_img <- read.gif("ecg.gif")

# Plot image
plot_image <- function(M, ...) image(t(M[nrow(M):1,]), ...)
plot_image(ecg_img$image, col=ecg_img$col)

# Capture mouse click coordinates along the curve via `locator()` function, which will return the (x,y) coordinates 
# of the mouse clicks. Done many times to decrease variance.
coords01 <- locator(22)
coords02 <- locator(22) 
coords03 <- locator(22) 
coords04 <- locator(22) 
coords05 <- locator(22)

# Concatenate all x's from all of the list and all y's from all of the lists. 
xs <- c(coords01$x, coords02$x, coords03$x, coords04$x, coords05$x)
ys <- c(coords01$y, coords02$y, coords03$y, coords04$y, coords05$y)

# Combine into df
df_ecg <- data.frame(xs, ys)

# Scale y's between 0 & 1
# Stretch out x's to be 1 second long
scaledX <- (1 / max(xs)) * xs
scaledY <- (1 / max(ys)) * ys

# Recombine into df 
ecgData <- c(scaledX, scaledY)

# Export to csv 
write.csv(ecgData, 'ecg_manufactured.csv')