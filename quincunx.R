http://www.econ.uiuc.edu/~roger/courses/476/routines/quincunx.R
"quincunx" <-
function(balls = 100, layers = 30, speed = 4, cex = 1)
#A Toy Quincunx -- RWK December 5, 1994, Revised for R:  March 8 2001.
{
	bins <- (-6:6)*2
	depth <- .25 + 2*balls/layers 
	plot(c( - layers, layers), c(-depth, layers), type = "n", bty = "n", 
		xaxt = "n", yaxt = "n", ylab = "", xlab = "")
	title("Quincunx")
	x <-  - layers:layers
	y <- abs(x)
	b <- 2 * (1:(layers/2)) - 1
	b <- c( - b, b)
	segments(b, rep(-depth, length(b)), b, rep(0, length(b)))
	for(i in 1:layers) {
		points(( - (i - 1):(i - 1))[(1:i) * 2], 
			rep(layers - i, i),pch="o",cex=.5*cex)
	}
	for(i in 1:(balls/speed)) {
		path <- matrix(2 * (runif(speed * layers) > 0.5) - 1, layers, 
			speed)
		x <- apply(path, 2, "cumsum")
		old.y <- rep(layers - 1.5, speed)
		old.x <- rep(0, speed)
		for(k in 1:layers) {
			eps <- rep(rnorm(1)/10, speed)
			points(x[k,  ] + eps, rep(layers - k - 1.5, speed), 
				cex = cex, pch = "o",
				col=rainbow(layers)[sample(layers,speed)])
			old.y <- rep(layers - k - 1.5, speed)
			old.x <- x[k,  ] + eps
			#system("sleep 1")
			points(old.x, old.y, cex = cex, pch = "o", col = 0)
		}
		hgt <- i/(balls/speed)
		bins <- c(bins, x[layers,  ])
		pile <- table(bins)-1
		colors <- rainbow(length(pile))
		names(colors) <- names(pile)
		points(x[layers,  ], -depth +.25*pile[paste(x[layers,])], 
			cex = cex, pch = "o",col=colors[paste(x[layers,])])
	}
	z <- density(as.numeric(rep(names(pile),pile)))
	lines(z$x,-depth+balls*z$y/2)
	return(table(bins))
}
quincunx()
