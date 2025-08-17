tvm <- function(pv="solve", fv="solve", pmt="solve", n="solve", i="solve", precision=2) {
	if (
		typeof(fv) == "double" &&
		typeof(pmt) == "double" &&
		typeof(n) == "double" &&
		typeof(i) == "double" &&
		pv == "solve"
	) {
		r <- i/100
		pv <- fv / (1+r)^n + pmt * (1 - (1+r)^(-n)) / r
		
		return(data.frame(pv=round(-pv,precision)))
	}
	else if (
	  typeof(pv) == "double" &&
	  typeof(pmt) == "double" &&
	  typeof(n) == "double" &&
	  typeof(i) == "double" &&
		fv == "solve"
	) {
		r <- i/100
		fv <- (pv * (1+r)^n + pmt * ((1+r)^n - 1) / r)
		
		return(data.frame(fv=round(-fv,precision)))
	}
	else if (
		typeof(pv) == "double" &&
		typeof(fv) == "double" &&
		typeof(n) == "double" &&
		typeof(i) == "double" &&
		pmt == "solve"
	) {
		r <- i/100
		pmt <- (pv + fv / (1+r)^n) / ((1 - (1+r)^(-n)) / r)
		
		return(data.frame(pmt=round(-pmt,precision)))
	}
	else if (
		typeof(pv) == "double" &&
		typeof(fv) == "double" &&
		typeof(pmt) == "double" &&
		typeof(i) == "double" &&
		n == "solve"
	) {
		r <- i/100
		n <- log((fv - pmt/r) / (-pv - pmt/r), base=1+r)
		
		return(data.frame(n=round(n,precision)))
	}
	else if (
		typeof(pv) == "double" &&
		typeof(fv) == "double" &&
		typeof(pmt) == "double" &&
		typeof(n) == "double" &&
		i == "solve"
	) {
		r.c <- c(0.00001, 1)
		x.c <- c(fv / (1+r.c[1])^n + pmt * (1 - (1+r.c[1])^(-n)) / r.c[1] + pv,
		         fv / (1+r.c[2])^n + pmt * (1 - (1+r.c[2])^(-n)) / r.c[2] + pv)
		
		if (x.c[1] == 0) {
			return(data.frame(I=0))
		}
		else if (x.c[2] == 0) {
			return(data.frame(I=100))
		}
		else if (x.c[1] < 0 && x.c[2] > 0) {
			lo <- r.c[1]
			hi <- 1
		}
		else if (x.c[1] > 0 && x.c[2] < 0) {
			lo <- 1
			hi <- r.c[1]
		}
		else {
			print("No sensible solution")
			return("Error")
		}
		
		repeat {
			r <- (lo+hi) / 2
			x <- fv / (1+r)^n + pmt * (1 - (1+r)^(-n)) / r + pv
			if (round(x) == 0) {
				break
			} else if (x < 0) {
				lo <- r
			} else {
				hi <- r
			}
		}
		
		return(data.frame(i=round(r*100,precision)))
	}
	else {
		print("Inappropriate specification")
		return("Error")
	}
}

