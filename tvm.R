tvm <- function(pv="solve", fv="solve", pmt="solve", n="solve", i="solve") {
	if (pv=="solve" &&
	  typeof(fv)=="double" && typeof(pmt)=="double" &&
	  typeof(n)=="double" && typeof(i)=="double") {
		r <- i/100
		pv <- fv/(1+r)^n + pmt*(1-(1+r)^(-n))/r
		return(data.frame(PV=round(-pv,2)))
	} else if (fv=="solve" &&
	  typeof(pv)=="double" && typeof(pmt)=="double" &&
	  typeof(n)=="double" && typeof(i)=="double") {
		r <- i/100
		fv <- (pv*(1+r)^n + pmt*((1+r)^n-1)/r)
		return(data.frame(FV=round(-fv,2)))
	} else if (pmt=="solve" &&
	  typeof(pv)=="double" && typeof(fv)=="double" &&
	  typeof(n)=="double" && typeof(i)=="double") {
		r <- i/100
		pmt <- (pv + fv/(1+r)^n) / ((1-(1+r)^(-n))/r)
		return(data.frame(PMT=round(-pmt,2)))
	} else if (n=="solve" &&
	  typeof(pv)=="double" && typeof(fv)=="double" &&
	  typeof(pmt)=="double" && typeof(i)=="double") {
		r <- i/100
		n <- log((fv-pmt/r)/(-pv-pmt/r), base=1+r)
		return(data.frame(N=round(n,2)))
	} else if (i=="solve" &&
	  typeof(pv)=="double" && typeof(fv)=="double" &&
	  typeof(pmt)=="double" && typeof(n)=="double") {
		r.c <- c(0.00001, 1)
		x.c <- c(fv/(1+r.c[1])^n + pmt*(1-(1+r.c[1])^(-n))/r.c[1] + pv,
		         fv/(1+r.c[2])^n + pmt*(1-(1+r.c[2])^(-n))/r.c[2] + pv)
		if (x.c[1] == 0) { return(data.frame(I=0)) }
		else if (x.c[2] == 0) { return(data.frame(I=100)) }
		else if (x.c[1] < 0 && x.c[2] > 0) { lo <- r.c[1]; hi <- 1 }
		else if (x.c[1] > 0 && x.c[2] < 0) { lo <- 1; hi <- r.c[1] }
		else { print("No sensible solution"); return("Error") }
		repeat {
			r <- (lo+hi)/2
			x <- fv/(1+r)^n + pmt*(1-(1+r)^(-n))/r + pv
			if (round(x) == 0) { break }
			else if (x < 0) { lo <- r }
			else { hi <- r }
		}
		return(data.frame(I=round(r*100,2)))
	} else {
		print("Inappropriate specification")
		return("Error")
	}
}


levered <- function(unlevered, margin, borrow) {
	return(margin^(-1)*unlevered - (margin^(-1)-1)*borrow)
}


optimal.portfolio <- function(debt, equity, cov, rf, A) {
#debt=equity=rf: $r, $s; cov=A: double
	R.d <- debt$r-rf$r; S.d = debt$s^2
	R.e <- equity$r-rf$r; S.e = equity$s^2
	w.d <- (R.d*S.e - R.e*cov) / (R.d*S.e + R.e*S.d - (R.d+R.e)*cov)
	P <- data.frame(
		r = w.d*debt$r + (1-w.d)*equity$r,
		s = ((w.d^2*S.d)+((1-w.d)^2*S.e)+(2*w.d*(1-w.d)*cov))^0.5
	)
	y <- (P$r-rf$r) / (A*P$s^2)
	print(paste("Debt:", round(y*w.d,3)))
	print(paste("Equity:", round(y*(1-w.d),3)))
	print(paste("Risk-free:", round(1-y,3)))
	return(data.frame(
		r = round(y*P$r + (1-y)*rf$r,3),
		s = round(y*P$s,3)
	))
}

