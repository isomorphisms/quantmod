require(RUnit)
require(quantmod)

test.getSymbols.FRED.NFP <- function() {
	purpose <- "Testing (at least): net connectivity; \n\t getSymbols.FRED thru its src='FRED' (all caps!) method; \n\t and that the first date in the source is what it was when I wrote this test!"
	first.day.of.nfp.mine.static <- structure(29923L, .indexCLASS = "Date", tclass = "Date", .indexTZ = "UTC", tzone = "UTC", src = "FRED", updated = structure(1498517478.59432, class = c("POSIXct", 
"POSIXt")), class = c("xts", "zoo"), index = structure(-978307200, tzone = "UTC", tclass = "Date"), .Dim = c(1L, 
1L), .Dimnames = list(NULL, "PAYEMS"))
	getSymbols('PAYEMS', src='FRED')
	# should be saved
	first.day.of.nfp.new.dynamic <- head(PAYEMS,1)

	RUnit:::checkEquals( first.day.of.nfp.mine.static, first.day.of.nfp.new.dynamic, msg=purpose )
}


test.getSymbols.oanda.xaucad <- function() {
	purpose <- "Testing (at least): net connectivity; \n\t currency specification \n\t new.env() call \n\t auto.assign=TRUE \n\t getSymbols.FRED thru its src='FRED' (all caps!) method; \n\t and that the date restriction (last 180 days) is what it was when I wrote this test!"

	currencies <- new.env()
	RUnit:::checkException(getSymbols.oanda('xaucad', env=currency, auto.assign=TRUE, from='2000-01-01'), msg="wrong currency specification")
	RUnit:::checkException(getSymbols.oanda('XAU/CAD', env=currency, from='2000-01-01'), msg="no auto.assign warning")
	RUnit:::checkException(getSymbols.oanda('XAU/CAD', auto.assign=TRUE, from='2000-01-01'), msg="need a new.env() to store it in!")
	RUnit:::checkException(getSymbols.oanda('XAU/CAD', env=currency, auto.assign=TRUE, from='2000-01-01'), msg="going back too far in dates")
}
