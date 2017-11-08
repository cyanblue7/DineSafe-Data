library(data.table);
library(plyr);

root.dir <- '~/Downloads/';
setwd(root.dir);
input.dat <- fread('2017-11-04_dinesafe-database_cleaned.tsv');
input.dat <- as.data.frame(input.dat);
rownames(input.dat) <- input.dat$establishment_id;

input.dat$inspection_fail <- as.numeric(input.dat$inspection_fail != 0);
input.dat$inspection_conpass <- as.numeric(input.dat$inspection_conpass != 0);
input.dat$any_fail <- unlist(
	apply(
		X = input.dat[,c('inspection_conpass', 'inspection_fail')],
		MARGIN = 1,
		FUN = function (x) { return(any(as.logical(x))) }
		)
	);
input.dat$any_fail <- as.numeric(input.dat$any_fail);

# check which establishment has more failures and closures
zip.closed <- as.data.frame(
	table(input.dat$any_fail, input.dat$neighbourhood),
	stringsAsFactors = FALSE
	);
colnames(zip.closed) <- c('closures', 'neighbourhood', 'freq');
zip.closed$closures <- as.numeric(zip.closed$closures);

zip.closed <- zip.closed[with(zip.closed, order(closures, -freq)),];
zip.closed <- dcast(zip.closed, neighbourhood ~ closures, value.var = 'freq');

zip.closed$sum  <- rowSums(zip.closed[,c('0', '1')]);
zip.closed$frac <- zip.closed$'1'/zip.closed$sum;

zip.closed <- zip.closed[with(zip.closed, order(-frac)),];
zip.closed <- zip.closed[which(zip.closed$sum > 50),];

zip.closed$p <- unlist(
	apply(
		X = zip.closed[,c('1', 'sum')],
		MARGIN = 1,
		FUN = function (x) {

			y <- prop.test(x[1], x[2], mean(zip.closed$frac))$p.value;
			return (y);
			}
		)
	);

zip.closed$q <- p.adjust(zip.closed$p, method = 'fdr');
zip.closed$diff <- zip.closed$frac - mean(zip.closed$frac);
zip.closed[which(zip.closed$q <= 0.05),];
