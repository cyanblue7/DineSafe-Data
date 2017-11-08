library(data.table);
library(plyr);

root.dir <- '~/Downloads/';
setwd(root.dir);
input.dat <- fread('dinesafe_data_export.csv');

# clean up data and only keep those that are restaurants and take outs
types.to.keep <- c('Bake Shop','Bakery','Food Take Out','Ice Cream / Yogurt Vendors','Restaurant');
input.dat <- input.dat[which(input.dat$latest_type %in% types.to.keep),];

input.dat.subset <- input.dat[,c('inspection_date', 'establishment_id')];
input.dat.subset$inspection_date <- as.Date(input.dat.subset$inspection_date);

establishment.counts <- as.data.frame(table(input.dat$establishment_id));
establishment.counts <- establishment.counts[with(establishment.counts, order(-Freq)),];

input.dat$inspection_date_year <- format(as.Date(input.dat$inspection_date), "%Y");
input.dat$inspection_date_year <- as.numeric(input.dat$inspection_date_year);

input.dat.min <- aggregate(
	x = input.dat.subset[,'inspection_date', drop = FALSE],
	by = list(establishment_id = as.factor(input.dat.subset$establishment_id)),
	FUN = min
	);

input.dat.max <- aggregate(
	x = input.dat.subset[,'inspection_date', drop = FALSE],
	by = list(establishment_id = as.factor(input.dat.subset$establishment_id)),
	FUN = max
	);

cols.to.aggregate <- c('address', 'lat', 'lng', 'latest_name', 'latest_type', 'deleted_at', 'postal_code');

input.dat.unique <- aggregate(
	x = input.dat[,cols.to.aggregate, with = FALSE],
	by = list(establishment_id = as.factor(input.dat.subset$establishment_id)),
	FUN = unique
	);

input.dat.paste <- aggregate(
	x = input.dat[,c('infraction_details', 'inspection_status', 'inspection_date'), drop = FALSE],
	by = list(establishment_id = as.factor(input.dat.subset$establishment_id)),
	FUN = paste,
	collapse = '; '
	);

input.merge <- merge(input.dat.min, input.dat.max,  by = 'establishment_id');
input.merge <- merge(input.merge, input.dat.unique, by = 'establishment_id');
input.merge <- merge(input.merge, input.dat.paste,  by = 'establishment_id');

colnames(input.merge)['inspection_date.x' == colnames(input.merge)] <- 'first_inspection_date';
colnames(input.merge)['inspection_date.y' == colnames(input.merge)] <- 'last_inspection_date';

input.merge <- input.merge[which(input.merge$first_inspection_date >= as.Date('2011-01-01')),];

# remove restaurants with multiple establishment ID but same address
count <- ddply(input.merge,.(input.merge$address,input.merge$latest_name),nrow);
names(count)<-c('address','latest_name','number')
remove <- as.character(count[count$number>1,c('latest_name')])
input.merge <- input.merge[which(!input.merge$latest_name %in% remove),]

input.merge$establishment_id <- as.character(input.merge$establishment_id);
input.merge$deleted_at['' == input.merge$deleted_at] <- NA;

today.date <- Sys.Date();

for (this.index in 1:nrow(input.merge)) {

	if (!is.na(input.merge$deleted_at[this.index])) {

		deleted.time <- as.Date(
			strsplit(input.merge[this.index , 'deleted_at'], split = ' ')[[1]][1]
			);

		if (deleted.time < as.Date('2017-01-01')) {

			input.merge[this.index, 'last_inspection_date'] <- deleted.time;
			}

		} else { input.merge[this.index, 'last_inspection_date'] <- today.date; }
	}

input.merge$status <- as.numeric(!is.na(input.merge$deleted_at));
input.merge$time   <- input.merge$last_inspection_date - input.merge$first_inspection_date;

input.merge        <- input.merge[which(input.merge$time > 180),];
input.merge$time   <- as.numeric(input.merge$time / 365.25);

input.merge$postal_code[which(input.merge$postal_code == 'NULL')] <- NA;
input.merge$postal_code[which(input.merge$postal_code == '')]     <- NA;
input.merge$neighbourhood <- substring(input.merge$postal_code, first = 1, last = 3);

# addinformation about inspection time and pass/conditional pass/closed statuses
input.merge$inspection_times   <- NA;
input.merge$inspection_pass    <- NA;
input.merge$inspection_conpass <- NA;
input.merge$inspection_fail    <- NA;

for (this.row in 1:nrow(input.merge)) {

	ins.dates  <- as.vector(as.matrix(input.merge[this.row, 'inspection_date']));
	ins.status <- as.vector(as.matrix(input.merge[this.row, 'inspection_status']));

	ins.dates  <- unlist(strsplit(ins.dates, '; '));
	ins.status <- unlist(strsplit(ins.status, '; '));

	if (length(ins.dates) == length(ins.status)) {

		input.merge$inspection_times[this.row] <- length(ins.dates);

		input.merge$inspection_pass[this.row]    <- sum(ins.status == 'Pass', na.rm = TRUE);
		input.merge$inspection_conpass[this.row] <- sum(ins.status == 'Conditional Pass', na.rm = TRUE);
		input.merge$inspection_fail[this.row]    <- sum(ins.status == 'Closed', na.rm = TRUE);

		} else { stop('Check your input!'); }
	}

write.table(
	x = input.merge,
	file = paste0(Sys.Date(), '_dinesafe-database_cleaned.tsv'),
	sep = '\t',
	quote = FALSE,
	row.names = FALSE
	);

setwd('~/Downloads');
input.merge <- read.delim(file = '2017-11-04_dinesafe-database_cleaned.tsv', as.is = TRUE);
rownames(input.merge) <- input.merge$establishment_id;

set.seed(19);
train.index <- sample(x = 1:nrow(input.merge), size = floor(0.7 * nrow(input.merge)));
input.train <- input.merge[train.index,];
input.test  <- input.merge[!rownames(input.merge) %in% rownames(input.train),];

write.table(
	x = input.train,
	file = paste0(Sys.Date(), '_dinesafe-database_train.tsv'),
	sep = '\t',
	quote = FALSE,
	row.names = FALSE
	);

write.table(
	x = input.test,
	file = paste0(Sys.Date(), '_dinesafe-database_holdout.tsv'),
	sep = '\t',
	quote = FALSE,
	row.names = FALSE
	);
