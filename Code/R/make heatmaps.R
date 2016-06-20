message('## CCSD: make heatmaps.R')
message('# This script can be used to produce heatmaps of concomitant medication use out of '
  'the tables produced by the SAS code of project CCSD.\n')

setwd('C:\\Users\\kuryfs\\Documents\\NLM\\Projects\\Medicare\\CCSD\\Output')
library(reshape2)
library(gplots)
library(colorRamps)

ATC_levels <- c(1,2,3,4,5)
cm_variables <- c('CM.Index', 'Beneficiaries')

pair_support <- 1000
line_support <- 0.2
lower_cap <- -1.5
upper_cap <- 1.5
fill_nas <- F
abbreviate_names <- F
remove_equal_names <- T
max_class_name_length <- 36
font_sizes <- c(0.5, 0.4, 0.25, 0.25, 0.25) # One for each ATC level

make.heatmap <- function(ATC_level, cm_variable) {
  # class_support <- 1000
  max_class_name_length <- 36
  if(ATC_level %in% c(2,3,4,5))
    max_class_name_length <- max_class_name_length + 12
  if(ATC_level %in% c(3,4))
    max_class_name_length <- max_class_name_length + 18

  cap_cm <- cm_variable == 'CM.Index'
  # classes_to_use <- c('codeine', 'buprenorphine', 'morphine', 'oxymorphone', 'oxycodone', 'tapentadol', 'hydrocodone',
  #  'hydromorphone')
  # class_filter_prefixes_a <- c('C07AB') # All prefixes must have same length
  # class_filter_prefixes_b <- c('B01AC') # All prefixes must have same length  
  # enforce_interfilter_heatmap <- T
  # codename <- paste(class_filter_prefixes_a, class_filter_prefixes_b, collapse = ' ')
  use_dendrograms <- T
  
  # Function to remove trailing space characters from a string
  rem_trailing_space <- function(str) gsub(' $', '', str, perl=T)
  
  # create table CM_ATC5 (A_ATC1 VARCHAR(1), A_ATC2 VARCHAR(2), A_ATC3 VARCHAR(1), A_ATC4 VARCHAR(1), A_ATC5 VARCHAR(2),
  # A_NAME VARCHAR(32),	B_ATC1 VARCHAR(1), B_ATC2 VARCHAR(2), B_ATC3 VARCHAR(1), B_ATC4 VARCHAR(1), B_ATC5 VARCHAR(2),
  # B_NAME VARCHAR(32),	Beneficiaries int unsigned, CMIndex double);
  
  make.chunks.by.size <- function(x, size)
    split(x,sort(rep(1:(trunc(length(x)/size)+1),size))[1:length(x)])
  
  ATC1 <- function(str) substr(str, 1, 1)
  ATC2 <- function(str) substr(str, 2, 3)
  ATC3 <- function(str) substr(str, 4, 4)
  ATC4 <- function(str) substr(str, 5, 5)
  ATC5 <- function(str) substr(str, 6, 7)
  
  
  create.ATC5.table <- function() dbNoTimeOutGetQuery('create table CM_ATC5
    (A_ATC1 VARCHAR(1), A_ATC2 VARCHAR(2), A_ATC3 VARCHAR(1), A_ATC4 VARCHAR(1), A_ATC5 VARCHAR(2), A_NAME VARCHAR(32),
    B_ATC1 VARCHAR(1), B_ATC2 VARCHAR(2), B_ATC3 VARCHAR(1), B_ATC4 VARCHAR(1), B_ATC5 VARCHAR(2), B_NAME VARCHAR(32),
    Beneficiaries int unsigned, CMIndex double);')
  
  create.ATC4.table <- function() dbNoTimeOutGetQuery('create table CM_ATC4
    (A_ATC1 VARCHAR(1), A_ATC2 VARCHAR(2), A_ATC3 VARCHAR(1), A_ATC4 VARCHAR(1), A_NAME VARCHAR(128),
    B_ATC1 VARCHAR(1), B_ATC2 VARCHAR(2), B_ATC3 VARCHAR(1), B_ATC4 VARCHAR(1), B_NAME VARCHAR(128),
    Beneficiaries int unsigned, CMIndex double);')
  
  create.ATC3.table <- function() dbNoTimeOutGetQuery('create table CM_ATC3
    (A_ATC1 VARCHAR(1), A_ATC2 VARCHAR(2), A_ATC3 VARCHAR(1), A_NAME VARCHAR(128),
    B_ATC1 VARCHAR(1), B_ATC2 VARCHAR(2), B_ATC3 VARCHAR(1), B_NAME VARCHAR(128),
    Beneficiaries int unsigned, CMIndex double);')
  
  create.ATC2.table <- function() dbNoTimeOutGetQuery('create table CM_ATC2
    (A_ATC1 VARCHAR(1), A_ATC2 VARCHAR(2), A_NAME VARCHAR(64),
    B_ATC1 VARCHAR(1), B_ATC2 VARCHAR(2), B_NAME VARCHAR(64),
    Beneficiaries int unsigned, CMIndex double);')
  
  create.ATC1.table <- function() dbNoTimeOutGetQuery('create table CM_ATC1
    (A_ATC1 VARCHAR(1), A_NAME VARCHAR(64),
    B_ATC1 VARCHAR(1), B_NAME VARCHAR(64),
    Beneficiaries int unsigned, CMIndex double);')
  
  
  make.ATC5.row <- function(cm_row) data.frame(A_ATC1=ATC1(cm_row$ATC.5.A), A_ATC2=ATC2(cm_row$ATC.5.A),
    A_ATC3=ATC3(cm_row$ATC.5.A), A_ATC4=ATC4(cm_row$ATC.5.A), A_ATC5=ATC5(cm_row$ATC.5.A), A_NAME=cm_row$ATC.5.A.Name,
  	B_ATC1=ATC1(cm_row$ATC.5.B), B_ATC2=ATC2(cm_row$ATC.5.B), B_ATC3=ATC3(cm_row$ATC.5.B), B_ATC4=ATC4(cm_row$ATC.5.B),
    B_ATC5=ATC5(cm_row$ATC.5.B), B_NAME=cm_row$ATC.5.B.Name, Beneficiaries=cm_row$Beneficiaries, CMIndex=cm_row$CM.Index)
  
  
  make.ATC4.row <- function(cm_row) data.frame(A_ATC1=ATC1(cm_row$ATC.4.A), A_ATC2=ATC2(cm_row$ATC.4.A),
    A_ATC3=ATC3(cm_row$ATC.4.A), A_ATC4=ATC4(cm_row$ATC.4.A), A_NAME=cm_row$ATC.4.A.Name, B_ATC1=ATC1(cm_row$ATC.4.B),
    B_ATC2=ATC2(cm_row$ATC.4.B), B_ATC3=ATC3(cm_row$ATC.4.B), B_ATC4=ATC4(cm_row$ATC.4.B), B_NAME=cm_row$ATC.4.B.Name,
    Beneficiaries=cm_row$Beneficiaries, CMIndex=cm_row$CM.Index)
  
  
  make.ATC3.row <- function(cm_row) data.frame(A_ATC1=ATC1(cm_row$ATC.3.A), A_ATC2=ATC2(cm_row$ATC.3.A),
    A_ATC3=ATC3(cm_row$ATC.3.A), A_NAME=cm_row$ATC.3.A.Name, B_ATC1=ATC1(cm_row$ATC.3.B), B_ATC2=ATC2(cm_row$ATC.3.B),
    B_ATC3=ATC3(cm_row$ATC.3.B), B_NAME=cm_row$ATC.3.B.Name, Beneficiaries=cm_row$Beneficiaries, CMIndex=cm_row$CM.Index)
  
  
  make.ATC2.row <- function(cm_row) data.frame(A_ATC1=ATC1(cm_row$ATC.2.A), A_ATC2=ATC2(cm_row$ATC.2.A),
    A_NAME=cm_row$ATC.2.A.Name,	B_ATC1=ATC1(cm_row$ATC.2.B), B_ATC2=ATC2(cm_row$ATC.2.B),
    B_NAME=cm_row$ATC.2.B.Name, Beneficiaries=cm_row$Beneficiaries, CMIndex=cm_row$CM.Index)
  
  
  make.ATC1.row <- function(cm_row) data.frame(A_ATC1=ATC1(cm_row$ATC.1.A), A_NAME=cm_row$ATC.1.A.Name,
    B_ATC1=ATC1(cm_row$ATC.1.B), B_NAME=cm_row$ATC.1.B.Name, Beneficiaries=cm_row$Beneficiaries, CMIndex=cm_row$CM.Index)
  
  
  batchInsertATC5Rows <- function(cm_rows, table_name) {
    dbNoTimeOutGetQuery(paste0('insert into ', table_name,
    " (A_ATC1, A_ATC2, A_ATC3, A_ATC4, A_ATC5, A_NAME,
  	B_ATC1, B_ATC2, B_ATC3, B_ATC4, B_ATC5, B_NAME,
    Beneficiaries, CMIndex) values ('",
      paste(apply(make.ATC5.row(cm_rows), 1, paste, collapse="', '"), collapse="'), ('"), "');"))
  }
  
  
  batchInsertATC4Rows <- function(cm_rows, table_name) {
    dbNoTimeOutGetQuery(paste0('insert into ', table_name,
    " (A_ATC1, A_ATC2, A_ATC3, A_ATC4, A_NAME,
  	B_ATC1, B_ATC2, B_ATC3, B_ATC4, B_NAME,
    Beneficiaries, CMIndex) values ('",
      paste(apply(make.ATC4.row(cm_rows), 1, paste, collapse="', '"), collapse="'), ('"), "');"))
  }
  
  
  batchInsertATC3Rows <- function(cm_rows, table_name) {
    dbNoTimeOutGetQuery(paste0('insert into ', table_name,
    " (A_ATC1, A_ATC2, A_ATC3, A_NAME,
  	B_ATC1, B_ATC2, B_ATC3, B_NAME,
    Beneficiaries, CMIndex) values ('",
      paste(apply(make.ATC3.row(cm_rows), 1, paste, collapse="', '"), collapse="'), ('"), "');"))
  }
  
  
  batchInsertATC2Rows <- function(cm_rows, table_name) {
    dbNoTimeOutGetQuery(paste0('insert into ', table_name,
    " (A_ATC1, A_ATC2, A_NAME,
  	B_ATC1, B_ATC2, B_NAME,
    Beneficiaries, CMIndex) values ('",
      paste(apply(make.ATC2.row(cm_rows), 1, paste, collapse="', '"), collapse="'), ('"), "');"))
  }
  
  
  batchInsertATC1Rows <- function(cm_rows, table_name) {
    dbNoTimeOutGetQuery(paste0('insert into ', table_name,
    " (A_ATC1, A_NAME,
  	B_ATC1, B_NAME,
    Beneficiaries, CMIndex) values ('",
      paste(apply(make.ATC1.row(cm_rows), 1, paste, collapse="', '"), collapse="'), ('"), "');"))
  }
  
  
  fp <- read.csv(paste0('15-12-14 CCSD Frequency of prescription 2009 - ATC-', ATC_level, '.csv'), sep=',')
  fp <- subset(fp, Age.group == 'All 65+')[,c(-1)]
  if(exists('class_support'))
    fp <- subset(fp, Beneficiaries >= class_support)
  
  cm <- read.csv(paste0('15-12-14 CCSD Concomitant medication 2009 - ATC-', ATC_level, '.csv'),
    sep=',', stringsAsFactors = F)[,c(1,2,3,4,5,6,17)]
  
  cm <- subset(cm, Age.group == 'All 65+')[,c(-1)]
  
  cm <- subset(cm, (eval(parse(text=paste0('ATC.', ATC_level, '.A'))) %in%
    eval(parse(text=paste0('fp$ATC.', ATC_level, '.code'))))  & (eval(parse(text=paste0('ATC.', ATC_level, '.B'))) %in%
    eval(parse(text=paste0('fp$ATC.', ATC_level, '.code')))))
  
  if(remove_equal_names)
    cm <- subset(cm, eval(parse(text=paste0('ATC.', ATC_level, '.A.Name')))
      != eval(parse(text=paste0('ATC.', ATC_level, '.B.Name'))))
  
  if(exists('classes_to_use'))
    cm <- subset(cm, tolower(ATC.5.A.Name) %in% tolower(classes_to_use)
      & tolower(ATC.5.B.Name) %in% tolower(classes_to_use))
  
  if(exists('class_filter_prefixes_a') && exists('class_filter_prefixes_b')) {
    cm <- subset(cm,
      ((substr(eval(parse(text=paste0('ATC.', ATC_level, '.A'))), 1, nchar(class_filter_prefixes_a[[1]]))
      %in% class_filter_prefixes_a) & (substr(eval(parse(text=paste0('ATC.', ATC_level, '.B'))), 1,
      nchar(class_filter_prefixes_b[[1]])) %in% class_filter_prefixes_b)) |
      ((substr(eval(parse(text=paste0('ATC.', ATC_level, '.B'))), 1, nchar(class_filter_prefixes_a[[1]]))
      %in% class_filter_prefixes_a) & (substr(eval(parse(text=paste0('ATC.', ATC_level, '.A'))), 1,
      nchar(class_filter_prefixes_b[[1]])) %in% class_filter_prefixes_b)))
  }
  
  cm[, 1] <- paste0('[', cm[,1], '] ', cm[,2])
  cm[, 1] <- ifelse(nchar(cm[, 1])>max_class_name_length,
    paste0(rem_trailing_space(substr(cm[, 1], 1, max_class_name_length)), '...'), cm[, 1])
  
  cm[, 3] <- paste0('[', cm[,3], '] ', cm[,4])
  cm[, 3] <- ifelse(nchar(cm[, 3])>max_class_name_length,
    paste0(rem_trailing_space(substr(cm[, 3], 1, max_class_name_length)), '...'), cm[, 3])
  
  cm <- cm[, c(-2, -4)]
  
  if(exists('pair_support'))
    cm <- subset(cm, Beneficiaries >= pair_support)
  
  cm[cm_variable] <- log10(cm[cm_variable])
  
  if(cap_cm) {
    cm[cm[cm_variable] < lower_cap, cm_variable] <- lower_cap
    cm[cm[cm_variable] > upper_cap, cm_variable] <- upper_cap
  }
  
  cm <- dcast(cm, eval(parse(text=paste0('ATC.', ATC_level, '.A'))) ~ eval(parse(text=paste0('ATC.', ATC_level, '.B'))),
    value.var = cm_variable)[,-1]
  
  if(exists('line_support'))
    cm = cm[rowSums(!is.na(cm))>=ceiling(line_support*nrow(cm)), colSums(!is.na(cm))>=ceiling(line_support*ncol(cm))]
  
  if(fill_nas)
    cm[is.na(cm)] <- min(cm, na.rm=T)
  
  if(abbreviate_names) {
    class.names <- names(cm)
    for(i in 1:length(class.names))
      if(nchar(class.names[i]) > 24)
        class.names[i] <- paste0(substr(class.names[i], 1, 24), '...')
    names(cm) <- class.names
  }
  
  do.heatmap <- function() {
    heatmap.2(as.matrix(cm), Rowv = use_dendrograms, Colv = use_dendrograms, colsep=F, rowsep=F, sepwidth=c(0,0),
    cexRow = font_sizes[[ATC_level]], cexCol = font_sizes[[ATC_level]], margins = c(12, 12), trace='none',
    main=heatmap_title, srtCol = 90, density.info = 'histogram', denscol = 'black', col = if(cm_variable == 'CM.Index')
    colorRampPalette(blue2red(1000))(n = 1000) else colorRampPalette(c('red', 'yellow', 'blue', 'green'))(n = 1000))
  }
  
  row.names(cm) <- names(cm)
  
  if(exists('enforce_interfilter_heatmap') && enforce_interfilter_heatmap)
    cm <- cm[substr(colnames(cm), 2, 1+nchar(class_filter_prefixes_a[[1]])) %in% class_filter_prefixes_a,
      substr(colnames(cm), 2, 1+nchar(class_filter_prefixes_b[[1]])) %in% class_filter_prefixes_b]
  
  heatmap_title <- paste0('ATC-', ATC_level, ', ', cm_variable, ifelse(exists('class_support'), paste0(', cs=',
    class_support), ''), ifelse(exists('pair_support'), paste0(', ps=', pair_support), ''), ifelse(exists('line_support'),
    paste0(', ls=', line_support), ''), ifelse(exists('codename'), paste0(' ', codename), ''))
  
  pdf(file = paste0(heatmap_title, '.pdf'))
  par(cex.main = 0.6)
  # The try() is needed so we get to close and release the PDF file in case the heatmap function fails.
  res <- try(do.heatmap())
  if(inherits(res, 'try-error')) {
    use_dendrograms <<- F
    try(do.heatmap())
  }
  dev.off()
}

lapply(ATC_levels, make.heatmap, cm_variable = cm_variables[[1]])
lapply(ATC_levels, make.heatmap, cm_variable = cm_variables[[2]])