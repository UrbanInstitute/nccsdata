



x <- read.csv( "cbsa2fipsxw.csv", colClasses="character" )

new.order <- 
  c( "statename", "countycountyequivalent",
     "fipsstatecode", "fipscountycode",
     "centraloutlyingcounty",
     "cbsacode", "cbsatitle", 
     "metropolitanmicropolitanstatis",
     "csacode", "csatitle",  
     "metropolitandivisioncode", "metropolitandivisiontitle" )


x <- x[ new.order ]

st <- 
structure(list(state = c("Alabama", "Alaska", "Arizona", "Arkansas", 
"California", "Colorado", "Connecticut", "Delaware", "District of Columbia", 
"Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
"Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
"Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
"Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
"New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
"Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", 
"South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
"Vermont", "Virginia", "Virgin Islands", "Washington", "West Virginia", 
"Wisconsin", "Wyoming"), st = c("AL", "AK", "AZ", "AR", "CA", 
"CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", 
"KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
"NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", 
"PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "VI", 
"WA", "WV", "WI", "WY")), row.names = c(NA, -53L), class = "data.frame")


x <- merge( x, st, by.x="statename", by.y="state", all.x=T )


# c("statename", "countycountyequivalent", "fipsstatecode", "fipscountycode", 
# "centraloutlyingcounty", "cbsacode", "cbsatitle", "metropolitanmicropolitanstatis", 
# "csacode", "csatitle", "metropolitandivisioncode", "metropolitandivisiontitle", 
# "st")

nm <- 
c("state", "county", "sfips", "cfips", 
"proximity", "cbsa", "cbsatitle", "metromicro", 
"csa", "csatitle", "mdivcode", "mdivtitle", 
"st")

names(x) <- nm


x$fips <- paste0( x$sfips, x$cfips )

x$proximity <- tolower( x$proximity )
x$countylabel <- paste0( x$county, ", ", x$st, " (", x$proximity, "): ", x$fips )

x$cbsatype <- tolower( substr( x$metromicro, 1, 5 ) )

x$cbsalabel <- paste0( x$cbsatitle, " (", x$cbsatype, "): ", x$cbsa )

x$csalabel <- paste0( x$csatitle, ": ", x$csa )
x$csalabel[ x$csalabel == ": " ] <- ""


x <- 
  x %>% 
  group_by( cbsa ) %>%
  mutate( cbsafipslabel=paste0( "c('", paste0(fips,collapse="','"), "')" ) ) %>%
  ungroup() %>%
  group_by( csa ) %>%
  mutate( csafipslabel=paste0( "c('", paste0(fips,collapse="','"), "')" ), ) %>%
  ungroup() %>%
  as.data.frame()

x$csafipslabel[ x$csa == "" ] <- ""


keep <- 
c("st", "state", "county", "fips", "proximity",  "countylabel",  
  "cbsa", "cbsatitle", "cbsatype", "cbsalabel", "cbsafipslabel",
  "csa", "csatitle", "csalabel", "csafipslabel")

x <- x[keep]


nm <- 
c("state", "state", "county", "fips", "countytype",  "countylabel", 
"cbsa", "cbsatitle", "cbsatype", "cbsalabel", "cbsafipslabel", 
"csa", "csatitle", "csalabel", "csafipslabel")

names(x) <- nm






metrofips <- x

write.csv( x, "metrofips.csv", row.names=F )


preview <- function( geo, within=NULL, type=NULL ) {

  if( ! is.null(within) )
  { metrofips <- dplyr::filter( metrofips, state %in% within ) }

  if( ! is.null(type) )
  { metrofips <- dplyr::filter( metrofips, cbsatype == type ) }

  geolabel <- paste0( geo, "label" )
  metrofips <- metrofips[ geolabel ]

  metrofips[ metrofips == "" ] <- NA
  metrofips <- na.omit(metrofips) %>% unique()

  metrofips <- dplyr::arrange( metrofips, !!!rev(geolabel) )

  print( metrofips %>% knitr::kable( align="r" ) )
  invisible( metrofips )
}




preview( geo="cbsa" )
preview( geo="cbsa", within="LA" )
preview( geo="cbsa", type="micro" )


preview( geo="cbsa", within=c("CA","NY") )

preview( geo=c("county","cbsa"), within=c("CA","NY"), type="metro" )

xx <- preview( geo=c("cbsa","cbsafips"), within="FL", type="metro" )
xx


preview( geo="csa", within="TX" )
preview( geo=c("csa","csafips"), within="TX" )


