# List Valid Filter Values

Returns the set of valid values for filters used in \[nccs_read()\], or
— with \`labels = TRUE\` — a tibble of codes alongside their
human-readable definitions sourced from bundled BMF lookup tables. No
network access.

## Usage

``` r
nccs_catalog(
  field = c("ntee_subsector", "exempt_org_type", "state", "subsection_code",
    "foundation_code", "affiliation_code"),
  labels = FALSE
)
```

## Arguments

- field:

  One of \`"ntee_subsector"\`, \`"exempt_org_type"\`, \`"state"\`,
  \`"subsection_code"\`, \`"foundation_code"\`, or
  \`"affiliation_code"\`. The last three are decoder fields backed by
  the bundled BMF lookups; they are not currently accepted by
  \`nccs_read()\` but are useful for joining external data.

- labels:

  Logical. If \`FALSE\` (default), returns a character vector of valid
  codes — preserves the legacy behavior. If \`TRUE\`, returns a tibble
  with at least a \`code\` and \`description\` column, sourced from the
  internal \`.nccs_lookups\` bundle (see \`data-raw/build_lookups.R\`).
  For \`"state"\`, the tibble pairs USPS abbreviations with state names;
  for \`"exempt_org_type"\`, codes come from
  \`subsection_classification_code\`.

## Value

A character vector (when \`labels = FALSE\`) or a tibble (when \`labels
= TRUE\`).

## Examples

``` r
nccs_catalog("ntee_subsector")
#>  [1] "ART" "EDU" "ENV" "HEL" "HMS" "HOS" "IFA" "MMB" "PSB" "REL" "UNI" "UNU"
nccs_catalog("ntee_subsector", labels = TRUE)
#> # A tibble: 12 × 2
#>    code  description                   
#>    <chr> <chr>                         
#>  1 ART   Arts, Culture, and Humanities 
#>  2 EDU   Education                     
#>  3 ENV   Environment and Animals       
#>  4 HEL   Health                        
#>  5 HMS   Human Services                
#>  6 IFA   International, Foreign Affairs
#>  7 PSB   Public, Societal Benefit      
#>  8 REL   Religion Related              
#>  9 MMB   Mutual/Membership Benefit     
#> 10 UNU   Unknown, Unclassified         
#> 11 UNI   Universities                  
#> 12 HOS   Hospitals                     
nccs_catalog("state", labels = TRUE)
#> # A tibble: 56 × 2
#>    code  description
#>    <chr> <chr>      
#>  1 AL    Alabama    
#>  2 AK    Alaska     
#>  3 AZ    Arizona    
#>  4 AR    Arkansas   
#>  5 CA    California 
#>  6 CO    Colorado   
#>  7 CT    Connecticut
#>  8 DE    Delaware   
#>  9 FL    Florida    
#> 10 GA    Georgia    
#> # ℹ 46 more rows
nccs_catalog("foundation_code", labels = TRUE)
#> # A tibble: 21 × 2
#>    code  description                                                            
#>    <chr> <chr>                                                                  
#>  1 0     All organizations except 501(c)(3)                                     
#>  2 2     Private operating foundation exempt from payment of section 4940 taxes…
#>  3 3     Private operating foundation                                           
#>  4 4     Private non-operating foundation                                       
#>  5 6     Undocumented (not in IRS BMF Extract Information sheet)                
#>  6 7     Undocumented (not in IRS BMF Extract Information sheet)                
#>  7 9     Suspense (a specific type not identified)                              
#>  8 10    Church—IRC Section 170(b)(1)(A)(i)                                     
#>  9 11    School—IRC Section 170(b)(1)(A)(ii)                                    
#> 10 12    Hospital—IRC Section 170(b)(1)(A)(iii)                                 
#> # ℹ 11 more rows
```
