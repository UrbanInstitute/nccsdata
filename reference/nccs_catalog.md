# List Valid Filter Values

Returns the set of valid values for filters used in \[nccs_read()\]. No
network access is needed.

## Usage

``` r
nccs_catalog(field = c("ntee_subsector", "exempt_org_type", "state"))
```

## Arguments

- field:

  One of \`"ntee_subsector"\`, \`"exempt_org_type"\`, or \`"state"\`.

## Value

A character vector of valid values for the specified field.

## Examples

``` r
nccs_catalog("ntee_subsector")
#>  [1] "ART" "EDU" "ENV" "HEL" "HMS" "HOS" "IFA" "MMB" "PSB" "REL" "UNI" "UNU"
nccs_catalog("state")
#>  [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "FL" "GA" "HI" "ID" "IL" "IN" "IA"
#> [16] "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH" "NJ"
#> [31] "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VT"
#> [46] "VA" "WA" "WV" "WI" "WY" "DC" "PR" "GU" "VI" "AS" "MP"
nccs_catalog("exempt_org_type")
#>  [1] "501(c)(1) - Corporations Organized Under Act of Congress"                         
#>  [2] "501(c)(2) - Title Holding Corporation For Exempt Organization"                    
#>  [3] "501(c)(3) - Religious, Educational, Charitable, etc."                             
#>  [4] "501(c)(4) - Civic Leagues, Social Welfare Organizations"                          
#>  [5] "501(c)(5) - Labor, Agricultural, Horticultural Organizations"                     
#>  [6] "501(c)(6) - Business Leagues, Chambers of Commerce"                               
#>  [7] "501(c)(7) - Social and Recreational Clubs"                                        
#>  [8] "501(c)(8) - Fraternal Beneficiary Societies and Associations"                     
#>  [9] "501(c)(9) - Voluntary Employee Beneficiary Associations"                          
#> [10] "501(c)(10) - Domestic Fraternal Societies and Associations"                       
#> [11] "501(c)(11) - Teachers Retirement Fund Associations"                               
#> [12] "501(c)(12) - Benevolent Life Insurance Associations"                              
#> [13] "501(c)(13) - Cemetery Companies"                                                  
#> [14] "501(c)(14) - State-Chartered Credit Unions"                                       
#> [15] "501(c)(15) - Mutual Insurance Companies or Associations"                          
#> [16] "501(c)(16) - Cooperative Organizations to Finance Crop Operations"                
#> [17] "501(c)(17) - Supplemental Unemployment Benefit Trusts"                            
#> [18] "501(c)(18) - Employee Funded Pension Trusts"                                      
#> [19] "501(c)(19) - Post or Organization of Past or Present Members of the Armed Forces" 
#> [20] "501(c)(20) - Group Legal Services Plan Organizations"                             
#> [21] "501(c)(21) - Black Lung Benefit Trusts"                                           
#> [22] "501(c)(22) - Withdrawal Liability Payment Fund"                                   
#> [23] "501(c)(23) - Veterans Organization"                                               
#> [24] "501(c)(24) - Section 4049 ERISA Trusts"                                           
#> [25] "501(c)(25) - Title Holding Corporations or Trusts"                                
#> [26] "501(c)(26) - State-Sponsored High Risk Health Coverage Organizations"             
#> [27] "501(c)(27) - State-Sponsored Workers Compensation Reinsurance Organizations"      
#> [28] "501(c)(28) - National Railroad Retirement Investment Trust"                       
#> [29] "501(c)(29) - CO-OP Health Insurance Issuers"                                      
#> [30] "501(c)(40) - Apostolic and Religious Organizations"                               
#> [31] "501(d) - Religious and Apostolic Organizations"                                   
#> [32] "501(e) - Cooperative Hospital Service Organizations"                              
#> [33] "501(f) - Cooperative Service Organizations of Operating Educational Organizations"
```
