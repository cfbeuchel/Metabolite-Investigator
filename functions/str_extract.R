str_extract <- function (string, extract) 
{
  str.extract <- substring(string, regexpr(extract, string), 
                           regexpr(extract, string) + attr(regexpr(extract, string), 
                                                           "match.length") - 1)
  return(str.extract)
}