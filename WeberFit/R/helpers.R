# General helper functions
#
# This contains general helper functions for dealing with seralised objects
#
#

unsquish<-function(object){
  `%>%` = tidyr::`%>%`
  object %>% base::charToRaw() %>% base::unserialize()
}

squish<-function(object){
  `%>%` = tidyr::`%>%`
  object %>% base::serialize(connection = NULL, ascii = T) %>% base::rawToChar()
}
