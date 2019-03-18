#
# Update use in sandbox



# dest server
baseurl = "https://sandbox.psi-mis.org/"
base = substr(baseurl.dest, 9,27)
username <- keyringr::get_kc_account(base, type = 'internet')
password <- keyringr::decrypt_kc_pw(base, type = "internet")

source("/Users/isaiahnyabuto/Documents/PSI Workspace/functions.R")



# get user 
loginDHIS2(baseurl,username,password)

r <- GET(paste0(baseurl,"api/users/jkj1vjLLosx"))
user <- fromJSON(content(r,"text"))

# get psi orgUnit 
rg <- GET(paste0(baseurl,"api/users/NwwXXzvSAdK"))
user2 <- fromJSON(content(rg,"text"))

user$organisationUnits <- user2$organisationUnits

d <- POST(paste0(baseurl,"api/metadata?&importStrategy=UPDATE"),
         body = toJSON(list(users = list(user)), auto_unbox = T),
         content_type_json())
