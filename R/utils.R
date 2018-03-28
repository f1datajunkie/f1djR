#Note that Verstappen changed code from VES to VER during 2017

driverCodeMapCodes=c("hamilton"= "HAM", "vettel"= "VET", "rosberg"= "ROS", "ricciardo"= "RIC",
              "kvyat"= "KVY", "max_verstappen"= "VER", "massa" = "MAS", "grosjean"= "GRO",
              "bottas"= "BOT", "ericsson"= "ERI", "raikkonen"= "RAI", "maldonado" = "MAL",
              "hulkenberg"= "HUL", "perez"= "PER", "sainz"= "SAI", "nasr"= "NAS",
              "button" = "BUT", "alonso"= "ALO", "merhi"= "MER", "stevens"="STE",
              "gutierrez" = "GUT","wehrlein" = "WEH","jolyon_palmer" = "PAL",
              "haryanto" = "HAR","kevin_magnussen"="MAG","ocon" ="OCO","stroll"="STR",
              "giovinazzi"="GIO","vandoorne" = "VAN","kobayashi"="KOB", "chilton"="CHI",
              "jules_bianchi"="BIA", "resta"="DIR", "gasly"="GAS", "leclerc"="LEC",
              "sirotkin"= "SIR", "brendon_hartley"="HAR"
)

driverCodeMap=function(name) unname(driverCodeMapCodes[name])

#Mapping from driver names as they appear in FIA press releases
## to three letter driver codes
driverCodes=c("L. HAMILTON"= "HAM", "S. VETTEL"= "VET", "N. ROSBERG"= "ROS", "D. RICCIARDO"= "RIC",
              "D. KVYAT"= "KVY", "M. VERSTAPPEN"= "VES", "F. MASSA" = "MAS", "R. GROSJEAN"= "GRO",
              "V. BOTTAS"= "BOT", "M. ERICSSON"= "ERI", "K. RAIKKONEN"= "RAI", "P. MALDONADO" = "MAL",
              "N. HULKENBERG"= "HUL", "S. PEREZ"= "PER", "C. SAINZ"= "SAI", "F. NASR"= "NAS",
              "J. BUTTON" = "BUT", "F. ALONSO"= "ALO", "R. MERHI"= "MER", "W. STEVENS"="STE",
              "R. HARYANTO"="HAR","P. WEHRLEIN"="WEH","J. PALMER"="PAL","K. MAGNUSSEN"="MAG",
              "E. GUTIERREZ"= "GUT", "S. VANDOORNE"= "VAN", "E. OCON"= "OCO", "M. CHILTON"= "CHI",
              "K. KOBAYASHI"= "KOB", "A. LOTTERER"= "LOT", "A. ROSSI"= "RSI", "L. STROLL" = "STR",
              "J. VERGNE"= "VER",  'A. GIOVINAZZI'='GIO',"J. BIANCHI"="BIA", "P. DI RESTA"="DIR",
              "P. GASLY"="GAS",
              "Stoffel VANDOORNE"='VAN', "Daniel RICCIARDO"='RIC', "Sebastian VETTEL"='VET',
              "Kimi RAIKKONEN"='RAI', "Romain GROSJEAN"='GRO', "Marcus ERICSSON"='ERI',
              "Pierre GASLY"='GAS', "Sergio PEREZ"='PER', "Fernando ALONSO"='ALO',
              "Charles LECLERC"='LEC', "Lance STROLL"='STR', "Kevin MAGNUSSEN"='MAG',
              "Nico HULKENBERG"='HUL', "Brendon HARTLEY"='HAR', "Esteban OCON"='OCO',
              "Max VERSTAPPEN"='VER', "Sergey SIROTKIN"='SIR', "Lewis HAMILTON"='HAM',
              "Carlos SAINZ"='SAI', "Valtteri BOTTAS"='BOT' )
driverCode=function(name) unname(driverCodes[name])
driverCodeErgast=function(name) unname(driverCodeMapCodes[name])
