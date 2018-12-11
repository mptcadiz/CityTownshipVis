library(tidyverse)


# Prep city and township data 1900 - 2000 ---------------------------------

pop.countysub.1900_2000 <- read_csv("countyPlaces_1900_2000.csv") %>%
  rename(Id2 = cousub) %>%
  distinct(Id2, .keep_all = TRUE) %>%
  filter(placeName != "Kabetogema (Unorganized)") %>%
  mutate(area = as.factor(area),
         #placeName = str_replace(placeName, " \\(City\\)", ""),
         #placeName = str_replace(placeName, " \\(Township\\)", ""),
         #placeName = str_replace(placeName, " Unorganized", ""),
         placeName = str_replace(placeName,"Unorganized","(Unorganized)"),
         Id2 = formatC(Id2, width = 5, flag = "0"),
         Id2 = str_replace(Id2, "64084", "64048"),
         Id2 = str_replace(Id2, "35621", "35648"),
         Id2 = str_replace(Id2, "33380", "33416"),
         Id2 = str_replace(Id2, "04240", "04204"),
         Id2 = str_replace(Id2, "54070", "54060"),
         Id2 = str_replace(Id2, "34208", "34207"),
         Id2 = str_replace(Id2, "56752", "56724"),
         placeName = ifelse(placeName == "Kabetogema" & Id2 == "32252", "Kabetogama", placeName),
         placeName = ifelse(placeName == "St. Anthony" & Id2 == "56680", "St. Anthony (Hennepin and Ramsey Counties)", placeName),
         placeName = ifelse(placeName == "St. Anthony" & Id2 == "56698", "St. Anthony (Stearns County)", placeName),
         placeName = ifelse(placeName == "Rush" & countyName == "Chisago", "Rush City", placeName),
         placeName = ifelse(placeName == "Pine" & countyName == "Pine", "Pine City", placeName),
         placeName = ifelse(placeName == "Minnesota" & countyName == "Winona", "Minnesota City", placeName),
         placeName = ifelse(placeName == "Lake" & countyName == "Wabasha", "Lake City", placeName),
         placeName = ifelse(placeName == "Hill (City)" & countyName == "Aitkin", "Hill City (City)", placeName),
         placeName = ifelse(placeName == "Cannon (City) (Township)" & Id2 == "09712", "Cannon City (Township)", placeName),
         placeName = ifelse(placeName == "Blue Earth (City) (Township)" & Id2 == "06706", "Blue Earth City (Township)", placeName),
         placeName = ifelse(placeName == "Forest (City) (Township)" & Id2 == "021734", "Forest City (Township)", placeName),
         placeName = ifelse(placeName == "Garden (City) (Township)" & Id2 == "23102", "Garden City (Township)", placeName),
         placeName = ifelse(placeName == "Holmes (City) (Township)" & Id2 == "29798", "Holmes City (Township)", placeName),
         placeName = ifelse(placeName == "Pine (City) (Township)" & Id2 == "51082", "Pine City (Township)", placeName),
         placeName = ifelse(placeName == "Winnebago (City) (Township)" & Id2 == "070978", "Winnebago City (Township)", placeName),
         placeName = ifelse(placeName == "Liberty (Township)" & Id2 == "36944", "Liberty (Unorganized)", placeName),
         area = replace(area, Id2 =="36944" ,"Unorganized"),
         placeName = ifelse(placeName == "Rice Lake (Township)" & Id2 == "54060", "Rice Lake (City)", placeName),
         area = replace(area, Id2 =="54060" ,"City"),
         placeName = ifelse(placeName == "Davidson Unorganized" & Id2 == "14903", "Davidson (Unorganized)", placeName),
         placeName = ifelse(placeName == "McCormack Lake (Unorganized)" & Id2 == "38938", "McCormack (Unorganized)", placeName),
         area = replace(area, Id2 =="04027" ,"Township"),
         placeName = ifelse(placeName == "Baudette (Unorganized)" & Id2 == "04027", "Baudette (Township)", placeName),
         area = replace(area, Id2 =="06966" ,"Township"),
         placeName = ifelse(placeName == "Boone (Unorganized)" & Id2 == "06966", "Boone (Township)", placeName),
         placeName = ifelse(placeName == "Center (City)" & Id2 == "10576", "Center City (City)", placeName),
         area = replace(area, Id2 =="11322" ,"Township"),
         placeName = ifelse(placeName == "Chilgren (Unorganized)" & Id2 == "11322", "Chilgren (Township)", placeName),
         placeName = ifelse(placeName == "Chisago (City)" & Id2 == "11350", "Chisago City (City)", placeName),
         placeName = ifelse(placeName == "Clara (City)" & Id2 == "11548", "Clara City (City)", placeName),
         placeName = ifelse(placeName == "Clover Leaf (Township)" & Id2 == "12304", "Cloverleaf (Township)", placeName),
         area = replace(area, Id2 =="01550" ,"Township"),
         placeName = ifelse(placeName == "Angle (Unorganized)" & Id2 == "01550", "Angle (Township)", placeName),
         area = replace(area, Id2 =="12718" ,"City"),
         placeName = ifelse(placeName == "Columbus (Township)" & Id2 == "12718", "Columbus (City)", placeName),
         area = replace(area, Id2 =="21690" ,"Township"),
         placeName = ifelse(placeName == "Forest Area (Unorganized)" & Id2 == "21690", "Forest Area (Township)", placeName),
         placeName = ifelse(placeName == "Forest (City) (Township)" & Id2 == "21734", "Forest City Township (Township)", placeName),
         placeName = ifelse(placeName == "Grove (City)" & Id2 == "26126", "Grove City (City)", placeName),
         area = replace(area, Id2 =="26250" ,"Township"),
         placeName = ifelse(placeName == "Gudrid (Unorganized)" & Id2 == "26250", "Gudrid (Township)", placeName),
         area = replace(area, Id2 =="28682" ,"City"),
         placeName = ifelse(placeName == "Hermantown (Township)" & Id2 == "28682", "Hermantown (City)", placeName),
         area = replace(area, Id2 =="33048" ,"Township"),
         placeName = ifelse(placeName == "Kiel (Unorganized)" & Id2 == "33048", "Kiel (Township)", placeName),
         area = replace(area, Id2 =="35208" ,"Township"),
         placeName = ifelse(placeName == "Lakewood (Unorganized)" & Id2 == "35208", "Lakewood (Township)", placeName),
         placeName = ifelse(placeName == "Lake Edwards (Township)" & Id2 == "34207", "Lake Edward (Township)", placeName),
         placeName = ifelse(placeName == "Hassan (Township)" & Id2 == "27476", "Hassan Township (Township)", placeName),
         placeName = ifelse(placeName == "Iron Range (Township)" & Id2 == "31256", "Iron Range Township (Township)", placeName),
         area = replace(area, Id2 =="64426" ,"Unorganized"),
         placeName = ifelse(placeName == "Tenney (City)" & Id2 == "64426", "Tenney (Unorganized)", placeName),
         placeName = ifelse(placeName == "Chaska (Township)" & Id2 == "10990", "Chaska Township (Township)", placeName),
         placeName = ifelse(placeName == "Dean Lake (Township)" & Id2 == "15094", "Dean Lake (Unorganized)", placeName),
         placeName = ifelse(placeName == "Forest Lake (Township)" & Id2 == "21788", "Forest Lake Township (Township)", placeName),
         placeName = ifelse(placeName == "Grand Rapids (Township)" & Id2 == "25136", "Grand Rapids Township (Township)", placeName),
         pop2000 = replace(pop2000, Id2 =="01565" ,"0"),
         pop2000 = replace(pop2000, Id2 =="04208" ,"5"),
         pop2000 = replace(pop2000, Id2 =="14778" ,"68"),
         pop2000 = replace(pop2000, Id2 =="30616" ,"3"),
         pop2000 = replace(pop2000, Id2 =="13626" ,"11"),
         pop2000 = replace(pop2000, Id2 =="09486" ,"16"),
         area = replace(area, Id2 =="08740" ,"City"),
         placeName = replace(placeName, Id2 =="08740" ,"Nowthen (City)"),
         Id2 = str_replace(Id2, "08740", "47536"),
         
         
         
         #Sudi's section
         placeName = ifelse(placeName == "McDougald (Unorganized)" & Id2 == "38988", "McDougald (Township)", placeName),
         placeName = ifelse(placeName == "McKinley UT" & Id2 == "39112", "McKinley (Unorganized)", placeName),
         placeName = ifelse(placeName == "Myhre (Unorganized)" & Id2 == "44887", "Myhre (Township)", placeName),
         placeName = ifelse(placeName == "North Red River (Township)" & Id2 == "47176", "North Red River (Unorganized)", placeName),
         placeName = ifelse(placeName == "Potamo (Unorganized)" & Id2 == "52208", "Potamo (Township)", placeName),
         placeName = ifelse(placeName == "Prosper (Unorganized)" & Id2 == "52672", "Prosper (Township)", placeName),
         placeName = ifelse(placeName == "Rapid River (Unorganized)" & Id2 == "53212", "Rapid River (Township)", placeName),
         placeName = ifelse(placeName == "Rulien (Unorganized)" & Id2 == "56238", "Rulien (Township)", placeName),
         placeName = ifelse(placeName == "Spooner (Unorganized)" & Id2 == "61726", "Spooner (Township)", placeName),
         placeName = ifelse(placeName == "St. Augusta (Township)" & Id2 == "56724", "St. Augusta (City)", placeName),
         area = replace(area, Id2 =="56724" ,"City"),
         placeName = ifelse(placeName == "Swiftwater (Unorganized)" & Id2 == "63945", "Swiftwater (Township)", placeName),
         placeName = ifelse(placeName == "Victory (Unorganized)" & Id2 == "67044", "Victory (Township)", placeName),
         placeName = ifelse(placeName == "Wabanica (Unorganized)" & Id2 == "67368", "Wabanica (Township)", placeName),
         placeName = ifelse(placeName == "Walhalla (Unorganized)" & Id2 == "67782", "Walhalla (Township)", placeName),
         placeName = ifelse(placeName == "Wheeler (Unorganized)" & Id2 == "69849", "Wheeler (Township)", placeName),
         placeName = ifelse(placeName == "Zippel (Unorganized)" & Id2 == "72264", "Zippel (Township)", placeName),
         placeName = ifelse(placeName == "Minnesota (City)" & Id2 == "43144", "Minnesota City (City)", placeName),
         placeName = ifelse(placeName == "Rush (City)" & Id2 == "56266", "Rush City (City)", placeName),
         placeName = ifelse(placeName == "Pine (City)" & Id2 == "51064", "Pine City (City)", placeName),
         placeName = ifelse(placeName == "Winnebago (City) (Township)" & Id2 == "70978", "Winnebago City (Township)", placeName),
         placeName = ifelse(placeName == "(Township) 157-30 ((Unorganized))" & Id2 == "65298", "Township 157-30", placeName),
         placeName = ifelse(placeName == "(Township) 158-30 ((Unorganized))" & Id2 == "65300", "Township 158-30", placeName),
         placeName = ifelse(placeName == "Kabetogema (Unorganized)" & Id2 == "32252", "Kabetogama (Township)", placeName),
         placeName = ifelse(placeName == "Kiel (Unorganized)" & Id2 == "33048", "Kiel (Township)", placeName),
         placeName = ifelse(placeName == "Lake (City)" & Id2 == "34172", "Lake City (City)", placeName),
         placeName = ifelse(placeName == "Lakewood (Unorganized)" & Id2 == "35208", "Lakewood (Township)", placeName),
         placeName = ifelse(placeName == "New Scandia (Township)" & Id2 == "45952", "New Scandia (City)", placeName)
  ) %>%
  #Sudi's code
  mutate(
    area = replace(area, Id2 =="38988" ,"Township"),
    area = replace(area, Id2 =="39112" ,"Unorganized"),
    area = replace(area, Id2 =="44887" ,"Township"),
    area = replace(area, Id2 =="47176" ,"Unorganized"),
    area = replace(area, Id2 =="52208" ,"Township"),
    area = replace(area, Id2 =="52672" ,"Township"),
    area = replace(area, Id2 =="53212" ,"Township"),
    area = replace(area, Id2 =="56238" ,"Township"),
    area = replace(area, Id2 =="61726" ,"Township"),
    area = replace(area, Id2 =="56752" ,"City"),
    area = replace(area, Id2 =="63945" ,"Township"),
    area = replace(area, Id2 =="67044" ,"Township"),
    area = replace(area, Id2 =="67368" ,"Township"),
    area = replace(area, Id2 =="67782" ,"Township"),
    area = replace(area, Id2 =="69849" ,"Township"),
    area = replace(area, Id2 =="72264" ,"Township"),
    area = replace(area, Id2 =="32252" ,"Township"),
    area = replace(area, Id2 =="45952" ,"City")
  )%>%
  
  rbind(c("13656", "Crane Lake (Township)", "137", "St Louis", "Township", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA, "13656" )) %>%
  rbind(c("21149", "First Assessment (Unorganized)", "35", "Crow Wing", "Unorganized", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA, "21149" )) %>%
  rbind(c("33495", "Klondike (Unorganized)", "69", "Kittson", "Unorganized", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA, "33495" )) %>%
  rbind(c("38042", "Long Lost Lake (Township)", "29", "Clearwater", "Township", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA, "38042" )) %>%
  rbind(c("18662", "Elko New Market (City)", "139", "Scott", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, "18662" )) %>%
  rbind(c("39538","Makinen (Unorganized)","137","St Louis","Unorganized",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("50062","Peatland (Unorganized)","69","Kittson","Unorganized",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  #need to add populations for Scandia
  rbind(c("58900","Scandia (City)","163","Washington","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("59161","Second Assessment (Unorganized)","35","Crow Wing","Unorganized",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("61330","Southeast Roseau (Unorganized)","135","Roseau","Unorganized",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,229,NA,NA,NA,NA)) %>%
  rename(Id2short = Id2) %>%
  mutate(Id2short = as.character(Id2short)) %>%
  
  #for adding places with multiple counties - Mika's code
  
  rbind(
    c("04798a", "Bellechester (City)", "49", "Goodhue", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("04798b", "Bellechester (City)", "157", "Wabasha", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("06382a", "Blaine (City)", "3", "Anoka", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("06382b", "Blaine (City)", "123", "Ramsey", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("06580a", "Blooming Prairie (City)", "39", "Dodge", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("06580b", "Blooming Prairie (City)", "147", "Steele", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("07282a", "Braham (City)", "59", "Isanti", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("07282b", "Braham (City)", "65", "Kanabec", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("08092a", "Brooten (City)", "121", "Pope", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("08092b", "Brooten (City)", "145", "Stearns", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("10918a", "Chanhassen (City)", "19", "Carver", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("10918b", "Chanhassen (City)", "53", "Hennepin", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("11008b", "Chatfield (City)", "109", "Olmsted", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("11008a", "Chatfield (City)", "45", "Filmore", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("11800b", "Clearwater (City)", "171", "Wright", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("11800a", "Clearwater (City)", "145", "Stearns", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("12772a", "Comfrey (City)", "151", "Swift", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("12772b", "Comfrey (City)", "33", "Cottonwood", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("15022a", "Dayton (City)", "53", "Hennepin", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("15022b", "Dayton (City)", "171", "Wright", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("15706a", "Dennison (City)", "49", "Goodhue", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("15706b", "Dennison (City)", "131", "Rice", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("18134a", "Eden Valley (City)", "93", "Meeker", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("18134b", "Eden Valley (City)", "145", "Stearns", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("19160a", "Elysian (City)", "79", "Le Sueur", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("19160b", "Elysian (City)", "161", "Waseca", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("25280a", "Granite Falls (City)", "23", "Chippewa", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("25280b", "Granite Falls (City)", "173", "Yellow Medicine", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("26990b", "Hanover (City)", "171", "Wright", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("26990a", "Hanover (City)", "53", "Hennepin", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("27530a", "Hastings (City)", "37", "Dakota", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("27530b", "Hastings (City)", "163", "Washington", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("31760a", "Jasper (City)", "117", "Pipestone", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("31760b", "Jasper (City)", "133", "Rock", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("33866a", "La Crescent (City)", "55", "Houston", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("33866b", "La Crescent (City)", "169", "Winona", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("34172a", "Lake City (City)", "49", "Goodhue", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("34172b", "Lake City (City)", "157", "Wabasha", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("36746a", "Le Sueur (City)", "103", "Nicollet", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("36746b", "Le Sueur (City)", "143", "Sibley", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("36746c", "Le Sueur (City)", "79", "Le Sueur", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("58612a", "Sartell (City)", "9", "Benton", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("58612b", "Sartell (City)", "145", "Stearns", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("61996a", "Spring Lake Park (City)", "3", "Anoka", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("61996b", "Spring Lake Park (City)", "123", "Ramsey", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("56680a", "St. Anthony (City)", "53", "Hennepin", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("56680b", "St. Anthony (City)", "123", "Ramsey", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    
    c("56896a", "St. Cloud (City)", "9", "Benton", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("56896b", "St. Cloud (City)", "141", "Sherburne", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA),
    c("56896c", "St. Cloud (City)", "145", "Stearns", "City", NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA)
    
    
  ) %>%
  #for changing the county names of places in multiple counties
  mutate(
    countyName = replace(countyName, Id2short =="04798" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="04798" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="06382" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="06382" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="06580" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="06580" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="07282" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="07282" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="08092" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="08092" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="10918" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="10918" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="11008" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="11008" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="12772" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="12772" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="15022" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="15022" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="15706" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="15706" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="18134" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="18134" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="19160" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="19160" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="25280" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="25280" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="26990" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="26990" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="27530" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="27530" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="31760" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="31760" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="33866" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="33866" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="34172" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="34172" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="36746" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="36746" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="39878" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="39878" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="43036" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="43036" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="11800" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="11800" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="43198" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="43198" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="44422" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="44422" ,"Multiple Counties"),
    countyName = replace(countyName, Id2short =="45808" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="45808" ,"Multiple Counties"),
    
    
    countyName = replace(countyName, Id2short =="46924" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="46924" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="47068" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="47068" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="48562" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="48562" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="48796" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="48796" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="51136" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="51136" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="52522" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="52522" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="53656" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="53656" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="55006" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="55006" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="55438" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="55438" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="56014" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="56014" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="56176" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="56176" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="56950" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="56950" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="62446" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="62446" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="63778" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="63778" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="67504" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="67504" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="69970" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="69970" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="58612" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="58612" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="61996" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="61996" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="56680" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="56680" ,"Multiple Counties"),
    
    countyName = replace(countyName, Id2short =="56896" ,"Multiple Counties"),
    countyFIPS = replace(countyFIPS, Id2short =="56896" ,"Multiple Counties")
  )%>%
  
  #Sudi's code
  rbind(c("39878a","Mankato (City)","79","Le Sueur","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("39878b","Mankato (City)","13","Blue Earth","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("39878c","Mankato (City)","103","Nicollet","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("43036a","Minneiska (City)","157","Wabasha","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("43036b","Minneiska (City)","169","Winona","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("43198a","Minnesota Lake (City)","13","Blue Earth","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("43198b","Minnesota Lake (City)","43","Faribault","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("44422a","Motley (City)","21","Cass","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("44422b","Motley (City)","97","Morrison","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("45808a","New Prague (City)","79","Le Sueur","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("45808b","New Prague (City)","139","Scott","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("46924a","Northfield (City)","37","Dakota","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("46924b","Northfield (City)","131","Rice","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("47068a","North Mankato (City)","13","Blue Earth","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("47068b","North Mankato (City)","103","Nicollet","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("48562a","Ormsby (City)","91","Martin","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("48562b","Ormsby (City)","165","Watonwan","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("48796a","Osakis (City)","41","Douglas","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("48796b","Osakis (City)","153","Todd","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("51136a","Pine Island (City)","49","Goodhue","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("51136b","Pine Island (City)","109","Olmsted","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("52522a","Princeton (City)","95","Mille Lacs","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("52522b","Princeton (City)","141","Sherburne","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("53656a","Redwood Falls (City)","127","Redwood","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("53656b","Redwood Falls (City)","129","Renville","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("55006a","Rockford (City)","53","Hennepin","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("55006b","Rockford (City)","171","Wright","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("55438a","Roosevelt (City)","77","Lake of the Woods","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("55438b","Roosevelt (City)","135","Roseau","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("56014a","Rothsay (City)","111","Otter Tail","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("56014b","Rothsay (City)","167","Wilkin","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("56176a","Royalton (City)","9","Benton","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("56176b","Royalton (City)","97","Morrison","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("56950a","St. Francis (City)","3","Anoka","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("56950b","St. Francis (City)","59","Isanti","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("62446a","Staples (City)","153","Todd","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("62446b","Staples (City)","159","Wadena","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("63778a","Swanville (City)","97","Morrison","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("63778b","Swanville (City)","153","Todd","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("67504a","Wadena (City)","111","Otter Tail","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("67504b","Wadena (City)","159","Wadena","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("69970a","White Bear Lake (City)","123","Ramsey","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  rbind(c("69970b","White Bear Lake (City)","163","Washington","City",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))%>%
  
  filter(placeName != "America/Beltrami Is (Unorganized)")




#for finding all places with the same name
duplicates.1900 <- pop.countysub.1900_2000 %>% 
  group_by(placeName) %>% 
  filter(n()>1)

# Prep city pop 2016 -----------------------------------------------

pop.countysub.2016 <- read_csv("countyPlaces_2016.csv") %>%
  separate(col = Geography,into = c("placeName"),sep=",") %>%
  mutate(placeName = str_replace(placeName,"city","(City)"))%>%
  mutate(placeName = str_replace(placeName,"township","(Township)"))%>%
  mutate(placeName = str_replace(placeName,"UT","(Unorganized)")) %>%
  #mutate(Id2short = substr(Id2,6,10)) %>%
  mutate(Id2short = str_sub(Id2,-5,-1)) %>%
  mutate(
    placeName = ifelse(placeName == "Forest City (Township)" & Id2short == "21734", "Forest City Township (Township)", placeName),
    
    #changing the Id2short of places in multiple counties
    Id2short = replace(Id2short, Id2 =="2704904798" ,"04798a"),
    Id2short = replace(Id2short, Id2 =="2715704798" ,"04798b"),
    
    Id2short = replace(Id2short, Id2 =="2700306382" ,"06382a"),
    Id2short = replace(Id2short, Id2 =="2712306382" ,"06382b"),
    
    Id2short = replace(Id2short, Id2 =="2703906580" ,"06580a"),
    Id2short = replace(Id2short, Id2 =="2714706580" ,"06580b"),
    
    Id2short = replace(Id2short, Id2 =="2705907282" ,"07282a"),
    Id2short = replace(Id2short, Id2 =="2706507282" ,"07282b"),
    
    Id2short = replace(Id2short, Id2 =="2712108092" ,"08092a"),
    Id2short = replace(Id2short, Id2 =="2714508092" ,"08092b"),
    
    Id2short = replace(Id2short, Id2 =="2701910918" ,"10918a"),
    Id2short = replace(Id2short, Id2 =="2705310918" ,"10918b"),
    
    Id2short = replace(Id2short, Id2 =="2704511008" ,"11008a"),
    Id2short = replace(Id2short, Id2 =="2710911008" ,"11008b"),
    
    Id2short = replace(Id2short, Id2 =="2714511800" ,"11800a"),
    Id2short = replace(Id2short, Id2 =="2717111800" ,"11800b"),
    
    Id2short = replace(Id2short, Id2 =="2701512772" ,"12772a"),
    Id2short = replace(Id2short, Id2 =="2703312772" ,"12772b"),
    
    Id2short = replace(Id2short, Id2 =="2705315022" ,"15022a"),
    Id2short = replace(Id2short, Id2 =="2717115022" ,"15022b"),
    
    Id2short = replace(Id2short, Id2 =="2704915706" ,"15706a"),
    Id2short = replace(Id2short, Id2 =="2713115706" ,"15706b"),
    
    Id2short = replace(Id2short, Id2 =="2709318134" ,"18134a"),
    Id2short = replace(Id2short, Id2 =="2714518134" ,"18134b"),
    
    Id2short = replace(Id2short, Id2 =="2707919160" ,"19160a"),
    Id2short = replace(Id2short, Id2 =="2716119160" ,"19160b"),
    
    Id2short = replace(Id2short, Id2 =="2702325280" ,"25280a"),
    Id2short = replace(Id2short, Id2 =="2717325280" ,"25280b"),
    
    Id2short = replace(Id2short, Id2 =="2705326990" ,"26990a"),
    Id2short = replace(Id2short, Id2 =="2717126990" ,"26990b"),
    
    Id2short = replace(Id2short, Id2 =="2703727530" ,"27530a"),
    Id2short = replace(Id2short, Id2 =="2716327530" ,"27530b"),
    
    Id2short = replace(Id2short, Id2 =="2711731760" ,"31760a"),
    Id2short = replace(Id2short, Id2 =="2713331760" ,"31760b"),
    
    Id2short = replace(Id2short, Id2 =="2705533866" ,"33866a"),
    Id2short = replace(Id2short, Id2 =="2716933866" ,"33866b"),
    
    Id2short = replace(Id2short, Id2 =="2704934172" ,"34172a"),
    Id2short = replace(Id2short, Id2 =="2715734172" ,"34172b"),
    
    Id2short = replace(Id2short, Id2 =="2710336746" ,"36746a"),
    Id2short = replace(Id2short, Id2 =="2714336746" ,"36746b"),
    Id2short = replace(Id2short, Id2 =="2707936746" ,"36746c"),
    
    Id2short = replace(Id2short, Id2 =="2700958612" ,"58612a"),
    Id2short = replace(Id2short, Id2 =="2714558612" ,"58612b"),
    
    Id2short = replace(Id2short, Id2 =="2700361996" ,"61996a"),
    Id2short = replace(Id2short, Id2 =="2712361996" ,"61996b"),
    
    Id2short = replace(Id2short, Id2 =="2705356680" ,"56680a"),
    Id2short = replace(Id2short, Id2 =="2712356680" ,"56680b"),
    
    Id2short = replace(Id2short, Id2 =="2700956896" ,"56896a"),
    Id2short = replace(Id2short, Id2 =="2714156896" ,"56896b"),
    Id2short = replace(Id2short, Id2 =="2714556896" ,"56896c")
  ) %>%
  
  filter(placeName != "County subdivisions not defined") %>%
  rbind(c("0600000US2701764750", "2701764750", "Thomson (City)", "0", "0", "64750")) %>%
  rbind(c("0600000US2705327476", "2705327476", "Hassan Township (Township)", NA, NA, "27476")) %>%
  rbind(c("0600000US2706131256", "2706131256", "Iron Range Township (Township)", NA, NA, "31256")) %>%
  rbind(c("0600000US2716764426", "2716764426", "Tenney (Unorganized)", NA, NA, "64426")) %>%
  rbind(c("0600000US2713918656", "2713918656", "Elko (City)", NA, NA, "18656")) %>%
  rbind(c("0600000US2713945736", "2713945736", "New Market (City)", NA, NA, "45736")) %>%
  rbind(c("0600000US2713702872", "2713702872", "Aurora (City)", "1651", NA, "02872")) %>%
  rbind(c("0600000US2706109316", "2706109316", "Calumet (City)", "358", NA, "09316")) %>%
  rbind(c("0600000US2701910990", "2701910990", "Chaska Township (Township)", NA, NA, "10990")) %>%
  rbind(c("0600000US2716321788", "2716321788", "Forest Lake Township (Township)", NA, NA, "21788")) %>%
  rbind(c("0600000US2706125136", "2706125136", "Grand Rapids Township (Township)", NA, NA, "25136")) %>%
  rbind(c("0600000US2713701565", "2713701565", "Angleworm Lake (Unorganized)", NA, NA, "01565")) %>%
  rbind(c("0600000US2713704208", "2713704208", "Bear Head Lake (Unorganized)", NA, NA, "04208")) %>%
  rbind(c("0600000US2713709486", "2713709486", "Camp A Lake (Unorganized)", NA, NA, "09486")) %>%
  rbind(c("0600000US2713713626", "2713713626", "Crab Lake (Unorganized)", NA, NA, "13626")) %>%
  rbind(c("0600000US2713714778", "2713714778", "Dark River (Unorganized)", NA, NA, "14778")) %>%
  rbind(c("0600000US2713730616", "2713730616", "Hush Lake (Unorganized)", NA, NA, "30616")) %>%
  rbind(c("0600000US2703515094", "270351509", "Dean Lake (Unorganized)", NA, NA, "15094")) %>%
  rbind(c("0600000US2713511791", "2713511791", "Clear River/Oaks (Unorganized)", NA, NA, "11791")) %>%
  rbind(c("0600000US2713531652", "2713531652", "Jadis (Unorganized)", NA, NA, "31652")) %>%
  rbind(c("0600000US2713728259", "2713728259", "Heikkila Lake (Unorganized)", NA, NA, "28259")) %>%
  
  #for places in more than 1 county
  rbind(c("0600000US2704915704798", "2704915704798", "Bellechester (City)", NA, NA, "04798"),
        c("0600000US2700312306382", "2700312306382", "Blaine (City)", NA, NA, "06382"),
        c("0600000US2703914706580", "2703914706580", "Blooming Prairie (City)", NA, NA, "06580"),
        c("0600000US2705906507282", "2705906507282", "Braham (City)", NA, NA, "07282"),
        c("0600000US2712114508092", "2712114508092", "Brooten (City)", NA, NA, "08092"),
        c("0600000US2701905310918", "2701905310918", "Chanhassen (City)", NA, NA, "10918"),
        c("0600000US2704510911008", "2704510911008", "Chatfield (City)", NA, NA, "11008"),
        c("0600000US2714517111800", "2714517111800", "Clearwater (City)", NA, NA, "11800"),
        c("0600000US2703315112772", "2703315112772", "Comfrey (City)", NA, NA, "12772"),
        c("0600000US2705317115022", "2705317115022", "Dayton (City)", NA, NA, "15022"),
        c("0600000US2704913115706", "2704913115706", "Dennison (City)", NA, NA, "15706"),
        c("0600000US2709314518134", "2709314518134", "Eden Valley (City)", NA, NA, "18134"),
        c("0600000US2707916119160", "2707916119160", "Elysian (City)", NA, NA, "19160"),
        c("0600000US2702317325280", "2702317325280", "Granite Falls (City)", NA, NA, "25280"),
        c("0600000US2705317126990", "2705317126990", "Hanover (City)", NA, NA, "26990"),
        c("0600000US2703716327530", "2703716327530", "Hastings (City)", NA, NA, "27530"),
        c("0600000US2711713331760", "2711713331760", "Jasper (City)", NA, NA, "31760"),
        c("0600000US2705516933866", "2705516933866", "La Crescent (City)", NA, NA, "33866"),
        c("0600000US2704915734172", "2704915734172", "Lake City (City)", NA, NA, "34172"),
        c("0600000US2707910314336746", "2707910314336746", "Le Sueur (City)", NA, NA, "36746"),
        c("0600000US2700914558612", "2700914558612", "Sartell (City)", NA, NA, "58612"),
        c("0600000US2700312361996", "2700312361996", "Spring Lake Park (City)", NA, NA, "61996"),
        c("0600000US2705312356680", "2705312356680", "St. Anthony (City)", NA, NA, "56680"),
        c("0600000US2700914114556896", "2700914114556896", "St. Cloud (City)", NA, NA, "56896")
  ) %>%
  
  #Sudi's code
  rbind(c("0600000US2713736075","2713736075","Leander Lake (Unorganized)",NA,NA,"36075"))%>%
  rbind(c("0600000US2713737384","2713737384","Linwood Lake (Unorganized)",NA,NA,"37384"))%>%
  rbind(c("0600000US2706140418","2706140418","Marble (City)","682",NA,"40418"))%>%
  rbind(c("0600000US2713740624","2713740624","Marion Lake (Unorganized)",NA,NA,"40624"))%>%
  rbind(c("0600000US2713744714","2713744714","Mud Hen Lake (Unorganized)",NA,NA,"44714"))%>%
  rbind(c("0600000US2706144980","2706144980","Nashwauk (City)","958",NA,"44980"))%>%
  rbind(c("0600000US2716345952","2716345952","New Scandia (City)","4078",NA,"45952"))%>%
  rbind(c("0600000US2706950065","2706950065","Peatland (Unorganized)",NA,NA,"50065"))%>%
  rbind(c("0600000US2713750643","2713750643","Pfeiffer Lake (Unorganized)",NA,NA,"50643"))%>%
  rbind(c("0600000US2713750730","2713750730","Picket Lake (Unorganized)",NA,NA,"50730"))%>%
  rbind(c("0600000US2714551586","2714551586","Pleasant Lake (City)",NA,NA,"51586"))%>%
  rbind(c("0600000US2700955366","2700955366","Ronneby (City)",NA,NA,"55366"))%>%
  rbind(c("0600000US2703560106","2703560106","Sibley (Township)",NA,NA,"60106"))%>%
  rbind(c("0600000US2713760888","2713760888","Slim Lake (Unorganized)",NA,NA,"60888"))%>%
  rbind(c("0600000US2713763246","2713763246","Sturgeon River (Unorganized)",NA,NA,"63246"))%>%
  rbind(c("0600000US2713763516","2713763516","Sunday Lake (Unorganized)",NA,NA,"63516"))%>%
  rbind(c("0600000US2713764886","2713764886","Tikander Lake (Unorganized)",NA,NA,"64886"))%>%
  rbind(c("0600000US2703569316","2703569316","West Crow Wing (Unorganized)",NA,NA,"69316"))%>%
  rbind(c("0600000US2702572040","2702572040","Wyoming (Township)",NA,NA,"72040"))%>%
  rbind(c("0600000US2709752954","2709752954","Rail Prairie (Township)",NA,NA,"52954"))%>%
  rbind(c("0600000US2714555096","2714555096","Rockville (Township)",NA,NA,"55096")) %>%
  
  #Sudi's code
  rbind(c("0600000US2701307910339878","2701307910339878","Mankato (City)",NA,NA,"39878"))%>%
  rbind(c("0600000US2715716943198","2715716943036","Minneiska (City)",NA,NA,"43036"))%>%
  rbind(c("0600000US2701304343198","2701304343198","Minnesota Lake (City)",NA,NA,"43198"))%>%
  rbind(c("0600000US2702109744422","2702109744422","Motley (City)",NA,NA,"44422"))%>%
  rbind(c("0600000US2707913945808","2707913945808","New Prague (City)",NA,NA,"45808"))%>%
  rbind(c("0600000US2703713146924","2703713146924","Northfield (City)",NA,NA,"46924"))%>%
  rbind(c("0600000US2701310347068","2701310347068","North Mankato (City)",NA,NA,"47068"))%>%
  rbind(c("0600000US2709116548562","2709116548562","Ormsby (City)",NA,NA,"48562"))%>%
  rbind(c("0600000US2704115348796","2704115348796","Osakis (City)",NA,NA,"48796"))%>%
  rbind(c("0600000US2704910951136","2704910951136","Pine Island (City)",NA,NA,"51136"))%>%
  rbind(c("0600000US2709514152522","2709514152522","Princeton (City)",NA,NA,"52522"))%>%
  rbind(c("0600000US2712712953656","2712712953656","Redwood Falls (City)",NA,NA,"53656"))%>%
  rbind(c("0600000US2705317155006","2705317155006","Rockford (City)",NA,NA,"55006"))%>%
  rbind(c("0600000US2707713555438","2707713555438","Roosevelt (City)",NA,NA,"55438"))%>%
  rbind(c("0600000US2711116756014","2711116756014","Rothsay (City)",NA,NA,"56014"))%>%
  rbind(c("0600000US2700909756176","2700909756176","Royalton (City)",NA,NA,"56176"))%>%
  rbind(c("0600000US2700305956950","2700305956950","St. Francis (City)",NA,NA,"56950"))%>%
  rbind(c("0600000US2715315962446","2715315962446","Staples (City)",NA,NA,"62446"))%>%
  rbind(c("0600000US2709715363778","2709715363778","Swanville (City)",NA,NA,"63778"))%>%
  rbind(c("0600000US2711115967504","2711115967504","Wadena (City)",NA,NA,"67504"))%>%
  rbind(c("0600000US2712316369970","2712316369970","White Bear Lake (City)",NA,NA,"69970"))%>%
  
  mutate(
    Id2short = ifelse(placeName == "Mankato (City)" & Id2 == "2701339878","39878b",Id2short),
    Id2short = ifelse(placeName == "Mankato (City)" & Id2 == "2707939878","39878a",Id2short),
    Id2short = ifelse(placeName == "Mankato (City)" & Id2 == "2710339878","39878c",Id2short),
    Id2short = ifelse(placeName == "Minneiska (City)" & Id2 == "2715743036","43036a",Id2short),
    Id2short = ifelse(placeName == "Minneiska (City)" & Id2 == "2716943036","43036b",Id2short),
    Id2short = ifelse(placeName == "Minnesota Lake (City)" & Id2 == "2701343198","43198a",Id2short),
    Id2short = ifelse(placeName == "Minnesota Lake (City)" & Id2 == "2704343198","43198b",Id2short),
    Id2short = ifelse(placeName == "Motley (City)" & Id2 == "2702144422","44422a",Id2short),
    Id2short = ifelse(placeName == "Motley (City)" & Id2 == "2709744422","44422b",Id2short),
    Id2short = ifelse(placeName == "New Prague (City)" & Id2 == "2707945808","45808a",Id2short),
    Id2short = ifelse(placeName == "New Prague (City)" & Id2 == "2713945808","45808b",Id2short),
    Id2short = ifelse(placeName == "Northfield (City)" & Id2 == "2703746924","46924a",Id2short),
    Id2short = ifelse(placeName == "Northfield (City)" & Id2 == "2713146924","46924b",Id2short),
    Id2short = ifelse(placeName == "North Mankato (City)" & Id2 == "2701347068","47068a",Id2short),
    Id2short = ifelse(placeName == "North Mankato (City)" & Id2 == "2710347068","47068b",Id2short),
    Id2short = ifelse(placeName == "Ormsby (City)" & Id2 == "2709148562","48562a",Id2short),
    Id2short = ifelse(placeName == "Ormsby (City)" & Id2 == "2716548562","48562b",Id2short),
    Id2short = ifelse(placeName == "Osakis (City)" & Id2 == "2704148796","48796a",Id2short),
    Id2short = ifelse(placeName == "Osakis (City)" & Id2 == "2715348796","48796b",Id2short),
    Id2short = ifelse(placeName == "Pine Island (City)" & Id2 == "2704951136","51136a",Id2short),
    Id2short = ifelse(placeName == "Pine Island (City)" & Id2 == "2710951136","51136b",Id2short),
    Id2short = ifelse(placeName == "Princeton (City)" & Id2 == "2709552522","52522a",Id2short),
    Id2short = ifelse(placeName == "Princeton (City)" & Id2 == "2714152522","52522b",Id2short),
    Id2short = ifelse(placeName == "Redwood Falls (City)" & Id2 == "2712753656","53656a",Id2short),
    Id2short = ifelse(placeName == "Redwood Falls (City)" & Id2 == "2712953656","53656b",Id2short),
    Id2short = ifelse(placeName == "Rockford (City)" & Id2 == "2705355006","55006a",Id2short),
    Id2short = ifelse(placeName == "Rockford (City)" & Id2 == "2717155006","55006b",Id2short),
    Id2short = ifelse(placeName == "Roosevelt (City)" & Id2 == "2707755438","55438a",Id2short),
    Id2short = ifelse(placeName == "Roosevelt (City)" & Id2 == "2713555438","55438b",Id2short),
    Id2short = ifelse(placeName == "Rothsay (City)" & Id2 == "2711156014","56014a",Id2short),
    Id2short = ifelse(placeName == "Rothsay (City)" & Id2 == "2716756014","56014b",Id2short),
    Id2short = ifelse(placeName == "Royalton (City)" & Id2 == "2700956176","56176a",Id2short),
    Id2short = ifelse(placeName == "Royalton (City)" & Id2 == "2709756176","56176b",Id2short),
    Id2short = ifelse(placeName == "St. Francis (City)" & Id2 == "2700356950","56950a",Id2short),
    Id2short = ifelse(placeName == "St. Francis (City)" & Id2 == "2705956950","56950b",Id2short),
    Id2short = ifelse(placeName == "Staples (City)" & Id2 == "2715362446","62446a",Id2short),
    Id2short = ifelse(placeName == "Staples (City)" & Id2 == "2715962446","62446b",Id2short),
    Id2short = ifelse(placeName == "Swanville (City)" & Id2 == "2709763778","63778a",Id2short),
    Id2short = ifelse(placeName == "Swanville (City)" & Id2 == "2715363778","63778b",Id2short),
    Id2short = ifelse(placeName == "Wadena (City)" & Id2 == "2711167504","67504a",Id2short),
    Id2short = ifelse(placeName == "Wadena (City)" & Id2 == "2715967504","67504b",Id2short),
    Id2short = ifelse(placeName == "White Bear Lake (City)" & Id2 == "2712369970","69970a",Id2short),
    Id2short = ifelse(placeName == "White Bear Lake (City)" & Id2 == "2716369970","69970b",Id2short)
  )%>%
  
  mutate(Id2 = as.character(Id2))



# Prep city pop 2010  -----------------------------------------------

pop.countysub.2010 <- read_csv("countyPlaces_2010.csv") %>%
  rename(pop2010=Total)%>%
  separate(col = Geography,into = c("placeName"),sep=",") %>%
  mutate(placeName = str_replace(placeName,"city","(City)"))%>%
  mutate(placeName = str_replace(placeName,"township","(Township)"))%>%
  mutate(placeName = str_replace(placeName,"UT","(Unorganized)")) %>%
  mutate(Id2short = str_sub(Id2,-5,-1)) %>%
  rbind(c("0600000US2701104204", "2701104204", "Beardsley (City)", "233", "04204")) %>%
  rbind(c("0600000US2701148706", "2701148706", "Ortonville (City)", "2011", "48706")) %>%
  rbind(c("0600000US2701132012", "2701132012", "Johnson (City)", "29", "32012")) %>%
  rbind(c("0600000US2706164048", "2706164048", "Taconite (City)", "360", "64048")) %>%
  rbind(c("0600000US2713733416", "2713733416", "Kinney (City)", "169", "33416")) %>%
  rbind(c("0600000US2713918656", "2713918656", "Elko (City)", NA, "18656")) %>%
  rbind(c("0600000US2713945736", "2713945736", "New Market (City)", NA, "45736")) %>%
  rbind(c("0600000US2713702872", "2713702872", "Aurora (City)", "1682", "02872")) %>%
  rbind(c("0600000US2706109316", "2706109316", "Calumet (City)", "367", "09316")) %>%
  rbind(c("0600000US2701910990", "2701910990", "Chaska Township (Township)", NA, "10990")) %>%
  rbind(c("0600000US2716321788", "2716321788", "Forest Lake Township (Township)", NA, "21788")) %>%
  rbind(c("0600000US2706125136", "2706125136", "Grand Rapids Township (Township)", NA, "25136")) %>%
  rbind(c("0600000US2713701565", "2713701565", "Angleworm Lake (Unorganized)", NA, "01565")) %>%
  rbind(c("0600000US2713704208", "2713704208", "Bear Head Lake (Unorganized)", NA, "04208")) %>%
  rbind(c("0600000US2713709486", "2713709486", "Camp A Lake (Unorganized)", NA, "09486")) %>%
  rbind(c("0600000US2713713626", "2713713626", "Crab Lake (Unorganized)", NA, "13626")) %>%
  rbind(c("0600000US2713714778", "2713714778", "Dark River (Unorganized)", NA, "14778")) %>%
  rbind(c("0600000US2713730616", "2713730616", "Hush Lake (Unorganized)", NA, "30616")) %>%
  rbind(c("0600000US2703515094", "270351509", "Dean Lake (Unorganized)", NA, "15094")) %>%
  rbind(c("0600000US2713511791", "2713511791", "Clear River/Oaks (Unorganized)", NA, "11791")) %>%
  rbind(c("0600000US2713531652", "2713531652", "Jadis (Unorganized)", NA, "31652")) %>%
  rbind(c("0600000US2713728259", "2713728259", "Heikkila Lake (Unorganized)", NA, "28259")) %>%
  
  #Sudi's code
  rbind(c("0600000US2713736075","2713736075","Leander Lake (Unorganized)",NA,"36075"))%>%
  rbind(c("0600000US2713737384","2713737384","Linwood Lake (Unorganized)",NA,"37384"))%>%
  rbind(c("0600000US2706140418","2706140418","Marble (City)","700","40418"))%>%
  rbind(c("0600000US2713740624","2713740624","Marion Lake (Unorganized)",NA,"40624"))%>%
  rbind(c("0600000US2713744714","2713744714","Mud Hen Lake (Unorganized)",NA,"44714"))%>%
  rbind(c("0600000US2706144980","2706144980","Nashwauk (City)","984","44980"))%>%
  rbind(c("0600000US2716345952","2716345952","New Scandia (City)","3939","45952"))%>%
  rbind(c("0600000US2706950065","2706950065","Peatland (Unorganized)",NA,"50065"))%>%
  rbind(c("0600000US2713750643","2713750643","Pfeiffer Lake (Unorganized)",NA,"50643"))%>%
  rbind(c("0600000US2713750730","2713750730","Picket Lake (Unorganized)",NA,"50730"))%>%
  rbind(c("0600000US2714551586","2714551586","Pleasant Lake (City)",NA,"51586"))%>%
  rbind(c("0600000US2700955366","2700955366","Ronneby (City)",NA,"55366"))%>%
  rbind(c("0600000US2703560106","2703560106","Sibley (Township)",NA,"60106"))%>%
  rbind(c("0600000US2713760888","2713760888","Slim Lake (Unorganized)",NA,"60888"))%>%
  rbind(c("0600000US2713763246","2713763246","Sturgeon River (Unorganized)",NA,"63246"))%>%
  rbind(c("0600000US2713763516","2713763516","Sunday Lake (Unorganized)",NA,"63516"))%>%
  rbind(c("0600000US2713764886","2713764886","Tikander Lake (Unorganized)",NA,"64886"))%>%
  rbind(c("0600000US2703569316","2703569316","West Crow Wing (Unorganized)",NA,"69316"))%>%
  rbind(c("0600000US2702572040","2702572040","Wyoming (Township)",NA,"72040"))%>%
  rbind(c("0600000US2709752954","2709752954","Rail Prairie (Township)",NA,"52954"))%>%
  rbind(c("0600000US2714555096","2714555096","Rockville (Township)",NA,"55096")) %>%
  
  mutate(
    placeName = ifelse(placeName == "Liberty (Township)" & Id2 == "2706136944", "Liberty (Unorganized)", placeName),
    placeName = ifelse(placeName == "Rice Lake (Township)" & Id2 == "2713754060", "Rice Lake (City)", placeName),
    placeName = ifelse(placeName == "Forest City (Township)" & Id2short == "21734", "Forest City Township (Township)", placeName),
    placeName = ifelse(placeName == "Forest City (Township)" & Id2short == "21734", "Forest City Township (Township)", placeName),
    placeName = ifelse(placeName == "Hassan (Township)" & Id2short == "27476", "Hassan Township (Township)", placeName),
    placeName = ifelse(placeName == "Iron Range (Township)" & Id2short == "31256", "Iron Range Township (Township)", placeName),
    placeName = ifelse(placeName == "Tenney (City)" & Id2short == "64426", "Tenney (Unorganized)", placeName),
    
    #Mika's code
    #changing the Id2short of places in multiple counties
    Id2short = replace(Id2short, Id2 =="2704904798" ,"04798a"),
    Id2short = replace(Id2short, Id2 =="2715704798" ,"04798b"),
    
    Id2short = replace(Id2short, Id2 =="2700306382" ,"06382a"),
    Id2short = replace(Id2short, Id2 =="2712306382" ,"06382b"),
    
    Id2short = replace(Id2short, Id2 =="2703906580" ,"06580a"),
    Id2short = replace(Id2short, Id2 =="2714706580" ,"06580b"),
    
    Id2short = replace(Id2short, Id2 =="2705907282" ,"07282a"),
    Id2short = replace(Id2short, Id2 =="2706507282" ,"07282b"),
    
    Id2short = replace(Id2short, Id2 =="2712108092" ,"08092a"),
    Id2short = replace(Id2short, Id2 =="2714508092" ,"08092b"),
    
    Id2short = replace(Id2short, Id2 =="2701910918" ,"10918a"),
    Id2short = replace(Id2short, Id2 =="2705310918" ,"10918b"),
    
    Id2short = replace(Id2short, Id2 =="2704511008" ,"11008a"),
    Id2short = replace(Id2short, Id2 =="2710911008" ,"11008b"),
    
    Id2short = replace(Id2short, Id2 =="2714511800" ,"11800a"),
    Id2short = replace(Id2short, Id2 =="2717111800" ,"11800b"),
    
    Id2short = replace(Id2short, Id2 =="2701512772" ,"12772a"),
    Id2short = replace(Id2short, Id2 =="2703312772" ,"12772b"),
    
    Id2short = replace(Id2short, Id2 =="2705315022" ,"15022a"),
    Id2short = replace(Id2short, Id2 =="2717115022" ,"15022b"),
    
    Id2short = replace(Id2short, Id2 =="2704915706" ,"15706a"),
    Id2short = replace(Id2short, Id2 =="2713115706" ,"15706b"),
    
    Id2short = replace(Id2short, Id2 =="2709318134" ,"18134a"),
    Id2short = replace(Id2short, Id2 =="2714518134" ,"18134b"),
    
    Id2short = replace(Id2short, Id2 =="2707919160" ,"19160a"),
    Id2short = replace(Id2short, Id2 =="2716119160" ,"19160b"),
    
    Id2short = replace(Id2short, Id2 =="2702325280" ,"25280a"),
    Id2short = replace(Id2short, Id2 =="2717325280" ,"25280b"),
    
    Id2short = replace(Id2short, Id2 =="2705326990" ,"26990a"),
    Id2short = replace(Id2short, Id2 =="2717126990" ,"26990b"),
    
    Id2short = replace(Id2short, Id2 =="2703727530" ,"27530a"),
    Id2short = replace(Id2short, Id2 =="2716327530" ,"27530b"),
    
    Id2short = replace(Id2short, Id2 =="2711731760" ,"31760a"),
    Id2short = replace(Id2short, Id2 =="2713331760" ,"31760b"),
    
    Id2short = replace(Id2short, Id2 =="2705533866" ,"33866a"),
    Id2short = replace(Id2short, Id2 =="2716933866" ,"33866b"),
    
    Id2short = replace(Id2short, Id2 =="2704934172" ,"34172a"),
    Id2short = replace(Id2short, Id2 =="2715734172" ,"34172b"),
    
    Id2short = replace(Id2short, Id2 =="2710336746" ,"36746a"),
    Id2short = replace(Id2short, Id2 =="2714336746" ,"36746b"),
    Id2short = replace(Id2short, Id2 =="2707936746" ,"36746c"),
    
    Id2short = replace(Id2short, Id2 =="2700958612" ,"58612a"),
    Id2short = replace(Id2short, Id2 =="2714558612" ,"58612b"),
    
    Id2short = replace(Id2short, Id2 =="2700361996" ,"61996a"),
    Id2short = replace(Id2short, Id2 =="2712361996" ,"61996b"),
    
    Id2short = replace(Id2short, Id2 =="2705356680" ,"56680a"),
    Id2short = replace(Id2short, Id2 =="2712356680" ,"56680b"),
    
    Id2short = replace(Id2short, Id2 =="2700956896" ,"56896a"),
    Id2short = replace(Id2short, Id2 =="2714156896" ,"56896b"),
    Id2short = replace(Id2short, Id2 =="2714556896" ,"56896c")
  ) %>%
  
  #Mika's code
  #Adding entries for places in multiple counties
  rbind(c("0600000US2704915704798", "2704915704798", "Bellechester (City)", NA, "04798"),
        c("0600000US2700312306382", "2700312306382", "Blaine (City)", NA, "06382"),
        c("0600000US2703914706580", "2703914706580", "Blooming Prairie (City)", NA, "06580"),
        c("0600000US2705906507282", "2705906507282", "Braham (City)", NA, "07282"),
        c("0600000US2712114508092", "2712114508092", "Brooten (City)", NA,  "08092"),
        c("0600000US2701905310918", "2701905310918", "Chanhassen (City)", NA, "10918"),
        c("0600000US2704510911008", "2704510911008", "Chatfield (City)", NA, "11008"),
        c("0600000US2714517111800", "2714517111800", "Clearwater (City)", NA, "11800"),
        c("0600000US2703315112772", "2703315112772", "Comfrey (City)", NA, "12772"),
        c("0600000US2705317115022", "2705317115022", "Dayton (City)", NA, "15022"),
        c("0600000US2704913115706", "2704913115706", "Dennison (City)", NA, "15706"),
        c("0600000US2709314518134", "2709314518134", "Eden Valley (City)", NA, "18134"),
        c("0600000US2707916119160", "2707916119160", "Elysian (City)", NA, "19160"),
        c("0600000US2702317325280", "2702317325280", "Granite Falls (City)", NA, "25280"),
        c("0600000US2705317126990", "2705317126990", "Hanover (City)", NA, "26990"),
        c("0600000US2703716327530", "2703716327530", "Hastings (City)", NA, "27530"),
        c("0600000US2711713331760", "2711713331760", "Jasper (City)", NA, "31760"),
        c("0600000US2705516933866", "2705516933866", "La Crescent (City)", NA, "33866"),
        c("0600000US2704915734172", "2704915734172", "Lake City (City)", NA,"34172"),
        c("0600000US2707910314336746", "2707910314336746", "Le Sueur (City)", NA, "36746"),
        c("0600000US2700914558612", "2700914558612", "Sartell (City)", NA, "58612"),
        c("0600000US2700312361996", "2700312361996", "Spring Lake Park (City)", NA, "61996"),
        c("0600000US2705312356680", "2705312356680", "St. Anthony (City)", NA, "56680"),
        c("0600000US2700914114556896", "2700914114556896", "St. Cloud (City)", NA, "56896")
  ) %>%
  
  #Sudi's code
  rbind(c("0600000US2701307910339878","2701307910339878","Mankato (City)",NA,"39878"))%>%
  rbind(c("0600000US2715716943198","2715716943036","Minneiska (City)",NA,"43036"))%>%
  rbind(c("0600000US2701304343198","2701304343198","Minnesota Lake (City)",NA,"43198"))%>%
  rbind(c("0600000US2702109744422","2702109744422","Motley (City)",NA,"44422"))%>%
  rbind(c("0600000US2707913945808","2707913945808","New Prague (City)",NA,"45808"))%>%
  rbind(c("0600000US2703713146924","2703713146924","Northfield (City)",NA,"46924"))%>%
  rbind(c("0600000US2701310347068","2701310347068","North Mankato (City)",NA,"47068"))%>%
  rbind(c("0600000US2709116548562","2709116548562","Ormsby (City)",NA,"48562"))%>%
  rbind(c("0600000US2704115348796","2704115348796","Osakis (City)",NA,"48796"))%>%
  rbind(c("0600000US2704910951136","2704910951136","Pine Island (City)",NA,"51136"))%>%
  rbind(c("0600000US2709514152522","2709514152522","Princeton (City)",NA,"52522"))%>%
  rbind(c("0600000US2712712953656","2712712953656","Redwood Falls (City)",NA,"53656"))%>%
  rbind(c("0600000US2705317155006","2705317155006","Rockford (City)",NA,"55006"))%>%
  rbind(c("0600000US2707713555438","2707713555438","Roosevelt (City)",NA,"55438"))%>%
  rbind(c("0600000US2711116756014","2711116756014","Rothsay (City)",NA,"56014"))%>%
  rbind(c("0600000US2700909756176","2700909756176","Royalton (City)",NA,"56176"))%>%
  rbind(c("0600000US2700305956950","2700305956950","St. Francis (City)",NA,"56950"))%>%
  rbind(c("0600000US2715315962446","2715315962446","Staples (City)",NA,"62446"))%>%
  rbind(c("0600000US2709715363778","2709715363778","Swanville (City)",NA,"63778"))%>%
  rbind(c("0600000US2711115967504","2711115967504","Wadena (City)",NA,"67504"))%>%
  rbind(c("0600000US2712316369970","2712316369970","White Bear Lake (City)",NA,"69970"))%>%
  
  mutate(
    Id2short = ifelse(placeName == "Mankato (City)" & Id2 == "2701339878","39878b",Id2short),
    Id2short = ifelse(placeName == "Mankato (City)" & Id2 == "2707939878","39878a",Id2short),
    Id2short = ifelse(placeName == "Mankato (City)" & Id2 == "2710339878","39878c",Id2short),
    Id2short = ifelse(placeName == "Minneiska (City)" & Id2 == "2715743036","43036a",Id2short),
    Id2short = ifelse(placeName == "Minneiska (City)" & Id2 == "2716943036","43036b",Id2short),
    Id2short = ifelse(placeName == "Minnesota Lake (City)" & Id2 == "2701343198","43198a",Id2short),
    Id2short = ifelse(placeName == "Minnesota Lake (City)" & Id2 == "2704343198","43198b",Id2short),
    Id2short = ifelse(placeName == "Motley (City)" & Id2 == "2702144422","44422a",Id2short),
    Id2short = ifelse(placeName == "Motley (City)" & Id2 == "2709744422","44422b",Id2short),
    Id2short = ifelse(placeName == "New Prague (City)" & Id2 == "2707945808","45808a",Id2short),
    Id2short = ifelse(placeName == "New Prague (City)" & Id2 == "2713945808","45808b",Id2short),
    Id2short = ifelse(placeName == "Northfield (City)" & Id2 == "2703746924","46924a",Id2short),
    Id2short = ifelse(placeName == "Northfield (City)" & Id2 == "2713146924","46924b",Id2short),
    Id2short = ifelse(placeName == "North Mankato (City)" & Id2 == "2701347068","47068a",Id2short),
    Id2short = ifelse(placeName == "North Mankato (City)" & Id2 == "2710347068","47068b",Id2short),
    Id2short = ifelse(placeName == "Ormsby (City)" & Id2 == "2709148562","48562a",Id2short),
    Id2short = ifelse(placeName == "Ormsby (City)" & Id2 == "2716548562","48562b",Id2short),
    Id2short = ifelse(placeName == "Osakis (City)" & Id2 == "2704148796","48796a",Id2short),
    Id2short = ifelse(placeName == "Osakis (City)" & Id2 == "2715348796","48796b",Id2short),
    Id2short = ifelse(placeName == "Pine Island (City)" & Id2 == "2704951136","51136a",Id2short),
    Id2short = ifelse(placeName == "Pine Island (City)" & Id2 == "2710951136","51136b",Id2short),
    Id2short = ifelse(placeName == "Princeton (City)" & Id2 == "2709552522","52522a",Id2short),
    Id2short = ifelse(placeName == "Princeton (City)" & Id2 == "2714152522","52522b",Id2short),
    Id2short = ifelse(placeName == "Redwood Falls (City)" & Id2 == "2712753656","53656a",Id2short),
    Id2short = ifelse(placeName == "Redwood Falls (City)" & Id2 == "2712953656","53656b",Id2short),
    Id2short = ifelse(placeName == "Rockford (City)" & Id2 == "2705355006","55006a",Id2short),
    Id2short = ifelse(placeName == "Rockford (City)" & Id2 == "2717155006","55006b",Id2short),
    Id2short = ifelse(placeName == "Roosevelt (City)" & Id2 == "2707755438","55438a",Id2short),
    Id2short = ifelse(placeName == "Roosevelt (City)" & Id2 == "2713555438","55438b",Id2short),
    Id2short = ifelse(placeName == "Rothsay (City)" & Id2 == "2711156014","56014a",Id2short),
    Id2short = ifelse(placeName == "Rothsay (City)" & Id2 == "2716756014","56014b",Id2short),
    Id2short = ifelse(placeName == "Royalton (City)" & Id2 == "2700956176","56176a",Id2short),
    Id2short = ifelse(placeName == "Royalton (City)" & Id2 == "2709756176","56176b",Id2short),
    Id2short = ifelse(placeName == "St. Francis (City)" & Id2 == "2700356950","56950a",Id2short),
    Id2short = ifelse(placeName == "St. Francis (City)" & Id2 == "2705956950","56950b",Id2short),
    Id2short = ifelse(placeName == "Staples (City)" & Id2 == "2715362446","62446a",Id2short),
    Id2short = ifelse(placeName == "Staples (City)" & Id2 == "2715962446","62446b",Id2short),
    Id2short = ifelse(placeName == "Swanville (City)" & Id2 == "2709763778","63778a",Id2short),
    Id2short = ifelse(placeName == "Swanville (City)" & Id2 == "2715363778","63778b",Id2short),
    Id2short = ifelse(placeName == "Wadena (City)" & Id2 == "2711167504","67504a",Id2short),
    Id2short = ifelse(placeName == "Wadena (City)" & Id2 == "2715967504","67504b",Id2short),
    Id2short = ifelse(placeName == "White Bear Lake (City)" & Id2 == "2712369970","69970a",Id2short),
    Id2short = ifelse(placeName == "White Bear Lake (City)" & Id2 == "2716369970","69970b",Id2short)
  )%>%
  
  filter(placeName != "County subdivisions not defined") %>%
  mutate(Id2 = as.character(Id2))

#for checking which parts of 2010 and 2016 do not merge on Id
anti2010_2016 <- anti_join(pop.countysub.2016,pop.countysub.2010, by=c("Id"))
anti2010_2016reverse <- anti_join(pop.countysub.2010, pop.countysub.2016, by=c("Id"))

#for merging 2010 and 2016 data on Id2short
subtotal_2010_2016 <- full_join(pop.countysub.2016, pop.countysub.2010, by = c("Id2short","placeName"), copy = FALSE, suffix = c(".16", ".10"))

#for checking which parts of subtotal_2010_2016 (2010+2016) and 1900_2000 do not merge on Id2short
anti1900_2010_2016 <- anti_join(subtotal_2010_2016,pop.countysub.1900_2000, by=c("Id2short"))
anti1900_2010_2016reverse <- anti_join(pop.countysub.1900_2000,subtotal_2010_2016, by=c("Id2short"))

#for merging subtotal_2010_2016 (2010+2016) and 1900_200 on Id2short
total <- full_join(subtotal_2010_2016,pop.countysub.1900_2000,by = c("Id2short","placeName")) %>%
  select(-Id2short,-"Id2.16",-ch9000pct, -beale93R, -beale03, -rec_code, -"Id.16", -"Id2.10",-"Margin of Error; Total") %>%
  rename(Id = Id.10) %>%
  select(Id, placeName, area, countyName, countyFIPS, pop1900, pop1910, pop1920, pop1930, pop1940, pop1950, pop1960, pop1970, pop1980, pop1990, pop2000, pop2010, pop2016) %>%
  mutate(
    area = as.factor(area),
    placeName = str_replace(placeName, " \\(City\\)", ""),
    placeName = str_replace(placeName, " City City", " City"),
    placeName = str_replace(placeName, " \\(Township\\)", " Township"),
    placeName = str_replace(placeName, " \\(Unorganized\\)", ""))

duplicates.total <- total %>% 
  group_by(Id) %>% 
  filter(n()>1)

duplicates.2010 <- pop.countysub.2010 %>% 
  group_by(Id2short) %>% 
  filter(n()>1)



# Prep data for visualization  -----------------------------------------------

total.tidy <- total %>%
  gather(year,population,pop1900:pop2016) %>%
  mutate(
    year = replace(year, year =="pop1900" ,"1900"),
    year = replace(year, year =="pop1910" ,"1910"),
    year = replace(year, year =="pop1920" ,"1920"),
    year = replace(year, year =="pop1930" ,"1930"),
    year = replace(year, year =="pop1940" ,"1940"),
    year = replace(year, year =="pop1950" ,"1950"),
    year = replace(year, year =="pop1960" ,"1960"),
    year = replace(year, year =="pop1970" ,"1970"),
    year = replace(year, year =="pop1980" ,"1980"),
    year = replace(year, year =="pop1990" ,"1990"),
    year = replace(year, year =="pop2000" ,"2000"),
    year = replace(year, year =="pop2010" ,"2010"),
    year = replace(year, year =="pop2016" ,"2016"),
    population = ifelse(year == "2010" & Id == "0600000US2704915704798", "175", population),
    population = ifelse(year == "2016" & Id == "0600000US2704915704798", "228", population),
    population = ifelse(year == "2010" & Id == "0600000US2700312306382", "57186", population),
    population = ifelse(year == "2016" & Id == "0600000US2700312306382", "61210", population),
    population = ifelse(year == "2010" & Id == "0600000US2703914706580", "1996", population),
    population = ifelse(year == "2016" & Id == "0600000US2703914706580", "1814", population),
    population = ifelse(year == "2010" & Id == "0600000US2705906507282", "1793", population),
    population = ifelse(year == "2016" & Id == "0600000US2705906507282", "1860", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2712114508092", "743", population),
    population = ifelse(year == "2016" & Id == "0600000US2712114508092", "630", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2701905310918", "22952", population),
    population = ifelse(year == "2016" & Id == "0600000US2701905310918", "24796", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2704510911008", "2779", population),
    population = ifelse(year == "2016" & Id == "0600000US2704510911008", "2821", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2714517111800", "1735", population),
    population = ifelse(year == "2016" & Id == "0600000US2714517111800", "1743", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2703315112772", "382", population),
    population = ifelse(year == "2016" & Id == "0600000US2703315112772", "383", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2705317115022", "4671", population),
    population = ifelse(year == "2016" & Id == "0600000US2705317115022", "5031", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2704913115706", "212", population),
    population = ifelse(year == "2016" & Id == "0600000US2704913115706", "162", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2709314518134", "1042", population),
    population = ifelse(year == "2016" & Id == "0600000US2709314518134", "981", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2707916119160", "652", population),
    population = ifelse(year == "2016" & Id == "0600000US2707916119160", "655", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2702317325280", "2897", population),
    population = ifelse(year == "2016" & Id == "0600000US2702317325280", "2557", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2705317126990", "2938", population),
    population = ifelse(year == "2016" & Id == "0600000US2705317126990", "3064", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2703716327530", "22172", population),
    population = ifelse(year == "2016" & Id == "0600000US2703716327530", "22501", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2711713331760", "633", population),
    population = ifelse(year == "2016" & Id == "0600000US2711713331760", "301", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2705516933866", "4830", population),
    population = ifelse(year == "2016" & Id == "0600000US2705516933866", "4808", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2707910314336746", "4058", population),
    population = ifelse(year == "2016" & Id == "0600000US2707910314336746", "3996", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2704915734172", "5063", population),
    population = ifelse(year == "2016" & Id == "0600000US2704915734172", "5111", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2701307910339878", "39309", population),
    population = ifelse(year == "2016" & Id == "0600000US2701307910339878", "40900", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2715716943198", "111", population),
    population = ifelse(year == "2016" & Id == "0600000US2715716943198", "110", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2701304343198", "687", population),
    population = ifelse(year == "2016" & Id == "0600000US2701304343198", "616", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2702109744422", "671", population),
    population = ifelse(year == "2016" & Id == "0600000US2702109744422", "635", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2707913945808", "7321", population),
    population = ifelse(year == "2016" & Id == "0600000US2707913945808", "7534", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2703713146924", "20007", population),
    population = ifelse(year == "2016" & Id == "0600000US2703713146924", "20405", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2701310347068", "13394", population),
    population = ifelse(year == "2016" & Id == "0600000US2701310347068", "13451", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2709116548562", "131", population),
    population = ifelse(year == "2016" & Id == "0600000US2709116548562", "100", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2704115348796", "1740", population),
    population = ifelse(year == "2016" & Id == "0600000US2704115348796", "1665", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2704910951136", "3263", population),
    population = ifelse(year == "2016" & Id == "0600000US2704910951136", "3314", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2709514152522", "4698", population),
    population = ifelse(year == "2016" & Id == "0600000US2709514152522", "4785", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2712712953656", "5254", population),
    population = ifelse(year == "2016" & Id == "0600000US2712712953656", "5086", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2705317155006", "4316", population),
    population = ifelse(year == "2016" & Id == "0600000US2705317155006", "4229", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2707713555438", "151", population),
    population = ifelse(year == "2016" & Id == "0600000US2707713555438", "142", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2711116756014", "493", population),
    population = ifelse(year == "2016" & Id == "0600000US2711116756014", "497", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2700909756176", "1242", population),
    population = ifelse(year == "2016" & Id == "0600000US2700909756176", "1262", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2700305956950", "7218", population),
    population = ifelse(year == "2016" & Id == "0600000US2700305956950", "7377", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2715315962446", "2981", population),
    population = ifelse(year == "2016" & Id == "0600000US2715315962446", "2968", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2709715363778", "350", population),
    population = ifelse(year == "2016" & Id == "0600000US2709715363778", "321", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2711115967504", "4088", population),
    population = ifelse(year == "2016" & Id == "0600000US2711115967504", "4079", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2712316369970", "23797", population),
    population = ifelse(year == "2016" & Id == "0600000US2712316369970", "24992", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2700914558612", "15876", population),
    population = ifelse(year == "2016" & Id == "0600000US2700914558612", "16442", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2700312361996", "6412", population),
    population = ifelse(year == "2016" & Id == "0600000US2700312361996", "6519", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2705312356680", "8312", population),
    population = ifelse(year == "2016" & Id == "0600000US2705312356680", "8896", population),
    
    population = ifelse(year == "2010" & Id == "0600000US2700914114556896", "65842", population),
    population = ifelse(year == "2016" & Id == "0600000US2700914114556896", "66770", population)
         )



