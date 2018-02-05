setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201712")

library(data.table)

# Read all necessary files

df = fread("BFpivot3.csv", header = TRUE, stringsAsFactors = FALSE, data.table = TRUE)
df = df[, c("Period", "Subbrand", "Size", "Age", "Scent", "PIECES", "VALUE", "VOLUME", 
            "Channel", "Coef", "Correction") := NULL] # or "Correction" in a full name

# Transform to upper case in order to subset
df[, Form:=toupper(Form)]
df = df[Form == "SOLID" | Form == "PURE" | Form == "POWDER"]

# Transform other columns to upper case since file consists of different letters' cases
df[, SKU:= toupper(SKU)]
df[, PS0:=toupper(PS0)]
df[, PS2:=toupper(PS2)]
df[, PS3:=toupper(PS3)]
df[, PS:=toupper(PS)]
df[, Brand:=toupper(Brand)]
df[, Company:=toupper(Company)]
df[, PriceSegment:=toupper(PriceSegment)]
df[, Additives:=toupper(Additives)]
df[, Region:=toupper(Region)]

# Some rename either to avoid long names or mess with the same brand names 
# belonging to different companies

df[Company == "KHOROLSKII MK" & Brand == "MALYSH", Brand := "MALYSH KH"]
df[Company == "KHOROLSKII MK" & Brand == "MALYUTKA", Brand := "MALYUTKA KH"]
df[Company == "NUTRICIA" & Brand == "MALYSH ISTRINSKII", Brand := "MALYSH ISTR"]
df[Company == "ASSOCIACIYA DETSKOGO PITANIYA", Company := "ASSOCIACIYA DP"]
df[Company == "ABBOTT LABORATORIES", Company := "ABBOTT LAB"]
df[Company == "KOMPANIYA EKSTREYD", Company := "EKSTREYD"]

# SKU resegmentation

df[SKU == "NAN_PREMIUM GIPOALLERGENNYI 3_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NESTLE",
   PS := "HYPOALLERGENIC"]

df[SKU == "NAN_PREMIUM KISLOMOLOCHNYI 3_400GR_12+_KISLOMOLOCHNYI_IMF_GUM_SPECIALS_GUM SPECIALS_NESTLE",
   PS := "DIGESTIVE COMFORT"]

df[SKU == "KABRITA_GOLD 3_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_HYPROCA NUTRITION" |
     SKU == "KABRITA_GOLD 3_800GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_HYPROCA NUTRITION" |
     SKU == "NENNI_NENNI 3_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_BIBICALL" |
     SKU == "NENNI_ZOLOTAYA KOZOCHKA_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_BIBICALL",
   PS := "SOY / GOAT"]

df[SKU == "NUTRILON_PEPTI JUNIOR_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NUTRICIA" |
     SKU == "NUTRILON_PEPTI JUNIOR_450GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NUTRICIA",
   PS := "ALLERGY TREATMENT"]

df[SKU == "NUTRILON_PEPTI JUNIOR_400GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NUTRICIA" |
     SKU == "NUTRILON_PEPTI JUNIOR_450GR_12+_N/S_IMF_GUM_SPECIALS_GUM SPECIALS_NUTRICIA",
   PS2 := "IF"]

# Add Kabrita to Fruit Plus

df = df[, .(ITEMSC = sum(PIECESC), VALUEC = sum(VALUEC), VOLUMEC = sum(VOLUMEC)),
        by = .(Ynb, Mnb, Brand, PS0, PS2, PS3, PS, Company, PriceSegment, Form,
               Additives, Region)]

# Check Price Segments

df[PS0 == "IMF", .(Sales=sum(VOLUMEC), Price = sum(VALUEC)/sum(VOLUMEC)), 
   by = .(Brand, PriceSegment)][order(Brand)]

df[PS2 == "DRY FOOD", .(Sales=sum(VOLUMEC), Price = sum(VALUEC)/sum(VOLUMEC)), 
   by = .(Brand, PriceSegment)][order(-Price)]

df[PS3 == "FRUITS" | PS3 == "SAVOURY MEAL", .(Sales=round(sum(VOLUMEC)/1000), 
                                              Price = round((sum(VALUEC)/sum(VOLUMEC)),2)), 
   by = .(Brand, PriceSegment)][order(Brand)]

write.csv(df, "BFprocessed.csv")