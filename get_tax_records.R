library('XML')
library('stringr')

#function to clean currency values to numerics
clean <- function (x) as.numeric(str_extract(gsub("\\$|,|^\\s+|\\s+$", "", x), paste(c('[0-9]', rep(".", str_count(x, '[0-9]')-1)), sep='', collapse='')))

#create dataframe to hold the owner/property info

prop_names = c('owner_names', 'location_address', 'property_class', 'parcel', 'neighborhood', 'homestead', 
              "cur_val", "prev_val", 
              "style", "heated_sqft", "interior", "exterior", "attic_sqft", "bsmt_sqft", "year_built", "roof", "flooring", "heat_type", "condition", "fireplaces")
sale_names = c('parcel', 'sale_date', 'deed_book', 'plat_book', 'sale_price', 'reason', 'grantor', 'grantee')
prop_info = as.data.frame(setNames(replicate(20, 'x', simplify = F), prop_names), stringsAsFactors = F)
sale_info = as.data.frame(setNames(replicate(8, 'x', simplify = F), sale_names), stringsAsFactors = F)

for (i in 1:513) {

# load the information for a given property

url = paste("http://qpublic7.qpublic.net/ga_display.php?county=ga_coweta&KEY=073D++++++", formatC(i, width=3, format="d", flag="0"), sep="")
doc = htmlTreeParse(url, useInternal=T)
tables = readHTMLTable(doc, header=T, stringsAsFactors=F)

#test to see if there is a valid entry for this parcel id - if not, skip to the next parcel
if (length(tables) == 1) {next}

#get the owner/property info for the given property
op_table = as.data.frame(tables[3], stringsAsFactors = F)
field_values = c(op_table$NULL.V2[c(1, 4, 6)], op_table$NULL.V4[c(2, 6, 7)])
parcel = op_table$NULL.V4[2]

#get the tax values for the given property
val_table = as.data.frame(tables[4], stringsAsFactors = F)
field_values = append(field_values, c(clean(val_table$NULL.V4[2]), clean(val_table$NULL.V5[2])))

#get improvement values for the given property
imp_table = as.data.frame(tables[6], stringsAsFactors = F)
field_values = append(field_values, c(imp_table$NULL.V1[2], try(clean(imp_table$NULL.V2[2]), silent=T), imp_table$NULL.V3[2], imp_table$NULL.V4[2], try(clean(imp_table$NULL.V5[2]), silent=T), try(clean(imp_table$NULL.V6[2]), silent=T), imp_table$NULL.V5[2], imp_table$NULL.V7[2], imp_table$NULL.V1[4], imp_table$NULL.V2[4], imp_table$NULL.V3[4], imp_table$NULL.V6[4], try(clean(imp_table$NULL.V7[4]), silent=T)))

#add the completed record to the prop_info data frame
prop_info = rbind(prop_info, field_values)

#populate the sale_info data.frame
sale_table = as.data.frame(tables[8], stringsAsFactors = F)
sale_table = sale_table[-1,]
sale_parcel = rep(parcel, nrow(sale_table))
sale_table = cbind(sale_parcel, sale_table)
names(sale_table) = sale_names

#add the sales info for the parcel to the sale_info data frame
sale_info = rbind(sale_info, sale_table)

#Wait a while so we don't get our IP blocked!
Sys.sleep(2)

}

#remove the initial junk record in the sale_info data frame and convert the sales values to numbers
sale_info = sale_info[-1,]
sale_info$sale_price = sapply(sale_info$sale_price, clean)
