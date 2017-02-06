HW 4 Q1
================
Jiewei Chen (999 494 235)
11/29/2016

1. Load All Files
=================

1.1 Load All File Names
-----------------------

``` r
# Set Working Directory
setwd("~/Documents/UCD/Course/7. 2016 FQ/STA 141A/HW 4")

# List all the names of advertisement files.
ad.names = list.files(path = "./CarAdvert")

# check if all files are listed
length(ad.names)  # 1531 
```

    ## [1] 1531

So all files are listed here.

1.2 Function for Extracting Text in One File
--------------------------------------------

``` r
# Write a function that can work with one file.  This function can extract
# the text inside the file.
read_file_text = function(file) {
    # use readLines() function to read the plain text.
    ln = readLines(file)  # Read each lne in the text
    
    # paste thing together to make it easier to search
    tex = paste(ln, collapse = "\n")  # text
    
}
```

2. Function for Finding Information from Ads
============================================

``` r
get_car_info = function(file) {
        # Information Needed
        
        # ========================================================================
        # (i) Model (Year and Make) of the car
        # pattern of year.make.model
        # This pattern basically extracts the first line of this text, 
        #       so it starts with 4-digit year and end with (\n)
        # later "year", "make" and "model" information can be extract from "model"
        model.ptn = "[1-2][0-9]{3} .*(\n)"
        Model = str_extract(file, model.ptn)
        # extract year
        Year = str_split_fixed(Model, " ", n = 2)[1]
        # extract maker, assuming all makers only contain one word
        #       Note that there are special cases, which will be addressed later
        Make = str_split_fixed(Model, " ", n = 3)[2]
        # Capitalize each word to make it easier for grouping
        Make = str_to_title(tolower(Make)) 
        # Add to two special cases
        Make = ifelse(Make == "Excel", "Excel Peterson", 
                      ifelse(Make == "Land", "Land Rover", Make))
        # extract Model Info
        Model = str_split_fixed(Model, " ", n = 2)[2] 
        Model = gsub("\n", "", Model) # formatting
        
        # ========================================================================
        # (ii) Vehicle Identification Number (VIN)
        # VIN has 17 characters and ended with digit
        VIN.ptn = "VIN: [a-zA-Z0-9]{16}[0-9]" 
        VIN = str_extract(file, VIN.ptn)
        VIN = gsub("VIN: ", "", VIN)
        
        # ========================================================================
        # (iii) Price
        # Price starts with $ and contains digits and ","
        Pr.ptn = "\\$[0-9]{1,3}(,[0-9]{3})+"
        Price = str_extract(file, Pr.ptn)[1]
        Price = gsub("\\$", "", Price)

        # ========================================================================
        # (iv) Mileage
        # Mileage starts with word "Mileage" and followed by digits
        Ml.ptn = "Mileage: [0-9]{1,3}(,[0-9]{3})*"
        Mile = str_extract(file, Ml.ptn)
        Mile = gsub("Mileage: ", "", Mile)

        # ========================================================================
        # (v) Color (Interior and Exterior)
        # Interior color is something between "Interior:" and 
        #       "Body" or "Transmission" or "Engine"
        Int.C.ptn = "Interior: [a-zA-Z\\s]*(Body|Transmission|Engine)"
        Int.C = str_extract(file, Int.C.ptn)
        Int.C = gsub("(Interior: )|(Body|Transmission|Engine)", "", Int.C)
        Int.C = tolower(Int.C) # formatting, easy for later categorizing
        
        # Exterior color is something between "Exterior:" and 
        #       "Interior" or "Body" or "Transmission" or "Engine"
        Ex.C.ptn = "Exterior: [a-zA-Z\\s]*(Body|Interior|Transmission|Engine)"
        Ex.C = str_extract(file, Ex.C.ptn)
        Ex.C = gsub("(Exterior: )|(Body|Interior|Transmission|Engine)", "", Ex.C)
        Ex.C = tolower(Ex.C) # formatting, easy for later categorizing
        
        # ========================================================================
        # (vi) Transmission
        # Transmission is something between "Transmission:" and "Engine"
        Trans.ptn = "Transmission: [a-zA-Z0-9\\s-\\(\\)]*(Engine)"
        Trans = str_extract(file, Trans.ptn)
        Trans = gsub("(Transmission: )|(Engine)", "", Trans)
        
        # ========================================================================
        # (vii) Engine displacement (in liters)
        # Engine in liters is in the format like, "2.4 L" or "2.4L"
        Eng.ptn = "Engine: [\\d][.][\\d](\\ )?L"
        Eng = str_extract(file, Eng.ptn)
        Eng = gsub("Engine: ", "", Eng)
        Eng = gsub(" ", "", Eng) # reformating pattern like 2.4L and 2.4 L to be same

        # Here another experssion of Engine is listed in cylinder/V6/V8 format
        Eng.ptn2 = "([46]{1} (cyl))|(v[68])"
        Eng2 = str_extract(tolower(file), Eng.ptn2) # formatting
        
        # ========================================================================
        # (viii) Name of company selling the car
        # Company name is something after "Offered by: "
        Com.ptn = "Offered by: [a-zA-Z0-9\\s\\'\\&\\,]*[a-zA-Z0-9\\.]"
        Company = str_extract(file, Com.ptn)
        Company = gsub("Offered by:", "", Company)
        
        # ========================================================================
        # (ix) Street address of the company
        # Company Address is something after "Address: "
        Add.ptn = "Address: [a-zA-Z0-9\\s\\,\\-\\&]*[0-9]"
        Address = str_extract(file, Add.ptn)
        Address = gsub("Address: ", "", Address)
        
        # ========================================================================
        # (x) Phone number of the company
        Ph.ptn = "Phone: \\([0-9]{3}\\)\\s[0-9]{3}[-][0-9]{4}"
        Phone = str_extract(file, Ph.ptn)
        Phone = gsub("Phone: ", "", Phone)
        
        # ========================================================================
        # (xi) Website of the company
        # Website starts with "Website: "
        Wb.ptn = "Website: [a-zA-Z0-9\\.\\-]*(\\/|(com)|(net))"
        Website = str_extract(file, Wb.ptn)
        Website = gsub("Website: ", "", Website)

        # Combine all information into result
        result = c(Year, Make, Model, VIN, Price, Mile, 
                   Int.C, Ex.C, Trans, Eng, Eng2, Company, 
                   Address, Phone, Website)
        
        return(result)
}
```

3. Extrating Information from 1531 Ads
======================================

``` r
HW4.q1 = function() {
    library(stringr)
    paths = str_c("./CarAdvert/", ad.names)
    text = sapply(paths, read_file_text)
    All_Car_Info = sapply(text, get_car_info)
    # name every row
    row.names(All_Car_Info) = c("Year", "Make", "Model", "VIN", "Price($)", 
        "Mileage", "Interior Color", "Exterior Color", "Transmission", "Engine(L)", 
        "Engine2(Cyls)", "Company", "Address", "Phone", "Website")
    # return the result in data.frame with each row representing the information
    # of each car
    result = as_data_frame(t(All_Car_Info))
    
    # add another column showing the name of each file for future diagnosis
    result[, 15] = ad.names
    
    return(result)
    
}

# Write result into a csv file
library(tidyverse)
setwd("~/Documents/UCD/Course/7. 2016 FQ/STA 141A/HW 4")
write_csv(HW4.q1(), path = "Question1.csv", na = "NA")
```

4. Check if Info are correct
============================

``` r
Info.Cars = read_csv("Question1.csv")
# head(Info.Cars) tail(Info.Cars)
dim(Info.Cars)  # 1531   15
```

    ## [1] 1531   15

4.1 Years, Makers and Models
----------------------------

``` r
sum(is.na(Info.Cars$Year))  # no missing for 'YEAR'
```

    ## [1] 0

``` r
sum(is.na(Info.Cars$Make))  # no missing for 'Make'
```

    ## [1] 0

``` r
sum(is.na(Info.Cars$Model))  # no missing value for 'MODEL'
```

    ## [1] 0

``` r
# table Make
Make.list = table(Info.Cars$Make)
Make.list
```

    ## 
    ##          Acura           Audi            Bmw          Buick       Cadillac 
    ##             49             36             89              6             24 
    ##      Chevrolet       Chrysler          Dodge Excel Peterson           Ford 
    ##            114             23             67              1            147 
    ##            Gmc          Honda         Hummer        Hyundai       Infiniti 
    ##             23             80              8             27             77 
    ##          Isuzu         Jaguar           Jeep            Kia     Land Rover 
    ##              2              4             79             18             10 
    ##          Lexus        Lincoln          Mazda  Mercedes-Benz        Mercury 
    ##            117              6             16             62              6 
    ##           Mini     Mitsubishi         Nissan        Pontiac        Porsche 
    ##              3              9            184              5             12 
    ##            Ram           Saab          Salem         Saturn          Scion 
    ##             10              5              1              5              6 
    ##         Subaru         Suzuki         Toyota     Volkswagen          Volvo 
    ##             24              6            125             35             10

So there is no missing value in year, maker, model. And it can be seen from the Make list, there is no misspelling.

``` r
ggplot(Info.Cars, aes(x = Info.Cars$Year,fill = factor(Info.Cars$Year))) +
        geom_bar(width=.5) + ggtitle("# of Cars in Different Years") + 
        xlab("Year") + ylab("# of Cars") + 
        guides(fill = guide_legend(title = "Years")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="HW_4_Q1_files/figure-markdown_github/4.1 Year-1.png" style="display: block; margin: auto;" />

This graph above shows the number of cars in each year.

``` r
sort(table(Info.Cars$Make))
```

    ## 
    ## Excel Peterson          Salem          Isuzu           Mini         Jaguar 
    ##              1              1              2              3              4 
    ##        Pontiac           Saab         Saturn          Buick        Lincoln 
    ##              5              5              5              6              6 
    ##        Mercury          Scion         Suzuki         Hummer     Mitsubishi 
    ##              6              6              6              8              9 
    ##     Land Rover            Ram          Volvo        Porsche          Mazda 
    ##             10             10             10             12             16 
    ##            Kia       Chrysler            Gmc       Cadillac         Subaru 
    ##             18             23             23             24             24 
    ##        Hyundai     Volkswagen           Audi          Acura  Mercedes-Benz 
    ##             27             35             36             49             62 
    ##          Dodge       Infiniti           Jeep          Honda            Bmw 
    ##             67             77             79             80             89 
    ##      Chevrolet          Lexus         Toyota           Ford         Nissan 
    ##            114            117            125            147            184

``` r
ggplot(Info.Cars, aes(x = Info.Cars$Make, fill = factor(Info.Cars$Make))) + 
        geom_bar(width=.5) + ggtitle("# of Cars of Different Makers") + 
        xlab("Maker") + ylab("# of Cars") + 
        guides(fill = guide_legend(title = "Makers")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        guides(fill=FALSE)
```

<img src="HW_4_Q1_files/figure-markdown_github/4.1 Make-1.png" style="display: block; margin: auto;" />

It can be observed that Nissan has the most cars on sale. The second and third is Ford and Toyota.

4.2 VIN
-------

``` r
VIN.list = table(Info.Cars$VIN)
1531 - sum(VIN.list)  # 2
```

    ## [1] 2

``` r
Info.Cars[which(is.na(Info.Cars$VIN)), 15]
```

    ## # A tibble: 2 × 1
    ##    Website
    ##      <chr>
    ## 1 1260.txt
    ## 2  789.txt

There are two missing values in VIN, which correspond to "1260.txt", and "789.txt". After checking these two ads, it can be found that in "789.txt", the VIN number is "12345678910", which can't be the true VIN. Same problem for "1260.txt", whose VIN is "1". So it is ok to leave these two as "NA" in the final result.

4.3 Colors
----------

``` r
Int.Color.list = table(Info.Cars$`Interior Color`)
1531 - sum(Int.Color.list)  # 340
```

    ## [1] 340

``` r
Ex.Color.list = table(Info.Cars$`Exterior Color`)
1531 - sum(Ex.Color.list)  # 36
```

    ## [1] 36

4.4 Transmission
----------------

``` r
Tran.list = table(Info.Cars$Transmission)
1531 - sum(Tran.list)  # 139
```

    ## [1] 139

``` r
Info.Cars$Trans = rep("Others", 1531)
Info.Cars$Trans[agrepl("auto", tolower(Info.Cars$Transmission))] = "Auto"
Info.Cars$Trans[agrepl("manual", tolower(Info.Cars$Transmission))] = "Manual"
Info.Cars$Trans[grepl("(continuously variable)|(cvt)", tolower(Info.Cars$Transmission))] = "CVT"
table(Info.Cars$Trans)
```

    ## 
    ##   Auto    CVT Manual Others 
    ##   1241     66     39    185

``` r
ggplot(Info.Cars, aes(x = Info.Cars$Trans, fill = factor(Info.Cars$Trans))) + 
    geom_bar() + ggtitle("# of Cars with Different Transmission") + xlab("Transmission") + 
    ylab("# of Cars") + guides(fill = guide_legend(title = "Transmission")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="HW_4_Q1_files/figure-markdown_github/4.4 Transmission-1.png" style="display: block; margin: auto;" />

It can be observed that most of the cars from these ads are automatic cars.

After checking the ads where Transmission Information is missing, it can be found that in the case, where transimission is listed at the last of that paragraph, or say, no engine information is listed, the transmission information will be missing. This is because in my regular expression of "Transmission", I used "Engine" as the end motif.

4.5 Engine
----------

``` r
Eng.list = table(Info.Cars$"Engine(L)")
1531 - sum(Eng.list) # 523 Missing
```

    ## [1] 523

``` r
ggplot(Info.Cars, aes(x = Info.Cars$"Engine(L)", 
                      fill = factor(Info.Cars$"Engine(L)"))) + 
        geom_bar(width=.5) + ggtitle("# of Cars with Different Engine") + 
        xlab("Engine") + ylab("# of Cars") + 
        guides(fill = guide_legend(title = "Engine")) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        guides(fill=FALSE)
```

<img src="HW_4_Q1_files/figure-markdown_github/4.5 Engine-1.png" style="display: block; margin: auto;" />

There are in total 523 missing value for engine in L. It can be seen that most cars have engine 3.5L and 2.5L.

``` r
# According to Piazza:
#       "cyl" or "cyls" or "cylinder" are being used often as 
#       part of the description of the engine.
#       In this case, you may extract that information as 
#       description of the engine.
Eng2.list = table(Info.Cars$"Engine2(Cyls)")
1531 - sum(Eng2.list) # 524 Missing
```

    ## [1] 524

``` r
# Almost same as Engine in L 

ggplot(Info.Cars, aes(x = Info.Cars$"Engine2(Cyls)", 
                      fill = factor(Info.Cars$"Engine2(Cyls)"))) + 
        geom_bar(width=.5) + ggtitle("# of Cars with Different Engine") + 
        xlab("Engine") + ylab("# of Cars") + 
        guides(fill = guide_legend(title = "Engine")) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="HW_4_Q1_files/figure-markdown_github/4.5 Engine2-1.png" style="display: block; margin: auto;" />

There are in total 524 missing value for engine in cylinder. It can be seen that most cars have engine V6.

4.6 Company Info
----------------

``` r
Com.list = table(Info.Cars$Company)
1531 - sum(Com.list) # no missing for Company name
```

    ## [1] 0

``` r
1531 - sum(table(Info.Cars$Address)) # no missing for Company address
```

    ## [1] 0

``` r
1531 - sum(table(Info.Cars$Phone)) # no missing for company phone
```

    ## [1] 0

``` r
1531 - sum(table(Info.Cars$Website)) # no missing for company website
```

    ## [1] 0

``` r
ggplot(Info.Cars, aes(x = Info.Cars$Company, fill = factor(Info.Cars$Company))) +
        geom_bar(width=.5) + ggtitle("# of Cars Offered by Different Companies") +
        xlab("Company") + ylab("# of Cars") + 
        guides(fill = guide_legend(title = "Company")) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        guides(fill=FALSE)
```

<img src="HW_4_Q1_files/figure-markdown_github/4.6 Company-1.png" style="display: block; margin: auto;" />

There are in total 65 companies. All cars have information for Company, Address of Company, Company Phone and Company Website.
