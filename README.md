# Qtest_shinyApp


The goal of Qtest_shinyApp is to implementate shiny application to QTMB as developed in N messure it mange it SFF project


# How to add a new crop 

- New set of paras for A C B M in excel  
  A should be 0 or positive values 
- Copy and paste into `crop parameters` sheet in the mother workbook  
- Rerun `formatConversion` to have an updated sqlite3  



# Basic information

The previous tool is in Excel format coded by VB. 

To unlock the excel sheet and have the raw data, please ask the project leader for the password. 

**How to unlock and unhidden things in the Excel**  
[Alt + F11] to trigger VB.  
[Click] `sheet2(Soil parameters)` on the `Project` panel.   
[Click] `Visible` on the `Properties` panel.  
[Click] the drop down list   
[Select] the `- 1 -xlSheetVisible`  
Repeat the steps to unhidden other sheets.   
The sheet `Quick Test Mass Balance Tool` has some hidden info  
[Click] `VIEW` tag on the meum  
[Tick] `Gridlines` and `Headings`  

**Where is the dropdown list from**   
`Crop` Panel  
`System` is from the sheet `AMN Input'!$A$7:$A$9` **user selection**  
`Crop` is from `Crop parameters'!$L$3:$L$30` **user selection**  
`Target yield` is from `INDIRECT($K$10)` **user selection**  
`Planting date` **user input**  
`Estimated seasonal N uptake (kg/ha)` =VLOOKUP(K10&K12,'Crop parameters'!C3:F290,4,FALSE)  
`Seasonal N Balance` Panel  
`Soil N supply` sum of `Total` of `Mineral N supply (kg/ha)` and `Remaining ON supply (kg/ha) ` from the `Soil Nitrogen` Panel   `Remaining crop N requirement `: $$=K14-($X$12+$X$11/(1+EXP(-$X$9*($X$8-$X$10))))$$  
`Net `=TEXT(K17-K18, "0")& IF(K17-K18>=0, " (surplus)"," (deficit)")  
`Next sampling date (SD)` Panel  
`Next SD`

**user input**  
`Crop N Requirement until next SD (kg/ha)` = IF(K25="", "NA", K18-(K14-($X$12+$X$11/(1+EXP(-$X$9*($X$15-$X$10))))))  
`Net (kg/ha)` =IF(K25="","NA", K17-K26)  
`Soil Nitrogen` Panel  
Nitrate Quick Test  
Sampling Date  
Sampling depth start (cm)  
Sampling depth end (cm)  
`Soil texture` ='Soil parameters'!$J$2:$J$13  
`Soil moisture` = 'Soil parameters'!$B$2:$B$6  
Quick test nitrate (mg/L)  
Quick test nitrate-N (mg/kg DM)  
Mineral N supply (kg/ha)  


### AMN Test -----  
Test value (kg/ha)   

if test value exist:  
Remaining ON supply (kg/ha) = crop period * data supply rate - (sampling date - planting date) * datasupply rate  
else  
Remaining ON supply (kg/ha) = default supply * default supply rate - (sampling date - planting date) * default supply rate  


datasupply rate = test results * 0.9 /crop period   
datasupply rate = test results * 0.5/crop period  
datasupply rate = test results * 0.3 /crop period  
default supply rate depends on the crop period:   
over 100 days = AMN default * 0.9 / crop period   
under 40 days = AMN default * 0.3 / crop period  
in between = AMN default * 0.5 / crop period  


# Steps

1. change the format of the data. From excel to a sqlite db  
2. static graphing  
3. UI development  


# Unit Testing  

`shinytest` is used for the testing.   
[More details](https://rstudio.github.io/shinytest/articles/shinytest.html)  



<span style="font-size:smaller;">Disclaimer</span>  
<span style="font-size:smaller;">
The data and related report in the app is copyright to the Foundation for Arable Research (FAR) and may not be reproduced or copied
in any form whatsoever without FAR’s written permission. The data and related report are intended to provide accurate and
adequate information relating to the subject matters contained in it and is based on information current at the time of
report. Information contained in the report is general in nature and not intended as a substitute for specific
professional advice on any matter and should not be relied upon for that purpose. It has been prepared and made
available to all persons and entities strictly on the basis that FAR, its researchers and authors are fully excluded from
any liability for damages arising out of any reliance in part or in full upon any of the information for any purpose.
</span>
