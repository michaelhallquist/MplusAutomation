###     test variable names           ###
### test0     F1-F10...               ###  Pass
### test1     F1_1-F1_10...           ###  Pass
### test2     F1_1_1-F1_10...         ###  Pass
### test3     F1_1-F1_7,F26,F27,F28...###  Pass
### test4     Fa-Fj...                ###  Pass
### test5     F_a-F_j...              ###  Pass
### test6     F_1_a-F_1_j...          ###  Pass
### test7     Faa-Faj                 ###  Pass




Groups <- unique(fit$parameters$unstandardized$Group)
### Read dataset in the targetfile
TargetFile <- {
  if (grepl("\\.dat$",fit$input$data$file) == T){
    read.table(fit$input$data$file)
  }
  else {
    read.csv(fit$input$data$file,header = F, sep = ",")
  }
}

## extract mplus variable names 
## need test more variable names...
ExpandColname <-{
  MplusVarNames = toupper(fit$input$variable$names)
  
  #### split Mplusvarnames into vector sep = ", or space"
  
  MplusVarNamescomma = unlist(strsplit(MplusVarNames,",|\\s+"))
  #### split Mplusvarnames into vector furthe sep = "-"
  
  MplusVarNameshyphen = strsplit(MplusVarNamescomma,"-")
  
  #### extract variable numbers in mplus, eg. u1-u6 or ua - uf returns c(1,6) or c("a","f")
  ExtractedNumber <-{
    PreExtractedNumberList = list()
    ExtractedNumberList = list()
    for (i in 1:length(MplusVarNamescomma)){
      PreExtractedNumberList[[i]] =  stringr::stri_sub(MplusVarNameshyphen[[i]],-1,-1) 
    }
    for (i in 1:length(PreExtractedNumberList)){
      if (PreExtractedNumberList[[i]][1]%in%LETTERS){
        ExtractedNumberList[[i]] = stringr::stri_sub(MplusVarNameshyphen[[i]],-1,-1)
      } else{
        ExtractedNumberList[[i]] = as.numeric(unlist(stringi::stri_extract_last_regex(MplusVarNameshyphen[[i]], "\\d+")))
      }
    }
    for (i in 1:length(MplusVarNamescomma)){
      if (length(ExtractedNumberList[[i]]) <=1){
        ExtractedNumberList[[i]] = MplusVarNamescomma[i]
      }
      else{
        ExtractedNumberList[[i]] = ExtractedNumberList[[i]]
      }
    }
    ExtractedNumberList
  }
  
  #### extract variable letters in mplus, eg. u1-u6 returns "u"
  ExtractedLetter <-{
    ExtractedLetterList = list()
    for(i in 1:length(MplusVarNamescomma)){
      if (length(ExtractedNumber[[i]]) <= 1){
        ExtractedNumber[[i]] = ExtractedNumber[[i]]
      }else{
        if(is.character(ExtractedNumberList[[i]])){
          ExtractedLetterList[[i]] = unique(stringr::str_extract(MplusVarNameshyphen[[i]], "\\w+(?=\\w{1}$)"))
        }
        else{
          ExtractedLetterList[[i]] = unique(stringr::str_extract(MplusVarNameshyphen[[i]], "\\w+(?=\\d{1}$)"))
        }
      }
    }
    ExtractedLetterList
  }
  #### create column names vector eg: u1-u3 returns u1,u2,u3
  ColumnNames <- {
    Columnnames = list()
    for (i in 1:length(ExtractedNumber)){
      if (length(ExtractedNumber[[i]]) <=1){
        Columnnames[i] = ExtractedNumber[[i]][1]
      } else{
        if(is.character(ExtractedNumber[[i]])){
          Columnnames[[i]] = paste0(ExtractedLetter[[i]][1],LETTERS[which(LETTERS%in%ExtractedNumber[[i]][1]):which(LETTERS%in%ExtractedNumber[[i]][2])])
        }else{
          Columnnames[[i]] = paste0(ExtractedLetter[[i]][1],ExtractedNumber[[i]][1]:ExtractedNumber[[i]][2])
        }
      }
      
    } 
    unlist(Columnnames)
  }
}
#### Selected Variable in mplus
SMplusVarNames = toupper(fit$input$variable$usevariables)
#### split Mplusvarnames into vector sep = ","
SMplusVarNamescomma = unlist(strsplit(SMplusVarNames,",|\\s+"))
#### split Mplusvarnames into vector furthe sep = "-"
SMplusVarNameshyphen = strsplit(SMplusVarNamescomma,"-")
#### Indentify Group name for the data set
Groupname =  toupper(unlist(strsplit(unlist(fit$input$variable$grouping)," "))[1])

Selected <- {
  if(SMplusVarNamescomma == "ALL"){
    ExpandColname
  } else{
    SelectedALL <- {
      SExtractedNumber <-{
        SPreExtractedNumberList = list()
        SExtractedNumberList = list()
        for (i in 1:length(SMplusVarNamescomma)){
          SPreExtractedNumberList[[i]] =  stri_sub(SMplusVarNameshyphen[[i]],-1,-1) 
        }
        for (i in 1:length(SPreExtractedNumberList)){
          if (SPreExtractedNumberList[[i]][1]%in%LETTERS){
            SExtractedNumberList[[i]] = stri_sub(SMplusVarNameshyphen[[i]],-1,-1)
          } else{
            ## I'm not good at regrex...this is what i can think...
            SExtractedNumberList[[i]] = as.numeric(unlist(stri_extract_last_regex(SMplusVarNameshyphen[[i]], "\\d+")))
          }
        }
        for (i in 1:length(SMplusVarNamescomma)){
          if (length(SExtractedNumberList[[i]]) <=1){
            SExtractedNumberList[[i]] = SMplusVarNamescomma[i]
          }
          else{
            SExtractedNumberList[[i]] = SExtractedNumberList[[i]]
          }
        }
        SExtractedNumberList
      }
      
      #### extract variable letters in mplus, eg. u1-u6 returns "u"
      SExtractedLetter <-{
        SExtractedLetterList = list()
        for(i in 1:length(SMplusVarNamescomma)){
          if (length(SExtractedNumber[[i]]) <= 1){
            SExtractedNumber[[i]] = SExtractedNumber[[i]]
          }else{
            if(is.character(SExtractedNumberList[[i]])){
              SExtractedLetterList[[i]] = unique(stringr::str_extract(SMplusVarNameshyphen[[i]], "\\w+(?=\\w{1}$)"))
            }
            else{
              SExtractedLetterList[[i]] = unique(stringr::str_extract(SMplusVarNameshyphen[[i]], "\\w+(?=\\d{1}$)"))
            }
          }
        }
        SExtractedLetterList
      }
      #### create column names vector eg: u1-u3 returns u1,u2,u3
      SColumnNames <- {
        SColumnnames = list()
        for (i in 1:length(SExtractedNumber)){
          if (length(SExtractedNumber[[i]]) <=1){
            SColumnnames[i] = SExtractedNumber[[i]][1]
          } else{
            if(is.character(SExtractedNumber[[i]])){
              SColumnnames[[i]] = paste0(SExtractedLetter[[i]][1],LETTERS[which(LETTERS%in%SExtractedNumber[[i]][1]):which(LETTERS%in%SExtractedNumber[[i]][2])])
            }else{
              SColumnnames[[i]] = paste0(SExtractedLetter[[i]][1],SExtractedNumber[[i]][1]:SExtractedNumber[[i]][2])
            }
          }
          
        } 
        unlist(SColumnnames)
      }
    }
  }
  }
#### Grep Group indicator in the dataframe
GroupIndicator = unlist(regmatches(fit$input$variable$grouping,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",
                                                                        fit$input$variable$grouping)))
Indicators = as.numeric(unique(GroupIndicator))
#### name dataset
colnames(TargetFile) = ExpandColname
#### subset dataset according SelectedColname
if(Groupname%in%Selected){
  TargetFileSubset = subset(TargetFile,select = Selected)
}else{
  TargetFileSubset = subset(TargetFile,select = c(Selected,Groupname))}
##### subset dataset lists
SeperatedFileList <- {
  #### Create datalist 
  DataList = list()
  for (i in 1:length(Indicators)){
    DataList[[i]] = TargetFileSubset[which(TargetFileSubset[,which(colnames(TargetFileSubset) %in% Groupname)] == i),][-which(colnames(TargetFileSubset) %in% Groupname)]
  }  
  names(DataList) = Groups
  DataList
}

