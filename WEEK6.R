# 14----
# 读txt 跳过空行
examTextRaw <- read_lines("EDAdata/stat101.txt",
                          skip_empty_rows = TRUE)

str_which(examTextRaw, "student") #包含student的行号
str_which(examTextRaw, "\\*")

str_replace_all(examTextRaw, pattern = "--",
                replacement = "NA")
str_remove_all(examTextRaw, pattern = "\\*")#删除某文本可以用这种方法或者在上面的方法中NA换成空

str_trim(examTextRaw)

str_sub(examTextRaw[1], start = 1, end = 7) #特定位置搜索文本

examTextClean <- examTextRaw %>%
  str_remove_all(pattern = "\\*") %>%
  str_replace_all(pattern = "--",
                  replacement = "NA") %>%
  str_trim() #删除字符串开头/结尾的空白

header <- str_which(examTextClean, pattern = "student")
endLine <- str_which(examTextClean, pattern = "denotes")
read_table(examTextClean[header:(endLine - 1)])


# 15----

examTextRaw <- read_lines("EDAdata/stat101.txt")

examTextClean <- examTextRaw %>%
  str_remove_all(pattern = "\\*") %>%
  str_replace_all(pattern = "--",
                  replacement = "NA") %>%
  str_trim()

header <- str_which(examTextClean, pattern = "student")
endLine <- str_which(examTextClean, pattern = "denotes")

read_table(examTextClean[header:(endLine - 1)]) %>%
  mutate(module = "stat101")
# clean 两个文本文件，上面是第一个，下面用循环做
modules <- c("stat101", "stat102")
paste0("EDAdata/", modules, ".txt")

moduleResults <- vector(mode = "list", length = length(modules))
moduleResults

# 用循环clean
modules <- c("stat101", "stat102")
filePaths <- paste0("EDAdata/", modules, ".txt")
moduleResults <- vector(mode = "list", length = length(modules))

for(i in 1:length(modules)){
  examTextRaw <- read_lines(filePaths[i])
  
  examTextClean <- examTextRaw %>%
    str_remove_all(pattern = "\\*") %>%
    str_replace_all(pattern = "--",
                    replacement = "NA") %>%
    str_trim()
  
  header <- str_which(examTextClean, pattern = "student")
  endLine <- str_which(examTextClean, pattern = "denotes")
  
  moduleResults[[i]] <- 
    read_table(examTextClean[header:(endLine - 1)]) %>%
    mutate(module = modules[i])
}

do.call(rbind.data.frame, moduleResults)

# Exercise 15.3 ----
modules <- c("stat101", "stat102", "stat103")
filePaths <- paste0("EDAdata/", modules, ".txt")
moduleResults <- vector(mode = "list", length = length(modules))

for(i in 1:length(modules)){
  examTextRaw <- read_lines(filePaths[i])
  
  examTextClean <- examTextRaw %>%
    str_remove_all(pattern = "\\*") %>%
    str_replace_all(pattern = "--",
                    replacement = "NA") %>%
    str_trim()
  
  header <- str_which(examTextClean, pattern = "student")
  endLine <- str_which(examTextClean, pattern = "NA")
  
  moduleResults[[i]] <- 
    read_table(examTextClean[header:endLine]) %>%
    mutate(module = modules[i])
}

do.call(rbind.data.frame, moduleResults)

# homework
TextRaw <- read_lines("EDAdata/sheffielddata.txt")

TextClean <- TextRaw %>%
  str_remove_all(pattern = "Provisional") %>%
  str_remove_all(pattern = "degC") %>%
  str_remove_all(pattern = "days") %>%
  str_remove_all(pattern = "mm") %>%
  str_remove_all(pattern = "hours") %>%
  str_replace_all(pattern = "---",
                  replacement = "NA") %>%
  str_trim()

header <- str_which(TextClean, pattern = "yyyy")
endLine <- str_which(TextClean, pattern = "52.9#")

newtable <- read_table(TextClean[header:endLine])
colnames(newtable)[2] <-"mm"
newtable
tail(newtable)

