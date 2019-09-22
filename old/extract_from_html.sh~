# extract_words.sh
#
# USAGE: bash extract_words.sh  (use bash, NOT sh)
#
# bash script to extract lists of vision words from all html files in a directory (one term per line) and save to text files with similar names. 
# begun 2016.03.13 by adc

dirName=submissions;

## file names will include the parent directory and a forward slash
fileNames=`ls $dirName/*.html`;

## uncomment this line for testing/debugging:
# echo $fileNames; 

## uncomment this line for testing/debugging:
# for f in $fileNames
# do echo $f
# done


for f in $fileNames
do pTagConts=$(sed  -n 's/<p>\([^>]*\)<\/p>/\1/p' $f);
echo "$f"
lowerCase=$(echo "$pTagConts" | tr '[:upper:]' '[:lower:]');
noEmptyLines=$(echo "$lowerCase" | grep -v '^$');
noLinesOfSpaces=$(echo "$noEmptyLines"| sed -n '/^\s*$/ !p');
noLeadSpaces=$(echo "$noLinesOfSpaces" | sed 's/^\s*//');
noTrailSpaces=$(echo "$noLeadSpaces" | sed 's/\s*$//');
#can't figure out why spaces still appear at ends, so for good measure:
#doesn't seem to help, though
echo "$noTrailSpaces"
#write new files
echo "$noTrailSpaces" > ${f/html/txt};

done






