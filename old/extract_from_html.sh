# extract_from_html.sh 
# 2016.09.03 by adc
# For PSYC 4114 Cognitive Psychology Fall 2016
# In-class assignment #2: color
#
# Modified from:
# extract_words.sh
#
# USAGE: bash extract_words.sh  (use bash, NOT sh)
#
# bash script to extract lists of vision words from all html files in a directory (one term per line) and save to text files with similar names. 
# begun 2016.03.13 by adc

DIR_NAME=submissions;

## file names will include the parent directory and a forward slash
FILE_NAMES=`ls $DIR_NAME/*.html`;

BIG_FILE="allData.csv";

## uncomment this line for testing/debugging:
# echo $FILE_NAMES; 

## uncomment this line for testing/debugging:
# for f in $FILE_NAMES
# do echo $f
# done


for f in $FILE_NAMES

# For some reason several of the lines include html <span> tags in addition to <p> tags.
# E.g. these successive lines:
#
# <p>Orange (25 100 100)</p>
# <p>Yellow <span>(58 100 100)</span></p>

do echo "$f"

# remove any existing text files, because command below uses append (">>").
rm ${f/html/txt}

# use awk to test each line separately
# Followed by one monstrously long string of sed commands
# Penultimate command removes any meta-characters (any groups of consecutive non-alphanumerica chars replaced with one comma.
awk '/<p>/' $f | sed 's/<p>\(.*\)<\/p>/\1/'  | sed 's/<span>\(.*\)<\/span>/\1/' | sed 's/[()]/ /g' | sed 's/[^\([:alpha:]|0-9\)]\+/,/g' | sed 's/,$//' >> ${f/html/txt}

done


# Now re-read the newly written files and concatenate their modified contents into one big file
# Yes, kludgy that I didn't make this part of the first loop above


# To check whether all files have same number of lines first:
# for f in $(ls ./submissions/*.txt); do wc -l $f; done
#
# To see id # as well:
# counter=1; for f in $(ls ./submissions/*.txt); do echo "   $counter"; wc -l $f; let "counter=counter+1"; done
#
# This is worth doing because some extra lines will get printed for some students, e.g. if they pasted the line of dashes into their answers.  Also good for finding answers with no text in them. 
# These kinds of problems should be fixed by hand, by editing the BIG_FILE.

# delete any existing version of the file
rm $BIG_FILE

# Print a header row for R:
echo "id,colorName,HUE,SAT,VAL" >> $BIG_FILE


counter=1;

for f in $FILE_NAMES
do echo "$f"

awk -v id="$counter" '{print "id"id","$0}' ${f/html/txt} >> $BIG_FILE

let "counter=counter+1"

done


