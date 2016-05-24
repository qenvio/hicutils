#!/usr/bin/env bash

# help message

show_help() {

	cat << EOF

	Usage: ${0##*/} [options] <in.bam> <chr:start-end>|<chr>

	Get a contact matrix from a hic-bam file. Only cis contacts are reported.

            -h          display this help and exit
            -f INT      only include reads with all FLAG bits set in INT [0]
            -F INT      only include reads with none of the FLAG bits set in INT [783]
            -w INT      size of the window in bp (resolution) [100000]

EOF

}


# get arguments

filterin=0
filterex=783
resolution=100000

while getopts "hf:F:w:" opt
do

    case "$opt" in
        h)
            show_help
            exit 0
            ;;
        f)  filterin=$OPTARG
            ;;
        F)  filterex=$OPTARG
            ;;
        w)  resolution=$OPTARG
            ;;
        '?')
            show_help >&2
            exit 1
            ;;

    esac

done

shift "$((OPTIND-1))"

if [[ $# -eq 0 ]]
then
	
    show_help >&2
    exit 1
	
fi	

inbam=$1
region=$2

# get coordinates

info=($(echo $region | tr ":-" " "))

chromosome=${info[0]}
n=${#info[@]}

if [[ "$n" == 1 ]]
then

        start=0
        end=1000000000

else

        start=${info[1]}
        end=${info[2]}

fi

# ensure cis-contacts

if [[ $filterex -lt 1024 ]]
then

	filterex=$((filterex+1024))

fi

# make contact matrix

samtools view -f $filterin -F $filterex $inbam $region | \
    awk -v w=$resolution -v OFS="\t" -v s=$start -v e=$end \
        '$8 >= s && $8 <= e{i = int($4 / w) * w; j = int($8 / w) * w; a[$3 OFS i OFS $3 OFS j] += 1}END{for(k in a) print k, a[k]}'
