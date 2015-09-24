#!/bin/bash
# This script loads all the systems in cl-ana individually and reports
# errors.
if [[ $# -ne 1 ]] ; then
    echo "Usage: load-cl-ana.sh <path_to_cl-ana>"
    echo ""
    echo "This script loads all cl-ana systems and reports errors"
    exit 1
fi
clana_path=$1
system_files="$clana_path/*/*.asd"
systems=$(for sf in $system_files; do
              basename $sf .asd
          done)

# systems="cl-ana.pathname-util"

i=0
declare -a retval
for s in $systems; do
    echo "Loading $s"
    sbcl --noinform --eval "(handler-case (require '$s) (error (err) (print err) (progn (exit :code 1))))" --eval '(exit :code 0)'
    retval[$i]=$?
    i=$((i+1))
done
i=0
no_errors=0;
for s in $systems; do
    if [[ ${retval[$i]} -ne 0 ]]; then
        echo "Error loading $s"
        no_errors=1;
    fi
done
if [[ $no_errors -eq 0 ]] ; then
    echo "SUCCESS"
else
    echo "FAILURE"
fi
