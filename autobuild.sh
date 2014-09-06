#!/bin/sh

# Autobuild script for OS X. Requires terminal-notifier and fswatch.

while : ; do 
    fswatch -v -1 *.ml Makefile *.mli
    make clean 
    (make 2> make_output.tmp) && \
    (echo "Compiles" | terminal-notifier -title "make: success" -group make ) || \
    (cat make_output.tmp | terminal-notifier -title "make: failed" -group make; cat make_output.tmp )
    sleep 1
done
