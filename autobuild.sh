#!/bin/bash

# Autobuild script for OS X and Ubuntu. Requires terminal-notifier and fswatch in OS X, inotify-tools and libnotify-bin in Ubuntu

if [ "$(uname)" == "Linux" ]; then
    WATCH="inotifywait -re modify ."
else
    WATCH="fswatch -v -1 src/*.ml Makefile src/*.mli"
fi

while : ; do 
    echo $WATCH
    $($WATCH) 
    #make clean 
    (make 2> make_output.tmp) && \
    (echo "Compiles" | terminal-notifier -title "make: success" -group make ) || \
    (cat make_output.tmp | terminal-notifier -title "make: failed" -group make; cat make_output.tmp )
    sleep 1
done
