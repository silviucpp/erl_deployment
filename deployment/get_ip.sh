#!/usr/bin/env bash

OS=$(uname -s)

case $OS in
    Linux)
        ip route get 8.8.8.8 | awk 'NR==1 {print $NF}'
    ;;

    Darwin)
        ifconfig $(route get 8.8.8.8 | grep "interface: " | sed "s/[^:]*: \(.*\)/\1/") | grep "inet " | sed "s/.*inet \([0-9.]*\) .*/\1/"
    ;;

    *)
        echo "Failed to get the IP"
        exit 1
esac