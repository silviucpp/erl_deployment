#!/usr/bin/env bash

APP=$1
export NODE_IP=$(ip route get 8.8.8.8 | awk 'NR==1 {print $NF}')
${APP} foreground > /dev/null 2>&1
