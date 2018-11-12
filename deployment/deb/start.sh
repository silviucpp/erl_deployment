#!/usr/bin/env bash

APP=$1
export NODE_IP=$(${get_ip_command})
${APP} foreground > /dev/null 2>&1
