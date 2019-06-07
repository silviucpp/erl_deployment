#!/usr/bin/env bash

replace_var() {
    variable_name=$1
    variable_value=$2
    file=$3
    sed -i "s/^\(.*\)\${$variable_name}\(.*\)/\1$variable_value\2/" $file
}

${multi_dc_config_replace}

ip=$(${get_ip_command})
replace_var NODE_IP $ip ${config_file}

for d in ${app_home}/erts-*/bin; do PATH="$PATH:$d"; done

${reload_command}
