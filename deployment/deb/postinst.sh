#!/usr/bin/env bash

replace_var() {
    variable_name=$1
    variable_value=$2
    file=$3
    sed -i "s/^\(.*\)\${$variable_name}\(.*\)/\1$variable_value\2/" $file
}

ip=$(${get_ip_command})
replace_var NODE_IP $ip ${config_file}

erl -name reloader@$ip -setcookie ${cookie} -noshell -eval "rpc:call('${app_name}@$ip', reloader, reload_all_changed, [])" -eval "init:stop()"