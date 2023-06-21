#!/bin/bash

echo 1 > /proc/sys/net/ipv4/ip_forward

iptables -F
iptables -t nat -F
iptables -X

export PORT=1766
export LOCAL=127.0.0.1
export DEST=10.0.0.122
iptables -t nat -A PREROUTING -p tcp --dport $PORT -j DNAT --to-destination $DEST:$PORT
iptables -t nat -A POSTROUTING -p tcp -d $DEST --dport $PORT -j SNAT --to-source $LOCAL

export PORT=1766
export LOCAL=0.0.0.0
export DEST=10.0.0.122
iptables -t nat -A PREROUTING -p tcp --dport $PORT -j DNAT --to-destination $DEST:$PORT
iptables -t nat -A POSTROUTING -p tcp -d $DEST --dport $PORT -j SNAT --to-source $LOCAL


iptables -t nat -L -n -v


