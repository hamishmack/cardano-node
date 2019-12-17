#!/usr/bin/env bash

#set -e

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

# start a tmux session:
# tmux new-session -s 'Demo' -t demo
if [ -z "${TMUX}" ]; then
  echo "can only be run under 'tmux' control."
  exit 1
fi

CMD="cabal v2-run --"

# VERBOSITY="--tracing-verbosity-minimal"
# VERBOSITY="--tracing-verbosity-normal"
VERBOSITY="--tracing-verbosity-maximal"

# EXTRA=""
EXTRA="
  --trace-block-fetch-decisions
  --trace-block-fetch-client
  --trace-block-fetch-server
  --trace-tx-inbound
  --trace-tx-outbound
  --trace-local-tx-submission-server
  --trace-mempool
  --trace-forge
  --trace-chain-sync-protocol
  --trace-block-fetch-protocol
  --trace-tx-submission-protocol
  --trace-local-chain-sync-protocol
  --trace-local-tx-submission-protocol
"

BASEPATH=$(realpath $(dirname $0))

genesis_root=${BASEPATH}/configuration/latest-genesis
genesis_file=${genesis_root}/genesis.json
genesis_hash=$(cat ${genesis_root}/GENHASH)


### prep cli arguments

function nodecfg () {
        sed -i 's/^GenesisHash: .*$/GenesisHash: '${genesis_hash}'/' configuration/log-config-${1}.yaml
        printf -- "--config configuration/log-config-${1}.yaml "
}
function dlgkey () {
        printf -- "--signing-key ${genesis_root}/delegate-keys.%03d.key " "$1"
}
function dlgcert () {
        printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json " "$1"
}
function commonargs() {
        printf -- "--topology configuration/realPBFT/simple-topology-real-pbft-node-$1.json "
        printf -- "--database-path ./db/ "
        printf -- "--genesis-file ${genesis_file} "
        printf -- "--genesis-hash ${genesis_hash}"
        printf -- "--socket-path /tmp/cluster3nodes-socket/$1 "
}

function nodeargs () {
        local extra="$2"
        commonargs $1
        nodecfg $1
        dlgkey $1
        dlgcert $1
        printf -- "${extra} "
        printf -- "--port $((3000 + $1)) "
}

# create tmux panes
tmux split-window -v
tmux split-window -h
tmux select-pane -t 0

# start nodes
tmux select-pane -t 0
tmux send-keys "cd '${BASEPATH}'; ${CMD} exe:cardano-node run $(nodeargs 0 "$(echo -n ${EXTRA})") " C-m
tmux select-pane -t 1
tmux send-keys "cd '${BASEPATH}'; ${CMD} exe:cardano-node run $(nodeargs 1 "$(echo -n ${EXTRA})") " C-m
tmux select-pane -t 2
tmux send-keys "cd '${BASEPATH}'; ${CMD} exe:cardano-node run $(nodeargs 2 "$(echo -n ${EXTRA})") " C-m
