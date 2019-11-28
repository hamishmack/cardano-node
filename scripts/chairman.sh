#!/usr/bin/env bash

[ $# -ne 1 ] && echo "Usage: $(basename $0) TargetSocketFilePath" 1>&2 && exit 1

SOCKET=$1

set -e

. $(dirname $0)/lib-node.sh

CMD=`find dist-newstyle/ -type f -name "cardano-node"`
test -n "${CMD}" || {
        cabal new-build exe:cardano-node || {
                echo "ERROR: couldn't find or cabal new-build exe:cardano-node" >&2
                exit 1
        }
        CMD=`find dist-newstyle/ -type f -name "cardano-node"`
}

set -x
exec cabal new-run exe:chairman -- \
                                --core-node-id 0 --core-node-id 1 --core-node-id 2 \
                                -k 10 -s 250 \
                                -t 1000 \
                                --genesis-file "${genesis_file}" \
                                --socket-path "$SOCKET" \
                                --config "configuration/log-config-0.yaml"
