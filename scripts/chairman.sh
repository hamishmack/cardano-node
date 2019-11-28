#!/usr/bin/env bash

[ $# -ne 1 ] && echo "Usage: $(basename $0) TargetSocketFilePath" 1>&2 && exit 1

SOCKET=$1

set -e

. $(dirname $0)/lib-node.sh

CMD=`find dist-newstyle/ -type f -name "cardano-node"`
test -n "${CMD}" || {
        cabal v2-build exe:cardano-node || {
                echo "ERROR: couldn't find or cabal v2-build exe:cardano-node" >&2
                exit 1
        }
        CMD=`find dist-newstyle/ -type f -name "cardano-node"`
}

set -x
exec cabal v2-run exe:chairman -- \
                                --core-node-id 0 --core-node-id 1 --core-node-id 2 \
                                -k 10 -s 250 \
                                -t 1000 \
                                --genesis-file "${genesis_file}" \
                                --genesis-hash "${genesis_hash}" \
                                --socket-path "$SOCKET" \
                                --config "configuration/log-config-0.yaml"
