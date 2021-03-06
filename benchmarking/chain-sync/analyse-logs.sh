#!/bin/sh

NETWORK=${1:-mainnet}

# get first block copy time
LOGFILE=`ls -1 state-node-${NETWORK}/node-0-*.log | head -3`
FIRSTSLOT=`grep -e '.*:cardano.node.ChainDB:.*TraceCopyToImmDBEvent.CopiedBlockToImmDB.*"tip":"[a-z0-9]*@' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*"tip":"[a-z0-9]*@\([0-9]\+\)".*/"\1 \2";\3/p; q;'`

LOGFILE=`ls -1r state-node-${NETWORK}/node-0-*.log | head -1`
LASTSLOT=`grep -e '.*:cardano.node.ChainDB:.*TraceCopyToImmDBEvent.CopiedBlockToImmDB.*"tip":"[a-z0-9]*@' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*"tip":"[a-z0-9]*@\([0-9]\+\)".*/"\1 \2";\3/p;' | tail -n 1`
LASTRSS=`grep -e '.*cardano.node-metrics:.*Mem.resident =' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*Mem.resident = \([0-9]\+\).*/\1 \2\t\3/p;' | { read a b c; while [ -n "$a" ]; do echo "\"$a $b\";$((c*4096))"; read a b c; done; } | tail -n 1`

# output git revision
echo -n "commit;"
git log | head -1 | cut -d ' ' -f 2
echo -n "slotfirst;"; echo $FIRSTSLOT
echo -n "slotlast;"; echo $LASTSLOT
echo -n "memorylast;"; echo $LASTRSS

