#!/bin/sh

confs="generic kepsilon komega MellorYamada"
GOTM=/home/kb/local/gcc/8/gotm/5.3/bin/gotm

for conf in $confs; do
   $GOTM ${conf}_gotm.yaml --output_id _$conf >& $conf.log
done
