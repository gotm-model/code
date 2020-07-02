#!/bin/sh

confs="generic kepsilon komega MellorYamada"
GOTM=./gotm

for conf in $confs; do
   $GOTM ${conf}_gotm.yaml --output_id _$conf >& $conf.log
done
