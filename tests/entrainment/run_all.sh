#!/bin/bash

GOTM=./gotm

confs="generic kepsilon komega MellorYamada"
for conf in $confs; do
   $GOTM gotm_${conf}.yaml --output_id _${conf} >& ${conf}.log
done
