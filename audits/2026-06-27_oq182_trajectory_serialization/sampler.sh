#!/usr/bin/env bash
# OQ-182 mechanism witness: sample swipl subprocesses every ~0.5s, wall-clock stamped.
# Each line: <epoch.frac> PID=<pid> RSS_KB=<rss> ETIMES=<secs> ARGS=<full argv>
# Stage disambiguation is by the unique loaded module in ARGS:
#   giant_comp -> giant_component_analysis.pl ; trajectory -> context_profile_report.pl
# Usage: sampler.sh <outfile>
OUT="$1"
: > "$OUT"
while true; do
  ts=$(date +%s.%N)
  # -ww disables args-width truncation; match swipl without matching the grep itself.
  ps -eww -o pid=,rss=,etimes=,args= | grep '[s]wipl' | while IFS= read -r line; do
    echo "$ts $line" >> "$OUT"
  done
  sleep 0.1
done
