# OQ-77 kill-condition runs — return-code table

Invocation under test (identical to `run_pipeline.py:416` `_prolog_giant_comp`, modulo corpus_path overlay for archive arms):

```
swipl -l stack.pl -l giant_component_analysis.pl -g "run_giant_component_analysis, halt."
```

cwd = `prolog/`. Live corpus at run time: n=39 (manifest `pipeline_run_at` 2026-06-09T23:19:26Z,
commit 0deb2114 dirty) — same n as the crashing leiden run of 2026-06-06 (n=39).

## Arm 1 — serial, live corpus n=39 (the kill-condition arm)

| run | rc | stdout lines | output vs run 1 |
|-----|----|--------------|-----------------|
| 1   | 0  | 288          | — (baseline; 0.44s wall, 13.1 MB maxRSS) |
| 2–10| 0  | 288 each     | byte-identical (diff -q) |

10/10 rc=0. All outputs byte-identical → deterministic at this corpus.

## Arm 2 — 12 concurrent co-resident processes, live corpus n=39

12 simultaneous identical invocations (12-core box, 7.7 GB RAM, WSL2).

rcs: 1:0 2:0 3:0 4:0 5:0 6:0 7:0 8:0 9:0 10:0 11:0 12:0 — 12/12 rc=0,
all 12 outputs byte-identical to Arm 1 run 1 (diff -q).

Pure co-residency (CPU/memory contention) does NOT reproduce the segfault.

## Arm 3 — serial, archived corpora (topology/stack-depth stressors)

corpus_path overlaid pre-run: `retract(config:param(corpus_path,_)), assertz(config:param(corpus_path,'<archive>'))`.

| corpus | n | run | rc | elapsed | maxRSS |
|--------|---|-----|----|---------|--------|
| archives/datasets/kernel_v1 | 1106 | 1 | 0 | 6.04s | 66.1 MB |
| archives/datasets/original_v6 | 3380 | 1 | 0 | 5:58.12 | 198.4 MB |
| archives/datasets/original_v6 | 3380 | 2 | 0 | 6:07.37 | 198.3 MB |
| archives/datasets/original_v6 | 3380 | 3 | (not captured*) | ~6 min | — |

\* run 3's rc line was lost: the wrapper shell was killed by the harness's 10-minute
command timeout mid-run-3; the swipl process completed as an orphan. Witness of clean
completion: `oq77_v6_3.out` is 18,159 bytes, ends with the `*End of giant component
analysis*` marker, and is **byte-identical** (diff) to runs 1 and 2 (both rc=0) — a
segfault would have truncated it. All three v6 outputs byte-identical → deterministic
at n=3380. The v6 report's largest component is 8,785 nodes (BFS over a component
~2.6× the corpus size — see phantom-node side-finding, writeup §5 — and a far deeper
traversal than any n=39 corpus could present).

## Detection-channel note (probe positive control)

The probe's detection channel is the process return code — the same channel that
caught the original crash (`rc=-11` via `subprocess`, = 139 in shell). A recurrence
would be caught by construction; no separate positive control needed for the channel.

## Raw files

- `oq77_serial_1.{out,err}` — representative serial run (full report + stderr)
- `oq77_serial_10.out` — last serial run (byte-identical to run 1)
- `oq77_kernelv1_1.err` — kernel_v1 run stderr tail (timing, load count)
- `phantom_node_probe.txt` — side-finding probe (see writeup §4)
