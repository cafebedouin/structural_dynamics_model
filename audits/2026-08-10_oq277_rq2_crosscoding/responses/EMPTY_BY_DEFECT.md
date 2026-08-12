# This directory is empty BY DEFECT, not because the phase was never reached

**The absence in this directory is the artifact.** It is the record of the 2026-08-11 live run,
and it is the most expensive event in OQ-277's arc.

## What happened

The run was authorized at the frozen preregistration (`a7327e33`, md5 `4118f64e`, 73 items / 219
calls). It **made all 219 calls and wrote zero response files.** The driver had no code path that
persisted responses at all. Every gate it passed was an *input* gate — count captured payloads,
count fixtures, sweep for leaks — so the run reported green on every check, printed its expected
totals, and produced nothing. Commit: `edc90409`. Discipline minted from it: `CLAUDE.md` /
`build_discipline.md` → *Gate the output, not only the input*.

## Why this file exists

**Git does not track empty directories.** Without this file the directory does not exist in a
fresh clone, and a cold reader cannot distinguish

- *the run happened and persisted nothing* (what occurred), from
- *this audit never reached the response phase* (what an absent directory looks like).

That is measured-empty versus didn't-look collapsing to one observation at the read site — Build
Discipline Pattern 6 — committed by the version-control layer, on the audit whose subject is that
collapse. The marker carries the framing to the read site, which is the repair the arc's own
findings prescribe.

## Where the surviving evidence is

| what | where |
|---|---|
| The 221 captured payloads (the input side, which DID persist) | `../payloads/` |
| The full account of the failure and the pre-flight asymmetry that caused it | `../audit_log.md`, below the freeze sentinel |
| The repaired driver, exercised at full 219-unit scale on a stub transport | `../payloads_stub/`, `../responses_stub/` (`mode: stub`, stamped 2026-08-11T20:56:11Z — **34 minutes after the failed live run**) |
| What the next preregistration must pin before any re-run | `../SPEC_next_preregistration.md` |
| Tracker entry | ISSUES OQ-277 |

## Do not delete this file to "clean up" the directory

If a future run persists real responses here, this file stays until its account is folded into
that run's record — a second run does not retroactively make the first one's absence
uninteresting. The empty state is a dated fact about 2026-08-11.
