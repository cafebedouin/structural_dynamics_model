# B1-scan — real-corpus upward-run scan (cheapest falsifier; selects the close-branch)

**Date:** 2026-06-26. **Plan step:** Part B / B1-scan. **Read-only** (no engine
change; `transition_paths.pl` unmodified). Instrument: the existing
direction-neutral `transition_paths:degradation_chain/3` over the `snapshot_type/3`
series — already present, no `repair_transition/4` needed to run this scan.

## Method
`degradation_chain(C, Chain, _)` builds the deduplicated chronological sequence of
`snapshot_type(C, Time, Type)` over each constraint's authored measurement series
(Len > 1). A step `A→B` is **upward (repair)** iff `B` decays-to `A` under the
transitive closure of the 8 `transition_path/4` decay edges (`unknown` excluded —
off the health ordering, OQ-37). Harness: `b1scan_harness.pl`.

## Positive controls (scan logic — pasted before the corpus verdict)
```
OK upward(snare,tangled_rope)        OK decay(tr->snare) NOT upward
OK upward(snare,rope)                OK decay(rope->piton) NOT upward
OK upward(tangled_rope,rope)         OK unknown excluded
```
The `upward/2` relation flags genuine repair edges and rejects decay edges and
`unknown`. The probe has teeth and is not flagging the decay direction by mistake.

## Result — NON-EMPTY on BOTH corpora ⇒ close-state 1
| corpus | constraints | non-trivial chains | **upward runs** |
|--------|-------------|--------------------|-----------------|
| `testsets/`   | 104  | 62  | **2**  |
| `kernel_v1`   | 1106 | 833 | **30** |

- testsets/: `lycurgan_laws__demographic_trap_reading` (snare→tangled_rope),
  `shinbutsu_ontological_commitment__incoherence_reading` (snare→rope).
- kernel_v1: 30 upward runs across distinct constraints, incl. genuine **multi-step
  repairs** — `homoousios_christology__arian_reading` (snare→tangled_rope **and**
  tangled_rope→rope) and `versailles_reparations_clauses__punitive_liability_reading`
  (snare→tangled_rope **and** tangled_rope→rope). Full list: `b1scan_kernel_v1.log`.

The real corpora **do** contain upward runs, surfaced by the existing instrument.
The single-edge strict-reverse definition (`b1scan_strict_v1.pl`) finds 1 on
testsets/ (lycurgan); the principled transitive-closure definition finds 2 (adds
the shinbutsu 2-level repair). Either way: non-empty.

## Close-branch selection (the operator's call — presented, not self-selected)
Per plan, B1-scan selects among three pre-registered close-states:
- **close-state 1** (existing atoms suffice AND real upward runs found) — **THIS ONE.**
- close-state 2 (atoms suffice, zero real runs) — NOT triggered (scan non-empty).
- close-state 3 (cannot author a positive without a new drift-event atom the corpus
  lacks) — NOT triggered (the existing `snapshot_type` series already yields repair;
  no new authored atom required).

⇒ matched close-claim: **"asymmetry closeable; repair observed in real corpus;
build B2/B3 to render real repair as commentary."** Whether to proceed on this
branch is the operator's seat (plan's B operator stop).
