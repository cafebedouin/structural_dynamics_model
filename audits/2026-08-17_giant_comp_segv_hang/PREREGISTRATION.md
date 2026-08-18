# PREREGISTRATION — giant_comp SIGSEGV + futex hang, round 2

**Written:** 2026-08-17, BEFORE any round-2 arm is run.
**OQ:** OQ-77 (reopen candidate), OQ-182 (attribution challenged).
**Baseline established (round 1, this session):** 7/100 failures on
`swipl -l stack.pl -l giant_component_analysis.pl -g "run_giant_component_analysis, halt."`
— 6 × hang (25 s timeout), 1 × SIGSEGV — serial, one process at a time, idle machine,
live corpus n=279. Every failure emitted exactly 967 bytes (a complete run is 7,745).

## Power, pre-committed

Taking p₀ = 0.07 as the baseline failure rate:

| n | P(zero failures \| p₀) | 95% upper bound if zero seen |
|---|---|---|
| 10 | 0.484 | 30% |
| 30 | 0.113 | 10% |
| 60 | 0.013 | 5% |
| **150** | **0.00002** | **2%** |

**Round-2 arms run at n = 150.** Round-1 scoping arms were run at n = 60 before this
power statement existed; n = 60 has 98.7% power to see ≥1 failure if nothing changed,
but only bounds a *clean* arm at 5%, so any clean n=60 arm is re-run at 150 before it
is cited as a fix.

**Note on the OQ-182 "cure" evidence.** That audit's N=10 liveness battery read 10/10
GREEN. At p₀ = 0.07, P(10 clean) = **0.48**. The battery could not distinguish "cured"
from "got lucky", so it is not evidence for the co-residency root cause. This is a
power objection, not a claim that the serialization fix was wrong.

## Arms and pre-committed readings

Each arm: 150 invocations, serial, `timeout 25`, idle machine, corpus frozen
(md5 of the testsets glob recorded before and after each arm; an arm whose corpus
fingerprint moved is void and re-run).

| arm | change | records |
|---|---|---|
| **A** baseline | none (default flags) | re-confirms p₀ under round-2 conditions |
| **B** no GC thread | `set_prolog_flag(gc_thread,false)` | AGC moved into the calling thread — tests *concurrency of* AGC |
| **C** no AGC at all | `agc_margin` set enormous | AGC effectively never triggers — tests *AGC itself* |
| **D** line-buffered | `set_stream(user_output, buffer(line))` | localizes the true death point (see caveat below) |
| **E** small corpus | `corpus_path` overlaid to an n≈39 subset | tests the size dependence that would explain OQ-77's clean 2026-06-10 arms |
| **F** newer swipl | 9.3.x built from source, arm A repeated | tests whether this is fixed upstream |

### Decision table (B × C) — committed before the run

- **B clean AND C clean** → AGC is involved and it is the *threading* that kills;
  `gc_thread=false` is a shippable mitigation.
- **C clean, B dirty** → AGC itself, regardless of which thread runs it; the margin is
  the lever, not the thread.
- **B clean, C dirty** → threading-specific; AGC firing is not sufficient.
- **B dirty AND C dirty** → the AGC hypothesis is wrong and the GC thread is a
  bystander; the `.data` mutex neighbourhood (stream layer) becomes the primary lead
  and arms B/C are abandoned rather than re-run at larger n.

### Arm D caveat, stated in advance

The 967-byte boundary is where stdout *stopped*, which is not necessarily where
execution stopped: with stdout a pipe, SWI block-buffers, so a flush at death (crash
handler / SIGTERM handler) is what makes 967 readable as a death point at all. Arm D
removes that inference by forcing line buffering. **If arm D's failures stop at a
different, later line than the Degree-Distribution header, then the round-1 claim
"dies right after `### Degree Distribution`" was a buffering artifact and is
retracted** — the localization, not the failure, was wrong.

### What would falsify the "one bug, two landings" reading

Arms B/C/E/F splitting the two modes — i.e. any arm where the SIGSEGV rate and the
hang rate move in *opposite* directions, or one vanishes while the other persists.
That would make them two bugs sharing a window, and each needs its own arm.

## Out of scope for round 2

Naming the crashing static function. It requires `swi-prolog-nox-dbgsym`
(`.gnu_debuglink` → `3d54da59530c9f6a780ae566ccc0d393e641de24.debug`), which needs an
interactive `sudo apt install` — operator action, not runnable here.
