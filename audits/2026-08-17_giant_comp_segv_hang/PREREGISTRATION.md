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

---

# Round-2 amendments (2026-08-23)

Written at execution, BEFORE any counted row. The arms, n=150, the B×C decision table and the
arm-D caveat above are **unchanged**. What follows records what the driver now does, what is
deferred, and the three instrument substitutions the substrate forced.

**Provenance.** Plan `can-you-review-oq-301-distributed-toucan.md`, RULED 2026-08-23 (R1–R7, RA,
RB). Pre-amendment `round2_arms.sh` md5 `9b3afcf7487655a38d779cb85d798562`; post-amendment md5
recorded in `audit_log.md`. Pre-amendment `PREREGISTRATION.md` md5
`a9aa0705d29d3506c6505c1eada5e340` (this section is the change).

## Arm A is **Set R only** (record-keeping); the watcher is deferred to arm A′

R3=(b). Arm A runs **unperturbed** — as close to round-1-equivalent as this substrate allows — so
the watcher is not on its critical path and cannot perturb it. Consequences, accepted in advance:
`rss_kb=na` on every arm-A row, no `.stack` files, and a counted row may be **unkeyable** to a core
(`pid=na`). If arm A fails, **arm A′** re-runs with the watcher (Set W) in the branch where
failures are known to exist, keeping arm A's snapshot. **A′ is diagnostic and NOT rate-poolable
with A** — the attach makes it non-equivalent; the two arms are never summed.

## What the driver records per row

Tab-separated `key=value`, no header, no bare numbers, flushed per iteration to `raw/<TAG>.tsv`:

```
i=<n>	rc=<n>	bytes=<n>	wall_ms=<n>	rss_kb=<n|na>	stray=<n>	pid=<n|na>
```

Per-row stderr → `raw/<TAG>/<i>.stderr` (round 1 discarded it to `/dev/null`). Outputs are namespaced
by `TAG` and **never shared**: the driver refuses to start if `raw/<TAG>.tsv` exists.

## The frozen failure detector

`TMO=25`, `timeout -k 5`, so `KTHRESH = (TMO+kill_after)·1000 − 1000 = 29000`.

| verdict | rule |
|---|---|
| did-not-complete-in-TMO | `rc=124` (TERM-responsive) **or** `rc=137` at `wall_ms ≥ 29000` (KILL-required) |
| segv | `rc=139` |
| external kill / OOM | `rc=137` at `wall_ms < 29000` (disambiguate against `dmesg`) |
| other `≥128` | escalate |
| driver/env fault | `1..127` — STOP, fix, re-run under a fresh TAG |

Frozen on a bare-shell control run **before** the detector was written down, both cells witnessed
(`audit_log.md` Step 0.4): TERM-ignoring → `rc=137 wall_ms=30019`; TERM-responsive →
`rc=124 wall_ms=25003`.

**`rc=124/137` is called "did-not-complete-in-TMO" in every artifact.** "Hang" is reserved for a row
with a parked-futex stack, which only arm A′ can produce.

## Frozen corpus snapshot (R5)

`corpus_snapshot/` is a one-time `cp -a` of the **285-file live-loadable set** (`testsets/*.pl`, the
loader's own non-recursive glob — not the 292 git-tracked files, which include run-tagged subdirs
the glob excludes). The driver overlays it with `asserta(config:param(corpus_path,'<abs>'))` —
`asserta`, never `assertz` (config.pl:509 defines the default first and the loader takes the first
solution). It is created once and never silently refreshed; it is git-excluded per-clone, so its
committed witness is the count + fingerprint in `audit_log.md`. Under a frozen snapshot the
per-arm fingerprint check **cannot fail** — it is a consistency check, not a guard; the co-residency
guard is the stray/survivor/census checks alone.

**A′ keeps arm A's snapshot.** Testing the round-1 corpus is a separate **arm A″** with its own
amendment line, and it is flagged unreliable: the nearest testsets commit `8c34157f7` is
2026-08-17 23:18, *after* round 1's ~21:58 run, and the tree was dirty — A″ is an approximation.

## Core capture

`ulimit -c 100544` (512-byte blocks = 51 478 528 B = 2 × the out-of-band peak RSS of 25 136 kB),
**not `unlimited`**. **A core whose size equals the bound is presumed TRUNCATED** and recorded as
such — a-priori, since the `sleep` control core is sub-MB and never exercises the bound. `raw/` and
`/tmp` are the same filesystem (device 2096), so cores and the 150 stderr files share one budget.

## Three instrument substitutions the substrate forced

1. **`pgrep -f` is not usable as the process matcher (F2).** A `-f` pattern matches the `timeout`
   parent, matches `valgrind` under `WRAP`, and — witnessed — matches *the checking shell's own
   command line*, which would VOID every arm at row 1 under the non-zero-stray rule. The driver
   matches on `/proc/<pid>/comm == swipl`, and the row's process is the swipl whose **parent's**
   comm is `timeout`. The arm-level census uses the bracket-trick form.
2. **`core_pattern`'s `%e` is the crashing process's comm at crash time (F3).** A core is therefore
   collected by the **set difference of `/tmp/core.*` around the row**, with the `core.swipl.<pid>`
   name as confirmation rather than as the key.
3. **`eu-stack -p` does not return on this kernel (F4).** Observed twice on a plain `sleep` target
   with `ptrace_scope=0`; `gdb -p <pid> -batch -ex 'thread apply all bt'` returns symbolized frames
   in seconds. **Arm A′'s sampler uses gdb.** Arm A does not attach at all, so Step 2 is untouched.

## Deferred, moot, and unreadable at n=150

- **Arm E** (n≈39 subset, size dependence) is **deferred, not dropped** — it needs its own prereg
  amendment pinning the subset before it may run.
- **Arm F is MOOT.** Its stated prerequisite ("a source-built swipl 9.3.x") is satisfied by the
  system interpreter, now 10.0.2. It is not a dependency of this OQ; the live dependency is a
  **symbol-bearing** swipl, which is a separate question and stands regardless of version.
- **The "one bug, two landings" falsifier is readable for the HANG mode only at n=150.** At round
  1's reported rates the SIGSEGV mode expects ~1.5 events in 150, so it essentially never clears
  the ≥5-per-mode readability floor. Stated here so it is not discovered after 450 runs. A mode
  reads *vanished* only if arm A showed it ≥5× (P(0/150 | 5/150) ≈ 0.006).

## How arm A differs from round 1 — the whole list

1. **Interpreter:** 9.2.9 → 10.0.2 (upgrade 2026-08-18 00:13:15; running package
   `10.0.2-1-gb8d8f931a-nobleppa2`).
2. **Corpus:** live leg n=279 → frozen 285-file snapshot.
3. **`timeout -k 5` added.** Round 1 ran a bare `timeout $TMO`, so it had **no KILL path at all**:
   a TERM-ignoring child would not have been reaped. The plan's phrasing ("the counted invocation
   *stays* `timeout -k 5 …`") is inaccurate about round 1 — `-k` is an **addition**, recorded here.
   It cannot change the outcome for a TERM-responsive process (round 1's reported hangs all ended
   at 25 s, i.e. TERM-responsive), and it converts a hypothetical unreapable row from an infinite
   stall into an `rc=137 wall_ms≥29000` row.
4. Per-row stderr is kept rather than discarded; per-row `wall_ms` and pre-launch stray count are
   new; the corpus is overlaid rather than taken from the live path.

A clean arm A therefore licenses "the regime is absent on **this** substrate", never a cause.
