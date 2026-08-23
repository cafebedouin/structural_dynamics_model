# OQ-301 — the giant_comp failure regime is ABSENT on the current substrate: 0 failures in 150 serial runs, cause permanently unattributable

**Executed:** 2026-08-23 (round-2 arms; directory dated 2026-08-17 = round-2 preregistration date)
**OQ:** OQ-301 (round 2) — with notes at OQ-77 (reopen candidate) and OQ-182 (attribution challenged)
**Verdict:** Arm A ran **150/150 clean** (`rc=0`, byte-identical output) on swipl 10.0.2 against a
frozen 285-file corpus snapshot; under an independence assumption this rules out a per-run failure
rate above **1.98 %** at 95 %, so round 1's reported 7 % regime is **not reproducible here**. It is
**not** "interpreter-resolved": the round-1 baseline was reported-not-witnessed and its binary is
purged, so the cause is permanently unattributable and the concurrency challenge to OQ-77/OQ-182 is
recorded **unresolvable in principle**, neither confirmed nor refuted.
**Substrate:** **no pipeline run.** The arm is a standalone serial invocation of
`run_giant_component_analysis`, not `run_pipeline.py`, so there is no `pipeline_output.json`
manifest to cite. Its substitute, pinned in `audit_log.md`: frozen corpus snapshot **285 files**,
`corpus_fp = ea0c60a50e6d7b063b7a51d9154f5893` (stable across the arm); interpreter **SWI-Prolog
10.0.2** (package `10.0.2-1-gb8d8f931a-nobleppa2`); code at commit `4c55e3027`, tree clean for
`python/run_pipeline.py` and `prolog/giant_component_analysis.pl`; box `SOMA-PC`, idle
(start and end process census both empty).
**Fired:** no — 0/150 on the failure question is pure confirmation of the ruled clean branch.
Three record corrections and three plan defects were found along the way and are reported in
§4 and §5; per the convention this commit writes into `audits/README.md`, **the `Fired:` bit tracks
the OQ's question, not apparatus self-test or record hygiene** — counting a fired control as `live`
would make nearly every audit `live` and the bit would stop discriminating.
**Evidence map:**
- `audit_log.md` — the execution record and the only committed witness for everything below: all 23
  Assumed-substrate lines with their output; the three Step-0.4 positive controls; the frozen
  detector; the Step-1b driver controls and plants; the Step-2 arm-A counts re-derived with the
  pinned expressions. Committed before the arm ran.
- `PREREGISTRATION.md` — the round-2 contract (unchanged: n=150, arms A–F, B×C decision table,
  arm-D caveat) plus the dated **Round-2 amendments (2026-08-23)** section written before any
  counted row.
- `round2_arms.sh` — the arm driver as amended (Set R). Post-amendment md5
  `a79f74f3465198ed470421cc6496ac3c`; pre-amendment `9b3afcf7487655a38d779cb85d798562`.
- `raw/arm_A.tsv` (150 rows) and `raw/arm_A.console`, plus the four control TSVs
  (`warmup`, `warmup2`, `ctl_hang`, `ctl_hang_kill`) — **committed**; these are the frozen
  evidence for every count in this writeup. The per-row subdirectories (`raw/<TAG>/<i>.stderr`,
  56 MB for arm A alone, and the `.matcher` files) are per-machine and git-excluded; their
  committed witness is the pasted content in `audit_log.md`.
- `corpus_snapshot/` (git-excluded, per-machine) — the frozen 285-file corpus. Committed witness is
  its count + fingerprint in `audit_log.md`.
- `r7_trigger_control.py` / `r7_trigger_control.txt` — the two-sided control for the R7
  `run_prolog` watcher landed by this close, exercised THROUGH `run_prolog` (not by calling
  the helper) so it witnesses that the warning is CALLED: fires on a signalled giant_comp
  retry whether giant_comp is named in the goal or only in the modules, declines on an
  unrelated goal, declines when no retry fires.
- **No core files and no stack files exist.** Nothing crashed. The RA ledger
  (`path + size + md5 + readelf -n`) is empty for that reason, not because collection was skipped.

---

## 1. What ran

One arm: **A, baseline, default flags, n=150, serial, one process at a time**, on an idle box,
under **Set R (record-keeping) only**. The watcher (Set W) was deliberately *not* built: under the
ruled R3=(b) it would only have been built in arm A′, in the failure branch, so that arm A stayed
unperturbed and reproduce-round-1 risk stayed off the apparatus's critical path. Arm A never failed,
so A′ never ran and the watcher does not exist.

Counted invocation, unchanged in shape from round 1 apart from `-k` and the stderr destination:

```
o=$(timeout -k 5 25 swipl -l stack.pl -l giant_component_analysis.pl \
      -g "asserta(config:param(corpus_path,'…/corpus_snapshot')), run_giant_component_analysis, halt." \
      2> raw/arm_A/<i>.stderr)
```

## 2. Result

| bucket | pinned expression | count |
|---|---|---|
| rows | `wc -l` | **150** |
| clean | `$2=="rc=0"` | **150** |
| failures | `$2!="rc=0"` | **0** |
| did-not-complete-in-`TMO` | `$2=="rc=124" \|\| ($2=="rc=137" && wall_ms>=29000)` | 0 |
| segv | `$2=="rc=139"` | 0 |
| external kill / OOM | `$2=="rc=137" && wall_ms<29000` | 0 |
| driver/env fault | `rc` in `1..127` | 0 |

Reconciliation `dnc+segv+extkill+faults+other≥128 == failures` → **0 == 0, RECONCILES** (the
tautological `clean+failures==rows` was not used as the check). Shape check: 150 rows, `NF==7`,
all seven key prefixes, 0 malformed, `i` complete 1..150.

**Every clean row emitted exactly 7 740 bytes** — the exact-equality margin witnessed by two
warm-up runs at Step 1b. The 0/150 is therefore not 150 runs that returned 0 while quietly
producing something different; the complete report was reproduced 150 times. Wall time
min 1 369 / mean 1 396 / max **1 470 ms** — the slowest row used 5.9 % of the 25 s timeout.

No VOID of any cause: `stray=0` on every row, no survivor of `timeout -k`, start and end census
empty, snapshot fingerprint stable, no OOM kill in a `dmesg` ring buffer proven readable.

**`rc=124`/`rc=137` are called "did-not-complete-in-`TMO`" throughout.** "Hang" is reserved for a
row with a parked-futex stack, which only arm A′ could have produced and which does not exist.

## 3. What this licenses, and what it does not

**Licensed.** *No failure occurred in 150 serial runs on this substrate; under an independence
assumption the per-run rate is below 1.98 % at 95 %; round 1's reported regime is not reproducible
here.* `P(0/150 | p₀ = .07) = 1.9 × 10⁻⁵`.

**Not licensed, and the reasons compound.**

1. **Not "the interpreter fixed it."** Attributing the cure to 9.2.9 → 10.0.2 from a single
   post-change observation is the OQ-251 single-variable-isolation error run in reverse. At least
   three things moved in the window: interpreter, corpus (n=279 live → 285 frozen), and
   `run_pipeline.py`.
2. **Not a re-measurement of round 1.** Round 1's evidence never reached this directory and its
   binary is purged; the 7/100 baseline is an assertion that can never be upgraded to witnessed.
3. **Not a cause.** No failure occurred, so no mechanism was observed. Arms B–D (the AGC decision
   table), arm E, arm F, the valgrind pass and the crashing-frame question are all **moot**: they
   are levers on a phenomenon this substrate does not exhibit.
4. **Not independent draws if the failure is state-dependent.** 150 runs in one 4-minute window on
   one box do not test a regime that needs a particular machine state to appear.

### The OQ-77 / OQ-182 challenge: unresolvable in principle (R1)

A close of "cause permanently unattributable" cannot simultaneously say the concurrency challenge
was answered. The identical note is filed at **both** OQ-77 and OQ-182 so the two cannot drift:

> OQ-301 round 2 (2026-08-23) found the regime absent on the current substrate. Because the round-1
> baseline was reported-not-witnessed and the round-1 binary is purged, this neither confirms nor
> refutes the co-residency attribution; the challenge is recorded unresolvable in principle.

OQ-182's power objection stands on its own terms and is untouched: its N=10 battery had
P(10 clean | p₀=.07)=0.48 and could not distinguish "cured" from "got lucky". This arm does not
rescue it — it measures a different substrate.

## 4. Zero classification (which zeros are tested and which are untested)

- **The 0 failures is a TESTED absence.** Both cells of the did-not-complete split were fired
  through this driver by plants at Step 1b — `GOAL="sleep(60)"` → `rc=124 @ 25 029 ms`;
  `GOAL="on_signal(term,_,ignore), sleep(60)"` → `rc=137 @ 30 004 ms ≥ KTHRESH` — and the same two
  cells were independently frozen in bare shell at Step 0.4 *before* the detector was written down.
  A did-not-complete row would have been recorded.
- **The 0 cores and 0 stacks are UNTESTED INSTRUMENTS.** The controls exercised the *channels* (a
  core is written at the bound; gdb attaches and symbolizes) — not the failure. The
  "core size == BOUND ⇒ truncated" rule is a-priori: the `sleep` control core is sub-MB and never
  exercised the bound.
- **`rss_kb` is unmeasured** (`na` on all 150 rows, Set R by construction). Swap and OOM behaviour
  is not addressed by this arm.
- **Round 1 is unrecheckable**, not merely unchecked.

## 5. Incidental findings

**Three plan defects**, reported rather than adapted around (all in `audit_log.md`):

- **F1** — the plan's substrate pin `grep -c '2>/dev/null' round2_arms.sh` → 1 was wrong: there are
  **three** sites and only one is the counted invocation's. Removal was scoped to that one; a
  whole-file count is not the witness, the diff is.
- **F2** — the plan's idle check `pgrep -af 'swipl|c-orchestrator|run_pipeline'` **self-matches the
  checking shell** whose command line contains the pattern. Left in the driver it would have VOIDed
  every arm at row 1 under the non-zero-stray rule. The driver matches `/proc/<pid>/comm == swipl`
  with parent comm `timeout`; the census uses the bracket-trick form.
- **F4** — `eu-stack -p` **does not return on this kernel** (observed twice against a plain `sleep`
  with `ptrace_scope=0`). `gdb -p <pid> -batch -ex 'thread apply all bt'` returns symbolized frames
  in seconds and is substituted for arm A′.

- **F5** — the plan's RA excluded `raw/` from version control wholesale. `raw/arm_A.tsv` is the
  frozen evidence for every count here and is 12 KB; the `audit cites` gate row refused an audit
  citing untracked evidence, and it was right. The exclusion was narrowed to the per-row
  subdirectories and the six row-level artifacts are committed.

**One substrate finding, F3:** `core_pattern`'s `%e` is the crashing process's `comm` *at crash
time*, so the plan's first core control produced `core.bash.<pid>` (the SEGV beat the `exec`) and
its `ls /tmp/core.sleep.*` read as "no core written" when a core had in fact been written. The core
channel works; the **name is not the key**. The driver collects by set-difference of `/tmp/core.*`
around each row, with the name as confirmation.

**Three record corrections** (re-witnessed at execution, not copied from the plan):

1. The sysctl pass is **done** — `/etc/sysctl.d/50-debug.conf` carries `kernel.yama.ptrace_scope=0`
   and `kernel.core_pattern=/tmp/core.%e.%p`, both live in `/proc`. **And the 2026-08-18
   "core dumped with no core" hazard is RESOLVED on this box**: control 2 wrote a real core at the
   bound.
2. OQ-301's claim that `/var/log/dpkg.log` "is unrotated, beginning 2026-08-18" is **false** — it
   begins `2022-02-16`, so the transition IS recoverable and is now recovered:
   `2026-08-18 00:13:15 upgrade swi-prolog-nox 9.2.9-2-g2a3e80b8e-jammyppa2 → 10.0.2-0-jammyppa2`,
   followed at `02:50:35` by an install of `10.0.2-1-gb8d8f931a-nobleppa2`, which is the running
   package. (OQ-301's in-entry `02:50:43` names only the second event.)
3. gdb, elfutils and valgrind are installed (2026-08-18 11:29–11:52). elfutils is present but, per
   F4, non-functional for live attach here.

**Incidental, and it bears on the detector:** the single pre-upgrade `giant_comp` row in
`outputs/prolog_children.log` (2026-08-17T21:58, inside round 1's own window) took **5.27 s**; all
14 post-upgrade rows sit at **1.24–1.66 s**. In-pipeline wall time fell ~3.5× across the upgrade.
`TMO=25` has far more headroom now than when it was chosen (`warm_up_wall / TMO = 0.056`).

## 6. Declared limits

- The 7/100 round-1 baseline is reported-not-witnessed and **can never be upgraded** (binary purged).
- Arm A differs from round 1 in interpreter (9.2.9 → 10.0.2), corpus (live n=279 → frozen 285-file
  snapshot), and the addition of `timeout -k 5` (round 1 ran a bare `timeout`, so it had **no KILL
  path**). A clean arm A licenses "absent now, rate < ~2 % under independence", never a cause.
- 150 runs in one window on one box are not independent draws if the failure is state-dependent.
- Engine frames in `libswipl.so.10` are unnamed (stripped, no dbgsym in the PPA); valgrind would
  have given the error KIND, not the frame. Moot here — nothing crashed.
- The "core size == `BOUND` ⇒ truncated" rule is **a-priori**, not confirmed reachable by control. A
  missing core could also be a shared-`/`-space failure (`raw/` and `/tmp` are the same filesystem,
  device 2096 — one budget).
- `corpus_snapshot/` and `raw/` are per-clone excluded; their committed witness is the count +
  fingerprint + pasted rows in `audit_log.md`.
- The round-1 corpus is **not** cleanly git-reconstructible (nearest commit `8c34157f7` is
  2026-08-17 23:18, after round 1's ~21:58 run, and the tree was dirty), so the optional **arm A″**
  would be an approximation at best. It was not run.
- The R7 `run_prolog` trigger fires on the IN-PIPELINE regime; a standalone-serial regression would
  go unwatched.
- `rss_kb` is arm-A-`na` by construction; it would have arrived only with A′.

## 7. Residue — what changed in substrate

- **`ISSUES.md` OQ-301 → `resolved`** with the pre-committed wording; its two `blocked_on_human`
  Deps are **retired at close** (clean branch), and the three record corrections above are written
  into the entry as dated, marked lines.
- **`ISSUES.md` OQ-77 and OQ-182** each carry the identical R1 unresolvable-in-principle note.
- **`python/run_pipeline.py`**: `run_prolog` now emits a warning naming OQ-301 when its retry fires
  on `giant_component_analysis` (R7 — scope beyond the docstring edit, flagged as such), and the
  docstring records the round-2 result plus the reason it does **not** license deleting the retry:
  different interpreter, different corpus, baseline never witnessed, regime unattributable rather
  than disproven.
- **`audits/README.md`**: the R2 convention — the `Fired:` bit tracks the OQ's question, not
  apparatus self-test or record hygiene.
- **`KNOWN_STATE.md`**: dated entry, including the 2026-08-18 "core dumped with no core" hazard
  recorded RESOLVED on this box.
- **`audits/INVESTIGATIONS.md`**: this run's line closed with `Fired: no`.
- **Next forward move, if anyone returns to this:** there is none on the current substrate — the
  arms are moot while the regime is absent. The re-entry condition is a **recurrence under serial
  operation**, which is exactly what the new `run_prolog` warning watches for in-pipeline. If one
  ever fires, this directory's driver, detector and controls are ready: `TAG=<fresh> ./round2_arms.sh A`
  reproduces arm A, and Step 3's arm A′ (Set W, gdb sampler) is specified in the plan and in
  `PREREGISTRATION.md` but deliberately unbuilt.
