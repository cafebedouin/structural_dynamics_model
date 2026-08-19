# OQ-302 — audit log

**Audit:** `audits/2026-08-19_oq302_bound_false_repair/` · **OQ:** OQ-302
**Executed:** 2026-08-19 · **Code state at open:** `a84cb693`

---

## Phase 0 — enumeration (read-only, no engine runs)

Artifact: `consumer_enumeration.md`. Raw: `caller_enumeration_raw.txt`, `bound_false_scan.out`.
Scripts: `bound_false_scan.sh`, `run_scan.sh`.

- Gate GREEN at open (all 27 rows; `python env` row read first — interpreter
  `.venv/bin/python`, py3.12.3).
- Consumer trace terminates at **0 callers** for both consumers of
  `boltzmann_invariant_mountain/2`; OQ-38 census rows 110/620 agree (`STATIC_ORPHAN`, 0).
- Zero Python/agent readers; zero occurrences in `outputs/pipeline_output.json`
  (manifest `2026-08-18T20:34:53Z`, n=279) — grep positive-controlled on the same file
  (`purity_score` 279, `h1_band` 422).
- Enumeration discrimination record: **2 bound-`false` sites at `0bfd3b31^`, 1 at `0bfd3b31`**
  (naturally-arising pair, not a planted fixture).
- OQ-302's "git-blame `:577`" instruction corrected: `:577` has no clean parent; the defect
  predates the Feb-2026 monolith split (`a0e8d772^` → `structural_signatures.pl:1224,1464`),
  first introduced `80aebdb3` 2026-02-07.

## Phase 1 — design checks (harness feasibility; NOT measurements of any pre-registered observable)

Run under `swipl -q -l stack.pl`, recorded here because they changed the frozen design:

```
bim/2 dynamic? no
clause/2 ok, first body: epistemic_access_check(_33330,false),!
assertz THREW _33528
eac/2 dynamic? no
```

Consequence: the plan's `with_overlay/3` mechanism cannot be executed as written (fact-only
snapshot + static predicate). Substituted mechanism and its declared residual gap are
PREREGISTRATION §0a.

---

## PREREGISTRATION FROZEN — 2026-08-19, before the Phase-2 probe was written or run

```
$ md5sum audits/2026-08-19_oq302_bound_false_repair/PREREGISTRATION.md
c7a7345ce4664871df94e9adf1fc682d  audits/2026-08-19_oq302_bound_false_repair/PREREGISTRATION.md
```

**Everything below this line is a result.**

---

## RESULTS — 2026-08-19

### R1. Phase-2 probe — six legs, 5,311 constraints, all pre-flights clean

Driver `run_probe.sh phase2`; probe `invariance_probe.pl`; raw `tsv_phase2/*.tsv`;
run log `probe_run_phase2.log`; readout `readout_phase2.md`.

Pre-flight (PREREG §6.7), per leg — `files_on_disk` == `[corpus] Loaded N` ==
`corpus_constraint_count`, and leg md5 **identical before and after** the run:

```
testsets                     279  md5 1efa04ac05cc58a7ba3d76cd8195ccfe -> 1efa04ac05cc58a7ba3d76cd8195ccfe
testsets_haiku               960  md5 e2a05434ed20e1f12223d2cfcece5b36 -> e2a05434ed20e1f12223d2cfcece5b36
testsets_flash               960  md5 e84f3f08ecc82cedfa0818a6c2369111 -> e84f3f08ecc82cedfa0818a6c2369111
testsets_kimi               1005  md5 8e4c17a221a1d8e1f50e3857c7007196 -> 8e4c17a221a1d8e1f50e3857c7007196
testsets_sonnet             1001  md5 f0556a6e59427bca9da8af14f2cdce48 -> f0556a6e59427bca9da8af14f2cdce48
archives/datasets/kernel_v1 1106  md5 66ba8ad01bd110040d09d5052a4d20df -> 66ba8ad01bd110040d09d5052a4d20df
```

`prolog/testsets/` counted at run time: **279 on 2026-08-19**. No recalled figure is used.
`source_arm` detected as `defect` on every leg (read from the clause, not assumed).

### R2. Controls

| Control | Result | Grade |
|---|---|---|
| *fires* (§5a) | `arm(defect)` = `inconclusive(insufficient_data)` for **5311/5311**, 100% on every leg | **wiring check with a known answer.** Not discrimination. |
| *declines* (§5b) | subjects found on **every** leg (43 / 466 / 212 / 29 / 1 / 2 = 753); **753/753 still `inconclusive(insufficient_data)` post-repair** | two-sided; carries the weight. `testsets_sonnet` n=1 — thin, declared. |
| §0b aggregation cross-check | **4558 `match`, 753 `not_reached`, 0 `MISMATCH`** | the per-test transcription reproduces the clause/2-composed Result on every reaching row |
| §4 escalation (`T4 = pass`) | **0 of 5311.** `T4 = fail(natural_law_signature)` on every constraint on every leg | does not fire; T4's darkness re-witnessed at full scale |
| §3 row 3 (throws) | **0** thrown-error cells and **0** failed cells across all 9 value columns × 6 legs | does not fire |

### R3. Disposition — `T1`–`T3` **VARY**, on every leg (PREREG §3 row 1)

`N_reaching` per leg: 236 / 494 / 748 / 976 / 1000 / 1104 = **4558**. No leg is at `≤ 1`, so
no leg is UNMEASURED. Distinct `(T1,T2,T3)` tuples over the reaching set:

```
testsets                     236 reaching -> 129 distinct tuples (T1 17, T2  7, T3 73)
testsets_haiku               494 reaching -> 234 distinct tuples (T1 24, T2  8, T3 88)
testsets_flash               748 reaching -> 133 distinct tuples (T1 16, T2  7, T3 66)
testsets_kimi                976 reaching -> 224 distinct tuples (T1 18, T2 10, T3 101)
testsets_sonnet             1000 reaching -> 263 distinct tuples (T1 16, T2 10, T3 100)
archives/datasets/kernel_v1 1104 reaching -> 270 distinct tuples (T1 25, T2  9, T3 86)
```

**Verdict marginal:** `arm(defect)` is `inconclusive` for 5311/5311. `arm(repaired)` is
`variant(...)` for 4558 and `inconclusive` for 753 — and **never `invariant`**, because T4 fails
on every row. Exactly the frozen verdict text: the verdict stays constant *in kind*
(no `invariant` is reachable), the payload does not.

**Site comment MAY state that the payload becomes per-constraint** (PREREG §3 row 1). It may not
say the check now works.
