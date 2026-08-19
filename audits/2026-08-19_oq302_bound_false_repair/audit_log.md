# OQ-302 — audit log

**Audit:** `audits/2026-08-19_oq302_bound_false_repair/` · **OQ:** OQ-302
**Executed:** 2026-08-19 · **Code state at open:** `a84cb693`

---

## Phase 0 — enumeration (read-only, no engine runs)

Artifact: `consumer_enumeration.md`. Raw: `caller_enumeration_raw.txt`, `bound_false_scan.out`.
Scripts: `bound_false_scan.sh`, `run_scan.sh`.

- Gate GREEN at open (all 27 rows; `python env` row read first — it resolved to the repo venv
  interpreter, py3.12.3, not system python3).
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

### R4. Commit-1 second witness — composed vs shipped, matched by arm flag

`compare_arms.py`; table in `commit1_second_witness.md`. The probe re-run against the
committed source detected `source_arm=repaired` **by reading the clause**, on all six legs
(Phase 2 detected `defect` on all six). The `arm(repaired)` column, matched by arm flag and by
`(leg, constraint_id)` — never by column position:

**5311/5311 rows identical.** `result__arm_defect` is `NOT_MEASURED` on 5311/5311 post-commit
(the defect clause no longer exists in the source). The composed program of PREREG §0a and the
shipped source agree exactly, so the Phase-2 table describes the program that actually shipped.

Suites after Commit 1: OQ-137 reading totality **10/10** on `run_pipeline`'s own load chain
(`-l stack.pl -l reading_registry.pl -l commentary_census.pl -l tests/test_reading_totality.pl`
+ `corpus_loader:load_all_testsets, run_tests(reading_totality)`); `run_dynamic_suite` **GOOD**
(0 errors; 3 pre-existing warnings, all on `zionist_legitimacy_basis__national_liberation_reading`).

### R5. The gate caught an under-scoped claim — `latent-B` asserts more than the enumeration proved

Reclassifying the registry row `finding` → `latent-B` turned the `codewalk caller` row **RED**:
the module-resolved arm found **1 bound call site** on the predicate. Phase 0's claim —
*"`:577` is the only reachable bound-`false` call site"* — was correct; the **class label** is
broader (`latent-B` = "no live BOUND caller"), and there is one: `boltzmann_compliant/2` at
`boltzmann_compliance.pl:94-95`, bound-**`true`**.

Adjudicated into `prolog/codewalk_caller_allowlist.txt` with `ATOMS=true` and a REMOVE
condition, on evidence this run already holds — a **naturally-arising two-sided record**, no
plant. `boltzmann_compliant/2`'s only access gate is that bound-`true` call, so its behaviour
over the corpus is a direct read on whether the idiom is over-permissive:

```
eac (unbound) = false : 753   -> T1 = fail(factorization, inconclusive(insufficient_classifications))  753/753
eac (unbound) = true  : 4558  -> T1 = pass(factorization) 512 | fail(factorization, non_compliant(...)) 4046
```

It **declines on exactly the 753 the unbound call declines on** and **fires on exactly the
4,558 it fires on** — zero over-permissive rows. That is the head-unification asymmetry the
file's own header states at `:476-477`, measured: bound-`true` fails through to *no solution*,
bound-`false` falls through to *a wrong one*.

`purity_scoring.pl:49` carries the same bound-`true` idiom and is **not** reported by this arm
(its declared blind spot: the walk resolves the defining module's program). Recorded so the
allowlist row is not read as a complete census of bound-`true` callers.

### R6. Registry disposition, and what it does NOT license

`("boltzmann_compliance.pl", "epistemic_access_check/2")` is now `latent-B` with the required
`LAST_ARG` fact `("output", authored: %% epistemic_access_check(+Constraint, -Sufficient))`.

**This re-populates a class OQ-303 recorded as EMPTY on 2026-08-19** (`latent-B` 0 → 1). The
count is correct and the OQ-303 line is corrected in place rather than left to read as still
true. **This row is a genuine class-B CONVERSION candidate on OQ-303's own criteria** — output
last argument, and *reached* (5 callers, on the `purity_score` / `fingerprint_coupling` /
`structural_purity` output paths). It was excluded from the 2026-08-19 rollout only because it
was classed `finding` at the time. Converting it owes the six-leg clean-vs-edited pair, not the
template; **routed to OQ-303 arm (a), deliberately not done here** — it is a live-output-path
semantics change and belongs to that OQ's spend, not this one's.

### R7. Close — trackers, gate, and the HEAD stamp pair

- `WRITEUP.md` landed with the required header and `**Fired:** live`.
- `ISSUES.md`: OQ-302 **resolved**, with its three wrong lines corrected **in place**
  (blast radius; "real invariant/variant verdicts"; the git-blame instruction), `bundled_with
  OQ-303` added to `**Deps:**`, and the residues routed (OQ-303 arm (a), OQ-317, GAP-08 §7).
  OQ-303's `latent-B is now EMPTY` line corrected in place — it is 0 → 1 the same day.
- `KNOWN_STATE.md`: one `landed` entry. Promotion test applied per candidate, two passes:
  - the `with_overlay/3` fact-only/rule-clause hazard is a **silent** mistake a fresh agent
    makes by following an always-loaded instruction → **PROMOTED** to CLAUDE.md
    (*Running the System*) and to `AGENTS.md`, with the mechanism in
    `swipl_load_path_and_probe_gotchas.md` §12; full provenance stays here.
  - "the spec prescribed the defect" and the `latent-B` 0 → 1 correction are routing/history,
    not silent-mistake tripwires → stay in KNOWN_STATE and the Pattern-7 ledger.
- `omega_resolver.py index` regenerated; `index --check` fresh.
- **HEAD stamp pair: open `a84cb693`, close `4f8f0e3f` + the close commit.** They differ only
  by this audit's own commits; `git diff --stat a84cb693..HEAD -- prolog/` shows only
  `boltzmann_compliance.pl` and `codewalk_caller_allowlist.txt`, both this audit's. No
  intervening writer.
- Final `./scripts/gate.sh`: **GREEN**, all 27 rows (pasted in the close commit message).
