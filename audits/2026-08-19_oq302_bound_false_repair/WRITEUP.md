# OQ-302 — the bound-`false` call at `boltzmann_compliance.pl:577`

**Executed:** 2026-08-19
**OQ:** OQ-302 (`splits_from OQ-296`; `bundled_with OQ-303`)
**Verdict (scoped):** The repair lands. Over six legs and 5,311 constraints,
`boltzmann_invariant_mountain/2`'s **verdict stays constant in kind** — `invariant(_)` is
unreachable because Test 4 is dead-by-range — while its **payload becomes per-constraint**:
4,558 constraints now reach the four-test body and produce 129–270 distinct `(T1,T2,T3)`
tuples per leg, where every constraint previously received the identical
`inconclusive(insufficient_data)`. **Blast radius is zero live consumers**, so nothing
downstream moves; the payoff is that the `bound selector` / `dispatch head` gate rows stop
carrying a known-benign row.
**Manifest cite:** `outputs/pipeline_output.json` — `pipeline_run_at` `2026-08-18T20:34:53Z`,
`n_constraints` 279, `code_commit` `6523046`, `code_dirty` true. `prolog/testsets/` counted at
run time: **279 on 2026-08-19** (never recalled).
**Fired:** live

**HEAD stamp pair:** open `a84cb693`, close `4f8f0e3f`. They differ **only by this audit's own
three commits** (`fb10708a`, `5f9ec36d`, `4f8f0e3f`) — no intervening writer. Blast radius on
the audit's read-set, `git diff --stat a84cb693..HEAD -- prolog/`:
`boltzmann_compliance.pl` (+28/−1) and `codewalk_caller_allowlist.txt` (+2), both this audit's.

**Prior-art grep** (`build_discipline.md`, same pass as the finding): **no hits** for
`epistemic_access_check`, `boltzmann_invariant_mountain`, or `structural_purity` — this is not a
re-discovery of a documented instance. `bound-probe` hits at `:601` (the Pattern 3 → 7 vacation
notice) and `natural_law_signature` / `has_viable_alternatives` at `:739–782,:920` (the OQ-43/44
fail-close), both of which are *background* for this finding, not the finding. Three rows appended
to the Pattern-7 incidence ledger, with a new sub-shape named (*the spec can prescribe the
defect*).

---

## Evidence map

| Artifact | What it holds |
|---|---|
| `PREREGISTRATION.md` | frozen before the probe was written; md5 `c7a7345c…` recorded in `audit_log.md` **above the first result line**. Verdict text, `N_reaching` rule, three dispositions, escalation clause, two-sided controls, the design correction (§0a) |
| `audit_log.md` | phase log, the freeze stamp, and results R1–R6 |
| `consumer_enumeration.md` | Phase 0: the zero-consumer census, the five `epistemic_access_check/2` call sites with arg-2 shapes, the vacuous-pipeline-pair declaration, the enumeration's discrimination record, the OQ-text correction |
| `caller_enumeration_raw.txt` | raw greps behind Phase 0 |
| `bound_false_scan.sh`, `run_scan.sh`, `bound_false_scan.out` | the enumeration scan and its 2→1 discrimination record at `0bfd3b31^`/`0bfd3b31` |
| `invariance_probe.pl` | the probe (arm detection by reading the clause; `clause/2`-composed repaired arm; `catch/3` per call) |
| `run_probe.sh`, `probe_preflight_{phase2,postfix}.log`, `probe_run_{phase2,postfix}.log.gz` | the six-leg driver and its per-leg pre-flight; full logs preserved gzipped (69,079 of 69,137 lines per run are SWI corpus-load `Warning:` lines — filtered by allowlist, never `grep -v Warning`; **0 `ERROR` lines dropped**) |
| `tsv_phase2/*.tsv`, `tsv_postfix/*.tsv` | 5,311 rows per arm-state, 13 columns |
| `analyze.py`, `readout_phase2.md` | the pre-registered readout |
| `compare_arms.py`, `commit1_second_witness.md` | Commit 1's second witness, matched by arm flag |

---

## 1. The finding

`boltzmann_invariant_mountain/2` clause 1 called `epistemic_access_check(C, false)` with `false`
**bound**. The catch-all `epistemic_access_check(_, false).` unifies with everything, so clause 1
fired for every constraint and the four-test body at `:579` had never executed on any corpus.

The defect sits **79 lines below the warning written about this exact idiom** (`:470–477`, added
2026-06-03 by `0bfd3b31` after the `structural_purity/2` episode). The header warns; the file did
it anyway. **The site has no clean parent** — the defect predates the Feb-2026 monolith split
(`a0e8d772^` carries it twice in `structural_signatures.pl`), first introduced `80aebdb3`
2026-02-07 — which is why OQ-302's "git-blame `:577` for the free before-commit control pair" is
corrected here.

**Root cause: the spec prescribed it.** `docs/logic_extensions.md`'s "How to Activate" told
implementers to add `boltzmann_invariant_mountain(C, true)` to `natural_law_signature/1` — a goal
that cannot unify with any of `invariant(_)` / `variant(_)` / `inconclusive(_)`, so it fails for
every constraint and would have *killed* the detector rather than strengthened it. The code
implemented the spec faithfully. Corrected at the origin in `4f8f0e3f`.

## 2. What the repair does and does not buy

**Does not:** make the check informative. Test 4 calls `signature_detection:
natural_law_signature/1`, which requires `HasAlternatives == false`; `has_viable_alternatives/2`
has exactly two clauses and neither can emit `false` (range `{true, unknown}`, OQ-113). Measured
here at full scale: **`T4 = fail(natural_law_signature)` on 5,311/5,311**. No `invariant(_)`
verdict is reachable, so "real invariant/variant verdicts appear for the first time" — OQ-302's
own recon line — is **false**; only `variant` appears.

**Does:** move the payload from constant to per-constraint, and remove a live instance of the
warned-against idiom so the gate rows stop carrying a known-benign row.

| leg | N | N_reaching | distinct (T1,T2,T3) | arm(defect) | arm(repaired) |
|---|---:|---:|---:|---|---|
| `testsets` | 279 | 236 | 129 | inconclusive 279 | variant 236, inconclusive 43 |
| `testsets_haiku` | 960 | 494 | 234 | inconclusive 960 | variant 494, inconclusive 466 |
| `testsets_flash` | 960 | 748 | 133 | inconclusive 960 | variant 748, inconclusive 212 |
| `testsets_kimi` | 1005 | 976 | 224 | inconclusive 1005 | variant 976, inconclusive 29 |
| `testsets_sonnet` | 1001 | 1000 | 263 | inconclusive 1001 | variant 1000, inconclusive 1 |
| `archives/datasets/kernel_v1` | 1106 | 1104 | 270 | inconclusive 1106 | variant 1104, inconclusive 2 |

No leg is at `N_reaching ≤ 1`, so no leg is declared UNMEASURED. Pre-registered disposition
**row 1 (`T1`–`T3` vary)** is the one that fired, so the site comment states per-constraint
payload variation — which the prereg permits only on this branch.

## 3. Controls, graded honestly

- **fires — a WIRING CHECK WITH A KNOWN ANSWER, NOT DISCRIMINATION.** `arm(defect)` =
  `inconclusive(insufficient_data)` on **5311/5311**, every leg. This follows from head
  unification, which *is* the finding, so as evidence it is close to vacuous. It is reported at
  that altitude and graded as a wiring check: it establishes only that the probe reaches the real
  predicate with real constraints.
- **declines — the two-sided control, and it carries the weight.** Subjects were found on every
  leg (43 / 466 / 212 / 29 / 1 / 2 = **753**) and **753/753 still report
  `inconclusive(insufficient_data)` post-repair**. OQ-112's "~92 masked" was demoted to a prior
  in the prereg and did no existence work. **Scoped residue:** `testsets_sonnet` has **n = 1**
  subject — tested, but thin; a leg-level claim there rests on one constraint.
- **transcription cross-check** (PREREG §0b): the probe's own aggregation of `T1`–`T4` reproduced
  the `clause/2`-composed Result on **4,558/4,558** reaching rows, **0 `MISMATCH`**.
- **escalation clause** (`T4 = pass` anywhere): **did not fire**, 0 of 5,311.
- **throw outcome** (the pre-registered third disposition, which would have outranked both
  others): **did not fire** — 0 thrown-error cells and 0 failed cells across 9 value columns × 6
  legs. Never-executed code, un-deadened, threw nothing.
- **enumeration control** — naturally-arising, not a plant: the scan reports **2** bound-`false`
  sites at `0bfd3b31^` and **1** at `0bfd3b31`.
- **pre-flight, per leg:** `files_on_disk` == `[corpus] Loaded N` == `corpus_constraint_count`,
  and leg md5 **identical before and after both arms** on all six.

## 4. The plan's Phase-2 mechanism was REFUSED, not silently repaired

The plan specified both arms via `probe_harness:with_overlay/3`. **Executed as written that
produces exactly the vacuous pair the plan warns against, silently** — so it was reported back to
the design rather than quietly worked around (`build_discipline.md` → *The receiver's license to
refuse*).

Two independent reasons, both measured: `with_overlay/3` snapshots **facts only**
(`probe_harness.pl:91–100`, `clause(M:T, true)`) and the target clause is a **rule**, so it is
never retracted and an asserted replacement would land *after* the defect clause; and the
predicate is **static**, so `assertz` throws. The "repaired" arm would have returned
`inconclusive(insufficient_data)` — byte-identical to the defect arm, for the wrong reason, with
no error.

**Substitute:** the repaired arm executes the engine's **own compiled clause-2 body**, fetched
with `clause/2` at run time. **Declared residual gap:** that is a *composed* program, and the
commit lands a *source edit*. Commit 1's second witness closes it — the probe re-run against the
committed source detected `source_arm=repaired` **by reading the clause**, and the arm-flag-matched
`arm(repaired)` column is **identical on 5311/5311 rows** to Phase 2's.

## 5. What the gate caught that the enumeration did not

Reclassifying the registry row `finding` → `latent-B` turned `codewalk caller` **RED**. Phase 0's
claim — *"`:577` is the only reachable bound-`false` site"* — was correct, but `latent-B` asserts
the broader *"no live BOUND caller"*, and there is one: `boltzmann_compliant/2` at `:94–95`,
bound-**`true`**.

Adjudicated into `prolog/codewalk_caller_allowlist.txt` (`ATOMS=true` + REMOVE condition) on a
**naturally-arising two-sided record this run already held**: `boltzmann_compliant/2`'s only
access gate is that bound-`true` call, and it returns
`fail(factorization, inconclusive(insufficient_classifications))` on exactly the **753**
constraints the unbound call reports `false` for, and a real verdict on exactly the other
**4,558**. It declines where the unbound call declines and fires where it fires — the
head-unification asymmetry the file's own header states at `:476–477`, measured rather than
asserted. `purity_scoring.pl:49` carries the same idiom and is **not** reported by this arm (its
declared blind spot), recorded so the allowlist row is not read as a complete census.

## 6. Residues, declared

1. **`latent-B` is no longer empty (0 → 1).** OQ-303 recorded it EMPTY on 2026-08-19; that line
   is corrected in place. The row is a **genuine class-B conversion candidate on OQ-303's own
   criteria** — output last argument, reached, 5 callers on live output paths — excluded from the
   2026-08-19 rollout only because it was `finding` then. Converting it owes the six-leg pair, not
   the template. **Routed to OQ-303 arm (a); deliberately not done here.**
2. **The orphan pair is an *Unwired ≠ worthless* adjudication, not a retirement.**
   `boltzmann_shadow_audit/2` and `drl_boltzmann_analysis:boltzmann_invariant_check/2` are
   zero-caller static orphans. The question is *what unique product do they yield?* — routed to
   OQ-317 (re-review 2026-11-17). **They may not be retired on wiring grounds.**
3. **`testsets_sonnet`'s declines-control has n = 1.** Tested, thin, declared.
4. **The pipeline clean-vs-edited pair was NOT run and that is deliberate** — with zero live
   consumers it is byte-identical by construction, i.e. a consistency check that cannot fail. The
   declaration lives in `consumer_enumeration.md` §1a so a later reader does not mistake its
   absence for an oversight.
5. **Process deviation, stated rather than hidden:** `audits/README.md` rules that `audit_log.md`
   is committed *before* any code commit. Here it landed in the same commit as the repair
   (`fb10708a`) rather than ahead of it. The freeze stamp is still physically above the first
   result line and the md5 is verifiable, so the ordering claim is witnessable — but the commit
   ordering the rule asks for was not followed.

## 7. Verification (plan §Verification, item by item)

| # | Item | Result |
|---|---|---|
| 1 | gate GREEN before and after | before: GREEN (27 rows, `python env` read first). After: in the close commit. |
| 2 | enumeration scan 2 at `0bfd3b31^`, 1 at `0bfd3b31` | ✔ `bound_false_scan.out` |
| 3 | fires-control 100% on `arm(defect)`, every leg, labelled a wiring check | ✔ 5311/5311 |
| 4 | declines-control has a subject per leg | ✔ all six; `testsets_sonnet` n=1 declared |
| 5 | `N_reaching` per leg; `≤ 1` ⇒ UNMEASURED | ✔ 236/494/748/976/1000/1104; none ≤ 1 |
| 6 | throw count per leg and per test | ✔ 0 everywhere, reported as a result |
| 7 | Commit 1's second witness, matched by arm flag | ✔ 5311/5311 identical |
| 8 | corpus md5 identical per leg around both arms | ✔ all six |
| 9 | `dispatch_head_check` + `bound_selector_check` green with the reclassified row | ✔ (and `codewalk_caller_check` green after adjudication) |
| 10 | tracker checkers green after the edits | in the close commit |
