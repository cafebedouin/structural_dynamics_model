# audit_log — OQ-303(a) class-B conversion rollout (Unit B)

Chronological. Every result line below the PREREGISTRATION md5 line was produced after that
freeze. Nothing above it is a result.

---

## OPEN — 2026-08-18

**HEAD stamp (OPEN):** `161707bb1832d40f3a418b615bee89a827fd6da0`, working tree clean.
Branch `oq303-classb-rollout` cut from it — this unit is multi-file and semantics-changing by
construction, which is the branch case in `CLAUDE.md` *Git autonomy*.

**Input:** `audits/2026-08-18_bound_caller_rewitness/partition.md` — 55 `converts-clean`,
1 `live-output-path`, 2 `converts-clean-minus-dataflow`.

### R1 (pre-freeze reads) — the `commentary` reachability question, answered by reading

Taken BEFORE the prereg was frozen, and written into it as the claim the pair will confirm or
refute. Recorded here as reads, not as results.

```
$ grep -rn "signature_grade" prolog/ python/ agent/ --include=*.pl --include=*.py | grep -v testsets
prolog/diagnostic_summary.pl:728:    (   signature_detection:signature_grade(C, SigGrade)
prolog/json_report.pl:1509:        format(S, '        "signature_grade": ', []),
prolog/signature_detection.pl:1952:    signature_grade(C, correction).
python/tensions_ledger.py:173:    grade = vj.get("signature_grade") if vj else None
python/shared/schemas.py:547:                       "signature_grade"):
```

```
$ sed -n '726,732p' prolog/diagnostic_summary.pl
    (   signature_detection:signature_grade(C, SigGrade)
    ->  true
    ;   SigGrade = none
    ),
    Join = verdict_join(Joined, Base, Cap, Alerts, GridProv, MeasProv,
                        SigGrade).
```

**The answer, and it is a distinction rather than a yes/no:** `commentary` is reachable
downstream as a **VALUE** — `verdict_join/3` queries `signature_grade/2` **unbound** at
`:728`, the result is serialized as `verdict_join.signature_grade`
(`json_report.pl:1509`) and read live by `python/tensions_ledger.py:173`. It is queried
**nowhere as a BOUND SELECTOR**; the engine's only bound caller is
`signature_detection.pl:1951`, at `correction`.

That distinction is what decides Ω_E vs Ω_P, so it is pre-registered with its consequences
(PREREGISTRATION §2) rather than left to be read off the diff afterwards.

---

## PREREGISTRATION FROZEN — 2026-08-18, before any conversion or six-leg run

```
$ md5sum audits/2026-08-18_classb_conversion_rollout/PREREGISTRATION.md
000742e0039894e98855c1187935aea4  audits/2026-08-18_classb_conversion_rollout/PREREGISTRATION.md
```

**Everything below this line is a result.**

---

## RESULTS — 2026-08-18

### R2. The clause-order census, and the control catching the census

Carry (ii). `audits/.../clause_order_census.pl` reuses `dispatch_head_check`'s ordered clause
reader through a new additive export `scan_file_clauses/2` — no second reader of the same
source (Pattern 2).

**The pre-registered control fired on the first version of the census, before any table was
read.** That version treated a variable-headed clause carrying a cut as an *unconditional*
commit and stopped scanning there:

```
COC_ROW: signature_detection.pl signature_grade/2 atom=commentary steal_risk=0 skipped=[] nclauses=3
COC_ROW: signature_detection.pl signature_grade/2 atom=correction steal_risk=0 skipped=[] nclauses=3
```

Zero at BOTH atoms — a clean-looking table, and wrong: Unit A measured `commentary` diverging
by 29–167 constraints per leg. A cut is not reached unless the body reaches it; a failing guard
falls through. Corrected model (head unification, not the cut, is what a bound query changes):

> `steal_risk(P, A)` = cut-bearing clauses of `P` whose head output arg is an atom ≠ `A`,
> appearing before the **last** clause that can yield `A` ("can yield" = output arg is `A` or a
> variable).

```
COC_ROW: signature_detection.pl signature_grade/2 atom=commentary steal_risk=1 skipped=[correction] nclauses=3
COC_ROW: signature_detection.pl signature_grade/2 atom=correction steal_risk=0 skipped=[] nclauses=3
```

Now matching the five-leg measurement exactly, in both directions. The driver **fails closed**
on this check rather than reporting, so the corrected split is a precondition of any zero in
the table.

### R3. Census result — NOT zero; 57 of 58, and the one exception is trivial

```
$ .venv/bin/python audits/2026-08-18_classb_conversion_rollout/clause_order_census.py
clause_order_census: control OK (signature_grade/2 correction=0, commentary=1)
  58 latent-B predicates, 218 (pred, atom) pairs
  57 predicate(s) with nonzero steal-risk at some atom
  165 (pred, atom) pair(s) with nonzero steal-risk
```

max steal-risk distribution across the 58:

```
  1 x 0     17 x 1     8 x 2     9 x 3     9 x 4     2 x 5     4 x 6
  2 x 7      1 x 8      1 x 10    1 x 11    1 x 13    1 x 14    1 x 17
```

The single zero is `signature_detection.pl resolve_with_perspectival_check/4`, and it is zero
for a trivial reason: **one atom**, so there is nothing for a bound query to skip. Worst rows:
`qualify_action/5` (17 atoms, max 17), `drift_severity/3` (14), `composition_rule/3` (13),
`cell_short/2` (11).

**Reading it honestly.** "May be zero" was the wrong prior, and the reason is structural rather
than surprising: these rows are in the registry *because* they carry the bound-probe shape, so
pre-conversion a nonzero steal-risk is close to definitional. What the census adds is the
magnitude and the granularity — **165 of 218 (predicate, atom) pairs are hazardous-if-called-bound
today**, and the `latent-B` label is holding all of that back on one fact alone: nobody calls
them bound. That is the exposure the conversion retires, and it is larger than "55 mechanical
edits" makes it sound.

**It also hands the conversion a by-construction witness stronger than any corpus diff.** After
conversion the census must read **0 of 218** — every head fresh-variable, nothing to skip. A
corpus diff can only say "no story exercised the difference"; the census says "the difference
is gone." Both are run.

### R4. Phase 1 — `signature_grade/2` converted; six-leg pair ZERO DIFF; prediction held

Conversion applied by `convert.py` (its own selftest green first). The diff:

```
-signature_grade(C, correction) :-                     +signature_grade(C, T) :-
     ... guard unchanged ...                                ... guard unchanged ...
-    !.                                                +    !,
-signature_grade(C, commentary) :-                     +    T = correction.
-    constraint_signature(C, _), !.                    +signature_grade(C, T) :-
                                                       +    constraint_signature(C, _), !,
                                                       +    T = commentary.
```

**By-construction witness** — the census on the converted predicate:

```
COC_PRED: signature_detection.pl signature_grade/2 atoms=0 max_steal_risk=0
```

**By-corpus witness** — the six-leg clean-vs-edited pair, 5,311 constraints:

```
$ .venv/bin/python audits/2026-08-18_classb_conversion_rollout/sixleg.py diff
testsets         n=  279  changed=0
testsets_haiku   n=  960  changed=0
testsets_flash   n=  960  changed=0
testsets_kimi    n= 1005  changed=0
testsets_sonnet  n= 1001  changed=0
kernel_v1        n= 1106  changed=0
sixleg diff: 0 changed constraint(s) across 6 legs
```

**The pair actually ran, and the halves are comparable** — the two things that make a
zero-diff a false pass if unchecked:

```
testsets         clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
testsets_haiku   clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
testsets_flash   clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
testsets_kimi    clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
testsets_sonnet  clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
kernel_v1        clean=2f459d3a dirty=True   edited=6c1bfa44 dirty=True   corpus_md5 same=True
```

Different code state per half; identical corpus md5 per leg across halves. Each leg's output
mtime was also asserted to advance past a pre-run marker (`sixleg.py`), so no half compares a
stale file against itself.

**The differ discriminates — a zero from a check that cannot fail witnesses nothing.** One
field planted in one record of one leg:

```
planted beneficiaries='__PLANTED__' on constraint ability_ceiling_reading
testsets         n=  279  changed=1
    first 10: ['ability_ceiling_reading']
      ability_ceiling_reading: beneficiaries: ['high_performing_learners', 'meritocratic_sorting_institutions'] -> '__PLANTED__'
sixleg diff: 1 changed constraint(s) across 6 legs
--- restored ---
sixleg diff: 0 changed constraint(s) across 6 legs
```

Fires on a single field in a single record, names the constraint and the field, declines when
restored. Same session, both directions.

**And the conversion did what it is FOR — the bound form now agrees with the engine.**
Re-running the Unit A agreement probe post-conversion:

```
                    BEFORE                              AFTER
testsets            commentary: bound=263 unb=234       commentary: bound=234 unb=234
testsets_haiku      commentary: bound=932 unb=899       commentary: bound=899 unb=899
                    correction: bound=45  unb=45        correction: bound=45  unb=45
                    correction: bound=61  unb=61        correction: bound=61  unb=61
```

The 29 and 33 over-permissive `commentary` answers are gone; `correction` is untouched — which
is exactly why the corpus diff is zero (nothing in the engine queries at `commentary`).

### R5. The pre-registered verdict, discharged as pre-registered

PREREGISTRATION §2 fixed the outcomes before the run:

- **Zero-diff across all six legs ⇒ Ω_E, discharged; the Ω_P escalation does NOT fire.**
  That is the result. It is a finding, not a caveat dropped: the escalation was live and
  declined.
- The predicted result was recorded as **ZERO DIFF** before the run, and it held. The
  reasoning it rested on (consumer-facing caller unbound + template output-preserving for
  unbound callers; the one bound caller at `correction`, measured identical) is therefore
  supported rather than merely consistent.

### R6. Registry and allowlist entries retired, per their own stated conditions

```
$ .venv/bin/python python/dispatch_head_check.py --check
dispatch_head_check: GREEN — 126 engine files, 69 shape hit(s) all declared (69 declared + 3 must-not-fire), 0 file(s) with read errors, selftest OK

$ .venv/bin/python python/codewalk_caller_check.py --check
codewalk_caller_check: GREEN — 73 registry spec(s) (57 latent-B), 99 loaded module(s), ...
```

70 → 69 declared shape hits; 58 → 57 `latent-B`. The allowlist row's REMOVE condition
("deleted when the class-B conversion of signature_grade/2 lands with its six-leg pair") was
met and the row is gone, so the allowlist is now empty of rows — retirement in the same change
as the repair, which is the orphaning rule.

### R7. Phase 3 HALTED — the batch of 55 is NOT output-preserving

`convert.py --all` converted 292 clauses across 28 files. Post-conversion the by-construction
witness was clean (0 atom-headed output clauses left across all 57; 2 compound-headed clauses
declared), the gate was GREEN, and the OQ-137 totality gate passed 10/10. **The corpus said
otherwise.**

```
$ sixleg.py diff batchclean batchedited
testsets         n=  279  changed= 129
testsets_haiku   n=  960  changed= 640
testsets_flash   n=  960  changed= 504
testsets_kimi    n= 1005  changed=1005
testsets_sonnet  n= 1001  changed= 538
kernel_v1        n= 1106  changed=1106
```

Provenance checked before reading any of it: corpus md5 identical per leg across halves, code
state different (6c1bfa44 vs 2fec71fe), and the differ's discrimination record already
established in R4.

**This is why the batch got a six-leg pair rather than the prereg's one-leg floor.** Every
structural check the batch owed came back clean. Only the corpus dissented.

### R8. Per-file attribution — 2 of 29 files, and the control makes the attribution readable

```
CONTROL all-reverted vs batchclean: changed=0
  abductive_helpers.pl               changed= 129  (fpn_band/2, seat_overrides/2)
  boltzmann_compliance.pl            changed=  17  (expected_power_divergence/4)
  [27 other files]                   changed=   0
2 of 29 files move the corpus on their own; sum of independent changes = 146
(whole batch moved 129 — a shortfall means files interact)
```

The all-reverted control is the load-bearing line: with every conversion reverted the probe
reproduces the baseline exactly, so the attribution is measuring conversions and not drift.

### R9. The mechanism — the template is invalid where the last argument is an INPUT

```
$ git show 348770f5:prolog/abductive_helpers.pl | sed -n '90,91p'
seat_overrides(C, false_ci_rope) :- !, \+ signature_detection:fcr_routed(C).
seat_overrides(C, constructed_high_extraction) :- !, \+ signature_detection:constructed_routed(C).

$ git show 348770f5:prolog/boltzmann_compliance.pl | sed -n '350,353p'
expected_power_divergence(powerless, institutional, _, _) :- !.
expected_power_divergence(institutional, powerless, _, _) :- !.
expected_power_divergence(powerless, analytical, _, _) :- !.
expected_power_divergence(analytical, powerless, _, _) :- !.
```

Neither predicate's last argument is an output. `seat_overrides/2` is a semidet TEST keyed on
the signature; `expected_power_divergence/4` is a pure semidet test with no output at all.
Converted, `seat_overrides(C, false_ci_rope) :- !, guard.` becomes
`seat_overrides(C, T) :- !, guard, T = false_ci_rope.` — which **matches every second
argument, cuts, and makes every later clause unreachable.**

`dispatch_head_check.pl:9-11` states the assumption this violates in its own header: *"OUTPUT
ARGUMENT is taken to be the LAST argument, by engine convention. This is a declared assumption,
not a fact about every predicate."* The registry even carries a class for the exception —
`input-key` — and these two were filed `latent-B`. **The misclassification is in the 2026-08-17
registry, and Unit A's partition inherited it: `converts-clean` answered "no bound caller", a
question that presupposes the last argument is an answer at all.**

### R10. The screen, and the 21 latent instances

Mechanical tell: **a clause whose body's FIRST goal is `!`** commits before testing anything,
so it is SELECTING on its head arguments. Added as `scan_file_clause_shapes/2` (additive export
on the existing reader) + `inputkey_screen.py`.

```
$ .venv/bin/python audits/2026-08-18_classb_conversion_rollout/inputkey_screen.py
inputkey_screen: controls OK (fires on the 2 corpus-attributed rows, declines on signature_grade/2)
  23 of 58 latent-B rows are INPUT-KEYED (cut as first body goal) — the template is invalid for these:
    abductive_helpers.pl           seat_overrides/2                  clauses [1, 2, 3]  <- corpus-attributed
    boltzmann_compliance.pl        expected_power_divergence/4       clauses [1..16]    <- corpus-attributed
    covering_analysis.pl           cell_short/2                      clauses [1..12]    <- LATENT
    covering_analysis.pl           sigma_label/2                     clauses [1..5]     <- LATENT
    data_repair.pl                 source_class/2                    clauses [1]        <- LATENT
    diagnostic_summary.pl          compute_verdict/4                 clauses [3, 4]     <- LATENT
    diagnostic_summary.pl          mismatch_source/2                 clauses [1]        <- LATENT
    drl_boltzmann_analysis.pl      qualify_action/5                  clauses [2,6,10,14,15,16,17] <- LATENT
    drl_composition.pl             composition_rule/3                clauses [1..13]    <- LATENT
    drl_counterfactual.pl          estimate_impact_indexed/5         clauses [4]        <- LATENT
    gap_diagnostic.pl              gate_description/2                clauses [1..8]     <- LATENT
    giant_component_analysis.pl    would_cross_threshold/5           clauses [1,2,3]    <- LATENT
    invertibility_analysis.pl      predict_rope_snare/4              clauses [1..6]     <- LATENT
    invertibility_analysis.pl      predict_rope_tangled/4            clauses [1..4]     <- LATENT
    invertibility_analysis.pl      predict_snare_tangled/4           clauses [1..4]     <- LATENT
    invertibility_analysis.pl      predict_three_type/4              clauses [1..4]     <- LATENT
    json_report.pl                 boltzmann_label/2                 clauses [1..7]     <- LATENT
    json_report.pl                 live_index_label/3                clauses [1]        <- LATENT
    metric_drift_events.pl         drift_severity/3                  clauses [4,7,10..13,15,18,19,22] <- LATENT
    orbit_report.pl                characterize_family/2             clauses [1..6,8..12] <- LATENT
    probe_oq197_controls.pl        status_kind/2                     clauses [1,2,3]    <- LATENT
    report_generator.pl            generate_scenario_for_omega/5     clauses [1]        <- LATENT
    signature_detection.pl         resolve_with_perspectival_check/4 clauses [3]        <- LATENT
  35 rows carry no cut-first clause
```

Controls are naturally arising and asserted in-process: it must FIRE on the two the corpus
attributed and DECLINE on `signature_grade/2` (converted, zero-diff over 5,311 constraints).

**The screen is a CANDIDATE-FINDER, not a verdict, and saying so is the honest reading.** A
cut-first clause can still have a genuine output last argument — `characterize_family/2` cuts
first but selects on argument 1, so its last argument really is a description to compute. So 21
of the 23 are *unadjudicated candidates*, not confirmed defects. What is confirmed is narrower
and worse: **the `latent-B` label does not license the template on its own**, which is the
premise the whole 55-row batch was resting on.

### R11. Batch REVERTED; Phase 1 retained

```
$ git checkout -- prolog/ python/dispatch_head_check.py
$ git status --porcelain | grep -E "prolog/|dispatch_head"
(empty)
```

The 55-row batch is gone from the tree. Phase 1 (`signature_grade/2`, committed at 348770f5
with its own zero-diff six-leg pair) stands. The two corpus-attributed rows are reclassified
`latent-B` → `input-key` in the registry, with the mechanism and the screen named at the class
definition. The other 21 are left `latent-B` and flagged as needing adjudication — reclassifying
them on a candidate-finder's say-so would be the same error in the other direction.

Gate GREEN.
