# OQ-303(a) — class-B conversion rollout: Phase 1 landed zero-diff on six legs, then the batch of 55 moved the corpus and was reverted — the `latent-B` label does not license the template

**Executed:** 2026-08-18 → 2026-08-19 (directory dated to the open)
**OQ:** OQ-303 (arm (a))
**Verdict (updated 2026-08-19 after the ruling):** Phase 1 landed: `signature_grade/2` converted, six-leg pair **0 changed
constraints over 5,311**, so the pre-declared **Ω_P escalation DECLINED** and that conversion is
Ω_E, discharged. **Phase 3 is HALTED and REVERTED**: the batch of 55 passed every structural
check it owed and moved the corpus on all six legs (129/279 up to 1106/1106). A per-file bisect
attributed all of it to two rows whose last argument is an **INPUT, not an output** — the
class-B template is invalid there, and both were misfiled `latent-B` in the 2026-08-17 registry.
**The finding is not "two bad rows": it is that the `latent-B` label does not license the
template on its own**, which is the premise the whole batch rested on. A mechanical
candidate-finder flags 23 of the 58, but it is not clearance: the licence was **rebuilt on the
direct question, per row** (§10–11), which found a **third** misfiled row the corpus could not
(`generate_scenario_for_omega/5`, its file diffed clean) and put the last-argument fact into the
registry with its evidence, gate-enforced. Reachability (§12) bounds what the corpus clearance
covers: **34 of 54 rows are cold on the leg it came from**. **STATUS: HALTED, licence rebuilt**
— the batch stays reverted; Phase 4 not begun; nothing merged to `main`.
**Substrate:** six legs — `testsets` (n=279), `testsets_haiku` (960), `testsets_flash` (960),
`testsets_kimi` (1005), `testsets_sonnet` (1001), `archives/datasets/kernel_v1` (1106) =
5,311 constraints. Clean half at `code_commit` 2f459d3a, edited half at 6c1bfa44, corpus md5
identical per leg across halves. swipl 10.0.2.
**Fired:** live — the batch six-leg pair caught a conversion that every structural check
called clean and that would have silently changed 129–1106 constraints per leg on a live output
surface. Also live: the census's pre-registered control fired on the census itself, and the
transformer's selftest fired three times on the transformer before it touched a tracked file.
**Evidence map:**

| artifact | what it is | which claim it witnesses |
|---|---|---|
| `PREREGISTRATION.md` | frozen before any conversion or six-leg run; md5 `000742e0…` recorded in `audit_log.md` above the first result line | the ordering, the two carries, the Ω_E/Ω_P outcome table, the predicted zero-diff |
| `audit_log.md` | chronological log, R1–R6, with pasted output | every numeric claim below |
| `sixleg.py` | the six-leg pair harness: mtime-advance gate, corpus md5 fingerprint per half, per-leg refusals | §2 — that the pair actually ran on comparable substrates |
| `sixleg_clean_manifest.json`, `sixleg_edited_manifest.json` | per-leg provenance for both halves | §2 |
| `sixleg_diff.txt` | the diff output | §2 |
| `clause_order_census.pl`, `clause_order_census.py` | the steal-risk census + its fail-closed control | §3 |
| `clause_order_census.md`, `clause_order_census_raw.txt` | the census table and raw output | §3 |
| `convert.py` | the template transformer with adversarial parse fixtures | §4 |
| `bisect_batch.py`, `bisect_batch.json` | per-file attribution of the batch diff, with an all-reverted control | §7 |
| `inputkey_screen.py`, `inputkey_screen.json` | the cut-first candidate-finder + its two-sided naturally-arising controls | §8 |
| `sixleg_diff_batchclean_vs_batchedited.txt` | the batch diff | §7 |
| `../2026-08-18_bound_caller_rewitness/` | Unit A: the partition this unit executes | inputs |

---

## 1. Carry (i) — `commentary` reachability, answered before the run

Pre-registered from reads, not read off the diff afterwards (`audit_log.md` R1):
**`commentary` is reachable downstream as a VALUE but is queried NOWHERE as a BOUND SELECTOR.**
`diagnostic_summary:verdict_join/3:728` queries `signature_grade/2` **unbound**; the value is
serialized as `verdict_join.signature_grade` (`json_report.pl:1509`) and read live by
`python/tensions_ledger.py:173`. The engine's only bound caller is
`signature_detection.pl:1951`, at `correction`.

That distinction is what decides Ω_E from Ω_P, which is why it was fixed in advance along with
what each outcome would mean.

## 2. Phase 1 result — zero-diff, and the three ways a zero-diff can lie, closed

```
testsets  0 | testsets_haiku  0 | testsets_flash  0
testsets_kimi 0 | testsets_sonnet 0 | kernel_v1 0        (5,311 constraints)
```

- **Did it run?** Per leg, the output file's mtime was asserted to advance past a pre-run
  marker. A `run_pipeline`-family gate that aborts before writing leaves a stale file that
  diffs byte-identical against itself.
- **Are the halves comparable?** Corpus md5 fingerprinted per leg on both halves and compared
  — identical. Code state differs (2f459d3a vs 6c1bfa44), so the edited half did pick up the
  edit.
- **Can the differ report nonzero?** One field planted in one record of one leg: it names the
  constraint and the field, and declines when restored. Both directions, same session.

**So the Ω_P escalation declined.** That is the result, not a caveat: the escalation was live,
pre-declared, and did not fire. The conversion is Ω_E, discharged.

**The conversion is not a no-op — it is a no-op *at the consumer*.** Re-running Unit A's
agreement probe post-conversion, the bound form at `commentary` moved from 263→234 (testsets)
and 932→899 (haiku), i.e. into exact agreement with the engine, while `correction` was
untouched. Precisely 29 and 33 over-permissive answers retired, and zero consumer-visible
change, because nothing in the engine queries at `commentary`.

## 3. Carry (ii) — the census: the exposure is 165 of 218

`steal_risk(P, A)` = cut-bearing clauses of `P` whose head output arg is an atom ≠ `A`, before
the last clause that can yield `A`. Upper bound by design.

**57 of 58 `latent-B` predicates carry nonzero steal-risk at some atom; 165 of 218 (predicate,
atom) pairs.** Max 17 (`qualify_action/5`), then 14, 13, 11, 10. The single zero,
`resolve_with_perspectival_check/4`, is zero trivially — one atom, nothing to skip.

"May be zero" was the wrong prior, and for a structural reason rather than a surprising one:
these rows are in the registry *because* they carry the shape. What the census adds is
magnitude, granularity, and this: **the `latent-B` label holds all 165 back on one fact alone —
nobody calls them bound.** Unit A already showed that fact resting on an instrument with a
proven false negative.

**The control caught the census.** The first `steal_risk` treated a variable-headed cut-bearing
clause as an unconditional commit and reported 0 at *both* of `signature_grade/2`'s atoms — a
clean-looking table contradicting a five-leg measurement. A cut is not reached unless the body
reaches it. The driver fails closed on that check, so the corrected split is a precondition of
any zero in the table.

**And it hands the batch a by-construction witness the corpus diff cannot give.**
Post-conversion the census must read 0 of 218; `signature_grade/2` already reads
`atoms=0 max_steal_risk=0`.

**The two instruments are NOT ranked, and §7 is why.** An earlier draft of this unit had the
census as the primary witness and the six-leg pair as corroboration. That ordering is falsified:
the census read 0 of 218 **clean** on a batch that moved 129–1106 constraints per leg. It counts
atom-headed output clauses, so it is structurally blind to *"this argument was never an output"* —
a misconversion that makes later clauses unreachable leaves it nothing to count. The census sees
differences the corpus may never exercise; the corpus sees a failure class the census cannot
represent. Different coverage, no ordering.

## 4. The transformer, and why it is a transformer

55 hand edits and one transformer make different *kinds* of mistake. A hand edit can drop a
guard in a way no structural check catches; a transformer makes the same mistake everywhere,
where the checks see it. The checks license it, not the code: clause count and cut-presence
compared before/after via the Prolog reader, no bare atom left in an output position, the file
still loads, then the batch-level six-leg pair and gate.

**Its fixtures fired three times before it touched a tracked file:** a `0'c` char-code
off-by-one that swallowed the rest of a file; comment text absorbed into the following clause's
span — which produced the *right* answer on `signature_detection.pl` by layout luck; and an
over-strong post-condition ("every output is a variable") that would have reverted any file
with a compound output argument. The second is the one worth keeping: a parser that is wrong
and right-by-accident on the file you tested it on is the failure mode this whole arc is about.

## 5. A tool that could not run the witness it exists for

`classify_corpus`'s `run_prolog` timeout is a hard-coded 300 s, sized on the live leg (~35 s).
Measured this session: haiku **288 s** (13 s of headroom), flash **530 s**, sonnet **724 s**,
kernel_v1 **577 s**. The first pair attempt burned three full-length attempts and refused on
flash. Fixed by a `timeout`/`soft_timeout` pass-through, default unchanged. Worth naming
because the failure was *loud and correct* — the harness refused rather than emitting a partial
— and still cost a full run to discover, since nothing about the default announced that it was
sized for one leg out of six.

## 7. Phase 3 — the batch moved the corpus, and every structural check said it would not

292 clauses across 28 files. Post-conversion: 0 atom-headed output clauses remained across all
57 predicates (2 compound-headed, declared), `dispatch_head_check` dropped 69 → 12 hits, the
gate was GREEN, the OQ-137 totality gate passed 10/10. Then:

```
testsets 129/279 | haiku 640/960 | flash 504/960 | kimi 1005/1005 | sonnet 538/1001 | kernel_v1 1106/1106
```

Corpus md5 identical per leg across halves; code state different. **This is the entire case for
running the six-leg pair on the batch rather than the prereg's one-leg floor:** every check
internal to the change passed. Only the corpus dissented.

Per-file bisect, with an all-reverted control that reproduces the baseline exactly (so the
attribution measures conversions, not drift): **2 of 29 files**, `abductive_helpers.pl` (129,
`seat_overrides/2`) and `boltzmann_compliance.pl` (17, `expected_power_divergence/4`).

## 8. The mechanism, and why it is bigger than two rows

```prolog
seat_overrides(C, false_ci_rope) :- !, \+ signature_detection:fcr_routed(C).
expected_power_divergence(powerless, institutional, _, _) :- !.
```

Neither last argument is an output. The first is a semidet test keyed on the signature; the
second is a pure semidet test with no output at all. Converted,
`seat_overrides(C, T) :- !, guard, T = false_ci_rope.` matches **every** second argument, cuts,
and makes every later clause unreachable.

`dispatch_head_check.pl:9-11` states the violated assumption in its own header — *"OUTPUT
ARGUMENT is taken to be the LAST argument, by engine convention. This is a declared assumption,
not a fact about every predicate"* — and the registry already carries the exception class,
`input-key`. These two were filed `latent-B`.

**Unit A's partition inherited the misclassification, and could not have caught it.**
`converts-clean` means "no bound caller under both instruments" — a question that *presupposes*
the last argument is an answer. Both arms answered it correctly and the answer was irrelevant.

**The screen.** A clause whose body's FIRST goal is `!` commits before testing anything, so it
is selecting on its head arguments. `scan_file_clause_shapes/2` (additive export, no second
walker) + `inputkey_screen.py`, controls naturally arising and asserted in-process: fires on
both corpus-attributed rows, declines on `signature_grade/2`. **It flags 23 of 58.**

**It is a candidate-finder, not a verdict.** A cut-first clause can still have a genuine output
last argument — `characterize_family/2` cuts first but selects on argument 1, so its last
argument really is a description to compute. 2 confirmed, **21 unadjudicated**. Only the 2 are
reclassified `input-key`; reclassifying 21 on a candidate-finder's say-so would be the same
error in the other direction.

## 10. The licence, rebuilt on the direct question — and the fact was already in the source

Operator ruling 2026-08-19: *"mechanical per a witnessed template"* is falsified because its
unstated premise — every `latent-B` row's last argument is an OUTPUT — is false at 2 rows,
unadjudicated at 21, and **unexamined** at the rest. Absence of the cut-first tell is not
presence of an output, and adopting a 2-instance-tuned heuristic as clearance one unit after
Unit A spent itself correcting exactly that shape would be the same error mirrored.

So the licence was rebuilt on the direct question, asked of **every** remaining row:
*does this predicate's last argument carry an answer out?*

**And the answer was already written in the source.** Both misfiled rows carry an authored mode
line three lines above their clauses:

```
%% seat_overrides(+C, +Signature)
%% expected_power_divergence(+P1, +P2, +T1, +T2)
```

Last argument `+`. INPUT. The fact the registry was supposed to encode was authored by hand,
sitting immediately above the code, and **nothing read it** — which is precisely how
`dispatch_head_check.pl:9-11` could state the last-arg-is-output assumption in a header while
two violations sat inside its own worklist. A stated assumption is not a checked one.

`mode_adjudication.py` reads it, with the two attributed rows as its FIRES control and
`signature_grade/2` (`%% signature_grade(+Constraint, -Grade)`) as its DECLINES control:

| | count |
|---|---|
| authored OUTPUT (`-`/`?` last arg) | 36 |
| authored INPUT (`+` last arg) | **1 — a THIRD misfiled row** |
| no mode line, adjudicated by hand read | 18 |

**The third misfiled row is `report_generator.pl generate_scenario_for_omega/5`** —
`%% generate_scenario_for_omega(+OmegaID, +Type, +Description, +Constraint, +GapPattern)`, every
argument an input. The corpus did **not** catch it: its file diffed `changed=0`. It is the latent
instance the ruling predicted, and the mode read found it where the corpus could not.

All 18 undeclared rows read as output-bearing, each with its evidence recorded in the registry
(§11). One deserves naming: `structural_property_holds/2` is not a dispatch predicate at all but
a **generator** — its sole caller is `findall(Prop, structural_property_holds(C, Prop), _)` at
`logical_fingerprint.pl:161` — so its last argument is an output enumerated on backtracking and
there is no bound-probe hazard for the template to retire.

**Cross-check, not clearance** (the partition's two-instrument discipline): mode read vs
cut-first screen, disagreement either way being the finding.

- **10 rows the screen flags where the author declares an output** — the screen over-flags, as
  `characterize_family/2` already suggested (it cuts first but selects on argument 1).
- **0 rows where the author declares an input and the screen missed it.**

So the screen has no false negatives *among the mode-declared rows* and a 10/46 false-positive
rate; it is ordering and cross-check, never clearance, and neither instrument clears a row alone.

## 11. Registry hygiene — the durable fix

`python/dispatch_head_check.py` now carries `LAST_ARG`: **one row per registry entry, its
verdict, and the evidence that settled it** (the authored `%%` line, or a dated read naming what
decided it). 57 rows, 54 output, 3 input.

Gate-enforced in the same checker, because a fact nothing checks is what produced this unit:

- a `latent-B` row with **no** `LAST_ARG` fact is **RED** — the class licenses the template, and
  the template is valid only on an output;
- a `latent-B` row recorded `LAST_ARG=input` is **RED** — it belongs in `input-key`.

All three input rows are reclassified `latent-B` → `input-key`. The class definition in the
checker now carries the mechanism and names both instruments.

## 12. Reachability — what the corpus clearance actually covers

The per-file bisect cleared 27 of 29 files at `changed=0` on the **live leg**. A cold predicate
clears that on absence of exercise, not correctness — so the clearance is bounded by what the
leg reaches, and that is measurable rather than assumable.

`reachability.py` profiles the same `run_json_report` goal and reads per-predicate call counts.
Controls fire hard in the same run (`dr_type/3` 159,777 calls, `classify_from_metrics/6`
188,901, `constraint_signature/2` 183,649 — 1,248 predicates observed).

**34 of 54 latent-B rows are COLD on `testsets`.** For those, the bisect's `changed=0` is an
absence-of-exercise result and licenses nothing. 20 rows are genuinely exercised, and for those
the corpus clearance is real evidence with a demonstrated firing control.

This is a `testsets`-only measurement, matching the bisect's own scope. Extending it to the
other five legs would shrink the cold set; that run is not done, and the number above should be
read as "cold on the leg the clearance came from", not "cold everywhere".

## 13. Residue — what is OPEN

- **Phase 3 is HALTED and reverted.** It needs an operator ruling, not a retry: 21 rows carry
  the input-key tell and are unadjudicated, and adjudicating "is this last argument an output?"
  is a per-row reading, not a mechanical pass. The prereg's §5 licence ("template application,
  no six-leg run each") was written on the assumption the class was uniform; it is not.
- **The rebuilt licence is now in place, and it is per row rather than per screen:** 54 rows
  carry an adjudicated `LAST_ARG=output` fact with evidence, gate-enforced. What that licenses
  is the template's *validity*, not its safety on any particular row — 34 of those 54 are cold
  on the leg the corpus clearance came from, so for them the remaining witness is the
  adjudication and the by-construction census, not the diff.
- **Not resumed on my own authority.** The batch stays reverted. The licence was rebuilt to the
  ruling's specification; whether to spend it, and on which rows, is the next decision and it is
  yours. My reading of the evidence: the 20 exercised rows carry adjudication + corpus + census;
  the 34 cold rows carry adjudication + census only, and a per-leg reachability pass would move
  some of them into the first group before any conversion needs to rely on the weaker footing.
- **Phase 4 not begun** (`json_report.pl` `boltzmann_label/2`, `live_index_label/3` — both are
  in the flagged 23, so they were never as safe as `converts-clean-minus-dataflow` suggested).
  **Ordering them last was right for a reason that did not exist when the ordering was chosen.**
  The prereg put them last because they carried the least *evidence* (no `evaluate(true)`
  codewalk verdict); the input-key tell that actually condemns them was not discovered until
  Phase 3 failed. Worth recording as luck, not foresight — the general form ("sequence the
  least-witnessed work last") paid out against a hazard it was not aimed at, which is an
  argument for the heuristic and not for the reasoning that produced this instance of it.
- **Nothing merged to `main`.** Branch `oq303-classb-rollout`.
- **`caller_sweep.py` is not retired by any of this**, and OQ-303(c) is untouched.
