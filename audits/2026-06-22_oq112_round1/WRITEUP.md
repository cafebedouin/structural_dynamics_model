# OQ-112 Round 1 — C4a (item 1) resolved + full 8-class member sort

**Date:** 2026-06-22  **Corpus:** live `testsets`, **LIVE=92** (pinned, see Round 0).
**Code:** working tree on `main` at `ab8d1d7` (+ this round's `diagnostic_summary.pl` fix).
**Method:** separated passes — read-only deciding pass (sort) precedes the write pass (fix);
every verdict below cites a pasted probe or code read, not the census prose.

---

## Round 0 — corpus pin (prerequisite, self-witnessing)

Inherited verdicts in the OQ-112 entry are conditioned on **62** (census + item-4 trace) and
**194** (item-3 NEUTRAL) row corpora. Different sizes are different *sets*; they are facts about
their corpus, not standing engine facts. The pin establishes the round-1 denominator.

- **Membership emitted, not a count.** `pinned_corpus.txt` lists all 92 `corpus_constraint/1`
  ids + manifest (`pipeline_run_at=2026-06-22T02:03:39Z`, `code_commit_short=ab8d1d7`,
  `n_constraints=92`). **LIVE=92** — printed by the run, not pre-committed; the membership makes
  the count self-witnessing (a silent default-`testsets` fallback would show the same ids only
  because `testsets` *is* the live leg — distinguished by the negative control below).
- **Negative control (overlay-took ≠ default-fallback).** (A) `asserta(config:param(corpus_path,
  'no_such_dir_xyz'))` → loader throws `error(corpus_empty(.../no_such_dir_xyz/*.pl), ...)` —
  fails loud, proving the asserted path is honored. (B) overlay `testsets_haiku` → `HAIKU_LIVE=960`,
  distinguishably different membership/count. So the 92 is a real load, not a silent default.
- **Consumer-predicate check.** `json_report.pl:64` enumerates the diagnostic path via
  `findall(C, corpus_loader:corpus_constraint(C), CRaw)` — with an explicit comment (`:57–59`)
  that it does **not** union `constraint_metric`/`classification` facts (which would pick up engine
  demos). The pinned predicate **is** the consumer's table, not a sibling.

**Carried, not re-witnessed:** "all upstream cross-refs resolved, no blockers" is the prior
investigation's claim, taken on faith and named as carried.

**Item-4 / item-3 re-witness obligation:** the item-4 SILENT verdict (62 rows) and item-3 NEUTRAL
(194 rows) are NOT standing facts on 92; they re-anchor in their own rounds (staged below). Round 1
needed the pin only so item-1's control is trustworthy.

---

## Round 1A — member-level sort of C4a (the 13 `; Signal = agrees` else-branches)

C4a = the 13 `Signal = agrees` else-branches in `diagnostic_summary.pl`'s probe predicates
(census §4). **The discriminator** (confirmed by reading all 12 probes, `:150–341`):

> An `agrees` is **sound** when it is reached *after the subsystem predicate SUCCEEDED* with a
> positive no-tension result (`none` / `[]` / `H1=0` / no-override / good purity zone). It is a
> **defect** when reached because the subsystem predicate *FAILED or its datum is ABSENT* (the
> `catch(..., _, fail)` else-branch) — there `agrees` folds didn't-look into the agreement count
> that feeds the OQ-98 verdict join. The 6 sibling probes already use `inconclusive` for exactly
> this case; only `probe_abductive`/`probe_signature` used `agrees`.

### Per-member verdict (read witness pasted)

| Site | Probe | Branch | Verdict |
|------|-------|--------|---------|
| `:154` | maxent | `DisInfo = none` → agrees | **sound** — maxent ran, measured no disagreement |
| `:163` | maxent | fallthrough → agrees | **defect (unreachable)** — see trace below |
| `:173` | cohomology | `descends(DetType)` → agrees | **sound** — measured descent match |
| `:179` | cohomology | `H1 =:= 0` → agrees | **sound** — measured zero obstruction |
| `:196` | abductive | nonempty triggers, none genuine → agrees | **sound** — measured, no genuine trigger |
| `:198` | abductive | **no `abd_triggers` fact / empty → agrees** | **DEFECT (live)** — data-absence |
| `:210` | signature | signature exists, no seat override → agrees | **sound** — measured, no override |
| `:212` | signature | **`constraint_signature` fail/absent → agrees** | **defect (unreachable)** — data-absence |
| `:234` | purity | pristine/sound zone → agrees | **sound** — measured purity |
| `:237` | purity | non-mountain/rope type → agrees | **sound** — purity not required for type |
| `:268` | fingerprint_voids | `Voids = []` → agrees | **sound** — measured no voids |
| `:272` | fingerprint_voids | non-extractive voids → agrees | **sound** |
| `:274` | fingerprint_voids | non-mountain/rope type → agrees | **sound** |

10 sound, 3 candidate defects (`:198`, `:212`, `:163`) — matches the inherited breakdown. The
three candidate-defect verdicts are now sharpened empirically:

### `:198` (`probe_abductive`) — LIVE defect, fires on 13 constraints

The producer (`abductive_report.pl:401–404`) enumerates only constraints with ≥1 `abd_hypothesis`,
**omitting the 13 with none**. The loader (`json_report.pl:121–138`) asserts `abd_triggers/2` only
for the 79 present in `outputs/abductive_data.json`. So 13 of 92 reach `:198` with no fact and got
`agrees`:

```
# probe_before.tsv, has_abd=no rows (abductive signal + join):
actinide_replenishment_mechanism_contradictions   abd=agrees   join=yellow
architectural_pattern_validity                     abd=agrees   join=yellow
demographic_resource_allocation                    abd=agrees   join=yellow
demographic_skill_mismatch_c0                      abd=agrees   join=yellow
digital_money_legitimacy_contradictions            abd=agrees   join=yellow
knowledge_legitimacy_biomedicine_contradictions    abd=agrees   join=yellow
nicene_creed_authority__liturgical_habituation_reading  abd=agrees  join=green
performance_legitimacy_contradictions              abd=agrees   join=yellow
polaris_document_status_contradictions             abd=agrees   join=yellow
propagation_speed_asymmetry                        abd=agrees   join=yellow
scale_ceiling_c0                                   abd=agrees   join=yellow
validation_judgment_separation                     abd=agrees   join=yellow
visual_evidentiary_authority_contradictions        abd=agrees   join=yellow
```

This is the channel-level Pattern 6: the Python enrich side already distinguishes
`None` (file absent → unavailable) from `abd_data.get(cid, [])` (`[]` measured-empty)
(`enrich_pipeline_json.py:164–169`); the **Prolog consumer is the only site that collapses them.**
`abd_triggers/2` is declared `:- dynamic` (`json_report.pl:44`), so even a missing file leaves the
subsystem "available" (`diagnostic_summary.pl:68–69`) and every constraint would hit `:198` →
`agrees` — the catastrophic form (file-missing reads as universal agreement). **Verdict:
`data-absence-defect`.**

### `:212` (`probe_signature`) — defect by statute, but UNREACHABLE (latent)

`NO_CONSTRAINT_SIGNATURE=0` on the pinned 92 — all 92 have a signature. `constraint_signature/2`
is **total**: metric-less ids hit the `\+ profile_metrics_authored(C), !` clause → `unknown`
(`signature_detection.pl:136–137`); metric-bearing ids fall to the profile classifier, which ends
in the total catch-all `classify_by_signature(_, _, ambiguous)` (`:353`). The constructed control
confirms even a fake id gets a signature:

```
CTRL :212 constraint_signature(fake) = unknown   (TOTAL; :212 unreachable)
```

I could not construct a firing case — which is itself the finding. **Verdict:
`data-absence-defect (unreachable/latent)`**; fixed as fail-closed hardening per the operator
guardrail (the fix lands regardless of firing — a non-firing site is a latent green-over-absence).

### `:163` (`probe_maxent`) — UNREACHABLE catch-all (the "needs-deeper-trace" item)

`classify_disagreement/7` (`maxent_classifier.pl:464–476`) is **total over exactly 5 shapes**:
`residual_override`, `hard`, `soft`, `entropy_flag`, `none` (final catch-all clause `:476`).
`probe_maxent` handles all 5 by name (`none`→`:154`; the other 4 → `disagrees`). The `:163`
fallthrough is therefore reachable only by a 6th shape `classify_disagreement` cannot emit. Doubly
dead on the live corpus: `maxent=inconclusive` for **all 92** (the `:165` catch-fail honest path —
maxent_disagreement fails/throws corpus-wide here). **Verdict: `data-absence-defect (unreachable)`**;
fixed as fail-closed hardening (if a future shape is added, the probe reports "uninterpretable"
instead of false agreement).

**Net C4a:** 10 sound · 1 live defect (`:198`) · 2 unreachable-but-fixed (`:212`, `:163`).

---

## Round 1B — the fix (separate commit, engine-only)

`diagnostic_summary.pl`, 3 lines, `; Signal = agrees` → `; Signal = unavailable` at `:163`,
`:198`, `:212` (`unavailable` and `inconclusive` are dropped identically at
`classify_signals_acc:359–362`, so neither is counted as agreement). Diff:
`diagnostic_summary_fix.diff`.

**One-sentence flag (carried into the commit):** the 6 sibling probes use `inconclusive` for their
catch-else; I follow the plan's `unavailable` — behaviorally identical (both dropped) but it labels
these as *converted data-absence* sites distinctly and is greppable.

### Witness — output change (probe before→after, deterministic within harness)

The probe (`probe_oq112_round1.pl`) reproduces the real diagnostic path (same module list as
`run_pipeline._json_report`, `load_abductive_data` asserts the 79). Two fresh runs are
byte-identical (determinism confirmed). 85/92 join verdicts match the committed
`pipeline_output.json`; the 7 that differ are bidirectional and pre-existing (the OQ-112
non-determinism / stale-`abductive_data.json` class — `abductive_data.json` mtime Jun-21 21:03
predates the Jun-22 02:03 snapshot — **not** introduced here). Using the probe as its own
before/after baseline isolates the fix:

```
abductive signal distribution:  BEFORE  79 disagrees + 13 agrees
                                AFTER   79 disagrees + 13 unavailable
before→after row diff: EXACTLY the 13 has_abd=no rows, abductive agrees→unavailable, nothing else.
join_verdict column before vs after: IDENTICAL for all 92.
```

**The fix is output-changing at the agreements-list level (13 spurious agreements removed) and
HEADLINE-NEUTRAL** (no green/yellow/red flip) — the join verdict is driven by tensions/rejections/
expected-conflicts, not the agreement count. `outputs/` is gitignored, so the committed
`pipeline_output.json` is not a version-controlled artifact; the next full pipeline run will
reflect the 13-row agreements change. `diagnostic_selftest` → **PASS** post-fix.

### Witness — two-sided constructed positive control (`probe_controls.txt`)

```
CTRL :198 no_abd_fact        -> unavailable   (didn't-look fails closed)
CTRL :198 abd_triggers([])   -> unavailable   (measured-empty; conservative — see follow-up)
CTRL :198 genuine trigger    -> disagrees(abductive_tension([...]))  (nonempty path intact)
CTRL :212 constraint_signature(fake) = unknown  (TOTAL; :212 unreachable)
```

**Follow-up flagged (not in scope this round):** `:198`'s fix makes the *measured-empty* case
(`abd_triggers(C, [])`, currently never produced — all 79 are nonempty) also `unavailable`. The
fully Pattern-6-correct form carries the provenance bit at authoring time: have the producer emit
an entry (`[]`) for every corpus constraint **plus** an `abductive_loaded` witness fact, and let
the consumer return `agrees` on `[]` (measured-empty) vs `unavailable` on missing-fact/no-witness.
That is producer+loader+consumer (output-changing, multi-file) — recorded under item 2's
completion-fact design.

---

## Round 1C — staged disposition of items 2–8 (corrected designs)

Each carries its **corpus-re-witness obligation on the pinned 92** (inherited 62/194 verdicts are
not standing facts). Full designs land in the OQ-112 ISSUES.md entry; summary:

- **Item 2 (A10-widened, channel absorbers).** Do NOT ship `catch(Goal,E,assert_failure)` alone —
  `catch/3` is blind to *failure* (W12a clause-failure before arithmetic; W12b `catch(_,fail)`
  row-drop). Invert the default: emit a positive `maxent_completed(N, witness)` on genuine
  completion and **fail-closed in `verdict_join` on its absence** (subsumes the loud-channel
  option). Item-2 control must **force a clause failure**, not just a `type_error`. Re-witness
  item-4 SILENT on 92 first. One deferred operator ruling (`blocked_on_human`): may a maxent stage
  ever legitimately emit zero constraints?
- **Item 3 (A6, 5 unmeasured sites).** `purity_scoring.pl:71,80,88`, `drl_boltzmann_analysis.pl:302`,
  `drl_fpn.pl:206`, `covering_analysis.pl:137`, `signature_detection.pl:1090`. Tripwire measurement
  on the pinned 92 ("NEUTRAL on 194" is not standing); then fail-closed per statute on any live site.
- **Item 4 (A3 idiom cleanup).** Dead branches confirmed, live firing empty on 62; re-witness on
  92, then the idiom cleanup.
- **Items 5 (C4b blind=stable), 6 (A2 statistic-on-empty), 7 (A10 catch→0.0 — folds into item-2's
  completion-fact design), 8 (low: C4c/A7/B2).** Report-grade; staged per success-shapedness order.

---

## Files

- `pinned_corpus.txt` — Round-0 membership + manifest (self-witnessing pin).
- `probe_oq112_round1.pl` — read-only diagnostic-path probe (copy; runs from `prolog/`).
- `probe_before.tsv` / `probe_after.tsv` — per-constraint abductive/signature/maxent signals + join.
- `probe_controls.txt` — two-sided constructed positive controls.
- `diagnostic_summary_fix.diff` — the 3-line engine fix.
