# Phase 4 witness — retire the Supp≤0.2 piton_signature (behavior-preserving)

Removed together (removing one without the other breaks the load): the dispatch clause
`classify_by_signature(Profile, _, piton_signature) :- piton_signature(Profile), !.` and the
helper `piton_signature/1`. Left in place with `% superseded by OQ-90 FCR refinement;
unreachable from profile path` comments: the atom-keyed handlers `compute_signature_confidence`,
`explain_signature`, `resolve_modal_signature_conflict`, and `signature_mapper.pl:26
resolve_mapping(piton_signature, piton)` (they pattern-match the atom, do not call the helper, so
they still load; removing them exceeds the ruled scope).

## Two-sided witness

The clause is corpus-dark, so a 0-row diff alone proves nothing (Build Discipline Pattern 5 —
absence satisfies the gate). Both sides shown:

**(i) Constructed positive control — fires-before / falls-through-after.**
Profile `profile(0.5, 0.1, 0.3, 2, true, evolving, 0.5)` satisfies the old gate
(Supp=0.1 ≤ 0.2, Resistance=0.3 > 0.2, HasAlternatives=true, TemporalStability=evolving):

- BEFORE removal: `piton_signature/1` TRUE; `classify_by_signature(P,0.3,R)` → **`piton_signature`**.
- AFTER removal: `piton_signature/1` GONE (undefined); same `classify_by_signature` call →
  **`constructed_low_extraction`** (falls through to the next cascade clause).

This proves the probe could fire (the gate was reachable in principle) and that removal changes
exactly the dispatch, with clean fall-through — not a silent no-op masquerading as a removal.

**(ii) Old-vs-new corpus diff = 0 rows.**
Pipeline before removal (Phase-3 committed state, `pipeline_output.refine1.json`) vs after removal
(`pipeline_output.phase4_postremoval.json`), both at `piton_refinement_enabled=1`:

```
verdict-field diffs (signature/perspectives/claimed_type/classifications): 0
```

The gate never fired on the live corpus; its removal moves no corpus verdict. Combined with (i),
this is "removed a reachable-but-corpus-dark clause," not "removed a clause that was already dead
for unknown reasons."

## Load integrity

`validation_suite` / `run_dynamic_suite`: 0 errors, 0 warnings, 1 info — the engine loads and runs
with the helper gone and the atom-keyed handlers retained.

## Follow-up (not a blocker, for the doc pass)

`python/axiom_reachability.py:171,207` is a standalone cascade replica that now models the removed
clause — to be recorded in the doc pass (it does not affect the engine; it is an analysis-side
mirror).
