# OQ-72 concept-key pilot — RECON (substrate probes, re-witnessed 2026-07-03)

All probes re-run this session (the planning-session probes are not carried forward as
witnesses). Raw commands and outputs below; grep-based counts are over the on-disk `.pl`
files (read-only; no corpus load required for fact-line censuses).

## 1. `cs_axiom_contradiction` facts per leg

```
$ for leg in testsets testsets_haiku testsets_flash; do grep -l "cs_axiom_contradiction" $leg/*.pl | wc -l; grep -h "cs_axiom_contradiction(" $leg/*.pl | grep -v "^\s*%" | wc -l; done
== testsets ==        9 files    30 facts
== testsets_haiku ==  0 files     0 facts
== testsets_flash ==  0 files     0 facts
```

30 facts (39 matching lines − 9 `:- multifile` directives), all in dedicated
`testsets/*_contradictions.pl` files. **Correction to the plan's context block:** the plan
said "testsets 34"; the re-witnessed count is **30**. Not load-bearing — the load-bearing
fact (twins carry NONE; the contradiction discriminating case is exercisable only on the
live leg) holds.

## 2. Contradiction-bearing kernels are all multi-reading (live leg)

Per-file contradiction fact counts (directive excluded) × readings-per-kernel census
(`cs_kernel_id` grep over `testsets/*.pl`):

| kernel | readings | contradiction facts |
|---|---|---|
| visual_evidentiary_authority | 4 | 6 |
| polaris_document_status | 4 | 4 |
| performance_legitimacy | 4 | 4 |
| generality_standard | 3 | 4 |
| moral_causation_locus | 3 | 2 |
| learning_difficulty_substrate | 3 | 4 |
| digital_money_legitimacy | 3 | 2 |
| knowledge_legitimacy_biomedicine | 2 | 2 |
| actinide_replenishment_mechanism | 2 | 2 |

9 candidates total — matches the plan.

## 3. Haiku leg breadth

```
$ grep -h "cs_kernel_id(" testsets_haiku/*.pl | sed 's/.../\1/' | sort | uniq -c | awk '$1>=2' | wc -l
328 multi-reading kernels
reading-count distribution: 3×1r, 49×2r, 259×3r, 18×4r, 2×5r
$ grep -h "cs_axiom(" testsets_haiku/*.pl | grep -v "^\s*%" | grep -v multifile | wc -l
2037 axioms
```

Both match the plan's context block (328 multi-reading kernels; 2,037 axioms).

## 4. Named contradiction pairs are CROSS-READING within their kernels

Each axiom located by `grep -l "cs_axiom(..., <name>)" testsets/*.pl`; kernel membership by
`cs_kernel_id` in the owning file:

| pair | reading A (file) | reading B (file) | kernel |
|---|---|---|---|
| state_monopoly_on_legitimate_issuance ↔ consensus_suffices_for_legitimacy | sovereign_cbdc_reading.pl | crypto_permissionless_reading.pl | digital_money_legitimacy |
| situational_primacy_over_disposition ↔ character_cross_situational_stability | situational_reading.pl | dispositional_reading.pl | moral_causation_locus |
| indexical_traces_recoverable ↔ verification_impossibility_at_scale | indexical_realism.pl | epistemic_collapse.pl | visual_evidentiary_authority |

All three pairs span two sibling readings of one kernel — the within-kernel positive
control is real (a cross-reading same-subject pair), not a single-reading internal tension.

## 5. Load-bearing code assumptions (clause heads pasted)

`prolog/axiom_diff.pl:72` — the concept join is per-axiom Name → Concept:

```prolog
axiom_aligned(concept, A, B) :- axiom_concept(A, C), axiom_concept(B, C).
```

`prolog/axiom_diff.pl:82-83` — unmapped names get unique vantages (blind, never silently
merged):

```prolog
ax_vantage(concept,    ax(Name,_,_), V) :-
    ( axiom_concept(Name, C) -> V = C ; V = unmapped(Name) ).
```

`prolog/tests/test_axiom_diff.pl:69-72` — the westphalia fixture (the one existing human
ruling: 4 names → 2 concepts) is CROSS-KERNEL (`westphalia_sovereignty` vs
`westphalian_sovereignty`), so it serves as the join/regression control only, never a
proposer control.

## 6. Cross-leg kernel-id collision note

`speech_protection_kernel` and `udhr_article_3` appear on BOTH the live leg (1 reading
each) and the haiku leg (4r / 2r). The pilot sweeps each leg separately and vocabularies
are kernel-scoped, but the registry facts are **axiom-NAME-keyed** (`axiom_concept/2`), so
a ratified mapping applies wherever that axiom name occurs, on any leg. Risk is bounded
(generation makes axiom names bespoke per reading), flagged for the R3 ratification pass:
before ratifying, check whether any pilot axiom name also occurs outside its pilot kernel
(`grep` census; expected 0 — if nonzero, those rows get an explicit cross-occurrence note).
