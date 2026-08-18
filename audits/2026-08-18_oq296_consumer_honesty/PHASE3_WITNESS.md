# Phase 3 — dark declarations, widening, regression lock (OQ-296)

Executed 2026-08-18. Behavior-preserving. **Every site was re-verified at edit time and each
annotation states what was confirmed** — no site was annotated on roster inheritance. That rule
came from the operator after Phase 2, on the ground that the roster had by then produced two
reclassifications out of ~20 and a comment-only phase verifies nothing by the act of editing.

## Roster verification result — 3 corrections, 18 annotations

**Reclassified OUT (not annotated):**

| site | why |
|---|---|
| `python/reports/queries/classification_audit.py:61` | Pre-existing correction, recorded at OQ-296's close: validates authored `constraint_claim` values — a different surface that can legitimately be non-empty. |
| `python/container_typology_analysis.py:151-157,331` | Does NOT read a constant zero — reads a stale 2026-05-16 artifact serving values to 0.9808. Spawned as its own OQ. See `PHASE2_SITE1_BLOCKER.md`. |
| `prolog/diagnostic_summary.pl:437` | **Already fully annotated** — carries the honest `once/1 + ==` query shape, cites OQ-113/OQ-296, and states the 0-firing fact. The plan asked to "extend the partial note"; there is nothing partial about it. Left untouched. |

**Annotated (18 sites, each confirmed at edit time):**

*Prolog* — `signature_detection.pl` (coordination_scaffold `== true` dead-by-empty-table;
profile-builder constant slot; `compute_signature_confidence` coordination_scaffold clause),
`dirac_classification.pl` (constant `coordination_choice(unknown)` + stale cite corrected),
`signature_mapper.pl:22`, `abductive_helpers.pl` (×2), `maxent_classifier.pl` (natural_law arm
only), `cs_pattern_detection.pl` (×2), `context_profile_mining.pl`, `reading_registry.pl:115`.

*Python* — `axiom_reachability.py`, `shared/maxent.py` (`:168` only), `audits/audit3_synthesis.py`,
and the four `SIGNATURE_FLAGS` copies (`cluster_space_phase4.py`, `cluster_space_phase5.py`,
`audits/cluster_space_audit.py`, `audits/g_orbit_proximity_probe.py`).

*Plus* `category_of/2` measurement-and-pointer notes at its three live consumer modules
(`isomorphism_report.pl`, `constraint_bridge.pl`, `data_validation.pl`) — **deliberately not dark
declarations**, since OQ-316 has not ruled and a declare-dark comment would pre-commit the answer.

## Findings that emerged from verifying rather than inheriting

1. **The stale cite was real and worse than recorded.** `dirac_classification.pl:357` cited
   `has_viable_alternatives/2` as `structural_signatures.pl:186`. That file is a **13-line
   re-export shim** with no line 186 and **0** occurrences of the predicate. Corrected to
   `signature_detection.pl:249`.
2. **`compute_signature_confidence(_, coordination_scaffold, _)` is dead twice over**, and the
   inner reason is the interesting one: even if reached, `high` is arithmetically unreachable —
   one of its three indicators is `HasAlternatives == true`, so `Count =< 2` always and the
   `Count >= 3` arm cannot fire. The clause would silently cap at `medium`: a degraded scale that
   still looks like a working one.
3. **`cs_pattern_detection.pl:353` is only PARTIALLY dead**, and annotating it as "dead" would
   have been wrong. It is a *negated* membership over three atoms, two dead and one
   (`coupling_invariant_rope`, 15 live firings) alive — so in practice it reads
   `\+ Sig == coupling_invariant_rope`. The guard still works but is **weaker than it looks**, and
   the error direction is pass-open. The annotation says exactly that.
4. **`reading_registry.pl:115` is the vacuous-totality instance** and was NOT deregistered —
   removing it hides the gate defect instead of fixing it. Spawned as OQ-313.

## Witness 1 — comment-only

```
18 files changed, 138 insertions(+), 2 deletions(-)     [annotations]
19 files changed, 201 insertions(+), 2 deletions(-)     [+ the test extension]

$ git diff -U0 | grep '^[+-]' | grep -vE '^(\+\+\+|---)' \
                | grep -vE '^[+-]\s*(%|#|$)'
(no output)
```
Zero non-comment lines changed across all annotation files. The 2 deletions are the replaced
stale-cite comment lines.

## Witness 2 — regression lock, 8/8 green

Extended `prolog/tests/test_oq113_dead_natural_law.pl` from 3 to 8 tests. **Authorship does not
leak** (D1 refused repair-by-authorship): the planted `true` lives inside a `profile/7` term passed
directly to the predicate — never asserted into a fact table, never in a corpus file or the load
chain. This follows test (a)'s existing safe shape.

```
% [1/8] positive_control_signature_fires ................ passed
% [2/8] live_corpus_zero_firings ........................ passed
% [3/8] has_viable_alternatives_never_false ............. passed
% [4/8] positive_control_coordination_scaffold_fires .... passed
% [5/8] coordination_scaffold_declines_on_unknown ....... passed
% [6/8] coordination_scaffold_corpus_zero ............... passed
% [7/8] has_viable_alternatives_never_true .............. passed
% [8/8] has_viable_alternatives_is_constant_unknown ..... passed
% All 8 tests passed
```

Test (e) is the **discrimination** half the plan did not ask for but the discipline requires: (d)
alone shows the probe *can* fire; (e) shows it **declines** on `unknown`, the value the engine
actually produces. Without (e), (f)'s corpus zero would rest on a one-sided control. Tests (g) and
(h) close the range from the other side — (c)+(g) together are the constancy claim, and (h) states
it directly so a future failure message names the fact rather than leaving it to be inferred.

**Intended alarm:** if GAP-08 §7 lands and the predicate starts discriminating, (g) and (h) go red
first. That is the design, not a regression.

## Witness 3 — clean-vs-edited pipeline pair

Baseline is **post-Phase-2 HEAD** (per the plan's amendment): Phase 2 changed serialized output on
purpose, so a pre-Phase-1 baseline would show Phase 2's intended diffs inside Phase 3's
behavior-preservation witness. Same session; Phase 3 stashed for the clean half.

```
EDITED half: exit 0, output mtime 2026-08-18 15:31:03
CLEAN  half: exit 0, output mtime 2026-08-18 15:32:14

per_constraint md5 EDITED: 6bab9cdc3880b6b7d21f95ad453b33cd
per_constraint md5 CLEAN : 6bab9cdc3880b6b7d21f95ad453b33cd
BYTE-IDENTICAL: True

top-level keys that DIFFER: ['manifest']
  EDITED manifest: pipeline_run_at 2026-08-18T20:29:55Z, n=279, code_dirty True
  CLEAN  manifest: pipeline_run_at 2026-08-18T20:31:06Z, n=279, code_dirty False
```

Both gates from the CLAUDE.md rule are satisfied: **exit 0 AND mtime advanced** on both halves, so
neither diff is against an unwritten file. `manifest` differing is expected and required — it
re-stamps `pipeline_run_at` every run and correctly records `code_dirty` for the working-tree half.

Corpus frozen across both halves (md5 per leg, before and after — identical):
```
testsets        0f85292734cb1fd45049e4cdb7197590
testsets_haiku  f697246d3331b4528e6f1b2591ae5b5c
testsets_flash  6c6a2dbd832f33031441e286089e3dd6
testsets_kimi   57d485238b4c33bf604c896ff3ebcec7
testsets_sonnet 2427448c1b3c7d6e4b607cb883d3918c
```
This matters because operator topic runs have landed stories mid-session twice before; without the
fingerprint, a corpus change during the pair would be indistinguishable from a behavior change.
