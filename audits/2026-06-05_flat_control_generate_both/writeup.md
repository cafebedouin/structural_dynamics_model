# Generate-Both Landed: forced-flat control on every kernel, with a mechanical alignment key (OQ-76 primary fix)

**Date:** 2026-06-05. **Ruling (operator):** generate-both promoted from fallback to PRIMARY fix
for the stochastic kernel/flat gate (OQ-76, K1 evidence), do-it-now for the Stage-2 rebuild.
Spec handed down: forced-flat construction mode (new piece), flat control generated on EVERY
kernel topic (asymmetric — never kernel-on-every-flat), construction-pair diff as its own
Stage-2 stratum, alignment-key requirement explicit (a perturbation diff without a mechanical
key is blind — the axiom axis's 0/935 lesson).

## What was built (commit refs in git)

- **`agent/generate_kernel_corpus.py`** — `flatten_manifests` emits one flat-control gen-seed
  per kernel manifest (`<kernel_id>_flat_control`, substrate = the manifest's
  `kernel_description`, `kernel_id=None` — NOT a reading); `build_cached_messages` adds a
  FLAT CONSTRUCTION block (single story, no readings, no cs reading_relations/axioms,
  **substrate only — the reading set is never shown**, so the two constructions stay
  independent); `process_batch_results` injects ephemeral `_flat_control_of` (mirrors
  `_kernel_id`); `stamp_kernel_linkage` extended (idempotency, mismatch guard, separate
  counter, no-cs_structure exception for flat-only stamping).
- **`python/generate_constraint_pl.py`** — emits `narrative_ontology:flat_control_of/2`
  (multifile decl + fact) from `_flat_control_of`, deliberately OUTSIDE the cs_structure gate.
- Single underscore in `<kernel_id>_flat_control` avoids the `__` reading-name convention; the
  flat control never carries `cs_kernel_id` or `cs_reading_relation`, so kernel statistics,
  the OQ-58 integrity sweep, and `cs_kernel_registry` are untouched by construction.

## Witnesses

1. **Compiler emission + negative control:** synthetic story with `_flat_control_of` → .pl
   carries `narrative_ontology:flat_control_of/2` decl + fact; without the key, zero
   occurrences.
2. **Seed emission on a real K1 manifest** (`manifest_X2_r1.json`, equal_protection_boundary):
   exactly one flat seed alongside 3 readings + 3 ordinary axes; `kernel_id=None`.
3. **Prompt independence:** flat task block contains the substrate and the FLAT CONSTRUCTION
   instructions; no KERNEL CONTEXT; none of the kernel's reading ids appear (leak grep).
4. **End-to-end** (run-tag `flatctl_probe`, single affirmative-action kernel seed):
   `equal_protection_kernel_flat_control` generated, validated, compiled with
   `narrative_ontology:flat_control_of(equal_protection_kernel_flat_control,
   equal_protection_kernel).`; 0 `cs_kernel_id`/`cs_reading_relation` facts in the flat .pl;
   stamp reports it under its own counter; integrity sweep did not quarantine it.
   (2 of 3 readings hit batch producer-gaps — pre-existing flakiness, instrument unaffected.)
5. **First construction-pair diff** (`flat_control_diff_probe.pl`, archived here; engine
   loaded both files): alignment key mechanically joined the pair via
   `flat_control_of/2` × `cs_kernel_id/2`. Verdict for colorblind_reading vs flat control:
   **construction-robust at the computed layer** — dr_type = tangled_rope at all 4 standard
   contexts in BOTH constructions — while **divergent at the authored layer** (reading: snare
   ε=0.65; flat: tangled_rope ε=0.48). The engine's computation survives the construction
   choice; the authored claim moves with it. That two-layer separation is the §7.1 datum the
   instrument exists to take.

## Stage-2 wiring (remaining, recorded in OQ-76/OQ-75)

The generation side is landed: every kernel topic in a rebuild now gets both constructions
unconditionally, so a recognizer miss can no longer cost the axiom axis (the gate no longer
gates it). Remaining work is READOUT-side, part of Stage-2 analysis design: report the
construction-pair diff (computed-type agreement per seat; authored-claim divergence) as its own
stratum in the cross-axis correlation. The interim kernel-bias hedge is superseded — the gate's
decision no longer determines whether the axiom axis exists.
