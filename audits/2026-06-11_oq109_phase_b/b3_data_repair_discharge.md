# data_repair.pl :154–203 — A3 (a)-class item discharged: NO migration needed

**Read 2026-06-12 (the deciding pass; sites at current line numbers :165, :198, :214):**

- `bridge_scaffold_markers_pure/3` (:165): `( classification scaffold exists -> bridge
  has_sunset_clause ; skip )`. On an empty authored table it silently skips — but the
  bridge exists for **v3.4-legacy testsets** that predate direct `has_sunset_clause`
  authoring. New-format (stakeholder-surface) stories author `has_sunset_clause` directly
  in `base_properties` (schema-required for scaffold claims), so the bridge is structurally
  irrelevant for every story the perspectives retirement affects. Absence-tolerant by
  design, not fail-open: nothing downstream reads the skip as a verdict.
- Claim-derivation priority chain (:198 PRIORITY 1 authored analytical cell → :206-ish
  PRIORITY 2 computed type → :214 PRIORITY 3 first authored cell): on a perspectives-free
  story, priorities 1 and 3 fail and the chain falls through to the COMPUTED type —
  exactly the Phase-C-correct behavior, already in place. The empty-table census filed
  this chain Class C (absence-tolerant with computed fallback).

**Verdict:** like AUDIT OPEN-1, this item was already in the migration target state.
The only data_repair defect on file is orthogonal (OQ-111: the omega bridge queries the
wrong module key — pre-existing, unrelated to perspectives retirement, stays with OQ-111).
