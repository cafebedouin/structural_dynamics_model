# OQ-37 census re-disposition — writeup

**Date:** 2026-06-30. **Method:** recon → decide → execute → writeup (separated passes). Every
finding below cites tool output or code inspection (see `recon_evidence.md`), not the drifted
OQ-37 census text. **Verdict: OQ-37 resolved** — every read-but-unauthored name is dispositioned;
no OQ-37-local engine work remains.

## The question

OQ-37 listed `constraint_metric` names the engine *reads* but that are *never authored* on any
corpus, asking per name: author it, or remove the dead read. The operator reframed it (this
session) as a generation-schema coverage question decided by a keep-gate — is the tell (a) on-target
and (b) non-redundant — where "0 authored" is confounded between forgot-to-wire and
deliberately-scoped-out.

## The root (settled, not assumed)

All six target names trace to one fact: the json→prolog compiler
`generate_constraint_pl.py:608-635` emits a **fixed** `constraint_metric` set and nothing else
(§0 of recon_evidence). So "author" = grow the compiler (+schema+validator+prompt); "remove" =
strip a consumer of a never-emitted name. The confound lives at the generation front, not the read
site — which is why a read-site census alone (the OQ-37 original) could not settle it.

## Evidence that contradicts / extends the census text (the high-value findings)

Per the audit discipline, findings that merely restate the census are low value; these correct it:

1. **`inevitability`'s read is already gone, and the capability is structurally superseded.** The
   census listed it as live engine work (`constraint_bridge.pl:22`). In fact the sole consumer was
   removed (D2 strip, `constraint_bridge.pl:20-25`), and `false_natural_law` already carries the
   "no alternative / inevitable" fraud *structurally* (`signature_detection.pl:1018,1040`) — so the
   scalar metric is not just unauthored, it is **off the detection path**. The census over-counted
   live work by one row.

2. **The `validate_edge_cases` piton check the census said "can now be removed" was already
   removed** (`1eacd2fc`, recorded under OQ-38). The OQ-37 text was stale by ~3 weeks. Code-read
   beats the stale document.

3. **The compound-metric row is resolved, not open.** OQ-93 (2026-06-11) made the leveled grid
   authorable (32 facts testsets / 696 haiku); `coercion_projection.pl:25-27` reads them live. The
   census "never emitted" predates OQ-93.

4. **The proposed `resistance_to_change → resistance` repoint is an OQ-64 morphology trap, declined.**
   `resistance` is the NL/coercion-grid metric (`grid_first_contact_gate.py:48`; mountain-signature
   feature, `signature_detection.pl:182`; validator-bounded for mountains); `resistance_to_change`
   is a drift-domain resistance-to-abolition concept used only in `function_obsolescence`. Distinct
   referents sharing a name-stem. Two further facts make the repoint *worse than useless*:
   `safe_metric/3` fails silently (no default, `metric_drift_events.pl:66`), so the detector dies at
   its FIRST goal (`alternatives_available`, never authored) — the repoint changes a line downstream
   of a failing guard (zero current behavior) while baking a latent wrong-metric identification that
   would activate the moment `alternatives_available` is livened. Declined; `function_obsolescence`
   routed to GAP-23 as a two-input liven-unit instead.

5. **Part D (masked unknowns) is moot, not pending.** The 3 uncharacterized pre-reset readings are
   absent from the live `testsets/` (post-reset 2026-06-05); two appear in the twins but as
   independent redraws (OQ-26 stochastic generation), not the same instances. The 2026-06-01
   diagnosis cannot be re-witnessed; characterizing masked-unknowns on the current corpus would be
   fresh work.

## What survived as genuine deferred capability (→ GAP-23)

Two tells are on-target, non-redundant, and genuinely unbuilt at the generation front — the honest
"deliberately-scoped-out (for now)" side of the confound:

- **`sunset_time`** — the self-supplied falsification tell (declared expiry, then violated). The
  detector `detect_sunset_violation` exists and is dark only for want of the input. Non-redundant
  with `scaffold_suppression_escalating` (metric-trend vs declared-expiry — orthogonal, probe b).
  Highest-priority liven.
- **`internalization_depth`** — the manufactured-consent quadrant. Two wiring breaks (module never
  loaded + input never emitted). Highest cost; kill-condition attached.

Both are output-changing operator-seat livens, staged post-rebuild per the "extract value from the
corpus we have first" posture. Recorded as **GAP-23** (the design-gap ledger is the honest form of
a declared deferral) with per-tell pricing and reopen conditions.

## Execution (this session)

- **One behavior-preserving engine edit:** dropped never-authored `resistance_to_change` from the
  `data_validation.pl:320` extreme-value monitor (commit `5b7a8b95`). Witness: validation suite
  `✓ No extreme values`, Errors 0 / Warnings 1 / Info 1 — identical before and after; provably
  byte-identical (0 `resistance_to_change` facts on every corpus, so the `member` could never match).
  This is a validation-channel read, not a `classify_from_metrics` path → it does not touch
  `pipeline_output.json`; the validation suite is the correct witness.
- **No other engine writes.** The repoint was declined (above); the honest print/null/label sites
  were ruled keep.
- **Docs:** GAP-23 added (`design_gaps.md`); OQ-37 reconciled to resolved with the per-name
  disposition + Deps fixed (`blocked_on OQ-90` — OQ-90 resolved — dropped; now `bundled_with OQ-48`);
  router regenerated; gate GREEN.

## Routing of residuals (so a cold read finds the next move)

- supp/ε-floor recalibration → **OQ-48** (bundled_with).
- `accumulation_speed` / zero-caller helpers → **OQ-38** dead-code clause pass (do not blind-strip).
- `sunset_time` / `internalization_depth` / `function_obsolescence`-pair livens → **GAP-23**
  (operator-seat, reopen on analytical-product demand).
- inevitability scalar → not a gap (superseded by FNL); a scalar cue *feeding FNL confidence* would
  be fresh low-priority work.
