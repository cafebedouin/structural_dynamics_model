# Phase 1 seed rescues — results and uniform declare-short (2026-08-25)

Tag: `seed_rescue1` (RULING-1, operator 2026-08-25), passed bare; driver prepends `+`.
Stratum on disk: `no_scope_rebuild_nemotron<_think>+seed_rescue1` (read-back verified, field 5).
Clocks: T0 = 06:49:53Z (pilot), full pass 06:57:38Z, hard stop T+45 = 07:34:53Z.

**Phase 1 was SYNC-ONLY by construction** (plan Correction 4/settled-at-planning): the Anthropic
and Gemini drivers are batch-only, so 257 of the 268 residue seeds were declared short by
construction and routed to **OQ-378** (minted this session). Only the OpenRouter driver's 11
nemotron seeds were reachable.

## testsets_nemotron — one pass (`--n 0`, 9 seeds, `--reasoning-effort off`)

**Final n = 1000 (was 996). Landed 4, short 5.** Driver artifact witness: `4/9 ladder-done;
4 .pl on disk` (run output, task bci3sqiq1); ladder and disk agree at 1000.

Landed (all read back with `+seed_rescue1`):
- `zero_as_number_entry__universal_discovery_reading`
- `maat_order_principle__distributed_maintenance_reading`
- `exercise_as_competence_maintenance__hybrid_decay_reading`
- `notability_guidelines__deliberative_reading`

Short — the shortfall seed ids (5), each failing all 3 attempts on persistent model behavior
(negative values where the schema floor is 0; an invalid `organizational` scope atom):
- `catastrophe_avoidance_retention__simulation_as_proxy_catastrophe`
- `divine_legitimacy_substrate__folk_syncretistic_reading`
- `lycurgan_laws__sacral_fidelity_reading`
- `zero_mathematical_status__parmenidean_rejection`
- `maat_order_principle__divine_mandate_reading`

Raw responses persisted: 9/9 in `outputs/no_scope_runs_nemotron/responses/`; failures ledger
`outputs/no_scope_runs_nemotron/failures.json`. No second pass (uniform declare-short).

**OQ-58 integrity sweep (manual — the no-scope path skips it): 1 unresolved edge quarantined**,
`maat_order_principle :: maat_order_principle__distributed_maintenance_reading
-coexists_with-> divine_mandate_reading` — the target story is exactly one of the 5
declared-short seeds above. Quarantine:
`prolog/testsets_nemotron/cs_reading_relation_quarantine.json` (run-scoped path; caught, not
crashed).

## testsets_nemotron_think — one pass (`--n 0`, 2 seeds, model-default reasoning)

**Final n = 1005 (was 1003). Landed 2, short 0 — LEG COMPLETE.** Driver artifact witness:
`2/2 ladder-done; 2 .pl on disk` (task b6dy1jv7a); completed 07:17Z, inside T+45. Landed:
- `ip_category_emergence__thinkability_reading` (attempt 1)
- `treaty_authority_cession__rangatiratanga_retention_reading` (attempt 2, REPAIRED; advisory
  SCAFFOLD_DANGER_ZONE lint, OQ-127 calibration class — recorded, not a failure)

Both read back with `no_scope_rebuild_nemotron_think+seed_rescue1` in field 5.

**OQ-58 sweep: 1 unresolved edge quarantined** —
`treaty_authority_cession__rangatiratanga_retention_reading -influences-> biculturalism_reading`
(target reading not a generated story in the leg; standard dangling-sibling disposition).
Quarantine: `prolog/testsets_nemotron_think/cs_reading_relation_quarantine.json`.

## Consequence for the coherent set

- CHANGED legs for the Phase-5 diff: `nemotron` (expect n_only_new == **4**), `nemotron_think`
  (expect n_only_new == **2**) — both matched against the drivers' provenance-tagged artifact
  counts, not the ladder.
- `STAMPED_FILE_COUNTS` re-pinned post-pass: nemotron 996 → 1000, nemotron_think 1003 → 1005.
- The remaining shortfall (5 nemotron + 257 batch-only) lives in **OQ-378** with the full
  per-leg table.
