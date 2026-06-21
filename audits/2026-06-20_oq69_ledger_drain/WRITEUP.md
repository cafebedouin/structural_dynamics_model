# OQ-69 ledger drain — 2026-06-20

OQ-69 was a **research-frontier backlog ledger** (Ω_P), not a single answerable question: ~15
inherited work items, each designed to "graduate individually to its own OQ when picked up." A ledger
resolves by being **drained** (every live item promoted to its own tracked OQ), not by executing its
contents. This audit records that drain.

**No engine code changed.** This was an `ISSUES.md` tracking restructure + index regen + doc-currency.

## Outcome

- **17 new OQs minted: OQ-154 … OQ-170.** The 16 ledger bullets became 17 OQs because the
  engine-hardening bullet is three legs (OQ-154/155/156) and the cluster bullet splits F/G
  (OQ-160 gates OQ-170).
- **OQ-69 closed** (`resolved`) with a provenance map (each ledger item → its OQ).
- Prior `check_stack` item had already graduated → OQ-142–145 (2026-06-18); recorded in the map.
- Priorities on all 17 are **provisional — operator to rule** (the declared seat). Scheme:
  distinct-within-band, bands overlap 1–10 (operator ruling 2026-06-20).

## Three pre-write witnesses (read-only deciding pass, before any edit)

### 1. δ (cognitive_displacement) — load-bearing or shadowed?

The ledger said "δ not load-bearing in current implementation"; triage found δ wired at
`constraint_indexing.pl:580`. Resolved by **perturbation counterfactual** with both controls
(`probe_delta_loadbearing.pl`, output in `delta_loadbearing_probe_output.txt`):

- **Negative control** (no-op overlay δ:=0.0): χ byte-identical → harness does not spuriously perturb.
- **Positive control / experiment** (overlay δ:=0.3 on δ's *own* param, caches cleared): χ flips on
  all 4 canonical contexts → δ flows `resolve_displacement/2 (:543) → D_eff = clamp(D+δ,0,1) (:580)
  → sigmoid → χ`. The control rides δ's own sink, not a proxy path.
- **Restore**: baseline recomputes identical → no overlay leak.

**Verdict:** δ is **wired and load-bearing WHEN SET**, but **inert at the default config**
(`config.pl:171` cognitive_displacement=0.0; `:174` profile=uniform; all `positional_displacement`
facts = 0.0), so `D_eff = D + 0.0 = D` and δ contributes nothing to live pipeline output as shipped.
δ is **live-but-zeroed**, not dead code. OQ-162's description corrected accordingly.

### 2. Close OQ-69 vs. keep open — ruled from the code, not a post-close `check`

- **Inbound-reference enumeration:** `grep -n "OQ-69"` across ISSUES.md shows OQ-69 only at its own
  header (line 3173) — **no other OQ's `Deps` line points at OQ-69**. External refs are CLAUDE.md:576
  (parenthetical), the generated INDEX files, and KNOWN_STATE history.
- **Dangling-detection source** (`omega_resolver.py:244–258`, `authority_report`): the Deps-target
  authority set is `set(entries)` = **all parsed OQs regardless of status**; a resolved OQ stays in
  `entries`, so it remains a valid Deps target — **a resolved parent does not dangle.** And `frontier`
  buckets only `active` OQs (`blocking_graph:312` + `:366` `active_only=True`), so a resolved OQ-69
  drops out of the menu cleanly.

**Verdict:** **close OQ-69** (the default). No need to keep it open as a thin drain-manifest parent.

### 3. Four PARTIAL scope floors — re-witnessed against the files

For PARTIAL items, the "what already shipped" claim *is* the new OQ's scope floor; a stale count
re-scopes silently. Re-witnessed 2026-06-20:

- **OQ-157:** `prolog/tests/` holds exactly `test_maxent_profile_indexing.pl` (1/5) → scopes the
  remaining four primitives.
- **OQ-160:** `enhanced_report.py` has no cluster section (only a prose string at `:2186`); compute
  lives in `cluster_space_phase{3,4,5}.py` → scopes the wiring.
- **OQ-163:** subdirs (`audits/`, `sweeps/`, `shared/`, …) exist; no `cli.py`/`__main__.py`/`main.py`
  → scopes the CLI.
- **OQ-164:** 6 `canonical_d_*` specced (`config_schema.pl:83–88`); `power_role_heuristic/4` +
  `exit_modulation/2` still hardcoded facts (`constraint_indexing.pl:469,477+`) → scopes those two.

## Verification

See the commit and KNOWN_STATE 2026-06-20: `issues_status --check` (170 parsed, 0 malformed),
`omega check` (0 problems), `omega selftest` (10/10), `omega menu` (arrival of 154–170 + departure of
OQ-69/OQ-63 from WORKABLE), `gate.sh` GREEN.

## Files

- `probe_delta_loadbearing.pl` — the δ perturbation probe (run from `prolog/` via
  `swipl -q -l ../audits/2026-06-20_oq69_ledger_drain/probe_delta_loadbearing.pl`).
- `delta_loadbearing_probe_output.txt` — its captured output (the witness).
