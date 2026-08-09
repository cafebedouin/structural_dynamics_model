# OQ-262 — IN FLIGHT: recon + prereg complete, paused at the R2 grammar sign-off; no verdicts exist yet

**Executed:** 2026-08-09 (Phases A–B only; Phases C–E pending R2)
**OQ:** OQ-262
**Verdict:** none yet — the audit is deliberately paused at the pre-registered R2
gate: the severance/intrinsicness grammar (PREREGISTRATION.md) awaits the operator's
sign-off, which freezes it BEFORE any verdict is produced. No pair has been judged.
**Substrate:** no pipeline run. Read-only recon over 6 corpus legs; live `testsets/`
loaded 235 constraints, `archives/datasets/kernel_test/` 229 (probe stderr counts in
the logs; corpus moves mid-session — Phase C re-fingerprints at execution).
**Evidence map:**
- `recon_probe.pl` — read-only recon probe (3 entry goals; per-process self-checks +
  overlay controls)
- `recon_live.log` — fiat edge table (30 edges, 13 coexists pairs), commitment
  inventory, live-leg census, obstruction witness
- `recon_kernel_test.log` — CP triplet (3 mutual pairs, zero axioms — rider 3
  confirmed) + the found axiom-bearing `state_killing_authority` family + archive census
- `census_testsets_{haiku,flash,kimi,sonnet}.log` — twin-leg censuses (zero prefixed
  targets; Phase-D movement is live-leg-only)
- `RECON.md` — Phase A findings + the Phase-D movement pre-derivation input
- `PROPOSAL.md` — what runs after R2, verdict constitution, evidence plan, R2
  decision points (incl. Arm A/B control-substrate choice)
- `PREREGISTRATION.md` — the verdict grammar, DRAFT until the §K signature; frozen
  after; never amended

## Status

Phase A committed at `7de8e5f9`; Phase B at `d3caca81`. **R2 SIGNED 2026-08-09:
Arm B, with operator edits 1–3 applied pre-signature** (contradictions file to
judge 2's set; thin-base triplet-undetermined non-HALT under Arm B; oversensitive
vs mutation-laden non-distinction pre-committed). Phases C–E executing.

**Writeup obligations recorded at R2 (must appear in the final writeup):**
- §J omits `miscoded_asymmetry`, so the blind judges work under a slightly
  narrower grammar than the main instance — harmless on symmetric CP profiles;
  one line in the writeup (operator A5 note).
- Residue: live-leg 154 unresolved + 48 orphan-source edges of 336 kernel-owned
  (~half) vs zero on all four twins — generation-era artifact or a second skew
  class the resolver doesn't cover; fiat's 30 all resolve so OQ-262 unaffected.
  Note in Residue; mint an OQ if not disposable in a sentence.
