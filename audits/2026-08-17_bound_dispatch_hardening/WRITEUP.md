# bound_dispatch_hardening — pilot conversion landed: bound calls now mean "engine assigns"; zero-diff on all six legs; three live finds the hand table missed

**Executed:** 2026-08-17 (= directory date; single session, Phases 0–5)
**OQ:** none at open (operator ruled: skip the OQ, run the recon live as one planned
unit); follow-ons minted at close: OQ-302, OQ-303
**Verdict:** the fresh-variable-head + unify-after-cut transformation of
`classify_from_metrics/6`, `constraint_signature/2`, and `classify_by_signature/3` is
**output-preserving on the witness set and semantics-changing by construction** — a
disagreeing bound call now fails instead of lying, for every caller forever; zero
per_constraint diff on six corpus legs means no witnessed disagreement REACHED recorded
output, NOT unchanged semantics (the 311 live manufactured cells sit on the is_X
surface, which per_constraint does not consume).
**Substrate:** all five live legs + kernel_v1 archive via `classify_corpus` pairs
(testsets n=279, haiku 960, flash 960, kimi 1005, sonnet 1001, kernel_v1 1106; leg-dir
md5 fingerprints stable around both halves of every pair); end-to-end
`run_pipeline.py` pair exit 0/exit 0 (pre 42.93s / post 39.61s), per_constraint 0/279
differ, manifest n_constraints=279.
**Fired:** live — the dispatch_bound_call pair fired RED at pre-fix HEAD and GREEN at
the fix commit; the dynamic probe produced non-empty manufactured-classification lists
on every leg (311/1093/1356/1172/762); the wrapper selftest caught a real walker bug
(census corrected 90→73); and the 3b probe FALSIFIED the plan's inherited-safety
premise for `cluster_by_signature` (0/26 members returned), converting the planned
site-comment into a witnessed repair.
**Evidence map:**
- `audit_log.md` — OPEN/CLOSE stamps, prior-art grep, baseline gate run + self-induced-
  red investigation, prereg md5, pre-fix RED paste, all Phase 3/4 witnesses.
- `RECON.md` — the read-only deciding pass: census instrument + correction (§1b),
  hand-table diff (§2), caller sweep (§3), 16 adjudications (§4), disagreement lists
  (§5), embedding table (§6), body reads (§7).
- `PREREGISTRATION.md` — frozen before any engine write (md5 in audit_log above the
  first result line); outcome semantics + kill condition, neither violated.
- `census_checker_run1_HEAD_9a5d8526.txt` / `census_checker_run2_cutfix_HEAD_e16f9c0f.txt`
  — the definition-site censuses (run 2 is authoritative; run 1 kept as the corrected
  record).
- `caller_sweep.py` / `caller_sweep_output.txt` — bound-caller sweep over 599 files
  (A/B split input; positive control: found the six is_X delegations).
- `disagreement_probe.pl` / `disagreement_probe_<leg>.txt` (×5) — the live
  manufactured-classification membership lists, same-path positive control green.
- `leg_sweep.py` / `leg_sweep_clean.json` / `leg_sweep_edited.json` — the six-pair
  witness driver with fingerprints and timings.
- `pair_diff.py` / `pair_diff_output.txt` — per_constraint diff: IDENTICAL ×6.

## What landed (commits, in order)

`e67659db`/`9a5d8526` Phase 0 (audit_log first; baseline red investigated as
self-induced; stub writeup) → `a76c21dd` Phase 1 recon → `a558a53b` prereg freeze →
`e16f9c0f` test RED at pre-fix HEAD → `5ce17390` walker cut-bug fix + census correction
+ gate wrapper → `6c40a0bb` **the pilot conversion** (+ same-change consumer updates:
`check_logic_symbolic_drift.py` end-anchor; two reasoned `bound_selector_check`
EXEMPTs) → `042554e6` gate row `dispatch head` wired + `cluster_by_signature` repaired.

## Scoping the zero-diff claim (per predicate)

Only the `classify_from_metrics/6` half of the six-leg zero-diff is evidence:
`constraint_signature/2` has no live bound callers (recon fact, independently
confirmed), so its half is guaranteed by construction and witnesses nothing.

## The three finds the hand table missed (checker-only hits)

1. **`epistemic_access_check/2` bound-`false` at `boltzmann_compliance.pl:577`** —
   `boltzmann_invariant_mountain/2` unconditionally `inconclusive(insufficient_data)`;
   its own header warned of exactly this call shape. Diagnostic surfaces only; fix is
   one line but output-changing → **OQ-302** (kept out of the pilot's diff attribution
   per prereg scope guard).
2. **`cs_verdict/2` bound goal in `validate_naturalization_gap.py:287`** — latent
   (manual script); repair proposed with its next witnessed run → OQ-303(b).
3. **`cluster_by_signature`** — the plan predicted the callee conversion made it
   correct by construction; the probe showed the UNBOUND-C findall is pruned by
   clause-1's cut instead. Repaired (enumerate `corpus_constraint/1` + once +
   post-filter), probe MATCH on unknown 26/26, false_ci_rope 85/85, natural_law 0/0.

Consumer sweep (conditional on the non-empty disagreement list): no recorded artifact
carries the bound-call diagnostics' output — the only `MATCHES_*` hit outside this
audit is a *source listing* of the script in `audits/2026-02-28_codebase_audit_data/`.
No fcr_ablation-shape correction owed.

## Applicability verdicts (each scoped)

- **SSU (`=>`):** not the primary tool for catch-all-bearing dispatch — the catch-all
  always matches, so SSU's fail-loud property never fires. Adopting it would require
  removing catch-alls: a rulings-level semantics change, not proposed here.
- **`det/1`:** wrong tool for the legitimately-semidet MaxEnt reads; the real hazard
  is the catch+default arms — named and routed to OQ-303(d), left OPEN.
- **Class-B latent conversions:** list stands in `dispatch_head_check.py`'s registry
  (~60 `latent-B` entries, kept fresh by the gate row); conversion is mechanical per
  this pilot's template → OQ-303(a).
- **`bound_selector_check` retirement:** PROPOSED, executable once the `dispatch head`
  row has fired in anger once, with a reason string naming that row as cover →
  OQ-303(c). Registry unchanged this pass (verified: 1 entry, 2 reasoned exemptions).
- **Mercury (the port that framed this audit) — REJECTED, added 2026-08-18.** This line
  was owed by the plan ("Non-goals: Mercury port (rejected, recorded in writeup
  context)") and did not land; it is appended here rather than back-dated. The audit
  began as a Mercury question (2026-08-17 session): Mercury's static type/mode/
  determinism system targets exactly this project's documented defect class, and the
  assessment conceded that `build_discipline.md` is in large part a hand-maintained
  substitute for such a checker. The port was rejected on three grounds, none of which a
  newer swipl changes: (1) **the dynamic database IS the architecture** — `asserta`
  `config:param` overlays, `probe_harness:with_overlay`, `cache_registry`, MaxEnt's
  corpus-fitted state, the run-tag glob are all assert/retract, and Mercury has no
  dynamic database; (2) **the interactive probe methodology dies** — Mercury is
  compile-and-run, so the whole gotchas §§2–7 workflow (in-session sweeps, retract/
  re-assert probes, `swipl -g` one-liners) has no equivalent, and for a research engine
  the probing is half the work product; (3) **the LLM co-development loop degrades** —
  every model's Mercury exposure is a fraction of its Prolog exposure, so generation,
  bug-spotting, and cold-read handoff all get worse. Cost: months of re-verification
  across five legs plus archives for zero new analytical capability, mid-alpha, with the
  corpus rebuild itself deferred. **Do not re-propose pre-beta.** Operator also ruled
  that the port gets no OQ of its own ("an OQ whose resolution is 'no' is a record
  without a reader") — which is why this writeup, not ISSUES.md, is its home. The
  salvage is everything above: the fresh-variable-head + unify-after-cut transformation.

## Timing (the class-B rollout price instrument)

Six paired clean→edited `classify_corpus` runs (same corpus, same session):
13.33→13.29, 61.76→63.57, 70.17→71.85, 64.33→65.82, 83.36→84.76, 61.49→61.49 s.
Worst pair +2.9%; end-to-end pipeline 42.93→39.61 s. The loss of first-clause indexing
on bound callers is not material at corpus scale.

## Residue (what changed in substrate)

- `prolog/drl_core.pl`, `prolog/signature_detection.pl` — the conversions.
- `prolog/isomorphism_engine.pl` — `cluster_by_signature` repaired.
- `prolog/dispatch_head_check.pl` + `python/dispatch_head_check.py` + gate row
  `dispatch head` in `scripts/gate.sh` — the definition-site guard, registry = the
  living class-B membership list.
- `prolog/tests/test_dispatch_bound_call.pl` — the standing semantics assertion.
- ISSUES.md: OQ-302, OQ-303 minted (index regenerated); KNOWN_STATE.md dated entry
  2026-08-17 (promotion test applied there).
- Stranded plan premises corrected in place: RECON §2 (run_pipeline "bound goals" were
  unbound; line drift), §1b (census correction), 3b (inherited-safety falsified).
