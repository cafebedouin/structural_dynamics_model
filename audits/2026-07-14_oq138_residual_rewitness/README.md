# OQ-138 residual re-witness + certify pass (2026-07-14)

Pre-close evidence for the OQ-138 residual disposition. Gathered in a read-only pass (no engine
edits). Settles: (1) the seven unconverted `resolve_modal_signature_conflict` clauses are
corpus-inert across all four current legs, controlled for residual shape; (2) the four CONVERT-row
magnitudes in ISSUES.md are artifact-confirmed, not summary-sourced.

## Files
- `oq138_residual_probe.pl` — per-seat `metric_based_type_indexed` vs `dr_type` diff, bucketed by
  winning signature. Overlays `corpus_path` (retract+asserta). Trap-proof: `dr_type/3:446` confirms
  `metric_based_type_indexed` IS the exact ModalType fed to the signature layer (no proxy).
- `four_leg_residual_counts.txt` — probe output on testsets / haiku / flash / sonnet.
- `oq138_residual_control.pl` + `positive_control.txt` — the residual-shape positive control.

## Result 1 — residual is a controlled zero across all four legs (~3,061 stories)
| leg | N | metric≠final | RESIDUAL | CI-rope control |
|---|---|---|---|---|
| testsets | 140 | 13 | **0** | 9 |
| testsets_haiku | 960 | 33 | **0** | 18 |
| testsets_flash | 960 | 46 | **0** | 34 |
| testsets_sonnet | 1001 | 160 | **0** | 37 |

CI-rope control fires on every leg ⇒ each zero is measured-empty, not didn't-look.

## Result 2 — residual-shape positive control PASSES
- **Control A (clause liveness):** all 7 residual clauses CHANGE the type when tripped (live, not
  dead code): `mountain+coordination_scaffold→rope`, `+constructed_low→rope`, `+constructed_high→
  tangled_rope`, `+constructed_constraint→tangled_rope`, and the three `unknown`-input variants.
- **Control B (real dispatch + discrimination):** `resolve_with_perspectival_check(_, mountain,
  coordination_scaffold, R)` routes `R=rope`; the probe's RESIDUAL bucketing reads that shape as
  RESIDUAL=yes while EXCLUDING a CI-rope-shaped changer (=no). So the RESIDUAL column can read
  non-zero AND discriminates residual from CI-rope — the four-leg zero is a residual-shape zero,
  not merely a machinery-works zero.

## Result 3 — the four CONVERT rows certify against their artifacts (content-read, not existence-check)
- **FSM:** `FINDINGS.md:44` "only the 3 FSM seats change; 89 others byte-identical" ✓
- **FCR-9:** `FCR9_FINDINGS.md:35` "7 change: 6 routed-verdict + statutory_debt (ensemble)"; the
  apparent "6 verdicts" vs 7-line diff reconciles — "verdict" = base|join|grade triple (grade
  correction→commentary counts); 3 routed seats byte-identical (kept `correction`). No divergence.
- **constructed-3:** `CONSTRUCTED3_FINDINGS.md` "#2 keeps RED byte-identical; 2 change; 47 inert";
  the file itself scopes the residual ("mountain-input + low/constraint have 0 live changers").
- **FNL:** `fnl_twin_diff_summary.log` haiku FNL=6 / flash FNL=8 (matches sweep 0/6/8/0/0); 8/14
  render RED; H¹ 0→3 present on the RED seats (artifact-witnessed; not re-run live this turn).

## Guard ruling (for the close)
Residual clauses convert to **route-to-`unknown` + informational flag** (clause-body, NOT a
validation-suite counter: `dr_type` is the universal classify path — 47 non-test callers — so a
suite counter is a lagging detector). Abstain target is `unknown`, never `untyped` (CLAUDE.md token
rule: `untyped` leaking into a type vector inflates H¹). Byte-identical on all four legs today;
loud on any future fire.
