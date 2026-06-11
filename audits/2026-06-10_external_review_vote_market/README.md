# External-review triage — vote-market run (2026-06-10)

First instance of the ongoing external-review practice. A Web Claude instance reviewed the six
vote-market constraint reports (`agenda_conditioning`, `information_suppression`,
`organization_floor`, `post_1998_convergence`, `scale_ceiling`, `veto_asymmetry`); eight defect
claims + a protect-list returned. Every claim was verified against the reports and engine code
before any OQ opened — external output is hypothesis, not evidence (audit discipline).

- Feedback: `raw_feedback.md` (reconstructed — see its provenance note; verbatim not preserved
  this session).
- Report evidence: `reports/` (copies of the six 2026-06-10 reports; `outputs/` is gitignored,
  so these copies are the stable substrate the OQ witnesses cite by line number).
- Run-outputs commit: **`2d54826c`** (six testsets/json + essay + source + `validation_suite.pl`
  regen on main).
- Gate witness: `gate_witness.txt` — `run_dynamic_suite` over the full 48-constraint corpus,
  exit 0, 0 errors / 0 warnings (positive control: the run reaches `work_displacement_dignity`,
  test_case 48, so it iterated the whole corpus rather than short-circuiting).

## Verification table (claim → witness → disposition)

| # | Claim | Verified witness | Disposition |
|---|-------|------------------|-------------|
| 1 | GREEN over a 0%-authored grid; provenance line unconsumed | `enhanced_report.py:518` `build_verdict_banner` reads only `diagnostic_verdict`; `[PROVENANCE] grid 32 = authored 0 + injected-0.5 4 + imputed-from-priors 28` at scale_ceiling:22 feeds nothing | **OQ-98** (new) |
| 2 | GREEN + "no tensions" (scale_ceiling:95-96) coexists with `! ALERT [severe]: type_1_false_summit` (:222); snare-signature finding untagged (agenda_conditioning:260) | alerts are Prolog-side (`report_generator.pl:62`), never cross into the Python verdict | **OQ-98** (merged) |
| 3 | ≥3 quantities named "confidence"; HARD DISAGREEMENT at rival P=0.95 | MaxEnt numeric, corpus histogram, Prolog categorical; trigger is type-mismatch only, ignores rival P | **OQ-100** (new) |
| 4 | `W1: 0.0000` with no positive control | read from upstream `persistence_results.json`; likely measures the leveled imputed grid | evidence → **OQ-93** + **OQ-98** |
| 5 | `Resistance to Change: MISSING (using default 0.0)` in the forensic audit (4× in scale_ceiling) | Prolog forensic-audit section; default is the value most likely to flip mountain→rope | **OQ-44** instance |
| 6 | "ONTOLOGICAL FRAUD DETECTION" overclaims | `report_generator.pl:59`, unconditional header; inputs are author-assigned ⇒ self-consistency only | **OQ-100** |
| 7 | Omega: identical N=30 five-step template; literal `Constraint: unknown` | `report_generator.pl:572-583` `resolve_omega_source/3` fallback binds `Constraint = unknown` | **OQ-99** (new) |
| 8 | ~360-line reports, 2–3× repetition, ~40 usable lines | reports 360-413 lines; repetition confirmed | **OQ-100(d)**, severable |

## Disposition kinds (for the ongoing-practice convention)

new-OQ / instance-into-existing-OQ / evidence-into-existing-OQ / already-covered /
declined-with-reason.

## Ongoing-practice convention (recorded here, applies to all future external reviews)

External-review feedback lands as `audits/<date>_external_review_<slug>/` with:
- `raw_feedback.md` — **verbatim** (this batch's was reconstructed because the verbatim text
  wasn't carried forward; do not repeat that — capture verbatim at receipt);
- `reports/` — copies of every report a witness cites (outputs/ is gitignored);
- `README.md` — the triage table (claim → witness → disposition), the protect-list, the
  run-outputs commit hash, and the disposition kinds above.
Every claim verified against code/reports before any OQ opens.
