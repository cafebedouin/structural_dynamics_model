# OQ-264 — k=3 pooled idiom-share range is a DENOMINATOR artifact (numerator-identical pair carries it all); gate PASS(sens1)/rev-1-INDET on the boundary; OPERATOR CHECKPOINT OPEN

**Executed:** 2026-08-06
**OQ:** OQ-264 (splits_from OQ-259)
**Verdict:** On the free Biopower triple (same-input decompose draws), the pooled idiom
share spans 0.500–0.750 (range 0.25 — PASS(sens1) under the recalibrated rule,
INDETERMINATE under the plan's rev-1 rule, on the exact band boundary), but the entire
range falls between two draws with identical TAG count: the spread is denominator churn
at fixed judgment (D = 6→4→6, a 33% unit swing; fewer readings perversely raise share),
so the pooled observable inherits per-reading churn rather than escaping it — a
specification finding that supersedes the instrument-candidate reading; scorer
disagreement 0/6 observed (95% upper bound 0.393); Phase C spend and closure await the
operator ruling.
**Substrate:** no pipeline run — six committed decompose manifests (three per file,
input md5s `722602a7…`/`18f726ab…` matching the `1bd57a84` baselines); corpus untouched
(no orchestrator runs; zero API calls).
**Evidence map:**

- `PROPOSAL.md` — pre-registration (observable, denominator formula, rubric with
  anchor/holdout split, blinding, gate bands, recalibrated sensitivity modifier,
  control semantics, declared confounds); committed `fd58d3a1` before any scoring.
- `CALIBRATION.txt` — mechanical denominator control (ALL PASS) + share/range lattice +
  quantization simulation; witnesses the rejection of the plan's rev-1 sensitivity
  modifier (P(INDET)=1.0 under every one-error stable null) and the calibration of the
  final rule (P(FAIL)=0 under all nulls).
- `TAG_INVENTORY.txt` — mechanical block-heading extraction of both source files; the
  rubric's fixed reference layer.
- `planted_control.manifest.json` — two synthetic known-answer entries (plant-tag /
  plant-card) for the judged HALT control; vocabulary presence/absence verified with
  positive controls.
- `packet.md` — pooled blinded packet, 37 entries (29 real + 2 planted + 6 seeded
  duplicates), sha256 `3d247582…`, committed `6fc1ef9a` with mapping withheld.
- `calls.json` — blinded idiom calls, committed `0a28d7ca` BEFORE the mapping.
- `mapping.json` — label→draw mapping, committed `e4c293d4` AFTER the calls
  (blind-order evidence is the commit order).
- `holdout_expected.json` — SCORING.md holdout transcription, written after the calls
  commit.
- `PHASE0_REPORT.md` — full compute output and analysis: gate verdict, component ranges
  (TAG/D/share separately), scorer-vs-generator attribution, Cap churn-extreme contrast
  incl. the categorical kernel-minting-churn draw, declared confounds, and the
  **operator checkpoint** (Phase C go/no-go + spend ceiling + k + AT Fiat inclusion,
  or standard-only closure). Opens with the dated **CORRECTION BLOCK** (post-checkpoint
  operator review): denominator-artifact finding, dual-rule headline, duplicate-bound
  correction, concordance result.
- `PROPOSAL_ADDENDUM.md` — post-checkpoint registration (committed `241ec42d` BEFORE
  the computations ran) of the exploratory NON-GATING denominator-sensitivity table,
  the Cap concordance drift probe with its interpretation rule fixed in advance, and
  the two reporting corrections.
- `DENOM_SENSITIVITY.txt` — the registered computations' output: per-convention shares
  and ranges (committed convention 0.250 with a NUMERATOR-IDENTICAL max-pair; fixed
  D=6 gives 0.167, judgment-driven), concordance table (all observables
  discordant/mixed → drift unsupported), duplicate binomial bound (0.393).

**Status: Phases A–B complete (zero spend); this WRITEUP.md is the checkpoint-state
entry point and will be extended at close (Phase C/D per the ruling).** No Phase-C run,
no ISSUES.md closure, and no Phase-D propagation until the operator rules.
