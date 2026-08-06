# OQ-264 — RESOLVED (standard-only): per-reading redraw stability is file-structure-dependent (0.33–1.00 measured); pooled share does not repair it (denominator artifact); k=3-unanimous presence standard minted

**Executed:** 2026-08-06
**OQ:** OQ-264 (splits_from OQ-259) — resolved this date, operator ruling in-session
**Verdict:** Same-input decompose redraw stability of per-reading identity is
FILE-STRUCTURE-DEPENDENT — measured 2/6–3/6 (Cap K NW, 340K arsenal), 4/6–5/6
(Biopower NW, 103K arsenal), 6/6 ×3 (AT Fiat, 34K single-voice) — so no global churn
floor exists; the pooled idiom-share repair FAILS by specification (its k=3 range of
0.25 fell entirely between numerator-identical draws: denominator churn at fixed
judgment, with fewer readings perversely raising share), and OQ-264 resolves as a
STANDARD, not an instrument: presence claims require k=3-unanimous same-input redraws,
share-like ratios must carry their denominator's own churn, and names are never
identity (kernel/reading names churned even at reproduce-rate 1.0).

## The minted standard (what future manifest-scoring audits design against)

1. **No per-reading/per-section claim from a single-draw manifest.** Reading identity is
   redraw-unstable with measured stability 0.33–1.00 depending on source structure; a
   single draw licenses draw-level observations only.
2. **Pooled ratios do not escape unit churn when their denominator is built from the
   same units.** Witnessed: the whole k=3 share range fell between two TAG=3 draws
   (D 6→4). Any share-like observable must (a) report numerator and denominator ranges
   separately, (b) ship a denominator-convention sensitivity table (per-draw D vs fixed
   D vs raw count — DENOM_SENSITIVITY.txt is the template), and (c) never gate at
   thresholds finer than the denominator's own churn.
3. **Presence standard, concrete k:** a manifest feature (reading, omega, flag) counts
   as REPLICATED only if present — by pre-specified name-blind criteria — in ALL 3
   same-input redraws (k=3 unanimous). Present in 1–2 of 3 = observation, not
   detection. The rule is operational and discriminates on measured data: AT Fiat's six
   stable readings pass it 6/6; the arsenal read-through readings (3 of 4 churned at
   n=2) fail it.
4. **Names are never identity** (redraw-hardened form of the standing kind-level
   doctrine): kernel ids took 3 values over 4 same-input AT Fiat draws at
   reproduce-rate 1.0; match by subject+stance, never by name, across any redraw
   boundary.
5. **Cap K NW stays out of scope for per-reading measurement** (operator ruling
   2026-08-05, unchanged); its triple serves as the churn-extreme contrast.
6. **Variance attribution obligation:** any stability gate over judged calls carries
   scorer-vs-generator decomposition instruments (seeded duplicates + mechanical
   comparators); observed-zero disagreement is cited with its binomial bound
   (0/6 → 95% UB 0.393), never as "zero."
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

- `PROPOSAL_ADDENDUM.md` §4 — Phase C pre-registration (AT Fiat k=3 reproduce-rate arm:
  pinned baseline reading set, HALT conditions, firewalls), committed `ac2650ae` before
  any call.
- `READOUT_atfiat.md` — Phase C result: reproduce-rate **6/6, 6/6, 6/6** (the standing
  "no Arm-0 measurement" rider closed with its expectation inverted — the smallest file
  is the only fully redraw-stable one); unit-population churn (D 6→7→7→6) and universal
  name churn persist at reproduce-rate 1.0; no OQ-259 item-3-qualifying genre flag
  (descriptive, non-gating). Manifests `fiat_*_20260806_*.manifest.json` + logs
  `atfiat_k3_run{1,2,3}.log` in this dir; corpus untouched after every run.

**Status: CLOSED (operator ruling 2026-08-06: no further Biopower share draws — the
observable is denominator-confounded; AT Fiat reproduce-rate arm executed as the sole
spend, ≈101K input tok; standard-only closure).** Phase-D propagation: ISSUES.md OQ-264
resolved + OQ-259 items 2–3 restated (k=3-unanimous), Amendment 6 on
`audits/2026-08-03_kritik_ingest/WRITEUP.md` (the plan called it Amendment 5; an
Amendment 5 already existed there), CLAUDE.md Generation-is-stochastic block updated,
KNOWN_STATE.md entry. The share-gate output PASS(sens1) remains on record as the
pre-registered gate's output, superseded in meaning by the correction block (the
specification finding), not re-gated.
