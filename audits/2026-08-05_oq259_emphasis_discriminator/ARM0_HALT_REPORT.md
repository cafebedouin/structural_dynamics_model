# Arm 0 result: pre-registered HALT rule FIRED — no Arm-1 spend

Date: 2026-08-05. Read-only deciding pass over the four Arm-0 manifests (copies in this
dir), scored against `PROPOSAL_ADDENDUM.md` §4 (committed `c4785da7` BEFORE any run).
**Outcome: Cap K NW fails the reproduce-rate gate in BOTH re-runs (3/6 and 2/6, rule:
HALT if < 4/6 in either). Arm 1 and Arm 2 were NOT run. Phase-3 P1/P2 scoring is moot —
there are no emphasis-aware manifests to score.** Total spend: 4 of ≤8 decompose calls
(≈884K input tok; the two Arm-1/Arm-2-reserved Cap-class calls and the AT Fiat call were
not made).

## Run witnesses (all four runs)

| Run | Input md5 (== `1bd57a84` baseline) | Ingest line | Manifest |
|---|---|---|---|
| A0-B1 | `722602a7…` ✓ | `102,807 tok / cap 975,616 (headroom 872,809) [claude-sonnet-5]` | `biopower_healthcare_kernel_2026_20260805_144612` |
| A0-B2 | `722602a7…` ✓ | identical | `biopower_nhi_debate_2026_20260805_144823` |
| A0-C1 | `18f726ab…` ✓ | `339,501 tok / cap 975,616 (headroom 636,115) [claude-sonnet-5]` | `capitalism_k_debate_2026_20260805_145017` |
| A0-C2 | `18f726ab…` ✓ | identical | `capitalism_kritik_ndi2026_20260805_145128` |

Per-run checks, all four runs: whole-doc single-prompt ingest (no chunking/windowing in
logs); **no `*_brief.md` written**; corpus untouched (listing-diff on `prolog/testsets/`
and `json/` empty against the pre-run snapshot after every run). Logs:
`arm0_{biopower,capk}_run{1,2}.log` in this dir. Inputs are byte-identical to the
2026-08-03 runs, so drift below = decompose stochasticity + model/API drift only.

## HALT scoring (name-blind subject+stance vs the §4 pinned baseline sets)

### Biopower NW — PASSES (4/6 and 5/6)

| Baseline reading | A0-B1 match | A0-B2 match |
|---|---|---|
| 1 NHI as constitutive biopower (totalizing) | `totalizing_capture_reading` | `foucauldian_orthodox_reading` |
| 2 NHI corrigible institution (Illich/iatrogenesis) | — | — |
| 3 counter-conduct vs state integration | `counter_conduct_praxis_reading` | `abolitionist_autonomist_reading` |
| 4 anti-state-phobia coalition politics (Ilott) | — | `governmentality_pluralist_reading` (Ilott verbatim) |
| 5 empirically underdetermined/unfalsifiable | `epistemic_artifact_reading` (Meloni) | `historicist_debunking_reading` (Meloni) |
| 6 post-politics self-fulfilling/disabling | `contestable_terrain_reading` (Kioupkiolis verbatim) | `liberal_political_reading` (Kioupkiolis/Dean) |
| **Reproduce-rate** | **4/6 = 0.667 (boundary pass)** | **5/6** |

Note on B1 row 6: `contestable_terrain_reading` blends #6's post-politics critique with
#4-adjacent state-as-mediation content; it credits exactly one baseline reading (best
match #6, Kioupkiolis named in its authority grounding). The rate is invariant to the
#4-vs-#6 assignment choice (either way exactly one of the two is credited).

Notable: baseline reading #2 (`reformist_iatrogenic_reading`) reproduced in NEITHER
re-run — the one hit SCORING.md had marked CONTESTABLE in 2026-08-03 is exactly the one
that churns. In B1 its Illich material was absorbed into the totalizing reading with the
OPPOSITE stance (radical monopoly as capture, not corrigibility).

### Cap K NW — FAILS in both re-runs (3/6 and 2/6) → HALT

| Baseline reading | A0-C1 match | A0-C2 match |
|---|---|---|
| 1 accumulation/exploitation compulsion, harms structural | `structural_necessity_reading` | `capitalism_structural_health_harm` (axis) |
| 2 market-exchange institutions, reform-correctable | `reformable_system_reading` | `capitalism_empirical_sustainability` (axis) |
| 3 imperial world-system of unequal exchange | — | — |
| 4 historically specific neoliberal financialized regime | — | — |
| 5 growth imperative separable (degrowth) | `growth_paradigm_critique_reading` | — |
| 6 framework axis (epistemology vs plan outcomes) | present only as sel=False/deferred → not counted | present only as sel=False/deferred → not counted |
| **Reproduce-rate** | **3/6 = 0.50 < 2/3 → FAIL** | **2/6 = 0.33 < 2/3 → FAIL** |

Robustness: under a LENIENT rule counting the deselected framework axis, C1 would pass
at 4/6 but C2 still fails (3/6) — the HALT verdict does not depend on the
selected-axes-only clause. C1's `innovation_engine_reading` is a second reading in
baseline #2's territory (Hayek/knowledge-problem), crediting nothing new. A0-C2 is the
extreme datum: SCOPE minted **no contested kernel at all** (`is_contested_kernel`
absent; 3 selected axes, arsenal-shaped: core-K vs sustainability vs alt-fails).

## Read-through churn control (§4; presence at ANY altitude, both re-runs required)

| Read-through reading | Re-run 1 | Re-run 2 | Verdict |
|---|---|---|---|
| Cap: `world_system_reading` | absent | absent | **CHURNED** |
| Cap: `growth_process_reading` | present (reading altitude) | absent | **CHURNED** |
| Bio: `coalition_governmentality_reading` | absent (no distinct match; nearest content fused into `contestable_terrain`) | present (`governmentality_pluralist_reading`) | **CHURNED** |
| Bio: `insurance_as_risk_technology_axis` | present (deferred) | present (deferred) | **STABLE** |

Consequence under §3: Cap K effective n = 0 (INDETERMINATE by construction — even
without the HALT, no Cap K P1 verdict was reachable); Biopower effective n = 1 (weak
verdicts only). Three of the four read-through readings the discriminator exists to test
churn at fixed input, without any conversion change.

## What the fired gate means (pre-committed semantics)

The predicted P1 effects are **inside the churn floor**: on the 340K-token file, two
same-input re-runs reproduce at most half the baseline reading set, and the specific
readings P1 bets on (the read-throughs) churn 3-of-4 at fixed bytes. An Arm-1
"vanished/survived" observation on Cap K would be uninterpretable, and on Biopower worth
at most one weak vote (insurance axis only). The n=2 floor caveat cuts the OTHER way
too: these are floors, so true churn may be higher, not lower.

**Standing-result impact (no new runs needed to state this):** the 2026-08-03
"replicate-stable (f)" verdict is *cross-file* stability; Arm 0 shows it does NOT imply
*within-file redraw* stability of individual readings. Any future OQ-259-family design
that scores individual-reading presence needs a churn arm sized to the effect, or a
churn-robust observable (e.g. idiom SHARE, which needs no per-reading identity).

## Options for the operator ruling (reassessment; ranked)

1. **(Recommended) Close item 1 as HALTED-INFORMATIVE:** the discriminator as designed
   cannot detect its target above the witnessed churn floor at n≈2; redesign around
   churn-robust observables (idiom share / tag-echo rate over many redraws, or k>2
   re-runs pooled) before any emphasis-aware spend. The emphasis-aware `.md` conversions
   and extractor are built, verified, committed — ready whenever a redesign runs.
2. **Proceed Biopower + AT Fiat only** (~$1–1.5): salvages a weak 1-vote insurance-axis
   read and the AT Fiat manipulation check; Cap K stays out per its failed gate.
3. **Proceed all three anyway:** spends ~$2.5–3 for results the pre-registration says
   are uninterpretable on Cap K; would need an explicit operator override recorded on
   OQ-259.

No further Phase-2/3 step executes without this ruling.

## POSTSCRIPT — operator ruling (2026-08-05, same day): option 1, extended

**Item 1 CLOSED; the Arm-0 result IS the finding** — bigger than the emphasis question
was going to produce. Options 2 and 3 ruled DEAD, not weaker: option 2's evidence base
is n=1 (one stable reading + a manipulation check that the addendum's own §2 framing
strips of evidential weight) — "an anecdote with a pre-registration wrapper"; option 3
adds a file whose gate reading (a re-run minting no contested kernel) is closer to
instrument failure than noise. Precision riders: Biopower's 4/6 is a zero-margin pass
at n=2; AT Fiat has no Arm-0 measurement at all (smallest file, proportional
per-reading noise likely worse, untested).

**Sampling check (executed with the ruling):** decompose's architect is
`claude-sonnet-5`; `agent/llm_call.py:112` `sampling_overrides` OMITS temperature for
that tier by design (400 on non-default) and the Anthropic API has no seed parameter.
Churn is therefore not an unpinned-parameter artifact, and no temp-0 comparison
regime exists — the measured magnitude is the production regime's own.

**Disposition:** program-wide propagation minted as **OQ-264** (single-draw
per-reading findings carry unquantified error bars; k-redraw variance-floor standard;
fewer-files × more-draws cost geometry; Cap K named out of scope for per-reading
measurement). OQ-259 items 2–3 blocked_on OQ-264; item 3's genre-flag standard
restated to appearance-across-k-redraws on the OQ-259 entry.
