# OQ-117 Spend A / Spend B — PRE-REGISTRATION (frozen before any draw exists)

**Freeze rule (inherited from OQ-109 `5f2a626c`):** this file is committed BEFORE the batches run.
A wrongly-specified criterion is **halt-and-escalate**, never inline-amended. Mismatch = finding.

**Common regime:** `claude-sonnet-4-5-20250929`, temperature 0.2, seed-spec = title + domain +
summary (`cohort_zero_regen.source_desc`), 5 kernels × 3 draws each. Driver: `oq117_spend_driver.py`
(same dir). Spend B adds one line to the source — `Declared claim type … : mountain` — and changes
nothing else.

---

## Spend A — non-naturalization withheld draws (resolves the within-arm proxy's idiom-vs-correct-read confound)

**Question:** does the model still author `claimed_type = mountain` when the kernel is NOT a
naturalization kernel? The within-arm proxy found mountain 15/15 on five *contested-naturalization*
kernels — which cannot separate "mountain is the idiom" from "these kernels are mountains."

**Kernels (all impositions / coerced reversals / transitions / declines — honest claim NOT mountain):**

| kernel | structure | pre-registered honest claim (NOT mountain) |
|---|---|---|
| `turkish_alphabet_reform_1928` | top-down imposition, zero prior practitioners | snare or scaffold |
| `meiji_calendar_dress_imposition` | forced top-down imposition | snare or scaffold |
| `mormon_polygamy_manifesto_1890` | reversal under US federal coercion | snare or piton |
| `gold_to_fiat_transition` | shift to a constructed monetary convention | scaffold or rope |
| `dueling_decline` | practice declining to unthinkability | piton or scaffold |

A `mountain` claim on any of these is a **false-mountain** (an imposition/decline claiming to be
natural/inevitable), not the honest read.

**Discriminating metric:** count of the 15 draws with `base_properties.claimed_type == "mountain"`.

**Binning (FROZEN):**
- **≥ 8 / 15 mountain → Outcome A1 (IDIOM-DEFAULT).** The model reaches for mountain regardless of
  kernel structure → `claimed_type` is contentless as a signal. Retro-resolves the within-arm
  proxy: "mountain 15/15" on the naturalization kernels was idiom, not correct-read. (c) reading:
  the claim channel is idiom, not instruction → decoupling it buys nothing → **document-as-condition**.
- **≤ 3 / 15 mountain → Outcome A2 (STRUCTURE-TRACKING).** The model reads the kernel from
  title+domain+summary and authors the honest non-mountain claim → "mountain 15/15" on the
  naturalization kernels was **correct-read**, not idiom. (c) reading: `claimed_type` is honestly
  reconstructed from the summary → feeding the hypothesis is redundant → **document-as-condition**,
  with provenance "summary-carried," not "instructed."
- **4–7 / 15 mountain → Outcome A3 (MIXED).** No clean idiom/read line — report the per-kernel
  distribution, escalate, do not rule.

Note: A1 and A2 both lean document-ish but for *different* reasons; Spend A's job is to resolve the
idiom-vs-read confound, not to decide (c) — that is Spend B's job.

---

## Spend B — matched fed-arm (tests whether feeding the claim manufactures metric concordance — the (c) axis)

**Question:** in production the claim (`hypothesis → claimed_type`) is instructed. Does instructing
it *pull the metric* (ε) toward claim-consistent values that the model would not otherwise author?

**Design:** the five σ/seat kernels (same as the withheld replicate arm), claim FED = `mountain`
(the value the withheld arm freely landed on). Claim is thus held constant across arms; the **only**
difference is fed-vs-withheld of that claim. Measurement = `base_properties.extractiveness` (ε).
A fed mountain claim should bias ε DOWN (mountains are low-extraction) IF feeding pulls the metric.

**Headline metric-twin (FROZEN):** `free_market_naturalization` fed-arm ε (3 draws + mean). This is
the sharpest cell — its withheld-arm ε was **0.68 stable** (claimed mountain, divergent/honest).

**Binning (FROZEN), on the free_market fed-arm mean ε:**
- **≤ 0.40 → Outcome B1 (MANUFACTURED CONCORDANCE).** Feeding the claim pulled ε down toward
  mountain-consistent → the production concordance partly measures instruction, not authoring →
  (c): **DECOUPLE** buys back real authoring variance.
- **≥ 0.55 → Outcome B2 (FEEDING INERT ON THE METRIC).** ε held near its honest withheld value
  despite the fed mountain claim → the divergence/concordance is idiom/honest, not instruction-driven
  → (c): **DOCUMENT-AS-CONDITION** (decoupling buys ~nothing).
- **0.40–0.55 → Outcome B3 (AMBIGUOUS).** Report per-kernel ε deltas, escalate.

**Secondary (reported, not gating):** set-mean ε delta (fed − withheld) across all 5 kernels;
fed-arm `claimed_type` stability (expected mountain since fed); any `validate_json` failures —
a fed mountain claim at high authored ε may trip `MOUNTAIN_METRIC_CONFLICT` (OQ-116), which is
itself a finding (the fed arm cannot be authored self-consistently) and is recorded, not discarded.

**Withheld-arm comparator (already on disk, the OQ-109 replicates):** qwerty 0.18, free_market 0.68,
total_war 0.18, printing_press 0.38/0.42/0.68, zero_as_number 0.08 — all claimed mountain.

---

## Discipline carried

- Freeze-before-draw: this file committed before either batch runs (witness = the commit preceding
  the batch IDs).
- No numeric disclosure of config thresholds to the authoring stage (OQ-78 direction-of-fix). The
  fed line names only the claim TYPE (`mountain`), never an ε number or bin boundary.
- Escalate-don't-redraw: a mismatch against these bins is the finding; no re-run of a failed
  prediction, no inline amendment.
