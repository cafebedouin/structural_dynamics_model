# OQ-117 Spend A / Spend B — RESULTS

Predictions + binning frozen at commit `9aa1c90e` (`PRE_REGISTRATION.md`) BEFORE either batch ran.
Spend A batch `msgbatch_01VKuJTK5LeoMvbJyYMixKPt`, Spend B batch `msgbatch_01MJ2Tsi8fSwNcsmmTJijAQj`
(both submitted after the freeze). Witnesses: `spend_results.txt`, `nonnat_withheld/*.raw.json`,
`fed_arm/*.json`. Cost: ~$1.30/batch uncached upper bound (sonnet-4-5, batch API), ~$2.60 total.

## Spend A — Outcome A2 (STRUCTURE-TRACKING), conclusive

**mountain count = 0 / 15** (frozen bin: ≤3 → A2). Every non-naturalization kernel tracked its honest
structure:

| kernel | structure | claimed_type (3 draws) | pre-registered honest claim | match |
|---|---|---|---|---|
| turkish_alphabet_reform_1928 | imposition | scaffold, scaffold, scaffold | snare/scaffold | ✓ |
| meiji_calendar_dress_imposition | imposition | scaffold, rope, scaffold | snare/scaffold | ✓ |
| mormon_polygamy_manifesto_1890 | coerced reversal | snare, tangled_rope, rope | snare/piton | ✓ (snare hit) |
| gold_to_fiat_transition | transition | tangled_rope ×3 | scaffold/rope | non-mountain (adjacent) |
| dueling_decline | decline | rope ×3 | piton/scaffold | non-mountain (adjacent) |

**Reads:** the model authors the honest **non-mountain** claim from title+domain+summary alone. So
"mountain 15/15" on the five contested-naturalization kernels (within-arm proxy) was **correct-read,
not idiom-default.** The selection confound on the within-arm proxy's claim-uniformity finding is
**killed** — `claimed_type` is a genuine structure-tracking authored read, not a default the model
reaches for. (c) implication on the **claim channel:** the claim is honestly summary-reconstructed,
so feeding the hypothesis is **redundant** with what the model authors anyway → document-as-condition
suffices on the claim side. (Coherence note: non-mountain claims came with high ε 0.68–0.82; mountain
claims with low-to-mid ε — the model authors claim and metric *coherently* from the summary, the
free-gate residual stands.)

## Spend B — Outcome B3 (AMBIGUOUS by the frozen headline), with an informative per-cell structure

Fed claim = mountain (the value the withheld arm freely landed on); ε is the measurement.

**Headline (frozen): free_market fed-arm mean ε = 0.427.** Bin 0.40–0.55 → **B3 (ambiguous →
escalate).** Per freeze discipline this is NOT narrated into B1 (≤0.40); it is reported as ambiguous.

| kernel | withheld ε (mean) | fed ε (mean) | Δ |
|---|---|---|---|
| qwerty_path_naturalization | 0.18 | 0.18 | +0.00 |
| **free_market_naturalization** | **0.68 (stable)** | **0.43** — draws [0.42, 0.18, 0.68] | **−0.25** |
| total_war_unthinkability | 0.18 | 0.08 | −0.10 |
| printing_press_reformation | 0.49 | 0.59 | +0.10 |
| zero_as_number | 0.08 | 0.08 | +0.00 |
| **set mean** | **0.32** | **0.27** | **−0.05** |

**The informative structure (the real finding under the ambiguous headline):** feeding "mountain"
moves ε **only where the honest claim and metric diverged.** free_market — the one kernel whose
withheld arm authored a claimed-mountain at *high, honest* ε (0.68, dead-stable) — is exactly where
feeding bites: ε drops 0.25 and **destabilizes** to [0.42, 0.18, 0.68], one draw collapsing to the
mountain-consistent floor. total_war nudges to the floor (−0.10). The two kernels already at
mountain-consistent ε (qwerty 0.18, zero 0.08) don't move — nothing to manufacture. printing_press
perversely rises (+0.10). So feeding partially manufactures concordance, **concentrated at the
divergent cell**, but heterogeneously and without clearing the frozen B1 line.

## Combined (c) read — escalate, operator's floor (not taken here)

- **Claim channel (Spend A, A2, conclusive):** honest / summary-reconstructed, not idiom →
  **document-as-condition suffices on the claim.** The earlier within-arm-proxy direction confound
  is resolved in this direction.
- **Metric channel (Spend B, B3, ambiguous-leaning-manufactured):** feeding the claim *does*
  partially drag ε toward claim-consistency **at exactly the cells where claim and metric honestly
  diverge** (free_market −0.25 + destabilization), but the headline did not cross the pre-registered
  ≤0.40 threshold. There is a **real but bounded** decouple rationale, localized to divergent cells —
  not the uniform manufactured-concordance B1 would assert, nor the inertness B2 would assert.
- **Net (escalated, not ruled):** the decouple-vs-document call is genuinely intermediate. The
  honest synthesis: document-as-condition is right for the **claim** side; the **metric** side shows
  partial, divergent-cell-localized manufactured concordance that a decouple would address but that
  did not clear the frozen bar. The operator's (c) floor stands; this evidence makes the call
  *intermediate*, not clean either way, and per the freeze it is escalated rather than narrated to
  a verdict.

**Discipline honored:** freeze-before-draw (`9aa1c90e` precedes both batch IDs); the ambiguous
headline is reported as ambiguous (no inline amendment of the bin); escalate-don't-redraw; no numeric
threshold disclosed to the authoring stage (the fed line named only the claim type).

## Files
- `PRE_REGISTRATION.md` — frozen predictions + bins (committed `9aa1c90e`)
- `oq117_spend_driver.py` — submit driver (reuses canonical building blocks)
- `collect.py` — collect-only re-fetch by batch ID (no re-spend)
- `spend_results.txt` — the combined A/B table (this writeup's source numbers)
- `nonnat_withheld/*.raw.json` (Spend A, 15), `fed_arm/*.json` (Spend B, 15)
