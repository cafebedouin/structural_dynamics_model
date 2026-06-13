# OQ-117(b) — within-arm proxy + matched-fed-arm positive control

**Date:** 2026-06-13. **Status:** evidence, not a ruling. The (c) design call (decouple at SCOPE
vs document-as-condition) remains the operator's; this writeup well-poses the fork, it does not
take it.

**Origin:** OQ-117's resolution path (b) — "the OQ-78 fate-2 read over OQ-109 Phase C: archive =
fed arm, cohort-zero regen = withheld arm on matched seeds — compare divergence rates and the
ε-grid endpoints across arms." Phase C landed (cohort-zero swap, replicate spend), so (b) became
checkable. This pass ran the comparator positive control (b) requires *first*, then the analysis
that survives it.

## 1. Positive-control verdict — the matched fed arm OQ-117(b) specifies does NOT exist

Witness: `comparator_positive_control.txt` (same dir).

- **Probe is live** (closes the empty-grep trap): the exact-name search FOUND the withheld
  replicates (`free_market_naturalization_d1/d2/d3.json`), so its NONE returns for fed-arm story
  files are real absence, not a dead probe.
- **`kernel_v2_test2` holds only `manifest.json`, zero story JSONs.** The "archive = fed arm" in
  (b) is empty of these stories.
- **No fed-arm story was ever generated for the 5 σ/seat kernels.** The only story-generating event
  is `dcfaea97` (the withheld replicate spend). Elsewhere the kernels appear only as
  `*.manifest.json` (SCOPE decompose output), not generated stories; `kernel_run_01/02/03` produced
  manifests + grouping files, zero stories.
- **kernel_v1 (the operator's pointer) is the trap, not the rescue.** It has topic-adjacent
  siblings under *different* kernel identity (`market_naturalization__*` ≠ `free_market_naturalization`,
  `qwerty_persistence_*` ≠ `qwerty_path_naturalization`, `press_reformation_causality__*` ≠
  `printing_press_reformation`, `zero_as_number_entry__*` split into 3 reading files), reading-split
  structure, and — checked on a sibling — **no `claimed_type` field at all** (pre-migration schema;
  PERSPECTIVE-prose, the OQ-70 bait template). Using it as the fed arm stacks three confounds on
  the fed/withheld contrast: (i) kernel identity (cross-sibling = analogical only), (ii) era/regime
  (pre-reset, pre-de-leak), (iii) feeding *mechanism* (kernel_v1 was fed via the leaked type-bands
  + perspective examples the de-leak removed — a different and stronger channel than the
  manifest-hypothesis one OQ-117 interrogates).

**Consequence (corrects the prior "near-free" framing):** (b) as literally specified — a cross-arm
divergence rate over matched story pairs — is **not zero-spend; it is not computable at all**
without funding a matched fed-arm generation. The positive control is what caught this; a
presence-check would have called kernel_v1 the comparator and produced a confounded number.

## 2. Within-arm proxy — computable now, and run

Witness: `within_arm_proxy.txt` / `within_arm_probe.py` (same dir). The decompose manifest carries
the per-axis `hypothesis` (the claim that *would* be fed in production); the withheld replicate
carries the model's freely-authored claim. That collapses the fed/withheld contrast into data on
disk. This is **a labeled proxy for (b)'s core question** (does withholding the hypothesis move the
claim) — **not** the story-pair divergence-rate instrument (b) names.

Result: **`claimed_type` = mountain in 15/15 draws, draw-stable.** The claim side is reconstructed
from title+domain+summary alone; the hypothesis token is not what selects "mountain." ε carries
real authored variance (0.08–0.68, mostly draw-stable), so the divergence machinery is **not**
purely manufactured.

### Mechanism-established, direction-confounded (the load-bearing distinction)

"Mountain 15/15" is consistent with two *different* mechanisms that point at *different* (c)
branches, and the proxy cannot separate them because all 5 kernels were **selected as
contested-naturalization kernels, and naturalization IS the mountain claim**:

- the model idiomatically defaults to mountain when the hypothesis is withheld (idiom-persists →
  document-as-condition); **or**
- these five kernels genuinely *are* mountains and the model read them correctly (the hypothesis
  token was never doing work because there was no work to do).

So the proxy establishes the **mechanism** (seed-spec withholds the claim by construction; the
claim is nonetheless reconstructed) but **cannot establish which idiom** without a
non-naturalization kernel in the set. It does **not** lean document-as-condition — reading it that
way is the selection confound leaking into the conclusion.

### What survives the confound cleanly

- **`free_market_naturalization` (its own line — the standing summary-smuggling example):** claimed
  mountain at **ε = 0.68, stable across 3 draws.** Claimed-vs-computed divergence **persists with
  the hypothesis withheld** — the summary smuggles the claim, ε is authored honestly, the gap is
  real. This is a *within-story* claim-vs-metric gap, so it does **not** depend on the
  kernel-selection question. It is the witnessed shape (b) existed to find, and it is the clean
  cell. One cell, not the verdict.
- **`printing_press_reformation`:** the lone ε-**unstable** kernel (0.38/0.42/0.68) — an OQ-118
  cast-instability echo on the *metric* side. Clean; cross-thread coherence check passing.

## 3. The (c) fork, well-posed (operator's call; not taken here)

- **Accept the mechanism + the free_market ε-divergence as sufficient for document-as-condition.**
  Defensible — the mechanism is established and the perturbation principle already frames ε as
  authored, so documenting claim-coupling does not betray the design. But it rules (c) on
  **direction-confounded** evidence; the close note must say the *direction* was not established,
  only the mechanism, with free_market's ε-divergence cited as the clean cell. **The entry must not
  claim the proxy showed mountain-is-the-idiom — it didn't.**
- **Fund the disambiguating evidence before ruling.** The buy that unblocks the *direction* is
  **non-naturalization kernels in the withheld set** (does the model still reach for mountain when
  the kernel isn't a naturalization kernel?) — that kills the selection confound. The
  **matched fed-arm spend** is a *separate, optional* buy: it yields the literal cross-arm rate (b)
  names but does **not** touch the selection confound. Named separately so a cold read does not
  conflate them.

### Two distinct spends (cost, batch API, sonnet-4-5; see §4)

| Spend | What it buys | Touches selection confound? |
|---|---|---|
| **A — non-naturalization withheld draws** (~5 non-naturalization contested kernels × 3) | unblocks the **direction** question for (c) | **Yes** — the buy that converts mechanism-established → rulable |
| **B — matched fed-arm** (same 5 σ/seat kernels × 3, hypothesis fed) | the literal cross-arm **rate** OQ-117(b) names | No — optional, rate-on-record only |

## 4. Cost (priced off the 15-draw batch that already ran, `dcfaea97`)

Anchor: `claude-sonnet-4-5-20250929`, Sonnet-tier **$3/$15 per MTok** in/out, **Batch API −50% →
$1.50/$7.50**, prompt-cache write 1.25× / read 0.1× of input. Per-draw empirical sizes: static
generation prefix ≈ 19K tokens (cacheable), seed tail ≈ 0.5K, output JSON ≈ 6K tokens.

- **Per 15-draw batch:** ≈ **$0.75–$1.20** (≈ $0.77 with the static prefix cached, ≈ $1.11 if the
  cache is treated as cold). Output dominates (~6K × $7.50/MTok ≈ $0.045/draw); input is near-free
  under caching.
- **Spend A ≈ $1. Spend B ≈ $1. Both ≈ $2 total.** Dollars are negligible; the real costs are the
  small code change for Spend B (extend the seed-spec to feed the hypothesis) and the
  analysis/witnessing time. Spend A needs no code change — pick 5 non-naturalization seeds (42
  candidates in `kernel_seeds.json`) whose honest claim is NOT mountain, run the existing
  `cohort_replicate_batch.py --ids ... --draws 3`.

## Files
- `comparator_positive_control.txt` — §1 witnesses (probe-fires control + fed-arm absence + kernel_v1)
- `within_arm_proxy.txt` / `within_arm_probe.py` — §2 within-arm proxy (manifest-hyp vs withheld claim/ε)
