# Cross-author ε probe — step 1 of the reader-profile plan (Claude-web, 2026-07-27)

**Date:** 2026-07-27 · **Type:** characterization (read-only; no engine run) ·
**Script:** `epsilon_cross_author.py` (re-runnable) · **Evidence:**
`epsilon_cross_author_results.json`, `epsilon_cross_author.log`

Context: a Claude-web planning thread proposed sweeping ε stability across *reader profiles*
(author models) rather than only across r=0.02 perturbation, with step 1 a pure read: per-reading
ε deltas between independently-authored legs over the same readings. This audit executes step 1,
widened from the proposed 2-leg (haiku/flash) probe to the full 4-author twin panel now on disk
(haiku, flash, kimi, sonnet — 957 readings shared by filename across all four).

## Claims checked (the plan's load-bearing assumptions)

1. **`epsilon_provenance/5` exists and records the author** — CONFIRMED
   (`narrative_ontology.pl:108`; `(C, ValueAsWritten, Author, GenerationRunId, Route)`).
   Author is already a **model atom** (`'claude-sonnet-5'`, `'kimi-k2.6'`) — i.e. a coarse
   reader-profile token exists today, joinable per-story. Coverage is generator-forward per the
   2026-07-03 ruling: kimi 1005/1005 and sonnet 1001/1001 files carry facts; haiku/flash 0/960
   (the declared loud-null stratum); live testsets 87/199.
2. **"If reconciliation harmonized ε, step 1 is void"** — the kill condition did NOT fire.
   ε was not harmonized: haiku~flash share-identical is **3.9%**, mean |Δε| **0.105** over 957
   shared readings. (Reconciliation at `0ccc03cf` moved files; it did not equalize values —
   corroborates the OQ-58 audit's byte-intact finding.)
3. **Twins share reading identity** — CONFIRMED: haiku/flash filenames match 960/960;
   kimi overlaps 960, sonnet 957; 4-way shared set = 957.
4. **Not already done** — the five-leg audit (`audits/2026-07-20_five_leg_twin_comparison/`)
   compared H¹ bands, verdict_join, and maxent_top_type across legs, never ε directly. This
   probe is new evidence, not a re-run.

## Findings (all numbers in `epsilon_cross_author_results.json`)

- **Cross-author ε divergence is large and pervasive.** Pairwise mean |Δε| 0.080–0.114
  (median 0.06–0.10); only **1.0%** of the 957 readings carry identical ε across all four
  authors; **32.9%** have 4-author spread ≥ 0.20 and **16.9%** ≥ 0.30. Mean spread 0.181.
- **Author-level systematic bias exists** (the cleaner profile signal — draw noise averages out
  over 957 readings): mean ε kimi **0.589** > haiku **0.565** > flash **0.508** > sonnet
  **0.490**. A ~0.10 span in mean ε across authors over the *same* readings. ε is
  author-profile-sensitive — the precondition for the reader-profile thesis.
- **Top diverger is on-thesis but n=1:** `animal_status__abolitionist_reading` — haiku ε=0.00
  vs flash 0.95 / kimi 0.88 / sonnet 0.91, a full-scale flip on the paradigm case of a harmed
  party whose harm is not text-statable. Other high-spread items
  (`aneyoshi_stone_commitment__commemorative_husk`, `quran_ontological_status__uncreated`,
  `dueling_disappearance_mechanism__contraction`) also lean tacit/unstatable-channel; the
  most-agreed items (`fisa_702_statutory_text__*` at 0.15–0.45 exactly equal 4-ways) lean
  text-legible statutory extraction. **This is the hypothesis-generating read only** — whether
  divergence *concentrates* on unstatable-channel constraints needs a per-item legibility coding
  of the top/bottom spread deciles (a judgment call, not computed here).

## Method + controls

Regex extraction of `constraint_metric(_, extractiveness, V)` (fallback
`base_extractiveness/2`) keyed by filename base. Positive control: the hand-verified
haiku/flash pair (`abrahamic_covenant__isaac_covenant_reading`, 0.81 vs 0.70) must reproduce —
PASS. 0 parse failures in any leg; 0 within-file cm/be drift in any leg (3,926 files).

## Caveats

- **Draw noise is not decomposed from profile effect at the per-reading level.** Per the
  *Generation is stochastic* doctrine, each leg is a new draw; pairwise |Δε| conflates
  draw-to-draw variance with author effect. The signed mean shifts are the defensible
  profile-level signal; per-reading deltas are seat-expressive draws. Within-model replicate
  draws would be needed to decompose (none exist on disk).
- **kimi is thinking-on; haiku/flash/sonnet were thinking-off** (runbook §7b regime caveat) —
  kimi's highest mean ε carries that asymmetry by construction.
- **[EDGE] status from the plan (all-LLM panel):** partially discharged, not fully. Four
  authors span three vendors and two reasoning regimes, so "the twins are one reader profile"
  binds less than for the 2-leg probe — and the result is NOT flat, so the flat-is-OPEN branch
  was not reached. But all four authors share the LLM text-legibility inversion: this probe
  shows ε moves with author *within* the LLM class; it cannot show what a non-LLM reader
  profile would author. That residue stands.

## What this settles for the plan's sequencing

Step 1 shows the instrument CAN see cross-author ε signal (both pervasive per-reading
divergence and systematic profile-level bias). Step 2 (matched-pair sweep, operator-picked
topics) is therefore *motivated but not yet forced* — the cheap intermediate is the per-item
legibility coding of the existing top/bottom spread deciles (operator-side read, no
generation). Step 3 (resleeving positive control) remains last and operator-authored.
