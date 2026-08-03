# OQ-258 referent discriminator — pre-registration (2026-08-03)

**Question.** The 2026-07-27 cross-author ε probe's headline (4-author ε spread sorts by
channel legibility, Fisher p=0.023) is DEGENERATE between two mechanisms: (i) reader-position
variance, (ii) ε-referent ambiguity in the generation contract (OQ-258 — the contract never
fixes whether ε names the arrangement a kernel-reading ENDORSES or the arrangement it
CONTESTS). This audit runs the OQ's pre-specified discriminator: fix the referent, re-author
the top-spread tacit/referent-weak items, remeasure cross-author spread — against a same-items
old-contract redraw null.

**Operator ruling (2026-08-03, recorded before this audit): referent (b) — the contested
arrangement.** ε names the standing arrangement under contest (what the story is about),
assessed by the reading's own lights. The fix pins what ε is *about*, not what it *says*;
values stay reading-indexed (OQ-26 untouched — a welfarist and an abolitionist reading share
the referent and still author different ε). Rationale: (a) generically drives advocacy-class
ε → 0 (haiku's 0.00 on `animal_status__abolitionist_reading` is what (a) produces, not an
outlier) and the flattening would propagate to H¹/type/orbit; (b) also gives reading-free
(flat) stories a referent. **Typed fallback (not chosen):** a declared per-story referent
field with within-declaration comparison — the recorded option if the discriminator comes
back ambiguous (interpretation row 3).

**The contract fix stands regardless of verdict** — the ruling fixes the rebuild's contract;
the discriminator only decides what the 2026-07-27 finding meant.

## Item set (pinned by script, never hand-copied)

`pin_items.py` (this dir) derives the set from the 2026-07-27 audit's `codes.json` +
`coding_batches.json`: **group == top30 AND channel ∈ {tacit, none_apparent} → 18 items**
(10 tacit, 8 none_apparent), full list + per-leg baseline ε in `items_baseline.json`.

Baseline re-extraction control (run 2026-08-03, output pasted in WRITEUP): 14/18 items match
the recorded `top_divergers` values EXACTLY; 4 items
(`genesis_creation_cosmology__literary_framework`,
`honor_violence_legitimacy__contraction_reading`,
`press_reformation_causation__technological_determinism`,
`total_war_possibility_space__nuclear_taboo_reading`) are absent from the results JSON (it
records only the top 25) and carry re-extracted baselines, flagged `reextracted_only`.

Baseline (18 items): mean spread **0.5933** (tacit 0.632, none_apparent 0.545).
**Declared inversion:** within the 18, the stratum gap is NEGATIVE
(none_apparent − tacit = −0.087) — selection into the top-spread set inverted the full-stratum
ordering (0.488 vs 0.427). The secondary statistic below is registered against this actual
within-18 baseline, not the full-stratum sign.

## Arms

- **Arm B (null — runs FIRST):** 18 items × 4 legs under the *unchanged committed* contract
  (prompt commit `8080348c`, schema commit `f1436bd4` at registration). The 18 were *selected
  as* top-spread, so any redraw regresses toward the mean; B separates the contract fix from
  regression.
- **Arm A (fix — runs after the Phase-2 commit):** same 18 × 4 under the referent-(b)-fixed
  contract. ~144 stories total.
- Output namespaces (glob-isolated from all live legs): haiku → run-tag `oq258_armb` /
  `oq258_arma` (`prolog/testsets/<tag>/`); flash/kimi/sonnet → wrapper scripts (this dir)
  that import the leg driver and rebind its module constants (testsets dir, json dir, ladder,
  **and OUT_DIR** — a fourth rebind beyond the plan's three, so failures/rejections logs stay
  arm-scoped instead of overwriting the shared leg logs) to `*_oq258_armb` / `*_oq258_arma`
  paths before `run()`.
- Baseline legs are never written. md5 fingerprints of all four baseline leg dirs taken
  before Arm B and re-taken after measurement — must be byte-unchanged. Runs serialize behind
  any running orchestrator/pipeline (OQ-77 rule; `pgrep` check before each run).

## Per-leg regime table (must reproduce the baseline regimes)

| Leg | Model | Sampling | Transport | Baseline stamp reproduced |
|---|---|---|---|---|
| haiku | `claude-haiku-4-5-20251001` | max_tokens=16384, temperature=api_default | Anthropic batch | `max_tokens=16384,temperature=api_default` |
| flash | `gemini-2.5-flash` | temp 0.1, thinking_budget=0, max 16384 | Gemini batch + cache | `max_tokens=16384,temperature=0.1,thinking_budget=0` |
| kimi | `kimi-k2.6` | max_tokens=32000, temp/reasoning model-default (thinking-ON, ~11.7k reasoning tok/story) | Moonshot batch (k2.6 batch-eligible); declared fallback `--sync` if batch errors — sampling params identical either way | `max_tokens=32000,temperature=model_default,reasoning=model_default` |
| sonnet | `claude-sonnet-5` | thinking disabled, temperature omitted, max 16384 | Anthropic batch | `max_tokens=16384,thinking=disabled,temperature=api_default` |

`prompt_commit` is stamped by `_git_commit_of` at generation time (`git log -1 -- <path>`,
verified `generate_kernel_corpus.py:85-93`; all four legs stamp through the shared
`process_batch_results` → `_provenance_stamp`). Commit-before-Arm-A is therefore sufficient;
the failure mode is an UNCOMMITTED prompt edit (git log ignores the working tree) — Phase 3
pre-flight asserts a clean `git status` on prompt + schema, and the stamp witness asserts
every Arm A story carries the post-fix commit and every Arm B story the pre-fix commit.

## Primary statistic (pinned before spend)

Per-item 4-author spread (max − min of the four legs' ε), 18 items, **Arm A vs Arm B
paired**. Report mean spread per arm + paired deltas + two-sided Wilcoxon signed-rank
(`scipy.stats.wilcoxon(spreads_A, spreads_B, alternative='two-sided')`, scipy 1.15.3).
If any item fails all 3 generation attempts in any leg in either arm, it drops from the
paired statistic (pairwise-complete only) and the drop count is declared in the writeup.

**Minimum effect threshold Δ = 0.15 mean paired spread**, pinned from the baseline numbers:
the 18-item baseline elevation over the legible reference band is 0.5933 − 0.168 = 0.425
(text_legible full-stratum mean spread, RESULTS_legibility_coding.md), so Δ = 0.15 requires
the fix to remove ≥ ~35% of the elevation — at least one entire legible-band's worth (0.168)
of disagreement, and 3× the modal 0.05 ε-authoring quantum. Below Δ, a real-but-partial
referent contribution is NOT headlined as ownership.

**Elevated/collapsed boundary for Arm B = 0.38** (midpoint of the legible band 0.168 and the
18-item baseline 0.5933): mean spread(B) ≥ 0.38 ⇒ B still elevated (redraw preserved the
phenomenon); < 0.38 ⇒ regression-to-mean dominated the redraw.

## Interpretation table (pre-written; A vs B carries every row — the baseline-era statutory
band ~0.17 is directional color only and never carries an interpretation row)

Evaluate in order; first row that fires is the verdict:

1. **mean(B) − mean(A) ≥ 0.15 AND p < 0.05** ⇒ referent ambiguity owned the
   channel-legibility finding (specificity confound declared, see below). OQ-258 resolved;
   2026-07-27 headline re-attributed.
2. **mean(B) < 0.38** ⇒ regression-to-mean dominates; discriminator underpowered — declare
   OPEN, consider the declared-referent-field fallback. (Checked before rows 3–4: if the
   null itself collapsed, neither surviving nor sub-Δ readings are licensed.)
3. **|mean(A) − mean(B)| < 0.05 OR p ≥ 0.05** (B elevated) ⇒ A ≈ B: reader-position variance
   survives its first real test; the channel-conditional reliability caveat
   (KNOWN_STATE 2026-07-27) hardens.
4. **mean(B) − mean(A) ∈ [0.05, 0.15) AND p < 0.05** (B elevated) ⇒ inconclusive; declare
   OPEN with the measured delta, no headline.

## Secondary statistic (pinned; descriptive, no verdict weight)

Within-18 per-stratum mean spreads (tacit n=10, none_apparent n=8), A vs B, against the
declared inverted baseline (−0.087). Directional expectation under the referent mechanism:
both strata collapse under A and not under B, with none_apparent (where no burden channel is
stateable and the referent is hardest to fix) collapsing at least as much as tacit. Reported
as a table; feeds prose, never the verdict row.

## Named single-item witness

`animal_status__abolitionist_reading`, haiku leg, Arm A: under referent (b) haiku should
author high-band ε (≥ 0.5; baseline 0.00 under referent (a), flash/kimi/sonnet baseline
0.88–0.95). Named witness for mechanism legibility, not a verdict criterion.

## Declared limits

- **Scope limit (no statutory arm):** all 18 items sit on the tacit/none_apparent side, so
  this design cannot recompute the channel-legibility SORT — it tests whether the high side
  comes down under the fix, not the sort itself. The within-18 stratum gap is the only
  (partial, secondary) sorting-signal recovery.
- **Specificity confound, declared not closed:** Arm A adds instruction where there was none;
  added specificity can raise agreement independent of WHICH referent it names. Mitigation:
  the added paragraph is minimal and referent-only. Any row-1 verdict is claimed as
  "referent fix (specificity confound declared)", never argued away post hoc.
- **Generation is stochastic (operator ruling 2026-06-12):** re-generated stories are new
  draws, not re-measurements; Arm B exists precisely because of this.

## Failure budget

Known classes from the bulk-generation runbook: schema enum violations (e.g.
`status:'contested'`), sibling reading_relations drift (OQ-58-class dangling edges), JSON
parse failures — each driver retries 3×. `failures.json` and `rejections.json` are
OVERWRITTEN per run: after EVERY one of the 8 runs they are copied into this dir as
`failures_<arm>_<leg>.json` / `rejections_<arm>_<leg>.json` before the next run starts.

## Execution order

Phase 1 Arm B (4 legs) → Phase 2 contract fix (prompt :71 ε-Invariance-adjacent paragraph +
:197 metrics-row qualifier + schema `extractiveness` description + legacy prompt.md
consistency edit; single commit) → Phase 3 Arm A (4 legs + stamp witness) → Phase 4
measurement (positive control first: the measurement script must reproduce the
`items_baseline.json` baseline spreads exactly before any arm is read) → Phase 5 writeup,
2026-07-27 postscript, KNOWN_STATE, ISSUES close-out, gate.
