# Leg diagnostic table — the corpus-level `diagnostic` block across 19 legs

**Executed:** 2026-08-23
**OQ:** recon for the corpus-level program proposed at the leg expansion (OQ-342 umbrella; feeds
OQ-347 step 3–4, OQ-236, OQ-61/OQ-239, OQ-155/OQ-175); no OQ of its own yet.
**Verdict (scoped to the `diagnostic` block that `classify_corpus` already emits — no report-stage
tool was run):** the block's type shares, purity coverage, drift-event rates and network
drifting/severe shares carry a **model fingerprint that reproduces across every same-model pure
redraw pair** (between-model spread 8–38× the within-pair spread), while **`corpus_wasserstein_
fracture` and `arakelov_threshold` are draw-dominated** (ratio < 3; Wasserstein median moves 1.9×
between sonnet2 and sonnet3 and 2.5× between haiku2 and haiku3 with the apparatus shown
deterministic byte-for-byte). Three block members carry no information: `boltzmann_summary` is a
deterministic coarsening of `coupling_summary`, `network_stability` reads `cascading` on 19/19
legs by an absolute threshold of 3, and `contextuality.by_type` is constant for mountain (1.0)
and scaffold (0.0) on every leg.
**Manifest cite:** 19 outputs, `outputs/pipeline_output.<leg>.json` (18) + `outputs/pipeline_
output.json` (`testsets`, `885151b`, the only one predating the OQ-306 `corpus_loader.pl` change);
per-leg `code_commit_short` / `pipeline_run_at` / `n_stories` in `leg_diagnostic_table.json`
→ `legs[]`. No engine `.pl` changed across the 18 per-leg stamps (`git diff --name-only f0ef08a
HEAD -- 'prolog/*.pl'` minus `testsets*`/`tests/` is empty, witnessed in the recon turn).
**Fired:** live — a standing claim moved (OQ-236's "flash under-authors `coordination_type`" is
now a kimi finding: kimi2 353/1005 files lack it vs flash3 92/958), and two block statistics that
a cross-model read would have cited as corpus structure are shown to be draw noise.

**Evidence map**
- `audit_log.md` — HEAD stamp pair (open `3459cc53`, close `0d6d9a11`; the three intervening
  commits are another instance registering the four Flash legs — `prolog/schema_shape.txt`,
  `python/corpus_census_baseline.json` — outside this audit's read-set).
- `leg_diagnostic_table.tsv` — 19 legs × 56 statistics (the flattened block; shares over the
  block's own denominators, rates per story).
- `leg_diagnostic_pairs.tsv` — per statistic: between-model spread (one representative leg per
  model×sampling regime, max−min) vs within-pair |Δ| on the 5 pure pairs, sorted by ratio.
- `leg_diagnostic_table.json` — everything above plus each leg's `story_provenance` summary
  (model / prompt commit / sampling string / source), which is what classed the pairs.
- `sonnet2_rerun_witness.txt`, `sonnet2_rerun.log` — the draw-vs-apparatus discriminator.
- Instrument: `python/audits/leg_diagnostic_table.py` (reads JSON only; no Prolog).

## What was done

1. Flattened every per-leg `diagnostic` block (`json_report.pl:1795–1890`) into 56 scalars.
2. Classed every same-model leg pair from `story_provenance` (model, prompt commit, sampling
   string on every story), not from directory names or recall. Result: 5 **pure** pairs
   (flash2/3, flash_think/2, haiku2/3, sonnet2/3, stealth2/3); 16 confounded by prompt and/or
   sampling. This reproduces OQ-347's table independently.
3. For each numeric statistic: between-model spread vs within-pure-pair |Δ|. Falsifier (from the
   recon proposal B): a statistic whose within-pair spread approaches its between-model spread
   measures draw noise, not corpus content.
4. The one statistic that failed the falsifier on a pure pair while every sibling passed
   (Wasserstein on sonnet2/3) got its discriminator: `classify_corpus('testsets_sonnet2', …)` at
   HEAD, compared per story to the 2026-08-22 output.

## Findings

**F1. The model fingerprint is real and reproduces at corpus level.** Ratio (between-model /
within-pure-max) ≥ 8 for: `drift_events_per_story.warning` 38, `purity.coverage` 29,
`network.drifting_share` 20, `coupling.strongly_coupled` 16, `network.severe_share` 14,
`monotonicity.non_monotone` 12, `type.mountain` 12, `type.tangled_rope` 11, `type.snare` 9,
every purity band 9–10. Per-leg values (table): tangled_rope share kimi 0.749/0.741, stealth
0.731/0.731/0.745, sonnet 0.689/0.695/0.691, haiku 0.614/0.635/0.617, nemotron 0.567/0.575, flash
thinking-off 0.428/0.422/0.422 vs thinking-on 0.480/0.508. These are k=2–3 per model now.

**F2. Thinking-on shifts replicate at corpus level on Flash (two pure pairs each side).**
tangled_rope 0.42→0.50, snare 0.19→0.15, critical drift events/story 1.45→1.8, contextuality
0.52→0.67, `monotonicity.incomparable` 0.74→0.77, purity coverage 0.90→0.93. Nemotron (partial
think leg, 732) moves the same direction on drift (1.35→1.59) and contextuality (0.28→0.40) but
not on tangled_rope. Corpus-level corroboration of OQ-343/OQ-349, not new evidence on mechanism.

**F3. Wasserstein fracture and Arakelov threshold are draw-dominated.** `wasserstein.fracture_
per_story` between-model spread 1.35 vs within-pure max 0.52 (ratio 2.6); `arakelov.threshold`
0.25 vs 0.089 (2.8). Per-story medians on pure pairs: sonnet2 1.124 vs sonnet3 0.602; haiku2 0.194
vs haiku3 0.492; flash2/3 0.979/1.014, stealth2/3 0.542/0.576, flash_think/2 0.815/0.766. Not a
tail effect (top-5 stories are 1.8% of the total on both sonnet legs; per-story ratio median
1.36, p10 0.07, p90 3.8 — the whole distribution moves). **Apparatus ruled out:** the re-classify
of sonnet2 at HEAD is byte-identical on all 1003 stories for Wasserstein, MaxEnt entropy/top
type, Arakelov, purity, h1, signature, sheaf status, and the whole diagnostic block
(`sonnet2_rerun_witness.txt`). The mechanism is the one CLAUDE.md names as *ensemble refit*:
`wasserstein_edge_transport` (`measurement_layer.pl`) is a pure function of
`maxent_classifier:maxent_distribution/3`, which is corpus-fitted; two same-model redraws fit
differently enough to move the corpus-level transport ~2× while the per-story top-type agreement
sits at 0.86. **Consequence:** neither statistic may be cited cross-model without the pair floor
beside it, and any pre-2026-08-22 cross-leg Wasserstein/Arakelov claim was k=1 on a statistic
whose draw floor is the size of its signal. Feeds OQ-155/OQ-175 (MaxEnt fit stability) directly.

**F4. `boltzmann_summary` is a deterministic coarsening of `coupling_summary`.** On all 19 legs
`compliant == independent`, `inconclusive == inconclusive`, `non_compliant == strongly_coupled +
weakly_coupled + nonsensically_coupled`, exactly. By construction: `boltzmann_compliant/2`
(`boltzmann_compliance.pl:94–103`) tests `CouplingScore =< Threshold`, which is
`categorize_coupling` clause 1 (`logical_fingerprint.pl`), and both read the same
`fingerprint_coupling/2` term (`json_report.pl:2281–2299`). Two names for one partition in the
published block — a consistency check that cannot fail (`build_discipline.md`). Not a value
defect; a reader comparing them learns nothing. OQ-61's "type-composition restatement" shape.

**F5. `network_stability` is saturated: `cascading` on 19/19 legs, 258 ≤ n ≤ 1005.** The rule
(`network_dynamics.pl` `network_stability_assessment/2`) is `NumSevere >= 3`
(`network_cascade_count_threshold`), an absolute count against a quantity that scales with n;
every leg has 205–618 severe. The informative carrier is already in the block —
`network.severe_share` ranges 0.21 (testsets) … 0.62 (stealth), ratio 14 — so this is the
quantitative confirmation of OQ-61's saturated-flag note and OQ-239's per-component future, with
the size-normalized form sitting next to it unused by the categorical.

**F6. `contextuality.by_type` is structurally fixed; `corpus_fraction` mostly restates the type
mix.** mountain 1.0 and scaffold 0.0 on all 19 legs; snare 0.985–1.0, tangled_rope 0.90–1.0,
rope 0–0.045, piton 0–0.17. `corpus_fraction` ≈ share(tangled_rope+snare+mountain) within ±0.05
on 15/19 legs; the sonnet (−0.10…−0.12) and nemotron (−0.115) residuals are their larger
undetermined share (determined-only denominator, `grothendieck_cohomology.pl:431`). The by-type
cells carry no corpus information; the corpus fraction carries the type mix plus the
undetermined share.

**F7. Purity coverage: the hole is kimi, not flash.** `purity.n_no_data` kimi 276, kimi2 333
(coverage 0.70/0.64) vs flash 89–98 (0.90), sonnet 70–91 (0.91–0.93), haiku 0–5, stealth 2–7,
nemotron 1–2. On disk: `coordination_type(` present in 652/1005 kimi2 files vs 866/958 flash3,
997/1005 stealth3, 992/993 haiku3. OQ-236 was minted on the June flash leg (292/960); post-backfill
flash is at the sonnet level and kimi is the under-author. Both kimi legs agree (a disposition,
k=2), so OQ-236's stratification discipline now has a 35%-exclusion leg to test on.

**F8. Four constants, for the record:** `type.unknown` 0 on every leg; `network.cascade_threshold`
3 (config); `purity.degraded` ≤ 1.1%; `coupling.nonsensically_coupled` ≤ 0.7% — the last two are
below the draw floor (ratio < 2) and should not be read cross-model.

## What this licenses and what it does not

- Licenses: citing F1's statistics cross-model **with the pair floor from `leg_diagnostic_
  pairs.tsv` beside the claim**; treating F3's two statistics as draw-bound until OQ-155/175
  settle the fit; proposing OQ-61/239's per-component carrier as `network.severe_share`.
- Does not license: any statement about the report-stage tools (orbits, FPN, giant comp, HAC,
  covering, fingerprint) — they were not run; the recon's proposal A (a per-leg report driver)
  is still the precondition for that, gated behind OQ-301.
- `testsets` (canonical, n=258, `885151b`) is a different population (topical, singleton
  readings) and a different engine stamp; it is in the table for completeness and in no pair.
- The between-model spread uses one representative leg per model×regime (first by name); a
  different representative changes the spread by at most the within-pair Δ, which is the
  quantity the ratio is already reporting.
