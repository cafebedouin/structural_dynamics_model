# OQ-45 + OQ-52 — the presents-as-natural / hidden-winner pair

**Date:** 2026-07-01. **Branch:** `oq45-oq52-hidden-winners`. **Question pair:**
OQ-45 (do any of the 404 NL certifications hide asymmetric winners?) and OQ-52 (do the
naturalized→snare manifest rows have an authored beneficiary?) — related as a partition of the
presents-as-natural space: OQ-52 is the beneficiary-AUTHORED side, OQ-45 the beneficiary-SILENT
side (0/404 carry any beneficiary fact, by FSM-cascade construction).

**Substrate pins:** kernel_v1 output `pipeline_run_at=2026-07-02T01:35:19Z, n=1106, commit
e8189d1`; original_v6 output `pipeline_run_at=2026-07-02T01:44:17Z, n=3380, commit 3b169bb`
(both via `classify_corpus`, own manifests, canonical `pipeline_output.json` untouched).

## Part A — OQ-52 (resolved)

The deferred W1-magnitude leg was delivered: `w1_sheaf_join.py` gained the
`wasserstein_incomparable_mass` per-row join and the PROVISIONAL ≥0.05 materiality label
(commit `e8189d10`; behavior-preserving on existing columns, witnessed by column-level diff);
kernel_v1 was classified same-run and the false-mountain selection ranked
(`a3_false_mountain_w1_ranking.{md,json}`).

**Control outcome (the finding):** all 5 recorded 2026-06-02 member names recover, with
member-level H1 EXACT (quran=4, article_9=5, abrahamic=6) — but the population count does NOT
reproduce: HEAD yields strict=235 + loose=58 of manifest=944 (vs 16 of 98 at n=772 on the
2026-06-02 engine). The 944 cross-witnesses the OQ-197 acceptance controls (commit `34ff919f`),
so this is the known current-engine regime — four weeks of engine drift, not corpus growth. The
original 16-member list was never saved to substrate and is not reconstructible. Lesson (already
in the OQ-52 close): population counts over engine-computed selections are engine-regime-relative;
member-level type assignments were the stable part.

**Core-claim re-measure at HEAD:** 289/293 selected rows carry BOTH authored beneficiary and
victim; 4 are victim-only (first partial-disqualifier cases; OQ-86 repair sentinels screened —
0/1106 rows carry any sentinel atom); 0 carry neither. The corrective-grade reading (authored
extraction visible only at the analytical seat) holds.

## Part B — OQ-45 (resolved: YES)

**B1 — population re-derivation.** The NL signature is unsatisfiable on HEAD
(`has_viable_alternatives` range {true, unknown} since `8b5a34b8`; the gate needs `false`), so
the probe (`b1_nl404_probe.pl`) swapped the pre-fix fallback back in (§3 abolish+assertz recipe)
with PRE (unknown) and MID (false) dispatch controls, and swept the cascade Sig-UNBOUND over
original_v6. Result: 3380 swept, **natural_law = 404 — aggregate control PASS** (matches
KNOWN_STATE 2026-05-31 / the 2026-06-10 liveness matrix). `intent_viable_alternative` = 0
corpus-wide (the OQ-43 empty channel, confirmed). Member list now on substrate:
`b1_nl404_members.txt`.

**B2 — engine screen (exhaustive, 404/404).** Same screen function run on both corpora;
positive control on kernel_v1 flags all 3 OQ-52 anchors strict and reproduces A3's 235. On the
404: **0 false-mountain-shaped, 0 authored beneficiaries, 0 victims** (member-level control on
B1 — the beneficiary-blind-residue claim holds member-by-member). Uniform HEAD reading across
all 404: `signature=ambiguous`, `sheaf_status=manifest_presheaf`, `h1_band=4` — a
template-uniformity artifact consistent with the OQ-70 one-prompt-regime finding (84.3% of
accessibility_collapse values are exactly 0.92). Screen blindness (declared upfront): it reads
the same authored metrics that certified NL; that is why B3 exists.

**B3 — content read (rubric-controlled, adversarially stratified).**
- **Rubric v1 pre-flight FAILED 0/3** on the OQ-52 anchors: their prose *contests* naturalness
  in the narrator voice while depicting it as an in-frame appearance. The control did its job —
  v1 could not fire on a known case, so a 0-flagged run would have been unfalsified.
- **Rubric v2** (naturalness as a live in-frame reading counts, even if another voice unmasks
  it): pre-flight **3/3 hidden-winner**, quotes prose-anchored. Registration + amendment history:
  `b3_rubric_and_strata_registration.md` (v1 kept visible).
- **Strata (frozen before selection ran):** flagged=0; adversarial 25 (H1-rank collapsed to its
  registered W1 tie-break — all 404 share h1=4 — plus emergence-vocab × named-party grep);
  random calibration 10 (seed 45). 35/35 read by 7 agents; verdicts hypotheses until
  spot-verified.

**Result: 6 hidden-winner / 4 ambiguous / 25 genuine-natural.** All six hidden-winner quote
pairs spot-verified verbatim against the raw files (8/8 quotes; one spans a comment line-wrap).

**The six, with an adjudicated split (the split is a declared seat, not a computed fact):**

*(i) Extraction wearing the mountain frame — the certification is content-contradicted:*
- `repeat_player_structural_advantage` — "16.6% award decline per pairing represents real
  extraction — workers receive systematically lower awards" beside "classified as a mountain
  because it emerges naturally… No policy created it."
- `demographic_elimination_imperative` — displacement framed as "not a policy choice but a
  logical entailment" while "the displacement serves the project's foundational objective."
- `attention_as_capturable_resource` — "Platforms extract value by exploiting the constraint";
  the prose itself names the fork ("either a genuine natural law or a successfully naturalized
  extraction mechanism"). **From the random calibration stratum.**
- `capability_compulsion_gradient` (borderline) — "organizations can free-ride on the gradient,"
  hedged as second-order.

*(ii) Genuine natural core with an unauthored winner around it:*
- `gilgamesh_mortality_limit` — mortality is real; "Religious institutions benefit from their
  role as mediators… gaining authority and resources."
- `ecological_carrying_capacity` — biophysical limit real; "Primary beneficiaries of resource
  extraction systems… wealthy actors externalize environmental costs onto poor ones" (prose
  calls it an institutional overlay).

Bucket (ii) is itself a finding about the NL gate's beneficiary criterion: a party benefiting
from a constraint's *persistence* does not entail the constraint is authored/extractive
(undertakers benefit from mortality). If the NL signature is ever re-powered (GAP-08 §7), the
beneficiary leg needs the gain-around vs gain-from-authoring distinction, else genuinely natural
constraints with named ecosystem winners fail certification while bucket-(i) stories that
merely omit the winner from prose pass.

**Verdict:** OQ-45 answers **YES** — mis-authored hidden winners exist among the 404 (bucket (i)
is the direct answer; witnessed per-story with verbatim quotes). Coverage, stated exactly:
exhaustive over the engine screen (404/404); content read 35/404 under an adversarial-primary
pre-registered design with a rubric proven to fire on known positives. NO prevalence claim is
made or licensed (chimera-era corpus: bait-confounded OQ-70, ID-reuse OQ-25; per-story reads
valid, rates not). One calibration-stratum hit suggests the phenomenon is not confined to the
adversarial selection — noted as a pointer, not a rate.

**Consequences:** none for the live engine — the NL signature is dark on HEAD (dead-by-range),
the B1 fail-close + FSM cascade already route beneficiary-bearing mountains away from NL, and
the live corpus is post-de-leak. The findings are (1) the archival content answer, (2) the
bucket-(ii) design note for any future NL re-powering, (3) the OQ-52 regime-relativity lesson.

**Non-exhaustiveness (owed line):** OQ-45 ∪ OQ-52 do NOT exhaust the presents-as-natural space.
A hidden-winner that is neither false-mountain-shaped (OQ-52's screen misses it) nor NL-certified
(OQ-45's population misses it) falls through both audits — e.g. a story naturalized at every seat
(no snare at analytical) that never met the NL metric gates. The partition is of the two OQs'
scopes, not a coverage proof over the space.

## B5 — Cross-leg check (2026-07-02, operator-requested): testsets_* + kernel_v1

The same instruments run over the remaining legs (probe: `b5_nl_probe_generic.pl`, all PRE/MID
dispatch controls PASS; twins classified via `classify_corpus` with the single-model fingerprint
gate, haiku `2026-07-02T06:06:27Z` / flash `2026-07-02T06:08:21Z`, both n=960).

**NL populations (pre-fix cascade):** original_v6=404, **kernel_v1=26 (matches the recorded
2026-06-10 matrix — aggregate control PASS)**, haiku=8, flash=5, live testsets=0 (n=119).
The twin+kernel sets were content-read EXHAUSTIVELY (39/39, rubric v2):

- **kernel_v1 (26): 4 hidden-winner** — `honor_satisfaction_mechanism__contraction_reading`
  (aristocrats "used it to resolve disputes and maintain status hierarchies" under "a genuine
  natural law of cultural cognition"; gainer historical/extinct), `press_reformation_causation__
  technological_determinism` (printers/reformers profit, Church the named victim, under
  technology-as-Mountain), `state_killing_authority__abolition_reading` (abolition states
  "capture moral authority" under first-principles framing), `tsunami_stone_commitment__
  catastrophe_validation_axis` (resources "do concentrate" into agencies/contractors/insurers) —
  6 ambiguous, 16 genuine-natural. All quote-pairs verified (5/5). Every hit is a SOCIAL
  constraint in the NL set; the formal/physical members read clean.
- **haiku (8): 0 hidden-winner** after adjudication (2 reader calls downgraded to ambiguous —
  gain attribution lived in sibling readings the file explicitly excludes; same-kernel flash
  draw agreed ambiguous), 6 ambiguous, 2 genuine-natural.
- **flash (5): 1 hidden-winner** — `temple_sacrifice_commitment__performance_only` ("Messianic
  restorationists are beneficiaries" declarative, beside a Mountain claim; verified) — 2
  ambiguous, 2 genuine-natural. **A live-leg instance:** the phenomenon is not archival-only.

**OQ-52 cross-leg replication:** the false-mountain selection's authored-channel finding holds
at 100% on every live leg — haiku 113/113 both channels, flash 83/83, live 8/8 (kernel_v1's
4 victim-only rows remain the only exceptions anywhere). Screen control re-fired per leg.

**Two artifact notes:** (1) the all-404-h1=4 uniformity is an original_v6 template artifact —
twins show h1∈{0,4} mixes; (2) 2 NL members per twin carry authored VICTIMS with no
beneficiary — the NL gate checks beneficiaries only, so victim-bearing stories can certify;
another coarseness datum for any future NL re-powering, same family as bucket (ii).

**Draw-variance instance (OQ-26 working as designed):** the `article_27_veto_power__
sovereignty_reading` kernel reads ambiguous on both twins but with different in-file
argumentative structure; `aneyoshi` flips genuine(flash)↔ambiguous(haiku). Same-name cross-leg
members are distinct draws; the per-leg verdicts are seat-indexed measurements, not
re-measurements.

## Artifacts

- A3: `a3_false_mountain_selection_kernel_v1.json`, `a3_false_mountain_w1_ranking.{md,json}`
- B1: `b1_nl404_probe.pl`, `b1_nl404_members.txt`, `b1_probe_witness_lines.txt`,
  `b1_probe_run.log.gz`
- B2: `b2_screen_404.json`, `b2_flagged_members.json`
- B3: `b3_rubric_and_strata_registration.md` (v1+v2), `b3_strata_selection.json`,
  `b3_verdicts_consolidated.json` (per-story quotes in the batch agent returns; six
  hidden-winner quote pairs re-verified against raw files, 8/8)
- B5 (cross-leg): `b5_nl_probe_generic.pl`, `b5_nl_members_{kernel_v1,haiku,flash,testsets}.txt`,
  `b5_probe_*.log`, `b5_screen_kernel_v1.json`, `b5_screen_twins.json`,
  `b5_cross_leg_consolidated.json`
