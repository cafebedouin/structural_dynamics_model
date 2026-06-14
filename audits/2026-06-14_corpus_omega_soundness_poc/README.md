# Corpus omega soundness POC (OQ-130 scale arm) — adjudication

*Date: 2026-06-14. Plan: `~/.claude/plans/brief-the-omega-glittery-wozniak.md`. Two-party
independence protocol: adjudicator (sealed held-sample key, `adjudicator_held_scores.json`,
committed `acc27d22` BEFORE the executor ran) + blind executor (subagent, probes 1–4,
`probe{1,2,3,4}_*.json` + `probes_writeup.md`). Read-only over `prolog/testsets_haiku/*.pl`;
no shared `outputs/` written, no `run_pipeline`.*

## POC bar (operator framing)
Roughness expected. The deliverable is **what it teaches** (the three fold-backs, with witnesses),
NOT a defensible corpus-wide soundness verdict. Under-claim by construction: probes 1–2 are
corpus-wide and mechanical; probe 3 is a 30-omega sample (bounds, not proves); probe 4 is aspirational.

## Recon reproduced (adjudicator, independently)
960 stories · 4,430 `/3` omegas · 3,755 distinct names · 3,598 singletons · 2,740 Ω_E / 1,588 Ω_C /
102 Ω_P · resolution-text near-dup max prefix = 2. Executor reproduced all of these exactly.

## Probe 1 — three-axis alignment (headline): **CROSS-CUT, confirmed**
Reference = `cs_kernel_id` (authored, 331 kernels). Recovered surfaces scored vs the kernel partition:

| surface | axis | clusters | ARI vs kernel | same-kernel→same-cluster | verdict |
|---|---|---|---|---|---|
| 1a fingerprint-shift | KIND | 61 | **−0.0004** | **7.65%** (73/954) | CROSS-CUT (predicted) |
| 1b orbit+classification | KIND | 61 | −0.0004 | 7.65% | CROSS-CUT — *identical to 1a* |
| 1c lexical frontier (4,430 ω) | FRONTIER | 2,901 | −0.0010 | 2.05% | MISALIGNED → 3rd-axis evidence |

- **KIND ⊥ topic, MEASURED.** ARI ≈ 0; only 7.65% of same-kernel sibling pairs share a structural
  cluster (≈ chance). The engine's structural organs split same-kernel siblings and merge cross-kernel
  same-structure constraints. **They cannot be the frontier-identity organ.** This converts fold-back
  #1 from assertion to measured result. Independently corroborated by CLAUDE.md's 6/7-cross-topic
  signature witness, which predicts exactly KIND ≠ topic.
- **1b == 1a** (not OPEN — computed cleanly): `gauge_orbit` and `fingerprint-shift` encode the same
  KIND structure; the `dr_type` prefix added zero resolution. One KIND organ, not two.
- **frontier ⊥ topic, MEASURED.** ARI ≈ 0; the `suppression_*structural_vs_internalized*` frontier
  family spans **225 kernels** (adjudicator's narrow regex) to **264** (executor's broader one) —
  robustly *hundreds*, far above its ~85 top-name count. **Adjudicator independently recomputed: 333
  omegas / 333 stories / 225 kernels.** The magnitude is robust to regex breadth; the qualitative claim
  (frontier spans hundreds of kernels ⇒ orthogonal to topic) is solid. The lexical proxy is a LOWER
  BOUND (misses synonyms; embeddings are the deferred real organ).
- *No surprise outcome:* no KIND surface aligned with kernel (would have been the strong/weighted
  signal). 1c↔kernel alignment is reported but not headlined (both topic-ish; shared-default inflation).

## Probe 2 — content-templating: **LOW (falsifier passes)**
Combined question+approach prefix[60] max-dup = 1 (every omega unique); 8-gram overlap 226/257,887 =
0.09% (all the kernel-contest boilerplate opener). **Authored omegas are NOT §8-style content
artifacts.** Soundness holds on the content axis: the artifact is **identity-overstatement, not
fabrication.**

## Probe 3 — soundness spot-check: **24/30 = 80% sound**; held-sample blind agreement **9/10**
By type: empirical 13/15 (86.7%) · conceptual 9/12 (75.0%) · preference 2/3 (66.7%).

**Held sub-sample (10 omegas the adjudicator sealed-scored before the run):**

| id | adjudicator (sealed) | executor (blind) | adjudication |
|---|---|---|---|
| 0,2,3,8,11,13,16,28 | SOUND | SOUND | **agree** (8/8) |
| 24 | TYPE_INCORRECT (pref→emp) | TYPE_INCORRECT | **agree** |
| 20 | TYPE_INCORRECT (con→emp) | SOUND (conceptual) | **disagree — unsettled by definition (see below)** |

**The one disagreement (id 20, `messianic_timeline_indeterminacy`) is NOT settleable against the
external authority.** The question is a frame-choice (maintenance vs perpetual-deferral → conceptual
on its face), but the stated resolution approach is empirical-historical (trace community adjustment
over centuries; theater-ratio measurement → empirical resolution mode). `omega_variables.md` itself
lists **"Hybrid dependencies — do genuine hybrids exist?"** as an *open question* of the framework.
id 20 is exactly such an apparent hybrid; the definition does not resolve it. Per human-ruled-
adjudication discipline I do not break the tie by preference. **Recorded as: Ω-type inter-rater
disagreement localizes onto the hybrid-type gap the framework already admits** — itself a finding
(Ω-type scoring reliability is limited precisely at the hybrid boundary).

**The 3 UNSOUND (executor) independently verified by adjudicator against the omega text:**
- **id 14, 18** — the approach text *answers its own question* using authored ε / victim /
  classification deltas (the "structural difference" is computable from internal authored facts).
  **Fails Irreducibility.** UNSOUND **correct**.
- **id 25** — approach gestures at an empirical discriminator (historical-trajectory prediction) but
  the consequence retracts it ("unresolvable within this framework; generate all readings" =
  documentation, not an exit). UNSOUND **defensible** (salvageable if the latent empirical
  discriminator were promoted to the stated resolution).
- All three are the **kernel-contest family** (`kernel_reading_contest`/`contestation_space`/
  `committer_frame__*`).

**Extension of the adjudicator's prior (this is the sharp lesson).** The sealed prior predicted *no
vacuous omegas* (four-property soundness ~100% on the held sample, which held: 10/10 met the four
properties). The executor surfaced a vacuous **class** the held sample didn't contain — and it is the
**same kernel-contest family** that drives the frontier-axis collapse in probes 1/4. **The soundness
defect and the identity-overstatement defect coincide in one family.** A semantic-frontier dedup organ
that collapses the kernel-contest family would *simultaneously* (a) fix identity-overstatement and
(b) quarantine the unsound class — the dedup pass and the soundness gate are **not independent.**

## Probe 4 — agenda shape (aspirational)
4,430 omegas; name-keyed "unique" 3,755; **semantic-dedup lower bound = 2,901 distinct frontiers**
(6 families absorb 1,485: suppression 456, kernel_reading 409, reading_contest 256, identity_lock 210,
founding_problem 88, natural_law 66). Aspirational agenda: **~2,901 frontiers, 61.9% Ω_E / 35.8% Ω_C /
2.3% Ω_P** — measurement-dominated.

## Push decision (the plan's escalated standing question)
The plan said: hold the pilot push until the POC soundness result is known; if the corpus is
*substantially unsound* (§8-class), the pilot's purpose shifts (routing-to-fix, not cataloging).
**Result: 80% sound, the unsoundness concentrated and explainable (one family), NOT §8-class.** The
hold condition is **not triggered** → pilot + POC are clear to push together (git-autonomy ruling;
docs current per Verification below).

## Honest limitations
- Probe 3 is a 30-omega sample (held key 10). The 80% bounds, does not prove, corpus soundness.
- 1b==1a means only ONE independent KIND surface was actually tested (the second collapsed onto it).
- The frontier organ tested is a *lexical* lower bound; the orthogonality claim could only strengthen
  with embeddings, not weaken (synonyms merge more, not fewer).
- Two-party independence is real here (separate sealed key, blind subagent) — stronger than the
  ISSUES.md pilot's single-author §E — but the held overlap is 10 omegas; agreement 9/10 is a small-N
  witness, not a calibrated inter-rater statistic.

## Files
`adjudicator_held_scores.json` (sealed key), `sampler.py`, `probe1_alignment.json`,
`probe2_templating.json`, `probe3_executor_scores.json`, `probe4_agenda.json`, `probes_writeup.md`.
