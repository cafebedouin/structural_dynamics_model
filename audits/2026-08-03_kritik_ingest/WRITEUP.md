# Kritik ingestion probe — WRITEUP

Date: 2026-08-03. Plan: `~/.claude/plans/temporal-soaring-cherny.md`. Pre-registration:
`PROPOSAL.md` (committed `1bd57a84` BEFORE any run). Scoring: `SCORING.md` (committed
`d7c49cdd` before the Phase-3 go). Phase-3 log: `atfiat_fullrun.log`.

## Question and verdict

**Pre-registered question:** does SCOPE kernel-detection recover coherent structure from
an arsenal-format document? **Verdict: (f) Partial recovery, replicate-stable** — per the
pre-registered rule, applied in `SCORING.md`:

- Both replicates coherent and control-comparable (control: fresh emotives dry-run, same
  flags). Cap K NW: precision 4/6, recall 5/10. Biopower NW: precision 5–6/6, recall
  4–5/9. Both pass (b)'s numeric thresholds; both FAIL (b)'s majority-tag-idiom conjunct
  — idiom is mixed tag/card, with two pure read-through readings per replicate
  (world_system + growth_process; coalition_governmentality + insurance-deferred) that no
  block heading names.
- (e) replicate divergence did NOT obtain: same outcome class, same failure pattern,
  under the strong design version (same camp + format; size-class residue recorded in
  PROPOSAL Deviation 2).
- (d) was not reachable (recall ≥ 1/3 in both) — and would in any case have been
  conditional under emphasis ruling (A).
- Structural summary the classes only partly capture: **SCOPE reads BOTH layers — the
  editorial block layer supplies subjects and stances; the card literature supplies the
  reading structure; the kernels it mints are definitional contests
  (`capitalism_referent`, `biopower_health_administration`), not the file's Neg/Aff
  organization.** Biopower's `omega_debate_genre_distortion` shows SCOPE recognized the
  arsenal form unprompted.

**Prescribed action taken:** ONE meta-layer file graduated (AT Fiat K, operator go at the
Phase-2 gate); the arsenal replicates did NOT graduate; K-file corpus not expanded further.

## (c) — AT Fiat K did NOT flat-route

Pre-registered (c) (flat-routing) did not occur: the single-voice answers-only file
produced a 6-reading contested kernel (`fiat_efficacy_kernel`), one reading per card's
authority type, with SCOPE itself flagging the one-sidedness ("all cards argue FOR…
none against") — a grounds-contest kernel (readings agree on the conclusion, contest the
grounds). Per pre-registration this carries NO weight on the arsenal question. As a
single-voice behavior check: the documented under-routing tripwire (KNOWN_STATE
2026-06-08) did not fire under the primed prompt here — one observation, UNGROUNDED
(uniform `--skip-search`; see PROPOSAL Deviation 3), not a resolution of the tripwire.

## Phase 3 verification (witnesses)

- Orchestrator summary: all steps success (research skipped by design); generate 507.6s,
  6,710→50,053 tok; total 546.3s (`atfiat_fullrun.log`).
- `run_pipeline` exit 0 AND `outputs/pipeline_output.json` REWRITTEN: md5 `f6d267a0…` →
  `91614b14…`, mtime 2026-08-03 11:31 (stale-diff tripwire discharged).
- Manifest: `n_constraints` 217 → **225** = +7 committed stories +1 uncommitted
  `fiat_efficacy_kernel_contradictions.pl` (linkage emission; leaving it untracked
  matches the standing convention — several prior `*_kernel_contradictions.pl` sit
  untracked).
- Reports: 7/7 exist in `outputs/constraint_reports/` (verified by listing).
- `_step_commit`: `69db90a1`, 14 files / 7 stories, witnessed via `git show --stat`.

## Source → cid map (OQ-230 workaround)

Source: `agent/analysis/originals/k_files/AT Fiat K - Michigan 2026 BCFP.md`
(committed `1bd57a84`); frozen manifest
`agent/decompose_manifests/flat/fiat_efficacy_kernel_2026_20260803_102258.manifest.json`
(run id `fiat_efficacy_kernel_2026_20260803_102258`); generated cids (commit `69db90a1`):

| cid | ε | claimed_type |
|---|---|---|
| empirical_precedent_reading | 0.42 | tangled_rope |
| scholarship_reading | 0.32 | tangled_rope |
| truth_procedure_reading | 0.28 | tangled_rope |
| predictive_synthesis_reading | 0.28 | rope |
| empathy_simulation_reading | 0.28 | rope |
| utopian_fiction_reading | 0.22 | rope |
| fiat_efficacy_kernel_flat_control | 0.38 | rope |

## OQ-258 ε-referent observation pass

Per authored ε, which referent does the story use (criticized arrangement vs advocated
alternative)? **Uniform across all 7 legs: ε is authored over the DEFENDED practice
itself** — the fiat/simulation institution the cards endorse (beneficiaries/victims are
debate-community-internal: debaters, coaches, instrumentalized movement communities) —
not over any contested external arrangement. No within-kernel referent split witnessed.
**Weak evidence by construction:** this is the meta-layer answers-only file — every
reading endorses the same arrangement, so referents (a) and (b) nearly coincide; the
specimen class where OQ-258 bites (a K that CONTESTS an arrangement) did not graduate.
Logged as evidence on OQ-258 with that caveat.

## Side-findings

1. **`_step_commit` manifest staging mislabels relative in-repo paths** — with
   `--manifest-file` given as a repo-relative path, `Path(lm).relative_to(REPO_ROOT)`
   (c-orchestrator.py:965) raises `ValueError` on the RELATIVE path and reports
   `not_staged: manifest outside repo (…)` for a manifest that is inside the repo;
   staging silently skips. Harmless this run (manifest already committed `47085548`).
   Fix is output-changing (alters future commit contents) and needs a run witness →
   documented, not fixed on sight; tracked in ISSUES (OQ-260).
2. **OQ-58 quarantine shape:** all 30 inter-reading edges (6×5 cross-product) landed in
   `prolog/cs_reading_relation_quarantine.json` with prefixed canonical targets
   (`fiat_efficacy_kernel__X`) while authored cids are bare (`X`). Same shape as the
   prior committed run (12 = 4×3 for `visual_evidentiary_authority`) — standing pattern,
   not new; noted, not treated as a defect of this run.
3. Stale `_persist_manifest` docstring path corrected (micro-fix flagged in the plan) —
   behavior-preserving, docstring only.
4. Cap K NW ingested whole-doc at **339,501 tok** under `--skip-search` — largest
   single-document SCOPE ingest witnessed in this repo; manifest-level output shows no
   degradation signature relative to the 103k Biopower run.

## Standing consequences and named follow-ups (→ OQ-259)

- Emphasis ruling (A) binds every claim here: results are properties of
  **emphasis-blind ingestion**, never of the card-file format. The named discriminator
  is the `w:highlight`/`w:sz` extractor + re-conversion + Phase-1 re-run.
- Uniform `--skip-search` consequence: AT Fiat K's manifest and stories are ungrounded;
  the plan's "manifest carries the dry-run's grounding" clause was STRUCK pre-run
  (PROPOSAL Deviation 3).
- Deferred deliverable (post-results, essay-layer only, NEVER pipeline input):
  `shanahan_kritik.md` mapping/convergence doc — must carry the review's corrections
  (ballot-as-forced-gluing as the one falsifiable mapping; common-descent and
  ballot-fitness limits; corrected perm↔Theorem-8 mapping with the
  severance/intrinsicness refinement offer).
