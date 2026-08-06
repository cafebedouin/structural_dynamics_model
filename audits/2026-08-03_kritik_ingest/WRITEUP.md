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

**Verdict qualifier (2026-08-03 review; the label stands, its reading changes).** The
(f) classification is retained — the pre-registered rule was applied as written and
post-hoc reclassification would defeat the registration. But the recall denominator
conflated two altitudes (see Amendment 4 → the position-vs-machinery finding): every
strict miss in the scoring tables is a Link/Impact machinery section that SCOPE preserved
at `expected_structural_delta` altitude rather than reading altitude — a category error
in the predicted list, not a recovery failure. Partial recovery **at the position
altitude with machinery preserved one level down** is nearer (b) than the bare label
suggests; recall-as-scored measured the predicted list's altitude mixing, not SCOPE's
coverage. Read the headline with this qualifier attached.

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

## Post-review corrections and amendments (2026-08-03, same-day operator review)

### CORRECTION 1 — the confound cuts one way; the two halves of the verdict are unequal

The blanket "all claims bind to ruling (A)" was over-uniform. Emphasis-blindness inflates
card-body token volume ~10× relative to the tag layer, so it works AGAINST tag recovery
and FOR read-through. Split accordingly:

- **Scaffold finding (subjects/stances track block headings) — evidence a fortiori.**
  Precision 4/6 and 5–6/6 happened DESPITE the editorial layer being outweighed by an
  order of magnitude. The emphasis-aware re-run should RAISE tag idiom, not overturn this
  half.
- **Read-through finding (2 no-block-counterpart readings per file) — confound-predicted.**
  Pure read-through is exactly what mechanical volume dominance produces. This is the
  half the OQ-259 discriminator can actually kill.

Directional prediction registered on OQ-259: emphasis-aware re-run raises tag-idiom
share; the falsifiable half is whether the read-through readings SURVIVE emphasis-aware
ingestion (survive ⇒ format property; vanish ⇒ conversion artifact).

### CORRECTION 2 — AT Fiat K is single-STANCE, not single-voice; the tripwire observation was mis-premised

The plan (and PROPOSAL/SCORING, which inherit it) called AT Fiat K "single-voice." Wrong:
the file is six attributed published authors (Galea; Badiou/McGee; Bagg; Mauri et al.;
McGee & Romanelli; social-movement case studies) defending ONE stance — multi-voice,
single-stance. The KNOWN_STATE 2026-06-08 under-routing tripwire concerns single-VOICE
inputs, so it was never applicable here; no-flat-route is EXPECTED behavior, not an
anomaly, and no "tripwire silently regressed" reading is licensed. The earlier framing
("tripwire did not fire — one ungrounded observation") is retracted as N/A rather than
weak evidence. Consequence: (c)'s exclusion of this file from arsenal weight was
motivated partly by a wrong premise — though the exclusion itself still stands on the
independent ground that the design contains no discriminator between its outcomes.

### Amendment 3 — n_constraints +8 arithmetic, witnessed

217→225 = +7 stories + `fiat_efficacy_kernel_contradictions` — and the latter is not
merely an untracked file: it IS a `per_constraint` entry (witnessed: 225 entries in
`pipeline_output.json`, including `fiat_efficacy_kernel_contradictions`; 21
`*_contradictions`-named entries corpus-wide). Corpus enumeration is by filename
(`corpus_constraint/1`), so linkage emissions load and count. The plan's "grew by the
generated count" criterion reads correctly as +7 stories +1 linkage entry; the 217
baseline already contained 20 such entries, so the convention is stable, not new.

### Amendment 4 — PRINCIPAL FINDING: reading granularity sits at the theoretical position (from the existing scoring table, INFERRED, no new runs)

Strict misses: Cap — Neg Link/Perm, Neg Impact, Neg Alt, Aff Impact---Other; Biopower —
Neg Framework (deferred), Neg Link, Neg Impact. The advocate-vs-pre-empt hypothesis
(SCOPE recovers what a file advocates, drops what it pre-empts) is NOT supported: the
pre-empting Aff answer blocks surfaced WELL (Biopower: 4 of 6 selected readings are
Aff-side answers; Cap: 2 of 5 kernel readings). The pattern is
**position-vs-machinery**: block sections that STAKE a theoretical position (1NC thesis,
Alt-as-advocacy, Aff answer-positions) surface as readings regardless of side; sections
that elaborate one position's argumentative machinery (Link, Impact — 4 of the 7 strict
misses, symmetric across both files) are absorbed into that reading's
`expected_structural_delta` rather than surfacing as readings. SCOPE's reading
granularity is the theoretical position, not the argumentative function — the machinery
content is present at delta altitude, not lost. (Framework splits: selected axis in Cap,
deferred in Biopower.)

### Amendment 5 — `omega_debate_genre_distortion` is the quotable result

The Biopower manifest flagged its own ingestion genre unprompted: source arguments
"selected for strategic/competitive utility rather than truth-seeking," readings "may be
strategically exaggerated or strawmanned versions of the underlying theory," non-debate
consumers told to verify against primary texts. If genuine detection rather than a
one-off, this is the strongest single result of the run — an ingestion pipeline
annotating the epistemic genre of its input. Routed to the essay layer via OQ-259 item 3
(quote it there); a second-specimen check (does an independent arsenal ingest produce a
comparable genre flag?) is the cheap verification before it is quoted as detection.

### Amendment 6 (2026-08-06) — OQ-264 qualifier on Amendment 4: single-draw error bar, now measured

Amendment 4's position-vs-machinery granularity claim was inferred from ONE draw per
file. OQ-264 (`audits/2026-08-06_oq264_kredraw_variance/`, resolving the churn floor
OQ-259's Arm-0 surfaced) measured what that costs: same-input redraw stability of
per-reading identity is file-structure-dependent — 2/6–3/6 (Cap K NW), 4/6–5/6
(Biopower NW), 6/6 ×3 (AT Fiat) — and manifest unit populations churn even where
identity holds. Amendment 4 therefore stands as a DRAW-LEVEL observation, not a stable
property of either file's ingestion: which sections surface as readings vs absorb at
delta altitude is itself inside the witnessed churn for arsenal files. Under the minted
standard (OQ-264, k=3-unanimous presence), the granularity pattern would need to hold
across 3 same-input redraws per file before it is citable as a property; the existing
Arm-0 pairs already show reading-set membership shifting between draws. The (f)
cross-file verdict is unaffected (cross-file agreement survives; per-reading and
per-section citations from single draws do not).
