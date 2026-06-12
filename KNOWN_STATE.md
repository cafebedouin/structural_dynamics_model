# Known State — Session Changelog

This is the dated session log split out of `CLAUDE.md` (2026-05-31) to cut the
auto-loaded instruction file's per-session token cost (~3,050 tokens / 45% of CLAUDE.md
were this section). **It is NOT auto-loaded** — read it on demand, and prefer the
query below to reading the whole file.

**Entry grammar (machine-readable, added 2026-06-04).** Every entry is:

```
## YYYY-MM-DD — <title>
**Files:** <comma-separated paths the entry concerns>
**Tier:** tripwire | correction-key | landed | history
```

Tiers: `tripwire` = standing do-not / silent-mistake warning; `correction-key` =
corrects prior claims or qualifies how results may be cited; `landed` = change/audit
shipped and witnessed; `history` = narrative/archival (roll-off candidate). Checker:
`python3 python/known_state_status.py --check` (run after editing this file; sibling of
`issues_status.py`).

**Before touching a file, query instead of reading everything:**
`python3 python/known_state_status.py --file <path>` lists the entries whose `Files:`
line names it — read those. (The old hand-maintained "read before touching" list is
superseded by the `Files:` lines; high-traffic files currently include
`signature_detection.pl`, `drl_composition.pl`, `json_report.pl`,
`generate_kernel_corpus.py`, `enhanced_report.py`.)

**Roll-off rule (monthly, with the CLAUDE.md "Memory Consolidation Review"):** entries
older than ~30 days get the CLAUDE.md promotion test once more, then are **compressed in
place** — keep the header + `Files:`/`Tier:` lines + a 2–4 line verdict + pointers
(commit hash, `audits/<date>_<slug>/`, OQ number); drop the body. Full text stays in
this file's git history; never create a separate archive file (Build Discipline
Pattern 2). `tripwire` entries are compressed only if their warning is promoted to an
always-loaded CLAUDE.md section or superseded.

**Standing warnings lifted into auto-loaded `CLAUDE.md` sections** (the tripwire lives there;
full provenance stays here):
- Green cut `product_site_export.pl:75–77` → `CLAUDE.md` Architecture Invariants.
- Run-tagged subdir glob isolation → `CLAUDE.md` Corpus Loading.
- Corpus is 223 not 3,337 / cite the manifest → `CLAUDE.md` Critical Distinctions.

Entries are roughly chronological. New session findings go here (see `CLAUDE.md`
End-of-Session Documentation Review), not in CLAUDE.md.

---

## 2026-06-11 — OQ-105 per-row sweep: PREDICTED bucket discharged — 4/23 misaligned rows timing-distorted, all one snare-floor mechanism; fork ruling still open
**Files:** ISSUES.md, audits/2026-06-11_oq105_row_sweep/
**Tier:** landed

Worktree `oq105-row-sweep` from `37ea069f`. Interpolation counterfactual over ALL 23
grid-misaligned suppression rows (62-file corpus): substituted scalar vs linear interpolation
of the constraint's own series through the same `classify_at_time_with_supp` clause path.
Controls: interp-identity 215/215 authored points exact; same-path re-derivation 0 failures;
enumeration census re-derives exactly the OQ-110 figures (23 rows / 11 constraints).
Default context: 3/23 rows diverge; all 156 product contexts: **4/23 rows** (181/3588 cells,
5.0%) — agenda_conditioning T=10, post_1998_convergence T=13,
technocratic_paradigm_vs_human_primacy T=9, truth_democracy_disinformation T=2 (non-default
contexts only). Every divergent cell is the one predicted mechanism: endpoint scalar ≥ snare
suppression floor (0.60), local series interpolates below → snare dated early
(sub=snare/interp=tangled_rope, no other type pair). 19/23 rows substitution-robust at every
context. Witness-bucket refinement: substantive_employment_reading T=9 (an original
flip-ON-substituted-row witness) is NOT timing-distorted — interpolated 0.62 also clears the
floor; flip-on-substituted-row was a weaker test than the interpolation counterfactual.
OQ-105 stays OPEN: the (a) grid-alignment-at-generation vs (b) labeled-interpolation-at-read
fork is the operator's; the sweep bounds (b)'s live-data payoff to exactly these 4 rows.
## 2026-06-12 — DETERMINISM-FRONTIER ruling promoted to CLAUDE.md; Phase C removal commit (schema perspectives[]/mandatrophy_resolved OUT, provenance/8 REQUIRED incl. model+sampling); archive-before-removal executed; replicate probe folded into cohort zero
**Files:** CLAUDE.md, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prolog/narrative_ontology.pl, prolog/guard_exclusions.pl, prolog/signature_detection.pl, prolog/stack.pl, agent/example_platform_commission.json, ISSUES.md, prolog/archives/datasets/pre_cohort_zero_2026-06-12/
**Tier:** landed

Operator ruling (via web-session analysis): "it's the LLM" is a hypothesis sitting where a
witness goes — three mechanisms produce same-material-different-results (generation
stochasticity / ensemble refit / fixed-input non-determinism), attributed by stage-hash
diff, never assumption. Record check WITNESSED all three in-repo: OQ-26 (ε
generated-not-invariant; Axiom 2 amended v6.13.1), press/Reformation 3-naming 9-file
triplet (kernel_v1), naming-drift siblings; the 57-story ensemble refit; OQ-112
order-dependency class + byte-identical same-code reruns (single commits only). Promoted
to CLAUDE.md Critical Distinctions: generation NEVER reproduces; committed JSON = the
checked determinism frontier; re-generated stories are NEW DRAWS never re-measurements.
Landed with it: GATE-0 exclusion (witnesses W1-W3, c_gate0_exclusion_witnesses.out);
archive-before-removal (pre_cohort_zero_2026-06-12: 62 pl + 60 json, schema-pinned at
046e0a40; ab_pilot_pair permanent per R4); schema removal + provenance/8 REQUIRED (model +
sampling_params per the ruling); compiler emits story_provenance/8, perspectives emission
retired delete-not-guard (reason left with the corpus); example carries honest
hand-authored provenance; witnesses W1-W5 (c_removal_commit_witnesses.out): example
PASS/compiles/lints clean, no-provenance fires, old-format invalid AS DESIGNED (archived
schema governs the archive). Replicate probe (3-5 stories x 3 draws, field-stability
table) folded into cohort zero — the table defines which fields n=1 meta-analysis may
compare. Remaining Phase C: regen driver + cohort zero (API spend), reading_diff
re-point, re-witness battery (C-arm + named pair + trio re-measure + corroborated_zombie
+ replicate probe), close-out.

## 2026-06-12 — OQ-114 RESOLVED: archive probe under frozen criterion → OUTCOME 3 (mixed) → operator ruled the live 3 SPLIT (2 in / trust_erosion out, kill conditions + fail-closed exclusion + named re-witness); rider: no-beneficiary conjunct WRONG
**Files:** ISSUES.md, audits/2026-06-12_oq114_archive_probe/
**Tier:** landed

Worktree oq114-archive-probe. Probe (criterion frozen at first commit c64f32a6): kernel_v1
41 mountain-claimed → both=32/Uonly=0/Conly=9/neither=0; v6 430 → 411/0/19/0; comparator
controls PASS both; include-semantics fix caught against the denominator before any
reading (archived duplicate facts multiplied bare findall). Structural finding: NL trio
filters NOTHING on archives — C ≡ claim-mountain there (live corpus authors the trio under
the stricter 2026-06-09 rule; archives cannot witness that). All 28 C-only inspected (≤25
per archive): instruments read all mountain-profiled (no snare-floor, ε≤0.18, low theater
except one deliberate piton); disagreements split duplicate-seat artifact (~6/9, ~8/19)
vs substantive distinct-seat dissent (thai_112 powerless-snare class) — BOTH shapes in
BOTH archives → outcome 3. Ruling: organization_floor + demographic_skill_mismatch IN
(first live C-arm decisions, named re-witness at Phase C); institutional_trust_erosion
OUT (substantive dissent × live FCR firing converging fail-open) with kill conditions
both directions and a FAIL-CLOSED per-story exclusion as the Phase C build item
(witnesses owed: excluded + two-sided control). Rider recorded in the entry: option 4's
no-beneficiary conjunct was WRONG, not over-restrictive (unanimous mountains declare
beneficiaries; the signal is FSM routing, not validity). Phase C now proceeds: extension
confirmed-as-amended → regen.

## 2026-06-12 — OQ-109 B4 gauntlet PASS against a pre-compiled expected-divergence manifest; Phase C ordering pinned (OQ-114 first); OQ-115 filed (check_stack divergence attributed pre-Phase-B)
**Files:** ISSUES.md, audits/2026-06-11_oq109_phase_b/B4_EXPECTED_DIVERGENCE_MANIFEST.md
**Tier:** landed

Manifest compiled BEFORE the run (operator: gauntlet = reconciliation against prediction,
not post-hoc explanation; unmanifested divergence blocks). Pipeline green; validation suite
EXCELLENT; plunit 14/14; check_stack = 4 baseline findings + 1 unmanifested →
investigated to attribution: abductive_helpers phantom-module under [stack]
(load-path-dependent, OQ-57 class; pipeline chain healthy via json_report →
diagnostic_summary; present at pre-Phase-B c22ec561, absent from the 2026-06-04 baseline
— OQ-98-era reference) → OQ-115, not Phase-B-attributable, does not block. Rows 1–10 all
reconciled (pipeline diff confined to the two A5 gaps nulls). corroborated_zombie
first-live-exercise flag carries into Phase C. **Phase B is COMPLETE.** Phase C ordering
pinned in the OQ-109 entry: OQ-114 ruling → C-arm extension confirmed → regen (no
dependency forces regen-first; archive probe rides corpus_path overlays).

## 2026-06-12 — OQ-109 B3: empty-table census CLOSED (A1–A6, B1–B3 all discharged); narrative_ontology A3/A4 detectors retired; linter migrated to agent-surface dispatch; gaps key carries coverage bit
**Files:** prolog/narrative_ontology.pl, python/linter.py, prolog/test_harness.pl, prolog/json_report.pl, prolog/report_generator.pl, python/shared/schemas.py, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

Census closure table in b3_empty_table_census.md. Retirements (zero-consumer grep
positive-controlled, dead exemption legs, successors named): check_indexical_relativity,
validate_indexical_completeness, detect_omega(mandatrophy), count_unresolved_omegas,
detect_mandatrophy_omega — products live in R5 crosscheck / FSM / T17 / linter
role-coverage; has_mandatrophy_declaration KEPT (exported, R5-grounded clause). Linter:
MISSING_AGENT_SURFACE dispatch mirrors data_validation; perspectival minimums + variance
legacy-gated; ROLE_COVERAGE minimal two-sided policy (uniform-claim exemption carried);
UNRESOLVED_MANDATROPHY satisfied by authored founding_problem_status; Rule 18b validates
stakeholder_d_override when present. Witnesses: B2 example lints 5→0; corpus sweep 92→80
fully decomposed (7 mandatrophy cleared via genealogy, 2 correctly retained, 2 no-data
stories consolidate to accurate MISSING_AGENT_SURFACE). A2 validate_per_index logs
[INDEX VACUOUS] + ran-count (two-sided witness). A5 gaps: null=didn't-look vs
[]=measured-empty; python/shared/schemas.py gaps made nullable (the enrich validator
caught the null LOUDLY first — the chain working); output diff confined to the 2 no-cell
stories. A6 PERSPECTIVAL_GAPS carries ran-witness (137 incl. engine demos — now visible,
was absorbed). Remaining B3: NONE — next is the B4 gauntlet, then Phase C
(gated on B4; C-arm live-service note + OQ-114 ruling govern the guard there).

## 2026-06-12 — OQ-109 B3: R5 zombie consumer LANDED (A7 seam recovered, first consumer of zombie_piton_crosscheck/2); CLAUDE.md mandatrophy note retired per its own condition; presence gates + emission seam landed same day
**Files:** prolog/report_generator.pl, prolog/data_validation.pl, python/generate_constraint_pl.py, CLAUDE.md, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

R5 consumer: Section-7 mandatrophy surface EXTENDED with r5_zombie_crosscheck_line/1
consuming stakeholder_seats:zombie_piton_crosscheck/2 (Phase-A primitive, zero consumers
until now). Pre-registered witness shape held exactly: 6 live firings (4
authored_zombie_uncorroborated + 2 computed_piton_unflagged), one additive line per firing
report inside the existing section, quiet control (scale_ceiling) clean, pipeline JSON
untouched. corroborated_zombie = 0 on the corpus — witnessed ONLY by the overlay control
(dead+world_rearranges onto computed-piton regulatory_measurement_gap); the live diff is
NOT evidence for that bucket. CLAUDE.md mandatrophy tripwire RETIRED (its stated condition
— the R5 rewire landing with witnesses — met); residual: mandatrophy_resolved is STILL a
dangling schema field, Phase C retires it alongside perspectives[] (provenance KNOWN_STATE
2026-06-07 / OQ-83 A7). Same-day earlier units: presence gates
(agent_surface_present/1 dispatch, 5 two-sided controls) + census-B1 emission seam closed
two-sided (compiler gates invariance_check on perspectives presence). Gotchas: report
Section 7 prints only the subject constraint's crosscheck line per report (subject-scoped
like the OQ-99 scenarios); data_validation NOT loaded by [stack].

## 2026-06-12 — SPEC CORRECTION: unanimity bridge disjunction → conditional dispatch; extension change fully reverted (byte-identical witness); OQ-114 exposure window recorded; ensemble-decomposition practice note banked
**Files:** prolog/signature_detection.pl, ISSUES.md, docs/technical/build_discipline.md, audits/2026-06-11_oq109_phase_b/
**Tier:** correction-key

The 790bb009 bridge landed as old ∨ C — but C ⊇ old, so the union IS C's extension: the
3-story protection, the FCR un-fire, and the regulatory_measurement_gap yellow→red were
LIVE on main for the same-day window, pre-answering OQ-114 (operator: spec
under-specification — "ordered so the authored path decides" meant dispatch, was written
disjunction; executable miss here — the 9/62 extension witness was in hand and not read as
"the deferral didn't defer"). Fix: conditional dispatch (authored cells present → old
semantics verbatim; else nl_certification_chain). Witnesses: dispatch extension = old 6
exactly; seam control still passes via C arm; pipeline diff vs PRE-BRIDGE baseline
BYTE-IDENTICAL (b3_unanimity_dispatch_diff.out). OQ-114 carries the exposure-window note
(trace any consumer of pipeline output in the window). Banked as infrastructure:
build_discipline.md → "Extension-touching diffs decompose into direct targets vs ensemble
refit" (3 signature changes refit 57 stories' corpus-relative statistics; determinism
control is the standard companion) — required reading before B4 gauntlet / Phase C regen
diffs.

## 2026-06-12 — OQ-109 B3 unanimity guard RULED+LANDED: option-2 bridge (authored-cells ∨ nl_certification_chain); census A1 seam closed; OQ-113/OQ-114 filed; output-changing (3 targets + ensemble cascade)
**Files:** prolog/signature_detection.pl, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

Both named criterion candidates FAILED the pinned gauntlet — computed-seat unanimity splits
mountain/rope on genuine NL profiles (metric path computes rope at mid-power seats on
ultra-low ε); natural_law_signature is unsatisfiable by construction (has_viable_alternatives
never returns false → pure_natural_law unreachable → OQ-113). Escalated; operator ruled
option-4-conditional → witness failed (C∧no-beneficiary retains 1/6) → option-2 bridge:
authored-cells arm first (DIES AT PHASE C, named retirement point in code comment) ∨
nl_certification_chain/1 (claim=mountain + emerges_naturally + NL collapse/resistance,
fail-closed, signature-layer-safe). Extension 9/62 = old 6 + 3 (OQ-114 adjudicates the 3;
all FSM-examinable). Output-changing commit landed alone: institutional_trust_erosion FCR
un-fired (→ coupling_invariant_rope, seats piton→rope); 57 non-target stories moved ONLY in
corpus-relative statistics (maxent/Wasserstein/Arakelov ensemble cascade; determinism
control: same-code rerun byte-identical); named non-target effects: 3 maxent_top_type
piton→rope + regulatory_measurement_gap headline verdict_join yellow→red. Gotchas worth
keeping: ε lives in domain_priors:base_extractiveness/2 (constraint_metric key is
`extractiveness` — a wrong-table bite-check read all-none before correction);
domain_priors:emerges_naturally is static+multifile (with_asserted cannot overlay it — use a
consulted scratch testset). Criterion-worked framing per operator: the pin rejected
everything offered; not grounds to loosen leg (1).

## 2026-06-11 — OQ-109 Phase B1+B2 LANDED: prompt cutover to stakeholder surface; new one-shot example (FNL statistics reset No. 2); schema/compiler perspectives-optionality (guard-not-delete)
**Files:** prompts/constraint_story_generation_prompt_json.md, agent/example_platform_commission.json, agent/story_generator_base.py, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

Worktree `oq109-phase-b`. B1: P/T/E/S tuple + Indexed Classifications sections dropped
(1008→872 lines); d-derivation + ε-invariance KEPT trimmed (operator-approved: substrate-
general surface for the OQ-110 derived-d ruling, not four-tuple surface); suppression-
ambiguity omega + cyclical-measurement guidance relocated, not lost; stakeholders +
six-questions+R5 promoted to required. Witness: 11 tuple-vocabulary terms 0 post / >0 pre
(b1_vocab_grep_witness.out). B2: example = app_store_commission pilot (minimum-prevalence
pick 2.483, example_prevalence.out), hand-mutated per EXAMPLE_INHERITED_SIGNATURES.md —
THE FNL-reset discount list; EXAMPLE_PATH repointed off verification_bottleneck.json;
prompt working-example pointer off testsets/antifragility.json (OQ-47 leak source).
**Boundary pin: B2 changed optionality ONLY** — `perspectives` left the schema required
list and the compiler tolerates absence (.get, 3 sites; emission loop intact — existing
corpus compiles byte-identical, witnessed); property, $defs/Perspective, and emission stay
until Phase C Pattern-3 diffs. Known pre-B3 state: linter fires 4 perspective-era rules +
UNRESOLVED_MANDATROPHY on the example (b2_example_validation.out) — must clear at the B3
linter migration. Pre-existing: 12/60 live-paired JSONs fail schema validation in BOTH
pre/post states (b2_schema_failset_diff.out; 2026-06-09 strictening predates them) —
cross-check against the Phase C regen list. Mountain-claimed perspectives-free stories
would emit invariance_check over an empty authored table — B3 seam, noted.

## 2026-06-12 — OQ-103 RESOLVED: contamination-edge provenance made load-bearing + count-based salience floor at the read site
**Files:** ISSUES.md, prolog/json_report.pl, python/enhanced_report.py, python/tests/test_contamination_provenance_salience.py
**Tier:** landed

Scope-corrected the OQ at close: the provenance bit was NOT absent — `constraint_neighbors/3`
tags each edge with `Source` → `json_report.pl` serializes `edge_type` → `enhanced_report.py`
already printed it. `edge_type == explicit` IS the story-authored-vs-corpus-derived bit. Defects
were (1) inert bit (no legend, equal interpretive weight) and (2) no salience floor.

Read-site fix (no engine classification change):
- `json_report.pl` `write_one_neighbor/6` now emits `shared_agent_count` per neighbor (distinct
  agents shared on the link type; null for explicit/inferred_coupling). It threads the subject `C`
  through `write_neighbor_array/4`→`write_neighbor_items/4`. `edge_strength = 0.3 × count`, so the
  count is the recalibration-proof salience input (don't back-derive from a literal 0.3).
- `enhanced_report.py` `build_contamination_network` gains Provenance/Salience columns + legend +
  `_edge_is_authored`/`_edge_is_salient` helpers; "primarily X" ranks salient edges only; explicit
  empty-above-floor sentence. Floor: authored always salient; derived agent edge salient iff
  count≥2; inferred_coupling (zero live coverage) falls back to strength≥0.6.

Witness: pipeline 2026-06-12T04:29:38Z n=62; 82/106 (77%) edges demote to low-salience; both filed
witnesses (reprogramming→digital_colonialism, trust→representation) render `corpus-derived|low`.
Unit test 5/5. Theorized dedup-mislabel checked on the one live overlap pair, NOT witnessed —
`edge_type` reliable. Back-propagation to existing essays declined (operator: fix-then-rebuild).
Synthesis enforcement stays OQ-101 (`tensions_ledger.py` can now consume the new fields). Commit
`ded4969d` (merge `1bb6e535`). No CLAUDE.md promotion: in-place OQ-103 comments + named helpers
are loud enough.

---

## 2026-06-11 — OQ-112 item-4 sentinel trace: verdict SILENT (three mechanisms); absorber-boundary class elevated to item 2; maxent_indexed_run order dependency found
**Files:** ISSUES.md, audits/2026-06-11_oq112_item4_sentinel_trace/, prolog/maxent_classifier.pl, prolog/json_report.pl
**Tier:** landed

Worktree `oq112-item4-trace` from `009c793a`. Driven-goal trace of the post-OQ-44 `unknown`
sentinel into maxent (read-only; probes + raw outputs in the audit dir). Confirmed: the
`; Supp = 0.0` branches (`maxent_classifier.pl:255/:761`) are dead; with profiles present both
LL paths throw `type_error(evaluable, unknown/0)` at `is/2` — loud in isolation — but (W8) the
only two absent-suppression constraints lack `constraint_claim` (drivers run 60/62; firing set
EMPTY on the live corpus), and every production boundary absorbs: `catch(_, true)` at
`json_report.pl:72/:76` + `trajectory_mining.pl:912` (vacuous success over a live throw,
W16); `catch(_, fail)` row drops (`maxent_report.pl:211`, `maxent_diagnostic.pl:395`); and
`maxent_threshold_proximity` absorbs UNCAUGHT via clause-failure-before-arithmetic (W12a — the
sink a catch-grep cannot see). Bonus: `maxent_indexed_run` quiet-fails standalone (hidden order
dependency on `maxent_run`, witnessed v3 vs v3b) — absorbed by the same json_report boundary.
OQ-112 re-ranked: widened absorber-boundary class (catch-true/catch-fail/clause-failure) is now
item 2. Tripwire for probe authors: the dynamic `maxent_profile/4` table is empty until
`maxent_run(Ctx)` runs in-process — sink probes that skip it get success-shaped LL=-10.0
(prior+bool) without touching the metric; witness profile-present before trusting a sink
result. Latent hazard: first claim-bearing story missing `suppression_requirement` silently
voids the whole maxent stage.

## 2026-06-11 — OQ-97 RESOLVED: Pattern-6 census executed (160/227/210 raw lines, 19 classes); 8 candidate classes filed as OQ-112; classification path clean
**Files:** ISSUES.md, audits/2026-06-11_oq97_pattern6_census/
**Tier:** landed

Worktree `oq97-pattern6-census` from `1bfd0b72`. Bounded grep census over 106 top-level
`prolog/*.pl` (denominator witness: subdir-load grep empty with 47-hit positive control on
stack.pl; scoped to STATIC load directives only — WRITEUP §7 residuals). Three shapes, raw
lists saved verbatim; all 7 pinned positive controls fired — and earned their keep: two Shape-C
grep iterations were rejected by the controls (bare-atom missed `pass(no_extraction_data)`;
no-comment-tolerance missed trailing-`%` defaults; Shape A regenerated with the same fix,
149→160). Class-based triage: 19 classes, file-don't-fix, zero engine edits. **No confirmed
candidate on the dr_type path** — drl_core.pl has zero Shape-A hits (the census itself
witnesses OQ-44 commit C's fix), and `signature_detection.pl:818/:905` tangled_rope branches
read as fired-signature override dispatch, not absence-defaults. 8 candidate classes → OQ-112
(top: diagnostic_summary agrees-on-absence probe signals, 13 sites, feeding the OQ-98 verdict
join as absence-of-alert). Census-surfaced interaction: post-OQ-44 `get_raw_suppression`
`unknown` sentinel makes `maxent_classifier.pl:255/:761` `; Supp = 0.0` dead branches and flows
an atom toward Gaussian-LL arithmetic (OQ-112 item 4). Row-26 tripwire strikes mapped by
content (purity_scoring :57→:58; coupling_factor :135, excess_extraction_factor :154);
`drl_fpn.pl:206` and `drl_boltzmann_analysis.pl:302` were NOT tripwired and stay candidates.

## 2026-06-11 — OQ-110 RESOLVED: residual join + pinned counterfactuals; operator ruled D-fork branch b NO-OPEN (derived-d stands); Backed deposit chain discharged
**Files:** ISSUES.md, python/audits/oq110_residual_join.py, audits/2026-06-11_oq110_residual_join/, prolog/temporal_residual.pl, prolog/drl_composition.pl, prolog/json_report.pl
**Tier:** landed

Worktree `oq110-residual-join`. Fresh pipeline at clean HEAD (manifest 2026-06-12T00:59:49Z,
`c22ec561`, n=62) — prior output was dirty-tree `25d6a637`; flip totals identical across runs.
(1.1) Backed end-to-end verified: controls A (fab_adjacent excluded via OQ-105
SuppBacked=false endpoint), B (backed flip present, deltas match JSON), C
(`with_retracted` eps@T2 removes flip, restore returns it) + full-corpus in-process identity
diff over 62 (comparator positive-controlled). The OQ-33 → OQ-46 → OQ-83 → OQ-110
Backed-verification deposit chain TERMINATES here. (1.2) Join: coverage both=11/62,
flips_only=23, stages_only=4, neither=24; 91 backed flips / 20 fab_adjacent; OQ-105
re-derived 23 rows/11 constraints (new host `institutional_trust_erosion`), 0 flips
on/adjacent. Committer moments are named atoms — presence-level join only. (1.3)
Pre-registered pins on all 91 flips × 2: 82 ε-explained / 9 supp-explained (ALL
snare-suppression-floor crossings at the analytical seat) / 0 genuinely unexplained; zero
third-type outcomes; identity-pin + expected-vanish controls pass. Aggregate v1's verdict
line keyed to the wrong bucket was corrected to the pinned ε-unexplained definition (buckets
untouched — implementation fix, not a criterion amendment). (1.4) Package escalated; operator
ruled branch b NO-OPEN with reopen condition: ≥1 backed flip surviving BOTH pins on a future
join. C1/C2 stubs persist; OQ-109 Phase C gate now reads "B complete" alone. Gotcha worth
keeping: `json_report.pl` is a NON-module script — its predicates live in `user`;
`json_report:write_temporal_residual/2` is an unknown-procedure error.

## 2026-06-11 — OQ-99 + OQ-100(a–c) RESOLVED: omega scenarios render authored protocols (subject-bound, fail-loud); report register coherence (qualified confidence labels, rival-P-graded disagreement, self-consistency header)
**Files:** prolog/report_generator.pl, python/enhanced_report.py, python/enrich_pipeline_json.py, agent/orchestrator.py, ISSUES.md
**Tier:** landed

Two output-changing commits (worktree oq99-omega-scenarios): `6b1092c0` (OQ-99),
`e9872538` (OQ-100 a–c). OQ-99: `generate_omega_resolution_scenarios/0→/1` takes the
report subject; `resolve_omega_source/3→/4` resolves omega_source → subject-binding →
fail-loud `unresolved_source` (never `Constraint: unknown`); authored 5-arity
`omega_variable` protocols (251 facts, 60/62 live testsets) now render per omega; catch-all
clause prevents mid-report abort. **Plan-correction worth keeping:** the 5-arity facts do
NOT land in module `user` — testsets declare `constraint_<id>` and the facts live there
(witnessed via wrong-guard first attempt: `current_predicate(user:omega_variable/5)`
failed silently and the generic template kept rendering; the module-keyed lookup also
disambiguates the 7 cross-file OID collisions). The 2 testsets without a module header
(`employment_boundary_contradictions`, `human_dignity_ai_governance_contradictions`)
author zero omega facts of any arity, so the no-5-arity path has no live instance — it
was witnessed by probe (typed template, bound constraint). Witnesses: scale_ceiling
before/after diff (4× unknown → 0); ai_governance gap omega still routes via omega_source;
probes A (unresolved [OPEN]) / B (catch-all on `empirical_v2`) / C (3-arity-only → typed
template); no-omega report byte-identical. OQ-100: labels `Pattern confidence
(categorical):` / `MaxEnt P(claimed):` (×2 sections — inventory sweep caught a 4th bare
label at the convergence section) / `MaxEnt bands (corpus):`; disagreement header graded
by rival P with cuts as `enrich_pipeline_json.py` constants (BAND_DEEP/BAND_MODERATE,
imported by enhanced_report.py; explicit None guard — bare comparison TypeErrors);
witnessed REJECTED at P=0.9969 (ai_governance_accountability), FAVORS RIVAL at P=0.5776
(institutional_trust_erosion), plurality + None via crafted entries (zero live <0.5
cases); `ONTOLOGICAL FRAUD DETECTION` → `DECLARED-TYPE vs OWN-ASSIGNED-METRICS
SELF-CONSISTENCY` (code grep zero outside archives). Legacy `agent/orchestrator.py:635`
regex updated to `MaxEnt P\(claimed\):` (groups unchanged, re.search witnessed). Engine
tests 10/10 + dynamic validation suite clean after each commit. OQ-100(d) subsumed by
OQ-101 ledger (partial-closure note in the OQ). Full-corpus report regeneration deferred
to the next `run_pipeline` (reports are re-derived artifacts). **Close-out residuals
(same day):** the wrong-module premise was swept repo-wide — single finding filed as
OQ-111 (`data_repair.pl` omega bridge guards on `current_module(IntervalID)`, imports 0;
probe-witnessed); the orchestrator regex match site was verified unchanged on a full
regenerated report (first match = convergence section line, before AND after the rename,
same value as `enriched_pipeline.json` entry confidence).

---

## 2026-06-11 — OQ-83 RESOLVED: measurement close-out; snapshot_type determinism guard; v7 §4.5 (A)/(B) census; OQ-109/OQ-110 filed
**Files:** ISSUES.md, prolog/transition_paths.pl, docs/deferential_realism_paper_v7.md, audits/2026-06-11_oq83_close/
**Tier:** landed

Operator-gated close of the stakeholder-layer migration's measurement question
(`audits/2026-06-11_oq83_close/`). **R4 ruled SATISFIED** (n=6 pilot diff = "produced and
preserved"; preservation witness 18 tracked pilot-arm JSONs — the plan's "20" reconciled as a
grep artifact catching 2 `phase_a_pilot_*` demos); corpus-scale census declined-with-reason
(structure pass named as what a re-open buys). **Ω_P transferred**, not answered: observer-axis
Type-B foreclosed (TWO_AXIS), committer C/B → OQ-87. **Classifier-sync item 5 resolved:**
nb_setval mechanism CONFIRMED at clinical T=0; milblogger T=18 graduates CLEAN (OQ-90/OQ-44
moved the piton path since the 2026-06-08 flag); NEW ε-sourcing mismatch
`challenge_as_commons_maintenance` T=5 (grid-misalignment class, no counted flip, unflagged).
Operator ruled determinism-fix-plus-document (counterfeit-witness rationale — a threading fix
would read as sync while the semantic ε-sourcing divergence remains): `snapshot_type/3` now
clears the classify_at_time nb-globals at entry (before/after witnesses + controls pasted;
`run_migration_tests` green; validation suite 0 warnings). The 2026-06-08 census substrate is
`archives/datasets/kernel_v2_test` (the then-live corpus, archived at `00c639da`) — overlay it
to reproduce. v7 §4.5 amended: one (A) data bridge (`influences`, drl_composition.pl:141) vs
≥3 (B) read-only seam diagnostics, all grep-witnessed live. Spin-offs: **OQ-109** (Phase B/C;
CLAUDE.md mandatrophy note retires there) and **OQ-110** (residual join + D-fork; inherits
consumer-side `Backed` verification). Phase-C calculus witnessed: live corpus 62 testsets,
47 with stakeholder facts / 49 with six-questions atoms → regen scope ≈ 13–15 stories.

## 2026-06-11 — Pew-typology review exchange landed: hedging-as-rigor dual, false-summit authoring discipline, OQ-107/OQ-108 filed, OQ-103 escalated
**Files:** docs/technical/build_discipline.md, CLAUDE.md, docs/design/design_discipline.md, ISSUES.md, prolog/testsets/institutional_trust_erosion.pl
**Tier:** landed

Operator review exchange over the Pew political-typology run (source:
`agent/analysis/originals/Pew_2026.5.10_political-typology_topline.txt`; four story files —
`institutional_trust_erosion`, `representation_legitimacy_gap`, `intra_party_fragmentation`,
`generational_value_divergence` — untracked in the main tree at landing time). What landed where:

- **Hedging-as-rigor (the under-confident dual)** → `build_discipline.md` → *Over-confident
  moves on the synthesis side* (new closing block) + a one-sentence tripwire as item (4) in the
  CLAUDE.md synthesis-side paragraph. "Held open" is earned only when no falsifier is
  specifiable; if a kill condition exists, commit and attach it. Trigger fires at generation
  time (drafting a both-readings passage), not at review. Corollaries recorded with it:
  claims-with-falsifiers-per-piece as the draft-time metric; weight reviewers' questions over
  their line edits when triaging. Instance: the "Counter-Reading, Held Open" section, written
  agnostic while the synthesis was available; an external reviewer's question forced the commit.
- **False-summit authoring discipline** → `design_discipline.md` §4: author testsets with the
  honest prior and let the engine fight it; never pre-conform claims to what classifies
  cleanly. Witness: `institutional_trust_erosion.pl:125` authored `constraint_claim(...,
  mountain)`, engine refused (false summit), and the refusal became the parent essay's spine.
  Includes the ontology-as-anomaly-detector point and the two-way essay↔engine loop.
- **OQ-107** (survey-wave witness adapter: instrument items → metrics; extends the OQ-102
  `measurement_basis/2` spine with a `witnessed` bucket; converts drift events from
  self-consistency checks into measurements) and **OQ-108** (per-position witness-coverage
  report; surveys sample powerless/moderate densely, institutional barely — flags which essay
  legs will be inference) filed in ISSUES.md.
- **OQ-103 escalated to load-bearing**: essays now make network claims; the
  trust↔representation `shared_victim` edge is the relocation thesis in graph form
  (`institutional_trust_erosion_report.md:142`), and it is corpus-topology, not story-authored
  (testset grep empty with positive control on `drl_purity_network.pl`).
- **"The mint"** (information regime as constraint — essay-generated hypothesis, first
  deliberate instance of the loop) queued as an OQ-69 ledger item.

## 2026-06-11 — OQ-90 RESOLVED: capture-keyed piton refinement in the FCR branch (piton un-darkened)
**Files:** prolog/signature_detection.pl, prolog/narrative_ontology.pl, prolog/config.pl, prolog/config_schema.pl, prolog/signature_mapper.pl, prompts/constraint_story_generation_prompt_json.md, ISSUES.md
**Tier:** landed

`piton` was dark corpus-wide: a piton's real distributed extraction trips `appears_as_rope`, a
Boltzmann failure fires FCR before the profile fallback, so every piton was subsumed as
`false_ci_rope`. Built the refinement (audit: `audits/2026-06-11_oq90_piton_refinement/`; commits
`f2368073` substrate, `64448411` output-changing, `fc724ab2` retirement, `3a4e0209` prompt):

- `narrative_ontology.pl`: `uncaptured/1` (POSITIVE-authored `diffuse`, never NAF), `piton_candidate/1`
  (uncaptured ∧ `prohibitive` fixing_cost), `transient_neglect/1` (uncaptured ∧ `cheap`; diagnostic only).
- `signature_detection.pl`: `fcr_evidence/6→/7` capture-disposition field (evidence trail, populated at
  the constructor — does NOT gate); new `resolve_with_perspectival_check/4` clause between the
  dead-coordination piton clause and the generic FCR clause, guarded by `piton_candidate/1` +
  `config:param(piton_refinement_enabled, 1)`. **Invariant: `dr_signature` stays `false_ci_rope`; only
  `dr_type` becomes `piton`.** Retired the `Supp≤0.2` `piton_signature` dispatch + helper (atom-keyed
  handlers left with superseded comments).
- **TRIPWIRE — `piton_refinement_enabled` fires even when `fcr_override_enabled=0`** (separate axis,
  intentional). Dedicated kill-switch; do not fold into `fcr_override_enabled`.
- **TRIPWIRE — read "piton sparse" only WITH the upstream-shadow caveat:** 4 corpus piton_candidates,
  but only 2 reach FCR (the other 2 are CI_Rope-certified upstream — designed shadow, not a bug).
  `transient_neglect` cell is corpus-EMPTY (all live diffuse claims are prohibitive).
- Output delta (`piton_refinement_enabled` 0→1): exactly 2 rows `tangled_rope→piton`
  (`regulatory_measurement_gap`, `institutional_trust_erosion`); leak controls `organization_floor` +
  `reprogramming_safety_toxicity` stay `rope`. The plan pre-registered 1 row on a 48-testset snapshot;
  live corpus is 52 (4 untracked working-tree testsets feed the pipeline) — re-registered to 2 after
  the K=0 diffuse hand-audit was extended to `institutional_trust_erosion`. **Reproducibility flag:** a
  fresh clone at HEAD sees only 48 testsets (the 4 are untracked) → would reproduce a 1-row delta; the
  4 untracked testsets must be committed for the 2-row result to reproduce.
- Superseded-pending (not removed): `drl_core.pl:344,403` theater piton clauses; maxent piton
  `default_profile` (`maxent_classifier.pl:153–155`, theater-keyed, now stale vs the capture
  definition); `python/axiom_reachability.py:171,207` cascade replica models the removed clause.
- Unblocks OQ-37's `validate_edge_cases` resistance-keyed piton-check removal (successor now exists).

## 2026-06-11 — OQ-44 RESOLVED: fail-closed-on-absence ruled (statute for new gates, marker carve-out, common-law for existing); OQ-43 closed; thermal_dissipation_constraint un-certified
**Files:** prolog/signature_detection.pl, prolog/drl_core.pl, python/shared/schemas.py, ISSUES.md
**Tier:** landed

Operator ruling (witnesses: `audits/2026-06-11_oq44_policy_close/`; ruling text: ISSUES.md OQ-44
still-operative block). Grounded in converged practice — five fail-closed conversions, none
reverted — with the instance-counter satisfied as confirmation only. Statute: new/modified gates
fail closed on absence (`unknown`/OPEN on empty; pass carries its witness). Carve-out: absence →
authored provenance only via positive-control inference at authoring/compile time (the
`suppression_profile` precedent), never emptiness-inference at the read site. Existing gates:
common-law per-instance, prioritized by success-shapedness. Dispositions: (1)
`has_viable_alternatives` default `false`→`unknown` (commit `8b5a34b8`, output-changing) —
`thermal_dissipation_constraint` UN-CERTIFIED (natural_law→ambiguous; NL→mountain override
dropped, rope at moderate/institutional, verdict green→red perspectival_incoherence; all 277
diffs single-cause); (2) `get_raw_suppression` 0-default → `unknown` sentinel + `number/1` guard
at `classify_from_metrics` (commit `966d53c8`) — the witness CORRECTED the "never consumed"
pre-derivation: the two non-story `cs_axiom_contradiction` files exported the fabricated 0 and a
`fingerprint_voids` agreement computed on it (both now honest; `shared/schemas.py` suppression
nullable, null = no authored scalar); (3) report-layer 0.0 defaults CONFORMING as-is (print
MISSING). OQ-43 resolved in the same stroke, fifth-instance disposition recorded there.

## 2026-06-12 — First-contact gate C-range corrected: slot-count!=32 removed (partial grids are LEGAL); first misfire had halted the pipeline on an OQ-90 flip target
**Files:** python/grid_first_contact_gate.py, python/grid_audit_ledger.json
**Tier:** landed

The gate's C-range clause carried the BATCH addendum's full-grid mandate ("slot count != 32 =
battery failure") into the standing first-contact gate — but partial grids are operator-CONFIRMED
legal (no fraction threshold; consumer-named-levels decides sufficiency; the coverage read
reports OPEN where insufficient). First live-prompt opt-in story
(`institutional_trust_erosion`, Pew run, 12/32 all-valid points, endpoints correct, no dupes)
was excluded and run_pipeline HALTED — colliding with OQ-90, whose witnessed delta needed the
story. Corrected: C-range = value outside [0,1] OR duplicate slots (the genuinely
schema/compiler-unreachable shapes); C-flat now evaluates the slot-groups PRESENT (>= 2 levels
at a (metric,time); fires only if evaluable groups exist and all span < 0.05); partial grids
pass with a `coverage` field + prompt-compliance NOTE in the ledger (surfaced, never excluded).
Witness 6/6 (`audits/2026-06-12_gate_partial_fix/gate_partial_fix_witness.txt`): misfire story
passes as legal partial; C-range still bites on out-of-range + duplicate; ECHO/FLAT controls
unchanged; NEW control — partial-but-degenerate grid still fires C-flat. Pipeline exit 0 on the
62-corpus, story ledgered `coverage: 12/32`. OQ-90's two-row delta preserved.

## 2026-06-11 — OQ-93 FLIP RULED + EXECUTED: live prompt opt-in grid section; κ gate → first-contact gate; 10 batch stories promoted (corpus 48→58); two latent defects found by promotion
**Files:** prompts/constraint_story_generation_prompt_json.md, prompts/grid_batch_addendum.md, python/grid_first_contact_gate.py, python/grid_audit_ledger.json, python/run_pipeline.py, python/python_test_suite.py, prolog/data_repair.pl, prolog/validation_suite.pl, json/, prolog/testsets/
**Tier:** landed

Operator ruling: flip now; the one-time κ gate becomes FIRST-CONTACT — every grid-authoring
story is audited once (three indicators, per-story fail-closed) before any consumer read,
ledgered in `python/grid_audit_ledger.json` (seeded with the 10 gate-passed batch stories);
C-echo in any new story HALTS run_pipeline and demands the flip be reverted. Gate controls
4/4 (first_contact_gate_witness.txt). Promotion witnesses: exactly the 10 stories carry
authored 32/32 grids in pipeline output (flip_promotion_witness.txt); suite 58/58 green —
48 grid-absent honestly OPEN + 10 real increasing_coercion verdicts on authored data
(flip_promotion_suite.txt) — the first live-corpus grid consumption in the construct's
history. **TRIPWIRE — baselines re-pinned:** every standing 0-diff witness referenced the
pre-promotion substrate (the "143/143 byte-identical" compiler sweep = 143 json files, now
153; the phase-6 suite diff = 48-corpus, now 58); cite those witnesses as
of-their-substrate, re-run before reuse (staleness ladder). Two latent defects found by
first contact and fixed with witnesses:
1. `data_repair:grid_provenance` read measurement/5 with the interval ANONYMOUS —
   56/58 constraints read other stories' grid points as their own the moment ten grids
   coexisted in one KB (build-unit-1 leakage class; single-interval loads had masked it).
   Interval-scoped now; post-fix pipeline shows exactly the 10.
2. `python_test_suite.py`'s unanchored interval regex matched PROSE before facts — three
   phantom test_case IDs ('18' from "interval (18 months)", '0', 'from') ran green against
   scenario-manager-injected anchors while those stories' real intervals never got their
   suite pass (success-shaped miss). Regex anchored to the compiled fact form + fallback;
   59 test_cases all real IDs except the two genuinely interval-less contradiction files.
Spot-check witnesses added at operator flag: phase-6 diff mechanically traced (105/105
before-lines name the retired flag; 105/105 after-lines carry RETIRED wording; 22 ELAPSED =
all 232 lines); FSM number/1 guard two-sided control (sentinel reaches clause, FSM abstains
cleanly, unguarded comparison witnessed throwing).

## 2026-06-11 — OQ-93 grid migration LANDED end-to-end (stages A–D + coverage read + shim retirement); OQ-96/OQ-101/OQ-102 closed with it; intent sub-fork filed as OQ-106
**Files:** schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prolog/coercion_projection.pl, prolog/pattern_analysis.pl, prolog/intent_engine.pl, prolog/report_generator.pl, prolog/signature_detection.pl, prolog/drift_report.pl, prolog/diagnostic_summary.pl, prolog/json_report.pl, prolog/narrative_ontology.pl, prolog/config.pl, prolog/config_schema.pl, prolog/scenario_manager.pl, prolog/data_repair.pl, prolog/data_verification.pl, prolog/domain_priors.pl, python/enhanced_report.py, python/run_pipeline.py, python/domain_priors.py, python/shared/schemas.py, python/tensions_ledger.py, agent/c-orchestrator.py, agent/generate_grid_batch.py, prompts/grid_batch_addendum.md
**Tier:** landed

Full audit package: `audits/2026-06-11_oq93_grid_migration/` (preregistration + per-stage
witness scripts/outputs). Worktree branch `oq93-grid-migration`, commits `bc41e8f4..` —
every stage carries its same-commit witness. Landed, in ruled order:
- **Stage A:** optional `coercion_grid` block (GridMetric/GridLevel enums DISJOINT from
  MeasurementMetric; `stakes_inflation` resurrected grid-side only); rider OQ-102(a)
  `basis` (observed|projected) on Measurement + grid points. 16/16 battery;
  143-file additivity sweep 0 deltas.
- **Stage B:** compiler emits sorted `*_grid_NN` measurement/5 facts (source_class
  authored); fail-loud integrity NOT bypassed by --no-validate: t0/tn == interval
  endpoints, time_point ∈ {t0,tn}, duplicate-slot REJECT (the contract licensing the
  once/1 cap in pattern_analysis). 143/143 byte-identical old-vs-new; constructed-
  duplicate control bit on both CLI paths. Rider: `measurement_basis/2` emission +
  `projected` bucket in `measurement_provenance` (meas_prov/5; json_report +
  shared/schemas carry the key).
- **Coverage read:** `system_gradient/4` carries coverage(Present, All); the `[]→0.0`
  fabricated default KILLED — empty reads FAIL → OPEN; `system_gradient_for/4` is the
  consumer-named-levels read; pattern/intent verdicts carry open(...) through (never
  mapped to stable). Two-sided witness: 8/32 one-level grid flips
  increasing_coercion→OPEN while all five probe stories hold exact pinned values; suite
  green with 48/48 [INTENT] OPEN.
- **Stage C:** grid-batch addendum (no worked value table — OQ-70 discipline) assembled
  with the live prompt at call time (no fork); N=10 batch (operator-ruled) generated;
  κ plausibility audit vs the operator-ruled split gate (C-echo zero-tolerance halt;
  C-flat/C-dir ≥2/10 escalate; per-story fail-closed exclusion): PASS 0/10 excluded.
  **Bug rider (the probe pattern repeating):** first audit read open(no_gradient_data)
  on ALL 10 — `time_point_in_interval` enumerated scalar-series times as gradient
  next-points; fixed with a compound(Metric) guard (grid times = grid-measurement
  times); probe stories had masked it (no scalar series).
- **Stage D:** `level_gradient_divergence/2` (rising-structural/falling-individual)
  wired POSITIVELY into FCR (new fcr_test_failure clause) + FSM (fsm_evidence/3,
  one-rung confidence bump; `open` on absence leaves pre-wiring values exactly) + the
  extraction-blindness omega (witnessed-process tail). OQ-94 sort respected (CI_Rope
  benignity gates untouched); `structural_coercive_intent` stays unwired (ruling (a) →
  OQ-106). Fire-on-migration: kappa `[CONDITIONAL: grid authored 16/32]` tag WITNESSED
  FIRING; moderate→yellow cap why-not recorded (0 correction-grade carriers on the
  48-corpus today).
- **Shim retirement (closes OQ-96):** `grid_shim_enabled` + injection/imputation/gate
  arms removed; `domain_registry.pl` regeneration + .gitignore fossil retired;
  domain_priors.py --output repo-relative; source_class buckets KEPT. Before/after
  full-suite diff: 0 unclassified lines (wording of the two retirement messages +
  [ELAPSED] noise only); per-class counts identical (FAIL 0/0, OPEN 513/513, SHIM
  48/48). NOTE: prereg said "0-diff"; actual = justified-wording-diff because the old
  messages named the retired flag — recorded here rather than silently absorbed.
- **OQ-102 closed:** (a) basis chain witnessed end-to-end (fixture → compiler →
  measurement_basis/2 → meas_prov(39,0,0,2,39) → ledger drift line); (b) drift
  severity joins its own confidence at the read site (`[warning | confidence: low]`
  witnessed live on agenda_conditioning) + projected caveat in the report trajectory
  section.
- **OQ-101 closed:** `python/tensions_ledger.py` (non-generative) replaces orchestrator
  step 6 (`_step_essay` REMOVED); 48/48 blocks witnessed on real pipeline output;
  fidelity spot-check vs two regenerated reports clean.

**PENDING OPERATOR (recorded, not self-resolved):** the live-prompt flip to
opt-in-by-story-focus — the N=10 PASS is necessary-not-sufficient by the operator's own
provision (supplemental batch optional); the 10 grid-batch stories sit in
`audits/2026-06-11_oq93_grid_migration/grid_batch/` (json+pl) pending a
promote-to-corpus decision with the flip ruling.

## 2026-06-11 — Backed semantics BUCKETED (follow-on to the OQ-46 close): compiler-stamped suppression_profile(static) sanction marker; OQ-105 filed; OQ-37 piton vacuous-green fixed
**Files:** prolog/drl_composition.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, prolog/data_validation.pl, prolog/testsets/thermal_dissipation_constraint.pl, ISSUES.md
**Tier:** landed

Same-day follow-on ruling to the OQ-46 close (evidence + witnesses:
`audits/2026-06-11_oq46_backed_reconciliation/`; commits `00040bb9`, `b0a0e380`, `609dbb47`).
The close left `Backed=false` on ALL scalar-supplied rows; the operator ruled **bucketed, keyed
on an explicit sanction, never emptiness-inference**: `suppression_profile(C, static)` is
compiler-stamped (`generate_constraint_pl.py` §8) only when the JSON authors other series but
deliberately omits suppression (positive-control absence); `classify_at_time` `SuppBacked` is
three-way — marker-sanctioned static scalar backs / grid-misalignment substitution excluded
(OQ-105) / **unmarked seriesless fails closed**. Decision witness: bucketed = 59 flips / 20
fab_adjacent unchanged (only `backed_times` rises, 7×4 contexts); blanket = 79/0, laundering
substitution-dated transitions into the OQ-83 D-fork flip count. Corpus-wide the scalar IS the
series endpoint (37/39 exact, pre-registered one-time query — 0 violations, so the equivalence-
lint question is closed-no-demonstrated-content) — which makes the misalignment substitution
ANTI-CAUSAL; it currently sets flip timing in 2 witnessed timelines
(`substantive_employment_reading` T=9, `post_1998_convergence` T=13; 1 checked-negative). The 7
seriesless testsets were recompiled from JSON (per-file diff = marker fact + decl only, zero
drift). Pipeline A/B: 30 diffs = 28 backed_times + 2 manifest, nothing else. Also: the
`data_validation` piton check joined over never-authored `resistance_to_change` and printed
"✓ No pitons detected" unconditionally — now prints a VACUOUS notice / joined-table sizes
(OQ-37 row updated; heuristic removal stays gated on OQ-90). Correction to the close-session
evidence: deletion-counterfactual phantom transitions surface via `temporal_residual`, not
`drift_trajectory` (raw series only).

## 2026-06-11 — OQ-46 RESOLVED: the classify_at_time scalar suppression fallback is SANCTIONED (operator ruling), not a retirable stopgap; OQ-46's premise contradicted the live generation prompt
**Files:** prolog/drl_composition.pl, docs/technical/classify_at_time_wiring.md, prompts/constraint_story_generation_prompt_json.md, ISSUES.md
**Tier:** landed

Read-only evidence pass + operator ruling (`audits/2026-06-11_oq46_close/`, branch
`oq46-ruling`). The OQ-46 retirement plan ("once the template authors a temporal
`suppression_requirement` series for every constraint, delete the scalar clause") rested on a
premise the prompt itself contradicts: since 2026-05-30 (commit `220739b8`, pre-reset)
`constraint_story_generation_prompt_json.md:457` instructs "Do NOT author
`suppression_requirement` measurements unless the story's narrative specifically tracks
enforcement-capacity change" — scalar-only is *deliberate authoring* for static-enforcement
stories, so the wait-state never terminates. Witnessed: 7/46 live stories scalar-only, all
prompt-conformant (physics/structural, supp 0.01–0.35, two 2026-06-09 batches incl. 3
regenerated under the required-metrics schema); 21 of 47 fallback rows are time-grid
misalignment inside 10 series-authoring constraints (series universality alone would not retire
the clause); deletion counterfactual flips 16/46 timelines (7 collapse to `[unknown]`, 9 gain
phantom `drift_trajectory` transitions); `snapshot_type`/`degradation_chain` have zero consumers
(positive-controlled grep), so the OQ-41 divergence concern is latent. **Operator ruled: accept
the prompt's design.** The read ladder (temporal at T → scalar-as-constant `Backed=false` →
fail-closed `unknown`) is permanent; no scalar/temporal equivalence check; Surface-3
temporal-suppression work gates on per-snapshot `Backed`, not corpus-wide series coverage.
Comment-only edits to `drl_composition.pl` (STOPGAP → sanctioned); wiring doc §1 re-ruled;
ISSUES.md OQ-46 compressed-on-close with the ruling block kept; cross-refs at OQ-33/OQ-40/OQ-41
updated. Side observation, same session: the two `*_contradictions` testset files are non-story
`cs_axiom_contradiction/2` records — they explain every "48 files / 46 classified" denominator gap.

## 2026-06-11 — Tripwire: the moderate→yellow verdict cap is confirmed-but-never-stressed; re-rule evidence arrives with the first correction-grade signature on a base-GREEN constraint
**Files:** prolog/diagnostic_summary.pl, prolog/signature_detection.pl
**Tier:** tripwire

At the OQ-98 close, severity=moderate for correction-grade signatures was confirmed only in
the sense that it changed nothing: all 13 correction carriers already had base ≥ yellow, so
zero moderate caps have ever shipped. The ruling has not been stressed. When the FIRST
correction-grade signature fires on a base-green constraint (corpus-content event, not
grid-gated), re-run the histogram gate
(`audits/2026-06-11_oq98_verdict_join/histogram_gate.pl`) and surface the transition to the
operator before trusting the new headline — that firing IS the re-rule evidence the
2026-06-11 ruling deferred to. Cross-listed in OQ-93's fire-on-migration witnesses (with the
kappa CONDITIONAL tail, the other dormant OQ-98 path).

## 2026-06-11 — OQ-98 RESOLVED: report headline verdict is now verdict_join (Prolog-side join over alerts + provenance, serialized with raw inputs); schema_version 1→2
**Files:** prolog/diagnostic_summary.pl, prolog/signature_detection.pl, prolog/json_report.pl, prolog/report_generator.pl, python/enhanced_report.py, python/run_pipeline.py, python/shared/schemas.py, ISSUES.md, audits/2026-06-11_oq98_verdict_join/
**Tier:** landed

Commits `e8ab707b` (plumbing, byte-identical pipeline witness) → `170db693` (pre-output
histogram gate) → `ce9a26ec` (output-changing, alone). `diagnostic_summary:verdict_join/3`
joins the base verdict with severity-floored alerts (`drl_core:dr_mismatch/3` + the new
`signature_detection:signature_grade/2`/`signature_severity/2`: correction-grade = override
signature that actually rewired the type, alerts at moderate; commentary never alerts) and
carries grid + measurement provenance (`data_repair:grid_provenance/2`, `source_class/2`).
Serialized in `json_report.pl` as a SIBLING of `diagnostic_verdict` (raw inputs alongside,
never instead); `enhanced_report.py` headlines `verdict_join.verdict`, prints BASE +
per-alert reconciliation when capped, ALWAYS prints the grid line, renders `[UNJOINED]` on
stale artifacts; sidecar verdict = joined. Corpus effect at close: 8/48 headlines changed
(6 green→red, 2 yellow→red, all severe claim-mismatch), zero moderate caps. P1 probe ruled
the grid question: BRANCH A — no diagnostic subsystem is grid-fed (0/48 changed under full
synthetic grids, positive control 46/46 `classify_interval`), so grid-diet lines carry
`[CONDITIONAL]` tags instead of gating the headline; revert to strict fail-closed if a
subsystem ever becomes grid-fed. Tripwire promoted to CLAUDE.md Architecture Invariants:
headline = `verdict_join.verdict`; `diagnostic_verdict.verdict` is a raw input, never a
headline. Witnesses W1–W4 + 2 falsifiers: `audits/2026-06-11_oq98_verdict_join/`.

## 2026-06-10 — OQ-95 resolved: constraint_neighbors/3 now fail-closed on phantom (zero-fact) constraints; giant_comp edges scoped to enumerated nodes; domain_registry throw hit independently (folded into OQ-96 at merge)
**Files:** prolog/drl_purity_network.pl, prolog/giant_component_analysis.pl, prolog/tests/test_phantom_neighbor_filter.pl, prolog/tests/test_forecloses_fpn_injection.pl, ISSUES.md, audits/2026-06-10_oq95_phantom_node_fix/writeup.md
**Tier:** landed

OQ-95's gating census found ALL five `constraint_neighbors/3` consumers (giant_comp, drl_fpn,
network_dynamics, json_report, drl_purity_network's own `bfs_path`/cascade walks) inheriting
phantom endpoints from 26 dangling authored `affects_constraint/2` facts, so the fix landed at
the shared source: `phantom_subject/1` (neither `constraint_claim/2` nor `constraint_metric/3`)
makes `constraint_neighbors/3` **symmetric fail-closed** — phantom endpoints are excluded and a
phantom *subject* returns `[]` (pre-fix the reverse-edge clause made phantoms traversable nodes;
`contamination_path` could route through a constraint that does not exist). Second layer:
`giant_component_analysis:precompute_edges_loop` scopes `assert_edge_canonical` to the enumerated
node set (`ord_memberchk`), making component > node-count impossible by construction.

Witnesses (`audits/2026-06-10_oq95_phantom_node_fix/`): live corpus largest component
118.9% → 56.8% (44→21 of 37); original_v6 259.9% → 89.2% (8,785→3,014 of 3,380); gc edges
75→49 = exactly the 26 dangling facts; post-fix phantom endpoint count 0 with firing positive
control; new 4-test suite `test_phantom_neighbor_filter.pl` (positive control + forward/reverse
exclusion + corpus census); `fpn_injection` 6/6; validation suite 39/39 exit 0; testset-embedded
threshold failures byte-identical before/after (9 pre-existing, unrelated).

**Contract change (the part a fresh agent could trip on):** the claim-OR-metric existence test
is NOT corpus membership — engine demos/probsets still pass — but a synthetic constraint
asserted by a test/probe now needs at least a `constraint_claim/2` to participate in the
network; `test_forecloses_fpn_injection` fixtures were updated for exactly this. Contamination
*values* never needed the fix (the `purity_score/2` `-1.0` sentinel already made phantoms
inert); the defect was purely topological. Generation-time fail-loud (option b) rejected:
dangling refs are an expected, separately-censused property of generated corpora
(`dangle_curve.py` OQ-58, `reading_reference_linter.py`).

Side-finding: hit the `domain_registry:domain_category/2` existence error independently in this
clean worktree — same defect the parallel session diagnosed deeper and fixed as **OQ-96** (module
deleted 2026-02-18; dead clauses removed; suite GREEN without the file). Three residue facts from
the independent path were folded into the OQ-96 entry at merge: the `.gitignore:8` fossil (stale
local copies mask the failure on long-lived checkouts), `run_pipeline.py:268` now regenerates a
file NOTHING consumes (Pattern-1 producer; retire with the shim flag), and
`python/domain_priors.py --output` defaults to an absolute path into the main checkout.
Note on the witness above: "validation suite 39/39 exit 0" was run pre-merge under the
stale-registry-file regime; re-witnessed post-merge under the shim-off regime (see merge commit).
## 2026-06-11 — OQ-33 RESOLVED: row-23 fail-close re-witnessed clean on live + kernel_v1; halt→disposition→control-gated clean re-scan; .gitignore unanchored-outputs tripwire found
**Files:** ISSUES.md, audits/2026-06-11_oq33_close/, prolog/drl_composition.pl, prolog/archives/pre_reset_outputs/, audits/2026-05-30_authoring_closure_fabricated_defaults/tripwire_fabricated_defaults_results.json, .gitignore
**Tier:** tripwire

Evidence pass for closing OQ-33 (plan retargeted from OQ-95). **The fix is sound on current
substrate:** live corpus (48 files/46 classified) 209 constraint×time rows = 162 temporal / 47
scalar-STOPGAP / **0 unknown-floor / 0 residual-0.5 anomalies**; kernel_v1 overlay (1,106 loaded,
path witnessed) 3,497 rows = 2,882/615/**0/0**; D2 `get_raw_suppression` else-branch 0/46. Every
census process ran its own positive controls (unknown-floor + STOPGAP synthetics; same-call-path
control for D2) before its zeros. `Backed=true` 161/162 temporal rows; the 1 false =
`techno_optimist_reading` t=5 (ε fallback, OQ-41 rows 24-25 scope).

**Correction-key (cite-discipline):** the `drl_composition.pl:191-197` comment figures
**471/562/91/0 are NOT kernel_v1 figures** — commit `b5ccee0d` (2026-06-02) measured them on a
562-testset working-tree state that was never archived (226 testsets tracked at that commit;
corpus reached 1,106 by the reset). kernel_v1 measures 934/1106 temporal, 172 scalar-only, 0
unknown. Do not cite 471/562 against any extant corpus; an exact-match expectation must pin the
substrate (corpus + commit), not just the figures.

**Close path:** evidence pass HALTED on the pre-registered Probe D condition — 4 pre-reset
artifacts live in `outputs/` (`pipeline_output.pre_agency_fix.json` manifest 2026-06-03;
`tripwire_fabricated_defaults_results.json`, the 2026-05-30 OQ-33 tripwire evidence cited from
gitignored `outputs/` by its audit; `schema_sieve/{analysis,features}.json` manifests 2026-06-04)
— escalated; operator ruled same day (archive / relocate-to-audit-dir / probe-then-archive /
delete scratch). Executed sha256-verified: archives at `prolog/archives/pre_reset_outputs/`,
tripwire JSON now inside its audit dir (citations fixed), 7 unparseable `scs_out_*.json` deleted.
Re-scan with in-run archive-side positive control (manifest ×3 + tripwire-content ×1 fired on
the relocated artifacts, THEN live scan): 1,055 JSONs, **NO HITS — witnessed-clean**. OQ-33 →
resolved (compressed); OQ-46 annotated with live coverage (the 2026-06-05 "20/20 universal"
template check did NOT hold — 7/46 live constraints are scalar-only); `drl_composition.pl:191`
comment re-stamped three-substrate/as-of-dated (comment-only; post-edit `[stack]` load witnessed).

**TRIPWIRE (RESOLVED same day, history kept) — `.gitignore:2` was an UNANCHORED `outputs/`:**
it silently swallowed ANY nested dir named `outputs` — a disposition commit dropped all four
archive files clean (witnessed; commit succeeded, files absent) until the archive dir was
renamed `pre_reset_outputs`, and `audits/2026-02-25_spectral_laplacian/outputs/` (25 evidence
files) had been gitignored since creation. Operator ruled: anchor, don't relocate. Landed as
commit `09390f0f`: rule anchored to `/outputs/`; pre-anchor survey of every nested outputs dir
(python/outputs empty; `prolog/archives/datasets/original_json/outputs/` 332 files/40M never
tracked → own ignore line, status-quo as a visible decision, track-or-not open); post-anchor
delta = exactly the 25 spectral files, plain `git add` sufficed (anchor-took-effect check).
Residual invariant (citations can dangle by other routes) filed as OQ-104.

## 2026-06-10 — External-review triage (two batches): OQ-98–103 filed; auto-essay synthesis ruled out (ledger replaces it); two topic runs committed under a live-witnessed gate
**Files:** ISSUES.md, audits/2026-06-10_external_review_vote_market/, audits/2026-06-10_external_review_xprize/, KNOWN_STATE.md, prolog/validation_suite.pl, agent/c-orchestrator.py
**Tier:** landed

Two external-review batches triaged against the reports/code/source (external output = hypothesis,
verified before any OQ). **Batch 1 (vote-market six, commit `2d54826c`):** 8 claims → OQ-98
(verdict banner is not a join — GREEN over a 0%-authored grid + alongside `! ALERT [severe]`;
`build_verdict_banner` reads only `diagnostic_verdict`), OQ-99 (omega generator prints
`Constraint: unknown`, `report_generator.pl:572-583`), OQ-100 (register incoherence: 3 "confidence"
meanings, HARD DISAGREEMENT at rival P=0.95, "ONTOLOGICAL FRAUD" overclaim; (d) severable),
+ notes on OQ-44 (resistance_to_change default `0.0` at `report_generator.pl:507`), OQ-93 (W1/purity
are arithmetic over the imputed grid, shim-era). **Batch 2 (XPrize three, commit `96113b05`):**
6 critiques → OQ-101, OQ-102, OQ-103, + OQ-94 cross-ref (who-bears vs who-benefits) + an OQ-98
framing line.

**Load-bearing ruling (operator, 2026-06-10): CUT orchestrator step 6 (the Sonnet auto-essay);
replace with a deterministic, non-generative tensions ledger (OQ-101).** The essay *form* collapses
plurality (the auto-essay announced *"converges on a single structural conclusion"*); `uke_think`
over-stated identically, so the defect is form-not-implementation and prompt guidance can't fix it.
The synthesis-fidelity discipline is NOT an OQ — it lives as a live-synthesis checklist in
`audits/2026-06-10_external_review_xprize/README.md`. Step 6 removal in `c-orchestrator.py` is
pending (OQ-101 build), not done this session.

Run-outputs gate: `run_dynamic_suite` re-run over the full 48-constraint corpus, exit 0 (witness:
`audits/2026-06-10_external_review_vote_market/gate_witness.txt`; positive control — reaches
test_case 48). `validation_suite.pl` auto-regen 39→48 (both runs' constraints) committed in
`2d54826c`. The earlier RED-gate-budget proposal was dropped (premise dissolved when OQ-96 went
GREEN before these commits). `essays/2026-06/who_owns_younger.md` left untracked (operator
finished-essay tree, not engine output). Staged plan: `~/.claude/plans/i-ran-an-article-merry-lagoon.md`.

## 2026-06-10 — OQ-92 RESOLVED: gain_flow receipt surface live end-to-end (schema→compiler→prompt→batch→gates); GAP-10 closed; OQ-90 Steps 2–4 unblocked
**Files:** ISSUES.md, docs/design/design_gaps.md, prompts/constraint_story_generation_prompt_json.md, prolog/narrative_ontology.pl, prolog/drl_core.pl, prolog/maxent_classifier.pl, prolog/signature_detection.pl, prolog/data_repair.pl, prolog/testsets/gfbatch1/, audits/2026-06-10_oq92_step3_preregistration/
**Tier:** landed

Stage C promoted stakeholders[] + six_questions + the receipt surface into the LIVE generation
prompt (additively — four-tuple arrays stay, OQ-83 R4 control arm intact; the live prompt had
carried NO stakeholder guidance, pilot-only). First batch (gfbatch1, 6 stories, run-tagged out
of the corpus glob): 6/6 author gain_flow + fixing_cost, 0 diffuse, referential integrity
clean end-to-end. Diffuse audit at K=0 against the pre-ruled criterion: **0/0 observed —
vacuous pass stated as vacuous**; 6/6 named-capture flagged authoring-convention-until-checked
(matters for OQ-90's piton side: a diffuse-starved corpus leaves piton_candidate unreachable —
check prevalence before reading a piton sweep as absence). Stage D:
`narrative_ontology:constraint_captured/1` (positive computation; absent/diffuse never block)
+ OQ-94 benignity gates rows 1–3 + maxent scaffold spec same-commit; two-sided controls all
landed (uncaptured→scaffold vs captured→rope; captured→pure_scaffold; CI_Rope deterministic
intervention with verified restore). Fabrication-ban grep witness in data_repair.pl. Suite
green; warning gate fired correctly on a deliberate maxent line-drift (allowlist updated
849→852). OQ-92 resolved with the Rulings block kept (operative); GAP-10 closed; OQ-90
Steps 2–4 now pure build on a real surface.

## 2026-06-10 — OQ-96 interim landed (shim OFF, suite green, warning gate wired) + OQ-93 viability probe: gradient cut-bug found and fixed; all pinned values exact post-fix; intent top verdict range-dead witnessed
**Files:** prolog/config.pl, prolog/config_schema.pl, prolog/scenario_manager.pl, prolog/data_repair.pl, prolog/data_verification.pl, prolog/domain_priors.pl, prolog/coercion_projection.pl, python/run_pipeline.py, python/load_warning_gate.py, prolog/load_warning_allowlist.txt, audits/2026-06-10_oq93_grid_viability_probe/
**Tier:** tripwire

**Standing behavior change:** `grid_shim_enabled=false` (config + schema spec) — the DR-AUDIT
grid shim is OFF by default: no injection, no imputation, the 32-point completeness gate
reports OPEN-and-witnessed instead of failing (or being satisfied by manufactured filler).
`[INTENT]` confidence on corpus stories now reads honest `low` (real 0/8), not manufactured
`high`. Set `true` only for archive replays of shim-era behavior. The dead `domain_registry`
references (module deleted 2026-02-18) are REMOVED — both clauses were throw-only for four
months (could never succeed), witnessed crashing the suite at TWO sites (repair imputation via
the Polaris story; `data_validation:127` once repair stopped crashing). Suite GREEN post-change
(0 errors/0 warnings, 47 [OPEN] witnessed-absence lines). **New pipeline gate:**
`python/load_warning_gate.py` + `prolog/load_warning_allowlist.txt` (4 known-benign records)
wired into run_pipeline beside the ISSUES gate — do NOT `grep -v Warning` over load output;
unexpected load warnings now abort the pipeline (negative control witnessed). **Tripwire for
anyone touching coercion_projection/pattern_analysis/intent_engine:** `system_gradient`'s
`[] → 0.0` fallback is a fabricated default — a failed gradient and a flat gradient emit the
same token; the OQ-93 probe witnessed an "(Optimized)" cut in `time_point_in_interval/2` that
made EVERY gradient ever computed fail into that 0.0 (stable-only basin = the cut, not data
starvation; one-char fix landed, corpus regression green). Probe verdict (preregistration
`e7e78a1b`, FINDINGS in the audit dir): post-fix ALL pinned values exact (G_sys ±0.588 etc.,
κ 5/5, all three pattern labels reached, first non-stable intent verdicts in the construct's
history); `structural_coercive_intent` RANGE-DEAD witnessed at the domain edge (max reachable
G_sys 0.98 < threshold 1.00 strict, with full hand-authored Conditions-2–4 evidence —
this probe authored those tables' first-ever facts). **Generalization (operator): the
`[] → 0.0` fallback is the success-shaped-default pattern — the cut was invisible precisely
because failure and "measured zero" were byte-identical at the read site; same channel-level
pathology as `grep -v Warning`, one layer down (suppressed-channel vs collapsed-value).
Ruling (a) recorded: intent top verdict RETIRE-OR-REDESIGN (sub-fork deferred); backward
contamination sweep WAIVED (forward only). Redundancy diff (REDUNDANCY_DIFF.md): zero by
DISJOINTNESS — κ-track's unique product is the level axis; bonus defect:
`coercion_vector`/`compute_completeness` interval-UNSCOPED (completeness=312.5 on loaded
corpus; single-story-safe only). Ruling (b) returns priced — then RULED keep-and-migrate
(named-consumer kind: the masking/naturalization verdict family; intent top verdict stays
retired; imputation killed permanently; sequence + κ-plausibility gate recorded in OQ-93).
Build unit 1 (interval scoping) landed: probe values unchanged exact, leakage healed
(312.5→0), suite green. **once/1 irony (operator flag): the slot-capping fix uses the same
first-solution-only mechanism as the cut bug it buried — sound ONLY under the
identical-by-contract premise, with the contract (duplicate slot authorship rejects loud)
enforced by the stage-2 compiler; once/1 is defense-in-depth, never primary semantics;
constructed-duplicate control queued to the stage-2 battery. Partial-grid threshold question
DISSOLVED on evidence: witnessed 8/32 one-level grid → G_sys=0.216 + increasing_coercion at
completeness 0.25 (findall absorbs missing levels — success-shaped absorption one aggregation
up); design answer = coverage-carrying G_sys + consumer-named-level requirements, confirm at
stage-2 prereg.**

## 2026-06-10 — OQ-94 read-site pass complete: rule sorted 12-file consumer surface; benignity-certification family escalated; prior 7-file census was head-truncated
**Files:** ISSUES.md, audits/2026-06-10_oq94_readsite_pass/READSITE_PASS.md, prolog/drl_core.pl, prolog/maxent_classifier.pl, prolog/signature_detection.pl, python/issues_status.py
**Tier:** correction-key

The OQ-94 per-site decision rule (ruled 2026-06-10) was applied to the full consumer surface.
**Census correction first:** the recorded "seven-consumer list" was `head -15`-truncated — the
untruncated census finds **12 files / 33 sites**, and the concealed ones were the most
load-bearing: `drl_core.pl:346` (scaffold clause) and `:373` (tangled_rope clause) in the
classification cascade itself, plus the `maxent_classifier` boolean_spec mirror and
`omega1_audit`. A probe-scope statement must name its output limits. **Sort result:** SOUND = the
four NL/FSM mountain-likeness gates (beneficiary presence already disqualifies; capture is
stronger evidence, same direction). FORBIDDEN = the tangled_rope cell (`drl_core:373` + maxent),
decay detection (`drift_events`, `transition_paths`), `separability_factor`, and two NAF-voids
(`logical_fingerprint:226,444`) that would FALSE-FIRE on captured constraints under a gate.
**ESCALATED (the one rule-unsorted family): benignity certification** — `drl_core:346` scaffold
clause (+ maxent scaffold spec) and `signature_detection:1019` CI_Rope gate ask "is this benign
coordination?", a third question; gate-on-not-captured there is plausibly correct (it is the
prototype's witnessed scaffold-push mechanism) but is the operator's call. Step-3 preregistration
carries TWO operator questions: diffuse tolerance + benignity-family ruling. Bonus finds:
`constraint_bridge.pl:96` is the first gain_flow-migration candidate;
`data_repair.pl:124-168` FABRICATES `constraint_beneficiary` from metrics on the DR-AUDIT path
(OQ-93 circularity). Estimator-classifier congruence: any `drl_core:346/:373` ruling must land in
maxent's boolean_spec table in the same change. Also this session: `issues_status.py` now fails
on duplicate OQ labels (pre-fix a duplicate entry was silently invisible — witnessed), and the
worktree rule is unconditional (CLAUDE.md). **Step-3 rulings landed (operator, same day): Q2
rows 1+3 GATE (scaffold clause + maxent mirror; pure_coordination subtype), row 2 deferred→
control RUN: synthetic vectors can't reach Boltzmann-gated signatures
(`inconclusive(insufficient_classifications)` — diagnosed), and the live-corpus existence check
witnessed CI_Rope ∧ beneficiary = 7/7 (gate runs entirely on beneficiary-bearers; captured-or-not
unknowable until gain_flow exists). Q1: K=0 on the observable, halt = Stage D only, N =
whole-batch-or-≥30 (convention), obviousness criterion pre-written, "0/N observed" never "clean".
Fabrication ban recorded (gain_flow never synthesized; data_repair.pl the named door). STAGES
A–C UNBLOCKED — schema → compiler → prompt per
`audits/2026-06-10_oq92_step3_preregistration/PREREGISTRATION.md`.** Row 2 then RULED GATE
(family gate-uniform; evidence-shape distinction preserved: row 1 misfire-witnessed, row 2
reachability-witnessed/misfire-pending-Stage-D — deferral would have inverted fail-closed).
**Stage A + Stage B LANDED same day** (schema fields + compiler emission + fail-loud
referential integrity + narrative_ontology declarations; witnesses in the prereg dir: 8/8
schema cases, two-sided additivity, 0-diff 134/134 old-vs-new, pilot branches incl. ghost-seat
REJECTED on both paths, swipl fact queryability). Standing fact with a number: **91/134
`json/` specs fail the CURRENT schema** — identical pre/post Stage A, the expected residue of
the 2026-06-09 required-fields tightening; latent (run_pipeline does not read `json/`; the
generator validates on entry) but a known surprise if old specs are recompiled or used as
fixtures. NEXT HUMAN GATE: the diffuse-audit "obvious capturing seat" criterion is written
BEFORE the first Stage-C batch is read (prereg Q1; operator-in-loop by design); Stage C prompt
work and everything else between is execution.

## 2026-06-10 — OQ-81 ruled SUPPRESS and wired: reading-typed wave-upstreams dropped at seed build; A/B finds verdict import in the gradable channel (theater_ratio), absorbed before the categorical
**Files:** agent/generate_kernel_corpus.py, agent/c-orchestrator.py, agent/story_generator_base.py, ISSUES.md, audits/2026-06-10_oq81_reading_upstream_recon/
**Tier:** landed

Full chain in `audits/2026-06-10_oq81_reading_upstream_recon/` (RECON → AB_PLAN pre-registered →
AB_RESULTS → WIREUP): recon established ZERO exposure to date (no story in any corpus was ever
generated under reading-verdict injection — pre-merge c-orch dropped readings, gkc --scope is
wave-free, post-merge live runs had no reading edges) and that the current SCOPE format emits
kernel-CONCEPT deps (21/21 dangling/inert), not reading deps. The A/B (3 arms × 3 reps, exact
pipeline params, injected verdict deliberately ≠ axis hypothesis): claimed_type held 9/9 snare,
but the three-line verdict block pulled authored theater_ratio 0.690→0.513 (zero range overlap;
kernel-substrate arm ≈ no-context arm). Operator reframe adopted as the closure language:
**verdict import occurred in the gradable channel and was absorbed before the categorical one**
— the categorical field is STICKY (anchored by the explicit hypothesis line), not safe; the R-arm
prose reasoning about theater is the positive control proving the injected verdict was read, so
the categorical null is real. Discovered en route: `axis_source_desc` already injects the
verdict-free kernel CSR into every supplementary-axis prompt — kernel substrate needed no new
wire; the fix-space collapsed to one bit. Wire: `_flat_seeds_from_manifest` drops reading-typed
deps from BOTH the seed's wave deps and the axis copy `upstream_context` reads (two read sites,
one filter point); same predicate in the serial escape hatch (code-read sync, NOT
payload-witnessed). Witness: germline byte-identical (8 flat injections preserved — §5.1 flat
design untouched); dutch+supp kernel capture 4/5 payloads identical, 5th loses exactly the three
verdict lines. Standing cautions (also in the compressed OQ-81 entry): (1) **injection channel
asymmetry** — categorical-stable / continuous-distorted is a general finding about context
injection (n=3, one axis: an instance, not an effect size); (2) the CSR line poisons
vocabulary-based leakage probes in ALL arms — key future leakage probes on tokens present ONLY
via the injected block.

## 2026-06-10 — OQ-77 closed: giant_comp SIGSEGV not serially reproducible (10/10 at exact crash size n=39; archives to n=3380) — concurrency artifact, operational rule promoted; OQ-95 filed (phantom network nodes)
**Files:** ISSUES.md, CLAUDE.md, prolog/giant_component_analysis.pl, prolog/drl_purity_network.pl, python/run_pipeline.py, audits/2026-06-10_oq77_serial_kill_condition/writeup.md
**Tier:** landed

OQ-77's pre-registered kill-condition executed (`audits/2026-06-10_oq77_serial_kill_condition/`):
serial 10/10 rc=0 at n=39 (the exact crash size; outputs byte-identical), 12/12 rc=0 under 12-way
co-residency, and serial archive runs at kernel_v1 n=1106 + original_v6 n=3380 ×3 (byte-identical
complete reports; 8,785-node component BFS). No serial recurrence ⇒ resolved as a concurrency
artifact per the kill-condition; mechanism inside the concurrent regime stays unidentified (pure
co-residency ruled out; mutating prep-interleave unsimulated; exact crashing corpus
unreconstructible). Operational rule promoted to CLAUDE.md Running the System: one pipeline at a
time against shared testsets/+outputs/ (within-pipeline parallelism fine). Reopen path: any
SERIAL segfault → kill-condition's "recurs serially" branch, this audit as baseline.

Side-finding → **OQ-95**: giant_comp's component BFS counts dangling `affects_constraint/2`
targets as nodes — 25 phantom atoms on the live corpus (component = 118.9% of network), ~2.6×
on original_v6 (259.9%). Node enumeration is corpus-scoped; edge discovery
(`drl_purity_network:constraint_neighbors/3`) is not. Probe positive-controlled against the
report's own edge count (75). Census other `constraint_neighbors/affects_constraint` consumers
before picking the fix point.

## 2026-06-10 — OQ-92 rulings recorded + step-2 gain-flow prototype PASSED 8/8: capture and fixing_cost separate on authored fields; step-3 surface build unblocked (OQ-92/OQ-90/GAP-10)
**Files:** ISSUES.md, docs/design/design_gaps.md, audits/2026-06-10_gain_flow_prototype/PREREGISTRATION.md, audits/2026-06-10_gain_flow_prototype/FINDINGS.md
**Tier:** landed

Operator rulings recorded (commit `4e04c2dc`, amendments landed BEFORE the rulings since recorded
rulings become precedent text): **(a)** build the authored gain-flow surface, prototype-first
(OQ-93 precedent); **(b)** ONE authoring surface, TWO distinct fields (gain_flow + fixing_cost),
justified on design grounds — the draft binary-bit argument ("one scalar can't encode two cuts")
was reviewed, found false as an information claim, and recorded as rejected in the OQ-92 Rulings
block to prevent re-citation. Tri-valued provenance design ruled: authored-gain-to-NAMED-seat /
explicit-`diffuse` / absent-fails-closed — with the trap named that NAF over authored fields is
authored-absence in disguise (uncaptured must be authored positively). Malformed-gain
(gain_flow → nonexistent seat) DECIDED to absorb into fail-closed at runtime, with a step-3
schema-rejection validation item so the absorption never hides a data error.

Step-2 prototype (preregistration committed `eb24a927` before the run): eight-control battery,
both fields hand-authored, prototype-only predicates, no production files. **Outcome 1 PASS,
8/8 as pre-registered.** Positive-control pairs held: 2↔7 (diffuse fires on the twin, making
absent's silence a witness) and 1↔8 (the `role_of/3` join fires on an existing seat, making the
malformed silence the absorption witness). Case 5 vs 4 (seat-identical, only `fixing_cost_class`
differs) **witnessed fixing_cost as load-bearing** — OQ-90's decisive pre-wiring control,
discharged. Under-claim holds: cases 1–6 are near-tautological as logic tests; the run witnesses
separation on these constructed cases, the join in both directions, and coherent authorability —
NOT corpus-range representability or generation-side honesty (that is the step-3 diffuse-audit
gate: hand-audit a pre-stated-size sample of generated `diffuse` claims with pre-stated tolerance
BEFORE the field drives classification — authored-diffuse is an authored universal negative with
no checkable witness, and OQ-70 is the template-convention precedent). Post-run promotions
(operator): the prototype's one production-engine touch — capturer seats computing **scaffold**
via `constraint_beneficiary/2` → `has_coordination_function/1` — homed as **OQ-94** (the same
fact-family will make opposite-direction calls once `seat_captures` wires into classification;
wide consumer surface incl. the Boltzmann/FCR coordination axis; collision structural since the
OQ-83 compiler derives constraint_beneficiary from role `beneficiary`); and the diffuse-gate
**tolerance/sample size RESERVED as an operator ruling at step-3 preregistration time**, not a
drafted default. Next forward move: OQ-92 step 3 = schema field + compiler emission + prompt
change per the OQ-83 Phase-A playbook — preregistration must carry both preconditions AND name
OQ-94 as known-interference.

## 2026-06-10 — OQ-57 re-witnessed post-reset: resolution holds; original behavioral witnesses were pre-reset/corpus-specific, now superseded by a corpus-independent positive control
**Files:** prolog/drift_events.pl, ISSUES.md, audits/2026-06-10_oq57_live_rewitness/FINDINGS.md
**Tier:** correction-key

OQ-57 (drift report threw on a missing `requires_active_enforcement/1` qualifier) was resolved
2026-06-04, but **all three behavioral witnesses ran on the corpus reset 2026-06-05** — they
describe constraints that no longer exist. Re-witnessed across live + archives:
- **Code fix durable** (`drift_events.pl:236`, `domain_priors:` qualifier). **Diagnostic positive
  control:** the pre-fix `narrative_ontology:` qualifier still throws `existence_error`, the fixed
  one resolves — the qualifier change is load-bearing, the probe is not vacuously clean.
- **Original emitter set reproduced exactly** on `kernel_v1` (1,106): `{kodashim_obligation__memorial_archival,
  statutory_debt_ceiling__constitutional_nullity_reading}` both fire CLEAN; `kodashim` →
  `evidence(extraction,0.08,theater,0.85)` byte-identical to the 2026-06-04 record.
- **Corpus-independent synthetic positive control** proves the clause fires when its guard is
  reached regardless of corpus content — the witness the original entry lacked.
- Full `drift_event/3` scan threw on **0 of 4,525** constraints across live(39)+kernel_v1(1,106)+
  original_v6(3,380); `run_dynamic_suite` live = 39/0/0.

**Tripwire carried:** the `internalized_piton` clause is currently **UNREACHED on the live
39-constraint corpus** (correct-but-dormant). A future "no drift throw on the live corpus" read
must not be mistaken for "exercised" — it is the Pattern-5 vacuous pass until a low-extraction/
high-theater constraint re-enters the rebuild. Not promoted (corpus-state-specific, self-resolving
as the rebuild grows); recorded so the next reader checks reachability before claiming exercised.

## 2026-06-09 — OQ-93 opened + mitigated: imputation shim diagnosed (unmigrated v3.4 grid contract) and made visible via three-bucket provenance threading
**Files:** prolog/data_repair.pl, prolog/scenario_manager.pl, prolog/test_harness.pl, prolog/intent_engine.pl, prolog/report_generator.pl, ISSUES.md, audits/2026-06-09_imputation_shim_census/census.md
**Tier:** landed

- **Class diagnosed (census: `audits/2026-06-09_imputation_shim_census/`).** The `[FIXED]
  Imputed 24–28 missing vectors` lines in every constraint report are an **unmigrated consumer
  contract**: the DR-AUDIT harness enforces the archived prompt-era 32-point leveled grid
  (incl. `stakes_inflation`, which greps to `prompts/archives/` only — positive control
  `suppression_requirement` fires in live schema+prompt), while the live schema's
  `MeasurementMetric` enum is `{theater_ratio, base_extractiveness, suppression_requirement}`,
  unleveled. **Empty intersection: 0/32 grid points authorable, ever, corpus-wide.** Sibling of
  the `mandatrophy_resolved` severance (OQ-83 A7, same JSON migration).
- **Blast radius:** shim fires only via `scenario_manager:load_and_run` (reports + validation
  suite); main pipeline / `pipeline_output.json` authored-fed. **MaxEnt confidences are
  authored-fed (scalar)** — the "0.95 over invented vectors" caveat was overstated; the
  fabrication-fed products are `[INTENT]` (only `stable` reachable; Confidence `high` derives
  from the imputer's own 8/8 completeness), the verification gate, and κ.
- **Phase 2 landed (visibility-only, witnessed):** `data_repair:grid_provenance/2` +
  three-bucket `[PROVENANCE]` line (authored / injected-0.5 `m_gen` / imputed `repair_m_*` —
  a binary split would launder injection into "authored", operator correction); stray-anchor
  `[WARN]` (injection hardcodes t=[0,10], ignoring the interval); diet flags on
  `[INTENT]`, report header Pattern/Confidence, and κ. Witnesses: report regen diff =
  provenance-lines-only (κ 0.39 and all classifications byte-identical); store-count probe
  matches `prov(0,4,28,0,32)` for transfer_gap_physics; `run_dynamic_suite` 0 errors /
  0 warnings after.
- **Unruled fork (OQ-93):** producer-side vs consumer-side migration completion. Adjudication
  constraint: every grid output ever produced was prior-flavored, so "unique product" is
  unanswerable from existing reports — "wire" requires a prototype with hand-authored grid data
  first.

---

## 2026-06-09 — OQ-80 + OQ-08 closed: generate-step token totals threaded (hard-0 retired); DR/CS Π-asymmetry annotated in both mismatch report layers
**Files:** agent/generate_kernel_corpus.py, agent/c-orchestrator.py, prolog/json_report.pl, python/enhanced_report.py, python/tests/test_token_acc_threading.py
**Tier:** landed

- **OQ-80 resolved.** `process_batch_results` gained an optional `token_acc` mutable out-param
  (None = NOT measured, never 0; return signature intact for gkc CLI callers); usage summed at
  receipt (spend is real even when the story later fails parse/validation);
  `generate_from_manifests` forwards per wave; `_step_generate` now reports real token counts on
  the StepResult instead of the hard 0 + "unthreaded (OQ-80)" note. Witness:
  `python/tests/test_token_acc_threading.py` — summed-at-receipt-incl-parse-failures,
  errored-only→0 negative control, and None-path-unchanged all pass (2026-06-09).
- **OQ-08 resolved.** When `cs_drift_mismatch` fires, `json_report.pl` emits
  `cs_drift_mismatch_note` and `enhanced_report.py`'s kernel-reading section appends the note:
  Π-asymmetric by design — DR instance-blind at the fixed analytical context, CS context-free
  authored facts; cross-frame disagreement, not two answers to one question. Witnessed both
  directions on each layer (Prolog: kernel_test archive, firing UID note+parses / silent UID no
  note; Python: mock-pipeline, note iff mismatch). Eventual permanent home: the OQ-15 mediator.

---

## 2026-06-09 — Three doc-sync OQs closed with witnesses: OQ-07 (mismatch candidate runtime-probed SILENT, blocking conjunct named), OQ-28 (seat-theorem amendment provenance), OQ-14 (bridge unblessed; mediator is the decided join)
**Files:** ISSUES.md, docs/seat-theorem-v1.md, docs/design/two_axis_architecture_v7.md, prolog/cs_drift_mismatch.pl
**Tier:** landed

- **OQ-07 resolved.** `cs_drift_mismatch/2` runtime-probed for the hand-traced UID `72c8aa61…`
  on the only corpus carrying it (`archives/datasets/kernel_test`, 229 testsets; UIDs are
  per-generation surrogates — same-named archive copies differ). Positive control: 11
  corpus-wide firings on the same load. Candidate: SILENT; decomposition shows the
  foreclosure half HOLDS (`axiom_foreclosure_trajectory`) and `cs_is_metric_stable` FAILS —
  runtime falsified exactly the hand-trace's unverified metric-stability assumption.
  Verdict: architecturally-possible-but-not-this-case. Evidence:
  `audits/2026-06-09_oq07_mismatch_runtime_probe/` (probe.pl, probe_output.txt, WRITEUP.md).
- **OQ-28 resolved (option a, as the entry pre-ruled).** `docs/seat-theorem-v1.md` gained an
  "Amendment provenance" section naming the witness-asymmetry: the §3 correction is a
  result-claim carrying its run-witness (`test_forecloses_fpn_injection.pl`); the §5 and §8
  edits are scope-clarifications owing declaration, not run-grounding.
- **OQ-14 resolved.** `docs/design/two_axis_architecture_v7.md` amended (2026-06-09 section):
  the `influences` bridge is no longer the one blessed cross-axis join (16 cross-axis
  surfaces in 7 modules); the OQ-15 mediator layer is the decided-but-unbuilt join; three
  grep-enforceable invariants recorded; four stale claim-sites corrected in place.

---

## 2026-06-09 — Capture-cut discriminating control HALTED (Outcome 2): `has_computed_capturer` proxy false-positives; capture needs an authored gain-flow surface (OQ-92 / GAP-10, gates OQ-90)
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/stakeholder_seats.pl, prolog/constraint_indexing.pl
**Tier:** correction-key

Ran the pre-registered Step-1 control for OQ-90's proposed capture cut (`has_computed_capturer/1` =
beneficiary-side seat with favorable `dr_type_for_stakeholder`) against four seat-sets. **Outcome 2 →
HALT:** the cut fires TRUE on a *mild-favorable non-capturer* (two-part witness: candidate-set
membership TRUE *and* cut TRUE on a seat with no `constraint_beneficiary`) and on an uncaptured
designed DMV's agenda_setter. Root cause: χ (`extractiveness_for_agent_d/4`) is
**extraction-from-seat, not gain-to-seat**, and every beneficiary-side role gets low `d`
(`config.pl:156–160`) → favorable type regardless of receipt; the cut degenerates into "C has a
beneficiary-side-*role* seat at all." Bonus: `constraint_beneficiary/2` (the only authored signal
nearby) feeds `has_coordination_function/1` (`narrative_ontology.pl:303`) → pushes a capturer toward
*scaffold*, the wrong way. **Capture is not computed-representable from current signals;** needs an
authored gain-flow / receipt surface (OQ-92, GAP-10; proposed — for operator ruling — to possibly
unify with OQ-90's `fixing_cost` term, flagged not folded). OQ-90 Steps 2–4 (piton refinement,
`Supp ≤ 0.2` gate retirement) stay gated on OQ-92; the proxy is NOT shipped. Pre-registration written
before the run; favorable-set choice shown irrelevant (problem is upstream in d-derivation).
Witnesses: `audits/2026-06-09_capture_axis_cut_control/` (PREREGISTRATION.md, FINDINGS.md,
step1_capturer_cut_control.out, capturer_cut_control.pl).

**Promotion test:** no silent-mistake tripwire — this corrects how a *prior proposal* may be cited
(the capture cut is rejected, not pending), which is correction-key, not an always-loaded warning;
the χ-is-extraction-from-seat fact is already in the cut's own comment in `stakeholder_seats.pl:86–88`
and now in GAP-10/OQ-92. Stays history-side, not promoted to CLAUDE.md.

---

## 2026-06-10 — Cell controls (witnessed): snare is capture-blind (`Supp ≤ 0.2` is not the piton discriminator); coordination "non-rope" cases scatter (FSM vs FCR) — falsification-grade; rebuild held (OQ-90/OQ-91)
**Files:** prolog/signature_detection.pl, docs/repair_dynamics.md, ISSUES.md
**Tier:** correction-key

Three pre-registered cell controls settled two theory claims **before** they landed (the reason for
running them first: the design-axis 2×2 reasoning had called a table coherent that the witness then
falsified). Witnesses: `audits/2026-06-10_signature_liveness_crosscorpus/{dmv_cell_control.out,desirepath_cell_control.out}`.

- **DMV** (designed, enforced **Supp 0.5**, distributed extraction, **no concentrated capturer**) →
  **snare** (`constructed_high_extraction`). A high-suppression *uncaptured* extraction reads as
  snare ⇒ **`Supp ≤ 0.2` cannot be the piton discriminator** (live `piton_signature` gate +
  prior `repair_dynamics.md` §4 both corrected) and **snare is capture-blind**. Capture and
  suppression are **separable on this witness**; "orthogonal across the range" is the opened
  hypothesis, not established. Pre-registered criterion ("DMV in snare ⇒ axis is capture") met.
- **Coordination side scatters** (pre-registered: FCR ⇒ shared home/not-scatter; not-FCR ⇒ scatter):
  undesigned/emergent coordination (`emerges_naturally` + agent-beneficiaries, low ε) →
  **`false_summit_mountain`**; designed-but-unmaintained coordination → **`false_ci_rope`**. Distinct
  cells ⇒ **"emergent coordination = one cell / piton's mirror" FALSIFIED (scatter)**. Emergent
  coordination → **FSM**, but **home-vs-shadow is OPEN** — whether FSM correctly absorbs it or is the
  lossy place it lands for lack of a proper cell (subsumption vs under-naming, same shape as the piton
  question) is not settled by this probe; it showed the cell non-empty, not that FSM is the right home.

**What is NOT yet established:** "orthogonal across the range" (one witness gives separable, not
independent everywhere). **What is held for operator go (construction, not deletion):** the
capture×coordination rebuild, the per-seat-χ no-capturer detector, the `Supp ≤ 0.2` gate fix, the
environment/perturbation variant. Method note: each control was **pre-registered** before the bash
call so the result couldn't be narrated into agreement — the standing fix for axis-introduction (a
new/relabeled axis owes a pre-registered discriminating control; the DMV is the template; see
build_discipline "false-unification"/memory).

## 2026-06-10 — Piton: agenda_setter is a BETTER proxy (the fixer role), but extraction<fixing_cost stays uncheckable; build as computed false_ci_rope refinement — OPEN pending the fixing_cost control (OQ-90)
**Files:** prolog/signature_detection.pl, prolog/stakeholder_seats.pl, prompts/constraint_story_generation_prompt_json.md
**Tier:** correction-key

Resolves the piton arm of the cross-corpus dark-signature finding (this same day's entry). Two
in-conversation overreaches corrected, both from incomplete recon (the failure the *"witness before
claiming"* / *"unwired ≠ worthless"* disciplines warn about; operator's DMV worked-example + the
agenda_setter pointer were the positive controls):
1. **NOT "operationalization invalid / resistance-sign inverted."** A piton has HIGH resistance
   (people complain) — the gate's `resistance > 0.2` is correct. What's absent is the *fix*. The
   gate (low enforcement + resistance + theater + evolving) is a lossy *symptom-proxy* of the
   cost-asymmetry, not backwards.
2. **NOT "fully representable / no new design" either — the headline overshot (corrected after
   Claude-web push-back).** The fixer exists as **`agenda_setter`** (d=0.12) over distributed
   `payer`s (d=0.85), authored + populated (22/57) — but that encodes only **"the fixer isn't much
   hurt,"** NOT the comparison `extraction < fixing_cost`. The piton condition has two terms; the
   proxy carries one. It is lossy in both directions: **misses** a moderately-hurt fixer for whom
   fixing still isn't worth it (canonical collective-action piton, moderate d), and
   **false-positives** transient neglect (low-d fixer + a cheap fix nobody's done yet — not a piton).
   So `fixing_cost`/benefit-of-fixing is **potentially load-bearing, not deferred**; "representable
   via the stakeholder layer" is **OPEN**, gated on the cheap-fix-not-done positive control (OQ-90),
   not a finding. What actually improved across the thread was proxy quality (theater_ratio →
   stakeholder structure); the mechanism is still not directly checkable.

**Design (operator-ruled 2026-06-10; full spec + drafts in OQ-90):**
- Piton ⊂ `false_ci_rope`, refined **in-branch** (no cascade reorder; piton is FCR-shadowed because
  its low ε trips `appears_as_rope` and FCR fires at priority 2 before the profile fallback).
- Snare implies a capturing beneficiary → keep piton OUT of snare; the split turns on *capture*.
- **The no-capture test is COMPUTED (per-seat χ), never authored beneficiary-absence** — gating on
  "no beneficiary authored" would be a Pattern-5 regression and violate OQ-83 R3 (authored absence
  must not drive classification). Idiomatic here: `in_contention`/`consensus_provenance` are
  computed-not-authored.
- Prompt fix is **non-leaky**: guide authoring of `agenda_setter`/`payer` roles + the cost-asymmetry
  qualitatively; DROP the `theater_ratio ≥ 0.70` recitation (threshold-leakage = tuning-to-target,
  same class as the 0.5 default). Theater becomes an honest-if-present symptom, not the test.

**Tripwire:** when building OQ-90, verify the `chi_for_stakeholder/3` sign convention before writing
`seat_captures/1` (capturer = beneficiary-side seat whose computed χ shows real gain); positive-
control on a constructed DMV seat-set (piton) vs a capturing seat-set (snare) before wiring.

## 2026-06-10 — Cross-corpus signature-liveness sweep: 7/12 signatures LIVE, 5 dark everywhere; the fail-closed fix makes archive sweeps runnable (OQ-89)
**Files:** prolog/signature_detection.pl, prolog/corpus_loader.pl, audits/2026-06-10_signature_liveness_crosscorpus/
**Tier:** correction-key

Corrects the naive read "8 signatures don't fire on the live n=34 ⇒ dead." Ran the current
`signature_detection:constraint_signature/2` across four corpora via `corpus_path` overlay
(retract default → assert `archives/datasets/<x>` → `load_all_testsets`; non-recursive glob =
top-level only). **0 throws on all four** (live 34, kernel_v1 1106, original_v5 702, original_v6
3380; bucket sums equal loaded counts) — the 2026-06-09 fail-closed fix is what makes this safe:
old under-vectored stories abstain to `unknown` instead of throwing. Matrix + provenance:
`audits/2026-06-10_signature_liveness_crosscorpus/MATRIX.md`.

- **7/12 signatures fire somewhere** ⇒ LIVE: false_ci_rope, coupling_invariant_rope,
  constructed_high_extraction, **natural_law** (404 on v6 / 26 on kernel_v1 — zero on live),
  **false_summit_mountain** (kernel_v1+v6 — zero on live), **false_natural_law** (15 on v5 only).
  The three bolded were zero on live → resolved **live-but-narrow**, not dead.
- **5 DARK across all ~5,222 stories:** `coordination_scaffold`, `piton_signature`,
  `constructed_low_extraction`, `constructed_constraint`, `ambiguous`. Strongest cruft-candidates
  but NOT a verdict — per CLAUDE.md *"Unwired ≠ worthless"*, firing-anywhere is evidence feeding the
  value question, not the answer. Next discriminator: the reference-exemplar control
  (`constraint_instances.pl`: SI-units→scaffold, QWERTY→piton) + what each would detect. The three
  constructed_*/ambiguous are intermediate/fallback bands (corpus data lands in constructed_high or
  is overridden) → narrow-data, not proven dead-code.
- **Consistency checks:** `natural_law`=404 on original_v6 reproduces the OQ-43 "404 NL on
  testsets_3000" figure; `false_natural_law`=0 on kernel_v1 (despite OQ-70 recording FNL-dominance
  on its ancestors) corroborates that the OQ-70 bait-clause removal worked.
- **Caveat:** counts are liveness, NOT prevalence — archives are bait-era/ID-reuse and 67–81%
  abstain under current schema.

**Tripwire:** to sweep an archive, overlay `corpus_path` (retract the default `param/2` first — it's
dynamic, first solution wins) to `archives/datasets/<x>` and call `load_all_testsets`; the
non-recursive glob skips run-tag subdirs. Do NOT cite archive firing RATES as corpus content (OQ-70
bait, OQ-25 ID-reuse, schema-drift abstention).

## 2026-06-09 — `accessibility_collapse`/`resistance` now REQUIRED for all constraint types; `get_metric_average` fail-closes to `unknown` (was 0.5); 3 articles regenerated (OQ-89)
**Files:** prolog/signature_detection.pl, schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md, python/generate_constraint_pl.py, agent/c-orchestrator.py
**Tier:** landed

Root cause (audit `audits/2026-06-08_coordination_washing_clean_pass/`): generation never authored
`accessibility_collapse`/`resistance` for non-mountain constraints; `get_metric_average/3` defaulted
the missing vectors to **0.5**, which exceeds `snare_epsilon_floor` (0.46) — so an extraction-less
constraint fabricated `constructed_high_extraction` from no data, and the 0.5 fill was load-bearing
for the throw the audit removed.

**Landed (witnessed; evidence under the audit's `rebuild_evidence/`):**
- **Schema** (`constraint_story_schema.json`): `accessibility_collapse` + `resistance` added to
  `base_properties.required`; rejects each independently (V1 witnessed). `_basic_validate` fallback
  in `generate_constraint_pl.py` made consistent (else jsonschema-absent path silently skips them).
- **Prompt**: both promoted to Core-required-for-ALL-types with honest non-mountain guidance
  (mountains high collapse/low resistance; snares lower collapse/higher resistance). `emerges_naturally`
  stays mountain-specific.
- **Engine** (`signature_detection.pl`): `get_metric_average` empty branch `0.5` → `unknown`; added
  abstain clause `constraint_signature(C, unknown) :- \+ profile_metrics_authored(C), !`; `number/1`
  guards on `natural_law_signature`/`coordination_scaffold_signature`/`piton_signature`/
  `constructed_constraint_signature` + a `profile_numeric` gate on `signature_confidence` so absence
  **fails-closed (abstains), never throws**. Witness: 0 throws across the corpus + probes; the
  fully-vectored constraints classifiable pre-guard are byte-identical post-guard (anti-over-abstain
  control); under-authored constructed_high → `unknown`.
- **Regenerated** magnifica_humanitas, china_blue_collar, world_model3 via c-orchestrator
  (`DR_TEMPERATURE=0`, `--skip-search` — web search hung ~3.5min on the API in-env; research grounding
  doesn't affect metric authoring). All 16 regenerated *stories* author both metrics. **V5 deterministic
  substitution (`probe_harness:with_overlay/3`, caches auto-cleared): B(swap metrics→0.5)==C for all
  16** → the formerly-defaulted metrics do not move these (extraction/suppression-driven) verdicts;
  fix value is structural, not a verdict change.

**Tripwire / residuals (OQ-89):**
- **Full re-run RE-DECOMPOSES into different axes** — not "same stories +2 metrics." world3 went
  3→4 axes with only `proxy` overlapping; magnifica 11→6; china →5. Old testsets are **orphaned**,
  left in place (operator ruling 2026-06-09). 9 corpus members now abstain to `unknown`: 2 are
  `*_contradictions` axiom meta-files (not stories — correct), 7 are orphaned originals
  (e.g. `war_normalization_ai_weapons` superseded by `war_normalization_autonomous_weapons`). Corpus
  n=34 carries orphan+replacement duplicate coverage until a cleanup pass.
- **Legacy corpus not retro-fixed:** ~94/116 historical `json/` files still lack the two metrics;
  the schema requirement binds future generation only.
- **Class generalization deferred** (narrow-scope ruling): the neutral-default-crosses-threshold
  pathology (0.5 > floor) may recur for other `get_metric_average`-style defaults / metric-threshold
  pairs — see OQ-89, cross-ref OQ-43/44.

## 2026-06-08 — Flat router stably under-routes a COUPLED methodological kernel (World3); false-mountain (mountain→rope) is a candidate missed-kernel signal (OQ-88)
**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, ISSUES.md
**Tier:** correction-key

First end-to-end `c-orchestrator.py` runs (kernel-first branch) audited against prior corpora and
essays. Routing discrimination works as designed: china wage-convergence + World3 → FLAT (no
`cs_structure`); magnifica AI encyclical → KERNEL (5 readings with `cs_structure.reading_relations` +
axioms; the seat layer reaches the essay — "Four Irreconcilable Frameworks", axiom contradictions,
foreclosure graph — which the pre-modification `magnifica_humanitas_ai_encyclical_original_run.md`
structurally could not produce). **But the flat path has a witnessed blind spot.** Comparing the
pipeline's `world3_recalibration_2024.md` to a thesis-driven web-Claude critique ("The Robustness Is
the Tell") surfaced a load-bearing seat — the policy REGIME (collapse is mountain-within-BAU,
rope-across-regimes; Stabilized-World = positive control) — that World3 never seated. Re-ran
`--dry-run --run-tag world3_kernel_probe` on the same source: **stably FLAT across 2 samples**
(`outputs/kernel_manifests/flat/…171605` and `…/world3_kernel_probe/…183123`, 0 readings). The
re-roll emitted the robustness fact itself as a standalone axis (`parameter_sensitivity_structural_robustness`)
AND `collapse_timing_credibility` as a separate axis but **never coupled them** (robustness ⊥
forecast), filing the regime as omegas (`omega_earth4all_paradigm_shift`,
`omega_belief_system_change_mechanism`). The gate decomposes a coupled kernel into independent axes +
epistemic omegas, dropping the coupling that makes it a kernel.

**Engine-vocabulary finding (the actionable one):** the mountain↔rope type-divergence IS the
necessity-vs-contingency kernel question. `collapse_mechanism_ambiguity` classified authored=mountain
→ computed=**rope**, conf 0.01, `type_1_false_summit` severe — the engine adjudicated the seat SCOPE
never built. Second witnessed instance same run: `demographic_skill_mismatch` (china, flat),
mountain→rope, conf 0.01. ⇒ **OQ-88**: flat-routed ∧ false-mountain = candidate kernel false-negative;
N=2 = positive control; a negative control sweep (don't blanket-fire on authoring-flinch
false-mountains) is REQUIRED before it auto-routes (else it repeats OQ-79's kernel-liberal
over-routing one level up). Also logged in the magnifica run (separate, not yet OQ'd): one of the 5
readings (`technocratic_paradigm_resistance`) carries 3 DANGLING `cs_reading_relation` edges to
`*_ai_governance` sibling-ids that were never generated (naming drift; OQ-58 integrity sweep is
skipped on the no-scope/kernel path) and duplicates the magisterial reading's axioms — the essay
silently treats it as 4 readings, but the broken 5th is in the corpus. Provenance: this analysis;
detector design + control requirement in ISSUES.md OQ-88.

## 2026-06-08 — Register OQ-83 committer-stage-time / observer-residual fields in pipeline schema
**Files:** python/shared/schemas.py, prolog/json_report.pl
**Tier:** landed

`json_report.pl` emits four OQ-83 fields per constraint — `cs_reference_frame`,
`cs_drift_moment`, `cs_drift_gap` (committer stage-time, commit ef5a9188) and
`temporal_residual` (Type-A observer residual, de3736a6) — but `PIPELINE_FIELDS` in
`python/shared/schemas.py` never listed them, so the drift detector printed
`[WARN] unexpected field: …` for every one on every constraint across every pipeline
tier (~280 lines/run; visible in the world3 orchestrator run). Added the four as
nullable declarations (str/str/dict/dict), grouped with their CS-UID siblings and the
temporal-trajectory block. Nullable ⇒ absence/null permitted, present values typed.

**Witness:** `PYTHONPATH=python` → `validate_pipeline_output` and
`validate_enriched_pipeline` on the on-disk artifacts both return **0 errors, 0
warnings** (was: 4 drift warnings × every constraint). No new type-validation errors.
Producer side (`json_report.pl`) unchanged — schema caught up to the emitter, not the
reverse.

## 2026-06-08 — make_brief: source-abstraction tool for oversized/refusing inputs (canonical llm_call; measured ingest ceiling; STOP-by-default refusal)
**Files:** agent/llm_call.py, agent/make_brief.py, agent/c-orchestrator.py
**Tier:** tripwire

Built a reusable brief tool so big/refusing source docs (spacex_s-1.txt 1.6 MB; the
PIIS vaccine paper that flat-refuses on Sonnet) can feed the orchestrator. Three pieces:

- **`agent/llm_call.py` — the ONE canonical Anthropic call path.** `get_client`,
  `call_with_retry`, `extract_text`, `count_tokens`, `context_window`, and
  `ModelCallError` (now carries `stop_reason`/`model`/`refusal_text`). `c-orchestrator.py`
  imports these; its `_call` is a thin wrapper. Consolidates the fix-#1 refusal detection
  (commit 7e85b261) into one spot so it cannot fork (Build Discipline pattern 2). NOTE: the
  orchestrator filename's hyphen blocks normal import — new callers import `agent.llm_call`,
  never the orchestrator.
- **`agent/make_brief.py` — NEUTRAL structural compression.** Emits MAIN IDEA / SOURCE'S OWN
  FRAMINGS / KEY FACTS / WHAT IS CONTESTED, and does NOT pre-partition into named READINGS
  (keeps primed SCOPE's kernel call un-anchored). Map-reduce over `SINGLE_PASS_BUDGET_CHARS`
  (~250 KB): Haiku maps chunks, Sonnet reduces. CLI: `python3 agent/make_brief.py f.txt`.
- **Orchestrator triggers (asymmetric, by design).** SIZE → auto-brief, but only when the
  topic exceeds the **MEASURED** ingest ceiling (`_ingest_decision`: window − step
  scaffolding − reserved − margin, min over research+decompose; **decompose binds** because
  the raw topic is packed only by research+decompose — generate works from the manifest).
  REFUSAL → **STOP by default** with a manual-route message (schema + scope prompt +
  build_prompt pointers); `--auto-bypass-refusal` is opt-in and logs the witness (refusal +
  the reframing that got it through), never a silent classifier bypass.

**Tripwires for a cold reader:**
1. **A brief is LOSSY — never feed one when the doc would fit whole.** The measured ceiling
   (~175K tok for decompose) deliberately sits far above the old asserted ~120 KB idea:
   witnessed spacex (~417K tok) briefs, but **magnifica (267 KB / ~69K tok) feeds WHOLE**
   (headroom +106K) — the old default would have needlessly briefed it (Phase-0: whole reads
   richer). Don't reintroduce a KB default below the measured ceiling.
2. **Neutral brief of a SINGLE-VOICE source under-routes to flat without research.** spacex
   S-1 is a prospectus (issuer voice only); the neutral brief faithfully says "no real
   contest… it is a prospectus." With `--skip-search` → SCOPE routes FLAT (8 risk axes,
   is_contested_kernel=None). WITH research grounding → recovers a contested kernel
   (`valuation_legitimacy`, 7 axes, readings dcf_fundamentalist/real_options_technologist/
   musk_cult_believer/governance_skeptic). **Research grounding is load-bearing for kernels
   from single-voice docs.** The hand-authored kernel-shaped `spacex_s-1_brief.md` (left
   untouched) imported external constituencies and routed kernel `dual_class_legitimacy` even
   without research — i.e. the two brief styles foreground DIFFERENT kernels (seat/framing-
   relativity), and a `--skip-search` manifest comparison is NOT apples-to-apples.

Verification (all witnessed this session): no-regression dry-run (no brief, manifest OK);
unit brief has no READINGS partition; measured ceiling (spacex trips, PIIS/magnifica fit);
map-reduce 44 chunks→6 KB brief, fidelity spot-checked against source (10:1 votes,
controlled-company, $41,311M deficit all present); PIIS default STOP prints manual route;
PIIS `--auto-bypass-refusal` succeeds on Haiku with logged before/after, fidelity confirmed
(DIOSynVax/S309/"not observed"/modest/baseline all in source).

## 2026-06-08 — Type-A snapshot floor + observer residual detector landed (time-aware d; ε-driven flips are NOT empty — 56/100)
**Files:** prolog/constraint_indexing.pl, prolog/drl_composition.pl, prolog/transition_paths.pl, prolog/temporal_residual.pl, prolog/json_report.pl, prolog/stack.pl, audits/2026-06-08_typea_template_extensibility/, docs/deferential_realism_paper_v7.md
**Tier:** landed

Pre-rebuild audit (`audits/2026-06-08_typea_template_extensibility/AUDIT.md`) then a **strict Tier-2,
schema-deferred** build of the Type-A (temporal) observer floor. The engine could express
classification drift over the authored timeline only through ε (both temporal classifiers read
time-varying ε from `measurement/5` but called `derive_directionality/3` with **no Time** — d
frozen). This build threads Time without authoring any time-indexed-d, and adds a read-only residual
detector.

**What landed (engine plumbing only; NO schema/template change):**
- `constraint_indexing.pl`: `derive_directionality_at/4` + deterministic `effective_time/3` (the C2
  frame_policy insertion point) + `:- dynamic time_indexed_directionality_source/4` (empty — the
  future C1 hook). Fail-closes to `derive_directionality/3`; **byte-identical on the current corpus**
  (no source facts).
- `drl_composition.pl`: `classify_at_time/5` surfacing `snap(D, Backed, Eps, Supp, Theater)` (the `/4`
  delegates; cs_kernel_registry + tests untouched). `Backed=false` flags the `:201` ε=0.5 fabrication
  and the STOPGAP scalar suppression — so phantom flips across real→fabricated transitions are
  excludable. Classification math unchanged.
- `transition_paths.pl`: `snapshot_type` `:130` swapped to `derive_directionality_at` (sync only, NO
  `backed` — it is default_context-only and nothing reads its backed).
- `temporal_residual.pl` (NEW, observer-only **category-B** seam diagnostic; reads NO `cs_`): per
  `(C,Context)` ran-witness (`times_examined`, `backed_times`) + flip composition — real flips only
  between adjacent `Backed=true` snapshots; type-changes touching a fabricated snapshot counted apart
  as `fabrication_adjacent_transitions` (a cross-metric hygiene counter, NOT signal). Emitted per
  constraint by `json_report.pl` (manifest-stamped via the single-writer pipeline).

**Finding (re-witness before citing): the residual is NOT empty on the current corpus.** 56/100
constraints show ≥1 backed flip; **155 counted flips** across the canonical contexts (e.g.
`ai_governance_accountability` at the analytical seat: scaffold→tangled_rope, t3→t6, Δε=0.05). Because
d is frozen on the current corpus (no time-indexed source), **every backed flip is observer-metric-
driven (ε/suppression/theater), not d-driven.** This contradicts the pre-build "expected empty" prior
and bears on the D-fork: substantial ε-driven flips at fixed role/d mean the cheap path produces
signal, so role-time-indexing (OQ-83 branch b) is NOT forced by emptiness.

**Bounds on the 155 (so it is not banked as an unqualified count):** |Δε| median 0.07, 120/155 > 0.05,
only 1/155 in the ≤0.02 jitter band → the flips track real ε movement, not boundary jitter; 150/155
flip-intervals sit on a fully-backed series. **Caveat (the classifier-sync OPEN, below): at the
default context — the only context with a second classifier — 2 of 52 counted flips touch a
snapshot_type-vs-classify_at_time disagreement point and are flagged classifier-sensitive for the
offline join.** Whether each flip is a genuine Type-A residual vs a committer-shadow is the OFFLINE
join — gated on the committer-time enrichment (see OQ-83 note).

**Verification (all 9 pass; audit dir has the recipes):** V1 pipeline byte-identical after stripping
the new block + manifest (no regression); V2 `derive_directionality_at` ≡ `derive_directionality` +
deterministic over 500 (C,T) pairs; **V3 — `test_snapshot_migration` green, but the named-test "sync"
is the WEAK claim: full `classify_at_time` ≡ `snapshot_type` is FALSE and was always false (3 unique
mismatch points at default context — the earlier "7" was metric-duplicated; my edit is sync-neutral,
witnessed on stashed code). The "two classifiers in sync" must-hold is OPEN, not passed. Contamination
join {3 mismatch}∩{52 default flips} = 2 flagged (clinical_deskilling_automation 0→2; milblogger
12→18). Likely cause [UNVERIFIED]: snapshot_type calls classify_from_metrics WITHOUT the `nb_setval`
temporal theater/eps state classify_at_time threads, so the piton/excess gates read stale/static.**
V4 residual reads d off
`/5` (0 second-derive); V5 real flip well-formed; V6 retracting an authored ε moves a real flip into
`fabrication_adjacent` and restores (guard fires); V8 no `cs_` in the module, imported only by
stack+json_report; V9 `git diff` touches only 5 engine files, no `schemas/`.

**Stale doc corrected (operator-flagged, substrate-confirmed):** v7 §4.5 "exactly one intentional
bridge … and nothing else" undercounts the cross-axis seam. The **(A) data bridge** is still exactly
one (`influences`→`detect_necessity_inheritance`); but **(B) read-only seam diagnostics** number ≥3
(`cs_drift_mismatch`, `cs_kernel_registry`→`classify_at_time`, `cs_pattern_detection`→
`constraint_signature`). Separation holds; the enumeration is wrong. (Audit §0; OQ-83 follow-on.)

**Follow-up (same day): committer stage-time enrichment LANDED** (OQ-83 follow-on #1). The CS block in
`json_report.pl` now emits `cs_reference_frame` (t0), `cs_drift_moment`+`cs_drift_gap` (t1) beside the
pre-existing `cs_drift_terminal` (t2) — 7 constraints carry it, null otherwise; no-regression empty
modulo the 3 new keys. Both temporal descriptors (observer integer-time + committer named-moment) are
now joinable per constraint `id`: the offline residual-subtraction join is unblocked; the
moment-to-integer reconciliation rule stays offline (not baked in-engine).

## 2026-06-08 — Observer-side temporal review: the DR "trajectory" is mostly dark; three "defects" dissolved; three deferrals are ONE coupled ruling gated on time-varying-d
**Files:** prolog/drl_composition.pl, prolog/transition_paths.pl, prolog/drift_events.pl, prolog/cs_kernel_registry.pl, ISSUES.md
**Tier:** correction-key

*[Merged late from worktree `sdm-temporal-records` on 2026-06-11. Superseded in part by later
entries: OQ-46's "12 scalar-only are GAPS" framing was overruled (scalar-as-constant SANCTIONED,
bucketed Backed, OQ-46 resolved 2026-06-11); OQ-83 RESOLVED 2026-06-11 with threads moved to
OQ-109/OQ-110; the time-varying-d D-fork was ruled NO-OPEN at OQ-110 (derived-d stands). The
OQ-41 rows 24–25 correction and the dormancy findings remain current as of the merge.]*

Pre-rebuild review of how the observer (DR) hub handles temporal declarations
(`narrative_ontology:measurement/5` series; Time = relative integer step, not calendar year).
Two hubs: **CS = discrete snapshots t0–t3** (straightforward); **DR/observer = a trajectory**
(per-time re-classification). Read against `docs/debugging_philosophy.md` Type A (frame-fixing).
Started as "fix three temporal defects pre-rebuild"; the substrate dissolved all three. No
engine-logic changed — records-only corrections on existing OQs. Witnesses (read-only, swipl
`[stack]+ensure_corpus_loaded`, N=100):

- **Coverage re-witnessed; prior 471/562 was pre-reset kernel_v1, STALE.** Live AS OF
  2026-06-08: temporal `suppression_requirement` **88/100**, `base_extractiveness` 100/100,
  `theater_ratio` 100/100; **12** scalar-only (STOPGAP), **0** unknown. Corrected in OQ-46 and
  the `drl_composition.pl:174–198` comment. Re-witness on corpus growth.
- **The 12 scalar-only are asymmetric-authoring GAPS, not scalar-by-design** — all 12 carry
  baseE+theater temporal series; only suppression's is missing. Completing it would not
  fabricate motion (no synchronic constraints in the set). Per-story "is suppression flat by
  design for any one" deferred to template/rebuild (authoring judgment, not engine fact). OQ-46.
- **`BaseX=0.5` (`drl_composition.pl:201`) is REACHABLE-BUT-LOCKED, not latent** — OQ-41 rows
  24–25's "extractiveness required-authored" reason is stale. Branch would fire at 11 (C,T)
  cells (e.g. `attribution_erosion-3`), **all non-zero-time (3,5,8,10,16,19), 0 at t=0**. The
  only live caller (`cs_kernel_registry`) classifies at t=0; non-zero times reached only via
  the dormant `constraint_history` sweep → not live. Corrected in OQ-41.
- **The DR trajectory classifier is DORMANT (positive-controlled).** Same consumer-probe finds
  `classify_at_time`'s consumer (`cs_kernel_registry.pl:66–67`) but **none** for
  `constraint_history`/`snapshot_type`/`degradation_chain`; their entry points
  (`transformation_detected`/`canonical_transformation`/`transformation_type`/
  `predict_transformation`) have **zero callers anywhere**. So the "fork" + nb_setval-asymmetry
  "defects" live in code nothing runs; the `measurement/5` series is authored/live but the
  thing that turns it into a classified trajectory is wired to nothing. Live temporal consumers:
  `classify_at_time` at t=0 only, and qualitative drift via `drift_report` (test/lifecycle, NOT
  `run_pipeline.py`).
- **The fail-closed-vs-impute choice is the deferred OQ-44 once-for-class ruling** — the three
  "fixes" were per-site moves on a class decision; recorded as class members (BaseX, snapshot_type
  defaults) under OQ-44, not fixed per-site.

**Coupling (operator ruling, due before the rebuild template is fixed).** Three deferred
temporal threads are **ONE ruling with three faces, gated on the time-varying-d decision**, not
three independent OQs: (a) **time-varying-d** (OQ-83 deferred Ω); (b) **revive-or-gap the dormant
trajectory classifier**; (c) **rebuild temporal-authoring density** (author dense series at
all?). Coupled because the trajectory classifier is dormant **and** would freeze directionality
even if revived: `derive_directionality/3` is not time-indexed and beneficiary/victim are
static-only (0 temporal beneficiary/victim/directionality facts live OR in archives), so
`check_capture_between/3` launders a role-shift it cannot see into an ε-magnitude event.
Reviving (b) is worth it only if (a) is in scope; authoring dense series (c) only if (b) will
consume them. Substrate finding + coupling recorded on OQ-83.

**Meta-pattern worth flagging at the rebuild.** This is the third consecutive deep-read this
session to resolve to "this doesn't need doing" (step-4b `in_contention` feeds nothing; OQ-85
disentangling info absent from substrate; these temporal fixes dormant/locked). The live
load-bearing surface is smaller than the activity around it — the engine carries more dormant /
dangling machinery than live. The **rebuild is the decision point for carry-forward vs. shed**:
regenerating dense temporal series feeds a trajectory classifier nothing consumes, so the
revive-or-gap of the observer trajectory is not tidy-up — it is whether the rebuild's authoring
cost is feeding a dark wire.

## 2026-06-07 — Stakeholder-layer migration Pass-1 audit: computed path ignores authored perspectives (controlled null); straitjacket witnessed; mandatrophy surface is a dangling wire
**Files:** prolog/constraint_indexing.pl, prolog/drl_core.pl, prolog/constraint_data.pl, prolog/probe_harness.pl, prolog/inferred_coupling_protocol.pl, prolog/drl_purity_network.pl, prolog/reading_diff.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, schemas/constraint_story_schema.json, audits/2026-06-07_stakeholder_layer_migration/
**Tier:** landed

Full report + evidence: `audits/2026-06-07_stakeholder_layer_migration/` (AUDIT.md leads with the
keystone). Tracker: ISSUES.md OQ-83 (rulings of record R1–R5, R4 reversed-from-consensus,
sequencing, deferred Ωs). Verdicts, each witnessed in AUDIT.md:

- **A1 keystone (controlled null):** flipping an authored `constraint_classification/3` fact
  (snare→mountain) leaves dr_type/χ/signature/H¹ byte-identical over canonical-4 + product-156
  (162/162 lines); the ε-overlay control on the same story moved EVERY register (120/160 type
  flips, 160/160 χ, sig false_ci_rope→constructed_high_extraction, H1 3→5). The computed
  classification path does not read authored perspectives — the stakeholder layer is an additive
  refactor on the engine side.
- **A2:** d keys on (power atom × beneficiary/victim EXISTENCE booleans × exit) — removing either
  single victim leaves d=0.5 untouched; removing all moves d to 0.46; the atom-keyed override
  moves every same-atom agent together. Two-powerful-agents collapse confirmed.
- **A6 guard asymmetry (silent-mistake warning):** the intra-kernel filter on `shared_agent_link`
  exists at `drl_purity_network.pl:96–98` but NOT at `inferred_coupling_protocol.pl:218–222` —
  same-kernel shared agent names DO enter `run_coupling_protocol`'s edge set. Any cross-reading
  stable-name convention must ride a NEW predicate or add the guard at the second site first.
- **A7 dangling wire:** schema `base_properties.mandatrophy_resolved` has ZERO compiler emissions
  (only `mandatrophy_analysis` commentary prose is emitted); `has_mandatrophy_declaration/1` reads
  `attribute(C, lifecycle, mandatrophy)` = 0 facts corpus-wide; `is_mandatrophy_resolved/1` = 2
  hardcoded archived-corpus facts. Authoring a `mandatrophy_resolved` value today does NOTHING.
  R5's genealogy consumer rewires this (OQ-83), not a third surface. Promoted to CLAUDE.md
  Critical Distinctions (operator, same day); retire that note when the rewire lands.
  **Abandonment reason git-witnessed same day:** emission never existed in any version — engine
  consumers entered at `6f997d71` (hand-authored era), the schema boolean at `3641ae71`
  (JSON-template migration) whose compiler only ever emitted `mandatrophy_analysis` prose. A
  dropped seam at the format migration, not a gameability wall; R5 inherits no hidden wall.
  A6's guard asymmetry split out as its own engine-hygiene item → OQ-84 (operator, same day).
- Probe infra (cost two failed runs): `probe_harness` is NOT loaded by `[stack]` (explicit
  `use_module` required); `domain_priors:base_extractiveness/2` is STATIC (retract throws) and is
  not on the ε read path — overlays target `narrative_ontology:constraint_metric/3` (the chain is
  drl_core.pl:84 → constraint_data.pl:11–13 → constraint_metric).
- A4 role-alignment: 85.0% (1046/1230) middle band → proceed + residue ledger
  (`a4_residue_ledger.md`): contender 6.3% (dial-set backgrounds contention), ritual_operator
  1.9%, dual_role, non_agent. Cuts 90/70 operator-declared, revisable against the ledger.
- **Phase A step 1 LANDED (same day):** `schemas/constraint_story_schema.json` gains OPTIONAL
  `stakeholders[]` (five-role declared dial-set; contender ruled out — contention is derived,
  relational; per-stakeholder agent-hood gate; name rule per OQ-84) + `six_questions` (Q3/Q4/Q5 +
  R5 genealogy, mismatch-consumer-only, provenance structurally required). Pattern-5
  authored-empty conditional enforced. Witness: `phase_a_schema_witness.py` 7/7 — pre-migration
  story still validates (additive), stakeholder story validates, four negative controls each bite
  at the intended guard; validated with Draft7Validator (the pipeline's actual validator —
  installed jsonschema has no Draft202012).
- **Phase A step 2 LANDED (same day): compiler emission closes the window.**
  `generate_constraint_pl.py` emits `constraint_stakeholder/7` (+ secondary_role/non_agent
  facts), `disappearance_verdict/2`, `founding_problem_status/2`, and role-derived
  beneficiary/victim (agent-gated; excluded derives NOTHING — R3; deduped, duplicate facts would
  inflate victim critical-mass counts). Witnesses: 0/100 old-vs-new diff; pilot with all five
  derivation branches; lint clean; swipl-loads. ALL witnesses re-ran against the post-fix file
  (fix → pilot recompiled to disk → branch greps → diff re-run → lint+load), not just the diff.
  **Bug caught pre-commit, and WHICH probe caught it is the lesson: the additivity diff (0/100,
  the strongest automated check) was STRUCTURALLY BLIND to it — no corpus story has
  six_questions, so the corrupted path never executed on the corpus and "0/100 differ" was a true
  statement about a probe that could not reach the defect. Only the pilot's per-branch greps — a
  positive control aimed at the path the corpus cannot yet exercise — could see it. PROBE
  PLACEMENT RULE: every compiler feature with no corpus coverage has this same blind spot; pair
  it with a per-branch pilot probe on the uncovered path, and never read a green aggregate diff
  as covering paths the corpus doesn't contain.** The trap itself (generic to generate_pl): a
  local named `lines` shadows the `emit` closure's accumulator and silently discards all prior
  output while reporting success — silent-on-success, nothing downstream complains; comment at
  the site. **A6 sub-gate inside step 2 — clean, but read its scope precisely:** 0 engine
  consumers of new predicates (control fires), 0 new cross-constraint shared atoms, guard sites
  untouched — this clears the DERIVED-name half only (derived names reuse existing naming +
  dedupe). The bespoke AUTHORED stakeholder names (the population the 504/25/38 baseline was
  about) arrive with step-3 projection, against the still-unguarded
  `inferred_coupling_protocol.pl:218–222` (OQ-84). **A6 is closed for derived facts and REOPENS
  at step-3 projection — the guard lands before-or-with the projection, same pass, never
  after.** Step 3 (engine layer) is the next forward move; until then stakeholder/7 + the two
  atoms are produced-awaiting-consumer (named hold), while derived beneficiary/victim are
  consumed immediately by existing d/FSM machinery.
- **Phase A step 3 LANDED (same day): engine layer; mechanism witnessed (scoped as plumbing —
  the experiment is step 4).** Delegation refactor `extractiveness_for_agent/3` →
  `extractiveness_for_agent_d/4` witnessed BYTE-IDENTICAL on the A1 harness (162 rows,
  canonical-4 + product-156); `dr_type_with_d/4` (skips resolve_coalition_power — caller owns d);
  `stakeholder_seats.pl` per-(C,Name) layer (role-d config params = DECLARED fitness-chosen seat,
  config.pl comment; all outputs commentary-grade); narrative_ontology decls (the five
  stakeholder predicates are dynamic — but **`cs_kernel_id/2` is STATIC: assert throws; consult
  a temp multifile file as the overlay tool**); R5 zombie clause (second
  `has_mandatrophy_declaration` clause over the two authored atoms, mismatch-only). OQ-84 guard
  added in the same pass (bug branch git-witnessed: coupling module frozen 2026-02-18,
  pre-kernels; live 72=72 no-op, synthetic same-kernel 1→0). Mechanism witness
  (`step3_mechanism.txt`): same substrate, atom-keyed all-solutions `[0.15]`/one type vs
  name-keyed 0.12/0.85 split, causally traced via payer-param overlay (only payer seats moved;
  restore verified); control story no-split. Untested this pass (deliberate mobile-isolation):
  exit-mod arms beyond trapped(+0.05), the d clamp. Validation suite 0 warnings post-change.
- **Phase A step 4 — 4b gate fired RENAMED-NOT-ESCAPED (same day); 4c NOT run; STOP, operator's
  to act on (OQ-85 filed).** Pilot stakeholder prompt (neutral, witnessed) + constant-scaffold
  adapter + scaffold-leak witness (PASS both axes — type and tuple inert, positive control
  fires) + 6 topics pinned-before-gen, model pinned gemini-2.5-pro. Across all 3 contention
  topics the headline antagonists never land same-power+opposed-role: streaming & hospital
  authored both as agenda_setter at the same power (institutional) — opposition only in prose;
  app_store opposed-role but power-atom-drifted. `in_contention` (same-atom AND opposed-role)
  fires on neither headline shape (but IS live — fired on 3 non-headline/non-contention/mountain
  pairs = positive control). Dual cause: generation (gemini renders co-equal contention as two
  agenda_setters) + vocabulary (the d-split only separates agenda_setter/beneficiary-vs-payer,
  so opposed co-administrators are invisible — A4 contender-residue with evidence). Bears on the
  A4 derive-don't-author ruling. Phase B + 4c + the 2×2 model Ω gated until OQ-85 ruled.
  Evidence: `audits/.../STEP4_4b_RENAMED_NOT_ESCAPED.md` + 6 `*.stakeholder.json`. No live-corpus
  writes; four-tuple prompt untouched.
- **OQ-85 RESOLVED same day — silence-is-correct; the 4b gate was guarding a non-problem; 4c
  unblocked.** Read-only decomposition audit (`audits/.../OQ85_DECOMPOSITION_AUDIT.md`).
  Load-bearing witness: **`in_contention` feeds NO classifier** (grep: zero consumers;
  dr_type/classify_from_metrics/signature read neither it nor `constraint_stakeholder`) — it is
  annotation, so its silence on co-equal antagonists cannot be a classification blind spot.
  General result (not corpus-contingent): the constructed no-anchor worst case `oq85_blindspot`
  (two co-equals both agenda_setter, zero victims) still computes `snare` — type is metric-driven,
  correct without the pairwise relation even when no powerless anchor exists. Corroboration (not
  load-bearing): both real stories carry a powerless anchor making the rivalry second-order. The
  4b renamed-not-escaped did NOT find a straitjacket gap — co-equal contention is outside
  `in_contention`'s job; the straitjacket was escaped for everything it exists to detect.
  Right-sizing: a consumer grep would have settled this at the 4b gate, three turns earlier.
  Residual filed standalone as OQ-86 (pairwise who-extracts commentary; not in the migration).
  **4c (cross-framing census / Ω_E / Type-C/B) is the unblocked next move.**
- **4c RAN (same day, PILOT n=6); report `STEP4C_PARTITION.md`; presented not ruled.** Per-topic
  partition (bin-blind evaluability = (a) same-object + (b) (HasB,HasV) profile; ε-pinned): 2
  survived, 2 flipped, 2 unevaluable. Headline type survived in all 4 evaluable (snare). **Both
  flips dissolved to a resolution artifact by per-flip scrutiny:** all-metric-pin control showed
  not-metric-drift; mechanism = victim COUNT × critical_mass_threshold(=3) via
  resolve_coalition_power at the powerless seat (flips = stake 3 victims vs four 2; hospital 3-v-3
  survived). **Criterion finding (next corpus-scale run, pre-register):** (b) incomplete — orbit
  reads victim count via coalition, not just the boolean; extend (b) to count-same-side-of-
  threshold. **Robust separate signal:** claim-layer framing effect — stakeholder claims `rope`,
  four-tuple claims `tangled_rope` (3/3 contention), engine corrects both to snare (claim moves,
  computed type doesn't). Engine has no framing-sensitive classification layer (orbit =
  f(metrics, victim-count); perspectives ignored per A1). Type-C/B not settled at n=6 → corpus
  run + 2×2 model Ω. **Swallowing-trap recurred:** the all-metric-pin control's first run
  returned empty==empty and falsely read "identical/metric-drift"; caught, re-run, corrected
  result was the OPPOSITE — a diff-of-two-empties is a false pass, not a witness.
- **Committer-axis thread BANKED/PARKED (2026-06-08); cold-read entry
  `audits/2026-06-07_stakeholder_layer_migration/COMMITTER_THREAD_HANDOFF.md`; tracker OQ-87
  (partial).** Banked verdicts: two-axis architecture real (observer orbit framing-blind incl. to
  cs_structure; committer axis separate structure-sensitive surface → observer-axis Type-B
  architecturally foreclosed); CA-1 committer field partition confirmed (framing-invariant,
  content-sensitive); CA-3 kernel_v1 diverge-A 74 is ~89% one drift convention (saturation), NOT
  load-bearing (per-item cause witness — banked the standing rule *a gating count needs its
  composition in the same pass*, build_discipline.md); Step 0 observer claim-drift MODEL-STABLE
  (rope/tangled_rope reproduces 3/3 under Sonnet); pilot Steps 1/1b/matched — kernel_v1
  husk-saturation is reading-set + magnitude-authoring, NOT a Haiku prior, NOT removable by the
  Haiku→Sonnet bump (the MATCHED run — same manifests, vary only GEN model — overturned the
  unmatched Step 1b "Sonnet de-saturates"; ack-false is reading-set-dominated 49–92%, substantial
  robustly 62–88%). Detection-independence existence proof UNPROVEN; next move is a fresh-decision
  LARGER de-leaked study (not a model swap), + CA-2 for committer C/B. Run-tagged pilot stories
  (`prolog/testsets/pilot_*`, `json/pilot_*`) untracked, glob-isolated, disposable. ≈284 gen calls.

---

## 2026-06-06 — Kernel-first router: `_step_decompose` now uses the PRIMED scope prompt (construction-as-classifier)
**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, outputs/kernel_first_phase0/PHASE0_READOUT.md
**Tier:** tripwire

`c-orchestrator._step_decompose` no longer builds the unprimed §3-independence prompt. It now calls
gkc `_scope_user_prompt({"human_readable": topic, "summary": ""}, research_context, self.axes)` —
the PRIMED prompt that asks the kernel question ("contested kernel? emit READINGS; else flat + collapse
omega"). This closes OQ-79 mechanism-2 (flat-miss: the old path never asked, silently flattening
genuine kernels — magnifica → 12 flat axes). Downstream is unchanged: `_step_generate` →
`generate_from_manifests` already handles kernel manifests (readings + the AUTO forced-flat control
from `flatten_manifests` lines 343–359 = the construction pair).

**TRIPWIRES (silent mistakes a fresh agent would make):**
1. **Do NOT revert `_step_decompose` to the unprimed "select every axis that survives §3" prompt.**
   It looks like the "normal" SCOPE call; reverting silently re-breaks kernel routing (the magnifica
   failure). The primed prompt is the single source in gkc `_scope_user_prompt` — both front-ends
   share it; do not fork a second copy (Build-Discipline Pattern 2).
2. **A kernel-positive (`is_contested_kernel=true`) means "admits a foundational construction,"
   dominance UNJUDGED — NOT "this topic IS a dominant/certified kernel."** The primed verdict is
   KERNEL-LIBERAL (Phase 0: routes to kernel whenever a foundational reading is constructible =
   contentful, `docs/seat-theorem-v1.md`; flat only when σ settles it). Loud means-disputes
   (nuclear-climate, reading-wars) route kernel. A downstream count / Tier headline / essay that
   reads the kernel set as "N genuine axiom-level contests" commits the seat-theorem no-seat pose
   (asserts a seat-free dominance ranking, which §6 forbids). Kernels accrue UNCURATED by operator
   ruling (2026-06-06, LIBERAL); a *seated* dominance stage is permitted but DEFERRED (design against
   a witnessed pile). See the promoted line in CLAUDE.md Critical Distinctions.

Witnessed (`--dry-run --skip-search` via the front-end): magnifica → `is_contested_kernel=true`
(3 readings) where the unprimed path flattened it; flat topic → `is_contested_kernel=false`
(reasoned rejection). Phase 0 + widen evidence + ruling: `outputs/kernel_first_phase0/PHASE0_READOUT.md`.
A3 grounding-leg DROPPED (Phase 0: wrong instrument — over-routed readings have real constituencies).

## 2026-06-06 — Generation-backend unification: c-orchestrator routed through the shared backend; the kernel-dropping fork DELETED
**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, agent/story_generator_base.py, python/audits/capture_generation_payloads*.py
**Tier:** landed

The silent fork (Build-Discipline Pattern 2) where c-orchestrator's flat-only generator silently
dropped recognized kernel readings (OQ-79 mech-1) is healed by DELETION. New shared backend
`generate_kernel_corpus.generate_from_manifests` is the single manifest->corpus path: seed-type
dispatch (flat -> c-orch framing via the moved `axis_source_desc`/`upstream_context` in
story_generator_base; reading/flat_control -> gkc `build_cached_messages`), c-orch's wave loop ported,
request defaults caller-supplied (sonnet/string-system for c-orch, haiku/list for gkc). c-orch's
`_step_generate` now calls it; the forked `_step_generate_batch` (44 ins / 255 del) + delegators +
dead imports are gone (grep 0). Serial escape hatch kept (self-contained inline source_desc, named
legacy duplication). OQ-79 guard demoted to a defensive assertion (no ledger; C4 co-mingling gone).

**Witness ladder (all in commits 0f61517c, 099066c4, a7d56a14, ed2ec212):**
- P0 old==old byte-identical across TWO COLD processes, FULL params (model/system/max_tokens) —
  the deterministic target is real.
- W1/W2 new==old byte-identical on 3 flat topics incl. germline (5-wave); re-confirmed AFTER the
  splice — the wiring that delivered kernels did not perturb the flat path.
- P3 LIVE: Zionism (frozen 222814 manifest) — the 3 readings the flat path dropped now land with
  cs_kernel_id; reading classifies tangled_rope/snare/rope/snare across seats.
- P4 mechanism: synthetic reading-upstream manifest — supplementary axis waves AFTER its reading
  with the reading's claimed_type injected (wave FIRES; appropriateness = OQ-81, NOT witnessed).
- The deterministic witness caught a real seed-building dup bug (readings in both axes[] and
  generation_sequence) BEFORE any live kernel run — fixed, germline still byte-identical.

**TRIPWIRE — partial unification:** gkc's `--scope` entry point STILL runs its own (working,
wave-free) kernel generation; it is NOT yet routed through generate_from_manifests (OQ-82). So two
generation implementations coexist — the BUG is gone (both handle kernels) but the literal one-path
goal + gkc-gains-waves remains. Do not assume gkc --scope already waves. New OQs: OQ-80 (generate-step
token totals unthreaded = NOT MEASURED, reports 0), OQ-81 (readings-as-wave-upstream appropriateness),
OQ-82 (the gkc --scope rewire). OQ-76 (never-recognized flat-miss) still uncovered.

## 2026-06-05 — Pre-build ruling session executed: OQ-70/64/63 ruled and landed, intent_* declared GAP-08, perturbation-principle §1.1 added
**Files:** prolog/signature_detection.pl, prolog/constraint_indexing.pl, prolog/narrative_ontology.pl, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prompts/constraint_story_generation_prompt_json.md, docs/design/design_gaps.md, docs/the_perturbation_principle.md
**Tier:** landed

Operator ruled the three pre-build items in one session, all on one principle (now written into
`the_perturbation_principle.md` §1.1): wherever two layers disagree about what an authored thing
means, the authored layer's definition is authoritative — the computed layer must never consume
what the author did not assert.
- **OQ-70-A as the CLASS** (`72ec2cdd`): claimed_natural source 2 + appears_as_rope's sibling
  removed — no signature may read a single authored perspective as a story-level claim. Live-20
  witness: FCR 16→5, FNL 3→1; positive control manpower_exhaustion_trap still fires FNL via
  source 1. Signature prevalence is a claims statistic from rebuild story 1.
- **OQ-64-A** (`e5fbc2e8`): `vindicated_propositions` schema array → `constraint_vindicates/2`
  (feeds NO metric/gate); beneficiaries are actors only; six witnesses incl. negative control.
- **OQ-63-A** (`28f2dfc8`): d-derivation consumes `agent_beneficiary`. ZERO-DIFF cutover
  (80/80 constraint×seat rows identical) + guard positive control (registry non-agent refused).
- **Item 2** (`f618c1f1`): intent_* = design_gaps GAP-08 (declared absence). Verification found
  the residual points PASS-OPEN: `has_viable_alternatives` defaults false on the empty table and
  NL certification REQUIRES false — OQ-43 fifth instance, fail-close deliberately not made
  (would un-certify all NL until intent is fed or the gate re-sourced; needs its own ruling).
- §1.1 added to `the_perturbation_principle.md` (operator-authored): the perturbable object is
  the authored story; the purpose is holding the seats without collapsing into one view or a
  view sub specie aeternitatis; every view is a view, even the God's-eye one.

## 2026-06-05 — CORPUS RESET: live testsets/ rebuilt from scratch under the de-leaked pipeline; all previous corpora consolidated to prolog/archives/datasets/
**Files:** prolog/testsets/, prolog/archives/datasets/, CLAUDE.md, AGENTS.md
**Tier:** tripwire

Operator reorganization (by hand; git-recorded as 13,532 renames in commit `29889e50`):
live pre-reset corpus (1,106 stories + stage1_probe/flatctl_probe/lineage_probe_01 run-tags)
→ `prolog/archives/datasets/kernel_v1/`; testsets_3000 (3,380 chimera-era) → `original_v6/`;
testsets_sotu (189) → `sotu/`; gaptests/recon_2/ab_test → dated `audits/` dirs;
commitment_corpus + fix → root `archives/`. New `prolog/testsets/` seeded with the first three
post-de-leak topic runs (20 stories). **Follow-up (same day, commit `1a0acfb8`): `json/` reset to
match — 4,410 pre-reset story specs + 21 pre-reset subdirs archived to
`prolog/archives/datasets/kernel_v1_json/`; `json/` now holds exactly the live stories.** **Tripwires:** (1) ALL pre-2026-06-05 empirical findings
(OQ-70 FNL stats, OQ-71 lineage, 55% coordination disagreement, sweep baselines, KNOWN_STATE
witnesses) were measured on `kernel_v1` or its ancestors — re-witness on the live corpus before
citing against it; retrospective audits overlay `corpus_path` to the archive dir. (2) run_pipeline
reports n_sotu=0 (graceful); sotu analyses must overlay the archive path. (3) The first-pass
new-vs-old comparison (this session): 3/20 new stories claim mountain and ALL fire
type_1_false_summit (incl. claimed-mountain ε=0.85 `manpower_exhaustion_trap`, unauthorable
pre-de-leak); old 0.58 ε-anchor gone, new 0.68 idiom (11/20, not band-linked — Stage-2 watch);
claimed-type diversity 5 types/run vs old tangled-dominance; seat-agreement 26/80 new vs 7/12 old
(old comparison biased: old pipeline steered claims into modal types).

## 2026-06-05 — c-orchestrator batch generation (dependency waves); repair de-fanged; report highlights authored-vs-computed divergence
**Files:** agent/c-orchestrator.py, agent/story_generator_base.py, python/story_repair.py, python/enhanced_report.py
**Tier:** landed

With the axis cap removed, 6–8 sequential Sonnet calls became the per-topic long pole.
`_step_generate` now dispatches to a BATCH path by default (`--serial-generate` /
`DR_SERIAL_GENERATE=1` keeps the legacy loop with its LLM retry-with-feedback): each §5.1
dependency WAVE is one Anthropic batch (50% cheaper; static prefix cache-controlled; `poll_batch`
reused from generate_kernel_corpus — no pattern fork); upstream claimed_type context flows
between waves; failed upstreams unblock dependents (no deadlock). `build_prompt` refactored into
`build_prompt_parts` (static/dynamic split) with a byte-parity witness (old vs new identical,
both arg shapes). Offline simulation witness (fake client): correct wave partitioning
(a/c/e → b → d), upstream context injected, cache_control present, 5/5 saved, tokens summed.
**Operator ruling folded in: generated stories are NOT linted at generation time and the
authored side is never "fixed" — divergence is read downstream.** Two enforcement changes:
(1) `story_repair.py` no longer fabricates `mandatrophy_resolved` from an extractiveness
threshold (band-keyed fabricated default writing an authored field; its schema conditional died
with the de-leak) — witnessed: repair leaves claim/metrics untouched, high-ε story without the
field validates; (2) `enhanced_report.py` CONSTRAINT IDENTITY now renders an explicit
"Authored vs Computed: DIVERGES at n/m seats — …(divergence is signal, not defect)" line in
both branches (witnessed both directions). The batch path contains zero lint calls (grep = 0).

## 2026-06-05 — Generate-both landed: forced-flat control on every kernel, mechanical alignment key flat_control_of/2 (OQ-76 mitigated)
**Files:** agent/generate_kernel_corpus.py, python/generate_constraint_pl.py, prolog/testsets/flatctl_probe/, ISSUES.md
**Tier:** landed

Operator ruling: generate-both promoted to PRIMARY fix for the stochastic kernel/flat gate —
the recognizer becomes REDUNDANT (every kernel gets a flat construction unconditionally) rather
than trusted; stratification and the kernel-bias hedge both routed through the broken detector.
Implementation: `flatten_manifests` auto-emits `<kernel_id>_flat_control` seed per kernel
(substrate = `kernel_description`; the reading set is NEVER shown to the flat author);
compiler emits `narrative_ontology:flat_control_of/2` from ephemeral `_flat_control_of`,
OUTSIDE the cs_structure gate; flat controls carry no `cs_kernel_id`/`cs_reading_relation`
(not pseudo-readings — kernel stats and OQ-58 sweep untouched); stamp_kernel_linkage extended
(separate counter, mismatch guard, no-cs exception). ASYMMETRIC BY DESIGN: flat-on-every-kernel
only; never kernel-on-every-flat. Witnesses: compiler emission + negative control; seed/prompt
independence on a real K1 manifest (no reading ids leak); E2E run-tag `flatctl_probe` — first
construction-pair diff via the key: computed dr_type construction-ROBUST (tangled_rope ×4 seats
both constructions), authored layer divergent (snare ε=0.65 vs tangled_rope ε=0.48).
Stage-2 residue: the readout stratum (OQ-76 Remaining). Interim kernel-bias hedge superseded.
Writeup + probe + seed: `audits/2026-06-05_flat_control_generate_both/`.

## 2026-06-05 — K1 kernel-gate replication: real topic-classed boundary band; under-firing misses against explicit §1.3-K criteria (OQ-76 filed; Stage-2 condition)
**Files:** python/audits/kernel_gate_replication_probe.py, prompts/uke_scope_v2_json.md, ISSUES.md
**Tier:** landed

Promoted from the count probe's side-observation by operator review: the kernel/flat gate routes
the SAME contested substrate (T5 manifests diffed — identical contestation as kernel readings vs
flat axis) onto the axiom vs observer axis, and a flat-miss destroys the axiom axis irrecoverably.
K3 hand-adjudication first (free): gig classification AND content moderation both pass all three
§1.3-K criteria → flat takes are gate MISSES, not definitional ambiguity. K1 (k=8 × 5 topics,
40/40 calls, pre-registered INVALIDATION conditions — personhood control <7/8 would have removed
the thin-band diagnosis from the menu): controls 0/8 and 8/8 (instrument valid); affirmative
action 8/8; gig 5/8; content moderation 3/8. Band is real and topic-classed (famous moral kernels
stable; statutory/regulatory contests near coin-flip); noise localized to the binary gate
(conditional reading counts perfectly stable: 4/3/3). Dispositions recorded in OQ-76, not built:
interim hedge = bias gate toward kernel on band topics (fail toward the recoverable side);
candidate fix = generate both representations (construction-pair diff is §7.1 signal); K2
phrasing-sensitivity probe licensed as cause-diagnosis. Stage-2 (OQ-75) now carries the routing
condition. Writeup + 40 manifests + driver: `audits/2026-06-05_kernel_gate_replication/`.

## 2026-06-05 — SCOPE count-distribution probe: 7-7-7 was coincidence + run noise, NOT an implicit target (OQ-75 watch resolved)
**Files:** python/audits/scope_count_distribution_probe.py, prompts/uke_scope_v2_json.md, agent/c-orchestrator.py, ISSUES.md
**Tier:** landed

Two-arm (current vs pre-`d179423d` SCOPE prompt — the lens instruction IS in the decomposition
system prompt, `c-orchestrator.py:177,421`, so one arm couldn't name a FAIL's lever), 8-topic
richness-spanning battery, 16/16 calls, pre-registered signatures including the upper-tier
masked-target sub-criterion (T4–T7 must spread among THEMSELVES; a binary floor rescuing global
range = FAIL). Result: selected counts 3→11 tracking richness; upper tiers spread (A: 5/6/6/11,
B: 5/7/6/9); deferrals fire (six non-zero cells — §4 triage visibly works, including deferring
an axis that overlapped another); replicate noise ±1; arms agree; T7's 11 axes shown
pairwise-distinct (1 borderline composite). Bridge replicate: gig-economy 7 (original) → 5
(re-run) — the original uniformity was mid-richness coincidence + temp-0.2 run noise. Stage-2
(OQ-75) is NOT gated on a SCOPE-framing fix; axis-count distribution at scale is a readout, not
a gate. Side observation (recorded, not gating): kernel-recognition is itself noisy — T5
decomposed as a kernel in one arm only. Writeup + 16 raw manifests + driver:
`audits/2026-06-05_scope_count_distribution/`.

## 2026-06-05 — Generation-pipeline de-leak: schema/prompt/feedback boundaries no longer hand the author the engine's bands (audit brief F1–F9)
**Files:** schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md, prompts/uke_scope_v2_json.md, python/linter.py, python/regenerate_stories.py, python/generate_constraint_pl.py, agent/c-orchestrator.py, agent/orchestrator.py, agent/uke_narrative_orchestrator.py, agent/story_generator_base.py, agent/generate_kernel_corpus.py, docs/logic_extensions.md, docs/technical/generation_path_resolution.md
**Tier:** landed

The authored-claim-vs-computed-type diff is the research signal (`the_perturbation_principle.md`);
the pipeline was handing the authoring LLM the engine's decision boundaries, collapsing it.
**Binding leak was the SCHEMA, not the prompt:** `allOf` conditionals tied `claimed_type` to numeric
bands AND the schema text ships verbatim in the generator prompt (`story_generator_base.py:28`,
`build_prompt`), with validation a retry-until-valid gate — a claimed-mountain/high-ε story (the
false summit the engine exists to catch) was literally unauthorable. Commits, each with same-turn
witnesses:
- `29cd45d4` linter coordination_type 4→6 (286 false INVALID_COORDINATION_TYPE cleared, corpus lint
  1821→1535, delta exactly 286; positive control still fires) + canonical 6-value table with
  offset-active/floor-inactive asymmetry → `docs/logic_extensions.md`.
- `9f2d050a` schema de-leak (user ruling: bands AND ε>0.46/0.70 triggers; allOf 9→6; structural
  conditionals kept; measurements/omegas unconditionally encouraged). Witness: synthetic
  claimed-mountain/ε=0.6 REJECTED before → AUTHORABLE after; tangled-without-victims still rejected.
- `b6c4e113` prompt de-leak, maximal scrub (qualitative type criteria; χ/sigmoid/f(d)/σ tables →
  prose, d∈[0,1] semantics kept for overrides; NL-profile 0.85/0.15 → presence-with-honest-values;
  worked-example ε anchors removed; epsilon_bin hand-off dropped in all three orchestrators).
  **Closing witness at the real interface:** assembled `build_prompt` payload, band-near-type hits
  19→0 and threshold-comparisons 28→0, both greps firing on the pre-change payload.
- `7ad86c5a` axes cap → optional ceiling (`--axes` default None in c-orchestrator + gkc;
  SCOPE "THREE IS THE BUDGET" → distinctness-is-the-budget; §4 = ranking/ordering only).
  No-cap witness on 3 topics: **uniform 7-7-7 axes, 0 deferred** — axes 4+ are NOT near-duplicates
  (distinct deltas/observables; contingency gate did not fire) but the uniformity suggests a new
  implicit count target; re-check distribution at Stage 2 (OQ-75).
- `07f7b1c0` regenerate_stories filters THRESHOLD_COUPLED lint codes (SCAFFOLD_DANGER_ZONE,
  LOW_THEATER_RATIO, MOUNTAIN_METRIC_CONFLICT) at the build_user_prompt choke point — covers BOTH
  channels (known_errors from lint_errors.txt + retry_errors). Witness: tripping story's lint shows
  the code, built prompt doesn't, MISSING_NL_PROFILE passes through. Rules stay as offline
  diagnostics: their firing rate IS the claim-vs-metric divergence readout.
- `d179423d` lens-diversity SCOPE instruction — **SEPARATE CHANGE VARIABLE** (user ruling):
  attribute reading-set shifts to this commit, not the de-leak, in Stage-2 readouts.

Engine-side verification (no engine changes): authored type lands as
`narrative_ontology:constraint_claim/2`, read ONLY by diff detectors (`drl_core.pl:566
dr_claim_mismatch/4`, `claimed_natural`); `dr_type/3` computes from metrics; no fallback returns
the claim (brief F8 moot). Probe controls: clean corpus mountain (`axiom_of_choice_consequence`)
reads claim=computed=mountain ×4 seats, no mismatch; synthetic false summit compiles and fires
`type_1_false_summit-severe` (computed tangled_rope at institutional, unknown elsewhere — OQ-37
surface). Stage-2 rebuild is OQ-75 (gated on operator go). New OQs: 72 (axiom alignment key), 73
(cross-frame probe), 74 (coordination_type kernel-vs-reading ruling; 55% = 158/286 re-witnessed).
NOT swept (recorded residuals): coordination offset/floor table in the prompt (engine cost params,
not classification bands); qualitative f(d)/χ direction-of-effect mentions; schema-validation error
messages outside c-orchestrator are unsanitized (harmless post-de-leak: the schema no longer
carries band values to echo). Known limitation (pre-existing): c-orchestrator `_step_generate`
resolves only `manifest["axes"]` — kernel-reading entries skip (witnessed twice); kernel topics go
through `generate_kernel_corpus.py`.

**Schema relocated (operator-ruled, same day): canonical schema now lives at
`schemas/constraint_story_schema.json`** (moved out of `python/`; the stale
`agent/data/constraint_story_schema.json` orphan — 158-line diff, loaded by nothing — deleted).
All loaders updated and witnessed (generate_constraint_pl `_load_schema` relative default,
regenerate_stories `SCHEMA_PATH`, story_generator_base, orchestrator, uke_narrative_orchestrator;
validate_constraint_story delegates to generate_constraint_pl); `DR_SCHEMA` env override
unchanged; assembled-payload band grep re-run post-move: still 0. Live docs updated
(`docs/technical/generation_path_resolution.md`, AGENTS.md Rule 3b, commitment_corpus/ROLLOUT.md,
apply_schema_patch docstring); archived papers/handoffs keep the old path (point-in-time
convention, audits/README).

## 2026-06-04 — OQ-71 depth-lineage probe: SCALE RUN COMPLETE — H1/H3 falsified beyond noise (boundedness is within-regime only)
**Files:** prolog/testsets/lineage_probe_01/, docs/design/a_hypothesis_about_corpus_size.md, ISSUES.md
**Tier:** correction-key

Full record + numbers in ISSUES.md OQ-71 (status `partial`); finding written into
`docs/design/a_hypothesis_about_corpus_size.md` §10. Headline: the 438-story depth-lineage arm
minted distinct 5-dim structural classes at ~1.5× the same-generator breadth control at every
matched n (95% bands non-overlapping), with list-richness matched, and the "generator-invariant"
MI couplings reshaped (props↔actors −0.23 bits, voids↔zone +0.26). **Citation discipline: this
falsifies UNCONDITIONAL boundedness/coupling-invariance only — the arm bundled nesting-depth
with seed-authorship (Opus-authored seeds vs SCOPE-derived control), so do not cite it as
depth-specific discovery until the authorship-controlled breadth arm in OQ-71 runs.** Corpus
note: `prolog/testsets/lineage_probe_01/` now holds 438 glob-isolated stories; flat corpus
verified unchanged at 1,106. One story lacks `cs_structure` (unstampable; named in OQ-71).

## 2026-06-04 — OQ-71 depth-lineage probe: machinery pilot (generator run-tag routing, fingerprint probe validated by exact reproduction)
**Files:** agent/generate_kernel_corpus.py, python/lineage_fingerprint_probe.py, audits/2026-06-04_oq71_depth_lineage/
**Tier:** tripwire

Pilot record; superseded operationally by the scale run above. Canonical artifacts in
`audits/2026-06-04_oq71_depth_lineage/` (NOT `outputs/` — outputs/ is gitignored, and the
salvaged dumps there are irreplaceable). Items a future editor needs:

- **`run_no_scope` now honors `--run-tag`** (`agent/generate_kernel_corpus.py:1087`): output
  routes via `run_dirs()` (json/<tag>/, testsets/<tag>/, run-scoped processed ladder); flag-off
  path unchanged. **The regression gate for this change is REQUEST-PAYLOAD identity, not
  story identity** — generation is stochastic (no temperature pinned in batch params), so
  comparing generated story bytes across runs is an invalid gate that can both false-fail
  (noise) and false-pass (rubber-stamp). The correct invariant: the constructed batch-request
  payloads, captured WITHOUT submission via the stubbed-client harness
  (`audits/2026-06-04_oq71_depth_lineage/gate2_capture.py`), byte-compared pre/post-change and
  flag-on/flag-off. Both gates witnessed 2026-06-04. Re-gate any future edit to the no-scope
  request path the same way.
- **`validate_reading_relation_integrity` writes its quarantine to the FLAT path**
  `prolog/testsets/cs_reading_relation_quarantine.json` even when called on run-tagged dirs —
  a run-tagged sweep silently clobbers any flat-corpus quarantine present (pilot's copy
  relocated to the audit dir). Pass/patch a run-scoped path before the OQ-71 scale-run sweep,
  or before relying on a flat sweep's quarantine after any run-tagged sweep.
- **`python/lineage_fingerprint_probe.py`** is a validated six-dim fingerprint dumper: its
  output reproduced the original v5 dump **exactly** (multiset equality, 3,380/3,380 lines,
  after the documented `catholic_church_1200` exclusion — the non-corpus demo that
  `known_constraint/1` finds under any `corpus_path` overlay because `[stack]` asserts it from
  `constraint_instances.pl`). The salvaged originals (`/tmp/v5_sixdim.txt`, 772-line cur) now
  live in the audit dir with md5s recorded in OQ-71.
- **Pilot (109/112 stories, run-tagged `lineage_probe_01`): machinery HOLDS; the pilot excess
  number is QUARANTINED** — n=83 matched < 300 pre-registered, so it is not an H1 readout and
  must not anchor the scale run (OQ-71 pilot ruling). Main flat corpus verified untouched
  (exactly 1,106 files before and after).

## 2026-06-04 — Probe/loading infrastructure hardening (gotchas → utilities; two commits)
**Files:** prolog/corpus_loader.pl, prolog/cache_registry.pl, prolog/probe_harness.pl, prolog/check_stack.pl, prolog/json_report.pl, python/run_pipeline.py
**Tier:** tripwire

The existence of `swipl_load_path_and_probe_gotchas.md` traced to five structural decisions;
four got standard-SWI fixes. Commit A (`1460e873`, behavior-preserving) + Commit B
(`801390a5`, output-affecting, separate per the output-changing discipline).

**Commit A:**
- **corpus_loader**: relative `corpus_path` now anchored to `prolog/` via
  `resolve_corpus_dir/2` (loading is cwd-independent — witnessed: from repo root 1106, was
  silent 0; from prolog/ unchanged 1106); 0-file glob **throws `corpus_empty`** (escape:
  `allow_empty_corpus`; witnessed throw with anchored path in the error term); new
  **`corpus_constraint/1`** membership registry, one fact per loaded file (1106; demo
  excluded by construction).
- **cache_registry.pl**: `clear_all_caches/0` over multifile `clear_hook/0`; hooks in
  boltzmann_compliance, covering_analysis, grothendieck_cohomology, drl_fpn,
  trajectory_mining, arakelov_height (nb_delete — a sentinel value would read back as a real
  threshold). maxent_* deliberately excluded (fitted model state, not a memo). Witnessed:
  6/6 cleared.
- **probe_harness.pl**: `with_retracted/2` / `with_asserted/2` / `with_overlay/3` —
  snapshot-first, setup_call_cleanup+once, cache clears before goal and after restore,
  VERIFIED restore (throws `probe_restore_failed`), fact-only with rule-clause warning,
  module-qualification required. 10 plunit tests passing
  (`prolog/tests/test_probe_harness.pl`).
- **check_stack.pl**: library(check) over the stack. **Baseline (2026-06-04, engine-only):
  4 undefined-predicate references** — `data_repair:constraint_beneficiary/2` (:123, :163),
  `data_repair:constraint_victim/2` (:136), `narrative_ontology:requires_active_enforcement/1`
  (drift_events.pl:175), `validation_suite:test_case/4` (test_harness.pl:26) — plus load
  warnings (constraint_instances weak-import overrides, one singleton, one not-exported
  import in arakelov_height). Findings beyond this list = regressions. NOT wired as a
  pipeline gate while the baseline is non-empty (cleanup tracked in OQ-69).
  **EXPECTED +1 (added 2026-06-12, REMOVE WHEN OQ-115 CLOSES):**
  `abductive_helpers:known_override_signature/1` referenced by
  `signature_detection:signature_grade/2` — known, filed, attributed (OQ-115: phantom
  module under the [stack] chain; pipeline chain healthy, no production no-op). Future
  gauntlets reconcile this line instead of re-running the investigation.

**Commit B (output-affecting, witnessed by full pipeline run 2026-06-04T14:15:56Z):**
- `run_json_report` enumerates `corpus_constraint/1` instead of
  `logical_fingerprint:known_constraint/1` (whose metric/claim/classification union admitted
  the `catholic_church_1200` demo via its classification clauses). Diff vs prior output:
  removed exactly that row; **classification-level changed rows 0; full-record changed rows
  0** (the demo had no metric facts → no corpus-fitted ripple). per_constraint now == manifest
  `n_constraints` (1106 == 1106).
- **Manifest single-writer**: swipl export writes `pipeline_output.raw.json`;
  `run_pipeline.py` is the sole writer of canonical `pipeline_output.json` (raw + manifest).
  Witnessed: direct re-export rewrote raw only, canonical md5 unchanged (`md5sum -c` OK).
- Consumers unchanged: enhanced_report/enrich/Tier-1/Tier-2 all read the canonical file
  post-manifest, exactly as before.

Deferred (recorded as OQ-69 bullets): check_stack baseline cleanup then gate-wiring;
incremental tabling to retire manual cache clearing (zero-diff witness first, per OQ-02
precedent); output write-path anchoring (the remaining cwd dependency, gotchas §9).

---

## 2026-06-04 — OQ-65 detector-bait census COMPLETE: bait=2 (no new), omega-routed=75, 6/10 firings expectation-authored
**Files:** python/audits/oq65_bait_census.py, audits/2026-06-04_oq65_bait_census/
**Tier:** landed

The per-file census OQ-65 demanded (greps known to undercount) ran end-to-end in one session.
Tool: `python/audits/oq65_bait_census.py` — 5 read channels (A beneficiary×FSM sentences 492
files/810 items, B purpose-verb 6/8+1 routed seam hit, C-ben balanced-paren omega capture
381/611, D dual-anchor ±200-char windows 79/160, F all beneficiary sentences in absence files
148/891) + 2 mechanical (C-fsm 349/503 flag-only, E 158 FSM-line files), partition
800+158+148=1106 with file-level cross-check 741+158=899. 10-assertion self-test green
(4 real positive controls, B-isolating synthetic, omega-truncation assertion, E
beneficiary-free assertion, partition, value-atom zero + seam probe each with positive
control, decoy marker-strip + C-ben pin). ~2,500 items read by the classifying model in 26
chunks; verdicts `audits/2026-06-04_oq65_bait_census/oq65_census_verdicts.jsonl` (845 = 842 files + 3 decoys);
final artifact `audits/2026-06-04_oq65_bait_census/oq65_bait_census.json` (verdict_source per file, sum exact).

**Results:** explicit_bait 2/1106 (maxwell + total_war structural_contraction ONLY — zero new
bait; OQ-63's suffix-probe scope qualifier CLOSED); omega_routed 75 (6.8%, ~37× the 2
previously known); adjudication_expectation_prose 13; expectation-authored union 87 (7.9%);
nonagent_referent_candidate 29 (OQ-64 feed); template_rule2_citation 3 (template mandates the
omega routing, verbatim citations). Firing crossing (fresh pipeline 13:46Z c463b17, firing
set 10, zero delta): **6/10 expectation-authored** (1 bait + 5 omega_routed) — up from 4/12
pre-registry-fix. OQ-65 → mitigated; OQ-63 filtering-ruling precondition MET. Adoption ruling
(promote flags to committer-axis signature?) escalated to operator in OQ-65.

**Method corrections discovered en route (witnessed):**
- The recon channel table was measured with a TRUNCATING omega regex (`omega_variable\([^)]*\)`
  stops at first inner `)`); balanced capture found 345 omega terms whose beneficiary mention
  the old regex cut off (C-ben 162→381 files). Pre-fix table superseded-unreproducible (same
  disposition as the 445 figure, which also did not reproduce: 461 at n=1106).
- Channel D must anchor on BOTH token families: beneficiar-anchored-only was
  boundary-asymmetric vs the E assertion (2 witnessed violations at ~200-char edges).
- Blind decoy outcome: no-marker-bait and false-positive controls PASS; the omega-routed decoy
  VOID by construction flaw (derived from env_instability's FIRST omega — the
  substantive-shaped one) — **the reader classified the decoy text correctly while blind and
  flagged the construction flaw in its evidence note**; key corrected with documentation
  (`audits/2026-06-04_oq65_bait_census/decoy_key.json`).
- Classification rules converged during the read (recorded in verdicts + session plan):
  omega_routed requires FSM/detector/signature/engine NAMED + fires/triggers-class verb tied
  to the omega's resolution; passive "false summit detected/confirmed/reclassifies" without a
  named engine = substantive; predictions-of-correct-firing on authored data = substantive;
  prose-vehicle FSM-as-evaluator misconception = flag adjudication_expectation_prose.
  Witnessed contrast pairs: sid 173 vs 592, 545 vs 535, 495-corrected vs 610.

---

## 2026-06-04 — Audit corpus consolidated into `audits/<YYYY-MM-DD>_<slug>/` (location mandate)
**Files:** audits/, python/audits/false_ci_rope_audit.py, python/audits/scaffold_piton_gate_audit.py, python/audits/bc_coupling_audit.py
**Tier:** tripwire

Audits were scattered (Pattern 2 at directory scale): writeups in `docs/`, one each in
`docs/audits/` and `docs/technical/`, findings unversioned in gitignored `outputs/`,
self-contained packages at root. All consolidated into a new root `audits/` directory — one
dated subdirectory per audit, writeup + evidence together. **Mandate recorded in CLAUDE.md
(Audit Methodology), `docs/technical/build_discipline.md` (Pattern 2), and
`audits/README.md`.** Files dated after the last clean commit were NOT moved (parallel
session protection), except this session's own fnl_* set.

**Move map (22 subdirectories):**
- `docs/` writeups → `audits/`: blocking_gate (2026-04-14), false_ci_rope (02-23),
  logic_divergence (03-07), report_generator (02-23), scaffold_piton_gate (02-23),
  trifurcation_mapping (05-02, + `phase1/` working set as its `phase1/` subdir),
  `docs/technical/schema_drift_audit.md` (05-30), `docs/audits/purity_audit_20260603.md`
  (06-03, + `outputs/purity_audit_20260603/` evidence). `docs/audits/` removed.
- Root packages → `audits/`: `audit/` → 2026-02-25_spectral_laplacian (its stale
  `__pycache__` deleted); `audit_data/` → 2026-02-28_codebase_audit_data;
  `audit_proposal/` → 2026-05-15_repo_reorg_proposal/proposal.
- `outputs/` audit families (were gitignored/unversioned, now tracked): ccdp (04-14),
  cluster_space (05-07), metric_two_hub (05-08), sheaf (05-08), audit3_profile_accumulation
  (05-17), bc_coupling (05-29), position_geometry (05-29),
  authoring_closure_fabricated_defaults (05-30), wiring_gap_census (05-31),
  reading_diff_census (06-03), fnl_bait_confound (06-04). The two censuses were a flagged
  gray zone (OQ-feeding censuses vs standalone audits); ruled in by the user 2026-06-04.
  Producer `prolog/reading_diff_census.pl` keeps its `outputs/` workspace write path.

**Fork notes (Pattern 2 instances surfaced by the consolidation):**
- `docs/scaffold_piton_gate_audit.md` (Feb 23, 21 scaffolds/95 pitons, pre-rebuild) vs
  `python/docs/scaffold_piton_gate_audit.md` (May 29, 1/1, post-rebuild) — two RUNS of the
  same generator at different corpus states, not an edit fork. Both kept in
  `audits/2026-02-23_scaffold_piton_gate/` (the rerun as `*_20260529_rerun.md`, with
  `*_data_20260529.json`).
- `audit_proposal/` vs `audit/agy/` — two generations of the May-15 repo-reorg proposal;
  both kept under `audits/2026-05-15_repo_reorg_proposal/` (`proposal/`, `agy_variant/`).
  Which is later/canonical was NOT adjudicated — they are point-in-time documents.

**Conventions established:** `outputs/` = live workspace (audit scripts in `python/audits/`
keep reading/writing there; re-runs regenerate); `audits/` = dated archive. Point-in-time
documents (the proposal set, archived writeup footers) were NOT retro-edited; only live
pointers were rewritten (ISSUES.md, this file, `docs/project_orientation.md`,
`docs/logic_extensions.md`, `docs/piton_scaffold_diagnostic_arc.md`,
`prolog/{signature_detection,boltzmann_compliance,config}.pl` comments,
`prolog/recon_2/recon_2_inventory.md`). **Two scripts wrote reports into `docs/` —
`python/audits/{false_ci_rope,scaffold_piton_gate}_audit.py` REPORT_PATH redirected to
`outputs/`** (workspace convention; archived copies noted in their headers). Consumers of
`outputs/bc_coupling_audit.json` (metric_audit, audit3_te_robustness, position_geometry,
position_geometry_metric_sensitivity) still read the workspace path — on a fresh clone or
after cleaning outputs/, run `python/audits/bc_coupling_audit.py` first (unchanged
behavior: the file was always gitignored).

---

## 2026-06-04 — FNL prevalence is template-bait-confounded (OQ-70): mechanism witnessed end-to-end, counterfactual run
**Files:** prolog/signature_detection.pl, agent/verification_bottleneck.json, audits/2026-06-04_fnl_bait_confound/
**Tier:** tripwire

Question (from an older evaluation): is the ~95% disguise-signature dominance (FNL 827 + FCR
219 of 1106) substantive or a generator artifact? **Answer for FNL: artifact, witnessed at
every link.** Full evidence + artifact paths in ISSUES.md OQ-70; probe outputs under
`audits/2026-06-04_fnl_bait_confound/fnl_probe*`.

- **Denominator ruling:** corpus = 1106 (testset constraints, 1:1 with files).
  `pipeline_output.json` per_constraint has 1107 entries — the extra is
  `catholic_church_1200`, an engine demo from `constraint_instances.pl` (loaded by
  `stack.pl:13`), claimed_type None. Exclude it from corpus statistics.
- **Mechanism (each link witnessed):** the one-shot generation example
  (`agent/verification_bottleneck.json`) authors an "ANALYTICAL OBSERVER / NATURAL LAW VIEW
  (MOUNTAIN)" perspective at (analytical, analytical); 908/1106 constraints copy it (922
  author a mountain perspective at some context; 1063 author a rope one — Mandatory
  Perspective #2). `claimed_natural/2` source 2 reads ANY authored mountain perspective as a
  naturality claim (827/827 FNL fire via that source — Probe 1); the Boltzmann 4×3 grid is
  non-compliant for ≥85% of every ε band ≥ 0.3 (Probe 3). FNL ⟺ mountain-perspective ∧
  non_compliant, exactly (Probe 0 funnel).
- **Counterfactual (Probe 2):** retract the 915 tuple-T mountain facts (908 constraints hold
  ≥1; some files author two) → FNL→FCR 809, FNL→FNL 14 (all hold non-T mountains; = 922−908
  exactly), FNL→CHE 4, FNL→genuine{NL, CI_rope} **0**; 809+14+4 = 827, closed. Controls:
  sensitivity (pre-named prediction false_ci_rope) PASS, specificity (41 explicit-claim
  unchanged) PASS, collateral 0. **Read as bait fungibility:** ex-FNL mass lands in FCR via
  the parallel rope-perspective gate; the FNL+FCR aggregate (1046→1042) is the WRONG
  observable — destination histogram only.
- **Override slice (Probe 5): zero discriminating work.** Of 189 FNL firings overriding a
  non-tangled_rope claim, discriminating subset (source 1/3) = ∅; 188/189 metrically
  consistent with their own claimed type's gates — steamrolled, not caught. The 1
  gate-inconsistent case (decalogue_image_prohibition, snare supp 0.58 vs 0.60 floor) also
  fired via source-2 bait — coincidental landing, credit to author error. Substantive yield
  of 827 firings: 0 detected, 1 coincidental.
- **Standing rule until OQ-70 is ruled:** do not cite FNL prevalence (or the FNL-driven
  tangled_rope ~70% dominance — inherited from authored claimed_type, 779/1106) as a
  detection result. FCR is the same gate pattern by static analysis + attribution (Probe 1b:
  174/219 via indexed rope perspective), counterfactual pending — the combined "94.5%"
  figure is unlicensed either way.
- **Probe discipline notes that paid off:** in-session signature sweep reproduced the
  pipeline 1106/1106 (load-path control); `constraint_signature/2` always called with the
  signature unbound (bound probe bypasses lock cuts, ISSUES.md OQ-49 note); facts findall'd
  before retract; caches (`cached_coupling`, `cached_classification`) cleared post-retract.

---

## 2026-06-04 — sheaf_status provenance traced end-to-end; arakelov_threshold now emitted + cited
**Files:** prolog/json_report.pl, prolog/arakelov_height.pl, prolog/sheaf_analysis.pl, python/enhanced_report.py
**Tier:** tripwire

Follow-up trace of the sheaf_status chain (json_report.pl → pipeline_output.json →
enriched_pipeline.json → enhanced_report.py). **Coherent:** H¹, heights, and sheaf_status all
run on the same site (`site_contexts/1`, `site_mode=canonical` config.pl:575 — and `site_mode`
was already recorded in the output's `config` dump, which `findall`s every param; a literal grep
misses it). Cross-field check on n=1107: `manifest_presheaf ⟺ h1_band>0` 0 violations;
genuine/fragile split exactly reproduces from co-emitted heights + recomputed p75, 0 mismatches.
**Gap fixed:** the governing `arakelov_threshold` (memoized corpus p75, arakelov_height.pl:127)
was not recorded anywhere — now emitted as `diagnostic.arakelov_threshold` (json_report.pl,
beside corpus_wasserstein_fracture) and consumed by enhanced_report.py, which cites
`[p75 this run: N]` on the two height-dependent regimes only. Witnessed: emitted 0.400076 ==
independent recompute (n=964 non-trivial heights); passes through enrich; per-file report check
across all three regimes (citation present on genuine/fragile, absent on manifest). No schemas.py
change needed (diagnostic section has no field whitelist). **Gap deferred (already ruled):**
`catholic_church_1200` reaches `genuine_sheaf` via *uncomputable height* (ε unauthored ⇒
`arakelov_height_pair` fails ⇒ fragile clause can't fire) — a second absence route independent of
OQ-51's all-unknown-seats route; recorded as OQ-51 evidence ("Second absence route") with the
build requirement that the ruled 4th value also fail-N/A on uncomputable height, not only on
RealSeats < 2. Do not patch sheaf_analysis.pl piecemeal — OQ-51's ruling moves ~30 consumers
together under output-changing discipline.

---

## 2026-06-04 — Schema drift fixed: `sheaf_status` added to `PIPELINE_FIELDS` (schemas.py)
**Files:** python/shared/schemas.py, prolog/json_report.pl
**Tier:** landed

Commit `205a8187` (2026-06-02) added per-constraint `sheaf_status` emission to
`json_report.pl:390–393` without updating the validator whitelist in
`python/shared/schemas.py` → 1107 `[WARN] unexpected field: sheaf_status` per pipeline run
(Build Discipline Pattern 1 in miniature: additive-to-producer still requires same-change
consumer-side schema sync). Fix: `("sheaf_status", str, True)` in `PIPELINE_FIELDS` beside
`h1_band`; `_SHEAF_STATUS_VALUES` enum (genuine_sheaf/fragile_presheaf/manifest_presheaf)
checked in `_check_structure`. Enriched validator inherits via `_ALL_ENRICHED_FIELD_NAMES`.
Witnessed: full `run_pipeline.py` clean (exit 0, 0 warnings, manifest
`2026-06-04T07:10:37Z`, n=1107, sheaf_status present on all 1107 in both
pipeline_output.json and enriched_pipeline.json); positive controls — bogus field still
warns, bad enum value errors.

---

## 2026-06-04 — Engine/shadow split anatomy (debt-ceiling probe): confidence-0 is wiring-determined for victim-less FSM hosts; filed on OQ-65/OQ-66
**Files:** prolog/maxent_classifier.pl, prolog/config.pl, prolog/signature_detection.pl
**Tier:** correction-key

**Probe (read-only, no engine change):** per-type MaxEnt log-likelihoods for
`statutory_debt_ceiling__constitutional_nullity_reading` at analytical context, profiles
precomputed over the loaded corpus (n=1106, post-agency-gate working tree):
mountain −168.91 (theater Gaussian: authored 0.95 vs profile μ=0.02/σ=0.05 ⇒ 18.6σ out),
rope −9.88 (least-bad; +1.0 boolean via `has_coordination_function` ← raw
`constraint_beneficiary/2`), piton −20.75 (killed by ε=0 vs μ_ε=0.65 and supp=0 vs μ=0.69),
tangled_rope −29.81 (boolean −8.0: two missing `required` features), snare −32.39,
scaffold −27.03. Rope wins by ~11 nats ⇒ softmax p(rope)=1.0, entropy 0. **The certainty is
least-bad selection over a constraint authored outside every cluster's support**, by design:
the reading's content is "void statute, persistent ritual" (ε=0 mountain claim + 0.95
piton-grade theater), and the taxonomy has no zero-extraction-pure-ceremony cell — corpus
pitons are degraded snares (μ_ε=0.65).

**Composed finding (recorded at OQ-65 evidence; cross-ref on OQ-66 debt-ceiling row):** FSM's
override target is tangled_rope (`config.pl:469`); the shadow's tangled_rope requires
`requires_active_enforcement` + `has_coordination_function` + `has_asymmetric_extraction`
(`maxent_classifier.pl:177–179`), and `has_asymmetric_extraction` reads `constraint_victim/2`.
So for every victim-less FSM host, engine=tangled_rope vs shadow p(tangled_rope)≈0 is
structurally guaranteed — confidence≈0 / "starkest engine-shadow split" is wiring, not
per-item calibration evidence (diagnostic layer already patterns it:
`signature_override_artifact`, yellow). Residual discriminator that does carry signal: the
shadow's TOP type — mountain at high p ⇒ beneficiary-field dirt (registry/OQ-64);
non-mountain at artifact certainty ⇒ outside-support/taxonomy hole.

**Post-fix FSM count witnessed this session:** 41 claimed mountains, FSM fires 11 (12
pre-agency-gate); maxwell_demon_impossibility does NOT fire; debt-ceiling still fires
(registry-scoped-out — OQ-66).

**Disposition:** no new OQ — the proposed item ("FSM gate reduces to beneficiary-presence /
non-agent false positives incl. maxwell / shadow-zero guarantee") was already 2/3 landed by
the 2026-06-03 agency-gate session (gate narrowed to `agent_beneficiary/2`; maxwell
un-stripped); only the composed shadow-guarantee clause was unrecorded, and it is now an
OQ-65 evidence bullet qualifying any FSM/shadow disagreement census.

---

## 2026-06-04 — Tracking-surface consolidation: AGENDA.md, AUDIT.md, TODO.md deleted; ISSUES.md is the single tracker
**Files:** ISSUES.md, CLAUDE.md, AGENTS.md, README.md
**Tier:** landed

Reviewed all three item-by-item against the substrate (not the entries) before deletion.
Verified shipped: maxent_profile/4 context-indexing (AGENDA I-1); reading_diff follow-ons #1–#4
(OQ-59 fully disposed); never-generated pipeline reclassify. Verified already-tracked:
regen-polish lint backlog + 4 hard-fail readings (OQ-58). Verified moot: 17
UNRESOLVED_MANDATROPHY (pre-rebuild corpus); "scope has zero classification effect" (σ(S) is in
canonical χ now); jinja2 CVEs (pins already >=3.1.6 in requirements.txt + pyproject.toml — NOTE:
installed env lags at 3.0.3, `pip install -U jinja2` is machine maintenance, not repo work).
Still-live items got OQs: **OQ-67** (legacy power-modifier χ path in drl_audit_core — in-code
TODO at :18 is live), **OQ-68** (qualification-bypass reads of unexported maxent_dist/3 — the
AGENDA I-3 class, re-verified against tree), **OQ-69** (research-frontier ledger: scope-design
validator + MaxEnt-arbitrary-sites + Arakelov-10-slice, spec-encoding tests, Packages B/C/D/E/F/G,
python toolset consolidation, directionality-constant parameterization, T4, framing_notes).
References fixed: CLAUDE.md End-of-Session list (AGENDA/PRIORITIES bullets removed —
PRIORITIES.md had already ceased to exist), AGENTS.md + README.md trees, OQ-10's PRIORITIES.md
citation, this file's AUDIT.md pointer. Full text of all three files: git history (last at
commit a1140d0d).

---

## 2026-06-04 — Ledger sweep: five trivial OQs closed (11, 12, 13, 24, 42)
**Files:** ISSUES.md, prolog/config.pl, prolog/drl_purity_network.pl
**Tier:** landed

Four were ALREADY DONE in substrate with stale ledger entries — OQ-11 (dead params commented out
at config.pl:291-292, zero live refs), OQ-12 (.gitignore:27 has .env, none tracked), OQ-13 (the
four audited pylint sites refactored away; current `pylint -E` output is environmental false
positives: E0401 root-cwd imports, E1131 on PEP-604 unions valid on Python 3.10), OQ-42
(correction already at KNOWN_STATE.md:1144, wrong sentence gone). One needed real work: OQ-24 —
the forecloses structural-exclusion comment now written at
`drl_purity_network.pl:compute_edge_contamination/7` (load verified); notable: a pointer at :63
citing that comment had existed WITHOUT the comment — a dangling doc-pointer
(produced-but-not-consumed in documentation form). Lesson for ledger hygiene: "open" in ISSUES.md
is a claim about the substrate that can go stale in BOTH directions; a closure sweep needs the
same witness discipline as a fix (verify the claim against the tree, not the entry).

---

## 2026-06-04 — OQ-57 RESOLVED: drift_events.pl:230 wrong-module qualifier (one-token fix, land-alone)
**Files:** prolog/drift_events.pl, prolog/json_report.pl
**Tier:** tripwire

`\+ narrative_ontology:requires_active_enforcement(C)` → `\+ domain_priors:requires_active_enforcement(C)`
at the internalized_piton guard. Derived-then-confirmed: exactly 2 corpus emitters
(kodashim_obligation__memorial_archival, statutory_debt_ceiling__constitutional_nullity_reading);
`run_dynamic_suite` completes (0 FAIL / Errors 0) where it previously aborted at kodashim;
pipeline JSON diff = 0 rows (the pipeline path was already correct).

**Mechanism worth remembering (full story in ISSUES.md OQ-57 resolution):** the bug behaved
differently per load path. Suite/REPL: predicate undefined → existence_error → scan abort.
Pipeline: `json_report.pl` is a NON-module file, so its `use_module(drl_core)` imports into
`user`, and modules inherit from `user` — the wrong-qualified call silently resolved to
drl_core's bridge and produced correct events. Tripwire: wrong-qualifier bugs can throw in the
REPL and silently work in the pipeline (or vice versa); when diagnosing module-resolution
behavior, test on the SAME load path as the consumer, and bisect `-l` chains when predicate
existence differs between paths.

---

## 2026-06-04 — OQ-63 diagnostic run: directionality's beneficiary read measured (read-only; no engine change)
**Files:** prolog/constraint_indexing.pl, ISSUES.md
**Tier:** correction-key

Measured the d→χ blast radius of `beneficiary_victim_directionality` (constraint_indexing.pl:417)
reading raw vs agency-filtered beneficiaries. Full per-item evidence in ISSUES.md OQ-63 (updated
same date). Headline, scope-qualified: across the suffix-probe-reachable population (6
all-candidate hosts, 17 partial), the mis-derivation on proposition-kind values is REAL (d
regime-switches to canonical for victim-less hosts; institutional-only −0.05 otherwise) but
χ-immaterial today — all |Δχ| ≤ 0.022, ZERO band crossings, ≥0.18 from any edge. Probe-reach
caveat is load-bearing: the suffix probe is known to undercount (OQ-65).

Method notes for reuse (learned from agency-gate pass, applied here):
- Per-host restore verification (×11 PASS) — restore checked after EACH re-assert, never once
  globally; sequential retract/re-assert otherwise silently corrupts later hosts.
- Suffix lied a second time: `classical_computation_framework` is AGENT by authored gain-talk
  (church_turing :90); the testset's own :236 even predicts the heuristic's d≈0.15. Per-item
  read, never suffix.
- `directionality_override/3` pins d before either read — enumeration of overrides must cover
  control hosts too (constitutional_text_authority :360 caught by anomaly, not by plan).
- Measurement script: /tmp/oq63_measure.pl pattern (findall facts FIRST, retract, snap, re-assert,
  re-snap, compare) — kept in conversation, not repo; trivially re-derivable from OQ-63.

Escalation CLOSED same date, ruled AGENT (witnessed in-file): commitment :225 directionality
logic names the institution as experiencer of benefit; :219's "narrative captures the legitimacy
value" is the mechanism-label. No registry write (AGENT = unlisted default). Sibling
`bureaucratic_continuity` (transmission :151/:103) closes identically. OQ-63 row updated to
"consumer working correctly"; mirror-direction overloading (agent referent under
proposition-shaped name) filed into OQ-64 — value-string morphology is orthogonal to
referent-kind in BOTH directions; disambiguate by authored directionality/gain text, never name.
Ruling-footing rule (durable): cross-sibling perturbation across DISTINCT kernels generates a tag
hypothesis; only an in-file witness makes it RULED — analogy alone = tag marked INFERRED.
Side observation filed in OQ-63: church_turing analytical χ = 0.6576 is 0.0024 below
snare_chi_floor (0.66) — knife-edge independent of this question.

---

## 2026-06-03 — FSM agency gate: agent_beneficiary/2 two-site narrowing (maxwell un-stripped; one-row manifest diff, derived then confirmed)
**Files:** prolog/narrative_ontology.pl, prolog/signature_detection.pl, prolog/tests/test_agent_beneficiary.pl
**Tier:** tripwire

**What changed (code):**
- `narrative_ontology.pl`: new exported `non_agent_beneficiary/1` registry (exactly two ruled
  entries: `entropic_universe_hypothesis`, `international_humanitarian_law_framework`) + derived
  `agent_beneficiary/2` (`constraint_beneficiary` minus registry; **unlisted = agent**, fail-open
  to status quo). The TWO-GATE PRINCIPLE for NON-AGENT entries is documented at the registry:
  gate 1 ontology-true (proposition-kind referent), gate 2 host-deserves-the-released-certification
  (metrics AND narrative/omegas converge on genuine-law). Agency test is ONTOLOGICAL; authorial
  purpose / counterfactual-ness / placeholder-ness never flip a tag (user rulings, this date).
- `signature_detection.pl`, two sites narrowed to `agent_beneficiary/2`: the FSM gate
  (`false_summit_mountain/2`) and `count_power_beneficiaries/2` (the NL profile's
  `BeneficiaryCount == 0`). FSM-ONLY narrowing was witnessed insufficient pre-write: with the raw
  beneficiary still visible to CI_Rope, maxwell would certify `coupling_invariant_rope` → rope×4.
- NO authored `constraint_beneficiary/2` facts edited. Untouched consumers + reasons: see
  ISSUES.md OQ-63 (directionality — HIGHEST STAKES, χ corpus-wide, undetermined) and OQ-66
  (`drl_core.pl:287` — agency-dependent but witnessed inert; full consumer surface
  :333/:362/maxent 173-176 on record).
- New `prolog/tests/test_agent_beneficiary.pl` (plunit, 7 tests + 10 sub-instances, all pass):
  registry-exact-contents guard, view dispatch controls, maxwell positive control
  (natural_law + mountain×4), 11 per-item FSM non-regression instances, and the **:287 inertness
  tripwire** — classifies every divergence candidate raw-vs-filtered (dr_type ×4 canonical
  contexts + maxent_top_type) via test-local redefinition with restore; fails loudly when the
  OQ-66 deferral goes stale. Gotcha encountered: `setup_call_cleanup/3` defers cleanup while the
  goal holds CHOICEPOINTS — the restore hadn't run when the post-control executed; goal must be
  wrapped in `once/1`.

**Witnesses (this session, all pasted in-conversation):**
- Before: maxwell = TR/scaffold/scaffold/TR, signature false_summit_mountain (pipeline manifest
  2026-06-03T16:10:13Z, n=1106, 669eab5 dirty). After: **mountain×4, natural_law**.
- Per-item FSM table over all 41 claimed mountains, before vs after: maxwell FSM YES→no is the
  ONLY flip; humane_treatment still fires with agent-count 2→1; other 10 firing + 29 non-firing
  unchanged.
- Manifest diff DERIVED pre-write (each registry value has exactly ONE host, main+SOTU, probe
  positive-controlled), then confirmed: classification-level diff = **exactly 1 row (maxwell)**
  out of 1106; zero maxent_top_type flips elsewhere.
- **Corpus-aggregate ripple (expected footprint, attributed):** all rows show small numeric
  drift in corpus-fitted fields (max |Δ|: raw_maxent_probs 0.015, arakelov_height 0.013,
  wasserstein 0.036) — pipeline proven DETERMINISTIC (same-code rerun diff = 0 rows), so the
  ripple is the real propagation of one reclassification through corpus-fitted aggregates, not
  noise. maxwell's own shadow: mountain 0.990 → 0.95, still top.
- Falsification check: debt-ceiling (statutory_debt_ceiling__constitutional_nullity_reading)
  UNCHANGED at both levels — cascade TR/rope/rope/TR + false_summit_mountain; shadow still
  rope=1.0/entropy=0 vacuum. Fix A did not resolve it (predicted; its
  `constitutional_supremacy_doctrine` is scoped out of the registry — OQ-66).
- Regression attribution: corpus-wide `run_tests` failures 851 (worktree) vs 850 (HEAD baseline
  worktree); failure-name diff = exactly the 3 `perspectival_gap` tests of the three NEW
  untracked testsets (chip/export/predictive_surveillance — prior session's files, absent at
  HEAD). Zero failures attributable to this change. `run_dynamic_suite` still aborts at the
  pre-existing OQ-57 throw — reproduced at BOTH endpoints (HEAD worktree + working tree) and
  root-caused: `drift_events.pl:230` queries `narrative_ontology:requires_active_enforcement/1`
  but the predicate is authored/bridged under `domain_priors:` everywhere — wrong-module
  qualifier, not a data gap (OQ-57 evidence updated).

**Detector-bait finding ([EDGE] scan):** 4/12 FSM firings were expectation-authored (explicit
bait: maxwell + total_war; omega-routed: env-instability + nuclear_impossibility) under a uniform
FSM-as-extraction-evaluator misconception; 445/1106 files co-discuss beneficiaries with the
detector (template-wide FSM-awareness); phrase scans undercount (maxwell's bait found only by
per-file read). Filed as OQ-65 — framed as a committer-axis DISCRIMINATOR, not only an ε-caveat.

**Open questions filed:** OQ-63 (directionality agency-dependence — ranked first, χ corpus-wide),
OQ-64 (constraint_vindicates/2 split), OQ-65 (detector-bait discriminator), OQ-66 (gate-two
holds ledger: press_reformation gate-2-fail with omega text; debt-ceiling scope-out; :287
deferral surface + tripwire pointer).

---

## 2026-06-03 — Purity audit: structural_purity/2 was dead (bound-probe bug, now fixed); correction key for purity readings
**Files:** prolog/signature_detection.pl, prolog/boltzmann_compliance.pl, prolog/purity_scoring.pl, docs/logic_extensions.md
**Tier:** tripwire

**Audit:** `audits/2026-06-03_purity/purity_audit_20260603.md` (raw evidence `audits/2026-06-03_purity/`,
12 files; pinned N=1106, manifest `669eab5`). Five purity surfaces audited: scalar
`purity_score/2`, bands `purity_zone/2`, categorical `structural_purity/2`, FPN
`effective_purity/4`, and a fifth surface found mid-audit (`fpn_zone/2`, a *second* band
vocabulary).

**Fix applied — `signature_detection.pl:975` bound-probe bug.** `structural_purity/2` called
`epistemic_access_check(C, false)` with `false` bound; the catch-all clause made it succeed for
every constraint, so `structural_purity` returned `inconclusive` unconditionally — the four purity
tests were unreachable dead code corpus-wide (witnessed 1106/1106 pre-fix). Fixed to call with an
unbound var + `Access == false`. **Post-fix witnesses (all pasted in audit §2 / postfix.tsv):**
live distribution = 1068/1106 (96.6%) `contaminated(...)`, 27 pure_coordination, 9
pure_natural_law, 2 inconclusive (exactly the 2 no-access sentinels); 0 mismatches vs the audit's
projected distribution; **0/1106 scalar purity scores moved** (#1 shares tests but not inputs with
#3). Downstream consumer impact: `genuine_findings_query.pl:107` STRUCTURAL_PURITY changes from
constant `inconclusive` to real values — expect ~96.6% contaminated there now. Warning comment
added at the `epistemic_access_check/2` definition (boltzmann_compliance.pl) — do not call it with
a bound second argument; bound-`true` (purity_scoring.pl:42) is safe.

**Text fixes applied:** `purity_scoring.pl:22-27` header comment corrected to canonical zones
(was stale ">0.8 sound / <0.3 contaminated"; canonical = logic_extensions.md §2.3 = `purity_zone`).
`logic_extensions.md` implementation snippets corrected (both reproduced the bound-`false` call
shape).

**Key findings (not fixed, tracked):**
- **OQ-60** — latent absence-reward: zero-evidence constraint scores pristine 1.0 (SI=1.25 via
  `variant([])`, out-of-range hidden by clamp; `cross_index_coupling` returns 0.0 coupling → F=1.0
  on "not enough data"; F=0.5 default branch unreachable, so historical "default_fired 0/N" counts
  are vacuous). 0 current victims; log-only by ruling — fixing moves the no-data fixed point.
- **OQ-61** — the header sentence ("cascading … N/M contaminated") is one signal twice: cascade
  severity derives from effective purity, trigger is an absolute count of 3 (witnessed 633 severe →
  saturated 211×); the contaminated band is 98.1% tangled_rope+snare (rope 92.3% pristine+sound,
  mountain 95.1% pristine — converse control), i.e. corpus-wide purity mostly restates type
  composition; purity_summary silently drops M no-access from the denominator. Three operator
  rulings pending.
- **OQ-62** — band vocabulary fork: `fpn_zone` (abductive evidence) and `purity_zone` (reports)
  band the same scalar with different names/boundaries; "contaminated" = [0.3,0.5) on one and
  [0.4,0.6) on the other; post-fix `structural_purity` adds a third cut (96.6% at excess ≤ 0.05).
  Both banders map −1.0 → worst zone if fed directly (latent; all current gating callers filter
  first). Do not auto-unify — which range wins is a design ruling.
- **Healthy (witnessed):** band table matches spec; all gating purity consumers sentinel-safe;
  FPN invariants hold (no-uplift 0/1106 with checker control, isolated ⇒ EP=IP 8/8); subscore
  formulas 0/1106 mismatches; max multi-edge contamination 0.478 exceeds the 0.30 *per-edge* cap by
  design (edges sum) — not a violation.
- The remembered "530/770 contaminated" ≈ 68.8%; today 753/1104 = 68.2% — the fraction is a stable
  structural property of scoring-on-this-composition, not story drift. `corpus_size` 1107 vs loaded
  1106 explained as corpus churn between pipeline run and audit load (`catholic_church_1200`
  present at pipeline time, absent now).

## 2026-06-03 — never-generated kernels generated (300/304); corpus 803→1103
**Files:** agent/generate_kernel_corpus.py, agent/build_never_generated_seeds.py, prolog/validation_suite.pl
**Tier:** tripwire

The never-generated set (SCOPE-declared contested kernels with **zero** declared readings
on disk) was examined read-only, then generated. Examination found the ~102 are
overwhelmingly **run-to-run kernel-id + family-id naming drift over an already-covered
contest space, not missing content** — generated anyway per the user's ruling that a
same-topic sibling kernel is a *distinct* kernel (different reading-set + ε), an invariant
probe, not a duplicate to prune. Committed `64cc249a`.

- **Counts:** 304 declared readings seeded (101 kernels) → **300 generated** (298 + 2
  re-roll); corpus testsets **803 → 1103**, loads exit 0. Tool:
  `agent/build_never_generated_seeds.py` (sibling of `build_completion_seeds.py`, which
  deliberately skips this set). Seed file `outputs/completion_seeds/never_generated_seeds.json`.
- **4 hard-fail tail** (fail-closed semantic schema — out-of-bounds metric `0.08>max 0.05`,
  invalid enum `'conceptual'`, required `'impact'`; reproduce across rolls →
  `outputs/no_scope_runs/failures.json`): `animal_status_kernel__abolitionist_reading`,
  `honor_satisfaction_substrate__cultural_contraction_reading`,
  `dueling_disappearance_mechanism__contraction_reading`,
  `total_war_possibility_space__space_contraction_reading`.

**Tripwire (silent):** `generate_kernel_corpus.py` **no-scope mode does NOT run the OQ-58
reading-relation integrity sweep** (`validate_reading_relation_integrity`) or
axiom-contradiction emission — those only run in `--scope` mode. It stamps
`cs_kernel_id`/`cs_story_uid` inline, so the files *look* complete, but dangling
`cs_reading_relation`/`affects_constraint` edges are never checked. After ANY no-scope
batch, run the sweep manually (see the snippet in this entry's session / memory
`reference_no_scope_skips_integrity_sweep`). This run: 16→8 after repairing 5
naming-variant edges; the 8 residual are collateral of the 4 hard failures (OQ-58).

**Follow-ups open:** ~~`python/run_pipeline.py` reclassify against the larger corpus~~ —
**DONE 2026-06-03**, manifest `n_constraints` 1103, all 41 stages ok, 80s; regen-polish the
~268 lint warnings (LOW_THEATER_RATIO 221, MISSING_SUNSET_CLAUSE 109, INVALID_COORDINATION_TYPE 95).

**Note (pipeline side effect, expected):** `run_pipeline.py` regenerates
`prolog/validation_suite.pl` from the live corpus (via `python_test_suite.build_suite()`,
`OUTPUT_FILE = prolog/validation_suite.pl`). A **modified `validation_suite.pl` after any
pipeline run is expected, not a hand edit** — it just re-synced its `test_case/4` list to the
corpus. Do not investigate or `git checkout` it as a surprise change (it desyncs the suite).
Same posture as `pipeline_output.json`.

## 2026-06-03 — `reading_diff.pl`: the cyclopean disparity operator (OQ-59 disposition)
**Files:** prolog/reading_diff.pl, prolog/axiom_diff.pl, prolog/stack.pl, prolog/reading_diff_census.pl
**Tier:** tripwire

**New module `prolog/reading_diff.pl`** (wired into `stack.pl` diagnostic load list; queryable after
`[stack]`). Diffs two readings cell-by-cell over the closed `(P,T,E,S)` tuple, keyed on a **declared
alignment relation** (the seat). Partitions into AgreementCells (situation-fixed = objective),
DisparityCells (standpoint-set = the depth), BlindSpots (coverage gap). OQ-59 ruled **preserve-and-diff,
not merge**: averaging two readings is the cyclopean move; the disparity is the depth.

**Invariants a cold reader must not break:**
- **Authored-cells-only.** Reads `constraint_indexing:constraint_classification/3` and (label-only)
  `cs_kernel_id/2` — NEVER recomputed χ, NEVER `product_site_orbits.json`. The existing
  `python/tests/cross_reading_diff.py` reads the computed export (full coverage → no blind-spots
  possible) and is a *relative*, not this operator. Do not "unify" them onto the export.
- **The regime is pair × key, not pair.** `report_pair` never emits a bare regime label; it tags each
  per-key regime with its key and ships a **stability verdict** (`robustly_binocular` / `key_fragile` /
  `robustly_undersampled`), defined **order-independently** (∀/∃ over the declared keys — well-defined
  for any key set, chain or not). A verdict that omits its key is the manufactured-center failure
  recurring *inside the tool*.
- **Counting = vantage-groups (declared).** Headline counts are over aligned vantage-groups (equivalence
  classes of the key); pair-level fan-out is reported separately as multiplicity. Pair-counting inflates
  disparity with an alignment artifact. Consequence: fuzzy self-diff = **0 disparity vantages** (a reading
  agrees with itself as a set-valued map) but **2 disparity pairs** in fan-out — the latter is the
  documented-non-zero, NOT a regression.
- **`all_keys` = `[exact, fuzzy_agent_power]` only** (a monotone chain). `weighted(Ws,Thr)` is opt-in
  with an explicit spec — it has no vantage partition (tolerance relation, not transitive), so
  `reading_diff/6` throws on it; use `aligned_pairs/5` / `report_pair/3`. The "blind non-increasing as
  the key loosens" line is a chain-only **observation**, not part of the verdict.

**Witnessed (exact key), cross-impl check in `tests/test_reading_diff.pl` (10 tests, all pass):**
self-diff 6/0/0; absolute pair (cross-kernel) 4 agree / 1 disparity / 2 blind, disparity piton↔rope at
`(institutional,civilizational,arbitrage,global)` → `robustly_binocular`; graded pair 3/0/6 exact,
fuzzy headline 2 disparity / fan-out 5 → `key_fragile`; conditional pair (third probe) 4/0/6 exact,
4/1/1 fuzzy → `key_fragile`.

**Kernel ruling (user):** `westphalia_sovereignty` (3 readings) and `westphalian_sovereignty` (5) are
**distinct sibling kernels, not a spelling-duplicate** (different reading-sets + ε calibration); both
preserved. So the westphalia↔westphalian test pairs are **cross-kernel** invariant probes. `reading_diff`
is kernel-agnostic by design. (Run with `run_tests(reading_diff)` to skip the ~626 pre-existing embedded
testset-assertion failures that a bare `run_tests` sweeps up — those are authored-claim tests, unrelated.)
Positive control on the wiring: stashed-baseline `run_tests` = 626 failed / 1440 passed, with-wiring =
626 failed / 1450 passed (+10 mine, 0 new failures) — `stack.pl` change introduces no load-order regression.

**OQ-59 #1 edge-repair DONE (2026-06-03).** Retargeted the 4 dangling
`westphalian_sovereignty__absolutist_reading` edges → `__absolute_sovereignty`
(`governance_quality_reading.pl` 130/321, `r2p_reading.pl` 139/341). Witnessed: git diff = only the
target arg changed (4 lines); R1 dangling `cs_reading_relation` **89→87**, `affects_constraint`
**1668→1666**, `absolutist` gone from the linter dangling report (was 1); corpus loads 803, edges resolve.
**Left flagged (NOT repaired):** prose at `governance_quality_reading.pl:226` names the alias AND
mis-characterizes it vs `absolute_sovereignty`'s authored cells — a content judgment for a later pass.

**OQ-59 #3 corpus-wide census DONE (2026-06-03).** `prolog/reading_diff_census.pl` over committed 803
testsets (189 multi-reading kernels, 615 within-kernel pairs): **53.7% key_fragile / 39.5%
robustly_binocular / 6.8% robustly_undersampled** — the alignment seat governs the verdict for a
majority of pairs (cyclopean seat is corpus-wide). Distribution stable if the in-flight 256 uncommitted
testsets are included (40.9/52.0/7.2). Results: `audits/2026-06-03_reading_diff_census/reading_diff_census.{md,tsv}`. Census ran on a
working tree mixing committed + an active ~1h generation run; the .md/.tsv report the committed-only
615 (citable); re-run after the generation lands. NOTE this session also consolidated branches: FF
`main`→`repair-affects-constraint-typos` then merged `docs-corpus-size-hypothesis` (ISSUES.md conflict
resolved by keeping both OQ entries); `main` = `repair` = the merge commit. NOT pushed to origin.

**OQ-59 #4 axiom-diff DONE (2026-06-03).** `prolog/axiom_diff.pl` (wired into `stack.pl`) lifts the
reading_diff partition to the cs_axiom layer; value compared = **grounding** (cs_axiom_status/2 is keyed
on the axiom NAME = global, so it cannot vary per reading — do not compare it). **Sharpened-seat finding:
0 of 935 within-kernel reading-pairs share even one axiom NAME** — no mechanical cross-reading axiom
identity; `exact_name` is structurally all-blind, and the `concept` key reads a caller-declared
`axiom_concept/2` map (multifile/dynamic, EMPTY by default — the seat is never baked; an empty map ⇒
all-blind, and report_axiom_pair says so). Demo (westphalian absolute pair, 4 axioms → 2 declared
concepts): grounding INVERSION — sovereignty_absolute A=conventional/B=deontological,
noninterference A=deontological/B=conventional → key_fragile. Tests `tests/test_axiom_diff.pl` (4, pass).
**OQ-59 fully disposed (#1–#4).** Don't compare cs_axiom_status across readings; don't bake axiom_concept.

---

## 2026-06-02 — Reading-reference linter + the "complete kernels, not patch edges" finding
**Files:** python/audits/reading_reference_linter.py
**Tier:** landed

**Tool:** `python/audits/reading_reference_linter.py` — a reporter (not a fixer). Census of every
reference to a reading/constraint name (`cs_reading_relation` + `affects_constraint`), three rules each
gated by a **synthetic positive control** that must flag a known-dirty fixture: R1 dangling, R2
non-canonical (short / delimiter-typo → existing), R3 within-kernel near-duplicate stems.

**Two corrections baked into the linter (its first cut had both defects):** (1) referential integrity
applies ONLY to `cs_reading_relation` (targets MUST be sibling readings); `affects_constraint` is a
causal edge whose targets may be abstract nodes (`mass_shooting_externality`) — 1680 of its refs are
"dangling" but that's NOT an integrity signal, excluded from the rate. (2) R3 **over-flags by design** —
near-naming is usually intentional (`nws/nnws`, `homoousios/homoiousios`, `created/uncreated` are
DISTINCT); R3 is a review-trigger, only westphalian `gradated/graduated` is a confirmed dup.

**Finding (the headline):** incompleteness rate = **143 dangling committer edges → 119 distinct missing
readings across 69 kernels** (37 missing >1). So the dangling-edge problem is a **kernel-completeness**
problem — ~69 contested kernels decomposed but only partly authored — NOT edge-patching. The
quarantine/linter is the generation backlog spec. See ISSUES.md OQ-58.

**Also this pass:** 4 forecloses edges repaired to existing readings (genesis/magna_carta/fair_use/npt,
commentary-cited; `real_closure` 95→98, quarantine 97→93). fair_use was held a turn earlier as a
stem-judgment, then licensed by the decomposition statement (`fair_use…:336` names the
transformative-right sibling) — held-then-narrative-confirmed, not auto-applied on stem similarity.

---

## 2026-06-02 — Reading-axis structural obstruction built + cs_reading_relation name-form repair
**Files:** prolog/cs_kernel_registry.pl, agent/generate_constraint_pl.py, agent/generate_kernel_corpus.py, prolog/cs_corpus_analysis.pl, prolog/json_report.pl
**Tier:** tripwire

**Built (OQ-54, "establish").** `cs_kernel_obstruction/4` + `cs_kernel_obstruction_status/2` +
`cs_kernel_obstruction_report/0` in `cs_kernel_registry.pl` — the committer-axis analog of the observer
H¹ (`grothendieck_cohomology.pl`), over the **reading** cover. Counts foreclosing reading-pairs
(`real_closure`) vs coexisting (`licensed_plurality`) vs none (`untyped` — **fail-closed on absence**,
NOT silently glued). **Observer-blind by construction** (reads only `cs_reading_relation`, never
χ/`live_index`/`classify_at_time`), so Theorem-7 gradient-orthogonality holds and `live_index` / the
none-compliant cross-tab are NOT prerequisites. Distinct axis from OQ-51's observer H¹/W1.

**Data defect found + repaired (the load-bearing part).** The independent must-flag oracle (NOT via the
probe) exposed that `cs_reading_relation` targets are authored in inconsistent name forms — **short
targets** (`ishmael_covenant_reading`) that don't match the registered **full** reading name
(`abrahamic_covenant__ishmael_covenant_reading`). Exact-match consumers — this probe **and pre-existing
`cs_corpus_analysis.pl:131-149` and `json_report.pl:1432`** — silently under-counted closure/plurality.
Partition (hard counts): forecloses exact 117 / fixable 16 / dangling 13; coexists 528/48/59; influences
212/22/27. **86 name-form edges normalized in-place** across **47 testset files** (short → `<kernel>__<short>`;
script asserted every edit matched, 0 zero-match). **Predicted-delta positive control passed exactly:**
`real_closure` 84→**94**, the 10 named movers (`abrahamic_covenant`, `magna_carta_1215`,
`rogers_commission_findings`, `second_amendment_boundary`, …) all in; dangling untouched (13/59/27).
**Decision discipline (do not relax):** option 2 (probe-only robust) and option 3 (read-time resolver)
were **rejected** — three consumers must not drift to three counts, and a resolver is self-blinding (it
would hide whether the generator fix worked; exact-match on cleaned data is self-witnessing).

**Flow fix (generator), two layers.** (1) `generate_constraint_pl.py:482` canonicalizes the emitted
target to `<kernel_id>__<short>` (hard transform, not a prompt request) — fixes the name-form class.
(2) `generate_kernel_corpus.py:validate_reading_relation_integrity/3` (called after
`stamp_kernel_linkage`) is the **hard-fail referential-integrity** check: a target must resolve to a
declared reading whose canonical file `<kernel>__<short>.pl` exists on disk — **no plausible-form escape**
(a well-formed but absent `<kernel>__<name>` fails like a typo). Unresolved edges route to a
**quarantine bucket** (`cs_reading_relation_quarantine.json`), reported loudly, **never auto-written /
rewritten / pre-classified**. Positive control passed (synthetic fixtures: canonical + short-of-existing
attach; absent-full + typo quarantine).

**Stock quarantine view:** `cs_kernel_registry:cs_reading_relation_unresolved/4` enumerates the existing
unresolved edges (currently **99**: forecloses 13 / coexists 59 / influences 27) — the review queue for
the OQ-58 narrative-read pass. The obstruction stays fail-closed on these (no invented gluing status).

**OQ-58 policy (ruled 2026-06-02):** exactly two automated outcomes — *canonical → attach*,
*everything else → quarantine*. **No** auto-rewrite tier (writes authored structure on an uncalibrated
confidence) and **no** plausible-form tier (well-formed ≠ intended; that's a syntactic tell, not
evidence). The missing-vs-typo-vs-noise sort is the **review's** output, made on the source narrative —
the only place that evidence lives — not a mechanical rule.

**Tripwire:** `cs_reading_relation` targets are now canonical full names in the live corpus, and consumers
stay **exact-match** (self-witnessing). Do NOT add a read-time short→full resolver — it re-hides the
defect. New dirty edges should fail loudly (under-count), then be repaired at source. Pipeline regenerated
green (manifest after repair, 49.6s, all steps ok).

---

## 2026-06-02 — Coupling liveness profile wired into per-constraint JSON (seat structure, not just verdict)
**Files:** prolog/boltzmann_compliance.pl, prolog/json_report.pl, python/query.py, python/enhanced_report.py
**Tier:** landed

**What & why.** The Boltzmann coupling test computed scope-violations and power-violations
separately inside `count_coupling_violations/4` and then summed them, discarding *which* observer
index moves the verdict (Build-Discipline Pattern 1: produced-but-not-consumed). Now surfaced.

**Changes (all behavior-preserving for the coupling score):**
- `boltzmann_compliance.pl`: refactored `count_coupling_violations/4` to delegate to a new
  `coupling_violation_components/5` (SOLE source of the violation logic — score path and the new
  liveness predicate both route through it, so they cannot drift). Added + exported
  `coupling_liveness/3` (rebuilds the Power×Scope grid, returns ScopeViolations, PowerViolations).
- `json_report.pl`: `write_coupling_object/2` now emits `scope_violations`, `power_violations`,
  `live_index` (`none|scope|power|both|inconclusive`) in the per-constraint `coupling` object.
  No-epistemic-access constraints emit nulls + `inconclusive` (absence reported as absence, not
  defaulted to a `(0,0)` "seat-free" reading).
- Consumers (so it is not produced-but-not-consumed): `query.py` (row dict + `--detail` display),
  `enhanced_report.py` (batch Identity block).

**Witness.** Full pipeline regenerated (manifest `ae10e7e`, 50.3s, all steps ok). Positive control:
engine-emitted `(scope_violations, power_violations)` matched an independent oracle
(`/tmp/decomp_out.csv`, same filter) for **772/772**, 0 mismatches; `live_index` agrees with the
(SV,PV) signs everywhere; 1 constraint correctly `inconclusive`. Corpus `live_index` distribution:
both 591, none 87, power 87, scope 7, inconclusive 1 — `none` (87) tracks the ~90
Boltzmann-compliant/invariant population.

**Score behavior-preserving (witnessed).** Direct `cross_index_coupling` under the old code (parent
`51612b0d`, pre-refactor) vs the new code on the same 772-constraint corpus is **byte-identical for
773/773** (`/tmp/old_scores.csv` vs `/tmp/new_scores_direct.csv`, 0 mismatches) — the refactor sums the
same components it now also exposes. The score path is unchanged; only the per-constraint JSON gained
the liveness fields.

**Framing note (corrected this session).** Boltzmann invariance is a *partial test for Mountain-ness*,
not a pathology flag: an index-invariant verdict is seat-free/contentless (Seat Theorem §4), so
`live_index=none` is Mountain-consistent and non-`none` = the verdict is seated on the observer index.
The reading-axis analog is NOT built — see GAP-04/05/06 and OQ-53..56.

---

## 2026-06-02 — Toy corpus finished 769/770; generator repair + 3 robustness fixes
**Files:** agent/generate_kernel_corpus.py, python/story_repair.py, prompts/constraint_story_generation_prompt_json.md
**Tier:** tripwire

**Result.** The kernel-aware toy corpus is complete at **769/770** (ladder `beta_processed.txt`;
~772 `.pl` in `testsets/` incl. 3 user-added). Composition: ~566 kernel readings (200 kernels
decomposed → 570 reading seeds) + ~200 plain (sampled from `beta_seeds.json`). One lone holdout:
`market_as_natural_default__genuine_natural_reading` — claims `mountain` but the model keeps
authoring `suppression 0.08 > 0.05`; this is a **false-mountain** (claimed-natural + real
suppression), correctly rejected by the schema's conditional mountain gate, not a pipeline bug.
Leave it or hand-author.

**Four fixes landed in `agent/generate_kernel_corpus.py` + new `python/story_repair.py`** (all
witnessed — plain went 0/200 → 199/200, residual 8 → 1):
1. **`overwrite=True` in the no-scope path (`run_no_scope`).** The 0/200 plain failure cause:
   `json/` still holds the **pre-rebuild ~4067-file corpus** (it was never archived alongside
   `testsets/`), so plain seeds reuse archive ids whose stale `json/<id>.json` exists and hit the
   `out_json.exists()` SKIP. The ladder (`beta_processed.txt`), not json-existence, is the rebuild's
   idempotence source. **Tripwire: don't "fix" this back to skip-on-exists, and note `json/` is
   stale — archiving it (like `testsets_3000`) is an open cleanup (GAP-style).**
2. **`poll_batch` transient-error retry** (≤30) for 503/overloaded/rate-limit/timeout — a single
   503 was crashing long batch runs mid-poll.
3. **Plain seed summaries capped to ≤500** (`prolog/toy_plain_seeds_capped.json`). Phase-0.5
   uncapped summaries (median 2585) made the model over-produce invented fields; the proven
   generation regime is ≤500 (median 338). The full `beta_seeds.json` stays uncapped (for the probe).
4. **`python/story_repair.py` — canonical deterministic repair**, wired into
   `process_batch_results` (strip + `repair_story` + re-validate before fail). Repairs required-
   defaults, **non-ASCII id transliteration across all 12 schema id-pattern fields** (incl.
   `cs_structure.axioms[].atom`, `network.affects_constraints`, `reference_frame`), null→0,
   unconditional [0,1] clamps. It does **NOT** touch conditional `allOf/then` bounds (claimed_type
   vs metric) — those are semantic; clamping would fabricate. `recover_historical_seeds.fix_story`
   now delegates to `repair_story` (fork removed; the one dropped nicety: commentary-key merge).

**Prompt hardening.** `prompts/constraint_story_generation_prompt_json.md` gained a
"TYPE↔METRIC CONSISTENCY IS A HARD GATE" block (piton ⇒ theater_ratio ≥ 0.70; mountain ⇒
suppression ≤ 0.05 / extractiveness ≤ 0.25). It nudged `tsunami_stone…` (piton) over the line on
retry; the mountain holdout resists because it is genuinely a false-mountain.

**Open / follow-ups:** (1) `json/` pre-rebuild corpus archive cleanup; (2) the 1 false-mountain
residual; (3) the static-ε-below-series-max authoring finding (70/499, prior entry) is still not
an OQ.

## 2026-06-02 — `sheaf_status` now persisted (W1×sheaf join built); orbit provenance is a sidecar
**Files:** prolog/json_report.pl, python/run_pipeline.py, python/w1_sheaf_join.py, prolog/sheaf_analysis.pl
**Tier:** tripwire

**If you are editing `json_report.pl`, `run_pipeline.py`'s `_manifest_step`, or anything that reads
`orbit_data.json` — read this.** Two additive changes landed plus a new join tool. The pipeline ran
clean afterward (exit 0, all steps `ok`).

**1. `json_report.pl` now emits `sheaf_status` per constraint (closes a produced-but-not-consumed
gap).** `sheaf_analysis:sheaf_status/2` (`sheaf_analysis.pl:54-63`: `manifest_presheaf` if H1>0; else
`fragile_presheaf` if Arakelov height > corpus p75 threshold; else `genuine_sheaf`) was computed but
never written to disk — only `h1_band` was. Added a `sheaf_status` emit beside the `h1_band` block
(after `json_report.pl:387`) and `:- use_module(sheaf_analysis, []).` (after `:33`; called
module-qualified). Additive only — the `sheaf_status/2`, cohomology, and W1 predicates are untouched.
Live result at n=563: `manifest_presheaf` 98 / `fragile_presheaf` 100 / `genuine_sheaf` 366; emit-sanity
holds (manifest count == h1_band>0 count == 98).

**2. `run_pipeline.py:_manifest_step` writes the `orbit_data.manifest.json` sidecar.** Same
`build_manifest(run_at)` dict as `pipeline_output.json`, so `orbit_data.json` is provably the same
run. **Tripwire (silent-corruption risk):** `orbit_data.json` provenance lives in the **sidecar, NOT
in-file**. Do **not** `inject_manifest` a `"manifest"` key into `orbit_data.json` — it is a pure
`id→orbit` dict that **7 consumers iterate** with bare `.items()` (`game_theory_nash.py:158`,
`game_theory_mixed_strategy.py:89`, `sheaf_audit.py:310`, `container_typology_analysis.py:259`,
`meta_reporter.py:100`, `extract_corpus_data.py:250`, `normalize_orbit_ids.py:43`); an inline
`"manifest"` key would be silently read as a fake constraint by all of them. This is recorded as a
design gap (`design_gaps.md` GAP-03). See OQ-51/OQ-52 in `ISSUES.md` for the findings.

**3. Bare-context vacuity extends to `sheaf_status` / Arakelov, not just W1.** A smoke test that ran
`sheaf_status/2` after `[stack] + load_all_testsets` but **without** `maxent_multi_run` returned
`fragile_presheaf=0` (vs 100 in the full pipeline). Arakelov height reads
`maxent_distribution_raw/3`, populated only by the MaxEnt run, so heights degenerate and the p75
fragile/genuine split collapses in a bare context — the same vacuous-path trap as W1
(`test_harness.pl:76`), one layer over. **Compute `sheaf_status` only on the maxent-first pipeline
path; a bare `[stack]` recompute is vacuous (reads as "no fragile presheaves").**

**4. New tool `python/w1_sheaf_join.py` (read + join, no Prolog recompute).** Reads
`pipeline_output.json` (W1=`wasserstein_total_fracture`, `h1_band`, `sheaf_status`) + `orbit_data.json`
(shift vector), guarded same-run by the sidecar; merges on id, sorts descending by W1, writes
`outputs/w1_sheaf_join.{json,md}` (full 564-row ranked table + 2×2 concordance + per-id off-diagonal
rows + the four positive controls). Run at n=563 (commit b5ccee0): W1 sum 33.47, nonzero 112, max
1.904589 (`privilege_architecture_coordination`). **W1-max field-identity CONFIRMED**
(`wasserstein_total_fracture` = sum of the 3 canonical edges, proven on the argmax); the recon's ~4.7
does **not** reproduce and appears nowhere in the repo as a W1 value — likely the longer tail of the
larger archived `testsets_3000` (3,380), **not** staleness or field-misidentification (testsets_3000
max unverified). 2×2 concordance: 58 off-diagonal (36 with H1=0∧W1>0, 22 with H1>0∧W1≈0) — see OQ-51.

## 2026-06-02 — Dirac Axis-1 (`derived_from/3`) removed → design gap; `gauge_fixed/3` straggler fixed
**Files:** prolog/dirac_classification.pl, docs/design/design_gaps.md
**Tier:** tripwire

**If you are editing `dirac_classification.pl` or looking for primary/secondary constraint
tracking — read this.** Two changes landed together; neither is output-changing for the live
pipeline (the affected predicates had no consumers).

**1. `derived_from/3` + `constraint_generation_order/2` removed (Dirac Axis-1, primary/secondary).**
Declared `:- multifile/:- dynamic` so testsets *could* assert derivation chains, read only by
`constraint_generation_order/2`, which was called only by `full_dirac_report/3`. **Zero producers
corpus-wide** — no testset, no generator, no engine code ever asserted a fact (witnessed:
`grep -rln derived_from` over `testsets/`, `testsets_sotu/`, `testsets_3000/` all empty). So
`constraint_generation_order/2` returned `primary` for every constraint via the `\+ derived_from`
cut — absence presenting as a presence (Build Discipline Pattern 5). The module's own header had
already sorted this axis into "merely relabels." Removed: the two export entries, the §4 block
(comment + declarations + both clauses; §5 renumbered → §4), and the `generation_order(Order)`
field of `full_dirac_report/3` (now 7 fields; nothing external destructured it). The capability it
reached for — systematic derivation-chain tracking, with a typed `Reason` slot the live
`affects_constraint/2` edge cannot carry — is now recorded as **GAP-01 in
`docs/design/design_gaps.md`** (new design-doc, a ledger of declared absences; pointer added to
CLAUDE.md "Design intent"). Re-opening is a framework-direction decision, not a code fix; do NOT
re-add an unfed `derived_from/3`.

**2. `gauge_fixed/3:208` straggler fixed.** It still called the removed `standard_context/1` (deleted
in the v2.0 SITE CONTEXTS migration, which moved `gauge_orbit/2` and
`preserved_under_context_shift/2` to `constraint_indexing:site_contexts/1` but missed this one).
Latent because `full_dirac_report/3` — its only path to `gauge_fixed/3` with a real `context(...)`
tuple — has no callers, so the `Unknown procedure` throw never surfaced. Now delegates via
`constraint_indexing:site_contexts(Contexts), member(AltCtx, Contexts)` like its siblings.
Witnessed end-to-end: `gauge_fixed(abrahamic_covenant__isaac_covenant_reading, <analytical ctx>, true)`
and `full_dirac_report/3` returns a complete 7-field `dirac_report(...)` (was: `Unknown procedure:
standard_context/1`).

**Standing note:** `full_dirac_report/3` itself is still a dangling wire (no consumers). It works
now, but if it stays unconsumed it is a candidate for the same removal treatment as Axis-1.

---

## 2026-06-02 — False-summit forensic detector repaired (was vacuous) + two report bugs + stale comment
**Files:** prolog/drl_core.pl, prolog/report_generator.pl, prolog/drl_composition.pl
**Tier:** tripwire

**If you are editing `drl_core.pl` false-mountain detection, `report_generator.pl`'s forensic
audit, or `drl_composition.pl`'s `classify_at_time` temporal comment — read this first.** Four
fixes landed together this session; three are output-changing. Open follow-ups are **OQ-50**.

**1. `drl_core.pl:548` `dr_claim_mismatch(_,_,type_1_false_summit,_)` was a vacuous gate that had
never functioned.** The body was `is_mountain(C, Context, fail)`. `is_mountain/3` has a second
clause `is_mountain(_,_,fail).` (`drl_core.pl:123`) that is an **unconditional catch-all** — it
unifies with any `(C, Context)` because the third arg `fail` matches; clause 1's head (third arg
`mountain`) never unifies with `fail`, so the metric test never runs. Positive control:
`is_mountain(C, boundCtx, fail)` SUCCEEDS while `is_mountain(C, boundCtx, R)` gives `R=mountain` —
i.e. the constraint **is** a mountain yet the `fail` call also succeeds. The trailing `!` then
committed to the **first** mountain-claimer, with `Context` left **unbound** (reports printed
`Context: _NNNN`). Net: the detector returned one arbitrary mountain-claimer — and that one
(`honor_satisfaction_mechanism__contraction_reading`) is a **genuine** mountain (`dr_type=mountain`
at all 4 contexts). It detected nothing and accused the floor.

**Fix: negate `dr_type/3` (post-signature), enumerate `standard_context`, drop the cut.** Now:
```prolog
dr_claim_mismatch(C, Context, type_1_false_summit, severe) :-
    narrative_ontology:constraint_claim(C, mountain),
    standard_context(Context),
    dr_type(C, Context, ActualType),
    ActualType \= mountain.
```
**Why `dr_type`, not `is_mountain` (evidence-settled, not preference).** `is_mountain`
(classify_from_metrics, **pre-signature**) returns non-mountain at the moderate+institutional power
contexts for **all 8** mountain-claimers — a χ=ε·f(d)·σ(S) power-scaling artifact (mid-power shifts
off the mountain band); the signature layer then restores genuine mountains in `dr_type`. So
negating `is_mountain` flags every claimer including the 4 genuine mountains; negating `dr_type`
flags only constraints whose authoritative classification actually departs from the claim. `dr_type`
does **not** call `dr_mismatch`, so no recursion. **Do not "simplify" this back to `is_mountain`,
and do not re-add the cut** (the cut stops the per-context enumeration that locates the break).

Live-corpus result after fix: **4** false summits across **14** (constraint, context) instances —
`papal_temporal_authority_mountain` (moderate+institutional; mountain at powerless/analytical),
`press_reformation_causality__technological_inevitability`, `statutory_debt_ceiling__constitutional_nullity_reading`,
`total_war_winnability_post1945__structural_contraction_reading` (all 4 contexts, never mountain).
The 4 genuine mountains (`honor_…`, `state_killing_…__abolition`, `tsunami_stone_…`, `zero_as_number_…`)
are correctly **excluded**.

**2. `report_generator.pl:445` queried `type_1_false_mountain` — an atom no clause produces.** The
producer emits `type_1_false_summit` (above). The `setof` therefore always failed → the forensic
audit always printed *"All mountains are structurally validated"* whenever any mountain was claimed
(Pattern-5 absence-pass: a dead query reads identically to a clean result). Positive control:
pre-fix old-atom solution count = 0; `type_1_false_summit` solution count = 14. Fixed the atom.
**This means the audit was doubly-dormant: wrong atom queried, and the detector under it vacuous.**

**3. `report_generator.pl:447` miscounted.** Header said "Detected N constraint(s)" using
`length(FalseMountains, N)` where `FalseMountains` is a list of (C, Context) **pairs** — 14 pairs
across 4 constraints read as "14 constraints." Now reports distinct constraints + instance count:
*"Detected 4 constraint(s) … across 14 observer-context instance(s)."* (Vocabulary note: the
report register is **context / observer / perspective**, not "seat" — "seat" is `design_discipline.md`
internal design language and must not appear in product output. There are only 4 observer contexts;
the 14 is constraint×context instances.)

**4. `drl_composition.pl:174` stale comment.** The OQ-41 fail-close comment cited "650/656 rows had
no temporal suppression series" — pre-rebuild (n=656 era) provenance. Engine-measured on the live
corpus: **471/562** carry a temporal `suppression_requirement` series (the temporal path), **91/562**
are scalar-only (hit the STOPGAP fallback), **0/562** reach `unknown` (every constraint authors at
least a scalar). The stopgap scalar clause is **still load-bearing for the 91** — do not delete it
until coverage is complete. Comment updated; code unchanged.

**Untouched, recorded as OQ-50 (do not assume these work):**
- `forensic_explain_false_mountain` (`report_generator.pl:459+`) re-derives its verdict from raw
  suppression/extractiveness heuristics **independent of `dr_type`** — it printed "AMBIGUOUS" for
  `papal` even though the detector correctly flagged it (`dr_type=scaffold≠mountain`). The
  explanation can disagree with the (now-correct) detection.
- Sibling clauses `type_3_snare_as_rope` (`drl_core.pl:555`) and `type_5_piton_as_snare` (`:562`)
  share the **bound-Context requirement** (clause 1 of `is_snare`/`is_piton` computes Chi from
  Context). They are **not** vacuous (they ask for the positive type atom → clause 1's real test),
  but would silently no-op if ever called with `Context` unbound. Currently only reached with bound
  context. Same latent-trap class as the `type_1` bug.

The vacuous catch-all gate is a **new Pattern-5 sibling** (absence-of-a-real-test satisfies via
clause-head unification, not empty-table — see `build_discipline.md` Pattern 5 / OQ-44).

---

## 2026-06-02 — Removed superseded observer-axis husk (saturation_floor) — commit ef92a61d
**Files:** prolog/drl_composition.pl, python/enrich_pipeline_json.py, python/enhanced_report.py, python/run_pipeline.py, python/shared/schemas.py
**Tier:** tripwire

**If you are looking for the `--- HUSK SIGNATURE ---` report section or `saturation_floor` /
`born_saturated` / `husk_metrics` and cannot find them: they were deleted, deliberately.**
Commit `ef92a61d` removed the observer-axis husk machinery. Do not re-add it as "missing."

**Two husks existed; only the observer one was removed. The CS one is live and untouched.**
- **Removed (observer axis, cruft):** `husk_series/3`, `ep_native_series/3`, `husk_exists/3`,
  `husk_point/5` in `drl_composition.pl`; the `husk_report.pl` standalone; `outputs/husk_data.json`
  + `outputs/husk_report.md`; `_load_eps_series` + the `saturation_floor`/`born_saturated`/
  `husk_metrics` block in `enrich_pipeline_json.py`; `build_husk_signature` + the HUSK SIGNATURE
  section in `enhanced_report.py`; the `_prolog_husk` pipeline step in `run_pipeline.py`; both
  `husk_metrics` schema rows in `shared/schemas.py`.
- **Kept (committer axis, live):** `cs_terminal_attractor(..., husk)` in `cs_drift_engine.pl` and
  its 9 consumers (`cs_pattern_detection`, `cs_axiom_engine`, `cs_drift_mismatch`, ...). This is
  the framework's real husk — design-endorsed (`design_discipline.md:344`), reads authored
  `cs_drift_state` gap vectors, NOT the ε series. **The "husk 57" §5.11 count (`ISSUES.md:803`)
  and the `husk_reading` corpus story used as a `:738` positive control are this husk, not the
  removed one — leave them.**

**Why removed (cruft, not a wiring gap).** Provenance: the observer husk landed 2026-05-25 06:43
(`e56bc18c`, "Second round") and was superseded ~4h later by the categorical CS husk attractor
(`624e3b66`, 10:41); the first draft was never deleted. It had **zero engine consumers** — a closed
produce→report loop terminating in a display string the report itself disclaimed as "ε authoring,
not an observed property." Wiring it to the CS engine would be a cross-axis reduction
(`two_axis_architecture_v7.md:124`), i.e. construction, not closing an existing wire.

**Blast radius (witnessed):** one behavioral change — generated reports no longer emit the
`--- HUSK SIGNATURE ---` block. Engine `[stack]` loads clean; the four Python files compile + import
clean; zero dangling references. No classification / χ / drift / purity / CS verdict read any
removed symbol.

**Two loose ends, both still OPEN (not closed by this commit):**
1. The 71 `[ENRICH] WARN ... saturation_floor suppressed` warnings are gone (the gated field is
   gone). No OQ ever tracked them; nothing to retract in `ISSUES.md`.
2. The **real** finding underneath is untouched: static `constraint_data:base_extractiveness/2`
   (the ε χ consumes) understates the depicted ε-series peak for **70/499 (14%) of with-series
   readings, one-sided (0 overshoots), ~2× higher rate among kernel readings (14.4% vs 7.1%)**.
   That is a χ-input question on the observer axis, independent of the deleted report field. It is
   **not** yet an OQ — open one if it graduates from "authoring-convention note" to a classification
   concern.

## 2026-06-01 — Corpus rebuild pipeline built + validated on N=1 (decompose → no-scope gen)
**Files:** agent/generate_kernel_corpus.py, python/merge_kernels.py, python/partition_probe.py
**Tier:** tripwire

**New CLI on `agent/generate_kernel_corpus.py` (default behavior CHANGED).** The script now
has three modes:
- **default = no-scope generation** (no flag): `python3 -m agent.generate_kernel_corpus [N]`
  reads a seed pool (`--seeds`, default `prolog/beta_seeds.json`), takes the **next N
  unprocessed** seeds per `prolog/beta_processed.txt`, generates full stories **flat** into
  `prolog/testsets/` + `json/`, with collision-proof naming (`base` else `base__<uuid8>`,
  checked vs corpus ∪ ladder) and **3× retry** → `outputs/no_scope_runs/failures.json`.
  Seeds carrying `kernel_id`+`reading_id` generate as kernel readings (stamp `cs_kernel_id`);
  others as plain. Repeated calls **advance** the ladder (no treadmill).
- **`--decompose KERNELS_JSON [N]`**: batch-SCOPE (Sonnet) the next N kernels into reading
  **seeds** (constraint-story seeds, NOT stories), namespaced `constraint_id=<kernel_id>__<reading_id>`,
  appended to `prolog/kernel_readings_pool.json`; idempotent via `outputs/decompose/decomposed.txt`.
- **`--scope --run-tag TAG`**: the legacy serial-SCOPE+generate, run-tagged (unchanged).

**Rebuild input assembly.** `python/merge_kernels.py` merges `prolog/kernels/*.json`
(per-model kernel proposals) + `prolog/kernel_seeds.json`, dedups (id OR normalized title) →
`prolog/kernels_merged.json` (**K=200**), and samples K plain seeds from `beta_seeds.json` →
`prolog/toy_plain_seeds.json` (200). `prolog/beta_seeds.json` is the full 3,380 re-harvest
(Phase 0.5).

**Probe finding (why kernels come from authored files, not the archive).**
`python/partition_probe.py` (+ `outputs/partition_probe/validity_analysis.md`): the
prolog_v5 archive is **observer-axis** — a tightened committer-kernel rubric finds **0
kernels / 99** there while detecting **74%** of authored kernels (positive control). So
committer-kernels are sourced from `kernels_merged.json`, the archive supplies plain seeds.

**Validated end-to-end (N=1).** Decomposed `homoousios_christology` → 3 readings → generated
3 `.pl` stories; engine loads, `cs_kernel_coverage(homoousios_christology, 3)`,
`cs_kernel_divergence` fires (semi_arian vs pro_nicene at analytical contexts). The 3
`homoousios_christology__*_reading.pl` in `testsets/` are the live PoC output (ladder records
them). Next forward move: scale incrementally — `--decompose prolog/kernels_merged.json 10`
then generate from `kernel_readings_pool.json`, and `--seeds prolog/toy_plain_seeds.json N`
for plain. Tripwire: a generation quirk (model emits an extra `'description'` property) fails
some seeds on first try but the 3× retry usually recovers; persistent ones land in
`failures.json`.

**Scale run (2026-06-01): decompose-all + generate-100, two engine-level fixes.**
- Decomposed all **200** kernels → **570** reading seeds in `kernel_readings_pool.json`
  (Sonnet batch, $7.21, $0.036/kernel). Generated **96/100** readings (4 skipped, below).
- **FIX — duplicate `story_uid` (engine-rejecting).** The generator minted a UUID only via
  `setdefault`, but Haiku copies the example's placeholder UUID (`550e8400-…`) into every
  story, so 10 stories shared one uid and CS validation halted the corpus
  (`duplicate story_uid`). Fixed at `generate_kernel_corpus.py:520` to **always overwrite**
  `header.story_uid` with a fresh `uuid4` (story_uid is a per-generation surrogate, never
  authored by the content model). Existing files repaired in place (re-mint + replace).
  **Tripwire: do NOT revert to `setdefault` for story_uid** — it readmits duplicates.
- **FOLLOW-UP — reading ids >64 chars are skipped (fail-loud).** `run_no_scope` skips seeds
  whose `constraint_id` exceeds the batch `custom_id` 64-char limit (4 of the first 100, e.g.
  `basic_law_interpretive_authority__parliamentary_sovereignty_reading`). They are logged,
  not generated. To recover them, shorten the `<kernel_id>__<reading_id>` namespacing (e.g.
  hash or abbreviate) in `run_decompose` before re-decomposing those kernels.
- Post-fix corpus loads clean: 102 testsets, 99 cs_story_uid, 33 kernels, swipl exit 0.

## 2026-06-01 — Corpus rebuild Phase 0: old corpora archived, `testsets/` emptied
**Files:** prolog/testsets/, prolog/archives/, python/sweeps/range_sweep.py
**Tier:** tripwire

**What changed.** Start of the kernel-aware corpus rebuild (plan:
`~/.claude/plans/i-rough-sketch-of-steady-squid.md`). Two `git mv`s and a retarget:
- `prolog/testsets/` (229 `.pl` + 11 run-tagged subdirs) → `prolog/archives/prolog_v6/`.
- `prolog/testsets_3000/` (3380 `.pl`) → `prolog/archives/prolog_v5/`.
- Fresh **empty** `prolog/testsets/` (only `.gitkeep`) is now the active corpus — the
  rebuild output destination. **The live engine corpus is empty until Phase 3 generates.**
- The 4 executable overlays that hardcoded `corpus_path='testsets_3000'`
  (`python/sweeps/range_sweep.py`, `python/tests/diff_cut_proof.py`,
  `python/tests/test_battery.py` ×2, `python/tests/alt_power_transform_test_3k.py`) were
  retargeted to `'archives/prolog_v5'`. Positive control: that overlay now loads
  **3380** from the new path (`[corpus] Loading 3380 testset files...`, exit 0).

**Archives are testable.** `prolog/archives/prolog_v5` holds the **3,380**-story pre-rebuild
corpus; `prolog/archives/prolog_v6` holds the prior 229-story live corpus. To test either,
overlay `corpus_path` to `archives/prolog_v5` (or `_v6`) before `load_all_testsets` — the
glob `Dir/*.pl` resolves relative to swipl's cwd (`prolog/`).

**Tripwire — `testsets_3000/` no longer exists; `testsets/` is empty.** A fresh agent that
overlays `corpus_path='testsets_3000'`, or expects the live `testsets/` to hold ~223 stories,
will **silently load 0**. The path is now `archives/prolog_v5`. (CLAUDE.md's "corpus is 223"
distinction is stale during the rebuild — pending end-of-session CLAUDE.md update.) Note: this
is the *archive convention* `prolog/archives/prolog_vN` matching the existing v1/v3/v4, not a
top-level `archives/`.

## 2026-06-01 — `signature_detection.pl`: honest `unknown` now SURFACES (override removed, OQ-37)
**Files:** prolog/signature_detection.pl, python/sweeps/regenerate_orbits.py, python/enhanced_report.py
**Tier:** tripwire

**What changed (commit `c90c5482`).** The FNL/FCR overrides no longer launder an honest
`unknown` modal type into tangled_rope. Two guards added:
- `resolve_modal_signature_conflict(unknown, false_natural_law, unknown)` before the
  unconditional FNL clause (`:738`).
- `resolve_with_perspectival_check/4` false_ci_rope branch (`:685`): `ModalType == unknown ->
  AdjustedType = unknown`.
The reversed comment at `:669-671` ("never preserve unknown") was updated to match.

**Tripwire — do NOT reinstate "never preserve unknown."** That behavior was removed by ruling
(correctness pivot: an honest `unknown` is an *absence* of metric classification — band-gap,
authored gap, or swallowed compute-error — and must stay VISIBLE, not be masked). A future agent
reading the old design intent might "restore" the launder; don't. The `unknown` surfacing is
load-bearing for OQ-37 (it's how a band-gap reading becomes observable).

**Witness.** Corpus-wide set delta (default context, full corpus): `unknown → tangled_rope : 8`
became `unknown → unknown : 8`; **all other (metric→final) rows byte-identical** (snare→tangled
90, scaffold→tangled 6, mountain→tangled 3, snare→snare 20, tangled→tangled 59, rope→rope 2,
scaffold→rope 4, mountain→mountain 2). Same-path positive control: catastrophic_tail / husk /
abolition (metric=snare, sig=false_natural_law — the *same* `:738` clause, non-unknown modal type)
**stay tangled_rope** — the guard does not over-fire. Validation suite 0 errors / 0 warnings.
N=8 masked-unknown population = 5 diagnosed (4 taxonomy holes / 1 authored gap, see ISSUES OQ-37)
+ 3 uncharacterized (`constitutional_supremacy_reading`, `hybrid_atrophy_reading`,
`relational_autonomy`).

**Consequence — orbits regenerated.** The change altered 8 dr_types, so
`outputs/product_site_orbits.json` (perturb.py's baseline, gitignored) was regenerated
(`python/sweeps/regenerate_orbits.py`, corpus_hash `0d2ecfce17ae`). perturb.py's staleness guard
checks only the *testsets* hash, **not engine state** — so after any engine edit that changes
classifications, regenerate orbits manually or every stability-band comparison reads a stale
baseline silently.

**Ledger.** `boltzmann_coupling_threshold` added to `enhanced_report.py` `_WITNESSED_PARAMS`
(equal_protection_clause, sovereign_legitimacy) as the Surface-2 lock lever (commit `739979c6`).
Co-lever `coordination_type_offset` is per-constraint (`boltzmann_compliance.pl:388`), NOT a flat
config param — it is **not** perturb-sweepable; documented in-comment, do not add it to
`_WITNESSED_PARAMS` (perturb would raise `param not found`).

---

## 2026-05-31 — Surface-2 primitive built; lock hypothesis witnessed (lever was misnamed)
**Files:** python/sweeps/surface2_lock_sweep.py, prolog/boltzmann_compliance.pl, prolog/signature_detection.pl
**Tier:** correction-key

**New tool.** `python/sweeps/surface2_lock_sweep.py` — the Surface-2 primitive (PoL graduated to
instrument). One swipl process, corpus loaded once, in-memory `retract/assertz` overlay of THREE
Boltzmann levers swept INDEPENDENTLY (never bundled): `boltzmann_floor_*` (observable
`excess_extraction`), `boltzmann_coupling_threshold`, `coordination_type_offset` (both gate
`boltzmann_compliant` via `complexity_adjusted_threshold = base + offset`). Does NOT extend
`perturb.py` (Surface 1). Derives its target in-engine (no inherited list). Results:
`outputs/surface2_lock_sweep_results.json`. Runs in ~2s.

**The handoff/ISSUES lever was wrong — corrected and witnessed.** Handoff 6/7 and ISSUES OQ-30
named `boltzmann_floor_*` as the lock lever (the −0.52 PoL moved `excess_extraction` via the floor).
But the FNL/CI_rope override gates do NOT consume `excess_extraction` (`signature_detection.pl:927-930`
removed that gating); the lock gate is `boltzmann_compliant`, driven by `cross_index_coupling` vs
`boltzmann_coupling_threshold + coordination_type_offset` (`boltzmann_compliance.pl:380-383`).
Perturbing the floor moves excess but leaves `boltzmann_compliant`/signature/`dr_type` unchanged for
the FNL majority.

**Witnessed (perturb-confirmed, full 96-reading sweep).** 96 Boltzmann-gated locked readings
(FNL 76, FCR 17, CI_rope 3); 56 load-bearing (override changes final type), 40 over-included
(final == metric — bare signature-read over-includes by 40). `boltzmann_coupling_threshold` flips
48/56 load-bearing final types (0/40 over-included — clean control). Floor flips only 5/96, all in
the Boltzmann-*compliant* CI_rope/FCR cluster (excess gates `false_ci_rope` via `collect_fcr_failures`,
priority 77 > CI_rope 114). `coordination_type_offset` is a real second lever (48 flips, same set).
Combined Surface 2 witnesses 50/56 load-bearing; 6 residual immovable (5 metric=unknown FNL re-pin to
tangled_rope; 1 has a per-constraint `boltzmann_floor_override` shadowing its floor). Original "floor
flips the locked kernels" hypothesis FALSIFIED as the primary lever; corrected coupling-threshold
hypothesis WITNESSED → Surface 2 is the critical path.

**No engine source was edited** — overlay is runtime-only. Two positive controls passed before the
sweep (PoL floor flip reproduced on `civic_eugenic_reading`; coupling overlay moves `boltzmann_compliant`
on `abolition_reading`). Set-not-count caught the 5 non-uniform floor flips an aggregate "0/N" would
have hidden.

**Witness-tier note (2026-06-01).** The per-reading row dump (48 coupling type-flips) was reported as
"48 rows pasted" but the terminal output **truncated** (only rows ~44–48 + the total survived). The
row-level witness tier is therefore **structure-closed + substrate-regenerable** from
`outputs/surface2_lock_sweep_results.json` (committed at `db66cc53`) — **not pasted-to-reviewer**. No
re-paste needed; regenerate from the per-value `sig`/`final` fields if the rows are wanted.

---

## 2026-05-31 — Commit A: row-23 fail-close in `drl_composition.pl` `classify_at_time` (OQ-41)
**Files:** prolog/drl_composition.pl
**Tier:** tripwire

**What changed.** `classify_at_time/4` no longer fabricates `Supp=0.5` when the temporal
`suppression_requirement` measurement is absent. New order: temporal measurement → else authored
**scalar** `constraint_metric(C, suppression_requirement, _)` → else `unknown`. Body factored into
helper `classify_at_time_with_supp/5` (module-private; `current_predicate/1` from `user` won't see it
— qualify as `drl_composition:`).

**Why the fix is scalar-fallback, not the literal `unknown` ruling.** Positive control found 650/656
temporal-timeline rows lack the temporal *series* but **all 650 carry an authored scalar** suppression
(genuine-no-data = 0). Returning `unknown` would discard real authored data — the same absence-as-value
sin as `Supp=0.5`. Witnessed impact: **268 rows corrected** vs the old fabricated 0.5 (185
tangled_rope→snare, 58 unknown→snare, 9 scaffold→mountain, 6 rope→mountain, 10 tangled_rope→unknown);
the absence `unknown` floor fires 0×; validation suite 0 errors / 0 warnings.

**STOPGAP — do not harden.** The scalar clause is a labeled temporary bridge. It is retired by
**OQ-46** (generation template must author a temporal suppression series; then delete the scalar clause
and let the temporal path stand alone). **Do not build a scalar/temporal equivalence check on it.**
The regen that retires it is gated behind the SCOPE→seed seam audit (**OQ-47**).

**Row-26 (same OQ-41) measured NEUTRAL** — `outputs/tripwire_row26_results.json`; the guard-falsity
count shortcut was caught vacuous by its positive control (guards succeed for a bogus constraint), so
the 999.9 branch-reachability tripwire is the sound test. Commit B (behavior-preserving: D2 strips, D5
row-14, D7 schema gate, D3 NL-gate fail-close + OQ-45, D6/D8 docs) is **not yet applied** — it rides
behind review of Commit A.

**Downstream-consumer audit of the 268-row shift (produced-but-not-consumed-at-a-seam check).** The
fix changed exactly one producer, `classify_at_time/4`. Traced every consumer of temporal types:
- **`classify_at_time` has ONE live Prolog consumer:** `cs_kernel_registry:cs_kernel_divergence/4`
  (at T=0), surfaced by `json_report.pl:1368` as `cs_kernel_divergence_count` in `pipeline_output.json`.
  **Persisted COUNT fields are invariant under the fix** — `cs_kernel_divergence_count` = 79 (new == old),
  `cs_kernels_with_divergence`, `diverging_pair_count` unchanged (0 pairs added/removed at pair
  granularity), confirmed in both `pipeline_output.json` and `enriched_pipeline.json`. **But the
  per-CONTEXT divergence set is NOT identical: 6542 → 7184 (+642) — the fix surfaces genuine
  divergence the fabricated 0.5 was homogenizing.** (Set-identity check, not just count.) **No
  persisted JSON carries the per-context map** — only counts — so no pipeline artifact is stale from
  this. The per-context shift is consumed by `cross_reading_diff.py` (live diagnostic, regenerates) and
  the **docs/memory `253/468` per-context numbers (`project_cs_kernel_registry.md`), which ARE
  stale-pending-regen.** pipeline_output.json is regenerated by `run_pipeline.py` (case (a); already
  generally stale vs HEAD, predating this commit; Commit A adds no new *persisted* staleness).
- **`constraint_history` / `transformation_detected` / `degradation_chain`** (full-timeline
  classify_at_time, where the 268-row shift lives): **no live consumer** — only internal callers + the
  sweep harnesses. The shift lands in an unconsumed producer.
- **`snapshot_type/3` (transition_paths) was NOT changed and already uses the scalar fallback**
  (`drift_events:safe_metric`, tier 2) — so the fix incidentally **converges** the two temporal paths
  that previously disagreed (classify_at_time=0.5 vs snapshot_type=scalar). Its consumers are unaffected.
- **All other pipeline temporal/drift fields** (`drift_events`, `drift_trajectory`,
  `transition_boundaries`, `cs_drift_terminal`, `cs_axiom_foreclosed`, `cs_drift_unacknowledged`) are
  produced by `drift_events`/`grothendieck_cohomology`/`cs_drift_engine`/`cs_axiom_engine`, **none of
  which call `classify_at_time`** — unaffected, regenerate from engine.
- **Docs/memory CS divergence numbers** (`project_cs_kernel_registry.md` 253/468 per-context): **STALE
  — the per-context divergence map shifted +642 (above); regenerate before citing.** (Correctly so:
  real suppression surfaces real divergence.)
**Verdict: no silent staleness blocks Commit B.** Persisted pipeline counts are measured invariant; the
per-context shift touches only regenerating live diagnostics + the now-flagged doc/memory numbers.

## 2026-05-31 — Commit B LANDED (behavior-preserving batch behind Commit A)
**Files:** prolog/signature_detection.pl, prolog/constraint_bridge.pl, python/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md
**Tier:** tripwire

Applied: **B1** NL-gate fail-close (`signature_detection.pl` `count_power_beneficiaries` now reads the
authored `constraint_beneficiary` table, not the empty `intent_power_change` join) — gate discriminates
(conception_reading 0→2), **live NL certs 5→2** (3 false natural-laws with authored beneficiaries
correctly declined; recorded as a finding per the D3 framing, not reverted). **B2** stripped the dead
`inevitability` clause of `constraint_bridge:constraint_status/3` (uncalled predicate; corpus loads
clean, validation 0/0). **B3** removed the unenforced "suppression must decline over time" scaffold
clause from `prompts/constraint_story_generation_prompt_json.md:27`. **B4** stripped the
`accessibility_collapse`/`resistance` thresholds from the mountain `allOf` gate in the canonical
`python/constraint_story_schema.json` (kept `emerges_naturally`/`extractiveness`/`suppression`; JSON
valid; generator still EMITS the two fields as documentation). **B5/B6** docs (D6 defer, D8 correction).

**Deferred / found during B (not done, with reason):**
- **`internalization_depth` strip (row 9) NOT done** — its only reader is `psych_bridge:with_psych_metric`,
  and **`psych_bridge` is never loaded by `stack.pl`** (dead, unloaded module). The read is doubly
  inert; the real action is whole-module removal, which is a dead-module audit (OQ-38 family), not a
  metric-read strip. Do not "strip the read" — remove or revive the module deliberately.
- **`resistance_to_change` strip (row 10) deferred** — reached by live report paths
  (`json_report:237` emits a field, `utils:safe_get_all_metrics/2`, `data_validation`); not a free strip.
- **Schema fork noted, NOT reconciled:** `python/constraint_story_schema.json` (canonical, loaded by
  `generate_constraint_pl.py`) and `agent/data/constraint_story_schema.json` DIFFER. B4 edited only the
  canonical one. The `agent/data/` copy is a pre-existing divergent fork (build-discipline Pattern 2) —
  resolve separately; confirm whether the orchestrator ever loads it via the `_load_context_file` path.

Original prep notes (the framings that guided B):

- **G5 CLOSED (bonus from Commit A): the two temporal classification paths now agree.** Before Commit A,
  `classify_at_time` fabricated `Supp=0.5` while `snapshot_type/3` (transition_paths) already used the
  authored scalar (`drift_events:safe_metric`) — a silent G5 scalar-vs-temporal split on suppression.
  The row-23 scalar-fallback converges them. Logged against OQ-40. (The split reopens if the row-23
  stopgap is removed without the generation template authoring a temporal series — see OQ-46.)
- **D3 framing for B1 (NL-gate fail-close) — a count shift can be the CORRECT outcome.** Success
  criterion is **"the gate stops passing on absence"**, NOT "the mountain/NL count stayed at its old
  value." A change in mountain count or the 404 NL count after fail-closing is a *possible correct
  result* (the gate was certifying by absence), not a regression. Witness the gate *behaviour*
  (passes only when a beneficiary datum was authored), and report any count delta as an expected
  consequence, not a failure. (T.1's "0 mountain change" was the prior observation, not the pass bar.)
- **D2 refinement from the caller-chain check — not all three dead reads are free strips.**
  `inevitability` (`constraint_bridge.pl:22`, in `constraint_status/3`) has live callers only in
  `archives/` → free strip. `internalization_depth` (`psych_bridge.pl:19`, `with_psych_metric/2`) has
  zero callers → free strip. **`resistance_to_change` is NOT a free strip** — read inside
  `safe_get_all_metrics/2` (utils), `json_report.pl:237` (emits a pipeline-JSON field), and
  `data_validation.pl:300/309`, all reached by live report paths; the metric is 0/0 so the read always
  defaults, but removing it changes report output. Handle deliberately (drop the emitted field + its
  consumers in one change, or leave it).

## 2026-05-31 — Legacy bullets imported from CLAUDE.md (2026-05-28 → 2026-05-31 items)
**Files:** prolog/product_site_export.pl, prolog/config_validation.pl, python/sweeps/perturb.py, python/sweeps/demotion_pass.py, python/enhanced_report.py, agent/generate_kernel_corpus.py, prolog/signature_detection.pl, prolog/drl_composition.pl
**Tier:** history

<!-- BODY: verbatim from CLAUDE.md Known State section as of 2026-05-31 -->
- **Corpus is 223 constraints (not 3,337).** The reduction reflects a deliberate rebuild:
  exploratory committer-axis generation runs reused constraint IDs across runs (the
  "chimera" documented in OQ-25 and v7 §5.11 "corpus provenance" note). Cleanup triaged
  collisions, archived stale duplicates, and reduced testsets/ to a single coherent run
  (kernel_run_03: 109 CS readings + ~114 observer-axis constraints). §5.11 trifurcation
  figures are verified single-run coherent. The 3,337 figure predates the rebuild.
- **Run-tagged subdirs (`prolog/testsets/<run_tag>/`) are isolated** — `corpus_loader.pl`
  uses a non-recursive glob (`testsets/*.pl`), so subdir stories are NOT loaded by default.
  This is **load-time** safety, not generation-time dedup. If `corpus_path` is ever changed
  to include a run-tagged subdir, or runs are flattened together, duplicate loading becomes
  live. The shield is the glob; removing it reopens the question.
- Last audit (2026-02-28): passing tests / param sweep — live items migrated to ISSUES.md (OQ-11 – OQ-13, all resolved 2026-06-04); historical record was AUDIT.md, deleted at tracking-surface consolidation 2026-06-04 (full text in git history, last at commit a1140d0d)
- Config params: see `prolog/config.pl` for current count (`grep -c "^param(" prolog/config.pl`)
- All numeric params inert at ±25%; all 17 directionality constants inert at ±25%
- Corpus is actively growing; param count and testset numbers will drift — cite the manifest
- **2026-05-28: green cut applied to `product_site_export.pl:75–77`** — added `!` after
  `write_one_entry` in `write_entries` clause 3 to enable LCO and fix OOM under
  compressed-ceiling sigmoid variants. Zero-diff verified (3,380 constraints, before/after
  outputs in `outputs/cut_proof_*.json`). Underlying choice-point question is OQ-02 in
  `ISSUES.md`.
- **2026-05-28: python/ phase-1 reorganization** — 8 tests → `python/tests/`, 12 sweeps
  → `python/sweeps/`, 19 audits → `python/audits/`. Frozen CLI commands
  (`run_pipeline.py`, `enhanced_report.py`, `config_sensitivity_sweep.py`,
  `directionality_sensitivity_sweep.py`) and all load-bearing pipeline modules stay in
  `python/` root. ~30 exploratory scripts stay (phase 2 pending). sys.path fixes applied
  to all 39 moved files. Verification script: `python3 python/verify_reorg.py`.
- **2026-05-28: v6 of observers_not_humans paper — §2.3 correction** — Sign-flip is
  load-bearing only in tangled_rope constraint family, not corpus-wide. Empirical
  concentration: Jaccard +0.21 in tangled_rope vs +0.014 in snare+rope (14.6× difference).
  H0 (sign-flip is load-bearing) conditionally confirmed; condition is that rope-gate
  bypass behavior is treated as given (OQ-01 in `ISSUES.md`). Corrected universality-class
  claim from corpus-wide to regime-specific. Unified §2.3 and §3.3 as one mechanism
  (institutional sign-flip at d < d_zero) viewed at two resolutions. Jaccard range
  corrected to 0.697–0.833 from published v5 range 0.685–0.828 (full-corpus rerun,
  3,380 constraints, testsets_3000). See `docs/observers_not_humans_v6.md` and witness
  files `outputs/alt_power_transform_results.json`, `outputs/range_sweep_results.json`.
  OQ-05 and OQ-09 resolved.
- **2026-05-28: OQ-25 resolved — ε coherence load guard** — `config_validation.pl`
  now includes a `config_violation/1` clause that fires inside `validate_config_postcorpus`
  (called at end of `corpus_loader:load_all_testsets`). Rejects any load where the same
  ConstraintAtom carries two distinct `constraint_metric(C, extractiveness, E)` values —
  the chimera failure mode. Grouping key is ConstraintAtom (not KernelAtom; OQ-26
  rationale). §5.11 divergence count confirmed unchanged (79 pairs / 34 kernels).
  See `docs/cs_load_discipline.md` (regeneration protocol) and
  `docs/technical/config_validation_wiring.md` (implementation notes).
- **2026-05-29: kernel-linkage join wired** — `agent/generate_kernel_corpus.py` is now
  canonical (6 evidence signals; `commitment_corpus/generate_kernel_corpus.py` and
  `commitment_corpus/uke_scope_v2_json.md` deleted). Fix applied: `story_uid` now minted
  before `_kernel_id` injection in `process_batch_results` (ordering gate); `stamp_kernel_linkage`
  post-batch function added. Migration script `python/migrate_kernel_linkage.py` wrote
  `cs_contradiction_of` facts into 32 `*_contradictions.pl` files (idempotent, all SKIP on
  second run). 22 orphaned readings listed in bucket B (hand-confirm worklist); 72
  candidate standalones in bucket C (eyeball only). Validation suite: clean after all edits.
  `cross_reading_diff.py` on `end_of_life_decision_authority`: 3 readings, no warnings.
- **2026-05-29: build-discipline patterns documented** — two recurring defects named in
  `docs/technical/build_discipline.md`: produced-but-not-consumed and silent-fork.
  See build_discipline.md for diagnostics and the corpus-you-want naming rule.
- **2026-05-30: Pattern 3 added to build_discipline.md** — bound-probe bypasses clause-order
  (query-binding-bypasses-cut). Bound `findall(C, constraint_signature(C, natural_law), Cs)`
  over-counts by bypassing lock cuts (`false_natural_law:70`, `false_ci_rope:77`,
  `false_summit_mountain:87`). Live demo: bound form yields `[behavioral_competence_reading]`,
  unbound+post-filter yields `[]` (actual sig: false_summit_mountain). Fix: query unbound,
  post-filter with `== natural_law`. See build_discipline.md Pattern 3.
- **2026-05-29: perturb() primitive implemented** — `python/sweeps/perturb.py` is the
  type-stability sweep primitive: `perturb(param, values) → re-export → fold-survival per
  kernel`. Uses Dialect A1 overlay (retract/asserta on config:param/2) + product_site_export
  re-export. Output schema: {fold_survival, stable, flipped, touched, coverage, per_reading}
  per kernel per param value. coverage=0 means "blind, not stable" (param didn't reach
  kernel's decision path at this value). Verified: determinism (byte-identical double-export
  diff=0), identity (snare_epsilon_floor=0.46: 0 kernels affected), detection (0.50:
  end_of_life_decision_authority fold_survival=0.917, coverage=0.167, 39 flips in
  vulnerability_protection_reading institutional contexts tangled_rope→naturalized).
  product_site_export must be explicitly loaded in overlay ([stack] alone does not load it).
  OQ-29 opened: 19/19 results files have no corpus_hash; bifurcation_results.json confirmed
  stale (7 flipping constraints are testsets_3000/ archive only, absent from live testsets/).
  dval_sweep does not exist in repo (grep exit 1). cross_reading_diff.diff() is the design
  model for the diff shape; the primitive has its own re-export loop. 5 type-stability sweeps
  collapse to perturb(); 9 resistant sweeps stay separate by design (see ISSUES.md OQ-29,
  plan file audit-only-do-not-functional-kay.md §6.1).
- **2026-05-29: stability band wired into enhanced_report.py (Phase 1 + Phase 2)** —
  `python/enhanced_report.py` now runs perturb() at generation time for kernel-linked
  constraints with confirmed governing params, renders a stability band section (E5), and
  writes `stability_band` to the JSON sidecar. Confirmed governing param: `snare_epsilon_floor`
  × `end_of_life_decision_authority` kernel (boundary at +8.7%, 39 flips; floor at +4.3%,
  no coverage). All other kernels render "not yet witnessed." Unlinked constraints render "no
  kernel linkage." Architectural finding: 76/97 kernel-linked readings have `false_natural_law`
  signature (unconditional tangled_rope) — chi_floor params reach the metric decision path
  (coverage>0) but the final type is signature-locked; they are NOT valid governing params.
  17/97 have `false_ci_rope` (conditional); 3/97 `coupling_invariant_rope`; 1/97
  `constructed_low_extraction`. `tangled_rope_chi_floor` is blind or signature-locked on all
  tested kernels. Phase 2 restructure: kernel cross-reading panel moved to top (immediately
  after verdict banner); Wasserstein, cohomology, game-theory, Level-3 distribution and
  structural sections deleted (not stubbed; option a taken — git diff 7af6b945 confirms
  five `-def` removals). File: 2670 lines (was 2836; 2698 was mid-session before deletion).
  OQ-31 resolved. Sidecar validator unchanged
  (extra fields pass silently).

- **2026-05-29: predicate denominator established + full 191-param sweep complete** —
  Bidirectional dataflow trace: 191 engine params (168 config.pl + 23 supplementary) +
  6 authored fields = 197 static-type surface. Three surfaces distinguished (static type,
  PoA, temporal/drift). 6 positional_displacement tagged SHADOWED. OQ-32 fixed (6 sweeps).
  Float ±10% batch (179 params): 21 survivors (pre-batch 2 + new 19). Integer ±1 batch
  (19 errored-untested): 3 more survivors (boltzmann_min_classifications, critical_mass_threshold,
  fcr_override_enabled). Total: 24 survivors. All wired into `_WITNESSED_PARAMS` (18 kernels,
  enhanced_report.py) and `_WITNESSED` (demotion_pass.py). Final demotion_pass:
  6 shadowed + 0 errored-untested + 20 unperturbable + 0 reachable-locked + 24 witnessed +
  141 backlog = 191. Results: `outputs/witness_backlog_results.json` (float),
  `outputs/witness_backlog_integer_results.json` (integer). Fisher probe wired into E5
  (all stability-band paths). Priority sort bug fixed. OQ-30 mitigated (18/38 kernels
  witnessed). `docs/engine_handoff.md` §2(a) updated with denominator and survivor section.

- **2026-05-30: 4 epsilon params characterized; all 141 backlog params now exhausted** —
  `--resume` confirmed all 141 PERTURBABLE_UNPERTURBED params already in results (swept at
  end of prior batch due to priority bug; not skipped). Corrected tiering for the 4 epsilon
  params: (1) `rope_epsilon_ceiling` split-tier: +10% permanently blocked by
  `config_schema.pl:482–487` `classification_rope_snare` invariant (`rope_epsilon_ceiling >=
  snare_epsilon_floor` → export_failed); −10% reachable-stable (23 kernels, fs=1.0, 0 flips).
  (2) `tangled_rope_epsilon_floor` perturbable-but-unperturbed EARNED: 25–26 kernels reached
  across full ±10% band, fs=1.0 on all — genuine stability finding. (3) `fpn_epsilon` and
  `piton_epsilon_floor` unreached-at-tested-range: coverage=0 or near-0 at ±10%; flip
  potential unknown; wider range required. Bucket split within 141: 2 unreached-at-tested-range
  (fpn_epsilon, piton_epsilon_floor); 139 remainder (includes rope_epsilon_ceiling one-sided
  and tangled_rope full-band). Top-level 191 count unchanged. OQ-30 updated.

- **2026-05-30: Surface 2 + Surface 3 perturbation primitive scoped (proof-of-life)** —
  Observable identified and proven per surface. Scripts: `python/sweeps/proof_of_life_surface2.py`,
  `python/sweeps/proof_of_life_surface3.py`.
  
  **Surface 2** (`excess_extraction/2`, `boltzmann_compliance.pl`): MOVED. Observable =
  `boltzmann_compliance:excess_extraction(C, ExcessEps)`. Overlay = `config:param/2`
  retract/assertz on `boltzmann_floor_identity_coordination` (0.08→0.60) for
  `civic_eugenic_reading`. Baseline: 0.60, perturbed: 0.08, diff: −0.52. Floor path
  confirmed as coordination_type (not override, not default) — overlay valid, not shadowed.
  Cache confirmed 0 before and after clear. Full primitive observable:
  `excess_extraction(C, ExcessEps)` per constraint per param value. Coverage analog:
  if `boltzmann_floor_for/2` takes the override path, perturbing the floor param is
  shadowed (coverage=0) — same blind-green trap as Surface 1.

  **Surface 3** (`constraint_history/3`, `drl_composition.pl`): NOT MOVED — with diagnostic.
  Observable = `constraint_history(C, Ctx, Timeline)` → `[state(T, Type), ...]`. Overlay =
  `narrative_ontology:measurement/5` retract/assertz (dynamic, confirmed). Constraint
  `civic_eugenic_reading` baseline at T=4: `unknown` (not tangled_rope). Perturbed
  base_extractiveness T=4 (0.68→0.95): Chi=1.30 > snare_chi_floor=0.66 and ε=0.95 >
  snare_epsilon_floor=0.46 — both snare thresholds crossed — yet type remains `unknown`.
  Binding variable: theater_ratio=0.55 at T=4 vs 0.42/0.48 at T=0/T=2; Supp=0.5 fallback
  at all time points. The piton gate (reading theater_ratio via nb_setval) appears to block
  at theater=0.55 without completing, leaving a gap where neither piton nor tangled_rope
  fires. Not-moved is a valid scoping output: observable confirmed, overlay confirmed,
  wrong metric targeted for this time point. Full primitive: use T=0 or T=2 as perturbation
  anchor (baseline tangled_rope) OR include theater_ratio as perturbable metric.

  **Reconciliation of prior-session claim**: "boltzmann_floor_override dead-ends at
  line 453" was correct at Surface-1 granularity (product_site_export never calls
  excess_extraction or boltzmann_floor_for — the control break holds). At Surface-2
  granularity it was imprecise: boltzmann_floor_for/2's output IS consumed by
  excess_extraction/2 and 14+ callers in drift_events.pl, drl_boltzmann_analysis.pl, etc.
  Both claims are true at their respective surface levels.

- **2026-05-30: 6 authored fields graduated from trace-asserted to grep-witnessed +
  perturb-confirmed** — All 6 live on Surface 1 (product_site_export → dr_type/3). Path
  split: extractiveness/suppression/theater_ratio/d_value reach classify_from_metrics/6
  via argument slots (BaseEps, Supp, TR lookup on C arg, Chi); accessibility_collapse/
  resistance reach dr_type/3 via integrate_signature_with_modal/3 (signature override
  layer, called AFTER classify_from_metrics in dr_type/3) — NOT through
  classify_from_metrics/6 arg slots. 197 denominator confirmed. Per-field type flips
  pasted in docs/engine_handoff_4.md witness-tier ledger. Key corpus fact: only 2
  constraints currently get natural_law signature with Sig unbound (as the engine calls
  it): explanatory_closure_mechanism, state_role_time_collapse. Liveness testing for
  AC/resistance requires testsets from this narrow set; most naturally-emerging
  constraints in the corpus get false_natural_law, false_ci_rope, or
  false_summit_mountain (which fire first). See
  docs/technical/signature_detection_wiring.md for query gotchas.

- **2026-05-30: Authoring-closure + fabricated-default census (OQ-33 updated)** —
  Full audit run; all 7 OPEN graduation steps executed. Key corrections to prior claims:
  (1) D1a (drl_composition.pl:179, Supp=0.5): LOAD-BEARING-WRONG confirmed. Tripwire
  yields 279/647 temporal rows changed: 219 tangled_rope→snare + 60 unknown→snare, 0→unknown.
  The plan's instance-reported "443 unknown flips" was WRONG — direction is reversed.
  snare_suppression_floor=0.60 blocks Supp=0.5 from snare; 50.4% of non-unknown temporal
  classifications are systematically mis-classified too low (tangled_rope instead of snare).
  (2) D2 (drl_core.pl:96, Supp=0): DORMANT, not LOAD-BEARING-WRONG. The 32 testsets
  missing suppression_requirement are _contradictions.pl stubs, excluded by
  all_corpus_constraints/1 (requires extractiveness metric). Tripwire: 0 changes on 191
  classified constraints. (3) D20/D21 (boltzmann_compliance.pl:245/251): DORMANT for
  same reason as D2. (4) D1b (drl_composition.pl:180, BaseX=0.5): LATENT-TRAP confirmed —
  fallback unreachable via constraint_history (all measurement time points have BaseX data).
  (5) requires_active_enforcement IS on main classification path (drl_core.pl:371/277/286) —
  A\P gap CLOSED. Scripts: python/sweeps/tripwire_fabricated_defaults.py.
  Results: audits/2026-05-30_authoring_closure_fabricated_defaults/tripwire_fabricated_defaults_results.json (moved from gitignored outputs/ 2026-06-11, OQ-33 close).
  Audit: audits/2026-05-30_authoring_closure_fabricated_defaults/audit_authoring_closure_fabricated_defaults.md. OQ-33 updated.

- **2026-05-31: NL circularity audit — cosmetic relabel, not manufacturing** —
  T.1 (testsets_3000, 3380 constraints): the 404 natural_law-signature constraints
  are 100% bucket A (metric-real mountains). eps range 0.00–0.22, supp range 0.00–0.04,
  all pass both mountain metric gates (eps≤0.25, supp≤0.05) with emerges_naturally.
  Bucket B = 0/404 — the NL→mountain signature override manufactures zero mountains.
  The AC=0.92 authoring stamp is cosmetic: removing the NL override changes the mountain
  count by zero (engine witness: NL=404 before and after strip).
  T.2: prompt `accessibility_collapse ≥ 0.85` threshold stable from first commit
  (`51033e8a 2026-02-21`) through entire testsets_3000 generation window. 84.3% of AC
  values are exactly 0.92 (one stable prompt regime, not drift).
  Generator strip artifacts: `fix/stripped_schema.json` and `fix/stripped_prompt.md`
  remove AC.minimum=0.85 and resistance.maximum=0.15 from the mountain allOf branch
  and matching prompt instructions; keep `extractiveness.maximum=0.25`,
  `suppression.maximum=0.05`, `emerges_naturally` intact. `ab_test/stripped_*` files
  over-strip (also remove ε and supp constraints) — do not reuse.
  Engine-insensitivity witnessed; generation-side stamp removal requires a live
  generation run with DR_GEN_PROMPT/DR_SCHEMA pointing to `fix/` artifacts.

- **2026-05-31: Empty-table pattern scoped (affects_constraint / intent_power_change)** —
  **CORRECTION (D8/OQ-42): `affects_constraint` is NOT empty** — it is a populated network edge
  (520 facts live / **9305 in testsets_3000**). Only `intent_power_change` (and the wider `intent_*`
  family) is genuinely empty (0/0 both corpora). The original claim here conflated the two; the
  empty-table finding holds only for `intent_*`. 10 distinct engine consumers identified via grep on
  prolog/*.pl. Two were SILENT-SAT; eight are SKIP-safe. SILENT-SAT consumers:
  (1) `signature_detection:count_power_beneficiaries/2` — **RESOLVED 2026-05-31 (Commit B1)**: it no
  longer joins the empty `intent_power_change`; it now reads the authored, populated
  `constraint_beneficiary` table, so `BeneficiaryCount==0` in `natural_law_signature` is a checked
  condition, not a vacuous pass. Live NL certifications dropped 5 → 2 (3 false natural-laws with
  authored beneficiaries correctly declined). (Supersedes the prior "cosmetically redundant / bailed
  out" note.)
  (2) `data_verification:verify_interval_completeness` — `forall(intent_beneficiary_class,
  intent_power_change)` vacuously succeeds; test-harness-only, not classification pipeline.
  No live classification bugs from empty tables. All eight SKIP-safe consumers either
  fail-and-backtrack or return empty findall lists with correct downstream behavior.
  Key architectural distinction: `natural_law_signature` checks BC via
  `count_power_beneficiaries` (reads `affects_constraint`/`intent_power_change`,
  EMPTY); `false_summit_mountain` checks beneficiaries via `constraint_beneficiary/2`
  (static authored facts, POPULATED for the 15 FSM targets). These are DIFFERENT
  predicates — FSM firings are real and unaffected by the empty interval tables.

- **2026-05-31: Build discipline Pattern 3 in live audit** —
  Calling `constraint_signature(C, natural_law)` with Sig BOUND bypasses the priority
  cascade (FNL/FCR/FSM clause heads fail to unify → bodies never run → cuts never fire).
  Bound form found 432 "NL" constraints; unbound form found 404 (the correct engine
  count). The 28-gap constraints get FNL or FCR in the real cascade but pass the NL
  body when queried directly. Always call `constraint_signature(C, Sig)` with Sig
  UNBOUND and post-filter for `Sig == natural_law`. Documented in
  docs/technical/signature_detection_wiring.md query gotchas.

- **2026-05-31: NL beneficiary gate is satisfy-on-absence, not belt-and-suspenders (OQ-43)** —
  Gap check (testsets_3000): of the 404 `natural_law`-signature constraints, **0/404** carry a
  `constraint_beneficiary/2` fact (corpus holds 6739, none on the 404) and **0/404** carry an
  `intent_power_change` beneficiary. `intent_power_change` is empty corpus-wide (0 facts), so
  `natural_law_signature`'s `BeneficiaryCount == 0` gate (`signature_detection.pl:295`) passes by
  absence for every constraint — dormant-over-empty-table, not a discriminating check. FSM coverage
  of the NL population is **0/404 by cascade construction** (FSM at `:87` requires a beneficiary fact
  and catches every beneficiary-bearing mountain before the NL clause at `:97`; the NL residue is the
  beneficiary-blind set). The `:84–86` source comment claiming FSM makes the NL gate "belt-and-
  suspenders" was **corrected** — it was false for the 404. The 404 NL certifications mean "no
  beneficiary **authored**," not "no beneficiary **exists**"; activating the gate is a content
  re-audit of the 404, not engine maintenance. Same satisfy-on-absence class as OQ-41 (G6 0.5
  defaults) and OQ-36/OQ-37 (empty `intent_*`) — policy decision (fail-closed vs keep-vacuous-pass)
  should be made once across the class. See ISSUES.md OQ-43.

- **2026-05-31: NL-gate fix is a diagnostic-layer decline, NOT classification-changing**
  **(corrects the handoff_6 ~:221 "3-case tail" / "cosmetic must not be cited unqualified" claim)** —
  VERIFY-OR-CORRECT pass re-derivation. The B1 NL-gate fix declined **3 raw `natural_law_signature`
  certifications (raw match 5→2)** — TRUE and a real diagnostic-layer improvement (the gate now
  discriminates on authored beneficiaries; all 3 carry ≥1 `constraint_beneficiary`, the 2 survivors
  carry 0). **But it changed no classification.** Final `dr_type` of all 3 declined
  (`behavioral_competence_reading`, `disparity_as_depth_signal`, `generational_economic_decline`) is
  **`tangled_rope` at BOTH `39630182` (parent-of-`3116ac08`, pre-NL-gate) and HEAD** — identical
  (cascade sig `false_summit_mountain`, claim source `explicit_mountain_claim`, both commits). They
  claim naturality via `explicit_mountain_claim`, and `false_summit_mountain` sits higher in the
  priority cascade than the `natural_law` clause (and reads `constraint_beneficiary` directly), so it
  captured them before and after; the raw 5→2 match was **shadowed** and never reached final
  classification. **Correct the conflation: declined-a-raw-certification ≠ classification-changing.**
  T.1 "cosmetic" is **fully cosmetic at the final-type level** (majority AND the 3-case tail); it is
  non-cosmetic **only** at the raw `natural_law_signature` certification layer (a diagnostic output).
  **Tier-flag:** the prior B1 "*perturb-confirmed*" tag was raw-count evidence (NL 5→2) standing in
  for a final-type claim — a witness one layer below the claim it backed; the final-type claim is now
  perturb-confirmed via the two-commit `drl_core:dr_type/3` (default_context) query over the 3, held.

- **2026-05-31: `demotion_pass.py` is engine-blind — its buckets cannot witness any engine change** —
  VERIFY-OR-CORRECT pass. `python/sweeps/demotion_pass.py`'s six-bucket sort (`6/0/20/0/24/141`) is a
  pure function of (a) a regex `param(...)` count over `config.pl` + `constraint_indexing.pl` (=191)
  and (b) the hand-maintained `_WITNESSED` / `_GENUINELY_UNPERTURBABLE` / `_SHADOWED` dicts inside the
  script. It runs **no `swipl`, no `subprocess`, and calls no classifier** (imports:
  `argparse/json/re/sys/pathlib` + `sweeps.perturb._compute_corpus_hash`). So a "block matches
  `6/0/20/0/24/141`" result is HELD **by construction** and **cannot witness** row-23 / NL-gate or any
  other engine change. The handoff's verify-item-1 ("re-run the demotion sort before trusting the
  block") is **mis-routed through this script.** The block's real validity rests on whether those
  dicts still match live `perturb.py` survival on the **post-fix** engine — **UNVERIFIED / OPEN**
  (graduation step: re-run `perturb.py` on the post-fix engine and diff against the dict contents).
  Route item-1-type verification through `perturb.py`, not `demotion_pass.py`.
