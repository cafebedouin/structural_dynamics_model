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

## 2026-07-04 — OQ-140 RESOLVED: `author_engine_divergence` characterized (confound re-ranks kinds; one kind — Ω_E stratum reproduces on both twins, Ω_C reading 3/3 twin-confirmed)
**Files:** ISSUES.md, prolog/routing_sink.pl, python/audits/oq140_divergence_extract.py, audits/2026-07-04_oq140_divergence_characterization/
**Tier:** landed

Branch-3 (hybrid G-A) characterization audit of the routing sink's majority address
(`author_engine_divergence`, 277/512 on 96/128 at HEAD `7762b2c0`). No engine edits.

- **Method finding (reusable):** partialling the mechanical confound BEFORE decomposing
  re-ranks the population. ε is uniform across seats 96/96 ⟹ 100% of per-seat orbit variation
  is `d`-driven; confound-free (G-A uniform-orbit) = 56/277 (20.2%), confound-exposed = 221/277
  (79.8%, *granularity-expected, not kind-shaped*). Pre-confound lead `tangled_rope→snare` (111)
  dissolves; surviving confound-free lead is `rope→scaffold`.
- **One validated kind (operator-ruled):** **`naturalization-over-claim (rope→scaffold
  correction)` [Ω_E]** — author over-claims `rope` for a constructed frame/reading/standard the
  engine corrects to `scaffold` (`emerges_naturally=False` 9/9); one-directional 21/28
  rope-claims→scaffold. Pre-registered name "permanence disagreement" KILLED (predicted sunset;
  `has_sunset_clause` NO 8/9).
- **Two-tier promotion (do NOT collapse):** Ω_E stratum reproduces on BOTH twins (rope→scaffold
  G-A: haiku 49 / flash 64); Ω_C reading is live-corpus + 3/3 twin-confirmed. Contested-kernel
  members (6/9) carry an Ω_P sub-note. **Ω_E typing clean 5/6 on the structured witness
  (`emerges_naturally` seat-blind 6/6); one member — `fictional_construct_reading:204` — holds
  an UNRESOLVED Ω_P prose-signal** (an authored seat-declaration of its rope-claim, the
  pre-registered falsifier firing through a channel the structured grep didn't cover). Does NOT
  retract the kind's Ω_E typing (correction flag seat-blind on all 9); open item parked in
  OQ-211(d), not a curiosity.
- **Scope pin (freshness):** kind name + counts valid only relative to `route_address/5` at HEAD
  `7762b2c0`; any OQ-138 sibling-clause edit invalidates the taxonomy (OQ-211 carries this).
- **Controls:** emit-independence byte-agreement 277/277 (`constraint_claim/2`+`dr_type/3` vs
  sink); D-ladder 49 baseline raw≠final seats; mountain 0-count w/ same-path positive control.
- Residuals → **OQ-211**. Commits `e90bf3db` (Phase 0/1), `9d7baf07` (Phase 2), this (Phase 4).

*Promotion test:* no CLAUDE.md tripwire — this is a resolved research finding, not a silent
pre-edit footgun. The one durable caution (scope-pin: sibling-clause edits invalidate the
taxonomy) rides OQ-211's `bundled_with OQ-138` edge and the audit WRITEUP, where a reader
editing `routing_sink.pl`/`signature_detection.pl` will meet it; not always-loaded material.

---

## 2026-07-04 — Drone-report audit (Claude-web critique): d-header fixed, signature wording softened, OQ-209/210 minted, regulatory_lag H¹ fracture witnessed ROBUST
**Files:** python/enhanced_report.py, prolog/signature_detection.pl, ISSUES.md
**Tier:** landed

External critique of the four 2026-07-03 22:16 drone reports (procurement_inertia,
technology_diffusion_asymmetry, weaponization_accessibility, regulatory_lag_extraction). Triage +
actions:

- **FIXED — d-comparability header was factually false** (`enhanced_report.py:356`). Old text: "d is
  a function of the observer POSITION (a config lookup)… identical d across constraints for the same
  position is by design." But `derive_directionality/3` (`constraint_indexing.pl:408`) precedence is
  override → `beneficiary_victim_directionality` (power role + has-benef/victim + `exit_modulation`,
  all authored per story) → `canonical_d_for_power` fallback. Only the fallback is a config lookup;
  the common path is authored, so the SAME position label carries different d (institutional d ∈
  {0.72, 0.45, 0.15, 0.12} across the four reports). Header rewritten to state the precedence and
  that cross-constraint "same seat" d-comparison is NOT apples-to-apples. Reports on disk keep the
  old header until regenerated.
- **FIXED — `coupling_invariant_rope` explanation overclaimed** (`signature_detection.pl:769,772`).
  "Passes all structural purity tests" was false: the signature gates on Boltzmann compliance + scope
  invariance only; `ExcessEps` is reported, not tested (procurement certified at excess 0.580, which
  its own drift section flags as `excess_above_floor(0.58)` + 2 critical drift events). Softened to
  "coupling-clean (snapshot) … NOT an excess-extraction or drift gate." Behavior-preserving (display
  atom only; grep-verified no parser). = OQ-210 (resolved).
- **OQ-209 minted (open)** — single-constraint scenario reports render corpus-scope metrics as
  success-shaped defaults: W1=0.0000 printed beside H¹=4 (`wasserstein_corpus_fracture` silently
  skips constraints lacking MaxEnt distributions → skip-zero rendered as measured-zero, and "Corpus"
  is a misnomer in a 1-constraint run); "Network stability: stable" is a 1-node network beside a
  corpus header of "cascading". Pattern-6; bundled_with OQ-97. Graduation = witness skip-vs-genuine.
- **FALSIFIER RUN (the "one thing the reports don't show") — regulatory_lag H¹=4 is ROBUST, not a
  config/transfer-function artifact.** Baseline orbit `[tangled_rope,snare,tangled_rope,snare]`,
  H¹=4, reproduced. Swept metric-ε and the f(d) seat-curve (`config:param(cognitive_displacement)`),
  clearing caches each step. H¹>0 survives ε ∈ [0.50,0.90] and d_offset ∈ [-0.15,+0.20]; collapses to
  agreement only at extreme shifts (d_offset −0.20 → all rope). The invariant throughout is the 2+2
  structure powerless≡institutional ≠ moderate≡analytical (Hub-2 immutability axis) — that IS the
  perspectival finding, and it is stable under perturbation of exactly the authored ε/d values the
  critique questioned. NB: `domain_priors:base_extractiveness` is STATIC (χ-side ε unperturbable by
  retract; the sweep moved the dynamic `constraint_metric(extractiveness)` metric-ε). The d-curve
  perturbation directly moved f(d) and the fracture held. Probe scripts in scratchpad (not committed).
  Caveat unchanged: ε/d are authored (OQ-102a) and Fisher/persistence remain STALE (OQ-29) — this
  witnesses robustness for THIS constraint, not a corpus-wide re-validation.

---

## 2026-07-04 — OQ-193 report-surface build: giant_comp provenance split (pooled + cross-kernel stratum)
**Files:** prolog/giant_component_analysis.pl, python/run_pipeline.py, python/enhanced_report.py
**Tier:** landed

The owed OQ-193 report-surface build (RULED (c) 2026-07-02) landed at **zero engine-behavior change**:
`pipeline_output.json` `per_constraint` is **byte-identical** (sha256 match) before/after a full pipeline run;
`constraint_neighbors_existing/2` and the `drl_purity_network.pl` sibling warnings are untouched. Two surfaces —
(1) `giant_component_analysis.pl` gains a `## Provenance split (OQ-193)` md section + a same-run
`giant_component_analysis.raw.json` co-product (pooled vs sibling-stripped stratum + per-constraint
membership/degree); (2) `enhanced_report.py` gains a per-constraint "NETWORK POSITION (OQ-193)" L1 section +
additive `network_position` sidecar with a four-branch interpretation.

**Method = retract-recompute, dead-last.** `deduplicate_neighbors` keeps the strongest edge per pair, so a
post-hoc `gc_edge` filter would miss an inferred edge that resurfaces on recompute; the faithful strip retracts
the same-kernel-explicit `affects_constraint` **substrate** and recomputes. Placed dead-last in
`run_giant_component_analysis` (after `report_embedded_facts`), in a subprocess that then exits, so the strip is
**never restored** (the probe's re-assert step is intentionally dropped) and nothing downstream reads stripped
topology. Does its own fresh pooled `measure_topology` first because phases 2/4 mutate gc state.

**Commit-Gate-1 outcome (the witnessed cause, not the assumed one):** `same_kernel_edges_surviving = 0` on
`testsets/` — dedup-resurfaced 0, never-stripped 0 (partition identity M1+M2==M asserted, held). So the
**`cross_kernel` label is HONEST** and the dedup subtlety was defensive-only (did not bite); **no operator
escalation triggered.** The M>0 branch (rename to `explicit_sibling_stripped` + escalate) exists in code but is
unexercised on this corpus.

**Witnessed values (testsets, 2026-07-04):** 68 sibling edges stripped; pooled giant 12 / 72 components →
stratum giant 9 / 95 components; positive control ok (raw `affects_constraint` 241→173, dropped 68 = strip
count). Matches the frozen probe at both endpoints. Node set = `all_corpus_constraints/1` = **119**
(extractiveness-bearing), a **subset of the 128-constraint corpus** (manifest `n_constraints`) — the plan's
"per_constraint == manifest" premise was slightly off; 119 is giant_comp's own denominator (phase-1 "Total nodes
= 119"), and the 9 excluded are all `*_contradictions` stories lacking an extractiveness metric, correctly
surfaced by enhanced_report as "not in node set."

**Run-scoped binding.** `_prolog_giant_comp` pre-deletes `giant_component_analysis.raw.json` as its FIRST
statement (`unlink(missing_ok=True)`) and asserts the `## Provenance split (OQ-193)` marker is in stdout before
writing the md (a standing guard against a future Prolog-side catch-wrap/soft-fail silently dropping the owed
section). `_manifest_step` stamps `giant_component_analysis.manifest.json` (mirroring the orbit sidecar) **only
when the `giant_comp` StepResult is `status=="ok"` AND raw.json exists** — executed-stage membership, so a
skip/fail path can never pair a stale raw.json with a fresh stamp. enhanced_report joins via `manifest_key`
same-run guard and degrades to NOT ASSESSED on stale/missing/unparseable.

**tripwire — giant_comp intermittently times out (900s) in the parallel Phase-2.** First full-pipeline run this
session, `giant_comp` hit the 900s subprocess timeout **despite running in 0.64 s / 18 MB standalone** — the
documented intermittent co-residency stall (OQ-182 class), NOT a regression from this split. The degrade design
worked exactly as intended: step logged `status=error` (non-critical, pipeline continued to exit 0), the
pre-deleted raw.json stayed absent, the md was not overwritten, and `_manifest_step` **skipped** the sidecar
stamp (no fabricated current identity). A re-run completed `giant_comp` cleanly and produced all artifacts
same-run. If a run's giant_comp surface is stale/absent, check for this timeout before suspecting the code.

**Probe is a frozen dated snapshot.** `audits/2026-07-02_oq193_giant_comp_ruling/probe_giant_ripple.pl` is NOT
edited; production adapts its strip/measure logic and is expected to diverge (the drift is declared, not silent).

---

## 2026-07-04 — OQ-75(b) grain precursor probe: throw LARGE, cell-count non-monotone under coarsening (statistic-spec inputs)
**Files:** python/audits/oq75b_grain_probe.py, prolog/axiom_concept_registry.pl
**Tier:** landed

Pre-registered unratified grain arms over the tranche-1 registry (10 pilot kernels, 42
pairs, both legs; `audits/2026-07-04_oq75b_grain_probe/`). One arbitrary refinement step:
cells 47→21, tordesillas conversion dead, contradiction-pair co-slotting 3/3→0/3.
Coarsen-max: alignment mass grows (theorem, disclosed) but the raw cell count FELL 47→42 by
vantage consolidation — a cell/vantage-count invariance statistic reads coarsening with the
WRONG SIGN. Verdicts grain-labile both directions (key_fragile 26→38→12). Constraints fed
to the future §7.1 correlation-statistic spec (recorded in OQ-75's ruled sub-item): grain
normalization load-bearing; no raw-count statistic; contradiction-pair reads
refinement-brittle; grain-stamp ax_stability_verdict aggregations. Controls fired:
overlay-took-effect (fact counts + A1 atom set), known-changer (A1 merged digital_money's
slots), A0 externally consistent with the OQ-72 sweep (47==47). Canonical registry never
edited (arms are in-process overlays). Stage 1 proper NOT discharged — statistic unbuilt.

## 2026-07-04 — OQ-72 consumer wiring: axiom concept alignment section in tensions_ledger (three-valued coverage); baker emits tranche-kernel facts
**Files:** python/tensions_ledger.py, python/axiom_concept_bake.py, prolog/axiom_concept_registry.pl
**Tier:** landed

Operator-directed post-close wiring (the ledger, NOT enhanced_report — per-constraint is the
wrong altitude for a cross-reading product). `tensions_ledger.py` now appends a kernel-level
"Axiom concept alignment" section: swipl subprocess each run (fresh compute, no stale sidecar),
both keys per within-kernel pair, agree/disparity cells rendered (a disparity cell = same
subject, opposed groundings = a tension by construction). Coverage is THREE-VALUED per kernel
and never collapsed: RATIFIED (cells) / NOT-YET-RATIFIED (tranche never ruled; blind BY DESIGN,
never "no shared subjects" — GAP-24) / single-reading (no pair exists; named, not dropped);
kernel-less constraints counted. Fails LOUD on swipl error (a missing section must not read as
measured-no-tensions), and carries an in-run TWO-SIDED join control closing the
CLEAN-EMPTY hole (reviewer catch 2026-07-04: a present-but-empty registry loads fine and
renders `concept 0/0/N` identically to a genuine no-shared-occupants pair): known
same-concept pair must align AND known distinct pair must not, else halt(3); fired-status
rendered into the section header. Both arms witnessed (positive: section line; negative:
in-process retractall -> exit 3). The control's own falsifier caught a real bug in its
first version (format/2-vs-format/3 on the failure branch). Baker now also emits `axiom_diff:axiom_concept_tranche_kernel/1` (one per
kernel in the ratified TSV, incl. hypothetical all-no_slot kernels) — the coverage provenance
bit travels in the registry; regen witnessed concept-facts byte-identical + C6 refusal re-run.
Witnesses: mixed-scope run (2 ratified kernels w/ cells + seat_gauge NOT-YET-RATIFIED + 1
kernel-less) and full-128 run (46 kernels: 3 ratified / 10 unratified multi-reading / 33
singletons named / 58 kernel-less), both pasted in-session 2026-07-04. New tension surfaced
immediately: moral_causation_locus accountability_intervention_locus disparity
[deontological]|[instrumental].

## 2026-07-04 — OQ-72 resolved: ratified concept key for the axiom axis (pilot); axiom_concept_registry born; westphalia tests re-frozen
**Files:** prolog/axiom_concept_registry.pl, python/axiom_concept_bake.py, prolog/stack.pl, prolog/tests/test_axiom_diff.pl, prolog/axiom_diff.pl, ISSUES.md, docs/the_perturbation_principle.md, docs/design/design_gaps.md
**Tier:** landed

OQ-72 closed at the scoped altitude "mechanism demonstrated" (mixed haiku/live 10-kernel
pilot; audit `audits/2026-07-03_oq72_concept_key_pilot/`, WRITEUP.md there has the control
table). The formerly-empty `axiom_diff:axiom_concept/2` seat is now populated by the NEW
CANONICAL `prolog/axiom_concept_registry.pl` (71 ratified facts, tranche 1), loaded from
`stack.pl`; regenerate ONLY via `python/axiom_concept_bake.py` (fail-closed on unratified
rows) from a ratified TSV — hand-edits lose ratification provenance. All six pre-registered
controls passed (C1 3/3, C2 3/3, C3 10/10, C4 fired w/ planted control, C5 green, C6
refusal); false-merge 0/71; both kill legs clear. Three standing cautions for future
sessions: (1) **the registry is name-keyed** — a mapping applies wherever the axiom name
occurs on ANY leg (witnessed: one pilot name recurs in the flash twin; disclosed in the
registry header) — never assume per-leg scoping; (2) **`cs_axiom_contradiction` is not
universally same-subject** (2 of visual_evidentiary's 3 pairs oppose across subjects and
cannot align under any assignment — don't read their non-alignment as proposer failure, and
don't build scale-up gates on contradiction⟹same-subject); (3) **epistemic reframe**: the
key makes the axiom axis RATIFIED-legible, not discovered (§7.1 amendment 2026-07-04);
OQ-75(b) carries a labeled asymmetry + pending blocked_on_human parity ruling. Also fixed
en route (pre-existing): `tests/test_axiom_diff.pl` westphalia tests had been silently
unrunnable-green since the 2026-06-20 regime swap (froze old corpus axiom names; now
fixture-local + corpus-independent), and their cleanup's blanket
`retractall(axiom_concept(_,_))` would have wiped the baked registry mid-session (now
scoped; post-run count 71 witnessed). SCOPE-time concept-slot emission = GAP-24. Scale-up
= separate spend-go (recipe in the OQ-72 resolution).

## 2026-07-03 — OQ-03 RESOLVED: operator declared DR's own seat (extraction-seeking skepticism); 03b mooted; self-application run snapshotted
**Files:** ISSUES.md, audits/2026-07-03_oq03_self_application/
**Tier:** landed

**The close.** Operator ruling in session: 03b (the empirical limb) is MOOTED — where DR sits is
not a fact a redraw could measure ("it doesn't matter how many times we reran this"); it is the
declared seat itself. Declaration (operative text in ISSUES.md OQ-03): DR is a variety of
philosophical skepticism whose seat is to look for extraction everywhere — a technique for
surfacing seats, particularly hidden ones, and the cover stories that conceal them; a lens with
different apertures and positions (`docs/seat-theorem-v1.md`, `docs/commitment_systems/*`,
`docs/debugging_philosophy.md`), not the truth; the focus shapes what it can see, which is what
makes it a seat. Known limit: the outside seat cannot read some internal dynamics
(`essays/2026-06/the_same_paper.md`).

**The datum.** Same day, the operator ran `docs/deferential_realism_paper_v8.md` through
`c-orchestrator` (5 stories, commit `72ab7663`, manifest n=128): seat-indexed plurality, no
single type — kernel siblings diverged (snare-family w/ extraction_blindness mismatch vs
all-scaffold w/ false_ci_rope commentary), flat control unknown/scaffold. Inputs LLM-drawn,
single draw, not pre-registered → illustrative seated datum only, never "DR is X." Ledger + 5
reports snapshotted (outputs/ is gitignored): `audits/2026-07-03_oq03_self_application/`.

---

## 2026-07-03 — OQ-205 RESOLVED: ε declaration discipline BUILT (11 units, Controls P/S green through the recurring gate)
**Files:** prolog/constraint_indexing.pl, prolog/boltzmann_compliance.pl, prolog/narrative_ontology.pl, prolog/data_validation.pl, prolog/json_report.pl, prolog/reading_registry.pl, prolog/tests/test_epsilon_declaration.pl, prolog/tests/fixtures/eps_controls/, python/generate_constraint_pl.py, python/run_pipeline.py, python/enrich_pipeline_json.py, python/enhanced_report.py, python/sweeps/epsilon_stability.py, python/epsilon_authorship_readout.py, docs/design/epsilon_declaration_discipline.md, docs/deferential_realism_paper_v8.md, ISSUES.md, audits/2026-07-03_oq205_build/
**Tier:** landed

Build phase U1–U11 landed same-day as the spec (commits `e9041905`…close; unit→commit map +
all transcripts: `audits/2026-07-03_oq205_build/README.md`). All five §9 graduation criteria
met; OQ-205 → resolved. Highlights a future session needs:
- **Both §3 fabrication fallbacks are DEAD** (U1 `get_true_metric` 0.0 → `unknown`; U2
  boltzmann `BaseEps=0.5`/`Supp=0` → fail-closed mirroring `is_X/3`). Witness: live +
  haiku + flash + kernel_v1 all byte-identical post-normalization; the U2 first cut
  (`Type=unknown` token) was REJECTED for emitting computed-looking `scope_violations: 0`
  over an unknown grid (Pattern 6) — fail is the honest arm.
- **No-backfill ruling recorded** (spec §3/§9): generator-forward; the whole pre-build
  corpus is the declared loud-null stratum (`"none_authored"` emission token, census
  110+1 live); corpus-complete at rebuild.
- **New recurring gates:** `_prolog_epsilon_declaration_gate` (suite + Control P fixture
  corpus through the real load path, `tests/fixtures/eps_controls/`) and the ε-stability
  sweep in the post-parallel slot (Control S selftest first, fail-closed; R3 tripwires
  fatal-live/advisory-overlay). Deliberate-break controls witnessed for both.
- **Sweep tripwire for probe authors:** `drl_core:base_extractiveness/2` is multifile
  STATIC; `carbon_tax_2026`'s direct fact is clause 1 and an UNPINNED read backtracks past
  it to any matching solution — took-effect guards must `once/1` the read (witnessed: the
  unpinned guard "passed" under the shadow).
- **New corpus finding:** `unstable_off_grid` (final type flips under ε±0.02 with ε
  band-interior — χ-gate crossings) is the largest flag class on every leg (43/110 live,
  452/1106 kernel_v1): ε-sensitivity is mostly NOT ε-threshold proximity. Routed to
  OQ-78/OQ-48 consumers.
- **OQ-78 standing readout** (`python/epsilon_authorship_readout.py`, pipeline Phase 9c)
  reproduces the census exactly (0.68×46/110=41.8%, .x8/.x2 rail; flash on .x5/.x0).

---

## 2026-07-03 — OQ-205 spec landed: ε declaration discipline (provenance + stability), read-only census with control PASS
**Files:** docs/design/epsilon_declaration_discipline.md, docs/design/design_discipline.md, ISSUES.md, audits/2026-07-03_oq205_epsilon_census/
**Tier:** landed

Spec-only session (no engine change, no threshold change; Controls P/S pre-registered, NOT
run — running them would un-pre-register them). `docs/design/epsilon_declaration_discipline.md`
authored per the approved plan: disambiguation vs DP-001/OQ-26 (never title anything "ε
invariance"), provenance carrier recommendation `epsilon_provenance/5` (R2), read-site table
anchored at `6c59615e`, stability protocol with census-informed r = 0.02 (R3, two kill
conditions), commentary-grade flag disposition (R4), graduation criteria. OQ-205 → `partial`;
design_discipline §7 cross-pointer added (bidirectional refs same-commit).

**Census findings** (`audits/2026-07-03_oq205_epsilon_census/`, 4 legs, planted in-memory
control at snare_epsilon_floor+0.0005 PASS): (1) **testsets_flash authors ε exactly ON
classification thresholds** — 218/960 (22.7%) at distance 0.000 (its .x5/.x0 grid lands on
0.45/0.30/0.25/0.10); these are unstable at every radius by authoring convention. (2) The
(0.45, 0.46) open interval is EMPTY on all four legs — the tight-radius binding constraint is
moot on current corpora (re-check at regeneration; kill condition on R3). (3) OQ-78
re-baseline: live 0.68-mode share 41.8% (46/110); the last-digit rail is model-specific
(flash 5/0, haiku+live+kernel_v1 8/2). (4) Recon corrections to the plan: the ε threshold set
includes `mountain_extractiveness_max` 0.25 (plan omitted it); a SECOND fabrication fallback
found at `boltzmann_compliance.pl:248–252` (`BaseEps = 0.5`, OQ-89 class) beside the known
`constraint_indexing.pl:902–903` `Val = 0.0`; every story file authors ε TWICE
(`domain_priors:base_extractiveness/2` + `constraint_metric/3`) — silent-fork surface, spec
§3 requires the build to equality-check or declare canonical.

**Same-day ratification (operator):** R2–R4 RATIFIED with two spec-text amendments folded in —
three-site equality check (§3: `ValueAsWritten` is a third ε site, covered by the check or
it's a fork) and two-class stability flag (§5: `on_threshold_grid` vs `near_threshold`; both
block anchors, split is for the readout); R4 gained its promotion trigger (concealed flip
that mattered downstream ⇒ verdict-grade). Audit-dir tracking witnessed (`git ls-files`, 8
files in `a2a87dc5`).

---

## 2026-07-03 — OQ-138 FNL sub-part BUILT: RECLASSIFY→ROUTE landed (d248a6b1 + 82aa372e), consumers keyed on the lever, census type-inert was default-context-scoped
**Files:** prolog/signature_detection.pl, prolog/config.pl, prolog/config_schema.pl, prolog/abductive_helpers.pl, prolog/maxent_classifier.pl, ISSUES.md, audits/2026-07-02_oq138_fnl_evidence/
**Tier:** landed
The OQ-138 FNL CONVERT ruling's owed build, in two commits with the twin-diff hard gate between them (operator approved with one condition, folded in). U1 (`d248a6b1`, output-changing): `:925` overwrite → route behind NEW `false_natural_law_override_enabled` (0=route default, 1=legacy; schema spec added — config_schema.pl gate fails loud on a spec-less param); `fnl_routed/1` outcome-keyed (dr_type/3 non-circularity TRACED at HEAD: 152-pred closure, 3 positive controls); victim-discriminated severity (vic>0→moderate). U2 (`82aa372e`, wiring): seat_overrides + maxent boost keyed on the LEVER, a deliberate departure from the plan's `\+ fnl_routed` shape.
- **Tripwire-grade finding: `fnl_routed/1` (and `fcr_routed/1`-style seat predicates generally) are DEFAULT-CONTEXT-keyed while `resolve_modal_signature_conflict` overwrites are ORBIT-wide.** Witnessed: `organization_floor_c0` ("type-inert" in the census) routes tangled_rope→scaffold at the INSTITUTIONAL position while default-context unknown — the census's type-inert column was default-context-scoped only. Consumers that would lie under default-keying (probe_signature via seat_overrides; the PER-CONTEXT maxent boost — apply_signature_override fires at all 4 Wasserstein contexts) were therefore keyed on the lever: at lever=0 NO seat overwrites (typed seats route, unknown seats abstain), so override-liveness IS the lever state, orbit-safe. FCR reconciliation: FCR's non-routed seats keep their boost because `fcr_override_enabled` defaults 1 (override still LIVE there) — one rule, "boost mirrors live overwrite," two outcomes. **Re-open condition (the SPECIFIC kill, not the general fact):** default-keying is FINE for the grade/severity consumers (`converted_at_seat` → SigGrade/severity are default-headlined by architecture, like verdict_join itself); what trips this is a FUTURE consumer that reads `fnl_routed`/`*_routed` for ORBIT-SENSITIVE override-liveness (anything evaluated per-context or aggregated over the orbit — a maxent-style per-context injector, an orbit-walking exporter). Such a consumer must key on the lever (or a per-context predicate), never on the default-keyed seat predicate.
- **Twin diff (THE behavior witness, `FNL_CONVERSION_DIFF.md`):** 8/14 routed seats render RED (census predicted green→yellow — prediction vs measurement, the FSM lesson again): type_1_false_summit informational→severe on routed snare + **h1 0→3 / sheaf→manifest** — the overwrite applied at every context and flattened the whole orbit into a manufactured global section (pasted orbit: competence_occupation OLD tangled_rope×4 → NEW {snare,snare,scaffold,snare}). Determinism control NEW-vs-NEW2 0/960; OLD arm byte-identical to the pre-conversion canonical baseline; twin spillover (8+31 seats) all maxent/ensemble refit, zero signature/type/grade changes; live leg 0 verdict changes (89 records move in wasserstein/arakelov/signature_pressure only — one seat's orbit change re-centers corpus-relative ensembles).
- **Gates:** 5-corpus sweep routed 0/6/8/0/0 with routed∩piton=0 retained as positive control; per-context consumer probe (org_floor_c0@institutional = scaffold + no_boost + agrees, BOOST-CONTROL fires on every leg); two-sided ablation (lever=1 restores legacy at every context, incl. the legacy computed-but-unrendered override_mismatch at org_floor_c0 — proving route-mode's `agrees` is an improvement, not a hidden artifact); gate.sh GREEN.
- **Correction-key (pre-existing failures, attributed NOT-mine by identical failure at HEAD-files+lever=1):** validation_suite has 119 PASS + 1 FAIL (`lycurgan_laws__demographic_trap_reading` BCE interval 480>330). Two-axis dating of that fail: the interval fact is byte-identical since pilot_05 (`f4c7b13d`, 2026-06-13) and the file WAS in the 2026-06-21 suite that read 92/0/0 — so the interval-validity check entered the regenerated suite AFTER 06-21; the plan-era "92/0/0" denominator is the 06-21 corpus size (suite is auto-generated 1 unit/file; corpus grew 92→119 via topic runs). Open corpus-content question flagged, not fixed (needs a BCE-encoding ruling, other BCE stories may share it): should BCE intervals be authored as negative years? `test_agent_beneficiary` fails 35/94 (per-testset threshold/profile validation units on the current corpus — the "green" expectation in the plan was stale); `test_contradiction_signatures` same 5-name set as its known baseline. Cite these as baselines, not regressions.

---

## 2026-07-03 — OQ-87 twins characterization DONE (zero-spend): committer axis byte-stable, magnitude convention model-idiosyncratic, existence proof re-scoped to de-baited rate; OQ-208 minted (CA-2 split)
**Files:** ISSUES.md, audits/2026-07-03_oq87_twins_ca3/, prompts/constraint_story_generation_prompt_DRIFTNEUTRAL.md
**Tier:** landed
Pre-registered read-only run (PLAN.md committed before any arm; four serialized swipl runs: kernel_v1 906-pool / testsets 89 / haiku 960 / flash 960; seven controls all discharged incl. two kill conditions). Full record: `audits/2026-07-03_oq87_twins_ca3/FINDINGS.md`; commits `8ac24afc`→`e99ccaf5` + this landing.
- **Committer axis byte-stable across 26 days of observer-engine evolution:** banked (2026-06-07) vs HEAD `dfe10734` on the 906 pool = **0 committer-verdict flips vs 42 observer-bucket changes** (same differ read both columns — internal positive control). Anchor diverge-A 74→82 wholly observer-side (11 gained / 3 lost, all stayed `dead`; OQ-51 null-exclusion = 0). Theorem-7-consistent characterization, NOT a proof (bait-bearing substrate).
- **fired = grep-candidate exactly on all four corpora** (16/16, 129/129, 136/136, 18/18): the `cs_axiom_foreclosed` conjunction is file-locally decidable at HEAD — grep-candidate counts may be cited as fired counts *at this code state*.
- **Magnitude convention is model-idiosyncratic:** substantial-rate haiku 0.870 vs flash 0.505 (|Δ|=0.365). Foreclosure-shaped authoring (`axiom_overriding`+non-minor+unack) clusters ≈0.21 on the three Anthropic-era corpora (0.213/0.206/0.211) and collapses on the Gemini twin (0.027). Flash fired-core 18 < pre-registered floor 20 → flash-side and shared-core rates are DESCRIPTIVE-ONLY (flash-rate 1,067-story and shared-rate 2,133-pair sizing figures may NOT license a spend; only the haiku-rate 141 is citable). Conditioned direction agreement 0.734 vs chance 0.687 — near-chance cross-model content under bait.
- **Rulings (operator):** (1) FOLD-IN — OQ-87's proof limb `blocked_on OQ-75`, with the edge tracking the MEASUREMENT (a de-baited fired-core rate), not the rebuild artifact; **DRIFTNEUTRAL pin currently ABSENT repo-wide** (controlled grep; only banked audit scripts reference the prompt) — pin note added to OQ-75; ~150-story pilot recorded as the sooner-option, trigger = a named downstream forcing function. (2) CA-2 SPLIT → **OQ-208** (Priority 3, `splits_from OQ-87`), Deps authored with BOTH exit branches: construct a framing-sensitive positive control OR prove none can exist (negative-by-construction close admitted; code-level branch (b) first, near-zero spend).
- **Tripwire (probe adaptation, carried in the audit dir):** the banked ca3 probes bucket `H0==1 else incoherent` — post-OQ-51 that silently misbuckets `H0=null` as incoherent (77/91 stories per twin are undetermined). Any reuse of pre-2026-06-25 H0-consuming probes needs the 3-way bucket.

---

## 2026-07-02 — Four blocked_on_human rulings landed: OQ-138 (CI-rope KEEP+close, FNL CONVERT), OQ-193 (giant_comp additive-split), OQ-75 (Stage-2 scoped-go)
**Files:** ISSUES.md, audits/2026-06-21_oq138_fsm_route_conversion/CIROPE_RED_ADJUDICATION.md, audits/2026-07-02_oq138_fnl_evidence/, audits/2026-07-02_oq193_giant_comp_ruling/, audits/2026-07-02_oq75_stage2_preflight/
**Tier:** landed
Witness-gathering + rulings for four blocked items (probes read-only w.r.t. engine substrate — reversible corpus overlays via `retractall+assertz`, verified restore, per-probe positive controls). No engine behavior changed; two CONVERT/build obligations recorded in ISSUES.md, not started.
- **OQ-138 CI-rope route-purity — RULED KEEP-as-written, limb CLOSED.** 5 rope-consumers re-witnessed at HEAD. Inherited neutron_star RED sub-item RESOLVED MOOT: at HEAD neither neutron_star nor superheavy is RED (OQ-128 discriminated severity + FCR-9 conversion each independently removed the cap). superheavy is a DOCUMENTED FCR-inert seat (CONSTRUCTED3_FINDINGS.md:21; 0-hit in FCR9_live_diff), verdict-absent because unknown-surfaced — absence discriminated by neutron_star's present verdict in the same dump. Kill condition stays live. Witness: `CIROPE_RED_ADJUDICATION.md`.
- **OQ-138 false_natural_law — WITNESSED + RULED CONVERT (build OWED, not started).** 4-leg census (testsets 1 inert / haiku 13, 6 changed / flash 8, 8 changed / kernel_v1 0). The 14 type-changers repeat the FSM/FCR shape (scaffold/snare→tangled_rope, green→yellow unmask, correction grade, claim+vic discriminant). ALL 22 firings source-1 explicit_mountain_claim, ZERO source-2 (OQ-70 fix holds). kernel_v1=0 is measured-empty (41 claims × 973 non-compliant, intersection 0). Both census + diff positive controls passed. Build owed: conversion + 5-corpus sweep + abductive_helpers/maxent consumer fixes. Near-free on the live leg. Witness: `audits/2026-07-02_oq138_fnl_evidence/FNL_EVIDENCE.md`.
- **OQ-193 giant_comp — RULED (c) additive provenance split (topology ruling, report-build OWED).** 3-leg ripple confirmed at HEAD (giant 12→9 / 549→47 / 334→70). Per-consumer price: FPN NO-DIFF (OQ-23 guard already zeroes sibling contamination — two-sided controlled: planted cross-kernel strip DOES move purity on testsets; haiku vacuous-but-consistent); json_report/network_dynamics/severity DO change (15/282 hub flips). Headline has zero downstream consumers. (c) = siblings stay in topology for all 5 consumers + giant_comp reports both pooled & cross-kernel counts. NOT zero-cost — it rules siblings intended topology. Witness: `audits/2026-07-02_oq193_giant_comp_ruling/RULING_EVIDENCE.md`.
- **OQ-75 Stage-2 — RULED SCOPED GO (a).** Part (a) diff-distribution authorized; part (b) cross-axis correlation (the OQ's headline staked falsifier) stays UNTESTED (standalone build; OQ-15 resolved 2026-06-24 so NOT gated on a mediator layer). Construction-pair stratum N/A this cohort (twins carry 0 flat_control facts vs testsets' 10; recorded in OQ-76). Preconditions before citable numbers: build the prevalence counter (harness-reuse extension of `oq49_override_remeasure.py` — confirm it counts prevalence not override-firing) + clean-tree twin reclassify (both twin manifests code_dirty). Witness: `audits/2026-07-02_oq75_stage2_preflight/PREFLIGHT.md`.
- **Correction-key:** two exploration-record errors corrected in the OQ-75 preflight — OQ-15 is RESOLVED (2026-06-24, `279d7c24`) not open; and a `false_*`/`dr_claim_mismatch` prevalence counter does NOT exist from scratch but CAN be built as an extension of `oq49_override_remeasure.py` (which counts override firing, not prevalence — confirm before citing the cost as cheap).

## 2026-07-02 — OQ-126 RESOLVED: drift terminal carries its authored-ack provenance (witness-not-verdict); external-anchoring tier ladder promoted to design_discipline.md §10
**Files:** prolog/json_report.pl, prolog/cs_drift_engine.pl, prolog/tests/test_cs_drift_engine.pl, python/shared/schemas.py, python/enhanced_report.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** landed

Gap 1 (the cyclopean shape: the engine consumed the AUTHORED `Acknowledged` bit in
`cs_drift_state` gap/3 as if it settled the seated honor-vs-reabsorb verdict) fixed as
provenance labeling, commit `ee51cdff` — Pass-0 gate re-witnessed the scoping claim first
(`cs_terminal_attractor/4` internal to cs_drift_engine.pl; all 5 `cs_drift_trajectory/3`
production consumers commentary-grade; no path to `classify_from_metrics/6`; HALT clause
unused). New fields at every terminal surface incl. the no-CS-UID default branch (that branch
was MISSED on the first edited run — 30/119 entries lacked the fields; test w3 now pins it):
`cs_drift_terminal_basis: "authored_ack"|null` and `cs_drift_ack_witness {authored,
acknowledged, confrontation_path, confronted_by}` where `confrontation_path: "none_exists"`
is a NO-PATH sentinel (no external instrument exists; OQ-107 `future`), NOT "checked, none
found" — operator null-semantics rider. `enhanced_report.py` renders the terminal conditional
("if authored acknowledgment taken at face value") — the decoration kill-condition control;
before/after panel diff witnessed on `ability_ceiling_reading`. RED control witnessed (both
w-tests FAILED with emission dropped, then restored green 24/24; test_cs_trifurcation 19/19 —
OQ-55 single-bit twin untouched). Scale: clean-vs-edited diff n=119 additive-only, 0
pre-existing value changes, warning sets byte-identical (1,428 pre-existing `fingerprint_shift`/
`repair_transitions` warnings — pre-existing condition, not this change's); twins
`testsets_haiku` n=960 + `testsets_flash` n=960 at `5d6f219`-dirty, 0 missing/unfaithful.
Item (c): tier ladder (Tier 1 external/dated / Tier 2 retained-record + NORMATIVE declared
record-boundary MUST / Tier 3 no temporal handle / declared stop) promoted from the OQ entry
into `design_discipline.md` §10. OQ-126 compressed-on-close; stale cross-refs corrected
(OQ-74 was cited as "pending ruling" — resolved 2026-06-14). Ω_P core (honor/reabsorb seated,
never engine-certifiable) closed as DECLARED, not solved.

## 2026-07-02 — OQ-195 RESOLVED: general-n H¹ gap spectrum proven at every cardinality; stakeholder frame makes it the live law; OQ-207 minted
**Files:** docs/h1_gap_spectrum_general_n.md, python/audits/oq195_h1_spectrum_check.py, prolog/tests/test_h1_spectrum.pl, prolog/grothendieck_cohomology.pl, docs/deferential_realism_paper_v8.md, docs/deferential_realism_paper_v7.md, docs/deferential_realism_paper_v6.13.1.md, ISSUES.md, audits/2026-07-02_oq195_general_n_gap/
**Tier:** landed

New proof doc `docs/h1_gap_spectrum_general_n.md` (commit `5d052990` + close commit): min
nonzero H¹ = n−1 at every cardinality ({1..n−2} forbidden — the four-seat forbidden-{1,2} is
the n=4 instance); exact band decomposition by largest agreement bloc with a self-similar
recursion; unconditional band-floor lemma; inter-band gap iff n ≥ j+3+C(j+1,2) (every value
in the gap forbidden); type-token bound T=7 (derived from code) truncating the top for n>7 —
LIVE, not hypothetical: the operator's pre-check surfaced the stakeholder frame (named seats,
roles {agenda_setter, beneficiary, payer, excluded, observer}, per-seat computed types) at
3–12 seats/story across the live legs (kernel_v1: zero — the frame post-dates it). Verified
under pre-registered BLOCKING criteria n≤40 with PER-BAND bookkeeping — the plan review
caught that the band union is invariant under dropping the parts-constraint, so a union
check cannot verify the classification; the unconstrained classifier ran as a discriminating
control (unions identical ∀n, bands mismatch 38/39). Engine witness `test_h1_spectrum.pl`
23/23 (exhaustive n=2–4, constructive n=5–12, OQ-51 filter at n=12, two negative controls).
Adversarial multi-agent review was blocked by a session subagent limit — substituted by the
operator's hand-derivation + enumeration + an author re-derivation pass that caught one
prose defect (B_{j+1} is the band FLOOR, exact minimum only for j+1 ≤ n−j−1) — recorded in
the WRITEUP. Propagated: v8 §3.4/§9.6/Appendix; v7 dated amendment note (band values are
seat-count-conditioned); v6.13.1 changelog item-6 landed-pointer; `grothendieck_cohomology.pl`
both range comments (comment-only, behavior-preserving). **Line-drift correction-key:** the
stale-range flag cited repo-wide as `grothendieck_cohomology.pl:158` actually lived at
ll.167–182 — cite it by predicate header, never by that line. **OQ-207 minted** (stakeholder-
frame H¹ build: `dr_type_for_stakeholder/3` vectors → `obstruction_from_vector/3`;
commentary-grade; registry-registered; `consensus_provenance/2` is its H¹=0 special case).

---

## 2026-07-02 — OQ-70 premise-rot correction: canon said "until ruled" for 27 days after the ruling; v8/README inherited it on authoring day
**Files:** CLAUDE.md, README.md, docs/deferential_realism_paper_v8.md, ISSUES.md
**Tier:** correction-key

Found while ranking the frontier: OQ-70 (FNL bait confound) was RESOLVED 2026-06-05 (option A
class ruling, bait clauses removed at `72ec2cdd`, detector-intact positive control), but
CLAUDE.md Critical Distinctions kept the pre-ruling "Until OQ-70 is ruled" framing in present
tense — and the v8 paper (§9.4/§9.6/Appendix) and README, authored 2026-07-01/02, inherited it
from CLAUDE.md the day they were written. **The corrected canon (all surfaces now derive from
it):** OQ-70 resolved; what survives permanently is (a) pre-reset/archive prevalence is
regime-bound (authoring convention, never detection), (b) live prevalence is citable only as a
CLAIMS statistic, (c) statistics reset twice (2026-06-05 class fix; 2026-06-11 example cutover
— discount per `audits/2026-06-11_oq109_phase_b/EXAMPLE_INHERITED_SIGNATURES.md`). Engine
re-witnessed correct at HEAD before any edit (`signature_detection.pl:1081–1096` source-2
RULED-OUT + explicit-claim clause present; `:1404–1422` sibling likewise). Corrected: CLAUDE.md
FNL block (present-tense bait description → past/regime-scoped + canon), v8 §9.4 (resolved +
surviving prohibition), §9.6 + Appendix open-lists → {OQ-195, OQ-205}, README open-obligations
likewise, MEMORY.md hook. **Downstream unblock:** OQ-138's `false_natural_law` member was
deferred "pending OQ-70" 12 days AFTER the close (stale premise at authoring; no residual limb
exists in the OQ-70 body) — gate-expiry annotation added, member now rulable-once-witnessed
(ruling stays in the OQ-138 blocked_on_human queue); the parked `neutron_star`/FCR RED
adjudication (orphaned "under OQ-70") re-homed into OQ-138's route-purity limb. Probe note:
the stale-phrase grep needed a wrap-proof pattern ("OQ-70 is ruled", not "Until OQ-70 is
ruled") — the v8 instances line-wrapped and the first probe missed them; control caught it.

---

## 2026-07-02 — OQ-135 RESOLVED: v8 adopted (seat/gauge/orientation); v8 paper authored; README/CLAUDE.md refreshed; vocabulary migration wave
**Files:** docs/deferential_realism_paper_v8.md, README.md, CLAUDE.md, AGENTS.md, ISSUES.md, docs/seat-theorem-v1.md, docs/one_seat_audited.md, docs/design/design_discipline.md, docs/metrics_as_routing.md, docs/technical/paper_versioning.md, docs/v8/foundations/README.md, docs/logic.md, docs/logic_thresholds.md
**Tier:** landed

Operator ruled v8 adoption + spec Q4 **wholesale** (plan approval, 2026-07-02). Shipped in
four phased commits: `4ea2c2d5` the v8 paper (77KB, entry point + canonical vocabulary; §5.4
bridge table; §5.7 kill-condition; Theorem 2 |real-seat| caveat in-body → OQ-195; §6.4 ε
declaration discipline handed forward); `16143c15` review-response Appendix (operator ruling
after multi-model review: adopt only the Perplexity point — a clean current-state statement at
document end, no terminology-evolution baggage; other suggestions filtered as LLM-bias);
`7c4cca6f` README rewrite (all quantitative claims re-witnessed on disk same session);
`64a44514` CLAUDE.md what-this-repo-is + canonical-paper pointer (v8 entry point; v7/v6.13.1
stay the detailed records — v7 §-references elsewhere in CLAUDE.md remain valid). Phase-4
commit: OQ-135 close (dead-hash note: `fd1ee561` does not resolve; guard cited by
artifact/gate/audit), OQ-03 03b unblocked, OQ-195 propagation update, migration notes in the
five named docs, memory sweep (2 files), foundations README `core_v4.2`→`core_v4.3` link fix
(3 occurrences). Verification witnessed at each commit (obligations grep-checklist; two-seat
sweep judged per-hit; fresh-agent self-containedness control 7/7, its confusion list applied).

**Near-fork DECLARED (Pattern 2 flag, not resolved): `docs/v8/foundations/` is source
material; `docs/` + `config.pl` stay canonical.** Survey (2026-07-02): 8 files are byte-identical
copies of live docs (incl. `core_v4.3.md`, `debugging_philosophy.md`, `metrics_as_routing.md`;
`prolog_v6.8.md` = `deferential_realism_paper_v6.8.md` renamed); 4 are STALE pre-April
snapshots of live docs (`logic.md`, `logic_extensions.md`, `logic_thresholds.md`,
`omega_variables.md` — cite docs/ for current claims); 11 have no repo counterpart by filename
(the v4.x domain suite); and `deferential_realism_paper_v6.9.md` there is ~12KB LARGER than
docs/' copy (carries a related-literature section absent from the live file — which v6.9 is
"the" v6.9 is unresolved). foundations' seven-category framing (incl. "Naturalized" as a
category, internally contradicted at core_v4.3.md:46,117) is historical; the live taxonomy is
six types + naturalized as cascade outcome (v8 §3.3).

**Mojibake REPAIRED (operator ruled fix, 2026-07-02) — and the history corrects two guesses.**
Scope was far larger than the flagged 132 `Ï‡`: `docs/logic.md` carried **1,791** mojibake
sequences across 79 distinct patterns (→ — ε χ × § ≤ ✅ ∧ …), and `docs/logic_thresholds.md`
carried **172** more (same disease, found by sweep). History evidence (per-revision `Ï‡`
counts): the count sat at 127–135 from ≥2026-02-15 through HEAD across every edit — so this
was NOT reintroduced by recent edits (operator's guess) and NOT a regression since March (this
entry's earlier framing): the Feb-2026 "repair" noted in CLAUDE.md was partial, and the
mojibake persisted continuously. Corollary: the clean `docs/v8/foundations/logic.md` is a
separately-CLEANED variant, not a byte snapshot of the repo file. Repair method
(scratchpad `moji_fix.py`, 5 positive controls passed pre-write: repairs-known / clean-untouched /
idempotent / C1-fallback / mixed-run-splits): per-run cp1252 round-trip with C1 fallback, plus
5 hand mappings for truncated sequences whose third byte a later quote/space normalization
destroyed (`â€"`→—, `â†"`→↔, `â†'`→→, `âœ"`→✔, `â¤ |`→`⤠|`, each context-verified against the
clean variant). Witnesses: residual audit ZERO suspicious runs in both files; diff balanced
1,127/1,127 lines, encoding-only; the pre-March rope-bypass line now byte-identical to the
clean variant's. One spec-content oddity surfaced, NOT decided in the encoding pass: the
rope-gate bypass symbol is `⤠` (U+2920) in both the repo file and the clean variant — likely
an ancient corruption of `⊤` (vacuous-true) predating all snapshots; cosmetic (the engine
implements the OQ-01 bypass regardless), flag only.

---

## 2026-07-02 — OQ-137 RESOLVED (reading registry + totality suite + pipeline gate + sweep fixes); OQ-136 evidence in (haiku/contradictions authoring artifact vs genuine mcc)
**Files:** prolog/reading_registry.pl, prolog/tests/test_reading_totality.pl, prolog/commentary_census.pl, prolog/signature_detection.pl, prolog/report_generator.pl, prolog/cs_drift_engine.pl, prolog/cs_axiom_engine.pl, prolog/tests/test_cs_drift_engine.pl, python/run_pipeline.py, python/audits/oq136_bucket_provenance.py, audits/2026-07-02_oq136_census_bucket_provenance/, audits/2026-07-02_oq137_reading_totality/
**Tier:** landed

**OQ-137 close (Phase 5+6, commits `486756fe`/`ed851eb7`+gate):** 41 predicates classified
(classification_table.md); defects fixed: explain_signature missing `unknown` clause → the
report signature section silently TRUNCATED on one claim-authoring unknown-signature constraint
(planted witness 0/110 → 111/111; latent on live corpus — contradictions files author no claims);
cs_terminal_attractor overlapping rows (dup + order-dependent terminals; row-disjoint, first
solution preserved on all 42 combos); cs_has_axioms/cs_axiom_inconsistent doc key +C→+UID
(constraint-name key never fires silently). test_cs_drift_engine was RED since the corpus reset
(fixtures deleted) — rebuilt self-contained, 11/11. **OQ-136 RULINGS EXECUTED (operator,
2026-07-02, post-review):** R1/R2+R6 → OQ-202 minted (ONE generation OQ: haiku +
contradictions paths under-emit stakeholders[]/founding_problem_status; contradictions also
stamps no story_provenance — folded, same path one witness); R4 → OQ-203 (excluded-role
evidential-vs-structural vocabulary, standalone) + OQ-204 (mcc first-class reporting GO, with
candidate-semantics + denominator-caveat design constraints); R3 q6_signature_unknown kept with
a ONE-LEGGED caveat written into WRITEUP + close (statistical leg only — the pre-registered
hand-read leg was not run for that bucket); R5 kept; census `no_agent_seats` out-of-domain
declaration RATIFIED (provisional stamp replaced). OQ-136 → resolved (compress-on-close;
denominator caveat kept intact as still-operative). **Standing guard: `run_pipeline.py`
`_phase_prolog` opens with the reading-totality suite as a sequential fail-fast gate** (first
plunit gate in the pipeline; adds one corpus-loading swipl run to each pipeline invocation);
wiring control witnessed: planted broken registry entry → SystemExit red; clean → green;
per_constraint byte-identical. **Tripwires for future instances:** (1) `[C]-m:g(...)` and
`V^m:g(...)` parse WRONG (`:` is priority 600, looser than `-`/`^`) — parenthesize `(m:g(...))`
in templates/setof; the first OQ-137 sweep passed VACUOUSLY on this until its planted controls
caught it. (2) When adding a reading predicate an aggregate could consume, register it in
`reading_registry.pl` in the same change — registration is opt-in (named residual risk).

**OQ-137 slice (commits `a81d4c83` behavior-preserving + `2453b922` output-changing):**
`reading_registry.pl` (`aggregatable_reading/3`: 5 proven-total seeds + `in_contention/3`
partial-by-design; `census_source_backing/2` anti-fork bridge) + registry-driven
`test_reading_totality.pl` (exactly-one over declared domain; two positive controls — planted
silent stub flagged AT the hole, two-sided). `commentary_source(consensus)` added: compound
`manufactured_consensus_candidate(Excl)` flattened to functor; `no_agent_seats` out-of-domain
declared PROVISIONAL (source comment — it pre-judges the OQ-136 bucket); `seats_untyped` absence;
no prevalence bucket (candidate flag ≠ positive finding). Witness: suites 10/40/8 green;
run_pipeline exit 0 + mtimes advanced; census diff additive-only; per_constraint byte-identical.

**OQ-136 (pre-registered; PROPOSAL frozen `0ba48b4c` BEFORE the join; execution `2b66dedc`):**
q6_unmeasured (26) + no_agent_seats (26) cluster on model AND prompt_commit (p_holm=8e-4;
haiku 16/28 + all 9 `*_contradictions`; 25/26 overlap) = ONE generation-path artifact — haiku
path authors prose + constraint_beneficiary but NO founding_problem_status / stakeholders[]
(prose plans the seats it never emits); contradictions path also stamps NO story_provenance/8.
q6_signature_unknown (16) + manufactured_consensus_candidate (9): NOT clustered; mcc hand-read
8/9 genuine (1/9 radiative_levitation false-positive by its own text → excluded-role
evidential-vs-structural vocabulary gap). extraction_unnameable (3): compound (seat limb =
haiku artifact; victim limb genuine-to-the-reading 2/3 RULED). **Dispositions R1–R6 are
blocked_on_human** (ISSUES.md OQ-136; proposals in the audit WRITEUP.md).

## 2026-07-02 — Cross-leg check: OQ-52 replicates member-level; OQ-45's phenomenon recurs via DISJOINT members (draw-variance); live-leg hidden-winner exists
**Files:** audits/2026-07-01_oq45_oq52_hidden_winners/, prolog/testsets_haiku/temple_sacrifice_commitment__performance_only.pl
**Tier:** landed

Operator-requested check of the 2026-07-01 closes against `testsets_*` + `kernel_v1` (B5 section
of the audit WRITEUP). NL populations under the pre-fix cascade: kernel_v1=**26 (matches the
recorded 2026-06-10 matrix — aggregate control PASS)**, haiku=8, flash=5, live=0; all PRE/MID
dispatch controls PASS; twins classified via `classify_corpus` with the model-fingerprint gate
(`pipeline_output_{haiku,flash}.json`, n=960 each). All 39 twin+kernel NL members content-read
exhaustively (rubric v2): kernel_v1 → 4 hidden-winner (all social constraints; quotes verified
5/5), haiku → 0 (two reader calls downgraded on adjudication: gain lived in sibling readings the
file excludes), flash → **1 hidden-winner on a LIVE leg** (`temple_sacrifice_commitment__
performance_only`, "Messianic restorationists are beneficiaries" beside a Mountain claim). OQ-52's
authored-channel finding replicates at 100% on every live leg (haiku 113/113, flash 83/83, live
8/8). Two NL-gate coarseness data recorded in the OQ-45 addendum: victim-bearing stories certify
(gate checks beneficiaries only); the 404 h1=4 uniformity is an original_v6 template artifact
(twins mix h1∈{0,4}). Draw-variance witnessed (OQ-26): article_27/aneyoshi kernels read
differently across twins — distinct draws, not re-measurements. CITATION AMBIGUITY WITNESSED: 'HEAD yields strict=235' was read as a canonical-corpus count
when it was the HEAD *engine* on kernel_v1 (one computation; live n=119 leg: manifest=71,
strict=4, loose=4) — when citing counts across classify_corpus runs, name BOTH the corpus and
the code state (rule promoted to CLAUDE.md Running the System). VERB DISCIPLINE: only OQ-52
replicated member-level; OQ-45's twin/kernel hits are disjoint from the six 404 hits, the
EXPECTED consequence of new draws — the phenomenon recurred, no member-level replication is
claimed or possible.


## 2026-07-01 — OQ-45 RESOLVED (YES: hidden winners in the 404) + OQ-52 RESOLVED (W1 leg delivered; population counts are engine-regime-relative)
**Files:** python/w1_sheaf_join.py, prolog/signature_detection.pl, audits/2026-07-01_oq45_oq52_hidden_winners/
**Tier:** correction-key

Both OQs closed as the presents-as-natural / hidden-winner pair (OQ-52 = beneficiary-AUTHORED
side, OQ-45 = beneficiary-SILENT side; NOT exhaustive — a hidden-winner neither
false-mountain-shaped nor NL-certified falls through both). Full evidence + writeup:
`audits/2026-07-01_oq45_oq52_hidden_winners/WRITEUP.md`. Branch `oq45-oq52-hidden-winners`.

**Citation corrections (why correction-key):**
1. **The OQ-52 "16 of 98" false-mountain count is engine-regime-relative — do not cite it as a
   current fact.** On HEAD, kernel_v1 yields strict=235 + loose=58 of manifest_presheaf=944 (the
   944 matches the OQ-197 acceptance controls, `34ff919f`). Member-level assignments were the
   stable part: all 5 recorded names recover with EXACT H1 (quran=4, article_9=5, abrahamic=6).
   The original 16-list was never saved and is not reconstructible. General lesson: save member
   LISTS, not counts, when a selection is engine-computed.
2. **The "all false-mountain rows carry both authored channels" claim is now 289/293:** 4
   victim-only rows exist at HEAD (repair sentinels screened, 0/1106). Cite the 2026-07-01
   re-measure, not the 2026-06-02 absolute.
3. **A naive NL sweep on HEAD returns 0 everywhere** (has_viable_alternatives dead-by-range,
   `8b5a34b8`/OQ-113). The 404 population is recoverable ONLY via the pre-fix overlay swap —
   recipe + controls in `b1_nl404_probe.pl` (PRE=unknown / MID=false dispatch controls,
   Sig-UNBOUND sweep; aggregate control: count==404 PASS).
4. **OQ-45 answer is YES, per-story only — no prevalence claims** (chimera corpus, OQ-70/OQ-25).
   6 hidden-winner (spot-verified quotes): bucket (i) extraction wearing the mountain frame
   (`repeat_player_structural_advantage`, `demographic_elimination_imperative`,
   `attention_as_capturable_resource`, `capability_compulsion_gradient` borderline); bucket (ii)
   genuine natural core with unauthored ecosystem winner (`gilgamesh_mortality_limit`,
   `ecological_carrying_capacity`). Bucket (ii) is the design note for any future NL re-powering
   (GAP-08 §7): gain-AROUND-persistence ≠ gain-from-authoring.

**Landed:** `w1_sheaf_join.py` rows now carry `incomparable_mass` + `material`
(W1_MATERIAL_PROVISIONAL=0.05, a LABEL freezing OQ-51's "~0.05" prose gate, never a filter);
stale "unknown=N/A declared but unbuilt" prose retired (`e8189d10`; column-diff witnessed
behavior-preserving). `classify_corpus` precedent: kernel_v1 (n=1106) and original_v6 (n=3380)
classified into own manifest-bearing outputs without touching the canonical artifact.

**Method note (rubric control):** the B3 content-rubric v1 pre-flight FAILED 0/3 on known
false-mountains (their prose contests naturalness in narrator voice); v2 (in-frame naturalness
counts) passed 3/3. A content rubric is an instrument — pre-flight it on known positives before
reading the target population, or a 0-flagged read is unfalsified.


## 2026-07-01 — OQ-41 RESOLVED (row-26 five-site expansion) + OQ-40 RESOLVED (doc lift) + OQ-201 minted (row-22 spin-out)
**Files:** ISSUES.md, docs/design/two_axis_architecture_v7.md, audits/2026-07-01_oq41_row26_expansion/, prolog/signature_detection.pl, prolog/drl_fpn.pl, prolog/covering_analysis.pl, prolog/gap_diagnostic.pl, prolog/omega1_audit.pl
**Tier:** landed
Row-26 five-site expansion (HEAD `27afde7a`; no engine change — behavior-preserving; gate GREEN,
validation 0 errors). Step-0 grep: all 2026-06-24 cites exact at HEAD (no drift). Two flagged unknowns
resolved against substrate: **(1) `drl_fpn:197` is a sentinel pass-through (`IP<0.0 -> NewEP=IP`, `IP=-1.0`
when `fpn_intrinsic` absent), NOT a fabricated default → CARVED OUT of row-26**, no verdict assigned; the
prior entry had conflated `:197`'s label with `:206`'s trigger. **(2) `covering:490`'s `0.5` branch is a
`constraint_metric`-presence guard, NOT "interpolation off-grid" — the plan's off-grid trigger-class had
zero members.** Verdicts: `covering:490`, `gap:120`, `omega1:102` (+ Supp/Theater sibs) = **DORMANT/LOCKED**
(reject-guard + must-fire control fires + 0 pipeline callers; OQ-44 once-for-class); `drl_fpn:206`
`Immunity=0.5` = **NEUTRAL-by-corpus (cosmetic-if-fired)** — firing-marker patch shows 0 natural fires over
testsets/(119) AND kernel_v1(1106), positive control fires (measured-empty, not didn't-look), sink is
`fpn_ep`→diagnostic only, never `dr_type`. OQ-40 rows 19–20 split RULED-INTENDED lifted into
`two_axis_architecture_v7.md` §"Representation grounding" (`constraint_metric/3`=scalar/observer,
`measurement/5`=temporal/committer). Row-22 → **OQ-201**: `compute_temporal_stability` reads the scalar
store not `measurement/5`; coverage witness — folded metric=`suppression_requirement`, 107/110 (testsets) &
934/1106 (kernel_v1) reach-the-gate constraints author an ignored temporal series (SUBSTANTIAL → repoint is
eventual fix, deferred per off-grid trap), and **>1 scalar level = 0 on both corpora → variance path dead,
gate is a degenerate presence-check**. Positive control catches a known series on both corpora.

## 2026-07-01 — R4 RULED → OQ-200: detector_calibration carried as corpus-level OQ, NOT wired; module now TRACKED-but-unwired
**Files:** ISSUES.md, docs/design/detector_calibration_omega_proposal.md, prolog/detector_calibration.pl, audits/2026-07-01_oq197_r4_recompute/
**Tier:** tripwire

Operator ruling closing R4 of the detector_calibration proposal. After OQ-197 unblocked the baseline and the R4
recompute retracted the ~3× inflation (net-new = **39/41 determinable**, real undetermined-inflation only 4/12),
the per-firing + per-constraint diversity measurement showed the net-new is **low-KIND-entropy** (5–6 distinct
`(Class, author→engine)` signatures, ~90% two directional patterns). Decomposition: **false-summit re-surface**
(`mountain→tangled_rope`, 13/8 constraints = OQ-70/FNL through the author-engine axis, not genuinely new) + a
**`tangled_rope→rope` author-over-claims-contestation residual** (21/27 constraints, the constraint-majority and
the module's genuinely-distinct signal) + a small severity/singleton tail. Volume-vs-breadth: per-SEAT-firing
false-summit dominates (loud-narrow); per-CONSTRAINT `tangled_rope→rope` dominates (quiet-broad) — both correct,
different denominators. **Ruling: carry as an aggregate corpus-level OQ (OQ-200), do NOT wire per-constraint** —
39 near-repetitive firings each carrying the identical "calibration open" caveat is a query, not 39 findings. The
binding reporting condition (same as OQ-199 for the gap omega): firings are "author↔engine directional
disagreement, calibration open (Ω_E), FP-rate unset (Ω_P)," NEVER "miscalibration detected."

**TRIPWIRE — `prolog/detector_calibration.pl` is now TRACKED but UNWIRED (supersedes the UNTRACKED tripwire
below).** Committed this session as reference implementation; loaded by nothing, wired into no report. The
question it computes is carried at corpus scope by OQ-200. Do NOT wire it (into `run_pipeline`, any report, or
via `use_module`) without REOPENING R4 — that needs an external calibration answer (Ω_E) + an accepted FP-rate
(Ω_P). The committed `already_covered/1` behavior (undetermined-aware post OQ-197) is measurement-only.

---

## 2026-07-01 — gate check added: human gap surfaces must distinguish no_gap from undetermined (Pattern-6 guard)
**Files:** scripts/gate.sh, python/check_gap_status_surfaces.py, python/query.py
**Tier:** tripwire

New `scripts/gate.sh` check `gap surfaces` (`check_gap_status_surfaces.py`): a paired synthetic no_gap/
undetermined fixture asserting the three human-facing gap renderers (`tensions_ledger.build_block`,
`enhanced_report.build_omega_section`, `query.format_gaps_block`) produce DISTINGUISHABLE, correctly-labeled
text — converting the Pattern-6-downstream class (undetermined reading as "no finding") from "caught if someone
asks" to "fails red". Motivated by the enhanced_report catch running on a passing question, not a control, on the
highest-propagation surface; this bug recurred at 4–5 sites in the OQ-197 work, each caught by attention. The check
is positive-controlled (embedded self-test + external monkeypatch both confirm it goes RED on a collapsing renderer,
so it is not vacuous). **When you add a NEW human-facing surface that renders gap/omega state, add it to this check's
renderer list** — otherwise the guard silently under-covers (its own Pattern-1 risk). `query.py` gap block was
extracted to `format_gaps_block/1` for testability.

---

## 2026-07-01 — OQ-197 ruling (a) bound to OQ-199 reporting-condition; R4 recompute retracts the ~3× inflation
**Files:** ISSUES.md, audits/2026-07-01_oq197_r4_recompute/, prolog/detector_calibration.pl
**Tier:** correction-key

Ruling (a) (keep stakeholder source) finalized as non-redundancy-established / reliability-UNRESOLVED — bound to
OQ-199 as a BINDING reporting condition: while OQ-199 open, (a)-sourced gap-omega firings are reported as
"authored-stakeholder disagreement," never "validated cover-story detection." **R4 recompute done READ-ONLY**
(inverting the circular hold — proposal ruling was starved for the number R4 produces; loaded detector_calibration.pl
read-only, no wire/commit). On the fixed detector guarding on undetermined: net-new = **39/41 determinable** (not
14/12, not 43/53); genuine undetermined-inflation = only **4/12** (≈1.1–1.3×). **The "~3× inflation" is RETRACTED** —
it mislabeled the no_gap bucket (25/29: extraction_blindness examined-and-CLEARED, detector_calibration adds a distinct
author-vs-engine axis) as artifact, the same no_gap↔undetermined conflation OQ-197 fixed. Consequence: the
detector_calibration wire/no-wire proposal ruling can no longer rest on inflation/redundancy (net-new is substantial);
it now turns on the module's own open axes (calibrated? — Ω_E; acceptable FP rate? — Ω_P). Ruling stays operator's seat,
now fed a witnessed number. Do NOT cite the old 14/12-genuine or 3×-inflation forward — superseded by 39/41 + 4/12.

---

## 2026-07-01 — OQ-197 acceptance controls PASS (kernel_v1 944 + twins 29/41 reproduced from substrate); case-(ii) refinement
**Files:** audits/2026-07-01_oq197_acceptance_controls/, prolog/report_generator.pl
**Tier:** landed

The graduation witness for the OQ-197 chain. Counts reproduced from substrate, not the doc:
kernel_v1 canonical-varying=944 exactly (stakeholder_facts=0); twin detector_calibration net-new=43/53 and
net-new ∩ stakeholders-present ∩ detect_gap_pattern-fails=29/41 exactly (read-only load of untracked
detector_calibration.pl). **Case (i):** 944 read undetermined(no_seats) under source (a), never silent 0;
negative control same run — canonical (b) discriminates gap=944/no_gap=152. **Case (ii) REFINEMENT:** the
29/41 are NOT uniformly undetermined — three-valued split is haiku 4 undetermined + 25 no_gap, flash 12 + 29.
The 4/12 (<2 power positions) are the genuinely-inexaminable false-`[]` rescues → now undetermined; the 25/29
have ≥2 seats spanning ≥2 powers agreeing → genuine no_gap. None silent 0 (all labeled). The doc's premise
that the 29/41 were uniformly "insufficient" was imprecise — the fix is more precise. Negative control same
run — source (a) produces gap+no_gap+undetermined on both twins. OQ-197 fix witnessed end-to-end; only (5) R4
recompute remains, held on the detector_calibration proposal ruling.

---

## 2026-07-01 — OQ-197 consumer wiring landed (4 live sites, labeled); detector_calibration.pl is UNTRACKED/unwired WIP
**Files:** prolog/json_report.pl, python/shared/schemas.py, python/query.py, python/tensions_ledger.py, prolog/detector_calibration.pl
**Tier:** tripwire

Commit `fffca9d1`. Wired the OQ-197 three-valued `gap_status` through every LIVE read site so undetermined
never collapses into measured-no-gap (Pattern 6), carrying the human-readable LABEL not just the internal
representation: json_report per-constraint `"gap_status"`+`"gap_undetermined_reason"` (schema-registered) and
corpus-level `constraints_gap_examined`/`constraints_gap_undetermined`; `query.py --detail` (also fixes a
latent `len(None)` crash — `.get("gaps",[])` returns None on present-null); `tensions_ledger.py` dedicated
gap-operability line; `enhanced_report.py` `build_omega_section` (a 5TH live site first cleared WRONG — grepped
for a "gaps" render, a proxy, not "distinguishes undetermined at a human surface"; it collapsed no_gap/undetermined
into "not yet enriched" at the primary `constraint_reports/*.md` surface; caught by the operator's question, now
leads with a gap_status line). Witnessed at the JSON boundary (pipeline exit 0, mtime advanced): behavior preserved
(`constraints_with_gaps`=57, `omega_count`=57), companions examined=89/undetermined=30, 0 consistency
violations, schema 0 errors, labels distinguish gap/no_gap/undetermined on both human surfaces.

**TRIPWIRE — `prolog/detector_calibration.pl` — SUPERSEDED by the 2026-07-01 R4-ruling entry at the top of
this file.** (At the time of this entry it was untracked and unwired, awaiting the proposal ruling.) It is now
TRACKED-but-unwired reference by the R4 ruling; still loaded by nothing and wired into no report. See the top
entry for the current disposition and the do-not-wire condition.

---

## 2026-07-01 — OQ-197 (a)/(b) cross-tab: canonical (b) ≡ h1_band, stakeholder (a) distinct; canonical-source bug fixed
**Files:** prolog/report_generator.pl, audits/2026-07-01_oq197_source_h1_crosstab/
**Tier:** correction-key

Commit `6bda83ec`. Made `gap_status`/`detect_gap_pattern` source-explicit (`/3`) to evaluate both (a)/(b)
sources per constraint; firing under default byte-identical (57=57). Cross-tab on the both-sources-determinate
testsets subset (n=84): canonical (b) firing EXACTLY coextensive with `h1_band>0` (58/58, 26/26, zero
off-diagonal — definitional, same orbit) ⇒ (b) is a redundant recomputation of `h1_band`; stakeholder (a)
distinct on 3/84 (authored-stakeholder disagreement `h1_band` lacks). Evidence points toward ruling (a); ruling
stays operator's seat, now evidence-fed. **Twins extension (2026-07-01):** on the both-determinate subsets
(testsets 84 + haiku 452 + flash 661 = 1197) canonical (b)↔`h1_band>0` has 0 off-diagonal (definitional —
confirms wiring, not new evidence); stakeholder (a) distinct on 36/1197 (3/19/14). Twin `h1` computed in
Prolog (`cohomological_obstruction/3`), positive-controlled vs pipeline `h1_band` on testsets (0/119) before
use. **Corpus-independence caveat:** haiku+flash are TWINS (same seed, different backend → correlated), so
this is ONE independent corpus + one correlated pair, NOT triple replication. Establishes non-redundancy
ONLY — (a) irreducible to `h1_band` as a construction; whether the divergences are (a)-correct vs authoring
noise is OQ-199 (open). **RULING (a) — keep the stakeholder source (operator, 2026-07-01):** (b)=duplicate
of `h1_band` (cruft), (a)=non-redundant; `gap_seat_source` stays `stakeholder` (no code change — default
already implements (a)). Ruling resolves redundancy, NOT reliability (OQ-199). Evidence
`audits/2026-07-01_oq197_source_h1_crosstab/`. **Correction to b616e625:** its canonical (b) seat clause used
`constraint_classification/3` with an UNBOUND context (mode `+Context`) → 0 seats for every constraint (a dead,
unwitnessed branch — my contract witnesses were all stakeholder-path). Fixed to `drl_core:dr_type/3` via
`logical_fingerprint:standard_context_for_power/2` over the 4 canonical positions (the `write_perspectives`/
`h1_band` source). Lesson: a branch dead under the default config still needs its own witness — the cross-tab
was the first thing to exercise it. Twins extension pending (see ISSUES.md OQ-197 step 3).

---

## 2026-07-01 — OQ-197 three-valued gap operability CONTRACT landed (branch, behavior-preserving); 6th consumer found
**Files:** prolog/report_generator.pl, prolog/tests/test_gap_operability.pl, python/tensions_ledger.py, python/json_report.pl, prolog/detector_calibration.pl
**Tier:** landed

Branch `oq197-three-valued-gap-operability`, commit `b616e625`. Added `report_generator:gap_status/2`
→ `gap(...)` | `no_gap` | `undetermined(no_seats|single_seat|single_power_position)`, closing the
Pattern-6 collapse in the gap detector. Built **source-parameterized** per operator ruling (2026-07-01):
`gap_seat_source/1` (default `stakeholder`; `canonical` via `constraint_classification/3` written) feeds
BOTH `detect_gap_pattern/2` and `gap_status/2` through `seat_type_reading/2-3`, so the OQ-197 (a)/(b)
ruling is a one-line change. `detect_gap_pattern/2` firing logic UNCHANGED — the split is additive.
`gap_coverage/1` lifted from ≥1-seat proxy to the operability precondition (case-(ii) fix at the `"gaps"`
field). Witnessed: firing byte-identical (57=57 diff-empty on testsets); `gap_status` total/deterministic
(119/119; gap=57 no_gap=32 undetermined=30); `dataset_recycling_amplification → no_gap`; 9 two-sided
plunit controls pass; 0 new corpus-suite failures (20 pre-existing mountain/nl drift, baseline-confirmed
old==new).

**Finding — the ledger "no gap pattern matched" line is a SIXTH consumer with its own bug.**
`tensions_ledger.py:131` computes index-mismatch from `perspectives`, not the `gaps` field, and
`{v for v in persp.values() if v}` counts `unknown` as a diverging value — so
`dataset_recycling_amplification` reads "perspectives diverge" purely because `analytical` is untyped
(scaffold-vs-unknown, not a real gap). OQ-197's detector fix does not reach it; it needs repointing to
`gap_status` or an `unknown` filter. Remaining OQ-197 graduation steps + full 6-site consumer map are in
ISSUES.md OQ-197 (Progress 2026-07-01). Sequenced: wire consumers → two positive controls → h1_band
cross-tab → (a)/(b) ruling → R4 recompute (held).

---

## 2026-06-30 — detector self-assessment: Slice A (author×engine cross-tab) LANDED; Slice B (calibration omega) proposal awaiting ruling
**Files:** prolog/routing_sink.pl, docs/design/detector_calibration_omega_proposal.md, outputs/routing_sink.json
**Tier:** landed

From the Elias-Thorne report review: web-Claude asked whether Prolog can address the
"is the snare/rope detector calibrated" question. Answer split three ways — (A) computable
seat-agreement aggregate, (B) an authored apparatus-directed omega, (C) auto-closing the
verdict = category error (no ground truth in the testset; authored type is a seat, seat theorem).

**Slice A LANDED** (`routing_sink.pl`, commit `f6921ac1`): added `author_engine_crosstab` +
`author_engine_crosstab_summary` to `routing_sink.json`'s manifest — a (authored_type ×
engine_type) confusion cross-tab over the existing per-seat `seat_diff` records. Diagonal =
agreement (`no_route`), off-diagonal = divergence by type-pair (tangled_rope→snare=106 dominant).
**Hard label: SEAT-AGREEMENT, NOT calibration** — `divergence_rate` (0.77) is a two-seat
disagreement rate, never a detector false-positive rate (convergence is stable, not correct).
Positive control reconciled: diagonal 91==no_route 91; off-diagonal 305==author_engine_divergence
(255)+engine_exit_table_review(50); both_speak 396 + both_silent 36 + engine_abstained 44 = 476 =
119×4. (Caught a self-inflicted A-E-vs-A-Engine template typo — unbound key, Pattern-5 vacuous
guard — via that control before shipping; added nonvar/2 guard.)

**Slice B PROPOSAL, awaiting operator ruling** (`docs/design/detector_calibration_omega_proposal.md`,
commit `c4864999`): a `detector_calibration` omega the engine MINTS OPEN (computable firing
condition: computed snare/rope ∧ hidden-extraction shape — theater∨coupling-masked∨no-exit-victim;
all fields verified present) but does NOT close. Typed as an Ω_E (hit rate, awaits external labeled
data) + Ω_P (acceptable FP rate, a value-decision) PAIR — conflation is the "when to stop verifying"
trap. NOT wired/fired: R1 threshold, R2 typing, R3 engine-minted-vs-authored, R4 mint+wire are the
operator's seat. Generalizes the existing story-local detector-doubt convention
(press_reformation_causality omega).

## 2026-06-30 — perspective_chi d/f_d fork fixed (resolved-context derivation); report frame added
**Files:** prolog/constraint_indexing.pl, prolog/json_report.pl, python/enhanced_report.py
**Tier:** landed

`json_report:write_one_perspective_chi` exported `d`/`f_d` derived on the UNRESOLVED
canonical power atom, while `chi` (via `extractiveness_for_agent/3`) resolves coalition
power internally. For any perspective whose power coalition-resolves (`powerless→organized`),
the exported `f_d` (from d=0.9) forked from `chi` (from d=0.5): **40/119 live constraints had a
`powerless` row where `chi ≠ ε·f_d·σ`**. Surfaced by web-Claude reading the Elias-Thorne
constraint reports (`f(d)=1.358606` appearing with `d=0.500`); its two hypotheses (f saturates;
d-table reused) were both falsified — `f` is d-dependent and `d` is observer-position-keyed
(`constraint_indexing.pl:478-487 power_role_heuristic/4`).

Fix: factored resolve+derive into `constraint_indexing:agent_resolved_directionality/4`
(exported), used by BOTH `extractiveness_for_agent/3` and the JSON writer so they cannot fork.
Witness: model_collapse_feedback powerless before `chi=0.4056 d=0.9 f_d=1.358606` (0.78·1.3586·0.8=0.848≠chi)
→ after `chi=0.4056 d=0.5 f_d=0.65` (=chi). Behavior-preserving: 0 type/classification changes,
0 chi changes across 119 constraints (re-run pipeline exit 0, mtime advanced); forked rows 40→0/440.
Commit `6d1df7d1`.

Also (commit `5e5830df`): prepended a "HOW TO READ THIS REPORT" frame to `enhanced_report.build_header`
— purpose is to surface SEATS, divergence (between seats / from authored commentary) is the finding,
RED = authored victim/beneficiary direction (OQ-187) not a moral verdict, d is observer-position-derived.
Tripwire for a future agent: any NEW consumer that reports `d`/`f(d)` alongside `chi` must derive them
via `agent_resolved_directionality/4`, never `derive_directionality/3` on the raw canonical context —
or the fork reopens silently for coalition-resolving perspectives.

## 2026-06-30 — OQ-38 RESOLVED: reproducible orphan-xref tool built; four calibration orphans stripped; OQ-196 minted
**Files:** prolog/orphan_xref.pl, python/audits/oq38_orphan_sweep.py, prolog/drl_composition.pl, prolog/utils.pl, ISSUES.md, AGENTS.md, audits/2026-06-30_oq38_orphan_xref/
**Tier:** landed

Replaced the discredited 2026-05-31 ad-hoc grep sweep (`217-candidate upper bound`,
hand-transcribed into ISSUES.md) with a reproducible **tool-native funnel**.

- **New tool `prolog/orphan_xref.pl`** — `library(prolog_xref)` clause-head-vs-body separator;
  mirrors `check_stack.pl` (load-path-independent, **diagnostic NOT a pipeline gate**). Emits per
  `Name/Arity`: file, exported?, static-caller set (module-stripped), class
  (`LIVE`/`ENTRYPOINT_CLI`/`STATIC_ORPHAN`). Caller matching is global `Name/Arity` —
  conservative-by-design (biases LIVE; a false orphan is the only dangerous error).
- **Driver `python/audits/oq38_orphan_sweep.py`** — masks static orphans against the dynamic
  surface (Python goal-strings + Prolog name-construction prefixes), emits the funnel.
  *Self-exclusion gotcha:* the driver NAMES its strip targets in `CALIBRATION_FOUR`, so it must
  exclude its own path from the Python-surface grep or it false-positives every target as
  dynamically-reachable (witnessed + fixed this session).
- **Funnel (121 sources):** 614 exports (grep claimed 528 — **+86, grep undercounted**), 201
  STATIC_ORPHAN (grep 217 — −16), 29 dynamic-masked, **M=170 real-orphan upper bound** (post-strip).
- **Stage-1 hard gate:** `cs_reference_frame/2` LIVE (the OQ-35 adversarial case), and
  `non_monotonic_trajectory/2` LIVE with caller in **`metric_drift_report.pl`** — the OQ census's
  `drift_report.pl:164` cite was stale (file absent); corrected in ISSUES.md.
- **Four stripped** (commits A `736783e4` slope-pair, B `6a3acf1d` safe_get-pair; tool +
  `c9be12ca`). Behavior-preserving witnesses: load gate exit 0, validation suite byte-identical
  (timing-normalized), pipeline `per_constraint` sha256 unchanged `d9c85bec…` mtime advanced.
- **Cascade:** Commit B newly orphaned `safe_get_category/3` (sole caller removed) — routed to
  **OQ-196** (value-adjudicate the M=170 remainder), NOT stripped (scope ruling = strip only the
  four). Full writeup: `audits/2026-06-30_oq38_orphan_xref/WRITEUP.md`.

## 2026-06-30 — OQ-37 RESOLVED (read-but-unauthored metric census re-dispositioned); GAP-23 minted
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/data_validation.pl, python/generate_constraint_pl.py
**Tier:** landed

OQ-37's read-but-unauthored `constraint_metric` census re-dispositioned at its root: all six target
names (inevitability, internalization_depth, resistance_to_change, accumulation_speed, sunset_time,
alternatives_available) trace to the **fixed compiler emit set** (`generate_constraint_pl.py:608-635`)
— "author" = grow compiler+schema+validator+prompt, "remove" = strip a consumer. Authoritative
cross-corpus census (FACT pattern `constraint_metric(_,Name,_)`, not bare name): **all 6 are 0 on
testsets/haiku/flash/kernel_v1 = 3,142 stories**; controls resistance/extractiveness fire on every
leg. Witness + per-probe evidence: `audits/2026-06-30_oq37_census_redispose/`.

Dispositions: `inevitability` read already removed (D2 strip, `constraint_bridge.pl:20-25`),
capability superseded structurally by `false_natural_law` (`signature_detection.pl:1018,1040`);
compound grid metrics resolved by OQ-93; χ-partition closed (`3ab3ace4`); Part D masked-unknowns
moot post-reset; `accumulation_speed`/zero-caller helpers → OQ-38; supp/ε-floor → OQ-48. The two
genuine deferred capability livens (`sunset_time` self-supplied falsification tell;
`internalization_depth` manufactured-consent quadrant + `psych_bridge` never loaded) → **GAP-23**
(priced, operator-seat, reopen on analytical-product demand).

One behavior-preserving engine edit (commit `5b7a8b95`): dropped never-authored
`resistance_to_change` from `data_validation.pl:320` extreme-value monitor. Witness: validation
suite `✓ No extreme values`, 0/1/1 identical before/after; provably byte-identical (0 facts → member
never matches); validation-channel only, does not touch `pipeline_output.json`.

**Correction-key (OQ-64 instance):** `resistance` ≠ `resistance_to_change` — `resistance` is the
NL/coercion-GRID metric (`grid_first_contact_gate.py:48`, mountain-signature feature), a distinct
referent from drift-domain resistance-to-abolition. The proposed `metric_drift_events.pl:174,247`
repoint (resistance_to_change→resistance) was **DECLINED**: `safe_metric/3` fails silently
(`:66`), so `function_obsolescence` dies at its first goal (`alternatives_available`) — the repoint
buys zero behavior while baking a latent wrong-metric identification. Liven the detector's two inputs
together (GAP-23) or leave it dark; never repoint by name-stem.

## 2026-06-30 — OQ-27 RESOLVED (signature-resolved H¹ disclosure); OQ-195 minted (general-n gap)
**Files:** prolog/grothendieck_cohomology.pl, docs/deferential_realism_paper_v6.13.1.md, ISSUES.md, issues/INDEX.*, CLAUDE.md, KNOWN_STATE.md
**Tier:** correction-key

**Ruling: disclosure, not redefinition; no engine behavior change.** The engine already computes H¹
over the **signature-resolved** `dr_type` orbit; OQ-27's gap was that no doc/comment said so. Under
**append-versioning**, `v6.13.md` + v6.8–v6.12 stay frozen — precision landed only in
`v6.13.1.md` (dated OQ-27 amendment + two inline "signature-resolved" qualifications at the intro and
§5.1) and an engine comment at `grothendieck_cohomology.pl` (`orbit_vector/2` + `type_at_context/3`).
v7 §Thm 7 already carried it (no-op; v7 untouched, confirmed by `git diff --stat`).

Path disclosed: `cohomological_obstruction → orbit_vector → type_at_context → dr_type`; inside
`dr_type`, `metric_based_type_indexed` (raw `classify_from_metrics`) **then**
`integrate_signature_with_modal`. So H¹=0 means the *signature-resolved* orbit is a global section —
raw per-context metric types may be maximally heterogeneous (the signature is the cover story, Thm 1).

**Witnesses (manifest `2026-06-30T00:08:22Z`, n=116), denominators kept distinct:** 65 of 86
four-real-seat constraints at H¹>0 (discrimination); *separately* 116/116 reproduction of stored
`h1_band` from the serialized `perspectives` orbit (orbit-reproduction control). GATE GREEN.

**Discovery → OQ-195:** Theorem 2's gap {0,3,4,5,6}/forbidden {1,2} is proven only for **|real
seats|=4**; under the OQ-51 N/A rule the real-seat count varies. Reachable spectra (proven by
enumeration) are n=3→{0,2,3}, n=2→{0,1}; the four `h1_band=2` constraints are 3-real-seat, NOT
counterexamples. The unwritten **general-n** induction is **OQ-195** (P3); the `grothendieck_cohomology.pl:158`
range comment carries a stale-range flag pointing to it.

## 2026-06-30 — OQ-194 RESOLVED: embedded mountain/nl "failures" are correct commentary; one rotted phantom fixture fixed
**Files:** ISSUES.md, prolog/tests/test_phantom_neighbor_filter.pl, python/generate_constraint_pl.py, prolog/testsets/*.pl (16 claim=mountain files), KNOWN_STATE.md
**Tier:** correction-key

Running `cd prolog && swipl -g "[stack], [tests/test_phantom_neighbor_filter], run_tests, halt"`
loads the whole corpus and registers every testset's embedded validation units → **21 fail / 93
pass** (live, 2026-06-29). The 21 are two unrelated things:

- **20 embedded** = 13 `mountain_threshold_validation` + 7 `nl_profile_validation` (the 7 a strict
  subset of the 13). These are the apparatus **correctly commenting** that stories which *claim*
  mountain (`constraint_claim(C, mountain)`, the SURFACE claim) lack true-mountain metrics —
  claim ≠ actual is the DR core. **Not regressions, not WIP.** Tight bars intentional (failure
  marks contention). **The red-as-signal reading is conditional on these tests staying NON-GATING**
  (not in `gate.sh`): a failing *assertion* conventionally means defect; the analogy to a neutral
  linter (OQ-116's `MOUNTAIN_METRIC_CONFLICT`) breaks the moment anything gates on them. Structural
  evidence (holds even if no OQ existed): generator emits both tests ONLY on claim=mountain with
  hardcoded bars (E≤0.25, S≤0.05, AC≥0.85, R≤0.15); 12 of 32 embedded assertions PASS on clean
  mountains (protein/radiative/actinide). All 20 are plunit `: failed`, **zero `: error`**; all 13
  failing files declare `constraint_claim(_, mountain)`.
- **1 `phantom_neighbor_filter:real_target_edge_fires`** = a genuine **fixture-rot defect**. The two
  hardcoded positive-control names (`ai_governance_accountability`, `retirement_security_deficit`)
  rotted out of the corpus at the 2026-06-05 reset (sole archive holder: `kernel_v2_test2`), turning
  the control into a phantom and making the exclusion tests pass **VACUOUSLY** — the OQ-95 guard
  guarded nothing. **Fix:** `two_real_targets/2` self-selects two constraints the filter would NOT
  drop (`\+ drl_purity_network:phantom_subject/1`) and **throws** `insufficient_real_targets` on
  under-supply. Witnessed: 4/4 phantom-filter green on live corpus; loud-failure control on a
  1-constraint overlay throws (setup error, not vacuous pass). Silent rot is now unreachable.

**Landing:** explanatory comment emitted from the generator + backfilled into all 16 current
claim=mountain testsets (`grep -lE 'constraint_claim\([a-z0-9_]+, mountain\)' testsets/*.pl` → 16);
header signpost in `test_phantom_neighbor_filter.pl`. **Deferred calibration → OQ-48** (the hardcoded
bars added as recalibration targets; no new OQ). OQ-194 closed. Two commits (docs ruling / code fix).

## 2026-06-29 — OQ-23/OQ-24 RESOLVED (narrow same-kernel contamination guard); OQ-193 deferred
**Files:** prolog/drl_purity_network.pl, prolog/tests/test_coexists_fpn_canary.pl, prolog/giant_component_analysis.pl, ISSUES.md, audits/2026-06-29_oq23_coexists_fpn_canary/
**Tier:** landed

Full arc (operator-guided, multi-round): a positive-controlled canary
(`prolog/tests/test_coexists_fpn_canary.pl`) FALSIFIED the premise it was built to backstop — the
`coexists_with` "zero contamination by definition" exclusion was NOT latent but ALREADY VIOLATED on
every populated leg (testsets/ 2, haiku 178, flash 361, kernel_v1 662) via the authored
`affects_constraint` side channel between sibling readings (the DP-001 ε-invariance "link ε-distinct
constraints via affects_constraint/2" instruction; `affects_constraint` is overloaded across
ε-linkage / UKE-dependency / generic). forecloses leaked the same way (relation-agnostic).
Per-consumer reachability witness: of 4 consumers reading the sibling edge, only **FPN
`effective_purity`** (ships to `pipeline_output.json contamination_network`) and **coupling baseline**
(ships to `coupling_protocol.md`) reach a product; composition `detect_extraction_dominance` (no
callers) and counterfactual `dependency_chain` (`simulate_cut` has no live caller) are inert.

**FIX (OQ-23 + OQ-24):** a same-kernel-donor guard as the first clause of
`compute_edge_contamination/7` — a same-kernel sibling contributes ZERO contamination.
Contamination-local by design (NOT `constraint_neighbors_existing/2`), so giant_comp topology is
unchanged. Witnessed: canary census `leaked` 2→0 (forecloses 1→0) on testsets/; `effective_purity`
returns to intrinsic for the leaking pairs; cross-leg post-fix census `leaked=0`; giant_comp
connectivity zero-change control (testsets baseline 66/12 unchanged); plunit regression gate
`no_coexists_or_forecloses_leak_on_loaded_corpus` GREEN.

**Why landed (tier):** the fix is committed and witnessed; the leak no longer ships. **Deferred:
OQ-193** — stripping same-kernel sibling edges from giant_comp connectivity collapses the giant
component 334→70 (kernel_v1); whether that is a correction (siblings aren't cross-kernel coupling, per
the OQ-84 precedent) or a loss (legitimate topology) is an unsettled Ω_C ruling, NOT resolved by this
fix (the contamination-local siting leaves giant_comp untouched precisely so OQ-193 can be ruled on
its own evidence). The coupling-baseline ship (also wrong by the module's own OQ-84 logic) is a
separate fix candidate noted in HOLD_FINDINGS.

**Tripwires (promotion test → file-local, not CLAUDE.md):** `compute_edge_contamination/7` and the
`drl_purity_network.pl` header comment both carry "do NOT extend a same-kernel guard into
`constraint_neighbors_existing/2` without resolving OQ-193 (changes 5 contamination-topology
consumers + a shipped headline metric)." An editor of that file sees it; the canary regression gate
self-flags a reopened leak.

## 2026-06-27 — OQ-124/OQ-149 committer-axis convention control: A=SIGNAL, B=CONVENTION, C=OPEN
**Files:** ISSUES.md, prolog/signature_detection.pl, python/story_repair.py, agent/run_no_scope_gemini.py
**Tier:** landed

Ran the OQ-70 bait-confound control on the three cross-model-divergent fields, per-field
pre-registered (`audits/2026-06-27_oq124_oq149_committer_convention_control/`). Twins re-classified
at one commit `bbf5c92` (the on-disk outputs were at 20fab78/8126231, straddling the OQ-138 ROUTE
conversion of `false_ci_rope`+`constructed_high_extraction` — non-comparable for Field A). Positive
controls held (claimed_type 0.7208, cs_kernel_id 1.000). Verdicts:
- **Field A (signature fork) = SIGNAL.** The CHE↔FCR fork is ~13:1 asymmetric (157 haiku-CHE/flash-FCR
  vs 12 reverse), and the dominant lean is a continuous extraction-magnitude difference (0/157 ride
  the `constraint_claim(rope)` template alone; all 157 have flash ε below the rope ceiling / haiku
  above the snare floor; cross-twin ext Spearman 0.86, flash systematically lower). Two-sided
  `with_retracted` control discharged. → signature lean carries a model index (v8 §3/OQ-72).
- **Field B (`cs_reading_relation`) = CONVENTION.** Flash leans more foreclosing (p=0.020) but the
  call fails to covary with the settled substrate on disagreeing slots (Spearman 0.156/0.162 < 0.20;
  agreeing-slot control 0.256/0.258). → needs a provenance bucket (precedent `becd0f87`).
- **Field C (`overridden` 51-vs-4) = OPEN-pending-instrumentation.** Per-slot coercion witness
  unrecoverable. *Enrichment (tripwire-adjacent):* `overridden` is **coercion-invariant** — a missing
  `cs_axiom_status` makes `generate_pl` KeyError → the story FAILS generation (generate_constraint_pl.py:672),
  it is NOT silently defaulted to `holdable`; and the `contested/foreclosed→holdable` remap
  (story_repair.py:89-90) is silent. So `overridden` counts are real authored values; only flash's
  `holdable` splits authored-vs-coerced, and that needs raw pre-repair capture (instrument
  `story_repair._normalize_axiom_status` to log `cid`). Third-model spend now warranted (A=signal),
  operator-gated.
**Files:** python/cohort_stability.py, python/cohort_sigma_seat_eval.py
**Tier:** tripwire

A `stable`/`match` verdict in a per-field comparison table can mean three structurally different
things, two of them hollow: (1) **content reproduced** — the real signal (`scalar`/`cat`/`nameset`
comparing values); (2) **presence-only matched** — the comparator sees only PRESENT vs EMPTY
(`prose_presence`/`list_presence`; apparatus `*.presence`), so the field reads "stable" whenever
the model emitted anything non-empty; (3) **the field is a constant** — zero between-item variance,
so it *cannot* be unstable (`emerges_naturally` True 18/18; `claimed_type`, `has_sunset_clause`;
`omegas.count` range 0.00). Aggregating across fields without splitting these silently inflates the
"stable" side and can **invert** a partition statistic, not merely soften it.

**Rule:** before trusting any aggregate over per-field comparisons, witness what each field's
comparator actually compares (read the extractor, not the column name), and run a between-item
(cross-story) variance check to flag degenerate constants. Worked instance — the OQ-118 re-probe:
removing presence-hollow fields from a σ/seat partition dropped consistency 47.9%→39.7% (an
inversion toward the unstable cast multisets), and the degeneracy sweep caught four constant
"stable" fields. Witness + re-runnable probe: `audits/2026-06-27_oq118_reprobe/` (commit
`fc57e833`); ruling landed `82c0693c`.

This is the per-comparator face of CLAUDE.md Build Discipline **Pattern 6** (measured-empty vs
didn't-look) and a sibling of **Pattern 5** (absence satisfies the gate) — the abstract tripwire
lives there; this is the worked instance, **not** promoted (the always-loaded form already exists;
over-promotion defeats the token-saving purpose). Cross-ref OQ-118.

---

## 2026-06-27 — OQ-182 family product SHIPPED: trajectory serialized + trajectory_enabled 0→1
**Files:** python/run_pipeline.py, prolog/config.pl, CLAUDE.md, AGENTS.md, ISSUES.md
**Tier:** landed

Flipped `config.pl:571 trajectory_enabled` 0→1, unblocking the OQ-182 family-product flip
that was held by a witnessed-NEGATIVE freshness criterion (a flag=1 run intermittently
stalled). Root cause: **concurrency memory pressure** — the `trajectory` stage (HAC
clustering, O(N²)) ran in the 4-worker Phase-2 thread pool **co-resident** with `giant_comp`
(also O(N²)); the two heavy swipl subprocesses overlapped. NOT a giant_comp bug (OQ-77:
serially fine at 87× the corpus).

**Fix (surgical, Python-only, no engine/classification change):** `run_pipeline.py`
`_phase_prolog` pulls `trajectory` out of the parallel `tasks` list and runs it
**sequentially after** `_run_parallel` returns — the `with ThreadPoolExecutor` joins
giant_comp's worker (and its synchronous swipl child) before returning, so the two heavy
stages never co-reside. Order is correctness-irrelevant: trajectory's only output
`context_profile_report.md` has no downstream consumer (C0 invariant). The 11 remaining real
stages stay parallel (the proven-fine pre-trajectory pool).

**Witnessed** (`audits/2026-06-27_oq182_trajectory_serialization/`): mechanism witness via a
~0.1s ps/RSS sampler over flag=1 pipelines — PRE-FIX arm captures co-residency (0.64s window
overlap, deterministic run-1 positive control); CURED arm shows disjoint windows (trajectory's
swipl starts 0.79s after giant_comp's exits). N=10 liveness battery 10/10 GREEN. Freshness
positive control PASS (non-vacuous). C0 re-witness zero classification diff (positive-controlled).
Measured trajectory alive-window 1.5s ⇒ 300s timeout held (≥175× margin, not bumped to 900).
`validate_config` PASS at flag=1; `trajectory_weights_sum` gate active+satisfied (sum=1.0).

**Promotion test → tripwire promoted to CLAUDE.md (Running the System):** a fresh agent could
silently re-fold `trajectory` into the parallel `tasks` list and reintroduce the intermittent
stall — the two O(N²) stages must never run concurrently. Tripwire lives in CLAUDE.md; full
provenance here.

---

## 2026-06-26 — OQ-91 resolved: commentary-grade repair-transition detector + report surface
**Files:** prolog/transition_paths.pl, prolog/json_report.pl, python/enhanced_report.py, docs/repair_dynamics.md, ISSUES.md
**Tier:** landed

Closed the observer-axis one-way ratchet (engine encoded decay, not repair). New
`repair_transition/4` in `transition_paths.pl` — the upward dual of the 8 decay
heads, **reusing** `degradation_chain/3` (the snapshot_type series) as source,
"upward" = transitive closure of the 8 `transition_path/4` decay edges read
backwards (`unknown` excluded). 4th arg = named op (`maintain`/`splice`/`replace`
rope line-ops; `scaffold_struck` construction op), a function of from/to + chain
prefix. **COMMENTARY-GRADE** — must never feed `classify_from_metrics/6`, the
signature layer, or `verdict_join`. Serialized as the `repair_transitions`
per-constraint field (`json_report.pl`, hermetic `preserve_classify_globals/1`
wrapper around the snapshot_type nb-globals), rendered by
`enhanced_report.py:build_repair_section` (single data direction; silent on
decay-only = honest absence).

Witnessed (`audits/2026-06-26_oq91_repair/`): real-corpus B1-scan non-empty
(testsets/ 2, kernel_v1 30, incl. multi-step homoousios/versailles
snare->tangled_rope->rope) => close-state 1, no new authored atom. B4 invariant
PASS (pipeline_output.json classification fields byte-identical with/without the
surface; only the new field added). Bug found+fixed: `repair_op` clause selection
must key on from/to/pre, not a bound 4th arg (else a bound-Op query mislabels via
the default clause). Suite 0 errors, snapshot-migration 10/10, warning gate 3/3.
Promotion test: wiring repair into classification would be LOUD (output changes,
caught by the diff) -> no silent-mistake tripwire -> no CLAUDE.md promotion.

## 2026-06-26 — OQ-182 C-gen: family product is generation-EXPRESSIVE (A4 flip still operator-gated)
**Files:** prolog/config.pl, prolog/context_profile_mining.pl, ISSUES.md, audits/2026-06-25_oq182_trajectory_revive/
**Tier:** correction-key

Part-A progress toward the OQ-182 trajectory gate flip. **A1/C0 PASS** (flag 0->1
changes only `config.trajectory_enabled` in pipeline_output.json; all
classification fields byte-identical; positive-controlled). **A3 C-prov PASS on
kernel_v1** (1106; classify_at_time globals unset post trajectory_run). **A2 C-gen
FAILED at its locked bar** (haiku<->flash family ARI=0.117 < 0.50) — a live
falsifier, treated as the finding. Operator ruled option-2 (re-specify, no
laundering): a freshly pre-registered, granularity-insensitive **substrate read**
(which constraints split, do splits track real fingerprint_shift differences) gives
**TRACK=162/162=1.000** — every inter-leg family split is backed by a real
per-reading shift difference, ZERO cut-height artifact. Dual finding (both stand):
global partition does NOT recover across generation (ARI fail) AND that failure is
**generation-EXPRESSIVE, not clustering noise** (locally stable PRES=0.83
descriptive-only; globally expressive). **A4 gate flip remains the operator's seat
— NOT flipped** (`config.pl trajectory_enabled` stays 0); kernel_v1 C-null deferred
(cost; cannot unblock a flip A2 already gates). Caveat for A4: one flag=1 run
stalled (likely giant_comp under added parallel pressure); a second completed in
12.6s.

## 2026-06-26 — OQ-104 resolved (scoped): gate.sh gains a 7th check (audit-citation frozen-evidence)
**Files:** scripts/gate.sh, python/audit_citation_status.py, audits/2026-06-18_oq104_citation_checker/controls.py, audits/2026-06-18_oq104_citation_checker/controls_run.sh, ISSUES.md
**Tier:** landed

Operator ruling: **gate the OQ-104 danger class.** The frozen-evidence danger (a unique evidence
file a fresh clone needs and lacks — the spectral_laplacian origin) is distinguishable from benign
descriptive refs by **regenerability**: an untracked cited path is dangerous **iff it is not under
top-level `outputs/`** (repo-root `outputs/` is rebuilt by every `run_pipeline`).

Changes:
- `audit_citation_status.py:classify()` — split the single `untracked-pending` sublabel by
  `c.startswith("outputs/")` into `untracked-frozen-evidence` (**GATING** — intrinsic ERROR, no
  flag) and `untracked-regenerable` (non-gating WARN). `--check` exits 1 iff frozen-evidence
  non-empty OR parse `problems`. `--promote-untracked` now lifts `untracked-regenerable`.
- `scripts/gate.sh` — new 7th `run` line `audit cites` (`audit_citation_status.py --check`).
- controls.py 23/23 → **25/25**: matched-pair (identical fixture content; frozen-arm in an audit
  dir vs regenerable-arm under `outputs/`) isolating the **prefix** as the deciding variable, plus
  a dotted `./outputs/` post-normalization control. controls_run.sh rot-fixture (non-`outputs/`)
  now flips pass→frozen-evidence (gating).

Witnessed: all **39** distinct untracked paths under `outputs/` → `untracked-frozen-evidence:0` →
gate GREEN; end-to-end RED-on-frozen-citation / GREEN-on-removal; full `./scripts/gate.sh` GREEN
(7/7). **Scope (do not over-read "resolved"):** one of two origin routes mechanized. Two residuals
stay non-gating with kill conditions (ISSUES.md OQ-104): a **typo'd** path lands in
`missing-pending-M` (gating `missing` would FP on all 70); a frozen artifact **parked under
top-level `outputs/`** reads `untracked-regenerable` (the prefix is a convention, not an invariant).
Promotion test: a new gate check fails LOUDLY (gate prints RED) → no silent mistake → no CLAUDE.md
promotion needed.

## 2026-06-26 — GAP-04/OQ-53 increment: cross-kernel reading-stance transpose (fingerprint_shift spine)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/cross_kernel_stance_report.py, prolog/tests/test_cs_kernel_registry.pl, docs/design/design_gaps.md, ISSUES.md
**Tier:** landed

Built the **reading-stance transpose** GAP-04 names absent and OQ-53's 2026-06-20 close RESERVED
(it shipped the draw-robust *observer-signature* orbit and explicitly deferred the semantic-stance
one as "model-relative only"). New in `cs_kernel_registry.pl`:
- `declared_stance/2` — THE SEAT (hand-declared cohort table; initial, for the exercised stances).
- `reading_stance/2` — authority = declared only (morphology is never a query-time fallback).
- `stance_cohort/2` — readings of a stance across kernels (transpose of `cs_readings_for_kernel/2`).
- `cross_kernel_stance_profile/2` — gathers each member's `(kernel, fingerprint_shift)`, derives a
  per-position majority **consensus** pattern (`$wild` = no majority), partitions convergent vs
  divergent, and reports the verdict WITH cohort provenance (morphology-suggested vs hand-declared).
- `cross_kernel_stance_report/0` + `cross_kernel_stance_export/1` (JSON).

`json_report.pl` now serializes `fingerprint_shift` per `per_constraint` entry (was absent: grep=0
pre-change; 104 entries post-run, pipeline exit 0 + mtime advanced) so the Python consumer reads
COMPUTED shifts, never recomputing `classify_at_power`. Consumer
`python/cross_kernel_stance_report.py` runs the transpose over both live twins →
`outputs/cross_kernel_stance.{json,md}`.

**Why the cohort is DECLARED, not derived (Seat-Theorem Cor 2b).** Morphology is unreliable both
ways, witnessed on the 7-member abolition cohort: an exact-stem rule (stem `abolitionist`) catches
only **4/7** (the stems fragment to {abolitionist, abolition, categorical_abolition,
abolitionist_rejection}); a substring rule over-admits `dharmasastra_corpus__abolitionist_rejection`
— a *rejection* of abolitionism. So a human curates `declared_stance/2`; the profile carries each
member's provenance and the verdict inherits it.

**Witnessed results (both twins).** abolition → convergent on `shift(*,snare,rope,snare)` **5/7 on
BOTH twins** (draw-stable / situation-fixed); the 2 outliers are genuine structural divergences (one
of them, `animal_status__abolitionist_reading`, is morphology-*suggested* yet structurally divergent
— morphology ≠ structure). deterrence flips convergent(haiku 4/1)↔divergent(flash 2/3) — draw-variant
/ seat-expressive. originalist is kernel-divergent (5/11). property has no shared signature. Read the
convergent/divergent split as a σ/seat partition, NOT a fixed label (determinism frontier).

Pinned by `prolog/tests/test_cs_kernel_registry.pl` (5 new corpus-free `transpose_*` tests on the
consensus spine — robust where the file's divergence cases are snapshot-fragile). NOTE: the
pre-existing `divergence_silent_at_observed_agreement_context` failure (19→24 of 25 pass) is the
documented archive-draw data-fragility, NOT caused by this change (the edit is purely additive; the
`cs_kernel_divergence/4` body is untouched). Provenance: ISSUES.md OQ-53 addendum, GAP-04 status.

## 2026-06-26 — OQ-21(b) CLOSED as a recorded design absence: the single-instance barrier is the module-collision, not DP-001
**Files:** ISSUES.md, prolog/corpus_loader.pl, prolog/config_validation.pl, prolog/json_report.pl
**Tier:** correction-key

Corrects two prior framings of OQ-21(b) ("does A12's multi-instance render branch fire on
real pipeline data?"). The committed "the gate is the MERGE MECHANISM, not the data" framing
(2026-06-26) was directionally right but unwitnessed and under-specified; a session-internal
hypothesis that **DP-001 is the single-instance barrier was falsified by running it**. Two
real `abolition_reading` draws (ε=0.88, ε=0.68) from
`archives/datasets/kernel_test/abolition_reading.pl` and
`archives/datasets/kernel_test/kernel_run_02/abolition_reading.pl`, co-loaded through
`corpus_loader` with `corpus_path` overlaid at a scratch dir:

**Witness 1 — the operative single-instance barrier is the per-story
`:- module(constraint_<name>,[])` collision (load survives, exit 0).** Both files carrying the
same module declaration → the second throws on consult and is silently dropped; only one ε loads:

```
[corpus] Loading 2 testset files...
[corpus] WARNING: Failed to load …/abolition_reading_b.pl: error(permission_error(redefine,module,constraint_abolition_reading),context(module/2,…))
[corpus] SKIPPED: …/abolition_reading_b.pl
[corpus] Loaded 1 testsets successfully.
[witness1] abolition_reading epsilon values loaded: [0.88]
[witness1] corpus_constraint ids: [abolition_reading]
=== EXIT CODE: 0 ===
(no config_violations.log written)        % DP-001 did NOT fire
```

**Witness 2 — DP-001 is the correct *complementary* observer-axis backstop (exit 1).** Renaming
only the second file's module (`constraint_abolition_reading_b`) so both files actually load
produces a fact-level chimera; DP-001 fires as designed:

```
[corpus] Loaded 2 testsets successfully.
ERROR: CS ERROR (OQ-25): reading abolition_reading has conflicting ε values [0.68,0.88] (must be single-valued per reading — DP-001); chimera load detected — see docs/cs_load_discipline.md
1 config violation(s) after corpus load. See config_violations.log. Halting.
=== EXIT CODE: 1 ===
```

**Close.** A12 (committer multi-UID render) and DP-001 (observer one-ε seal) are the two halves
of the intended two-axis model, not a tension. A12's render branch is **correct** (shipped test
`prolog/tests/test_a12_multi_instance_render.pl`), but its trigger — a shared-ε, committer-varied
replicate set (one name → N UIDs, one ε) — **has no demonstrated populator**: stochastic
generation gives each draw a *different* ε (OQ-26 / Axiom 2), i.e. exactly the conflicting-ε
chimera DP-001 rejects. So (b) is a **declared design absence**, not a pending witness.
**Reopen condition:** a generation mode that canonicalizes ε per reading (committer variation as
the only multi-instance axis) would produce the set A12 needs; if such a populator is named,
OQ-21(b) reopens and Option 2 (replicate multi-instance loader) becomes the build. No code change.

## 2026-06-25 — OQ-21(a) RESOLVED: A12 multi-instance selector — dead recency clause fixed, @< pinned
**Files:** prolog/json_report.pl, prolog/tests/test_a12_multi_instance_render.pl, ISSUES.md
**Tier:** landed

The positive control written to close OQ-21(a) found a real defect instead of confirming
correctness. In `write_per_constraint_entry/4` the multi-instance branch's documented "pick
latest instance by `cs_created_at`" path was DEAD: `aggregate_all(max(T-U), …, max(_-UID))`
evaluates `T-U` as **arithmetic**, and UIDs are atoms (UUIDs), so it throws
`type_error(evaluable, …)`, is swallowed by the surrounding `catch(_, fail)`, and *always*
falls through to the `msort/last` `@<` fallback. Selection has been by `@<` UID-order, never
by timestamp, for the branch's whole life. "Verified by manual dual-consult" read the comment's
intent, not the code's behavior.

**Reusable Prolog tripwire:** `aggregate_all(max(Key-Val), …, max(_-Witness))` — the common
argmax idiom — **evaluates `Key-Val` arithmetically** and throws on non-numeric (atom) keys. A
`catch/…fail` around it then silently degrades to whatever the fallback is. Witness both the
firing AND the fallback before trusting such a selector.

**Ruling (operator): `@<` is canonical; recency is the WRONG selector** — instances of one name
are parallel draws, not versions (determinism frontier), so there is no canonical-latest. Only
live correctness-bearing consumer of the selected fields is `orbit_operator.py`'s committer
terminal-projection orbit (via `cs_drift_terminal`); it needs determinism+stability, which
standard order of UID atoms supplies (never reads timestamps). Dead clause removed; `@<` is the
sole selector; in-code comment now carries the parallel-draws reason so the bug can't grow back.
Behavior-preserving on the live corpus (81 names / 81 `cs_story_uid` facts — branch never fires).
Test pins `@<` with bundle coherence + a recency-pin; positive control witnessed t1 RED under
reintroduced recency selection. (a) resolved; (b) pipeline-firing open, gated on a future
multi-instance load (OQ-17 pointer is stale — disposed). Commit `cfb5fa03`; `[GATE]` GREEN.

---

## 2026-06-25 — OQ-19 RESOLVED: drift-trajectory trigger thresholds made durable + fail-loud
**Files:** python/enhanced_report.py, python/tests/test_drift_trajectory_granularity.py, ISSUES.md
**Tier:** landed

Closed OQ-19 (temporal-shape trigger magic numbers). Single-file, behavior-preserving:
hoisted the 6 `build_drift_trajectory_section` thresholds (7 occurrences) into a named
`_DRIFT_*` constant block keyed to `_DRIFT_MEASUREMENT_GRANULARITY = 0.01`; Trigger A is
encoded *derived* (`4 * _DRIFT_MEASUREMENT_GRANULARITY == 0.04`, IEEE-754 byte-identical
to the literal — witnessed), B/C stay literals (empirically tuned, not granularity-
derived). Added `_series_granularity` guard that prepends `[CALIBRATION WARNING]` when a
rendered constraint's series are finer than the floor.

**Correction-to-the-OQ-premise (worth a cold read):** the original OQ-19 entry and the
plan both assumed "live data is 2-decimal today." FALSE as of this corpus — 4 constraints
(`longevity_mismatch`, `propagation_speed_asymmetry`, `protein_anabolic_resistance`,
`validation_judgment_separation`) carry **authored** (not projected) 3-decimal values.
None currently fire a trigger, so the guard is inert on rendered output (29 live sections,
0 warnings), but the feared finer-granularity regime is already partly present in authored
data — making the guard more valuable, not less. Witnesses (float kill-condition, grep
completeness 7→0, byte-identical per-trigger A/B/C diff vs HEAD, positive-control test) in
ISSUES.md OQ-19 resolution block.

**Promotion test:** history-only. A future agent editing the trajectory section now sees
named constants + an in-code guard + a granularity NOTE comment, so the silent-
miscalibration trap is structurally removed — no CLAUDE.md promotion needed.

---

## 2026-06-25 — OQ-182 C-null PASS: HAC structural families validated as MEANING-bearing (testsets/ leg)
**Files:** audits/2026-06-25_oq182_trajectory_revive/c_null_harness.pl, audits/2026-06-25_oq182_trajectory_revive/c_null_results.log, audits/2026-06-25_oq182_trajectory_revive/c_null_distribution.json, audits/2026-06-25_oq182_trajectory_revive/c_null_protocol_FROZEN.md, audits/2026-06-25_oq182_trajectory_revive/c2_domain_finding.md, ISSUES.md
**Tier:** landed

Spend-tier C-null leg of OQ-182 (plan `~/.claude/plans/bright-jumping-cocke.md`). Standalone control-first
Prolog harness in the audit dir; **no engine edits** (`config.pl trajectory_enabled` stays `0`; `git status`
shows only the audit dir + docs). Commentary-only invariant intact.

- **VERDICT: PASS — family product validated meaning-bearing.** RealSil = **0.161119** (97 clustered
  constraints, 11 families) > **P95(null) = −0.026436** over **200 per-component-independent shuffle draws**
  (0 degenerate). **0/200 null draws reach RealSil** — real lies beyond the *entire* null. TEETH PASS
  (null_median −0.0945 < RealSil; standardized gap **+5.01σ**). Null family-count centers at **15 vs real
  11** — the frozen doc's predicted false-FAIL-leaning direction, so the PASS is conservative. Reproducible
  under seed `20260625` (run-twice → identical P95; SWI 9.2.9; Python percentile cross-check matches every
  statistic).
- **Control-first, all pasted BEFORE the verdict and gating it:** INTERNAL-CHECK (Σ w_k·comp_k == engine
  `pair_dist`, max-diff 0.0), GROUPING-FIDELITY (`make_groups@identity` == engine `group_by_shift`, 26
  groups), FIDELITY (`P0 == RealPartition`, |S0−RealSil|=0), JOINT-TOOTHLESS (S_joint = RealSil to 1e-16,
  relabel-match=yes — the false-PASS the per-component design avoids, demonstrated), TIE-BREAK (overlay
  regime σ-pure). The per-component-vs-joint contrast is the teeth-witness.
- **MECHANISM CORRECTION (frozen quantities unchanged).** The frozen "Chimera surgery map" was
  mechanically wrong: `group_by_shift/2` keys the shift pre-grouping via
  `logical_fingerprint:fingerprint_shift/2` on the **constraint identity**, ignoring `trajectory_cached` —
  so a chimera `trajectory_cached` + `run_hierarchical_clustering/1` pins shift grouping to real boundaries
  regardless of σ_shift (toothless / false-PASS). The harness builds shift-groups itself (`make_groups/4`,
  keyed on `fingerprint_shift(C[σ_shift(i)])`) and reuses only `cluster_all_groups/2` + `assign_families/1`;
  the per-component shuffle is a pure index recombination over precomputed real component matrices. Erratum
  in `c_null_protocol_FROZEN.md`.
- **Scope:** families = safe + stable + **meaning-bearing**; **twins remain OPEN** (parallel report: 448
  twins / 4656 pairs, near-vacuous cross-domain gate; deferred to rebuild). Remaining OQ-182 legs: C0
  pipeline-diff corroboration, C-gen (haiku↔flash), kernel_v1 re-checkpoint, then the gate flip.

---

## 2026-06-25 — OQ-182 minted: revive + validate the dormant HAC trajectory-mining subsystem (cheap tier)
**Files:** prolog/context_profile_mining.pl, prolog/config.pl, prolog/isomorphism_engine.pl, prolog/constraint_bridge.pl, prolog/report_generator.pl, ISSUES.md, audits/2026-06-25_oq182_trajectory_revive/
**Tier:** landed

Cheap tier of the OQ-182 plan (`~/.claude/plans/fancy-splashing-pancake.md`). The plan was authored
naming "OQ-180"; that label was already taken by the OQ-51 build (commit `cef5dc6e`), so this work is
**OQ-182** (highest pre-existing was OQ-181). Audit dir: `audits/2026-06-25_oq182_trajectory_revive/`.

- **C-prov PASS (testsets/ leg, witnessed).** `prolog/context_profile_mining:trajectory_run/2` on
  `testsets/` (104 `corpus_constraint/1`; 97 yield trajectories → 11 families, 448 twins) leaves BOTH
  `classify_at_time_eps` and `classify_at_time_theater` globals **unset** — so the 2 passive
  `nb_getval` leaf reads (`drl_core.pl:306`, `boltzmann_compliance.pl:510`) fall back to authored
  `constraint_metric`, no imputed `BaseX=0.5`/`0.10` coupling. Excluded-constraint count = 0. Positive
  control (separate process): `classify_at_time/4` on an **on-grid** constraint+Time DOES set
  `classify_at_time_eps = eps(...,0.03)` — the probe is sensitive. **Note (OQ-178 trap, hit live):** the
  first positive-control draw at `Time=0` on an off-grid constraint returned `unknown` and bailed before
  the `nb_setval`; the control only proves sensitivity when fed a Time on the constraint's authored
  suppression grid. Witness: `c_prov_runtime.log`. **The same global-unset check re-runs on `kernel_v1`
  in the spend tier — C-prov gates both corpora, not just the narrow one.**
- **Cross-domain-twin fork verdict (Step 2, log-only, behavior-preserving).**
  `context_profile_mining:cross_domain_twins/3` is the **canonical** producer (live via
  `context_profile_report`, reachable). `isomorphism_engine.pl` is a **loaded-but-non-executing**
  Pattern-2 fork: loaded via `constraint_bridge.pl:11` + `report_generator.pl:31` (both in `stack.pl`),
  but all callers dead — `constraint_bridge:check_for_social_twins/2` (NOT in the export list at
  `constraint_bridge.pl:2–5`, never called), `report_generator:cross_domain_audit/0` (defined, never
  called), `isomorphism_report.pl` (NOT in `stack.pl`, unwired) → its `generate_cross_domain_index/1`
  has no live caller. Positive control: the grep DID find these sites, so a live caller would have
  surfaced. **NOT deleted** — deletion is a separate multi-file output-neutral cleanup (2 `use_module` +
  3 dead call sites) with its own diff-witness; mint as its own OQ if wanted. See `design_gaps.md` GAP-20.
- **Spend tier (C0/C1/C2/C3/C-null, gate flip) is gated behind an operator checkpoint** — not yet run.

## 2026-06-25 — OQ-51 main build: `unknown` is N/A on the canonical sheaf/H1 path (commits `f8ae0c9c` + `15cca7ed`)
**Files:** prolog/grothendieck_cohomology.pl, prolog/sheaf_analysis.pl, prolog/json_report.pl, prolog/product_site_export.pl, python/shared/schemas.py, python/shared/loader.py, python/w1_sheaf_join.py, python/enhanced_report.py, python/orbit_characterization.py, python/run_drift_mismatch.py, python/sweeps/epsilon_sensitivity.py, python/sweeps/range_sweep.py, python/sweeps/product_site_delta_sweep.py
**Tier:** tripwire

OQ-51's main build item landed (the cs_kernel_comparison site was f456896b; this is the
canonical path). `unknown` is N/A — not a disagreeing type, not a value that agrees with itself.

**Standing tripwires for a fresh agent:**
- **`h1_band` in `pipeline_output.json` is now NULLABLE.** null = UNDETERMINED (`<2` real seats —
  the obstruction is N/A, NOT 0). Any new reader MUST handle null, never `.get("h1_band", 0)` /
  `... or 0` — that silently reads undetermined/manifest as genuine. Use
  `shared.loader.h1_band_or_raise(entry, source)` (fails loud, distinguishing key-absent=stale
  artifact from null=undetermined). Same for the **product-site `"h1"`/`"h0"`** in
  `product_site_orbits.json` (separate file; `None > 0` CRASHES — null-guard it).
- **`sheaf_status` gains a 4th value `undetermined`** via TWO routes with distinct provenance in the
  sibling field `sheaf_undetermined_reason` (`insufficient_seats` | `uncomputable_height`). Route 2
  is **h1==0 AND undetermined** (height uncomputable) — so `undetermined ⟺ h1==null` is NOT an iff.
  The true partition (asserted by `w1_sheaf_join`): manifest⟺h1>0; genuine/fragile⟹h1==0;
  undetermined⟹h1∈{null,0}.
- **`arakelov_height/2` needs MaxEnt that a bare `[stack]` load does NOT populate** (computes for
  0/104 in a bare context; `arakelov_height.pl:16-18` "pipeline diagnostic only"). So a bare-context
  probe of route 2 (`uncomputable_height`) is an ARTIFACT — every h1=0 reads route-2. Route-2
  liveness is **pipeline-authoritative** (`pipeline_output.json`); in the live pipeline it is dormant
  (route 1=15, route 2=0). Any future route-2 census needs the arakelov-computable positive control
  (see `tests/test_sheaf_na.pl`). **A pipeline reorder that serializes `sheaf_status` before
  `maxent_run` would silently turn EVERY h1=0 constraint undetermined** — guarded by `w1_sheaf_join`
  Control 2b (dies if no h1=0 constraint is genuine/fragile); two-sided witnessed. Full mechanism +
  the ordering hazard: `docs/technical/sheaf_status_maxent_ordering.md`.

Witness: test_sheaf_na 10/10 + live route-1=15; dynamic suite 0 errors; pipeline exit 0 + mtime
advanced; schema gate green; diff 26 h1_band / 22 sheaf_status moves, 15 undetermined; w1
partition_ok; containment trips loud; 0 partition violations on testsets_haiku(960)/flash(960)/
kernel_v1(1106). Branch `oq51-sheaf-na-canonical`. Residual: OQ-180 (sibling `\=` + 3 audit-dir
silent sites), OQ-181 (per-site undetermined semantics for the 13 readers + `load_per_constraint`).

---

## 2026-06-25 — fix: OQ-57-class wrong-qualifier in the dormant trajectory-mining path (commit `fc9b4688`)
**Files:** prolog/context_profile_mining.pl, prolog/check_stack.pl
**Tier:** landed

Surfaced during the OQ-16 rename (rename-independent — byte-identical pre/post). `standard_contexts/1`
called `dirac_classification:standard_context/1`, removed 2026-06-02 (dirac kept `gauge_orbit/2`,
`preserved_under_context_shift/2` — both still valid — but deleted its local `standard_context/1`;
see `dirac_classification.pl:115`). Re-qualified to `drl_core:standard_context/1`, which retains the
identical 4-context generator (verified to enumerate the same 4 canonical contexts as
`constraint_indexing:site_contexts/1`). It was the **only** rotted call in the file.

**Witness:** the report generator (run_pipeline's exact load chain + `run_trajectory_report`) now
exits 0 with no existence/unknown-procedure errors, all 4 contexts processed, **135-line report
produced** (was crash → empty). Production path unchanged (`trajectory_enabled=0`); fix only bites
when enabled. **Why it sat unnoticed:** `context_profile_mining.pl` is NOT loaded by `[stack]` (only
in the separate trajectory chain), so `check_stack.pl`'s undefined-predicate scan never saw it.
**Gap closed (commit `a82d7ed0`):** `check_stack.pl` now loads the trajectory chain faithfully
(mirrors run_pipeline `_prolog_trajectory`) before `check/0`, so wrong-qualifier rot in
`context_profile_mining.pl`/`context_profile_report.pl` is now caught — positive-controlled
(reintroducing the bug makes check flag it), baseline unchanged (same 5 known undefineds). Honest
boundary recorded in-file: the OTHER standalone report scripts (abductive/orbit/fingerprint/…
report) remain uncovered — co-loading non-module scripts into one image cross-contaminates;
a faithful per-chain check needs a fresh process per chain (larger item, not done).

**Forward (not done here):** the crash is gone, but *validating* the now-runnable trajectory-mining
(HAC structural-family) output / deciding whether to revive the subsystem is the revive-or-gap design
call — OQ-91-adjacent (OQ-91 itself is the sibling `transition_paths`/repair-transition thread, a
distinct dormant module). No new defect OQ minted; the fix removed the sharp edge per the
fix-simple-errors ruling.

---

## 2026-06-25 — OQ-16 RESOLVED: temporal vocabulary rename pass (name-only, 5 renames, 3 commits)
**Files:** prolog/metric_drift_events.pl, prolog/metric_drift_report.pl, prolog/context_profile_mining.pl, prolog/context_profile_report.pl, prolog/network_dynamics.pl, prolog/stack.pl, prolog/drl_lifecycle.pl, prolog/transition_paths.pl, prolog/cs_pattern_detection.pl, prolog/cache_registry.pl, python/run_pipeline.py, scripts/pipeline_dashboard.sh, ISSUES.md
**Tier:** landed

Executed the deferred "drift"/"trajectory" rename pass — the words each named two
different concepts on opposite axes (metric/network drift ≠ CS commitment-drift;
observer-context "trajectory" ≠ CS commitment-trajectory). Name-only, no logic/threshold
moved. Five renames in three commits:

- `0a204af1` — predicate `detect_network_drift/3 → detect_network_contamination/3`
  (network_dynamics.pl + all qualified callers + the `drl_lifecycle` facade call).
- `1d861cee` — file+module renames `drift_events→metric_drift_events`,
  `drift_report→metric_drift_report`, `trajectory_mining→context_profile_mining`,
  `trajectory_report→context_profile_report` (file only, no module decl); imports/reexports/
  load order; `run_pipeline.py` + dashboard + `.legacy` output paths
  `trajectory_report.md → context_profile_report.md`.
- `1bcc07c5` — genuine code-pointer tokens across 15 live reference/implementation/design docs.

**Operator rulings:** `metric_*` over `dr_*` (no `dr_` scheme exists today; `cs_` is a concept
marker, not a file-prefix convention) — so `dr_` would be a lone scheme splitting the cluster.
One complete pass (sources + generated `.md` + genuine doc refs) so no half-renamed mismatch is
manufactured. **Left out of scope (logged, not missed):** JSON output field `drift_events`
(`json_report.pl`, python schemas), internal predicate `run_trajectory_report`, doc *filenames*,
and dated recon/essay docs (`recon_2_scope*.md`, `when_frame_isnt_foreground.md`) where the old
name is the subject of a historical narrative.

**Witness:** `[stack]` loads ok; `detect_network_contamination/3` present, `detect_network_drift/3`
absent; `[abductive_triggers]` loads through the reexport facade; `check_stack.pl` clean (positive
control for a missed qualifier); full `run_pipeline.py` exit 0 writing `context_profile_report.md`;
dashboard reads the renamed path. **Promotion test:** no tripwire — a missed reference fails
*loudly* at load (existence_error) or is caught by `check_stack.pl`, so this stays history, not a
promoted warning. **Side-finding (rename-independent — surfaced here, FIXED separately):**
`context_profile_mining.pl` called `dirac_classification:standard_context/1`, which
`dirac_classification.pl` deliberately removed (comment :115) — a pre-existing dangling call in the
production-disabled (`trajectory_enabled=0`) trajectory path, byte-identical pre/post rename, so not
an OQ-16 regression. Resolved by a concurrent instance (`fc9b4688`): re-qualified to
`drl_core:standard_context/1`, and `check_stack.pl` extended to load the trajectory chain so the
class is caught going forward (`a82d7ed0`). Authoritative entry: the standard_context fix entry above
(KNOWN_STATE 2026-06-25) + `swipl_load_path_and_probe_gotchas.md` §1 (loaded-image coverage boundary).

**Doc-scope refinement (commit `76eae0c1`, operator ruling 2026-06-25):** the 4 dated
recon/essay docs (`recon_2_scope.md`, `recon_2_scope_v2.md`, `when_frame_isnt_foreground.md`,
`commitment_systems/construction_over_inspection.md`) are NOT untouched — their **bodies are
preserved** as dated records, but each got a **per-doc end-note** pointing at the OQ-16 rename
table (only the renames appearing in that doc). Confirmed narrative-only first (no live
`see prolog/X` pointer) before preserving. **Final-grep exclusion (record so a future run reads
remaining hits as intentional-preserved, not a missed rename):** old tokens still legitimately
appear in (a) the JSON output field `drift_events` — `json_report.pl`, ~10 python files,
`report_sidecar_schema.json`, `diagnostic_integration_architecture.md:42`; (b) the 4 historical
docs above (body + end-note); (c) verbatim external review transcripts `docs/review/expansions.txt`
+ `docs/review/jaynesian-gemini.txt` (quoted `detect_network_drift/3`). A correct
"no dangling refs" grep excludes these basenames; every other old-token site is gone. **Note:**
this pass interleaved on `main` with a concurrent instance that committed the live docs (`1bcc07c5`)
and the close-out (`fb45c0e3`) — outcomes converged (it referenced these same hashes), but 6 claude
instances were running; multi-writer hazard per CLAUDE.md.

---

## 2026-06-25 — OQ-39 RESOLVED: scaffold rising-suppression gets a COMMENTARY verdict (rows 14–18 disposed)
**Files:** prolog/cs_pattern_detection.pl, prolog/tests/test_oq39_scaffold_escalation.pl, ISSUES.md
**Tier:** tripwire

OQ-39 row 14 (scaffold "suppression must decline over time", no engine enforcer) resolved **by
commentary, not gate-vs-drop** (operator ruling). Reclassifying a rising-suppression scaffold to
rope/tangled_rope would assert *coercion* the evidence doesn't show — it only shows the decline rule
is violated. New clause `cs_verdict(C, scaffold_suppression_escalating)` (commentary-grade,
annotate-only; flows to the `cs_verdicts` output field, touches no classification/override path)
fires when a constraint certifies `scaffold` at any standard context AND its authored
`suppression_requirement` *series* is rising (`drift_events:metric_trend`). **14 live constraints
fire** (witnessed; cross-checked against an independent inline probe — same 14).

**Cross-leg finding:** rising:falling ≈ 5–6:1 in every leg (testsets/ 13:2, haiku 53:7, flash 43:9
@ institutional). The two reconciled legs share one generation prompt → this rules out one model's
idiosyncrasy (NOT prompt-independence). Since the rule *is* a generation-prompt rule, the sharp
reading: the prompt's own "suppression declines" instruction is systematically not honored by
generation — which strengthens the commentary case. (A strict "require decline" gate would deny
18/20 institutional scaffolds; "deny on rising" 13–14/20 — both large reclassifications the ruling
rejects.) `metric_trend/3` reads the `measurement/5` series directly (earliest→latest delta); its
consumers do not route through `classify_at_time`, so the check is time-independent and **moot to
OQ-178's off-grid Time=0 wrinkle**.

Rows 15–18 closed: 15 (final-measurement==base_extractiveness) no validator, low-stakes,
positive-controlled absence; 16 (piton atrophy) enforcer exists (`coordination_dead/1` wired into
`classify_from_metrics/6`); 17 (Goodhart) leave diagnostic-only (`detect_metric_substitution/1`
report-path only); 18 (perspective-min) lives correctly at the linter eval surface, not an engine
enforcer.

**Tripwire — `cs_verdict/2` clause placement/cut gotcha.** Every existing `cs_verdict` clause ends
in `!`, harmless among themselves because each is gated on a DISTINCT single-valued `cs_pattern`
(mutually exclusive). A NEW clause gated on something ORTHOGONAL to `cs_pattern` (here
`dr_type=scaffold`) is NOT mutually exclusive: placed BELOW the family, an earlier clause's `!`
silently prunes it on a constraint that matches both; given a trailing `!`, it prunes the others.
**Rule: a new orthogonally-gated `cs_verdict` clause MUST be the FIRST clause and commit with
`once/1` (local cut over inner goals only — NO trailing `!`)**, leaving sibling clauses reachable so
`findall` gathers this verdict PLUS any `cs_pattern` verdict. Proven by the cut-regression control in
`tests/test_oq39_scaffold_escalation.pl` (a dual-verdict constraint carries BOTH). Mode note: the
clause needs C BOUND (it calls `dr_type(C,...)`); the production consumer (`json_report.pl:562`)
always binds C, but a `findall(C, cs_verdict(C, scaffold_suppression_escalating), _)` with C unbound
returns 0 — query by iterating `corpus_constraint/1`.

## 2026-06-25 — OQ-178/179 SUPERSEDED/RESOLVED: cs_kernel_divergence reverts to static `dr_type/3` (time-neutral)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, ISSUES.md
**Tier:** tripwire

`cs_kernel_divergence/4` and the `compare_kernel_readings/3` JOIN now classify with static
`dr_type/3` (time-neutral), mirroring `perspectival_incoherence` — reverting the interim OQ-178
latest-snapshot probe-fix (`9fde36c9`). Commit `5b069ae1`.

**Tripwire:** a `cs_*` cross-reading comparator uses static `dr_type/3` — its moving axis is
reading/perspective, NOT time. Do NOT wire it to the DR `measurement/5` series / `classify_at_time`:
that crosses the DR temporal element into a CS-layer predicate. *Latest*-snapshot specifically reads
a COLLAPSING constraint at its terminus (latest authored ε can be 0 → `unknown`; `unknown==unknown`
reads as agreement, masking real divergence). The CS lifecycle trajectory
(`cs_reference_frame`→`cs_drift_state`→`cs_drift_trajectory`) is a SEPARATE temporal element.
*(Promotion test: NOT promoted to CLAUDE.md — the `cs_kernel_registry.pl` header (lines ~14-23) now
carries this warning in-code, so a fresh editor sees it before touching the predicate; the
silent-mistake risk is covered at the edit site.)*

**Witness (probe == regenerated `json_report`):** live corpus n=97, `cs_kernel_divergence_count`
16→18, kernels 8→8. The +2 recovered pairs are both `visual_evidentiary_authority`
(`post_evidentiary` × `indexical_realism` / × `distributed_verification`), genuine type≠type
(`snare ≠ tangled_rope`/`naturalized`), zero unknown-pairings (OQ-37 artifact did NOT occur). Twin
corpora corroborate the direction: `testsets_haiku` 861→893 (+32, +3 kernels), `testsets_flash`
813→846 (+33, +4). `shinbutsu` (the interim audit's collapse exemplar) is now a SINGLETON live
reading → no live pair; the reversal stands on the principle. OQ-179 closed mis-premised; its genuine
DR-axis observation (sibling readings change DR-type across their own grids) re-homed to the DR
temporal subsystem (`drift_trajectory`/`temporal_residual`, OQ-110 family). OQ-105 BC-encoding fold
moot for this path (static `dr_type` never takes `max(T)`).

---

## 2026-06-25 — OQ-51 build-extension RESOLVED: `unknown` is N/A in cs_kernel_comparison (trichotomy + divergence enumeration)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/enhanced_report.py, prolog/tests/test_cs_kernel_registry.pl
**Tier:** landed

Applied OQ-51's N/A rule (`unknown` = not-agree, not-diverge) to the `cs_kernel_comparison`
surface — the site the original OQ-51 build never enumerated, surfaced by the OQ-178 audit
(all-`unknown` context was scored `agree(unknown)`, inflating robustness).

- **Verdict trichotomy** (`ctx_reading_verdict/2`): `agree(Type,NUnk)` / `diverge(TypeMap,NUnk)` /
  `undetermined(NReal,NUnk)`. Each carries `NUnk` = #unknown readings so abstention reads uniformly
  off the verdict (`verdict_unknown_count/2`). LENIENT (operator ruling): ≥2 real readings ⇒ verdict
  over the real ones; a lone unknown does NOT demote (strict = absence-as-presence reversed).
- **`cs_kernel_divergence/4`** and **`pair_reading_agreement/7`** now require BOTH types real before
  counting agree/diverge (shared `is_real_type/1`). Load-bearing for the join invariant
  (Σ DivergeN == #cs_kernel_divergence) — must not refactor back to bare `\=`. Jaccard = `null` when
  the pair has no comparable (both-real) context (was a misleading 1.0).
- **JSON**: `specific_context_count` → `divergent_context_count` (recomputed as `#diverge`, NOT
  relabelled NCtx−robust); new `undetermined_context_count`, `abstaining_context_count`
  (cross-cutting — a context can be agree AND abstaining; NOT a 4th partition cell), and
  `divergence_patterns` (deliverable ii: ENUMERATES the disagreement, keyed on the real-typed submap
  — abstention carried as sub-annotation, never in the key — capped at top 5 with a
  `divergence_patterns_truncated` notice). Partition: `robust + divergent + undetermined == total`.
- **Report** (`build_kernel_reading_section`) now renders the distribution + the divergence
  enumeration (`diverges: settler=snare / cultural=scaffold (117 contexts)`) instead of bare counts.

**Two silent footguns fixed:** (HOLE A) `write_jaccard_pair`'s `~6f` threw on a null Jaccard and
aborted the whole JSON write — now branches to literal `null`; (HOLE B) `enhanced_report`'s `:.3f`
threw on `None` — now renders `n/a`. (Arity fold A) `json_report.pl:2024`'s `agree(_)` would
SILENTLY fail-match the new `agree/2` token → RobustN=0; updated to arity-2.

**Witnesses (this commit):** unit suite 20/20 (incl. 6 synthetic N/A controls + join invariant);
dynamic suite 0 errors; pipeline exit 0, `pipeline_output.json` rewritten; partition invariant
9/9 kernels; `cs_kernel_divergence_count` 20→16, `cs_kernels_with_divergence` 9→8 (actinide's only
"divergence" was unknown-vs-real — now correctly 0 real divergences, 117 undetermined where the old
report falsely read 117 reading-specific); JSP report enumerates settler=snare/cultural=scaffold.
**Note:** robust_context_count can RISE on abstention-heavy real-agreement kernels (performance_legitimacy
21→147) — the lenient rule reclassifying real-agree-with-abstention from specific→robust; this is the
RULING applied (Blast-radius prose under-predicted the direction; the output is the authority).
**Note (dormant in serialized output — do not misread a no-op diff):** the all-unknown→`agree(unknown)`
robust-INFLATION case (the OQ-178 motivator) fires on **0 of the 9 serialized kernels** — each has 0
all-unknown contexts, so no serialized `robust_context_count` drops via this path; a `pipeline_output.json`
diff shows the inflation correction NOWHERE. It is witnessed by the synthetic control
`na_rule_all_unknown_is_undetermined` and in-predicate on 13 NON-serialized singleton kernels
(`doomsday_clock_metric` 120 all-unknown ctxs, `maat_order_principle` 126, `gita_kurukshetra` 76 …),
which the `L>=2` filter (`json_report.pl:1734`) excludes from output. The OQ-178 witnessed `robust 0→156`
required JSP's two readings to fail-close to `unknown`; live they are real-typed (snare/scaffold), so the
live serialized robust effect is the abstention-tolerant RISE (performance_legitimacy 21→147), not a drop.
Join invariant Σ DivergeN == #cs_kernel_divergence holds **9/9 live** serialized kernels (the plan's 42/42
was the `testsets_haiku` twin corpus, not this leg). **Note:** no live kernel currently has a zero-comparable
pair, so the null-Jaccard path is witnessed by the synthetic unit test + direct writer/guard probes, not live
data. Scope: only `cs_kernel_comparison`; the original OQ-51 `count_disagreeing_pairs`/`sheaf_status`/H1 sites remain
OQ-51's separate open item. Console drift: `cs_corpus_analysis.pl:110` divergence count drops
(expected). OQ-119 probes/exports see fewer divergences (expected).

## 2026-06-24 — OQ-37..41 census Pass 1: 2 strips landed; OQ-41 BaseX=0.5 is off-grid, not absence; OQ-178 minted
**Files:** prolog/data_validation.pl, prolog/drl_composition.pl, prolog/cs_kernel_registry.pl, ISSUES.md, CLAUDE.md, audits/2026-06-24_oq41_basex_t0/
**Tier:** correction-key

Implementing the OQ-37..41 census plan. **Landed (behavior-preserving, commit `1eacd2fc`):**
stripped the vacuous `resistance_to_change`-keyed piton sub-check in `validate_edge_cases/0`
(superseded by OQ-90) and `predict_transformation/3` (0 callers; helpers `linear_slope`/
`slope_accum` now orphaned → OQ-38 clause-pass candidates). Item-3 `inevitability` read was
already stripped in a prior session.

**Correction-key — the temporal path is LIVE, not dormant (overturns prior OQ-41 text):**
`classify_at_time` is consumed live by `cs_kernel_registry` (probes at **Time=0**, feeding
pipeline `validation.cs_kernel_*`), `temporal_residual`, `boltzmann_compliance`, `drl_core` —
NOT only via the dormant `constraint_history`/`snapshot_type`/`degradation_chain`. So the OQ-41
rows 24–25 `BaseX=0.5` site is live at Time=0. Fail-closing it (the OQ-44 reflex, attempted then
REVERTED) is output-changing (`cs_kernel_divergence_count` 17→16) and WRONG: all 15 affected
constraints author `base_extractiveness` as a temporal series at real years (none at the synthetic
Time=0) — 0/15 genuinely absent. The default is OFF-GRID PROBING, not absence; fail-closing erases
a real `snare`-vs-`scaffold` divergence (`jewish_sovereignty_palestine`). **Fix RESOLVED via
OQ-178 (2026-06-25, commit `9fde36c9`):** `cs_kernel_registry` now reads each reading at its LATEST
authored time (`reading_snapshot_time/2`), not the synthetic Time=0 — falsifier resolved (output
not time-aligned → per-reading-own-time). Witness: divergence count 17→20, JSP preserved, 0/15 still
off-grid, invariant 42/42, 32 readings re-based from authored ε. Single-snapshot is lossy (9/15
readings change type across grids) → trajectory successor **OQ-179**. OQ-39 row 14 reopened (same
premise); OQ-51 build-extension logged (cs_kernel_comparison counts unknown==unknown as agreement).
Audit: `audits/2026-06-24_oq41_basex_t0/`.

**Tripwire (reusable):** when witnessing behavior-preservation via a `pipeline_output.json` diff,
a `run_pipeline.py` whose **load-warning-gate aborts** (e.g. a `*/` inside a `/* */` Prolog comment)
exits non-zero and does NOT rewrite the output — so the diff reads FALSE-IDENTICAL against the
stale file. Always check exit code AND the output mtime changed before trusting a "byte-identical"
pipeline diff (Pattern 6).

---

## 2026-06-23 — OQ-15 RESOLVED (core): cross-axis taint guard LANDED, Phase 2 ruled policed-in-place
**Files:** prolog/check_axis_boundary.pl, prolog/axis_boundary_allowlist.txt, python/check_axis_boundary.py, python/run_pipeline.py, prolog/tests/axis_boundary_ctl_run1.pl, prolog/tests/axis_boundary_ctl_run2.pl, prolog/tests/axis_boundary_ctl_payload_widen.pl, prolog/tests/axis_boundary_ctl_nonbridge_seam.pl, scripts/gate.sh, ISSUES.md, docs/design/design_gaps.md
**Tier:** landed

Resolved the load-bearing half of OQ-15 (= v8 §8 item 1 / OQ-135 priority-1 artifact;
closes GAP-12). Commits `c6fe7edb` (Phase 0a/0b), `fd1ee561` (guard).

- **Phase 0a witnesses** (`audits/2026-06-23_oq15_crossaxis_witnesses/`, read-only):
  W1 MIXED (cs_drift_mismatch reaches observer machinery *transitively* via
  cs_is_metric_stable → grep is blind → guard load-bearing); W2 the `influences`
  bridge is the *unique committer→observer* dataflow (bucket-1 comparisons run the
  other direction); BC no runtime back-channel — re-witnessed engine-wide 2026-06-24
  (`bc_rewitness.txt`): non-vacuous probe (flags a planted cs_ assert) + complete
  assert-target enumeration → zero cs_ committer facts written at runtime. STATIC
  witness only ("found none," not a runtime snapshot-diff), and a SEPARATE surface
  from the guard (guard = static reads; writes = this enumeration). Corrected from the
  original inspection-only read (which swept only cs_*/drl_*). XR/SA confirmed. **constraint_bridge.pl `compute_veto_actors` is NOT cross-axis**
  (reads dr_type + authored `constraint_beneficiary` substrate, no cs_) — the plan's
  "reverse DR→CS read" hypothesis was *false*; NOT added to Files, NOT whitelisted.
- **The guard** (`check_axis_boundary.pl`): reachability over the LOADED call graph
  (clause/2, descends control constructs + meta-calls + **nested module qualifiers** —
  a missing-recursion blindness the positive controls caught before landing). Python
  harness diffs edges vs `axis_boundary_allowlist.txt` (load_warning_gate pattern,
  fail-closed); `--selftest` runs negative + 2 required controls (path-b payload widen,
  path-c non-influences seam — both fire). Wired into `scripts/gate.sh` (static check,
  no corpus). GATE GREEN; behavior-preserving (no engine file touched; guard absent
  from stack.pl/run_pipeline/corpus_loader load path).
- **Census beat the hand inventory:** 8 boundary edges; only 2 are observer-VERDICT
  reads (sanctioned `influences` bridge + bucket-3 `cs_kernel_id` exclusion → "exactly
  one forward bridge" confirmed in place). The other 6 are comparison/validation tooling
  (`axiom_diff`, `reading_diff`, `config_validation`) — modules OQ-15's `Files:` OMITTED.
- **W2 corrected (kind vs cardinality):** the relation-atom type system axis-segregates —
  `influences` (entailment, 38) read ONLY at the observer derivation; `forecloses` (47) /
  `coexists_with` (104) committer-modal, never cross. So single-bridge is principled-IN-KIND
  but "exactly one" is convention-not-theorem, guard-enforced-in-CARDINALITY. The earlier
  "principled" gloss asserted the conclusion W2 was scoped to test — dropped. Guard is
  corpus-INDEPENDENT (live/haiku/flash all → same 8 edges, byte-identical sets).
- **Phase 2 RULED policed-in-place (v8); core CLOSED (2026-06-24).** Operator's named reading:
  a green gate is sufficient; the boundary need not be source-legible today. The guard IS the
  resolution. **Synthesis (v7 named mediator) PRESERVED, not foreclosed** — v7 unbuilt-but-
  available; trigger = **a SECOND committer→observer bridge is proposed** (falsifiable,
  witness-tied; NOT "first legibility failure"), mechanically wired (such a bridge fires the
  guard RED → allowlist header → OQ-15 synthesis decision). The guard is now SOLE enforcement
  of a convention, so its two positive controls run in BOTH recurring gates (shown-firing):
  `scripts/gate.sh --selftest` AND `run_pipeline.py` (axis-boundary gate beside load-warning).
  Vocabulary migration remains human-gated under OQ-135. Bundled OQ-15 ↔ OQ-135.

---

## 2026-06-23 — OQ-06 RESOLVED: off-case fixtures witnessed for cs_drift_unacknowledged / cs_axiom_foreclosed
**Files:** prolog/cs_pattern_detection.pl, prolog/cs_axiom_engine.pl, prolog/narrative_ontology.pl, ISSUES.md
**Tier:** correction-key

All four off-case conjuncts now witnessed in BOTH directions (fires-when-it-should AND
stays-silent-when-it-should). Method: search all four real corpuses with a two-sided
planted control per off-bucket + per-corpus overlay fingerprint (Phase A), then a transient
matched-pair matrix (Phase C). Evidence: `audits/2026-06-23_oq06_offcase_fixtures/`.

Findings worth carrying:
- **Stale `Files:` corrected.** ISSUES.md OQ-06 pointed at `cs_drift_engine.pl` for the
  predicates; that file only *mentions* `cs_drift_unacknowledged/2` in a comment (lines
  34–35). The real definitions are `cs_pattern_detection.pl:412–416`
  (`cs_drift_unacknowledged/2`) and `cs_axiom_engine.pl:137–141` (`cs_axiom_foreclosed/2`).
- **`cs_axiom/3` is multifile-but-STATIC** in `narrative_ontology.pl` (NOT in the `:- dynamic`
  block, unlike `cs_drift_state/3` and `cs_axiom_grounding/3` which ARE dynamic). So
  `probe_harness:with_asserted` on `cs_axiom/3` throws `No permission to modify static
  procedure` — declare `dynamic(narrative_ontology:cs_axiom/3)` in the probe process first
  (does not change how readers see it; the process halts, no leak).
- **drift-C3 (Dir=stable + non-minor + unacknowledged) is a structural absence**, not a
  coverage gap: across all four corpuses, unacknowledged stable drifts are always minor and
  non-minor stable drifts are always acknowledged. The transient probe is its permanent
  witness — no synthetic fixture belongs in `testsets/` (THREE-LIVE-LEGS sparsity is intended).
- **Sequential multi-corpus scans must be one-corpus-per-process.** `load_all_testsets/0` is
  `corpus_loaded`-guarded (no-op after first load) and `consult` accumulates
  `narrative_ontology` facts — a one-process 4-corpus loop loads only corpus #1 and pollutes
  counts. (Already in CLAUDE.md Corpus Loading for the count-mismatch case; reinforced here.)

Promotion test: the stale-`Files:` correction is local to OQ-06 (now fixed in place, won't
re-mislead). The `cs_axiom/3` static-procedure gotcha is the candidate tripwire — but it is
narrow (only bites a probe that asserts `cs_axiom/3`) and fails LOUDLY (immediate permission
error, not silent), so it stays history here rather than promoting to an always-loaded section.

## 2026-06-23 — OQ-10 RESOLVED: reading-robustness as first-class report output (+ OQ-176 spawned)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/enhanced_report.py, prolog/tests/test_cs_kernel_registry.pl, ISSUES.md
**Tier:** landed

Added the summary/verdict layer OQ-10 needed. The comparison ENGINE already fired live
(`cs_kernel_divergence/4` + `write_kernel_comparison_entry` + `build_kernel_reading_section`);
the "no predicate/script/report section performs this comparison" premise was stale. New:
- `compare_kernel_readings/3` (cs_kernel_registry.pl, exported): per-context verdict profile over
  the SAME `classify_at_time/4` evaluations the divergence engine walks — a JOIN, not new compute
  (it makes FEWER classify_at_time calls than cs_kernel_divergence, which re-evals per pair).
  Invariant: Σ per-pair DivergeN == #cs_kernel_divergence solutions (166==166 on the live twin;
  unit test `compare_join_consistency_with_divergence_engine`, corpus-independent). **SUPERSEDED
  2026-06-25 by the OQ-51 trichotomy** — verdict tokens are now `agree(Type,NUnk)` /
  `diverge(TypeMap,NUnk)` / `undetermined(NReal,NUnk)`; see the 2026-06-25 entry.
- `pipeline_output.json` `validation.cs_kernel_comparison[].reading_robustness` object fields:
  `total_contexts`, `robust_context_count`, `divergent_context_count` (**renamed from
  `specific_context_count` 2026-06-25**), `undetermined_context_count`/`abstaining_context_count`/
  `divergence_patterns`/`divergence_patterns_truncated` (NEW 2026-06-25, OQ-51), `h1_band_robust`
  (true/false/null — null = fail-closed on missing H¹), `per_reading_h1[]`, `pairwise_jaccard[]`.
  Jaccard is CONTEXT-ALIGNED over presheaf section graphs (global-vocabulary Jaccard rejected —
  scores ~1 on type permutations); `null` when the pair has no comparable (both-real) context.
- `enhanced_report.build_kernel_reading_section` renders the robustness summary + Jaccard table.

Witness: `classify_corpus('testsets_haiku', …)` (full-pipeline load route) → twin
`end_of_life_decision_authority`, 156 ctx → 73 robust / 83 specific; H¹ all band 5; Jaccard
0.63/0.53/0.31. Two-sided control passed (known-divergence ctx→diverge; agree ctx→0 divergence
solutions). H¹ instance-blindness UNCHANGED (per-reading H¹ is a join: each reading is its own
`per_constraint` entry with its own `h1_band`). Commit `d2cb9bb7`.

OQ-176 spawned: `cohomological_obstruction/3` returns `H1=0` for an ABSENT constraint
(`orbit_vector/2` yields a uniform all-`unknown` vector) — Pattern-5 measured-flat-vs-didn't-look.
Latent for any consumer reading H¹=0 on an unvalidated id as "measured flat"; does not affect OQ-10
(readings always real). Engine-behavior change to a 6+-consumer core predicate → logged, not patched.

---

## 2026-06-23 — OQ-112 RESOLVED (close-out): arc is latent-hardening, structurally latent across all three live legs
**Files:** ISSUES.md, audits/2026-06-23_oq112_closeout/
**Tier:** landed

Combined witness pass closing OQ-112 (no engine `.pl` edits). Under a pre-registered **field-level**
bite-definition: only **item 1** touched live output on the 92 (13/92 abductive `agrees`→`unavailable`,
**headline-neutral**); items 2/4/7 latent-hardened; items 3/5/6/8 do **NOT** fire as live bites.

**The reusable finding (two tripwires for a future instance):**
1. **A guard-predicate count over-reports a Pattern-6 firing.** The v1 item-3 sweep said "6 of 92
   hit the absence branch"; the v2 **consumed-output reachability** pass showed those 6 short-circuit
   at `epistemic_access_check=false` → `purity_score: null` — the absence value never reaches a
   reader. Witness "does the absence value survive the upstream gate into a consumed field," not
   "does the guard fire."
2. **"Latent on the live 92" is not "latent engine-wide" until checked on a denser corpus.** The
   A6/C4c gates were re-checked on both live twins (`testsets_haiku` 960, `testsets_flash` 960,
   overlay-took witnessed): **0 live bites on all three legs.** The masking is **structural** —
   `epistemic_access_check` / the compliance-sufficiency guard require the same metric family the
   downstream absence-gate needs, so absence of the datum implies failure of the upstream gate (same
   mechanism as the claim-less maxent exclusion that makes items 2/4 latent). Archives NOT swept
   (declared scope boundary; retrospective-audit breadth, OQ-89 pattern).

Items 3/5/6/8 fix-shapes recorded in the writeup, **declared-not-landed** (latent-hardening judged
not to earn its spend pre-rebuild). The arc hardened against absence-defects but caught no live
user-facing defect — a reasonable stop under imminent rebuild, recorded as dual-status not papered over.

---

## 2026-06-23 — OQ-112 item 4 RESOLVED (Round 3, Commit 1 alone): maxent-local accessors fail-closed; Commits 2/3 falsified
**Files:** prolog/maxent_classifier.pl, docs/design/design_gaps.md, ISSUES.md, audits/2026-06-23_oq112_round3/
**Tier:** landed

The A3 metric-fallback-`0.0` idiom in the four maxent-local accessors
(`get_constraint_metrics/4`, `metric_value/3`, `get_constraint_metrics_indexed/5`,
`metric_value_indexed/4`) now returns the `unknown` sentinel on absence of
base_extractiveness / extractiveness_for_agent / theater instead of a fabricated `0.0`; the two
dead `;Supp=0.0` branches removed; `maxent_threshold_proximity/4` gained a `number/1` fail-closed
guard. **Blast radius is contained to `maxent_classifier.pl`** — Round-0 recon found the local
accessors have no cross-file consumers (the shared sources `base_extractiveness` etc. are
untouched; the hybrid fixes the *local accessor*, not the shared predicate).

**Live-unexercised on 92 (do not read as a live catch).** WA witness: 0 sentinels are produced
over the 86 claim-bearing constraints (all carry every metric), so every new else-branch is
unreached and genuine values are byte-identical to pre-edit. Item 4 is LATENT on 92, same as the
item-2 case.

**Round 0 falsified Commits 2 and 3 — they did NOT land** (the read pass killing the write,
escalated and re-ruled by operator):
- *Commit 2 (findall silent-drop) DROPPED.* The mechanism is a LOUD throw, not a silent drop:
  `sum_list` is OUTSIDE the findall and throws on `unknown`; the throw aborts precompute
  (`maxent_classifier.pl:897`) BEFORE `maxent_indexed_run_info` is asserted (`:905`), so item-2's
  completion gate already floors it. WC witnessed this end-to-end (constructed theater-absent claim
  constraint → throw → run_info absent → indexed void alert). Item-2 is NOT blind to it.
- *Commit 3 (boundary external-crash) DISSOLVED into Commit 1.* `maxent_boundary_analysis/3` has
  zero callers; `maxent_threshold_proximity`'s only live callers (`maxent_report.pl:211`,
  `maxent_diagnostic.pl:395`) are already `catch`-wrapped. The `number/1` guard is folded into the
  commit that introduces the `unknown` (hardening-at-point-of-introduction). `boundary_analysis`
  adjudicated unfinished-value (not cruft) → **GAP-19** logged (wire-it opportunity: per-constraint
  nearest-edge fragility view, the dual of the live per-boundary report).

**Tripwire candidate? NO** — the contained-blast-radius and the latent status are stable facts but
produce no *silent* mistake for a fresh agent before editing a file; they live as history here.
The general rule (maxent absence → `unknown` sentinel → item-2 gate) is already covered by the
AGENTS.md completion-witness invariant from item 2.

**Round-4 gate installed in the OQ-112 entry:** before any further round on items 3/5/6/8, point
to one verdict a user saw change across the arc (items 1/2/4/7) or declare it latent-hardening and
stop. Preliminary read: latent-hardening, pending that positive control.

**The cross-file diag-site idiom instances are NOT swept in** (deliberately): `constraint_indexing.pl:860/892/895/898`
and `invertibility_analysis.pl:111–115` carry the same `->;=0.0` idiom on the *shared* sources, but
each is outside the contained blast radius with its own consumers and its own live-bite/latent
question — a per-site adjudication deferred to the operator, not a blanket conversion.

---

## 2026-06-23 — OQ-112 item 7 RESOLVED → ROUND 2 COMPLETE: wasserstein incomparable-mass provenance tokens
**Files:** prolog/json_report.pl, python/shared/schemas.py, ISSUES.md, audits/2026-06-22_oq112_round2/
**Tier:** landed

Last staged Round-2 piece (different surface from the item-2 gate). `json_report.pl:438–442` had
four per-context arms `(catch(measurement_layer:wasserstein_incomparable_mass(C,Ctx,WM),_,(WM=0.0))
-> true ; WM=0.0)` collapsing THREE states into `0.0`: genuine measured zero, no-distribution
(producer fails), thrown (producer throws). Replaced with `wm_token/3` (float | absent | errored)
+ `wm_emit/3` (serializes float | `null` | `"errored"`). Helper carries a **fourth-state guard**
(`var(M) -> Tok = errored`): a succeed-with-unbound-M would emit a malformed JSON hole; routed
fail-closed. That state is **unreachable through the real producer** — it is STATIC (cannot be
extended at runtime) and its only success path runs `extract_chain_probs/3`, whose terminal
`IncompMass is max(0.0,…)` always binds or throws — so the guard is defensive against a future
producer change. `schemas.py:228` inner-value contract widened **in-comment only** (the
`(…, dict, True)` tuple is unchanged; the validator never type-checked inner values, so mixed
float/null/"errored" passes).

**Output-changing at the schema → landed ALONE.** Witnesses (`audits/2026-06-22_oq112_round2/`):
- `item7_wm_token_controls.txt` — 4-state forced control, all PASS. genuine 0.0→`0.000000`;
  nonzero→`0.400000`; absent→`null`; errored→`"errored"`; unbound-M→`"errored"` (guard). The shipped
  `wm_token/3` clause is pasted via `clause/2` so the state-4 guard-decision control goal is
  diff-able against the shipped guard subterm; states 1–3 run the REAL shipped helper via a
  `probe_harness:with_overlay` of the dynamic `maxent_dist/3`.
- `item7_before_after_diff.txt` — item-7-ISOLATED diff (clean BEFORE regenerated at HEAD `a5593f7`
  with item-7 reverted, vs AFTER): **ZERO other top-level fields moved, ZERO wasserstein cell flips.**
  On the live 92: 86/92 fire the section as a dict (6/92 whole-field `null` = the unchanged outer
  transport-profile failure branch), **344/344 cells genuine float** incl. measured `0.0` correctly
  kept as float (NOT collapsed to null); absent/errored arms **0-firing**. So the fix is
  **output-identical on the live corpus** — contract widening is **forced-witnessed, live-UNEXERCISED**
  (the item-2 posture applied to a contract surface).
- `item7_schema_validation.txt` — 0 schema errors over the regenerated `pipeline_output.json`.

`0.0` stays a *legal measured value* here (unlike `N=0`), so emitting `null`/`"errored"` is a
consumer-CONTRACT change, not a value change. Realized in-repo numeric-reader set was **empty**
(grep bounded to in-repo: `w1_sheaf_join.py` reads other wasserstein fields; `audit3_synthesis.py`
parses a different predicate's source; `test_harness.pl` `catch(_,fail)`). Out-of-repo / notebook
float-readers are genuinely out of reach and **unwitnessed** — a per-context value read as a float
now gets `null`/`"errored"` where the state was absent/errored.

**ROUND 2 COMPLETE.** Dual-status (both true, the second NOT subsumed by COMPLETE): round-level
"Round 2 COMPLETE" AND gate-level "item-2 maxent completion gate live-fire UNEXERCISED on 92 (0/92
latency), live trigger named as falsifier" — COMPLETE ≠ gate-proven-live. item 4 (A3) → Round 3;
items 3,5,6,8 staged.

---

## 2026-06-23 — OQ-112 item 2 RESOLVED: completion-witness-or-fail-closed gate (maxent stages)
**Files:** prolog/diagnostic_summary.pl, prolog/json_report.pl, prolog/maxent_classifier.pl, AGENTS.md, ISSUES.md, audits/2026-06-22_oq112_round2/
**Tier:** landed

Round 2 of OQ-112. A voided maxent stage previously read GREEN (probe_maxent → inconclusive,
dropped; the indexed stage is read by nothing in the verdict path = fully silent). Fix, three
commits: `d69d5d39` (Round 0 re-witness on 92 + witness-truth controls), `4ee4ce08` (the
**distinct** `maxent_indexed_run_info/3` completion fact — NOT shared with `maxent_run_info`,
because indexed needs a prior classical run so a shared fact couldn't distinguish "indexed done"
from "classical done, indexed voided"), `0ef5bf6d` (the gate: `maxent_attempted/1` markers +
`maxent_void_alerts/1` per-attempted-stage fail-closed in `verdict_join` + absorbers widened to
`( catch(G,_,fail) -> true ; true )` so a stage FAILURE continues the run). Severity moderate/
yellow (operator ruling — a void is absence-of-measurement, not measured-severe). Invariant
**promoted to AGENTS.md** ("completion-witness-or-fail-closed"); provenance here.

**Gate status — forced-witnessed, live-UNEXERCISED.** Matrix (`GATE.md`): COMPLETE→green no-op;
THROW-indexed & FAIL-indexed (the `:871-874` no-priors `failed_plain`, catch-blind) → yellow
void[indexed]; THROW-classical → yellow void[classical]; N0-legal (fact present, N=0) → green;
cross-term classical-present+indexed-void → void[classical=no, indexed=yes]. `LATENCY/92` = 0 of
92 voided → live-fire unexercised by construction. **Do not cite as "verified live on 92"** — the
live trigger is the first claim-bearing story missing `suppression_requirement` (count 0, W2),
re-checked via the item-4 reachability probe, NOT a re-run on today's 92. Deferred zero-legal
ruling: (B) defer, TWO falsifiers (zero-with-witness via W3; claim-less→claim-bearing via item-4
probe). Items remaining: 4 → Round 3; 3,5,6,7,8 staged (7 = wasserstein, lands alone, schema-level).

## 2026-06-22 — OQ-112 item-1 (C4a) RESOLVED: diagnostic_summary data-absence else-branches fail closed
**Files:** prolog/diagnostic_summary.pl, ISSUES.md, audits/2026-06-22_oq112_round1/
**Tier:** landed

Round 1 of OQ-112 (Pattern-6 census batch). Corpus pinned self-witnessing: **LIVE=92**
(membership emitted + manifest + negative control: bad `corpus_path`→`corpus_empty`,
`testsets_haiku`→960; consumer-predicate check: diagnostic path enumerates
`corpus_constraint/1` at `json_report.pl:64`). C4a = 13 `; Signal = agrees` else-branches in
`diagnostic_summary.pl`; member sort: **10 sound · 3 defects**. Discriminator: `agrees` is sound
after the probe predicate *succeeded* with a positive no-tension result (`none`/`[]`/`H1=0`/
no-override/good-zone), a defect when reached from the `catch(_,_,fail)` else (data-absence).
Fixed (commit `4e6cf6e9`): `:198`/`:212`/`:163` `agrees`→`unavailable` (dropped identically with
`inconclusive` at `classify_signals_acc:359–362`).

- **`:198` (`probe_abductive`) is the only LIVE site:** 13/92 constraints have no `abd_triggers`
  fact (producer `abductive_report.pl:401–404` enumerates only ≥1-hypothesis constraints; loader
  asserts no fact for the rest). Was counted as agreement; now dropped. **Output-changing at the
  agreements list, HEADLINE-NEUTRAL** — join verdict identical for all 92 (witness
  `probe_before.tsv`/`probe_after.tsv`; the join is driven by tensions, not the agreement count).
- **`:212` unreachable:** `constraint_signature/2` is total (metric-less id → `unknown` clause
  `:136`; metric-bearing → `classify_by_signature(_,_,ambiguous)` catch-all `:353`). 0 live
  firings; fixed as fail-closed hardening per the operator guardrail.
- **`:163` unreachable:** `classify_disagreement/7` is total over 5 shapes; `probe_maxent` handles
  all 5 by name. Fixed so a future 6th shape reports uninterpretable, not agree.

Tripwire (don't make this mistake): the Python enrich side already distinguishes file-absent
(`None`→unavailable) from cid-not-in-file (`[]`→measured-empty) at `enrich_pipeline_json.py:164–169`;
the Prolog consumer was the only site collapsing absence→agreement, and `abd_triggers/2` is
`:- dynamic`, so a *missing* `abductive_data.json` would leave the subsystem "available" and route
every constraint to `:198`→agrees (file-missing = universal agreement). Items 2–8 staged in ISSUES.md
with corpus-re-witness obligations (inherited 62/194-row verdicts are not standing on 92).

## 2026-06-22 — OQ-20 + OQ-174 RESOLVED: DR baseline code/data diff (PERTURBED, stable core)
**Files:** ISSUES.md, prolog/json_report.pl, prolog/drl_purity_network.pl, python/audits/oq20_strip_cs.py, python/audits/oq20_dr_diff.py, python/audits/oq20_make_rekey.py, python/audits/oq20_analyze.py, audits/2026-06-22_oq20_dr_baseline_diff/
**Tier:** correction-key

Corpus-fixed / code-varied diff of DR output, tag `v3-dev-baseline` (`3e75f90b`)
vs HEAD, via `run_json_report` only (bypasses the diverged `run_pipeline.py`).
Cells A/B (original_json), C/D (original_v6_csfree), E/F (kernel_v1 cs-strip);
all cells byte-identical across repeats (empty noise floor). Full method +
controls: `audits/2026-06-22_oq20_dr_baseline_diff/WRITEUP.md`.

**Arm 1 (OQ-20) = PERTURBED, replicated on both corpora.** Two type surfaces, one
moved: the **priority-cascade** classification is BYTE-STABLE (identical 13-field
zero-diff set incl. `claimed_type`, `classifications`, `base_extractiveness`,
`suppression`, `theater_ratio`, `victims`, `beneficiaries`, and the χ/ε/d/f_d
values), but the **MaxEnt `maxent_top_type` is NOT** (29% flips original_json,
**73% original_v6**, concentrated as `tangled_rope→snare` ≈2261 → minted OQ-175
to bisect that boundary move). Also changed: `signature` (~85%), MaxEnt
distribution. `gaps` list→null is **NOT a
regression** — it's OQ-109 B3's coverage-bit + the 2026-06-14 detect_gap_pattern
rebuild (null=didn't-look vs []=examined). Code-vs-noise attribution is
witnessed: the empty noise floor is positive-controlled (fresh-process repeats
independently recompute; warm in-process 2nd run byte-identical to cold), so it's
real, not a cache shadow — #5's non-determinism (session-overlay memos, Python
phases) is bypassed by the `run_json_report`-only path.

**Correction-key items (how to cite):**
- The original OQ-20 mechanism ("checkout tag, byte-diff") is CONFOUNDED — the
  tag swaps the corpus (reset 2026-06-05). Hold corpus fixed, vary only code.
- The per-constraint `id` relabeling (tag in-file id → HEAD filename base) is
  commit **`801390a5`** (`known_constraint/1`→`corpus_constraint/1`), **not** the
  UUID migration. Do not attribute on the ratio alone.
- **Tripwire:** running HEAD on a legacy/archive corpus whose **filename ≠ in-file
  constraint id** yields **null DR output for those stories** (HEAD enumerates the
  filename, queries facts under it, finds none — no error). 133/1151 in
  original_json; 0 in original_v6 and the live corpus (filename==internal there).
  Re-key by in-file `constraint_metric` subject before any cross-id comparison.

**Arm 2 → OQ-174 (Ω_C, RESOLVED — benign carve-out).** Stripping all `cs_*` from
kernel_v1 leaves the DR observer core fully detection-independent (Theorem 7
holds) EXCEPT `contamination_network` (180 stories incl. 28 cs-free neighbours),
where `constraint_neighbors/3` reads `cs_reading_relation` into `explicit` edges
(`drl_purity_network.pl:67,92,257`). Crux settled by substrate:
`cs_reading_relation` is an **authored corpus fact** (written into testsets,
never asserted by code — read-only in `once`/`\+` guards), so this is a
**shared-input dependency, not detection-dependence** — Theorem 7 (which forbids
detection output feeding detection) is intact. The "200 cs-free byte-identical"
negative control "fails" because the authored edge couples cs-free neighbours —
a feature, not a bug.

---

## 2026-06-21 — OQ-35 RESOLVED: wiring-gap census rows 1–6 adjudicated (cruft-vs-wire)
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/probe_oq35_field_counterfactual.pl, python/audits/oq35_field_counterfactual.py, prolog/narrative_ontology.pl, audits/2026-06-21_oq35_field_counterfactual/
**Tier:** correction-key

Adjudicated the 6 authored-field wiring gaps (`audits/2026-06-21_oq35_field_counterfactual/writeup.md`).

- **Rows 2–3 `accessibility_collapse`/`resistance` — RETAIN, load-bearing (census REVERSED).** The
  2026-05-31 census called them "cosmetic (T.1)"; that was NL-override-specific and superseded by the
  OQ-128/OQ-138 routing-sink conversion. Counterfactual probe over 5 corpora (full observation tuple
  `obs(dr_type, signatures, verdict, alerts, signature_grade)` — NOT `dr_type` alone, which shows a
  false 0-diff because the signatures these feed ROUTE post-OQ-138). Load-bearing in every presence>0
  corpus; null control clean=0 everywhere; positive control passes everywhere. Treatment diffs:
  testsets 55/92, haiku 691/960, flash 537/960, kernel_v1 26/44, original_v6 421/3380. **Citation
  caveat:** "cosmetic" must NOT be cited for these fields — the diff variable is the routing observable
  (signature/verdict/alerts/grade), not `dr_type`.
- **Row 1 `is_mandatrophy_resolved/1` — dead facts, STRIPPED (operator go).** The 2 facts + comment
  removed from `narrative_ontology.pl` (retirement note left); output-neutral, **diff-proven**
  (validation-suite output byte-identical bar `[ELAPSED]` jitter; pre-existing lycurgan interval warning
  unchanged). Zero goal-body/meta-call readers (grep). The only mandatrophy surface
  (`format_mandatrophy_gap/3`→`compute_chi_v6/6`) is independent of the facts → strip safe. That surface
  is itself dead on the live corpus (0 GAP lines; gate needs `constraint_classification/3`
  powerless≠institutional, 0 powerless facts live) → logged as a dangling consumer (`design_gaps.md` GAP-18).
- **Row 4 `cs_reference_frame/2` — RETAIN on the OQ-133 bet** (inert consumption: serialized at
  `json_report.pl:590`, no join). `design_gaps.md` GAP-17 + kill condition. **OQ-38 corrected:** its
  "confirmed dead `cs_reference_frame/2`" was stale (`:590` is a real read site).
- **Rows 5–6 `uke_scope.*`, `commentary.*` — by-design, no action.**

OQ-35 status open→resolved. No engine behavior changed (probe is pure evidence; the row-1 strip is
output-neutral, diff-proven).

## 2026-06-21 — OQ-173 RESOLVED: MaxEnt signature-override boost made seat-aware (OQ-138 maxent residual)
**Files:** prolog/maxent_classifier.pl, prolog/load_warning_allowlist.txt, ISSUES.md, docs/design/design_gaps.md, docs/technical/signature_detection_wiring.md, audits/2026-06-21_maxent_seat_aware/FINDINGS.md
**Tier:** landed

`apply_override_for_sig/3→/4`: `C` threaded from the single call site (maxent_classifier.pl:318); the
two converted signatures skip the MaxEnt boost at routed seats — `false_ci_rope` at
`signature_detection:fcr_routed/1`, `constructed_high_extraction` at `constructed_routed/1` (reused
verbatim, unbound-cascade keyed; `DistOut = DistIn` reverts the seat to its pre-override raw dist).
Non-converted clauses ignore `C` (byte-identical). Covers BOTH serialized surfaces (`maxent_top_type`/
classical `maxent_probs` and `maxent_indexed`) — both classify paths call the same
`apply_signature_override/3`.

**Witness** (`audits/2026-06-21_maxent_seat_aware/diff_witness.out`): exactly the 12 routed seats
(9 fcr + 3 constructed) revert to raw; **0** non-routed seats move on any maxent surface (negative-half
byte-clean via the raw-probs discriminator); **1** categorical flip — `shinbutsu` indexed top
tangled_rope→snare (the one genuinely-manufactured verdict); **0** `verdict_join` changes.
**Premise refinement (correction-key):** OQ-138 framed the residual as the boost flipping
`maxent_top` to tangled_rope; substrate shows the conditional ×3 boost **never flips a CLASSICAL top**
(positive control: only 2 corpus-wide flips, both non-converted UNCONDITIONAL overrides
`false_natural_law`/`coupling_invariant_rope`) — the manufacturing was classical-mass + the indexed
top, not a classical-argmax flip. 21-corpus generality sweep: `routed_STILL_boosted=0` everywhere,
non-converted boosts intact; `original_v5` PARTIAL (pre-existing `maxent_run` failure, stash-confirmed
NOT a regression — recorded as partial, not swept-clean). `validation_suite` 92/0/0; `check_stack`
baseline-clean; `gate.sh` GREEN. Incidental: renamed a pre-existing `[C2]` singleton → `_` and pruned
the now-stale `maxent_classifier.pl:852` load-warning allowlist line.

**Tripwire (promotion candidate — held to history, loud-not-silent):** when converting a future
signature override RECLASSIFY→ROUTE, the MaxEnt boost in `apply_override_for_sig` is a THIRD surface
to make seat-aware (after `dr_type` and the diagnostic consumers) — skip-guard it on the same
`*_routed/1` predicate. NOT promoted to CLAUDE.md: the omission fails loudly (the next conversion's
pipeline diff shows the routed seat still at the override target), and the recipe now lives in
`signature_detection_wiring.md §4`.

## 2026-06-21 — OQ-138 constructed-3 sub-part RESOLVED: claim-discriminant conversion (keeps #2's floor)
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, ISSUES.md, AGENTS.md, audits/2026-06-21_oq138_fsm_route_conversion/CONSTRUCTED3_FINDINGS.md
**Tier:** landed

Routed the 3 live `constructed_high_extraction` unknown→snare seats to the honest abstain `unknown`. NEW
**claim discriminant** (mountain→severe, else→informational — victim doesn't distinguish, all 3 vic>0): a
mountain claim over high-extraction is the concealment, kept at severe, REPLACING the floor the manufactured
snare used to carry via `type_1_false_summit` (which now reads informational at dr_type=unknown). **Kill
condition MET:** #2 (institutional_trust_erosion, claimed mountain) keeps RED byte-identical (floor source
moved type_1→signature); #1/#3 route to yellow/commentary; 47 inert + all non-constructed byte-identical;
5-corpus `mountain-routed→severe` holds everywhere. Reused the seat-aware template (`constructed_routed/1`,
`converted_at_seat/2`, `seat_overrides/2`). **`constructed_routed`/`fcr_routed` keyed on the UNBOUND cascade
winner** — a bound-arg query trips on the detector even when shadowed (caught `superheavy_decay`, an FCR seat,
in constructed_routed; §1 gotcha; the fcr_routed fix was behavior-preserving). **Maxent residual confirmed
(operator's warning):** the boost (`maxent_classifier:341`) flips #1/#3's maxent_top→tangled_rope at the
pipeline surface (unlike FCR top=rope) — benign (headline yellow; #2 red via severe), seat-aware maxent (plumb
C) tracked as a shared GAP. validation_suite 92/0/0; check_stack clean. Full: CONSTRUCTED3_FINDINGS.md.

## 2026-06-21 — OQ-138 FCR-9 sub-part RESOLVED: false_ci_rope SEAT-AWARE conversion (template didn't transfer)
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, prolog/diagnostic_summary.pl, ISSUES.md, AGENTS.md, audits/2026-06-21_oq138_fsm_route_conversion/FCR9_FINDINGS.md
**Tier:** landed

**The FSM template did NOT transfer directly — false_ci_rope is SEAT-SPLIT** (9 routed / 3 piton / 13 inert,
one signature). FSM had no inert/piton, so its signature-level mechanism worked; for FCR, signature-level
keying would flip the 13 inert seats' grade and (witnessed by ablation) disturb the 12 already-mismatched
piton+inert seats. Built **seat-aware**: type route (resolve_with_perspectival_check clause 3 else
tangled_rope→ModalType); `fcr_routed/1` keyed on the stable dispatch GATES + the dr_type OUTCOME (NOT a
`metric_based_type_indexed` proxy — that proxy diverged from the live ModalType on 2 haiku+4 flash seats,
**caught by the 5-corpus generality sweep** before shipping, then replaced by the outcome check which also
removed the dispatch-mirror fragility); `converted_at_seat/2` (signature-level FSM, seat-level FCR);
`seat_overrides/2` (abductive_helpers, exported) threaded through diagnostic_summary `probe_signature/3` + P1/P7
so the routed-9 are non-override (honest unmask) while piton/inert keep override semantics.

**Witness:** 9 seats route tangled_rope→scaffold/snare; 6 verdicts change (vic>0 correction/moderate, vic=0
commentary/informational, sig=AGREE — no spurious override_mismatch; milder than FSM, mostly yellow); piton-3
TYPES unchanged + 13 inert FCR + all non-FCR byte-identical. **Carve-out relaxed:** statutory_debt (piton)
shifts yellow→red via the corpus-relative maxent ENSEMBLE (entropy_flag) — type unchanged, OQ-90 not
relitigated (Position-A). 5-corpus invariants pass (routed∩piton=0, routed-still-tangled_rope=0,
piton-not-piton=0); validation_suite 92/0/0; check_stack clean. **Residual:** maxent FCR boost
(maxent_classifier:331) still signature-level (no C) — benign for the 9 (maxent top=rope), logged for
constructed (same shape at :341). Full: FCR9_FINDINGS.md.

## 2026-06-21 — OQ-138 FSM sub-part RESOLVED: false_summit_mountain converted RECLASSIFY→ROUTE; routed false-summits read RED
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, prolog/config.pl, ISSUES.md, AGENTS.md, docs/technical/signature_detection_wiring.md, audits/2026-06-21_oq138_fsm_route_conversion/
**Tier:** landed

**What landed.** `false_summit_mountain` no longer overwrites `dr_type` (config
`false_summit_override_target` default `tangled_rope→mountain`; existing hook neutralizes the overwrite
and stays an ablation lever; unknown-input clause `→unknown`, 0 live fires/unverified-in-commit).
Shared severity template: `signature_detection:converted_signature/1` + `signature_diagnostic_severity/3`;
`signature_grade/2`+`signature_severity/2` grade converted signatures on the victim discriminant
(`vic>0→moderate/correction`, `vic=0→informational/commentary`), NOT on the now-zero type delta
(`dr_claim_mismatch/4` precedent). FSM removed from `abductive_helpers:known_override_signature/1`+
`override_target/2` (else `probe_signature/3`/P1/P7 misfire post-revert).

**Two divergences from the plan (both witnessed, both material).** (1) Live corpus grew **57→92**; it now
has **3** FSM seats incl. one **vic>0** (`protein_anabolic_resistance`) — the kill condition is on live
main. (2) **The report-surface verdict goes yellow→RED, not the plan's expected green.** The override was
masking dirac(`second_class`)+cohomology(`fails_descent`)+abductive tensions by setting the type to
tangled_rope (where they are "expected"); reverting to mountain unmasks them as genuine contradictions.
`claimed_type=mountain` preserved (route ≠ reclassify). **Operator ruling 2026-06-21:** the engine adds
commentary, does not change classifications, and it is OK for diagnostics to render different verdicts —
**Position A** (let subsystems speak; red is honest) over Position B (suppress dirac/cohomology to force
green). The victim discriminant lives in the commentary layer (`signature_grade`/alert severity).
Evidence: **82 FSM seats across 5 corpora** (≈6,500 stories) ALL carry cohomology/dirac → 0 where the
discriminant would be headline-visible (the tensions are structural invariants of false summits).

**Witnesses.** Full-pipeline corpus diff = **only the 3 FSM seats change, 89 byte-identical**
(`PIPELINE_OLD.txt` vs `PIPELINE_NEW.txt`). `severity_floor/2` two-sided positive control discharged.
Trap (silent green) averted: headline RED; protein keeps `correction` via the discriminant despite zero
type-delta (a naive revert drops it to commentary). `validation_suite` 92/0/0; `check_stack`
baseline-clean; `test_contradiction_signatures` 5-fail is pre-existing CS-axis fixture (identical OLD vs
NEW, confirmed by stashed-build run). **Subtlety:** `constraint_signature/2` is a cut-cascade returning
ONE signature; a BOUND-arg query bypasses the cuts (the build uses the unbound form — correct).
OQ-138 stays **partial**: FCR(19)/constructed(41+)/CI-rope(4) OPEN with named witnesses, FNL deferred
(OQ-70). Full detail: `audits/2026-06-21_oq138_fsm_route_conversion/FINDINGS.md`.

## 2026-06-21 — OQ-119 RESOLVED: feeding moves the verdict layer, committer invariant (Theorem-7); + the cs_-facts generator tripwire
**Files:** ISSUES.md, agent/cohort_replicate_batch.py, agent/generate_kernel_corpus.py, python/audits/oq119_spend_driver.py, python/audits/oq119_analyze.py, prolog/export_oq119_corpus_join.pl, audits/2026-06-21_oq119/, audits/2026-06-21_oq119_gate0/
**Tier:** tripwire

**Tripwire (the reusable, silent-mistake fact):** the **single-story generation path**
(`cohort_replicate_batch.py` / `story_generator_base.build_prompt_parts`) authors **NO `cs_` facts** —
a regenerated single story has observer + temporal but **no `cs_kernel_id` / `cs_reading_relation`**
(witnessed: `audits/2026-06-13_oq117_within_arm_proxy/fed_arm/*.json`). The **committer/CS axis exists
only on the kernel-generation path** (`generate_kernel_corpus` no-scope / `c-orchestrator` scope, which
authors `cs_structure.reading_relations` → `generate_constraint_pl.py:666`). **Any fed/withheld or
perturbation experiment that needs the committer axis MUST use the kernel-regen path**, or it silently
measures ≤2.5 axes while looking complete (the exact vacuity OQ-119 forbids). Corollary: `GEN_MODEL` is
**Haiku**, which intermittently drops the schema-required `stakeholders[]` (OQ-149 `allOf[0]` gate fires
loud → coverage holes; a Haiku pass left 2/5 kernels at full coverage, Sonnet → 5/5). Override to Sonnet
for precision fed/withheld spends; the bulk-build Haiku default stays.

**Result (OQ-119, full detail in `audits/2026-06-21_oq119/WRITEUP.md`):** 96 Sonnet generations,
parties-fixed fed framing, per-axis `median(D_A) > max(F_A)` against the measured generation-noise floor,
observer de-weighted. **Feeding moves the DIAGNOSTIC VERDICT layer (4/5 kernels: false_natural_law
escalates commentary→correction +1 alert) and leaves the COMMITTER obstruction/divergence INVARIANT
(0/5 — Theorem-7 detection-independence holds, measured not assumed).** Observer + temporal-rate move
softly. The verdict move is substantially the claim-gated FNL path (semi-expected); the committer
invariance is the non-trivial result. Committer is generation-noisy (withheld redraws flip
real_closure↔licensed_plurality) → routed to OQ-149.

**Correction-key:** the schema's mountain no-parties `stakeholders` exemption (`allOf[0]`) is
**deliberate** (OQ-149 2026-06-19 `becd0f87` + OQ-83 Pattern-5 omit-vs-authored-empty) — do NOT "tighten"
it to rescue a weak generator; the fix is the model + a parties-fixed fed framing, not the schema.

## 2026-06-20 — OQ-71 depth-lineage: Phase A closes the design question (mitigated, no spend)
**Files:** ISSUES.md, docs/design/a_hypothesis_about_corpus_size.md, python/audits/oq71_a2_richness_alldims.py, audits/2026-06-04_oq71_depth_lineage/, python/build_lineage_seeds.py, agent/generate_kernel_corpus.py
**Tier:** correction-key

Ran the OQ-71 plan's **Phase A (zero-spend, read-only)**; it closed the design question and
demoted the spend, so OQ-71 → **mitigated** (not resolved; not the spend).

- **A0 (the feasibility gate, witnessed):** the kernel-nesting relationship **never reaches the
  Haiku generator**. `build_lineage_seeds.py:114–134` forks the generation `seeds`
  (→ `lineage_seeds.json`, fed to the model — **no `parent_kernel`/`level`**) from a separate
  `lineage.json` sidecar (parent/level, consumed only post-hoc by the fingerprint join). Generator
  prompt `generate_kernel_corpus.py:430–486` reads only flat seed fields; grep finds no
  `parent_kernel`/`lineage` read (`:104` comment "kernel lineage is carried separately"). Origin
  plan `~/.claude/plans/virtual-inventing-allen.md` confirms this was **deliberate** ("only seed
  authoring and output routing differ"; generator frozen) → mitigated, NOT
  inconclusive-by-construction. **Consequence:** the plan's breadth arm reading-(a) ("strip
  `parent_kernel`, regenerate") is a **provable no-op** — `depth − breadth ≈ 0` by construction —
  so branch 1 (depth-realized-at-generator) was never in the experiment and the instrument can't
  isolate it.
- **Two-path architecture (why no_scope is blind to nesting — by design; operator-flagged).** SCOPE
  path (`_scope_user_prompt`/orchestrator `_step_decompose`) hands the MODEL a topic and lets it
  CONSTRUCT the kernel; no_scope renders PRE-DECOMPOSED readings. Batch generation forces
  decompose-FIRST (can't SCOPE-construct inline across a batch) → per-reading prompt structurally
  blind to nesting; inherited by any breadth arm. The CONTROL's structure was itself
  model-SCOPE-constructed then harvested (`build_never_generated_seeds.py` pulls `is_contested_kernel`
  SCOPE manifests). So depth-vs-control at the structure level = Opus-designed nested tree vs the
  SCOPE model's flat decompositions → branch 2 ("author-identity") = *who constructed the kernel
  structure*, not just prose.
- **Correction-key — claim widths:** the 1.5× excess is **not generator-visible parent-nesting**;
  it is the authorship-bundle (Opus identity and/or lineage-structured authoring, undistinguished).
  Cite at THAT width — not "the excess IS authorship" (residual-elimination overclaim) and not
  "depth re-opens discovery." "Generator never saw depth" is too strong: `sibling_reading_ids`
  reaches the prompt and covaries with level (r=−0.366) — say "never saw **parent-nesting**,"
  co-channel bounded by the length-stratification control. The "156>118" line is **color**
  (non-matched-n, cross-regime), not evidence.
- **A2 (list-inflation closed, all 5 dims):** matched n=294, K=2000 — JOINT distinct-class excess
  +38.7 vs largest single-dim MARGINAL excess +2.8 (zone); depth uses *fewer* props/voids/actors
  values → new combinations, not proliferation; positive-controlled. Closes the prior 2-of-5-dims
  caveat. Witness: `audits/2026-06-04_oq71_depth_lineage/a2_richness_alldims_results.json`.
- **Watch-out (witnessed):** `outputs/completion_seeds/never_generated_seeds.json` **drifted
  2026-06-13** — missing 26 of the 300 frozen control ids, so it no longer reproduces the audit's
  length-2+ stratum (294→268). `control_membership.json` (the 300 ids) is the durable authority;
  A2 ran drift-immune on full frozen arms + the current stratum (same verdict both). Any OQ-71
  re-run keying on that seed file inherits the drift.
- **Graduation step (→ resolved, deferred):** Opus authors ~300 *flat* seeds, same frozen generator
  (origin plan reading-(b)) — splits author-identity from lineage-structure (the only live question
  once branch 1 is out of scope). Needs spend; declined this session; recorded in OQ-71 + §10.1 for
  a future instance.
- **Construct-validity gap → OQ-171 (minted this session).** §3's bounded-attractor claim is about
  the SCOPE construction path; OQ-71 falsified only *substrate-level* boundedness (Opus/no_scope),
  never the SCOPE path. Do NOT read mitigated as "§3 tested" — §3 stands within-regime. OQ-171
  registers the context-controlled batch-of-one design (vary inline-context, hold topics) and
  declines the naive small-batch proxy (inherits OQ-71's disjunction). May be non-constructible
  (A0 obstruction recurs); spend + pricing = operator seat.

## 2026-06-20 — OQ-69 research-frontier ledger DRAINED → OQ-154–170; OQ-69 closed
**Files:** ISSUES.md, issues/INDEX.md, issues/INDEX.json, CLAUDE.md, audits/2026-06-20_oq69_ledger_drain/
**Tier:** landed

OQ-69 was a backlog *ledger* (Ω_P), not a single question — it resolves by being **drained** (each
live item promoted to its own OQ), not by executing its contents. Drained the 16 still-live bullets
into **17 new OQs (OQ-154–170)** and closed OQ-69 `resolved` with a provenance map in its body. The
16→17 expansion: the engine-hardening bullet is three legs (OQ-154/155/156) and the cluster bullet
splits F/G (OQ-160 `gates` OQ-170). The prior check_stack item had already graduated → OQ-142–145.
**No engine code changed** — tracking restructure + index regen + doc-currency only.

Two operator rulings this session (both escalated, not self-resolved — genuine source conflicts in
the plan): (1) **cluster splits F/G** → OQ-170 `blocked_on OQ-160` added (the ledger's "Pkg F then
Pkg G after" is a real edge; the splitting rule + §5's BLOCKED-G witness outweighed the stale
"count=16"); (2) **priority scheme = distinct-within-band, bands overlap 1–10** (Higher 1–3 / Medium
1–5 / Lower 1–9). All 17 priorities are **provisional — operator to rule** (the declared seat).

Correction (Pattern-5 premise rot): the priority parser is **not** capped at 10 — regex
`^\*\*Priority:\*\*\s*(\d{1,2})\b` (omega_resolver.py:69) accepts 1–99; "1–10" in omega_resolver.md
is doc convention only. δ correction (OQ-162): the ledger's "δ not load-bearing" was the stale half —
witnessed perturbation probe shows δ is **live-but-zeroed** (wired `resolve_displacement →
D_eff=clamp(D+δ) → χ`, flips at δ:=0.3, but config default 0.0/uniform makes it inert as shipped).

Close-vs-keep-open ruled from code: `omega_resolver.py:244–258` authority set is all parsed OQs
(resolved included) → a resolved parent doesn't dangle; no inbound Deps edge points at OQ-69 →
**close** (not keep-open as a thin parent). Witnesses (all pasted at commit): `issues_status --check`
170/0, `omega check` 0 problems, `selftest` 10/10, `menu` arrival of 154–170 (156+170 BLOCKED, 168
BLOCKED-ON-YOU) + departure of OQ-69 **and control OQ-63** from WORKABLE (resolved items excluded),
`gate.sh` GREEN. Full writeup + δ probe: `audits/2026-06-20_oq69_ledger_drain/`.

## 2026-06-20 — OQ-58 cross-corpus census, non-gating linter wired, three-leg/beta corpus ruling
**Files:** python/run_pipeline.py, python/audits/reading_reference_linter.py, agent/generate_kernel_corpus.py, ISSUES.md, docs/design/design_gaps.md, CLAUDE.md, audits/2026-06-20_oq58_cross_corpus_incompleteness/
**Tier:** tripwire

Re-measured OQ-58 after the 2026-06-05 reset stale-ified its counts; wired the
referential-integrity linter as a non-gating `reading_linter` step in
`run_pipeline.py` `_phase_post_prolog` (writes `outputs/reading_reference_census.json`,
manifest+corpus_hash; `summarize()` added to the linter, behavior-preserving).
Witness: pipeline 47/47 OK in 10.4s, step "163 dangling → 158 missing / 66 kernels
(5 id≥2 defensible) — NON-GATING"; linter selftest PASS; gate GREEN.

**Census (read-only, `audits/2026-06-20_oq58_cross_corpus_incompleteness/census_driver.py`):**
LIVE testsets 92 files / 169 csr edges / 163 dangling / **93.5%**; testsets_haiku
960/2004/127/**3.7%**; testsets_flash 960/2008/101/**2.3%**; kernel_v1 1106/1774/94/**4.8%**.
LIVE 93.5% is a SPARSITY artifact (1.03 readings/kernel, 97% singletons), not a frontier.
GAP-07 bounded-attractor answer (split): rate bounded ~2-5% across lineages; defensible
id≥2 count ~40 reproducible WITHIN a lineage (haiku 39 ≈ flash 41, haiku∩flash 39), NOT
tri-lineage (kernel_v1 8; common core 1).

**Regime swap (git, corrects the planning note's direction):** the 06-13 rebuild pilots
BUILT testsets/ to 1000 files / 2.92 r/kern (reconciled multi-reading corpus); commit
`0ccc03cf` then moved it OUT to the twins (haiku/flash, byte-intact 960/960) and testsets/
reverted to a singleton working set (51 → 92). The "accidental clobber" fear is falsified.

**TRIPWIRE — three live legs, beta posture (operator ruling 2026-06-20; promoted to
CLAUDE.md Critical Distinctions "THREE LIVE LEGS, and the beta posture").** `testsets/` is
the live leg ON PURPOSE — a deliberately singleton topical working set to exercise the
engine while building it and surface live issues; `testsets_haiku/`+`testsets_flash/` are
the reconciled twins (comparison baseline). The singleton sparsity is INTENDED — do not
complete/flatten/rebuild testsets/ on sight. Currently ALPHA, working toward BETA: extract
maximum value from the current corpus so it earns its way to beta before any rebuild; a
fresh `testsets_*` rebuild comes only after
schema/wiring/enough-of-ISSUES.md are worked out (many OQs open → a ways off). A future
instance MAY suggest a rebuild when accumulated changes warrant it, not propose one lightly.
This resolves the OQ-58 corpus-identity flag (was `blocked_on_human`).

OQ-58 downgraded partial → mitigated, Priority 1 → 3; generation deferred (two backlogs
recorded: durable twin-reproducible 39; stream-relative live 5/3). Quarantine JSON
documented as a per-run artifact, not the live backlog (note at the writer + ISSUES).
Commits `1c5c97a7` (code), `9532ffe4` (docs).

---

## 2026-06-20 — grid-diet display: one-informative-line-when-absent + stale "unauthorable" fixed (OQ-93)
**Files:** prolog/report_generator.pl, prolog/data_repair.pl
**Tier:** landed

Two display fixes to the OQ-93 grid-provenance surface (OQ-93 is RESOLVED; grid is opt-in by story
focus, authored-or-absent — NOT a bug when 0/32). Consumer of these reports is a MODEL doing essay
synthesis; operator ruling 2026-06-20: it needs relevant outputs, not Prolog internals.

1. **One informative line when absent** (`report_generator.pl`, the report-body grid line). The
   ABSENCE is itself the signal — a story that could author a leveled coercion grid and didn't is
   not level-resolved-coercion focused. So on `authored+injected+imputed == 0` the body now prints a
   single plain line: `Leveled coercion grid: not authored (story not level-resolved-coercion
   focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)`.
   (Superseded the same-day "terse + [CONDITIONAL] token" form, commit 5c23830e — the operator ruled
   the model doesn't need the Prolog `[CONDITIONAL]` jargon; the plain text carries the same
   "ungrounded" meaning.) **OQ-98 ruling 1 preserved for PARTIAL grids** (0<authored<total still
   prints `[CONDITIONAL: grid authored X/Y]`); only the fully-absent case went plain. Grid stories
   print the full verbose line unchanged (witnessed: `sex_gender_category__identity_reading` →
   `authored 32/32`, Kappa 0.67, coverage 4/4). Surfaced in the .md via `run_prolog_report`.
2. **Stale message fixed** (`data_repair.pl:356` print + `:291` comment). Both claimed the grid is
   "unauthorable under the live generation schema" — false since OQ-93 resolved 2026-06-11 (3 live
   testsets author grids). `report_grid_provenance/1` is REACHABLE (`repair_interval/1`, used by
   scenario_manager/test_harness), so reworded, not deleted. Now: "opt-in by story focus
   (authored-or-absent; injection/imputation retired)".

Hinge witnessed (not assumed): `grid_provenance` reaches `pipeline_output.json` — 86/92 constraints
carry it in `verdict_join`, 0/32 stories show `{authored:0,…,absent:32,total:32}`. So trimming a
display surface cannot drop provenance; the machine-readable sink keeps it.

STILL OPEN (the bigger half): `assemble_report` embeds the FULL Prolog stdout into the model-facing
.md (witnessed: `header + prolog_output`), so a 0/32 story still carries ~12 grid-absent DEV-preamble
lines the model doesn't need (`[SHIM]`, `[REPAIR]`, `[OPEN] N/N grid components absent` ×8,
`[PROVENANCE]` ×2, `[WARN]`, `[INTENT] OPEN (no_gradient_data) [grid diet:…]`), plus the banner
`_grid_line` (`Grid: authored 0/32 …`). Decluttering these is content-removal from the model artifact
— pending operator go (show-before-delete). Sibling: `intent_engine.pl:75`.

---

## 2026-06-20 — OQ-56 + OQ-53 closed: canonical cross-kernel reading-stance vocabulary ruled
**Files:** python/orbit_operator.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** landed

The last two open items of the kernel/reading-axis thread are closed.

**OQ-56 (Ω_P ruling — resolved).** Canonical cross-kernel reading-stance vocabulary = the two Tier-1
draw-robust keys, `observer_signature` (reading-unit, twin-agreement 0.722) + `obstruction_class`
(kernel-unit, 0.734). The six Tier-2 keys (incl. `seat_role_vector`, 0.245) are report-only,
model-relative. Made a **checked fact, not a memory** (Build Discipline Pattern 2):
`CANONICAL_VOCABULARY = {"observer_signature","obstruction_class"}` in `orbit_operator.py`, surfaced as
`canonical` on every orbit record (witness: `canonical=true` on exactly the 2 Tier-1 keys / false on the
6 others). Owned seat written first-person in `design_discipline.md` §0.1 (decline-not-refute the
seat-role-vector rival). **Kill condition recorded but NOT armed** (ISSUES OQ-56): a live downstream
consumer that *requires* `seat_role_vector` inside the canonical vocabulary to FUNCTION (not display)
reopens it as Option 2. None exists as of 2026-06-20 — witnessed two-pronged grep (named-key +
generic-`canonical`, pre- and post-`canonical`-stamp), each with an `observer_signature` positive
control. Detection is **manual, not automatic** — nothing trips if such a consumer is built later; the
condition is a documented reopen trigger re-evaluated by hand (re-run Step 0 grep), not a live tripwire.

**The headline Ω_E finding (recorded as the result, not buried).** OQ-56's motivating question — name the
semantic stances (naturalizing / coordination / power-revealing) comparably across kernels — has **no
draw-robust answer on this corpus**: reproducible keys are structural/coarse; the one semantically-aligned
key (`seat_role_vector`) is draw-fragile. The semantic-stance transpose is **foreclosed-as-draw-robust**,
model-relative only — an Ω_E, reopenable by a more reproducible extraction.

**OQ-53 (transpose leg — resolved, Branch 1 witnessed-live).** Within-kernel leg already satisfied (OQ-55
router). Transpose query — hold `observer_signature` fixed, sweep across kernels — runs live and finds
multi-kernel orbits: `constructed_high_extraction` spans **25 genuine multi-reading kernels**,
`false_ci_rope` spans **11** (positive control: 89 distinct kernels present, query detects 5 multi-kernel
orbits — not byte-identical to an empty read). `logical_fingerprint.pl` stays prefix-opaque by design, so
the close is (a-restricted).

Promotion test: this is a one-time ruling, not a silent-mistake-before-editing-X trap → history-grade, no
CLAUDE.md tripwire. The one durable do-not (`canonical` is a checked set, don't re-derive from `tier`)
lives at the code in `orbit_operator.py`'s docstring.

**Downstream consumer wired (enhanced_report.py).** The cross-kernel orbit artifact has no per-constraint
report consumer (and shouldn't — its product is the corpus-level transpose query). The one genuinely new
per-constraint datum is a **draw-robustness tag on the Signature line**: the report's `Signature:` IS
orbit_operator's `observer_signature` key, so it now reads `Signature: <label>  (canonical stance ·
twin-agreement 0.722)`, reusing `orbit_operator.KEY_META` as the single source (not a hardcoded number —
witnessed: flipping `canonical` in KEY_META flips the tag). It qualifies the *vocabulary's* draw-
reproducibility, not the specific value. Helper `_signature_robustness_tag()` in `enhanced_report.py`.

---

## 2026-06-20 — orbit regeneration wired into the pipeline (was a manual pre-step; OQ-29 follow-up)
**Files:** python/run_pipeline.py, python/sweeps/regenerate_orbits.py
**Tier:** landed

`run_pipeline.py` now runs `regenerate_orbits.py` as a sequential **Phase 1b** step (`regenerate_orbits`,
after `prep`, before the parallel Prolog phase and before `manifest_inject`). Previously orbit
regeneration was a MANUAL pre-pipeline step, and `manifest_inject`'s `check_orbits_corpus_hash` would
fail-closed if you forgot it (the recurring "product_site_orbits.json is stale: corpus_hash … != …"
error). Operator ruling 2026-06-20 (the regenerate-every-time vs on-demand tradeoff): regeneration is
cheap (~1.3s on the live corpus) and the manual-step friction wasn't worth the stale-orbits error, so
run it with the pipeline. The `manifest_inject` corpus_hash check is **kept as the fail-closed backstop**
(catches a regen that failed or was skipped) — the OQ-29 Thread-C guard is unchanged.

Sequential placement is deliberate: it must not race the shared `product_site_orbits.json` with the
parallel Phase-2 swipl analyses (serialization rule). Runs as a subprocess because the script
`sys.exit()`s on failure (a `SystemExit` that `_run_step` would not catch); non-zero exit → `RuntimeError`
→ recorded step error, with the manifest_inject guard still firing downstream. Caveat:
`regenerate_orbits.py` always exports the DEFAULT `testsets/` corpus (exactly what `manifest_inject`
checks); a non-default `classify_corpus` run is unchanged (pre-existing, not made worse). Witness:
pipeline now 0 errors (was 1 — `manifest_inject` stale), `regenerate_orbits ok [1.3s]`, total time ~8.8s
(unchanged — the regen replaced the error, not added to it).

## 2026-06-20 — within-kernel trifurcation router built + wired (OQ-55 resolved; OQ-53 within-kernel leg closed)
**Files:** prolog/cs_trifurcation.pl, prolog/json_report.pl, prolog/tests/test_cs_trifurcation.pl, prolog/stack.pl, ISSUES.md
**Tier:** landed

New module `cs_trifurcation.pl` (`cs_reading_trifurcation/3`) routes *why* a kernel's readings disagree
into the `debugging_philosophy.md` §6 trifurcation, **within-kernel only**. Dispatch on the authored
obstruction edge (`cs_kernel_obstruction_status/2`), refined by two computed within-kernel diagnostics:
`real_closure`→Type B (confirmed/edge_only via `cs_axiom_foreclosed`), `licensed_plurality`→Type C,
`untyped`+`cs_drift_unacknowledged`→Type A, `untyped`+no-drift→`unknown` (Pattern-5 fail-closed, NOT a
default), `singleton`→no verdict. Live consumers: (1) `reading_trifurcation` field in `json_report.pl`'s
`cs_kernel_comparison` (`scope:within_kernel` stamped inline; **commentary-grade**, never overrides
classification) — survives the enrich step into `enriched_pipeline.json`; (2) `enhanced_report.py`'s
`build_kernel_reading_section` renders a `Reading disagreement: <type> [within_kernel; obstruction=…, …]`
line in the human report (added 2026-06-20 follow-up — the field reached enriched_pipeline.json but was
unrendered; Pattern-1 second-wire closed). Wired into `stack.pl`.

**§6 mapping confirmed against the definitions** (not the table paraphrase): Type B = "impossible by
definition" = `forecloses`; Type C = stable coexisting frames = `coexists_with`; Type A = unmarked
mutation treated-as-stable = the `false` (unacknowledged) flag in the drift gap. Type A is the **sole
computed branch**; two layered controls hold obstruction at `untyped` and vary only the drift signal:
(1) two-twin (`tk_drift` vs `tk_nodrift`) — drift signal is the discriminator, not obstruction riding
along; (2) **single-bit** (`tk_drift` vs `tk_drift_ack`) — direction + magnitude held identical (checked
by in-test unification), only the `acknowledged` flag flips false→true, and the verdict flips
`type_a_drift`→`unknown`. Isolates the unacknowledged bit specifically (side-by-side pasted in the OQ-55
follow-up turn, 2026-06-20).

**Re-scope ruling (operator, 2026-06-20):** OQ-55 was `blocked_on OQ-56` — a *soft* block. The
within-kernel router needs no cross-kernel vocabulary; OQ-56 gates only OQ-53's transpose leg. Edge
dropped. **Re-scope witness = input-boundary trace:** every router input is gated by `cs_kernel_id(_,K)`,
so no cross-kernel fact enters the verdict (traced on `tk_drift`).

**Draw-robustness transfer caveat:** the 0.734 twin-agreement on the obstruction-class orbit was
measured *cross-kernel* (OQ-150). Its transfer to within-kernel use here is **inferred**, and is
discharged by the input-path trace (the router reads only per-kernel/per-member facts), NOT by that
number — the number describes a different (cross-kernel) measurement.

**Witnesses.** `test_cs_trifurcation.pl` 8/8 green (4 branches + singleton negative + two-twin
discriminator + cross-kernel-leak control). Live corpus (`run_pipeline.py`, all 9 multi-reading kernels
non-null): `type_a_drift`×5, `type_b_structure`×1 (`jewish_sovereignty_palestine`), `type_c_ambiguity`×2
(`press_reformation_causation`, `zero_mathematical_status`), `unknown`×1 (`polaris_document_status` —
fail-closed fires on real data). OQ-55 resolved; OQ-53 within-kernel leg closed, transpose leg stays
`blocked_on OQ-56`.

Note: the pipeline's `manifest_inject` step errors on `product_site_orbits.json` staleness (corpus_hash
mismatch, OQ-29) — pre-existing, orthogonal to this change (neither modified file references it).

## 2026-06-20 — kernel/reading orbit operator built + wired (OQ-150/OQ-53 Phase 3)
**Files:** python/orbit_operator.py, prolog/kernel_orbit_export.pl, python/run_pipeline.py, outputs/reading_orbits.json, outputs/kernel_orbits.json
**Tier:** landed

The cross-kernel orbit operator (commit `0c488468`). `orbit_operator.py` reads the canonical
`pipeline_output.json` (6 keys: observer-signature, terminal-observer/committer, apparatus,
seat-vector, grounding) + `kernel_obstruction.json` (the 2 keys not serialised in
pipeline_output: obstruction-class + grounding, produced by `kernel_orbit_export.pl`) → writes
`outputs/{reading,kernel}_orbits.json`. Wired into `run_pipeline.py`: `kernel_orbit_export` in
`_phase_post_prolog`, `orbit_operator` after `w1_sheaf_join` (dependency-ordered, non-critical).

**Two tripwires for a fresh agent:** (1) the operator's LIVE output is **sparse by design** — the
live corpus has ~3 multi-reading kernels, so `reading_orbits.json`/`kernel_orbits.json` on a live
run look near-trivial; the meaningful orbit populations are on the TWINS (run
`python3 python/orbit_operator.py --twin haiku`). Do NOT read sparse live orbits as a bug or as
"orbits don't form." (2) Per operator ruling 2026-06-20, only Tier-1 keys (observer-signature
0.722, obstruction-class 0.734) are declared draw-robust; Tier-2 keys carry their twin-agreement
number INLINE on every orbit record and are model-relative — do not cite a Tier-2 orbit membership
as a stable finding. Same-run guard: `orbit_operator` drops `kernel_obstruction.json` to
`source_missing` if its `n_constraints` ≠ the pipeline manifest (fail-closed; positive-controlled).

## 2026-06-20 — orbit-key declarability: judge against the extraction baseline, NOT the permutation null
**Files:** audits/2026-06-20_kernel_reading_orbits/, ISSUES.md
**Tier:** correction-key

OQ-150 cross-twin orbit measurement (8 keys, haiku/flash n=960; `phase1_orbit_keys.py` +
`phase1b_agreement.py`; controls pass — `claimed_type` 0.7208, K1 reproduces 2026-06-18 M3
0.134). **Citation correction:** a key clearing the permutation `band95` (`lo>band95`) means
*beats random labels*, NOT *draw-robust enough to declare as a vocabulary*. All 8 keys beat
chance; only 2 reproduce at the **extraction baseline (~0.72, the substrate's own
reproducibility — the natural floor)**: `kernel-obstruction-class` (0.734) and
`observer-signature` (0.722). The other 6 are above-chance but membership-fragile (0.13–0.57).
Judge orbit-key declarability against the baseline, not the null. The plan's `lo>band95` gate
under-operationalized the **reproducibility** filter the plan's own Context elected — applying
the baseline honors that election, it is not a retroactive switch.

Two substantive Ω_E findings: (1) **committer axis is fragile FINE but reproducible COARSE** —
apparatus/grounding model-relative (0.49/0.27) yet the 4-way obstruction verdict reproduces
(0.734); granularity governs declarability, not axis. (2) **apparatus orbit is gradient-orthogonal
to observer** (normalized MI 0.063, Theorem 7) — genuine second axis, keep separate. OQ-53
report-path witness: kernel is first-class in `cs_kernel_registry.pl` + `json_report.pl`
(`cs_kernel_comparison`), prefix-opaque only in `logical_fingerprint.pl`. Two operator picks
reserved (OQ-56 vocabulary; OQ-53 committer-transpose disposition); empty-menu kill did NOT fire.
Commits `b07e84f1`, `17dba90e`, `0fdc9d7a`.

## 2026-06-19 — the orbits-staleness warning is EXPECTED after every c-orchestrator run (not a bug)
**Files:** python/run_pipeline.py, python/sweeps/regenerate_orbits.py, agent/c-orchestrator.py
**Tier:** history

A c-orchestrator topic run grows `prolog/testsets/`, so `outputs/product_site_orbits.json`
(the perturbation-sweep baseline, regenerated only by `regenerate_orbits.py`) is stale by
construction the moment generation finishes. The `manifest_inject` step's
`check_orbits_corpus_hash` (`run_pipeline.py:1133`) then raises `RuntimeError: product_site_orbits.json
is stale: corpus_hash … != current …` — this is **non-critical**: `_run_step` catches it, the
manifest is already stamped (injected before the check), and the pipeline reports `42/43 steps OK`.
Do NOT re-diagnose this as a pipeline failure. The live classification path is unaffected — it
runs on `orbit_data.json`, which IS regenerated each pipeline run (`Matched orbit data for N/N`).
Only the sweeps (`perturb.py`, `product_site_delta_sweep.py`, …) consume the stale `product_site_orbits.json`;
run `python3 python/sweeps/regenerate_orbits.py` (atomic swipl export + hash stamp) before a sweep
that needs it. Operator ruling 2026-06-19: keep orbits DECOUPLED, regen on demand — deliberately
NOT wired into the orchestrator (the export is expensive and most topic runs never sweep). Lineage: OQ-29.

## 2026-06-19 — the engine's "H1" is a disagreement tally, not a cohomology rank (citation correction)
**Files:** prolog/grothendieck_cohomology.pl, ISSUES.md
**Tier:** correction-key

`cohomological_obstruction`'s H1 = `count_disagreeing_pairs` — by its own comment a *"Cech
1-cocycle proxy"*, the count of disagreeing context-pairs (range 0..6 = C(4,2)). It is NOT
dim H¹ / a Betti number. Witness: a role-gauge `[naturalized,snare,snare,snare]` gives tally 3,
but the first Betti number of that disagreement graph (star K₁,₃) is E−V+C = 3−4+1 = 0. **H⁰
(global section ⟺ all contexts agree) is legitimate; "H1" is a contextuality/disagreement count.**
Caught by a three-model review (the counterfeit-rigor register). Do not cite `H1` /
`contextuality_fraction` (=H1/6) / `sheaf_status` as cohomology results without that caveat — they
are a disagreement tally over a 4-point site with no overlapping cover. A real Čech H¹ needs a
nerve with overlaps (reading_diff's vantage alignment is the candidate). Lineage: OQ-151, OQ-51.

## 2026-06-19 — schema: conditional stakeholder-coverage gate (the false-negative root cause)
**Files:** schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md
**Tier:** landed

Diagnosis (OQ-149; audits/2026-06-18_oq56_*): 423/466 haiku no-stakeholder stories had authored
beneficiaries/victims (obvious parties) yet emitted no `constraint_stakeholder`. Root cause =
contradictory signals: the generation prompt prose marks stakeholders REQUIRED, but the schema
omitted it from `required` AND its field description said "optional alongside perspectives during
the A/B perturbation" — the proximate signal for structured generation, so the weaker model
(haiku) dropped it; flash wrote it on 75% of the same slots. Not truncation (six_questions, a
later field, survives more) and not surface-substitution (ben/vic co-occur with stakeholders).

Fix (commit `becd0f87`): CONDITIONAL `allOf` gate — if `base_properties.beneficiaries` or
`victims` non-empty → require `stakeholders` (minItems 1); a true mountain with no parties
(gravity) stays EXEMPT. Description rewritten to state the contract. **Forward-only** — gates
new/regenerated stories; existing corpus untouched (no consumer re-validates committed stories;
no test validates `json/`). Witnessed Draft7 (the pipeline's validator): example still validates,
example−stakeholders now CAUGHT, gravity exempt. **The prompt-prose reinforcement is the
operator's edit** (driving the c-orchestrator loop). Schema is the binding gate.

---

## 2026-06-19 — reading_diff un-stranded onto the live stakeholder-seat schema + stale test corpus
**Files:** prolog/reading_diff.pl, prolog/tests/test_reading_diff.pl, ISSUES.md
**Tier:** landed

`reading_diff:reading_cells/2` read only authored `constraint_classification/3`, which the
de-leak rebuild stopped authoring — every within-kernel pair on the live/twin corpus read a
vacuous `robustly_undersampled` (OQ-56 D1). Fix (commit `01cff6a7`): `reading_cells/2` now
UNIONS two cell-sources, mutually exclusive across corpora — authored
`constraint_classification/3` (archives) and `stakeholder_seats:stakeholder_context/3` +
`dr_type_for_stakeholder/3` (live; same `context/4` tuple, so alignment keys untouched).
Witnessed: haiku census 0/0/954 → 136 binocular / 111 fragile / 707 (now MEASURED) coverage
gaps; non-regressive on `kernel_v1` (0 stakeholders → clause inert; suite 10/10 pass with
archive overlaid). Twin both-stakeholder pair coverage: 26% haiku / 61% flash (model-asymmetric
stakeholder authoring — folds into OQ-149).

**Tripwire:** `prolog/tests/test_reading_diff.pl` fixtures are **pre-reset westphalia readings**
absent from the live `testsets/`; running it the documented way (default corpus) shows **7/10
FAILED** — a corpus-overlay artifact, NOT a reading_diff bug. Run it with the archive overlaid:
`swipl -g "asserta(config:param(corpus_path,'archives/datasets/kernel_v1')), [stack],
corpus_loader:load_all_testsets, [tests/test_reading_diff], run_tests(reading_diff), halt"`
→ 10/10 pass. (Stale-fixture repointing is unfiled — candidate OQ.)

---

## 2026-06-18 — OQ-147 crash floor + OQ-148: classifications regression (corpus-wide producer break)
**Files:** python/audits/sheaf_audit.py, python/audits/tests/test_sheaf_audit.py, ISSUES.md
**Tier:** landed

**OQ-147 (loud, resolved).** `sheaf_audit.py:515` raised `ZeroDivisionError` because its working set
(constraints with ≥2 of the 10 Tier-1 slice contexts) is empty. Fixed with one `insufficient =
(working_set_size == 0)` predicate reused on three surfaces (markdown early-return, console one-liner,
JSON null rates + `verdict: insufficient_data`); verdict single-sourced via `_verdict()` so JSON and
markdown can't drift, happy-path bands byte-identical to old 464–471. A naive `if n_total else 0.0`
was rejected — it sets `crossing_rate=0.0` → the `== 0.0 → "PRESERVED (zero crossings)"` branch,
making empty indistinguishable from measured-flat (Pattern 5/6). New fixture
`python/audits/tests/test_sheaf_audit.py` (4/4 PASS) pins the empty-case markdown + the
non-self-witnessing `_verdict` string swap. Witnesses: pre-fix crash at :515; post-fix exit 0, JSON
`crossing_rate: null`. Loud crash → stays history (no tripwire).

**OQ-148 (quiet, open — the real bug, candidate tripwire).** Root cause of the empty working set:
`outputs/pipeline_output.json` carries `classifications: []` for **all 80** constraints (2026-06-18),
but committed snapshots prove it populated on 2026-06-11 (46/48 @ 287 entries, 50/52 @ 312 entries) —
a **producer regression** in the intervening week (corpus also reset/regrew 48/52→80, so the break
may be in the data path, not a code commit — falsifier pre-registered in OQ-148). `classifications`
is a declared schema field (`shared/schemas.py:195`) referenced across ~40 python files; `sheaf_audit`
was the only one that crashed loudly. **The Pattern-5 risk is the quiet consumers that absorbed `[]`
into committed outputs reading as measurements** — this blast radius is OQ-148's spine and a
**candidate Critical-Distinctions tripwire** once the true consumer set is characterized. Pointer:
ISSUES.md OQ-147/OQ-148; commit at close.

## 2026-06-18 — OQ-146: orbits metadata-key landmine — single-source `load_orbits_constraints`
**Files:** python/shared/loader.py, python/oracle_gap_analysis.py, python/game_theory_nash.py, python/sweeps/product_site_delta_sweep.py, python/sweeps/structural_config_sensitivity.py, python/tests/alt_power_transform_test.py, python/tests/alt_power_transform_test_3k.py, ISSUES.md
**Tier:** landed

OQ-29 stamping put a top-level `corpus_hash` (a `str`) into `product_site_orbits.json`, a flat
`{id:{…,contexts}}` dict with no metadata namespace. Every consumer iterating top-level keys as
constraints crashed on it ("worked before" = un-stamped orbits had no such key). Census (`git grep
-ln product_site_orbits` + iteration-idiom grep; positive control re-found all 5 known exposures
**and** surfaced `structural_config_sensitivity.py:529`) → 6 exposed consumers. Fix: one fail-loud
predicate `shared.loader.load_orbits_constraints` — **partition-and-assert**: keep dict-with-`contexts`,
drop only allowlisted metadata (`_ORBITS_METADATA_KEYS={"corpus_hash"}`), **raise** on any
unclassifiable top-level key (no silent undercount). All 6 consumers repointed (inline `7b5801f0`
filter in oracle_gap replaced too). **Crash-over-drop ruled safe by producer construction:**
`product_site_export.pl:80–96` emits `"contexts"` unconditionally; key set is a static Cartesian
product (`constraint_indexing.pl:1052`) that never reads the corpus → every entry (live + every
archive) has `contexts`; a top-level entry lacking it can only be metadata/corruption.

**Tripwire (NOT promoted — distinct from a silent mistake; the failure is a loud crash):** anywhere
you iterate an orbits file as constraints, use `load_orbits_constraints`, never raw `json.load` +
`.items()`. When a new top-level metadata key is added, hand-bump `_ORBITS_METADATA_KEYS` AND the
hardcoded literal in the loader's set-equality test (the deliberately-unshared literal is the
tripwire proving the metadata set was consciously expanded). Witnesses: ISSUES.md OQ-146 (set-equality
75 vs raw 76; partition-assert raises naming `junk`; orbit_data.json no-op 75; per-consumer two-sided
all yield exactly 75; oracle_gap + game_theory_nash run end-to-end). Out of scope: `sheaf_audit.py:515`
ZeroDivisionError (corpus-size bug, not this class).

## 2026-06-18 — OQ-104: audit_citation_status.py built (standing checker, ungated)
**Files:** python/audit_citation_status.py, ISSUES.md, audits/2026-06-18_oq104_citation_checker/
**Tier:** landed

New `python/audit_citation_status.py` — sibling of `issues_status.py`/`known_state_status.py`;
verifies every path cited from `audits/*.md` exists-AND-tracked OR is allowlisted-ephemeral
(the fresh-clone invariant). **NOT in `scripts/gate.sh`** (ungated until FP rate is ruled).
Three WARN sublabels, three destinies: `untracked-pending` (`--promote-untracked`),
`missing-pending-M` (`--promote-missing`), `grammar-ambiguous` (never promotes). A gitignored
path inside the repo root is **never** allowlisted — it IS the OQ-104 signature.

Census: 1224 citations/85 dirs. **untracked-pending = 35 distinct, all `outputs/*`** — all
descriptive references to canonical regenerable outputs (schema docs, CLI defaults, command
lines), none the dangerous frozen-evidence class. **Operator ruling 2026-06-18:** leave
flagged, non-gating; copy-into-audit-dir inapplicable (outputs/ regenerated → faith-merge),
allowlist forbidden; `--promote-untracked` deferred. **missing-pending-M = 66 distinct** (drove
278 plan-upper-bound → 66; every survivor classified as relocation/illustrative/archive-shorthand/
deleted-output — no live broken citation). Controls: `controls.py` 23/23 (caught a `/etc/passwd`
field-list bug), `controls_run.sh` idempotence + rot-sensitivity (pass→flag on `git rm --cached`).
Promotion conditions + brace/glob + descriptive-outputs seats recorded as wiring triggers.
Evidence: `audits/2026-06-18_oq104_citation_checker/FINDINGS.md`. OQ-104 stays **open**.

## 2026-06-18 — OQ-29 RESOLVED: corpus_hash single-sourced; 14 producers stamp; consumers fail-closed
**Files:** python/corpus_hash.py, python/run_pipeline.py, python/enhanced_report.py, python/sweeps/perturb.py, python/sweeps/census_sweep.py, python/sweeps/persistence_sweep.py, python/axiom_reachability.py, python/sweeps/epsilon_sensitivity.py, python/audits/metric_audit.py, python/audits/sheaf_audit.py, AGENTS.md, ISSUES.md
**Tier:** landed

The corpus staleness fingerprint was a **Pattern-2 silent fork** — four byte-identical
`_compute_corpus_hash` copies (`perturb.py`, `run_pipeline.py`, `census_sweep.py`, + the
perturb-imported copies). The plan's census found 2; grep found the 3rd (`census_sweep.py`).
Consolidated into `python/corpus_hash.py` (`compute_corpus_hash` + fail-closed
`assert_corpus_current`); identity witness = every path `d2b3ec9429f1` on current `testsets/`.
Commits `b6aefb5a` (A), `4ab980ff` (B/C), `7b016978` (D).

- **10 producers self-stamp** (Thread B): the 9 plan-listed sweeps + `persistence_sweep` (a 10th
  the plan missed — it produces `persistence_results.json`, consumed by `enhanced_report`). Also
  fixed `persistence_sweep.py:32` standalone-import crash (`parents[2]`→`parents[1]`).
- **Consumer guards fail-closed** (Thread C): `run_pipeline.check_orbits_corpus_hash` upgraded
  presence-only → match (closes the residual: a stale-but-stamped orbits file used to pass);
  `enhanced_report.build_persistence_section` surfaces STALE/WARNING; `persistence_sweep` warns on a
  stale bifurcation input. Three-sided witness: match=pass, mismatch/absent=raise, no-file=pass.
- **Thread D, set-level discipline corrected the plan twice:** plan said "5 dead orphans, none
  cited." A set-level doc-citation probe (positive control: flags v3 + bifurcation) showed only 2
  are clean deletes (`config_sensitivity_results_test`, `structural_config_sensitivity_results_original`
  — deleted). Two others (`alt_power_transform_results_3k`, `test_battery_results`) have LIVE
  write-only test producers in `python/tests/` (no reader anywhere) → kept, excluded as a class
  (one runs vs the 3k ARCHIVE, so a testsets-keyed stamp would be wrong). One
  (`config_sensitivity_results_v3`) is doc-cited → kept + annotated. Pre-reset annotations added to
  `project_orientation.md`, `config_sensitivity_v3.md`, `CONFIG_SENSITIVITY.md`, **AGENTS.md** (the
  set-probe caught a third live-framed site the plan's "only two" missed).
- **Residual CLEARED (→ resolved):** the 4 scoped-out producers now stamp (`axiom_reachability`,
  `epsilon_sensitivity`, `metric_audit`, `sheaf_audit`; runtime control = `sheaf_audit_results.json`
  freshly stamped). The Fisher consumer (`enhanced_report.py:1903`) is guarded — stale/absent-hash
  `epsilon_sensitivity_results.json` surfaces STALE, never renders pre-reset numbers (four-sided
  witness). Audit-script ruling settled by probe (NOT defaulted): both load live
  `pipeline_output.json`/`orbit_data.json`, so a testsets-keyed stamp is the correct identity.
- **Two pre-existing bugs surfaced while exercising (NOT OQ-29, not fixed here):**
  `sheaf_audit.py:515` ZeroDivisionError (empty working set on the small post-reset corpus);
  `oracle_gap_analysis.py:143` `entry["contexts"]` indexed on a string.

**Promotion test:** the standing convention ("new producers stamp `corpus_hash` via
`corpus_hash.py`, never re-define the body; archive runs stamp the archive corpus") is promoted to
AGENTS.md (Config sensitivity sweep §); not CLAUDE.md (not a silent-mistake tripwire before editing a
named file — it's a build-time convention for NEW producers).

## 2026-06-18 — OQ-115 RESOLVED: abductive_helpers phantom under [stack] fixed; check_stack back to 4-finding baseline
**Files:** prolog/stack.pl, prolog/signature_detection.pl, prolog/check_stack.pl, ISSUES.md (OQ-115, OQ-142/143/144/145)
**Tier:** landed

Under bare `[stack]`, `signature_detection:signature_grade/2` (`signature_detection.pl:1624`)
called `abductive_helpers:known_override_signature/1` where the module was a phantom
(`current_module` TRUE, `module_property(_,file(_))` FAILS) → existence_error. The pipeline
was unaffected (loads it via json_report → diagnostic_summary), so the green B4 gauntlet hid
it; the OQ-98 alert path minted the reference after the 2026-06-04 baseline, making it the one
check_stack regression. **Fix:** `:- use_module(abductive_helpers, []).` in `stack.pl`
(`check_stack.pl:27` is `:- [stack].`, so the checker's image picks it up). **Option 1
rejected by evidence** — importing in signature_detection cycles tighter than the in-file
comment said: `abductive_helpers → maxent_classifier → signature_detection:constraint_signature/2`
(`maxent_classifier.pl:60`), plus the grothendieck→drl_core arm; the falsified `:1611-1617`
comment was corrected. **Witnesses (cold `[stack]`, corpus-free):** bite-call before → `THREW`
(`existence_error(procedure, abductive_helpers:known_override_signature/1)`); after →
`RETURNS`. check_stack after: no abductive line; 4 documented baseline findings.

**Class sweep (operator expansion):** partitioned all 4 remaining baseline findings, each with
its own pasted non-bite witness (none inherits baseline-trust). Discriminator = **phantom ×
guarded × reachable**: a reference bites only when target-absent at the call's load chain AND
unguarded AND reachable. OQ-115 was the only unguarded bite. `validation_suite:test_case/4` =
the guarded negative control (then-arm under `current_predicate/1`; else-arm doesn't reach it).
`data_repair:constraint_{beneficiary,victim}/2` = xref mis-attribution of `acc_has/2`'s
`narrative_ontology:Fact` goal-call into a dynamic/multifile target (`fails_clean`, not a
throw). `drift_events.pl:175` = a real latent OQ-57-class wrong-qualifier (`narrative_ontology:`
should be `domain_priors:`; the OQ-57 fix patched the sibling `:236`, missed `:175`) held off
only by being unreached. Tracked as **OQ-142** (parent) + **OQ-143/144/145** (the plan's
`142a/b/c`; renamed because the tracker label grammar is `OQ-\d+` — lettered sub-IDs are
invisible to `issues_status`/`omega_resolver`, witnessed). **Promotion test:** stays history —
the failure is a loud `existence_error`, not a silent miscompute.

**OQ-145 RESOLVED same session (the one code change of the sweep):** `drift_events.pl:175`
wrong qualifier `narrative_ontology:` → `domain_priors:` (mirrors the OQ-57 sibling fix at
`:236`). Reachability control-backed before landing: static unreached (probe 0, positive control
`drift_event`=19 fired), runtime-constructed path left explicitly unverified — fix correct
regardless. **Witness (cold `[stack]`, synthetic constraint extractiveness 0.05 / theater_ratio
0.80 to reach `:175`):** before → THREW `existence_error(procedure,
narrative_ontology:requires_active_enforcement/1)` in `context(drift_events:detect_is_piton/1)`;
after → `SUCCEEDED_CLEAN`. **check_stack baseline now 3** (was 4). OQ-143/144 remain annotate-only.

## 2026-06-18 — OQ-111 RESOLVED: dead data_repair omega bridge retired (zero-diff removal)
**Files:** prolog/data_repair.pl, ISSUES.md (OQ-111), docs/design/design_gaps.md (GAP-13)
**Tier:** landed
`bridge_omega_variables_pure/3` keyed its module lookup on the BARE interval id while testsets
declare facts in module `constraint_<id>` — so it always missed and imported zero omegas on every
report run (Pattern 6; OQ-99's wrong-module twin). RETIRED, not fixed: operator ruled
`prolog/archives/datasets/*` out of scope, closing the bridge's only genuine consumer (v3.4-legacy
UNPAIRED testsets; the live corpus is 100% paired and authored omegas already render via
`report_generator.pl:709`/`:776-794`). Removed the predicate + its `bridge_v34_data/2` call + the
now-dead `persist_single(omega_variable(...))` clause (tombstones in-file); also retired a secondary
defect (the /5 branch fabricated type `empirical` for a typeless 5-arity fact). Deferred capability
logged as GAP-13 with the re-introduction recipe. **Witness:** pre-removal probe on
`border_control_legitimacy__freedom_of_movement_primary` confirmed the no-op fired (bare_module FALSE
/ constraint_module TRUE / 5-arity present / 0 imported); removal is behavior-preserving — ZERO DIFF
on three omega-authoring reports across raw `run_scenario` + `enhanced_report.py`; dynamic suite GREEN
(80/0/0); [GATE] GREEN. No tripwire promoted (the bridge is gone; nothing silent remains).

## 2026-06-18 — OQ-48 recalibration-readiness audit: 0 thresholds recalibratable against the twins (all MODEL-CONFOUNDED)
**Files:** ISSUES.md (OQ-48), audits/2026-06-18_oq48_recalibration/, python/audits/oq48_threshold_distributions.py, python/audits/oq48_analyze.py, python/audits/oq48_triangulate_kernel_v1.py
**Tier:** landed

Read-only distribution-break audit of the 7 in-scope χ/ε/suppression classification cuts (config.pl,
691-corpus-provenanced) against the twins (`testsets_haiku`/`testsets_flash` = 960 each). Pre-registered
verdict rule (KDE antimode + bandwidth-robustness + lobe-mass + Dip; cross-twin agreement = validity gate).
**All 7 → MODEL-CONFOUNDED, 0 proposed values, no `config.pl` edit.** Every metric multimodal on both twins
(Dip p=0), but flash's antimodes fail bandwidth-robustness where their locations track haiku's ("soft
agreement, hard disagreement") → no DRIFTED candidate. Two cuts corroborated by haiku alone (`snare_chi_floor`
0.66≈0.666, `snare_epsilon_floor` 0.46≈0.484). Confounded kernel_v1 arm (1106, pre-reset/pre-de-leak,
corroboration-only, never pooled per OQ-26) cross-regime-corroborates `snare_epsilon_floor` (0.46); the rest
uncorroborated. Controls pass (LOADCOUNT 960/960/1106 via asserta; 0 unknowns; byte-identical re-run;
planted-gap recovered 0.4506). **OQ-48 stays open** — closure waits on corpus regeneration beyond the twins
(same-regime third corpus breaking the tie, or the live rebuild reaching the ~700-story Tier-4 bar).
Promotion test: NO — a result qualification, not a silent-edit tripwire; stays here. Provenance: twin TSV
sha256 haiku `7039d37b…`/flash `3c24b1d2…`, metric-code commit `0a629077`.

---

## 2026-06-18 — OQ-122 CLOSED: physics-RED fixed by OQ-128; FSM victim-gate DROPPED, discriminant handed to OQ-138
**Files:** ISSUES.md, prolog/drl_core.pl (witness only, no edit), outputs/pipeline_output.json (witness)
**Tier:** landed

Closed OQ-122. The control-inversion / physics-false-RED that filed it is FIXED by OQ-128's type_1
discrimination, NOT by the held FSM victim-gate: on live (commit `2172d55`, manifest 2026-06-18) both
`radiative_levitation_stratification` and `actinide_replenishment_mechanism_flat_control` read
`verdict_join.verdict=yellow`, `cap_applied:none`, `type_1_false_summit=informational` at every seat.
`false_summit_mountain` still fires (vic=0) but only adds a `signature_correction/moderate` alert while
`base_verdict` is independently yellow — so the gate's verdict benefit is now ≈0.

**FSM victim-gate (`oq122-fsm-victim-gate`, `ab1e9b26`) DROPPED — superseded by the engine-ROUTES-never-
RECLASSIFIES architecture (OQ-128).** The gate is a suppress-the-detector reclassification, the shape
OQ-128 removed; the branch's single-clause diff is recoverable at `ab1e9b26`. Its INSIGHT survives,
re-shaped for **OQ-138**: discriminate the FSM signature's severity (`vic=0→informational/route`,
`vic>0→moderate/floor`) — the exact analogue of the type_1 ε-split — with the pre-witnessed discriminant
(`testsets_flash` 18 vic=0 / 22 vic>0, `audits/2026-06-13_oq122_retype_discriminator/breadth_sweep_results.txt`)
handed to OQ-138 as its FSM-clause build spec. neutron_star/FCR stays under OQ-70. Branch to be deleted
after merge.

---

## 2026-06-17 — OQ-128 type_1 cap RULED + BUILT: discriminated severity (withhold high-ε snare, route low-ε artifact)
**Files:** prolog/drl_core.pl, ISSUES.md (OQ-128)
**Tier:** landed

The type_1 RED-cap ruling (a NEXT RULING after the sink). The type_1 `severe` alert was OVERLOADED:
it fired identically on (a) a mountain-claim the engine degrades to SNARE (high-ε real false summit, a
defect) and (b) degrading to rope/other (the arc's universal non-diagnostic degradation of genuine low-ε
mountains — the same artifact that made natural_law a free retirement). Witnessed clean ε gap in the
mountain-claimed population (snare-at-seat ε≥0.50, rope-at-seat ε≤0.25, nothing between, KILL=0 across six
corpora ~7000). Operator ruled **discriminated severity**: the `dr_claim_mismatch` type_1 clause
(`drl_core.pl`) is split — degrade→snare = `severe` (withhold, RED floor); degrade→other = `informational`
(routes via the sink, no headline floor). **Tripwire:** do NOT collapse it back to a single `severe` — that
re-overloads the alert and re-launders genuine math/physics mountains into RED. Acceptance witness: RED
389→102 across six corpora (287 RED→non-RED), all 10 v5 mountain-claimed snare-at-analytical STAY RED,
`dr_type` byte-identical. **KILL:** a future corpus authoring a mountain-claimed snare-at-analytical at
0.25<ε<0.50 breaks the clean gap → re-run the χ-decomposition. The `severe` of type_3/type_5 is untouched.

## 2026-06-17 — OQ-128 routing sink BUILT (engine ROUTES the author↔engine diff, never reclassifies)
**Files:** prolog/routing_sink.pl, prolog/signature_detection.pl, python/run_pipeline.py, python/enhanced_report.py, ISSUES.md (OQ-128), audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md
**Tier:** landed

The routing-sink design (ROUTING_SINK_DESIGN.md) was built. Three changes:
1. `:867` `resolve_modal_signature_conflict(_, natural_law, mountain)` RETIRED (tombstoned) in
   `signature_detection.pl` — the overwrite that manufactured mountain verdicts. The DETECTOR
   (`natural_law_signature` / `constraint_signature(C,natural_law)`) is LEFT INTACT as a socketed
   router input (unpowered: `HasAlternatives==false` is builder-unreachable). Witnessed behavior-neutral:
   `dr_type` byte-identical (288 rows), `dr_claim_mismatch` byte-identical (52 rows).
2. `prolog/routing_sink.pl` — per-SEAT `seat_diff/7` router. **Seven typed MECE addresses** (operator ruling:
   split, not a catch-all): §4's four (generation_gap / authoring_review / engine_exit_table_review / no_route)
   + `both_silent` / `engine_abstained` / `author_engine_divergence`. No `unrouted_residual`; each
   self-describing. Taps `dr_claim_mismatch/4` UNMODIFIED. Emits `outputs/routing_sink.json` with a coverage
   manifest. **Tripwire:** the leaf is per-SEAT — any predicate collapsing seats to one constraint verdict is
   the KILL condition (§9b.4), the aggregate-merge that recurred 3× in the arc.
3. Wired into `run_pipeline.py` Phase 2 (`routing_sink:run_routing_sink`). **Consumed by `enhanced_report.py`**
   — CONSTRAINT IDENTITY section renders each seat's address per-seat (after "Authored vs Computed").

Controls reproduced the arc's witness files exactly: thermo (clean uniform-mountain) →
`engine_exit_table_review` at moderate/institutional; topological (contested) → `generation_gap`
(moderate, the spec's literal example) + `author_engine_divergence` (institutional) + `authoring_review`
(analytical [mountain,rope]). Address-extension ruling RESOLVED 2026-06-17 (split into 7 typed addresses).
Next rulings (not built): type_1 RED-cap route-vs-adjudicate, FNL/FCR/FSM family, powering the detector
socket (§7).

## 2026-06-16 — Typed-absence corollary added to design canon + OQ-137 (reading-layer census)
**Files:** docs/design/design_discipline.md, ISSUES.md (OQ-137)
**Tier:** landed

Promoted the OQ-121 typed-absence convention from tooling notes to design canon:
`docs/design/design_discipline.md` §5 gains "Typed absence — a reading's silence is itself a
declaration" (corollary of S2/Corollary 2a). A reading an aggregate could consume must return a
typed token (`out_of_domain`/`absence`/measured), never fail silently — NOT "every predicate is
total" (genuinely relational lookups like `in_contention/3` correctly have no reading off-domain).
Templates: `constraint_signature/2`, `q6_cell/2`. **OQ-137 minted** to census the whole reading
layer against the convention (classify each aggregatable predicate total-on-domain / partial-by-
design / silently-failing-defect; fix defects; ideally a standing guard generalizing
`test_seat_totality.pl`). Scope discriminator + diagnostic positive-control requirement are in the OQ.

## 2026-06-16 — `census_sweep.py`: commentary census as a perturb measurement surface + denominator caveat
**Files:** python/sweeps/census_sweep.py, ISSUES.md (OQ-136)
**Tier:** correction-key

New tool pairing the perturb.py overlay method (retract/asserta a `config:param`, run a goal, diff vs
baseline) with the commentary census as the MEASUREMENT SURFACE. Diffs per-source bucket histograms +
`n_in_domain` / `coverage` / `prevalence`. Has a built-in positive control: the null perturbation
(re-apply the baseline value) MUST be inert, else it fails loud (overlay/parse bug). Commentary-grade,
so the sweep is pure observation (never feeds classification). `--param/--to` for one-offs, `--corpus`
to overlay a twin.

**Findings (live corpus, n=72):**
- **CORRECTION-KEY — a census RATE can move purely by domain-shrink.** `tangled_rope_chi_floor`
  0.35→0.85 raised extraction `prevalence` 0.060→0.067 (+12%) while `extraction_blindspot_fired` held
  at **3** — 5 extractive constraints fell out of the domain (`n_in_domain` 50→45). A single
  "prevalence" number reads this as a signal; it is a denominator artifact. **Rule: report raw `fired`
  + `n_in_domain` alongside any rate, or hold the domain fixed — esp. across config/schema-refit/corpus
  comparisons (the OQ-136 clustering test must use raw counts, not rates).**
- q6 `coverage` decomposes: `q6_unmeasured` (authoring) is config-INVARIANT; `q6_signature_unknown`
  (computational, dr_type→unknown) is config-VARIANT (8→10 under the same perturbation). Not one figure.
- The two census surfaces have ORTHOGONAL config-sensitivity: snare ε/χ-floor moves q6 (snare↔tangled,
  both extractive) but leaves extraction inert; only the extractive↔non-extractive boundary moves
  extraction. On this corpus the extractive domain's binding edge is the χ-floor, not the ε-floor.
- `config_validation` bounds the reachable sweep surface: single thresholds can't cross their neighbor
  (`snare_epsilon_floor`<`rope_epsilon_ceiling`; `tangled_rope_extraction_floor`<`…_ceil`). The tool
  records the rejection and continues. Witnesses: `audits/2026-06-16_census_sweep/`.

## 2026-06-16 — Partial-silent commentary predicates totalized (`consensus_provenance/2`, `seat_perceived_vs_real/4`) + OQ-136 minted
**Files:** prolog/stakeholder_seats.pl, prolog/tests/test_seat_totality.pl, ISSUES.md
**Tier:** landed

OQ-121 follow-up: the two remaining partial-silent R3 commentary predicates brought up to the
never-fail convention. Neither has any consumer outside the module (verified — no callers, no tests,
no negation-as-failure), so zero blast radius.
- `consensus_provenance/2`: was silent on `Ns=[]`; now TOTAL with explicit `no_agent_seats`
  (out-of-domain) and `seats_untyped` (absence). Live: plural 37 / no_agent_seats 21 / manufactured 8
  / unanimous 6 (Σ=72) — the 21 no_agent_seats silently failed before.
- `seat_perceived_vs_real/4`: was silent when the per-seat type couldn't derive on an existing seat;
  now returns `Computed = untyped`. Total over 370 live seats; `untyped` branch is a defensive guard
  (0 live triggers). Non-existent seat still correctly has no reading (domain boundary, not silence).
- Regression `prolog/tests/test_seat_totality.pl` 8/8; commentary_census 40/40, oq86 14/14 unaffected.
  Commentary-grade — not on the dr_type path. `mandatrophy_gap` is the last unconverted member.

**OQ-136 minted** (investigation): now that the census reports honestly, its absence/out-of-domain/
unnameable buckets are the first corpus measurements to interpret — 5 `extraction_unnameable`, 20
`q6_unmeasured`, 8 `q6_signature_unknown`, 21 `no_agent_seats`, 8 `manufactured_consensus_candidate`.
Pre-registered test: cluster by generation provenance/run-tag/topic ⇒ authoring artifact (generation
fix); spread + genuinely diffuse on hand-read ⇒ real category (keep/report). Witnesses:
`audits/2026-06-16_partial_silent_totalization/`.

## 2026-06-16 — OQ-121 RESOLVED: totalize the commentary family + domain-relative census coverage
**Files:** prolog/stakeholder_seats.pl, prolog/commentary_census.pl, prolog/tests/test_commentary_census.pl, python/run_pipeline.py, outputs/commentary_census.json
**Tier:** tripwire

A closer look at OQ-121 (operator asked) found a structural issue bigger than the missing coverage
ruling. **The engine already has a never-fail discipline** — correction-grade `constraint_signature/2`
(`signature_detection.pl:136`, explicit `unknown` fallback "instead of a default-fabricated verdict")
and `q6_crosscheck/3` (explicit absence buckets) always return an EXPLICIT token, never fail silently.
The rest of the R3 commentary family never got it: `extraction_reading/2` **failed silently**,
destroying the provenance bit at the source so no aggregate could reconstruct it (Pattern 6 in its
purest form).

**Built:**
- `stakeholder_seats:extraction_state/2` — TOTAL (mirrors `q6_cell/2`): every constraint reaches
  exactly one of `out_of_domain` / `extraction_clear` / `extraction_unnameable` / `extraction_fired(Es)`.
  `extraction_reading/2` now rides on `extraction_fired`, so its fire-or-silent report contract is
  UNCHANGED (oq86 14/14 green; report/sidecar output identical).
- `extraction_unnameable` (extractive ∧ no victim ∧ no nameable extractor) is its own bucket —
  **5 live constraints surfaced that the silent failure had hidden entirely.**
- `commentary_census.pl`: three bucket KINDS (out-of-domain / absence / measured), `coverage` is now
  **domain-relative** (`(n_in_domain − Σabsence)/n_in_domain`), `prevalence` (`fired/n_in_domain`) is a
  DISTINCT number. q6 unchanged (universal domain → 0.611); extraction `coverage 1.0`/50, `prevalence 0.06`.

**TRIPWIRE (the silent mistake a fresh agent makes):** when adding a new `commentary_cell/3` source to
the census, (1) make the per-constraint hook a TOTAL function (return an explicit out-of-domain/absence/
measured bucket — NEVER let it fail; a bare failure collapses out-of-domain, measured-clear, and absent
into one token); (2) declare its out-of-domain buckets — census `coverage` is DOMAIN-relative
(`n_in_domain = n_corpus − Σood`), NOT corpus-relative; a corpus-relative coverage silently claims
coverage of constraints the reading never applied to; (3) coverage ≠ prevalence ≠ corpus-fraction —
keep them separate; (4) a source ships a coverage ratio ONLY if `commentary_coverage_decidable/1` flags
its bucket sets ruled-complete. The full convention + rationale is in `commentary_census.pl`'s header.
`consensus_provenance/2` and `seat_perceived_vs_real/4` are still partial-silent but NOT census sources,
so not a live defect — bring them up to the total shape if/when censused.

Witnesses: `audits/2026-06-16_oq121_totalization/`; plunit 40/40; full resolution in ISSUES.md OQ-121.

## 2026-06-16 — OQ-134 RESOLVED: generic commentary-grade corpus census (`commentary_census.pl` + pipeline wire)
**Files:** prolog/commentary_census.pl, prolog/tests/test_commentary_census.pl, python/run_pipeline.py, outputs/commentary_census.json, outputs/commentary_census.md
**Tier:** landed

New read-only aggregator automating the by-hand q6 census as a kept-fresh pipeline artifact.
`prolog/commentary_census.pl`: a GENERIC commentary census (operator ruling — build the generic
exporter, not a q6-special one). Multifile `commentary_cell(+Source,+C,-Bucket)` hook (one clause
per source), `commentary_absence_bucket/2` (didn't-look buckets), `commentary_coverage_decidable/1`
(absence set RULED-complete → coverage ratio allowed), `commentary_census/2`, `run_commentary_census/0`.
Sources: `q6` (= `stakeholder_seats:q6_crosscheck/3`) and `extraction_reading` (= OQ-86, fired/silent).
`python/run_pipeline.py:_prolog_commentary_census` (Phase-2 task `commentary_census`,
`_PREAMBLE_MARKERS['commentary_census']`) parses the `CENSUS*` lines → `outputs/commentary_census.{json,md}`
with a corpus-identity manifest (n_constraints, corpus_hash, commit). Commentary-grade — own swipl
process, reads only, never on the classification path.

**Key design facts (carried so a fresh agent extending it stays honest):**
- **Sum invariant is the contract enforcer.** Census tallies via `findall` over the BUCKETS (not
  per-constraint `once`), so Python asserts `Σ buckets == n_corpus` AND `n_corpus > 0` per source.
  A non-deterministic `commentary_cell` over-counts (caught), a failing one under-counts (caught) —
  "exactly one bucket per (source, constraint)" is a CONSEQUENCE of the check, not a trusted property.
  The `n>0` clause closes the vacuous `0==0` that a forgot-to-load run would pass.
- **Coverage = "both sides MEASURED," not "landed in a named cell"** — so `q6_unclassified` counts as
  covered (q6 coverage=0.611=44/72; the 28 absent = `q6_unmeasured`(20)+`q6_signature_unknown`(8)).
- **`extraction_reading` coverage ships `null`/N/A, NOT a default 1.0** — whether `extraction_silent`
  is present-residual or didn't-look is UNRULED; a 1.0 we cannot defend is the exact Pattern-6 absence.
  Honesty wired structurally: a source ships a coverage ratio ONLY if `commentary_coverage_decidable/1`
  declares it (empty absence-set ≠ ruled-none). **[SUPERSEDED same day by OQ-121 — see below: extraction
  was totalized, coverage is now 1.0 over its 50-constraint domain, prevalence 0.06.]**
- **Absence buckets are load-bearing (fail-closed control):** pre-stakeholder archives
  (kernel_v1/v5/v6/sotu) route 100% to `q6_unmeasured`, ZERO named cells — the census never fabricates
  a verdict from absence. `q6_unclassified` is `0` on live but corpus-reachable on twins (haiku=1,
  flash=5) — the manifest's corpus identity makes the live `0` self-labeling, never hardcoded.

**Extension point:** a new commentary source is a one-clause `commentary_cell/3` add (+ source/absence/
decidability decls). Future-cheap family: `consensus_provenance/2`, `seat_perceived_vs_real/4`,
`mandatrophy_gap` — no open OQ requests them yet. Witnesses + raw output:
`audits/2026-06-16_oq134_commentary_census/`; full resolution in ISSUES.md OQ-134.

## 2026-06-16 — OQ-86 RESOLVED: `extraction_reading/2` R3 commentary (no-authored-victim blindspot)
**Files:** prolog/stakeholder_seats.pl, prolog/report_generator.pl, python/enhanced_report.py, prolog/tests/test_oq86_extraction_commentary.pl, prolog/data_repair.pl
**Tier:** tripwire

Shipped the OQ-86 reporting feature: `stakeholder_seats:extraction_reading/2` (+ `extractive_type/1`,
`authored_victim/1`), `report_generator:extraction_reading_line/1` (Section 7, beside the q6 crosscheck),
and the `extraction_reading` sidecar (`enhanced_report:extract_extraction_reading`). R3 commentary —
NEVER a classifier input. Fires on the blindspot shape: constraint-level `dr_type ∈ {snare,tangled_rope}`
∧ no **authored** victim ∧ ≥1 beneficiary-side agent seat; names the beneficiary-side seats, flags the
cost-bearer as prose-only. 24/24 plunit (positive + channel + 3 single-var negatives + bridge regression).

**TRIPWIRE (the silent mistake a fresh agent makes):** the data-repair bridge `data_repair.pl:153`
(OQ-93 shim-family) FABRICATES `constraint_victim(C, inferred_subject)` whenever E>0.46 ∧ S>0.40 and no
victim is authored — i.e. on the EXACT blindspot metric profile. So by report time the DB ALWAYS holds a
victim for the very case OQ-86 exists for; a naive `\+ constraint_victim(C,_)` guard is INERT on every
real report (Build Discipline P5/P6 — a fabricated success-shaped token fills the no-victim hole). Any
predicate that means "the STORY authored no victim" must exclude the `inferred_subject` sentinel
(`authored_victim/1` is the template). Witnessed: without the exclusion the end-to-end channel witness on
the blindspot fixture was silent; with it, the line + sidecar emit.

**Empirical (cross-corpus census, witnessed):** fires on 3 live testsets (plan predicted 0 — wrong),
10/960 testsets_haiku, 34/960 testsets_flash — ALL `tangled_rope`, ALL no authored victim. **0** across
kernel_v1/v5/v6/sotu (~5,377 stories): guard C fail-closes (those pre-stakeholder archives have 0 seat
facts; 62 kernel_v1 constraints pass guards A+B but cannot name extractors → silence, correct). EVERY real
firing is `tangled_rope` — omitting it from `extractive_type/1` would make the feature 100% inert on real
corpora (snare never fires outside the constructed fixture). Sets the table for OQ-134 (uniform sidecar shape).

## 2026-06-16 — Seat/orientation invariant audit + v8 "seat/gauge/orientation" design spec (engine votes one-seat)
**Files:** docs/design/v8_seat_gauge_orientation_design_spec.md, audits/2026-06-16_seat_invariant_vs_prolog/, docs/seat-theorem-v1.md, docs/deferential_realism_paper_v7.md
**Tier:** landed

Read-only seat/orientation invariant audit ran (REPORT.md + evidence; merges `c58611a8`/`864c961d`):
per-prediction verdicts P1–P9 + theory-killers, conditional-decision-tree synthesis (no net vote).
Headline R3 ("genuine second seat?") settled by a pre-registered **presentation-vs-structure** probe
(`evidence/probe_r3_presentation_vs_structure.pl`, merge `77e33bca`): `cs_pattern`/`cs_classify`
(cs_pattern_detection.pl:108–169) is a **pure function of authored presentation** (`cs_kernel_codification`
+ `cs_authority_grounding`), **blind to binding structure**; the `cs_verdict` false-X layer audits the
presentation against authored metric/beneficiary reality, **one-directionally**. → **engine votes ONE
seat**; the committer/CS axis is the **orientation (showing) face**, not a second content-seat. The R3
*declaration* is the operator's seat — evidence supplied, not ruled.

**v8 design spec** drafted through rev3 (`docs/design/v8_seat_gauge_orientation_design_spec.md`; merges
`403375e4`/`f6c22b81`/`1e81bc0f`): unifies `seat-theorem-v1` (law) + v7 (two-axis realization) + the CS
engine (mechanism) under **seat / gauge / orientation**; seat/face line drawn by **audit direction**;
standing invariant = a **transitive cross-axis taint property** (no committer field reaches observer
computation by *any* path except entailment-typed payload on the single forward `influences` bridge);
kill-condition = any other committer→observer *computation* path (reverse bridge / payload widening /
(B)-seam promotion). **Spec is a draft FOR REVIEW, pre-implementation.**

**TRIPWIRE (soft now; hard on v8 adoption):** v7's word **"seat" = v8's "gauge"** (an observer position);
v8's **"seat" = v7's ε-invariant content**. Reading "seat" across v7 and the v8 spec without the spec's §4
bridge table miscounts them as two content-seats (the error that produced the discarded two-seat hypothesis).

Two related docs added by the operator (web instance), **untracked**: `docs/one_seat_audited.md` (the
One-Seat *verification corollary*; superseded the two-seat draft) and `docs/provenance_is_not_proof.md`
(investigative essay — **NOT for commit** per the finished-essay convention; names a living person under a
defamation check → only *structural* claims may migrate to v8, with the intent-humility framing).

**NEXT STEP (needs an operator-authored OQ — `Priority:` is the operator's seat):** adopt v8 (rule the spec's
Q4 vocabulary) → a fresh CC instance plans implementation from spec §8 (priority-1 = promote the transitive
taint guard to a checkable **dataflow** guard with the two positive controls; then the low-stakes vocab
migration). Blocked on operator adoption + web review.

## 2026-06-16 — Orientation is a deferred Ω_E, NOT Ω_P (OQ-133 relabel) + verification-depth discipline
**Files:** ISSUES.md, docs/technical/build_discipline.md, CLAUDE.md
**Tier:** correction-key

**The relabel.** OQ-133 was filed `Ω-type: Ω_P (orientation)`. Resolved against
`docs/omega_variables.md`'s own definitions, that is wrong: Ω_P is a value judgment that differs
*legitimately across stakeholders* (resolved by those bearing the cost deciding); **orientation**
(a concealment's enclosure vs survival vs defense) is a **fact about the actor's actual stance** —
observers differ in *access*, not legitimately in *values* — whose named resolution operation is
*world-observation* (the longitudinal Cor-3 confrontation-response signature = the paradigm Ω_E
operation). So orientation is a **deferred Ω_E**, status: awaits the t0-anchor tier (OQ-133 itself).
**Why it was load-bearing, not taxonomic:** the Ω-type field routes resolution — `Ω_P` routes to
"someone bearing the cost declares it," which licenses the encloser to **self-certify as a defender
by fiat** (the concealment move blessed by the routing); `Ω_E` withholds that license, forcing the
verdict to be earned from the honor/reabsorb pattern. **Boundary (the Ω_E claim's falsifier):** the
signature tracks orientation only absent strategic gaming (a sophisticated encloser can *perform*
honoring, forging the longitudinal witness); under gaming it falls **outside the framework entirely**
(`omega_variables.md` Mechanism Boundaries exclude strategic gaming) — Ω_E in the non-gaming regime,
out-of-framework under gaming, **never Ω_P.** Do NOT collapse this with `contested_open` (rule 11),
which IS a genuine Ω_P/Ω_C (legitimate dispute about the founding problem; engine abstains): same
surface OPEN, opposite type/operation (route-to-deferred-measurement vs abstain-as-preference).

**The discipline added (CLAUDE.md synthesis-list (5) + `build_discipline.md` → *When to stop
verifying*).** "Verified enough" is a seat with no floor (`seat-theorem-v1.md` §8); the checkable
substitute is the conceal-an-open check: for each verdict/name, name a tier-available falsifier or
**downgrade to OPEN = route to a typed Ω**. This is `omega_variables.md`'s structural-convergence
stopping rule — but cite its **cost-benefit** line, not the stable-marriage terminus, because the
verification regress is *generative* (manufactures new dials), which the doc's Mechanism Boundary
excludes. Provenance: the `q6_crosscheck` review arc; the confident "Ω_P" was itself a concealed
open this check would have caught.

---

## 2026-06-16 — R5 Q6 synchronic crosscheck completed: `q6_crosscheck/3` replaces `zombie_piton_crosscheck/2`
**Files:** prolog/stakeholder_seats.pl, prolog/report_generator.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, python/linter.py, ISSUES.md
**Tier:** landed

`stakeholder_seats:zombie_piton_crosscheck/2` (single dead×piton cell) is GONE — replaced by
`q6_crosscheck(C, Cell, Daylight)`, the full status×computed-signature matrix. Commentary-grade
(NEVER overrides `dr_type`; sole caller `report_generator.pl:r5_zombie_crosscheck_line/1`, NOT in
json_report's per_constraint path so classification is byte-identical by construction). A fresh
agent grepping for the old name or the old `corroborated_zombie` verdict will not find them —
loud failure (predicate absent), so this is history, not a CLAUDE.md tripwire. Four non-verdict
buckets kept distinct: `q6_unmeasured` (authored absent) / `q6_signature_unknown` (computed
absent) / `q6_unclassified` (present, fell through — mountain/scaffold/naturalized × live/dead) /
out-of-domain → lint fail-loud. `q6_cell` is a mode-robust if-then-else (computes into a fresh var,
unifies last) — a multi-clause first-match with an unguarded catch-all let `q6_crosscheck(C,
q6_unclassified, _)` spuriously match all 71 (caught by its own positive control). Witness:
`dr_type/2` = `default_context` = analytical (constraint_indexing.pl:156–161); `q6_unclassified`
WITNESSED 0 on live corpus but reachable on twins (haiku=1, flash=5, all `live × mountain`).
Daylight axis (`founding_problem_corroboration_class/2`, authored atom, lint-gated) SHIPS INERT —
all stories `daylight(unstated)` until a bounded R5 backfill lands (OPEN graduation step). Audit:
`audits/2026-06-16_q6_crosscheck_completion/`. Tracking: OQ-83 follow-through; deferred diachronic
(confrontation-response) tier → OQ-133.

---

## 2026-06-16 — `python/paths.py` is the canonical path source (depth-agnostic); 3 absolute-path bugs fixed
**Files:** python/paths.py, python/domain_priors_expander.py, python/sweeps/range_sweep.py, python/tests/diff_cut_proof.py, AGENTS.md, ISSUES.md
**Tier:** tripwire

New code MUST import filesystem roots from `python/paths.py` (`REPO_ROOT, PROLOG_DIR, TESTSETS_DIR,
JSON_DIR, OUTPUTS, SCHEMAS, PROMPTS, ...`) — never re-derive with `Path(__file__).parents[N]`
(depth-fragile) or hardcode `/home/...`. Root detection walks up to the `pyproject.toml` marker
(depth-agnostic; survives worktrees where `.git` is a file, and tarball/CI). Nested scripts use the
byte-identical bootstrap in AGENTS.md §3 (same sentinel walk → copy-safe from any depth). Fixed the
3 files that hardcoded `/home/scott/...` (domain_priors_expander, sweeps/range_sweep,
tests/diff_cut_proof). Witnessed: paths.py resolves == the old hardcoded values; bootstrap finds the
same root from 6 depths. ~69 scripts still re-derive inline — migrating them + the
package-vs-`paths.py` decision is OQ-132 (held; do not bulk-migrate before A-vs-B is ruled).

## 2026-06-15 — OQ-131 Q1 (Ω_E) measured: 6-vs-4 observer site is consonant-suppressing, NOT a combinatorial artifact
**Files:** prolog/constraint_indexing.pl, prolog/config.pl, prolog/config_schema.pl, prolog/config_validation.pl, python/audits/oq131_six_observer_probe.py, audits/2026-06-15_oq131_six_observer/, ISSUES.md
**Tier:** landed

Added three **additive** observer site modes to `constraint_indexing:site_contexts_for_mode/2`
(commit `a06b5c7f`): `canonical_6` (canonical 4 + powerful/organized seats), `power_only_4`,
`power_only_6`. First-arg indexed, **no catch-all**, so `canonical`/`product` resolve byte-for-byte
as before. New seats are appended AFTER the canonical four ⇒ the 6 canonical observer-pairs stay
positional and the entire 4→6 H¹ delta is the 9 new pairs. **Tripwire:** the canonical-first
ordering is load-bearing — it is what makes the `(H¹₆−H¹₄)/9` headline conditioning valid and was
witnessed (9-pair basis PASS for every constraint, all three corpora). Don't reorder.

Seat bundles are **declared-revisable** `config.pl` params (`observer_bundle_powerful` =
logic.md:530 elite perspective; `observer_bundle_organized` revisable; `observer_baseline_tes` =
moderate canonical coords for the single-coordinate control). These are **compound** terms — I added
a `type_ok(compound, V)` clause to `config_validation.pl` and `compound` to the `config_schema.pl`
type vocabulary + three `param_spec`s; **every config param needs a schema spec or `[stack]` halts
at load** (witnessed: 3 "no schema spec" errors before I registered them).

**Finding (`audits/2026-06-15_oq131_six_observer/`, pre-registered):** observed `(H¹₆−H¹₄)/9` falls
BELOW the permutation band (N=1000, seed=20260615) on live (0.446 vs [0.741,0.825]), haiku (0.562 vs
[0.738,0.755]), flash (0.550 vs [0.754,0.775]) → **consonant-suppressing**; the new seats echo the
canonical four more than chance (`echoes_both` 82/69/62%). The combinatorial artifact is FALSIFIED.
Power-atom-driven (power_only ≈ headline), bundle-robust within the sweep envelope; twin model gap
0.012 on the 873 non-grid matched stratum (grid census 87/0). Exchangeability gate PASS (dr_type pure
fn of C). ISSUES.md OQ-131 stays `future` (Q2/Ω_C corpus-adoption deferred); Q1/Ω_E folded in.
**Scope walls:** H⁰/H¹ only (subobject-classifier on a larger site stays OPEN); finding is
seat-bundle-dependent, not "the 6-point cohomology of this corpus."

---

## 2026-06-15 — OQ-108 resolved: per-position witness coverage shipped; OQ-107 closed `future`; new `future` status token
**Files:** prolog/stakeholder_seats.pl, prolog/json_report.pl, python/tensions_ledger.py, python/issues_status.py, ISSUES.md
**Tier:** landed

Witness coverage over the **6-atom authoring power vocabulary** (powerless/moderate/powerful/
organized/institutional/analytical, `docs/logic.md:293`) — distinct from the **4-position
observer fingerprint** (`logical_fingerprint:fingerprint_shift/2`; `powerful`/`organized` have
π and canonical-d but no `standard_context_for_power`, hence no perspective column). New:
`stakeholder_seats:power_witness_count/3` + `power_witness_map/2` (reuse
`constraint_indexing:canonical_d_for_power/2` as the 6-atom enumerator, no forked list);
serialized as `perspective_witness` in `json_report.pl` (64/64 constraints); rendered in the
tensions ledger. A 0 = that perspective is inference-only, NOT measured-absent (Pattern 6: zeros
SHOWN). Witnessed: `geopolitical_settlement_competition` types `powerless=tangled_rope
moderate=snare` but authored `powerless=0 moderate=0` — argued-not-evidenced legs made visible.

Also: tensions ledger now SUPPRESSES the `grid coverage` line when fully absent (only 3/64 live
constraints author a grid; was noise on every block) — grid line prints only when
authored+injected+imputed>0; report `.md` generators deliberately unchanged (their grid lines are
load-bearing CONDITIONAL/OPEN captions + the OQ-98 always-print banner).

New status token **`future`** (operator ruling 2026-06-15): closes a REAL question deliberately
not slated for work but keeps it searchable + full-bodied; NOT in `omega_resolver`'s ACTIVE set,
so it drops out of the workable frontier; carries no resolution witness, so the rotted-witness
check skips it. Added to `issues_status.py` TOKENS + the ISSUES.md footer grammar +
`run_pipeline.py` comment. OQ-107 (survey-wave/external-instrument adapter) closed `future` —
operator does not see it getting done; OQ-108 was `blocked_on OQ-107` but the witness is the
authored stakeholder (no survey wave needed), so that dep was wrong and is dropped.

## 2026-06-14 — corpus omega soundness POC (OQ-130 scale arm): authored omegas 80% sound, NOT §8-class; identity is three orthogonal axes
**Files:** audits/2026-06-14_corpus_omega_soundness_poc/, ISSUES.md, docs/design/design_gaps.md, prolog/testsets_haiku/
**Tier:** correction-key
Ran §C soundness gate as a POC under a two-party independence protocol (sealed adjudicator held-sample
key committed `acc27d22` BEFORE a blind executor subagent ran probes 1–4; read-only over `testsets_haiku`,
no shared `outputs/` written, no `run_pipeline`). Results: (1) soundness 24/30 = 80% (Ω_E 86.7/Ω_C 75/Ω_P
66.7); content-templating LOW ⇒ the corpus artifact is **identity-overstatement, not fabrication** — so
the OQ-130 blocking precondition is discharged (the authored 4,430 are NOT §8-class; OQ-130 scale-arm
build is de-risked). (2) **Identity is three orthogonal axes, MEASURED:** KIND (signature/orbit) ⊥ topic
(`cs_kernel_id`) at ARI≈0 / same-kernel-cluster 7.65%≈chance — and `gauge_orbit`==`fingerprint-shift`
(one KIND organ, not two); frontier (omega question) ⊥ topic (suppression family spans 225–264 kernels).
(3) The unsound class = kernel-contest family = the same family driving the frontier collapse → dedup
organ and soundness gate are coupled. Fold-backs landed: **GAP-11** (frontier-identity organ missing;
embeddings the real instrument, lexical proxy a floor), OQ-130 (ranking gap named; §1b freshness key =
source content-hash not git HEAD). Caveats: probe 3 is a 30-omega sample (bounds, not proves); the one
held-sample disagreement (id 20) is a hybrid `omega_variables.md` itself leaves open. `issues_status
--check` green (129/0). **Citation rule:** cite "80% sound on a 30-omega sample," never "the corpus is
80% sound." **External adjudication (separate instance, `…poc/adjudication_external.md`):** probe-3
independence was within-instance (executor sealed its own held key); the first separate read corrects
id-20 → ≈77% (23/30), the only external look moving the rate *down*. Caveats it adds: probe 1b≡1a
(ONE KIND surface, not "the KIND organs are orthogonal"); the unsound mass = the kernel-contest family
whose noise-vs-legitimate-committer-Ω_P-frontier reading is **CONTESTED/OPEN** (Seat Theorem Cor 2b →
likely sound-but-mistyped, not restatement). Not §8-class; push pre-condition holds.

## 2026-06-14 — omega-resolver pilot validated on ISSUES.md (OQ-130 minted); §8 landed into OQ-129 OPEN-A
**Files:** python/omega_resolver.py, ISSUES.md, audits/2026-06-14_omega_resolver_pilot/, audits/2026-06-14_extraction_blindness_existential_label/
**Tier:** landed

Ran the omega-resolver memo's pilot (read-only catalog views over ISSUES.md prose + one authored
`Deps:` field; no `issues/` migration). New apparatus `python/omega_resolver.py`: loader / authority
control / SCC-condensation frontier view (§D) / checker / planted-fixture selftest (8/8 controls).
- **§8 re-witnessed** (not transcribed): `extraction_blindness` is an existential-labeling artifact —
  live 16/20 (80%) mirror, haiku 258/358 (72.1%), avg 2.73–2.85 types. Landed into OQ-129 OPEN-A.
  `audits/2026-06-14_extraction_blindness_existential_label/` (probe_mirror.pl reproduces).
- **§E verdict** (the only claim in doubt): frontier view vs independent naive cold-reader baseline →
  57 confirm, 7 contradict, 0 standoff; each contradict settled by an external fact (resolved-blocker
  status for OQ-37/41; own Ω_P type for OQ-03/56/58/69/82). Pilot success criterion met.
- **Model gap surfaced + fixed:** active Ω_E entries blocked on operator-spend-go/substrate are a human
  gate that is not an OQ edge → added relator `blocked_on_human <freetext>` (OQ-71/75/119).
- 16 `Deps:` edges authored by hand in ISSUES.md (values from prose, §1e). `issues_status --check`
  intact (129 parsed). OQ-130 minted for the corpus scale arm (gated on a §8-style omega-soundness
  spot-check before any agenda is trusted). `omega_resolver.py` is read-only, NOT a pipeline gate.

## 2026-06-14 — OQ-129: perspectival-gap feeder rewired onto authored stakeholder seats (was reading the retired constraint_classification)
**Files:** prolog/report_generator.pl, prolog/json_report.pl, ISSUES.md, audits/2026-06-14_omega_gap_reconstruction/
**Tier:** tripwire

`omega_from_gap/5` had been silently dead corpus-wide since the 2026-06-05 rebuild — not broken,
**stranded**: its feeder `report_generator:detect_gap_pattern/2` queried
`constraint_indexing:constraint_classification/3`, the pre-rebuild per-power-seat stored-type surface
that the rebuild retired (0 facts on live bar one engine demo). Rewired onto
`narrative_ontology:constraint_stakeholder/7` via the canonical seat path
`stakeholder_seats:dr_type_for_stakeholder/3` (per-`(C,Name)` d — escapes the same-power atom collapse;
chosen over the plan's inline `dr_type/3`, witnessed verdict-equivalent: both → gap=20). Gap = ≥2
distinct non-`unknown` seat types, fail-closed on <2. `omega_from_gap/5` is now **labeling** (computed
into fresh vars then unified, so a pre-bound pattern can't bypass the priority — same leak `dr_type/3`
guards): `extraction_blindness` → `omega_extraction_blindness_<C>` (critical), else
`general_type_mismatch` → `omega_perspectival_<C>`. `json_report.pl` gaps-array guard moved off the dead
`constraint_classification` to `report_generator:gap_coverage/1`.

**Tripwire for a fresh agent:** before touching gap/omega code, know that `detect_gap_pattern/2` reads
**authored stakeholder seats**, NOT `constraint_classification` (which is dead on the live corpus — a
probe over it returns 0 and looks like "no gaps" when it means "no facts"). Live: 20 GAP / 17 no_gap /
20 abstain; pipeline serializes 20 `omega_extraction_blindness_*`; check_stack clean, validation 0 errors.
OPEN-A..D (labeling finer-partition, abstainer deliberate-vs-hole, all-`unknown` seats, dedup) carried on
**OQ-129**. Gap-Ω prevalence inherits the OQ-70 authoring-convention caveat — do not cite gap counts as a
detection result. Witnesses: `audits/2026-06-14_omega_gap_reconstruction/`.

## 2026-06-14 — OQ-50 closed (explainer rebased on dr_type + type_3/type_5 per-context); OQ-74 core ruled reading-relative; OQ-122 fixture-blocker found STALE; OQ-128 minted
**Files:** prolog/report_generator.pl, prolog/drl_core.pl, ISSUES.md, docs/logic_extensions.md, audits/2026-06-14_oq122_fixture_triage/, audits/2026-06-14_oq49_remeasure/coord0_conjunction_positive_control.txt
**Tier:** landed

Closed OQ-50's two follow-ups (engineering, no design ruling): **OPEN-1** —
`forensic_explain_false_mountain/2` now headlines the post-signature `dr_type` ActualType (the
detector's own notion) with the suppression/extractiveness heuristic relabeled a non-headline
METRIC-LEVEL ANNOTATION; fail-closed `dr_type: unbound` guard, `dr_type/3` total over the reached
set (0/44 no-solution; comment forbids calcifying totality). **OPEN-2** — `type_3_snare_as_rope` /
`type_5_piton_as_snare` (`drl_core.pl:622,629`) lead with `standard_context(Context)` + drop the cut
(matching type_1): the unbound-Ctx trap is gone (type_3 live: 1 phantom-context solution → 4
standard-seat solutions; type_5 0→0). Full caller census clears the multiplicity falsifier (all
setof/findall/`\+`); `/3` legacy path single-solution preserved. Regression: contradiction-sig 5/12
identical to baseline, validation_suite 57/0.

**OQ-74 core RULED reading-relative** (operator, Ω_C/Ω_P): coordination_type is a seventh authored
field, the 55% sibling disagreement is signal; guard holds (no promotion into classify_from_metrics).
**OQ-49 hand-up limb MOOT** — the coord=0 clean-laundering subset is positive-controlled empty on both
twins via the *conjunction* probe (synthetic coord=0+asym row returned; coord+asym excluded), witness
in the oq49 audit dir.

**OQ-122 fixture-blocker is STALE (correction):** re-measured on live HEAD, the FSM victim-gate
(`oq122-fsm-victim-gate`, NOT merged) introduces **zero** new test failures — test_agent_beneficiary
baseline 20≡gate 20, test_contradiction_signatures 5≡5 (delta ∅ both). The "36 fixtures" fail from
2026-06-05 corpus drift (0/11 fsm_agent_mountains + maxwell absent), not the gate; gate's live effect
is a clean 2→0 on the vic=0 physics false-positives. The fixture-cost half of the hold rationale no
longer applies; hold now rests on OQ-128 (physics-RED). A 36-row triage is moot until fixtures are
rebuilt. Evidence: `audits/2026-06-14_oq122_fixture_triage/`. **OQ-128 minted** (mid-power-mountain→rope
power-scaling Ω_C, `drl_core.pl:605-613`). OQ-122 stays open; gate held for bundled landing.

---

## 2026-06-14 — OQ-116 split-closed: de-leak lint chokepoint (linter.py SSOT); MMC = non-collapsing seat divergence; SDZ → OQ-127
**Files:** python/linter.py, python/regenerate_stories.py, agent/cohort_zero_regen.py, python/tests/test_deleak_chokepoint.py, audits/2026-06-12_cohort_zero/pilot_witness.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** tripwire

OQ-116 resolution (operator ruling: *the linter is for the operator, not the engine; linting
stories would be orchestrated bias*). Threshold-coupled lint codes (`SCAFFOLD_DANGER_ZONE`,
`LOW_THEATER_RATIO`, `MOUNTAIN_METRIC_CONFLICT`) must never reach the authoring LLM —
de-leak-in-reverse (OQ-74). The set + strip now live in `linter.py` as the single source of
truth (`THRESHOLD_COUPLED_LINT`, `build_author_feedback`); `regenerate_stories.py` imports it
(was a Pattern-2 fork); `cohort_zero_regen.py` routed through it (latent — its feedback is
validate_json errors, not lint). MMC messages reworded: dropped the authoring imperative, framed
as a claim-vs-metric **seat divergence that need not collapse to one true type** (OQ-74 / seat
theorem) — NOT "the engine corrects the claim."

Engine witness (`audits/2026-06-14_oq116_mmc_engine_witness/`): all 9 live-corpus MMC firings run
through the engine — metric seat diverges from the mountain claim 9/9 (snare/rope/tangled_rope/
unknown), FNL fires only 1/9 (Boltzmann-gated). Corrects OQ-116's own premise: "FSM exists for
it" was WRONG (FSM needs ε ≤ 0.25; firings have ε > 0.25); the analog is the metric classifier
(primary) + FNL (secondary). `institutional_trust_erosion_c0` → snare, FNL=no. (Consistent with
the sibling 2026-06-14 entry: linter reads ε from the authored `domain_priors:base_extractiveness`
regex; the engine classifies on `constraint_metric` — different ε sources, which is *why* MMC is a
coarse proxy.) SDZ half (5/7 calibration) refiled as **OQ-127** (open); de-leak membership correct,
calibration is the open operator call.

**Promotion test (applied, two-pass):** a fresh agent could re-add a lint→prompt loop or
re-declare the tuple. But that mistake is now made **loud, not silent** — `test_deleak_chokepoint.py`
has a census tripwire that fails when a new module joins the {builds-prompt ∧ touches-lint} set,
and `design_discipline.md` §4a states the principle. Per the roll-off rule ("loud failures stay
history, not promoted"), this does **not** graduate to an always-loaded CLAUDE.md section — the
guard + §4a are the durable substrate. Kept here as `tripwire` provenance.

## 2026-06-14 — Engine reads ε from constraint_metric, NOT the testset's domain_priors:base_extractiveness (corrupt-test / ε-trace tripwire)
**Files:** prolog/drl_core.pl, prolog/constraint_data.pl, prolog/domain_priors.pl
**Tier:** tripwire

Surfaced while building the twin-comparison negative control (`audits/2026-06-13_twin_comparison/`):
corrupting a testset's `domain_priors:base_extractiveness(C, 0.68)` changed **nothing** in
classification; corrupting `narrative_ontology:constraint_metric(C, extractiveness, 0.68)` flipped
the signature and moved χ. The verified ε path for classification is:
`drl_core:base_extractiveness/2` (drl_core.pl:85) → `constraint_data:base_extractiveness/2`
(constraint_data.pl:11–13) → `config:param(extractiveness_metric_name, N)` →
`narrative_ontology:constraint_metric(C, N, V)` (N = `extractiveness`). The
`domain_priors:base_extractiveness/2` fact authored in a testset is a SEPARATE domain-prior path
the classifier does not read for corpus constraints (`drl_core:base_extractiveness(_,_):-fail` is
the domain_priors default, domain_priors.pl:33). **Silent-mistake guard:** anyone corrupt-testing
or tracing ε who edits `base_extractiveness` will see no effect and wrongly conclude ε is inert —
edit `constraint_metric(_, extractiveness, _)` (the authoritative source). This is the
"base_extractiveness bridge" the memory index references, now witnessed.

## 2026-06-14 — OQ-49 SPLIT-CLOSE: signature-override re-measure on live corpora; FNL collapse witnessed by source-attribution
**Files:** python/audits/oq49_override_remeasure.py, audits/2026-06-14_oq49_remeasure/, ISSUES.md, prolog/signature_detection.pl
**Tier:** landed

Plan `review-oq-49-in-issues-md-twinkly-mochi.md`. OQ-49's (a)/(b) laundering-vs-load-bearing
ruling was un-answerable as posed — the substrate is gone twice over: `testsets_3000` is a dead
corpus (reset 2026-06-05) and the FNL bait driver was deleted (OQ-70, `72ec2cdd`). Re-measured
read-only on the live corpora (`testsets` 57, `testsets_haiku` 960, `testsets_flash` 960) with a
reconstructed probe (the ad-hoc 2026-06-01 probe was never saved); resolved as a SPLIT-CLOSE under
OQ-74's seat frame. No engine/corpus write; no clause removed.

**The collapse witness is structural, not numeric:** every FNL firing on all three corpora tags
source-1 (`constraint_claim(_,mountain)`); zero source-2, zero unaccounted. Kill condition (any FNL
firing tagged neither = a third path) NOT triggered. The 827/1106 pre-reset bait firings are gone
*by construction*; the raw `1661 → ≤8` count drop is size-confounded (3380 vs 960) and is color,
not the witness. FNL override-effective is now 0/6/8 (was 1661). The override layer's dominant
effect on live is `false_ci_rope → tangled_rope` (override-effective 6/56/78, ~10× FNL→TR's 0/6/8),
not FNL. Inert on live: `:867` natural_law→mountain (0 firings) and `:877` FNL-unknown-fill (0).
Residual = the FULL FNL override-effective union (0/6/8 = 14 across both twins; snare→TR 0/4/4 +
scaffold→TR 0/2/4): **14/14 carry coord+asym**, coord=0 arm positive-controlled (fires elsewhere:
haiku 18 no_coord) → the clean-laundering coord=0 subset is EMPTY on live, escalation dissolves to
zero; the 14 are two-seat signal handed to OQ-74. Twin paired diff (generator-convention signal,
analogue-not-witness): 81 shared / 87 haiku-only / 100 flash-only override-effective ids.

**Citation qualifier (correction-key):** do NOT cite OQ-49's `testsets_3000` 1730/1661 numbers as a
live result — that corpus is dead and pre-OQ-70. The live re-measure is `audits/2026-06-14_oq49_remeasure/`.
OQ-49 status is now `resolved`; any witness-not-verdict engine change is OQ-74's gated pass, not OQ-49's.

## 2026-06-13 — Twin cross-model comparison harness + two generation-quality fixes (classify_corpus driver; Fix A axiom-status, Fix B sibling snap)
**Files:** python/run_pipeline.py, python/story_repair.py, agent/generate_kernel_corpus.py, python/audits/twin_comparison.py, audits/2026-06-13_twin_comparison/
**Tier:** landed

Plan `federated-toasting-sedgewick.md` implemented in four commits.

**Fix A (generation-quality, forward-only — does NOT alter the built twins, which
author zero out-of-enum statuses):** `generate_kernel_corpus.py` prompt now offers only
`holdable`/`overridden` (not `foreclosed`, which is engine-derived via
`cs_axiom_foreclosed/2`). `story_repair.py` coerces `contested→holdable`,
`foreclosed→holdable` (NOT `overridden`: that over-claims displacement unless a
`cs_axiom_contradiction` is authored, which repair cannot see — contradictions live in
the scope manifest / separate `_contradictions.pl`, so it takes the plan's safe-fallback
branch). Any OTHER out-of-enum value is COUNTED in `repair_stats` + reported to stderr +
coerced holdable; `process_batch_results` surfaces a nonzero count as an escalation line.

**Fix B:** `snap_sibling_id()` snaps a drifted `cs_reading_relation` sibling_id to
`<kernel>__<declared_sibling>` only on a UNIQUE confident match (exact, then unique
suffix-normalized) against the seed's `sibling_reading_ids`; ambiguous/unmatched stay
as-authored → quarantine (OQ-58), never wrong-snapped. Applied in `process_batch_results`
before `generate_pl` + JSON write.

**B1 — `classify_corpus(corpus_path, output_name, expected_model)` in run_pipeline.py:**
fresh-process driver classifying a NON-default corpus into its own manifest-bearing
output, WITHOUT running the full pipeline (no overwrite of shared outputs/ or tracked
validation_suite.pl) and never touching canonical pipeline_output.json. Single
deterministic corpus_path overlay (`retractall` default + `assertz` one clause).
Refuses on: zero-glob; load-incomplete (corpus_constraint != glob); model-swap (every
loaded story_provenance model prefix-matches expected_model, with #provenance==glob so
non-vacuous — a count CANNOT catch a name-identical haiku↔flash swap); stale raw;
seen!=classified. `expected_model=None` for mixed corpora. `build_manifest` gained a
`testsets_dir` param + stamps `corpus_path` ONLY for non-default corpora (no-arg manifest
byte-identical — witnessed).

**B-result (audits/2026-06-13_twin_comparison/):** haiku vs flash twins (960 each),
classified serially at one commit (8126231), joined over n=960 by twin_comparison.py
(N=1000 permutations, pre-registered H1/H2). **H1 (structural, per-field, no aggregate):
all 7 fields HOLD** — Wilson-95 lo > permute band95. Powerless seat most model-sensitive
(rate 0.397); institutional highest agreement (0.672) but narrowest chance margin.
Recurring signature lean `constructed_high_extraction`(haiku)↔`false_ci_rope`(flash) —
STRUCTURAL coding not detection (OQ-70). **H2 (continuous): the pre-registered drift test
(obs > band95) FAILED for all 5 fields → H2-drift FALSIFIED.** Observed Δ fell BELOW the
band for all 5 (consistent with continuous invariance), but the lower tail was
pre-registered only to be REPORTED, carries no pre-committed falsifier, and may be partly
ENTAILED by H1 (perspective_chi feeds the χ-classification) — so it is EXPLORATORY, needs
its own registered test, NOT a second confirmation. (Earlier draft over-claimed
"invariance fired"; corrected.) Forward work promoted to OQ-123 (powerless-seat
model-sensitivity), OQ-124 (the constructed_high_extraction↔false_ci_rope signature lean),
OQ-125 (the pre-registered H2-independence colocation test) — filed once the concurrent
OQ-122 writer finished, clearing the label-collision risk.

## 2026-06-13 — Branch cleanup: merged oq117-evidence-block into main; landed the China-legitimacy topic-run artifacts; gitignored *.pdf
**Files:** KNOWN_STATE.md, ISSUES.md (merge), .gitignore, prolog/testsets/{demographic_resource_allocation,livelihood_security_reading,performance_legitimacy_contradictions,performance_legitimacy_flat_control,property_sector_overhang,qualitative_development_reading,quantitative_growth_reading,techno_nationalist_reading}.pl, json/ (7 matching), essays/2026-06/captive_on_both_ends_v3.md
**Tier:** landed

`oq117-evidence-block` (8 OQ-117 audit/docs commits, never pushed) had diverged at `f3f347fe`
while `main` did the twin-corpus rebuild. Merged with `--no-ff`; the only conflict was
`KNOWN_STATE.md` (both branches prepended a dated section — resolved by keeping BOTH, the
twin-corpus and the essay-synthesis entries). ISSUES.md auto-merged; `issues_status.py --check`
passed (120 parsed, 0 malformed). Then committed the China-legitimacy c-orchestrator artifacts the
prior branch documented but never committed (8 testsets + 7 json + the v3 essay). Stale local
pipeline edits to `validation_suite.pl` and `cs_reading_relation_quarantine.json` were DISCARDED
(both are pipeline-regenerated, and main's rebuild had moved them on; the local copies were
pre-rebuild). `.gitignore` now excludes `*.pdf` on principle (already-tracked PDFs unaffected;
the 26MB GO-MAD.pdf and the other untracked `agent/analysis/originals/*.md` source articles were
LEFT in the tree, not committed). Branch deleted post-merge.

**NEXT STEP (not done — operator's call to run):** `python3 python/run_pipeline.py` so
`validation_suite.pl` + classifications pick up the 8 new testsets — they were committed
generate-only, so pipeline outputs are stale w.r.t. them until a run.

---

## 2026-06-13 — Two-model TWIN CORPUS: full never-generated rebuild (Haiku, 988) + Gemini Flash twin (971) reconciled into testsets_haiku/ + testsets_flash/ + testsets/ (branch corpus-rebuild-fresh, merged to main)
**Files:** agent/run_no_scope_gemini.py, agent/_pilot_ladder_strip.py, agent/generate_kernel_corpus.py, prolog/testsets_haiku/, prolog/testsets_flash/, prolog/testsets/, json_haiku/, json_flash/, prolog/beta_processed_flash.txt, ISSUES.md (OQ-75), CLAUDE.md (Corpus Loading)
**Tier:** landed

Branched `corpus-rebuild-fresh` off `main`, cherry-picked the five-defect provenance fix
(`2e3e1998`→`dc12bf5a`), and ran the full never-generated reading-seeds pool (1005 readings /
331 kernels — NOT the plan's remembered 304/101; manifest-pool growth, builder byte-identical)
through the fixed Anthropic/Haiku no-scope path in 8 chunks: **988/1005 generated, 17 named
failures**, n_constraints 5→993, ~$27 Haiku batch. Then generated the SAME pool with
**gemini-2.5-flash** via a faithful kernel-aware port (`agent/run_no_scope_gemini.py`: reuses
`build_cached_messages` + `process_batch_results` verbatim through an Anthropic-result-shaped
adapter; only the batch API/provider + destinations differ; `thinking_budget=0`): **971
generated, 34 failures**. Reconciled by filename → `testsets_haiku/` (960) and `testsets_flash/`
(960) are the INTERSECTION (set-equal, 0 mismatch either way — the controlled two-model
comparison set; JSON in `json_haiku/`/`json_flash/`); `testsets/` (44 = 28 Haiku-only + 11
Flash-only + 5 Sonnet baseline) is the standard location reserved for the c-orchestrator essay
corpus. All five provenance/robustness defects held at scale (993/993 then 960/960 provenance
facts, zero "Redefined static procedure"; Flash stamps `gemini-2.5-flash`). One grid-gate firing
all run (`dueling_disappearance_mechanism__contraction_reading`, pilot_04) — regenerated per the
increment-0 ruling, not waived.

Tripwires promoted to CLAUDE.md (Corpus Loading): **overlay `corpus_path` with `asserta` /
`retractall`-first, never plain `assertz`** — appends after config.pl's default and is silently
ignored (witnessed: loaded 44 instead of 960, no error). Residuals (ISSUES OQ-75, not blockers):
17 Haiku + 34 Flash readings to redraw, dominant cause the generation-side `status:'contested'`
enum violation (valid `holdable|overridden|foreclosed`); naming-drift quarantine class
(model mangles sibling-edge targets, all CAUGHT not crashed); run_pipeline's JSON_DIR is hardcoded
to `json/` so a twin-comparison harness must point its json source at the matching mirror.

---

## 2026-06-13 — Essay-synthesis read-site: report scalars over a propaganda-artifact testset are formalization-of-a-reading, not measurement; OQ-102(a)/OQ-103 are RESOLVED, not open
**Files:** outputs/constraint_reports/{demographic_resource_allocation,livelihood_security_reading,performance_legitimacy_flat_control,property_sector_overhang,qualitative_development_reading,quantitative_growth_reading,techno_nationalist_reading}_report.md, essays/2026-06/captive_on_both_ends_v3.md, docs/technical/build_discipline.md (Instrument-richness section), ISSUES.md (OQ-102, OQ-103)
**Tier:** correction-key

Claude-web synthesized `captive_on_both_ends_v3.md` from the seven China-legitimacy reports
generated 2026-06-13 00:12. Its substantive reading is **correct and report-witnessed**: every
report shows `grid authored 0/32 (absent 32)`, `[INDEX VACUOUS] … ZERO per-index checks ran (not a
clean pass)`, structural verdict `OPEN(no_gradient_data)`, and drift series flagged
`authored-as-PROJECTED (guesses, not observations)`. So the confident scalars (ε=0.42, χ=0.575,
purity=0.667, Boltzmann non-compliance, Wasserstein transport, theorems T2–T6) are a formalization
of **one analyst's reading of one translated press conference** — a regime self-presentation
artifact — not measurement of China. Treat them as a well-structured restatement of the
interpretation, never as evidence for it; the rhetoric's *structure* is anatomized, the *mechanism
it describes* is not. Durable rule promoted to `build_discipline.md` → *Instrument richness is gated
on substrate instrumentation* (read-site paragraph).

**Correction (the citation-staleness rung):** Claude-web cited **OQ-102(a)** (basis=projected
provenance) and **OQ-103** (contamination-edge provenance) as *open*. Both are **resolved** —
OQ-102 closed 2026-06-11, OQ-103 resolved 2026-06-12. The very flags the synthesizer relied on to
see the soft spots — the `basis=projected` drift tail and the `Provenance | Salience` edge columns
— **are those two fixes working**, not live gaps. Do not propagate "OQ-103 open / NOT CARRIED" into
substrate. The coupling-thesis check still stands: lean on a contamination edge only when its
`Provenance` column reads `authored` (livelihood↔{qualitative,quantitative,techno} edges are
`explicit | authored | 1.00`); a `corpus-derived` edge is corpus topology, not the story's claim.

**Open editorial next-step (not yet in substrate, the user's call):** the essay's "dominant Western
frame" contrast (beat-separated coverage, demographics-as-crisis, techno-nationalism-as-threat) was
characterized from general knowledge, not from a read of how the March 2026 conference was actually
covered. The whole "what isn't being said" claim rests on that contrast and would need a check
against real recent coverage before it is rigorous rather than gestural.

---

## 2026-06-13 — OQ-109 RESOLVED: replicate spend ran (15 draws, batch), σ/seat prediction FALSIFIED-AS-TESTED (Fisher p=0.649) → discharged to OQ-118 (draw-stability tracks field-construction-type, not the σ/seat line)
**Files:** agent/cohort_replicate_batch.py, python/cohort_stability.py, python/cohort_sigma_seat_eval.py, audits/2026-06-12_cohort_zero/, ISSUES.md (OQ-109 resolved, OQ-118 filed)
**Tier:** landed

Gated spend authorized + executed (batch `msgbatch_01UbfPq13BcHgJKxcsqK549i`, commit `dcfaea97`):
15 draws = 5 contested kernels (qwerty/free_market/total_war/printing_press/zero_as_number) × 3,
sonnet-4-5 @ temp 0.2, seeded from `prolog/kernel_seeds.json` through the FROZEN seed-spec
(title+domain+summary) so SIGMA_SEAT_PREDICTION (`5f2a626c`) applies. Runner reuses the batch
primitives (cache_control prefix, poll_batch) + cohort_zero_regen's source_desc/stamps; draws are
probe artifacts (replicate dir, none join the corpus). New stat instrument: self-contained Fisher
exact in cohort_sigma_seat_eval, validated vs scipy to 6 sig figs (4 cases) BEFORE use.

**σ/seat partition test FAILED its pre-registered falsifier:** 6 stories, 188 (field,story) cells,
47.87% prediction-consistent, **Fisher two-sided p=0.649 = NO SEPARATION**. The noise hypothesis
the prediction named as its own falsifier was NOT rejected. Operator ruling (split): ROBUST =
apparatus-presence mis-bucketing (boltzmann/network/interval 6/6 stable, predicted seat — no naming
confound, firmest) + the scoped null; CONFOUNDED-HELD (two halves, graduations) = cast/σ fields
(exact-match conflates fresh-cast vs renamed-cast → re-test with the already-built graded distance
metric) and verdict-stability (n=6 + temp 0.2 confound → temp-sweep or accept-as-confounded).
META-FINDING (the yield): draw-stability is an artifact of FIELD-CONSTRUCTION-TYPE (free-authored
cast vs schema-mandated/computed), not the σ/seat line — gates the corpus's analysis contract
(which fields a cross-story claim may trust). NOT noise-over-seat (confounded halves can't
adjudicate). Within-vs-between distance separates cleanly (within ~0.37 < between ~0.59;
printing_press d1-d3=0.543 reproduces the signature-identity witness's "one draw escapes").

**OQ-109 → resolved** (migration complete; σ/seat residual DISCHARGED to OQ-118, not answered —
the close note says discharge-to-successor explicitly). **OQ-118 filed** carrying the robust pair as
settled, the two confounded halves with graduation conditions, the escalate-don't-redraw discipline
(a graded re-test is a NEW pre-registered test, not a retrofit of `5f2a626c`), and reading_diff's
cohort-one carry. Process: settled empirical artifacts committed BEFORE escalating interpretation;
the theory ruling was the operator's, not stamped in auto mode.

## 2026-06-13 — OQ-109 Phase C analytical tail CLOSED to partial: population correction (Iran pair → separate cohort, n=7→n=5) + stability/σ-seat instruments wired & witnessed; two named residuals (gated σ/seat spend, cohort-one reading_diff)
**Files:** prolog/testsets/ (n=5 restored), prolog/archives/datasets/iran_essay_2026-06-11/, python/cohort_stability.py, python/cohort_sigma_seat_eval.py, ISSUES.md, audits/2026-06-12_cohort_zero/
**Tier:** landed

Phase C wire-only close (operator spend boundary = gate the replicate draws). Branch
`oq109-phasec-closeout`; WRITEUP `audits/2026-06-12_cohort_zero/WRITEUP.md`.

- **Step 0 population correction (RESOLVED, witnessed):** two untracked Iran-essay stories
  (`proxy_integration_narrative`, `strategic_victory_narrative`) were loading the live corpus at
  **n=7**. Different generation regime than cohort zero (sonnet-4 / temp 1.0 / `seeded_from=none`
  vs `_c0`'s sonnet-4-5 / temp 0.2 / archive-seeded) ⇒ NOT cohort-zero-homogeneous. Iran-count
  fork CLOSED positive-controlled (genuine 2-story essay: `tensions_ledger.md` + grep both return
  exactly two — possibility 2, not an interrupted-run fragment). Archived to
  `prolog/archives/datasets/iran_essay_2026-06-11/` (commit `d26d04a2`, byte-identity proven before
  live removal); corpus restored to clean **n=5** (pipeline manifest `2026-06-13T03:01:15Z`,
  `1f517a0`). NEVER mix into cohort-zero denominators.
- **Step 1 instruments (LANDED, commit `1f517a08`):** `cohort_stability.py` (per-field
  draw-stability + within-vs-between distance; **Pattern-5 absence-split** — agreement-in-absence
  reported separately, never as positive-stable; witnessed on `organization_floor`×3 + `--selftest`
  PASS) and `cohort_sigma_seat_eval.py` (parse-check reproduces the frozen `SIGMA_SEAT_PREDICTION.md`
  buckets with **zero drift**; population gate **REFUSES a verdict below 3 stories × 2 draws**,
  returns NO TEST at n=1 — operator ruling: a degenerate "insufficient power" number would be a
  counterfeit witness).
- **Two named residuals (status `partial`):** (1) σ/seat partition test awaits the GATED replicate
  spend (`agent/cohort_zero_regen.py --replicates <set>`, set chosen against the seat-side
  prediction fields; then re-run both instruments); (2) `reading_diff` re-point is COHORT-ONE —
  `constraint_stakeholder/7` is Unknown procedure on the corpus, so it has no live positive control
  (inert-proving-inert); deferred until a stakeholder-cell story lands. Homogeneity falsifier
  (item 6) threads to cohort two.

---

## 2026-06-12 — design_discipline v1.3: §9 recorded — engine's pipeline seat is discovery not justification; no-verdict-skips-adjudication; benign-constraint bias control independently re-derived
**Files:** docs/design/design_discipline.md, essays/2026-06/marked_to_market.md
**Tier:** landed

New §9 in `design_discipline.md` (v1.2 → v1.3) records a post-essay review comment (external
model on the `marked_to_market.md` run, relayed by operator) as design doctrine: (1) the engine
sits in the context of discovery, where miscalibration is nearly free because nothing is
load-bearing — its contribution is well-formed questions (anomaly seeds, omegas-as-kill-
conditions, theorems-as-lenses), not calibrated scores; (2) the standing condition is that no
verdict skips adjudication (engine→prose direct wiring = design drift); (3) the surviving risk
is systematic bias not random error (review checks facts, not distributions) — the proposed
benign-constraint control independently re-derives the doc's open item (b) false-positive-on-
high-trust probe, upgrading its standing; (4) convergence under component failure is the design
working, with the audit-of-audits lesson (right-verdict-wrong-mechanism is a finding one level
up; recursion terminates only where a stage holds the substrate). Wiring-state claims in §9 are
attributed to the review, not independently witnessed. Also removed a stray
`marked_to_market.md:Zone.Identifier` Windows download artifact from `essays/2026-06/`.

## 2026-06-12 — OQ-78 evidence pass: ε clustering two-layer; bin boundaries EQUAL config thresholds; circularity → OQ-117; THEN probe HALTED pre-spend — epsilon_bin channel DEAD at the generation interface (hypothesis is the live channel)
**Files:** prompts/uke_scope_v2_json.md, prompts/constraint_story_generation_prompt_json.md, prolog/config.pl, agent/story_generator_base.py, agent/generate_kernel_corpus.py, agent/c-orchestrator.py, ISSUES.md
**Tier:** correction-key

- The ε↔claimed_type correlation (kernel_v2_test2 n=60: snare 0.68–0.78, mountain 0.02–0.15,
  bands near-separable; recorded-bin conformance 15/15, "high"→0.68 in 8/13) is AUTHORING
  CONVENTION — never citable as a detection result (OQ-70-analog).
- OQ-78's "NOT a leak" status REVISED: the bin boundaries disclosed at
  `uke_scope_v2_json.md:292` include 0.10 and 0.30, EXACTLY `piton_epsilon_floor` (Rule Z) and
  `tangled_rope_epsilon_floor`; bin-conformant stories pre-satisfy those two gates by
  construction. 0.55 matches nothing — the rope/snare split (0.45/0.46) is NOT transmitted.
  Disclosure reaches the SCOPE bin-assigner only.
- Ruling RATIFIED same day (OQ-78 → partial): three-fate SPLIT — quantization half CLOSED
  working-as-designed (report ε at bin resolution, ~4 levels); idiom half (0.68 point mass, .x8
  rail) OPEN, re-baselined on cohort zero, graduation = bin-withdrawal probe; independence
  circularity → OQ-117, whose decouple-vs-document design call is SEQUENCED AFTER the probe
  (decision logic recorded in OQ-117 ahead of the run).
- Probe greenlit (15/arm), then HALTED PRE-SPEND in pre-flight recon (halt-and-escalate, not
  inline-amended): NO production path feeds epsilon_bin to the authoring model — unified
  backend / gkc kernel path / c-orchestrator inline all pass `Hypothesis type` only; sole bin
  consumers are two streamlit display lines; the prompt's mapping table is
  instruction-without-data. Historical numeric channel = the PRE-de-leak prompt's type-band
  table (config thresholds verbatim), scrubbed at b6c4e113 (2026-06-05) — every post-reset
  story authored with NO numeric ε instruction and NO bin token. Recorded uke_scope blocks
  are MODEL-FABRICATED (no writer in code; free-text tokens; fabricated dates) ⇒ the 15/15
  bin-conformance was self-labeling. Witnesses W1–W3:
  `audits/2026-06-12_oq78_dead_bin_channel/`. epsilon_bin = Pattern-1 dangling wire;
  disposition in OQ-117 (c), default NOT re-wire.
- Fate-2 graduation RE-ROUTED, zero marginal spend: OQ-109 Phase C regen = withheld arm on
  matched seeds by construction (seed spec is title/domain/summary only); kernel_v2_test2
  archive (n=60) = fed arm and comparator (cross-arm is the test; archive shares are labeled
  context: rail 86% n=91 / 77% n=60; exact-0.68 ~30%). Phase C withdraws MORE than hypothesis
  (full-manifest withdrawal): persistence ⇒ idiom a fortiori; collapse ⇒ hypothesis-vs-rest
  unresolved, finer hypothesis-only arm becomes the designed follow-up.
- Free-gate residual (operator amendment, in OQ-117) — SUPERSEDED same day with the mechanism
  correction: on the ε side ALL gates are numeric-instruction-free in the live pipeline; the
  fed side is the CLAIM (hypothesis-echo), uniformly. Re-weighting principle survives
  restated: discount divergence evidence by what the claim side was fed. OQ-117's live
  mechanism = hypothesis-feeding; boundary disclosure (0.10/0.30) reaches generation only
  indirectly (SCOPE co-authors bin + hypothesis; the hypothesis travels).
- Reconciliations: the 60th story is regime_change_structural_break (sole claimed piton,
  ε=0.28); the live cohort-zero corpus already band-breaks (institutional_trust_erosion_c0,
  claim=mountain ε=0.68 — the OQ-116 MOUNTAIN_METRIC_CONFLICT firing) while LANDING on the rail
  — pre-noted in OQ-78 as the probe's "partial" signature appearing unprompted (n=1,
  hypothesis-pointer), so it cannot be read back as confirmation after the run.
- Boundary-ancestry question (config-copying vs logic.md zones) ruled ARCHAEOLOGY — not chased;
  effect identical either way; ambient monitor is organic corpus growth.
- Direction-of-fix: no target-ε disclosure; no tightening bin boundaries toward thresholds.

## 2026-06-12 — OQ-106 RESOLVED: RETIRE ruled and landed — `structural_coercive_intent` top verdict deleted (range-dead, producerless, consumerless); capture-as-design ratified as piton intension with recorded kill condition; GAP-08 revival stays generic
**Files:** prolog/intent_engine.pl, prolog/config.pl, prolog/config_schema.pl, ISSUES.md, docs/design/design_gaps.md, audits/2026-06-12_oq106_retire/
**Tier:** landed

Worktree `oq106-retire` from `f3f1e99f`. Deciding pass added a third death to the audit's
two: the verdict token had NO consumer even if it fired — `report_generator.pl:22` imports
intent_engine `except([classify_interval/3])` and substitutes its own pattern-only
version; only reachable surface was a format line in validation output via test_harness.
"Unwired ≠ worthless" adjudication came out duplicate-except-the-conjunction (each
conjunct has a live constraint-level near-duplicate: κ-track gradient, agent_beneficiary +
FSM agency gate, authored suppression/resistance metrics, has_viable_alternatives).
Operator ruled retire via web-review option (i): capture-as-design is the piton intension
(`constraint_captured/1` carries designed/decayed; origin-intent not type-constitutive);
kill condition recorded in the OQ-106 close — a proxy/intuition split case arms GAP-08
revival; option (ii) (naming piton as standing candidate consumer) explicitly declined to
avoid an OQ-36 build-mid-baseline license misread. Deleted: the 4-condition clause,
`collect_intent_evidence/1`, `refine_confidence/3`, dead helpers, five params+specs
(config bijection check forces pairwise deletion). Preserved: lower verdicts, OQ-93 open()
passthrough, gradient-fact guard (control flow), intent_* tables + the OQ-43 fail-closed
NL gate. Witness (Pattern 3): full suite before/after byte-identical on substantive lines
(5 [INTENT] lines identical); warning-attribution residue positive-controlled as same-code
run-noise (two identical-code runs drift the same way). Rider: GAP-08's stale residual
paragraph (still described the NL gate as pass-open) updated to record the 2026-06-11
fail-closed ruling.

## 2026-06-12 — OQ-105 RESOLVED: operator ruled fork (a) ALONE; alignment rule landed (prompt + fail-closed validate_json gate); live exposure 0 after the cohort-zero swap retired all 11 hosts
**Files:** ISSUES.md, prompts/constraint_story_generation_prompt_json.md, python/generate_constraint_pl.py, audits/2026-06-12_oq105_alignment_gate/
**Tier:** landed

Worktree `oq105-alignment-rule` from `7ca48e0b`. Operator ratified (a)-alone — grid
alignment at generation, no read-side interpolation machinery — with two amendments: the
densification trade NAMED in the entry (unlabeled generation-side value assertion vs (b)'s
labeled interpolation; defense: model-authored-at-generation = same epistemics as any
authored point; the defect was code injecting endpoints post hoc), and a time-bound reopen.
Substrate moved under the ruling: the OQ-109 cohort-zero swap (`7ca48e0b`) retired ALL 11
OQ-105 hosts to kernel_v2_test2 — live misaligned rows re-derived to 0 (all 5 `_c0`
stories author one shared grid; series-less ones carry `suppression_profile(static)`).
The ratified time-bound ("regen the 11 within Phase C or by a named date") was discharged
by that retirement; its successor clock — rule enforced BEFORE cohort-one generation —
is met by this unit. Landed: prompt rule "One time grid per story" (union grid =
first-class authoring requirement: assert each value / thin to a shared sparser grid /
drop the series, never backfill; OQ-46 scalar-static path untouched) + `_grid_alignment_errors`
in `validate_json` (BOTH jsonschema and fallback paths; all generation drivers import it;
cannot fire on absence — <2 authored grids returns clean, the sanctioned case). Witnesses:
W1 synthetic misalignment fires; W2 5/5 live `_c0` JSONs clean (+ full-validate CLEAN
regression on 2); W3 gate over the 60 archived pre-cohort-zero JSONs flags EXACTLY the 11
known hosts, 0 false positives — extension equals the defect set. Scope note recorded:
the row-sweep's "19/23 robust" is robust relative to LINEAR INTERPOLATION ((b)'s payoff
enumeration), not ground truth. Reopen conditions for (b) in the entry (gate defect /
densification cost turns real on cohort one / Backed-blind consumer over a
misaligned-row corpus).

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
## 2026-06-12 — SIGNATURE-IDENTITY WITNESS: the engine types KINDS, not stories — naming-drift triple probed in fingerprint space; identity-by-signature ruled out for the Phase C regen; seeded_from + draw index added to cohort-zero provenance spec
**Files:** audits/2026-06-12_signature_identity_witness/, ISSUES.md, CLAUDE.md, prolog/logical_fingerprint.pl, agent/c-orchestrator.py
**Tier:** correction-key

Question (operator, after two instances proposed name-/inheritance-keyed identity across
the cohort-zero regen): can the math (fingerprint/orbit/Boltzmann) carry story identity
across generation draws, licensing meta-analysis despite LLM variability? Probe: the
kernel_v1 press/Reformation naming-drift triple (3 runs, 3 names, "same" reading) + 3
topic-distinct controls, pairwise `fingerprint_match/4` over all 7 dims
(`audits/2026-06-12_signature_identity_witness/`, raw output pasted). Result: draws 1&3
= 6/7 with IDENTICAL shift pattern; draw 2 = different mechanism class
(mountain/rope/rope/mountain vs tangled_rope/scaffold/scaffold/tangled_rope), sharing
NOTHING positive with its siblings (its 3/7 = agreement-in-absence: voids []=[], zone
negligible, coupling independent); all 9 between-pairs 0/7; BUT control pair
blockchain|neural_interface also 6/7. Verdict, both directions witnessed: same-material
draws can escape their kind; different-material stories can share one (by design —
fingerprints are domain-abstracted isomorphism classes). KIND-level meta-analysis over
draw-stable fields survives generation stochasticity (the apparatus's purpose — and on
this triple the CLASSIFICATION ITSELF, shift, was draw-unstable: type prevalence over
n=1 draws samples generation noise, consistent with OQ-26). STORY-level identity must be
authored forward (`seeded_from` at regen time), never recovered backward by matching —
signature-keying the trust_erosion exclusion list would have lost draw 2. OQ-109 item 4
updated: seeded_from + draw index schema-required for cohort zero; replicate probe
gains within-vs-between pairwise distances; stability table gates CLAIMS not generation.
Caveats: one triple, 3 controls, old-prompt regime (upper bound on drift); the funded
replicate probe is the calibration, this is data point zero. RULING APPENDED same day
(operator, citing docs/seat-theorem-v1.md): a category shift on redraw is the mechanism
working CORRECTLY — verdicts are seat-indexed, a redraw is a new seat, a classification
that could not shift would be contentless (Coupling Theorem); the analysis product is
SHAPE (hypothesis generation), not draw-invariant truth; determinism-as-desideratum is
part of the problem. Mechanical halves stand (no name/signature keying across regen;
seeded_from = provenance plumbing, no identity semantics); the "identity does not
survive" valence is WITHDRAWN — there was no seat-free identity to lose. Stability table
reads as an empirical sigma/seat partition (draw-stable = situation-fixed; draw-unstable
= seat-expressive), not a noise filter. WRITEUP addendum + ISSUES.md OQ-109 + CLAUDE.md
paragraph all amended with the ruling.

## 2026-06-12 — COHORT ZERO LIVE: pilot 7/7 generated, swap executed (live corpus = 5 _c0 stories; pre-cohort set retired to kernel_v2_test2); C-arm first live decisions witnessed; trio falsifier RESOLVED (filters on new regime); OQ-116 filed
**Files:** prolog/testsets/ (corpus swap), json/, prolog/guard_exclusions.pl, prolog/archives/datasets/kernel_v2_test2/, agent/cohort_zero_regen.py, ISSUES.md, audits/2026-06-12_cohort_zero/
**Tier:** landed

Operator rulings executed: pilot-only-for-continuity (5 continuity-critical seeds:
3 ruled stories + scale_ceiling + adjunctification; organization_floor ×3 replicate);
archive = kernel_v2_test2 (RENAMED from pre_cohort_zero_2026-06-12, not copied; manifest
carries both names + schema pin; transient symlink during in-flight rename, removed at
swap). Pilot: 7/7 driver-owned checks PASS; lint-only failures → OQ-116 (scaffold-zone
calibration; MOUNTAIN_METRIC_CONFLICT contradicts independence doctrine); operator ruled
swap-with-findings-recorded. Battery (battery_witnesses.out): trio FILTERS on cohort zero
(1/4 mountain-claims certify) — archive C≡claim-mountain was old-regime artifact;
demographic_skill_mismatch_c0 protected on own evidence; organization_floor_c0 examined
(redraw not NL-certified — ruled-IN = chain decides + we inspect); trust_erosion_c0
excluded AND chain-false (exclusion bite latent), redraw independently authored the
substantive-dissent shape (claim-mountain ε=0.68) from topic+summary alone;
corroborated_zombie none (flag armed); 12 failing JSONs dispositioned
archived-with-reason. Replicate datum: organization_floor ε=0.42 across all 3 draws
(against contaminated OQ-26 expectation, with frozen σ prediction; n=1 story, table
OPEN). Pipeline green at n=5 (manifest 2026-06-12T17:48:34Z). REMAINING OQ-109 TAIL:
reading_diff re-point (inert until then — no authored cells live), stability table
(needs cohort-one draws), σ/seat evaluation (frozen prediction 5f2a626c awaits table),
OQ-109 close-out.

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
- **check_stack.pl**: library(check) over the stack. **Baseline (UPDATED 2026-06-18, engine-only):
  3 undefined-predicate references** — `data_repair:constraint_beneficiary/2` (:134, :174),
  `data_repair:constraint_victim/2` (:147), `validation_suite:test_case/4` (test_harness.pl:26)
  — plus load warnings (constraint_instances weak-import overrides, one singleton, one
  not-exported import in arakelov_height). Findings beyond this list = regressions. NOT wired as
  a pipeline gate while the baseline is non-empty. **Each is tracked with its own non-bite
  witness under OQ-142** (parent; from the OQ-115 class sweep): OQ-143 (validation_suite guarded
  phantom), OQ-144 (data_repair xref mis-attribution of a clean dynamic call) — both
  annotate-only. **Two findings removed 2026-06-18:** the OQ-115 +1
  (`abductive_helpers:known_override_signature/1` ← signature_detection:1624; loaded in stack.pl),
  and **OQ-145** (`narrative_ontology:requires_active_enforcement/1` ← drift_events.pl:175 — the
  one real code fix, wrong qualifier → `domain_priors:`, dropping the prior 4-finding baseline
  to 3).

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
