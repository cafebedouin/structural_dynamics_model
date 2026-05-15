# Recon-2 Scope Document: Cross-Abstraction Drift Audit

**Status.** Proposal stage. Awaiting framework-author review before Claude Code handoff.
**Anchor.** Treats `logic_divergence_audit.md` (Feb 15, 2026, with March 7 supplement) as recon-1.
**Methodology.** Project standard: recon → proposal → execution → writeup. This document is the proposal for execution.
**Drafted.** May 14, 2026.

---

## 1. What This Audit Tests

Recon-1 was a same-abstraction-level audit: doc-side threshold/formula values vs. code-side threshold/formula values. It found 10 critical, 8 moderate, 12+ low-priority drifts. All critical findings were resolved by updating docs to match code. A March supplement found 2 additional threshold drifts (C11, C12) plus a confirmed pattern of three-way zone-taxonomy divergence (C13).

Recon-2 tests a different hypothesis: that the audit instrument used in recon-1 cannot detect the drift surface where stable doc-level labels mask operationally different code-level realities. The hypothesis frames this as a Type A drift mechanism operating across abstraction levels — a specific case of the cross-artifact-drift finding from `when_apparatus_sharpens_taxonomy.md` §4. The prediction is that the audit will surface drift the same-level instrument couldn't have caught.

The hypothesis is testable. Negative findings (label-content correspondence holds across the audited scope) would weaken it. Positive findings (label-stable, content-drifted) would strengthen it and produce material for a follow-on synthesis on complexity-as-Type-A-substrate.

## 2. Scope

The doc-anchored constraint logic. Stages 1-9 as defined in `logic.md`, `logic_extensions.md`, and `logic_thresholds.md`. Other engines (abductive, MaxEnt, Dirac, trajectory mining, isomorphism, downstream bridges) are separate audit subjects; one of them — abductive — is included as a small comparison-framed side study.

**In scope by direct doc reference** (~22 files):

- *Stages 1-6 core:* `drl_core.pl`, `drl_modal_logic.pl`, `drl_lifecycle.pl`, `structural_signatures.pl`, `signature_detection.pl`, `signature_mapper.pl`, `signature_config.pl`, `constraint_indexing.pl`, `constraint_bridge.pl`, `constraint_data.pl`, `constraint_instances.pl`, `drift_events.pl`, `type_metadata.pl`, `measurement_layer.pl`, `domain_priors.pl`, `domain_priors_expanded.pl`, `domain_registry.pl`, `narrative_ontology.pl`
- *Stages 7-9 extensions:* `boltzmann_compliance.pl`, `purity_scoring.pl`, `drl_purity_network.pl`, `network_dynamics.pl`, `drl_fpn.pl`, `fpn_report.pl`, `logical_fingerprint.pl`, `fingerprint_report.pl`, `giant_component_analysis.pl`
- *Config / validation:* `config.pl`, `config_schema.pl`, `config_validation.pl`

**In scope pending Pass A resolution** (Claude Code traces predicates and decides whether the module implements doc-claimed logic or runs analysis against it):

`sheaf_analysis.pl`, `grothendieck_cohomology.pl`, `arakelov_height.pl`, `covering_analysis.pl`, `coercion_projection.pl`, `invertibility_analysis.pl`, `bifurcation_export.pl`, `omega1_audit.pl`, `post_synthesis.pl`, `drl_audit_core.pl`, `drl_boltzmann_analysis.pl`, `product_site_export.pl`, `inferred_coupling_protocol.pl`, `gap_diagnostic.pl`, `diagnostic_summary.pl`

The decision rule for Pass A: if the module's exported predicates implement a claim in `logic.md` / `logic_extensions.md`, it's in scope for Passes B and C. If it computes analysis on the apparatus from outside (audit tooling, sensitivity probes, sheaf/cohomology computations *over* the logic rather than *as* the logic), it's out and gets noted as such with a one-line characterization.

**Out of scope** (separate engines / downstream / tooling):

- Abductive: `abductive_engine.pl`, `abductive_helpers.pl`, `abductive_report.pl`, `abductive_triggers.pl` (see §6, side study)
- MaxEnt: `maxent_classifier.pl`, `maxent_diagnostic.pl`, `maxent_report.pl`
- Dirac: `dirac_classification.pl`
- Trajectory: `trajectory_mining.pl`, `trajectory_report.pl`
- Isomorphism: `isomorphism_engine.pl`, `isomorphism_report.pl`
- Other engines / bridges: `intent_engine.pl`, `psych_bridge.pl`, `uke_dr_bridge.pl`, `scenario_manager.pl`, `drl_composition.pl`, `drl_counterfactual.pl`
- Sensitivity-sweep tooling: all `scs_*.pl` (auto-generated, by design override canonical params)
- Reporters with no logic of their own: `drift_report.pl`, `json_report.pl`, `report_generator.pl`, `orbit_report.pl`, `quantum_verification_report.pl`, `global_delta_report.pl`, `abductive_report.pl`, `maxent_report.pl`, `trajectory_report.pl`, `isomorphism_report.pl`, `fingerprint_report.pl`, `fpn_report.pl` — *except* where they implement logic-bearing definitions (Pass C will catch this; `fpn_report.pl` is already known to implement a purity-zone taxonomy and is therefore retained in scope)
- Test / data infrastructure: `test_harness.pl`, `validation_suite.pl`, `corpus_loader.pl`, `data_validation.pl`, `data_verification.pl`, `data_repair.pl`, `genuine_findings_query.pl`, `pattern_analysis.pl`, `transition_paths.pl`, `tangled_rope_examples.pl`, `belief_battery`, `gaptests`, `probsets`, `testsets`, `testsets_sotu`, `archives`, `stack.pl`, `utils.pl`, `persistence_export.pl`, `scenario_manager.pl`

*Assumption to flag.* The in-scope and out-of-scope lists were generated from filename pattern matching against the doc set, plus the recon-1 findings. Pass A's first task is to validate these calls. Files mis-categorized here are themselves findings — a file that looks like a reporter but implements logic, or a file that looks like logic but is really a test harness, is exactly the kind of label-content drift the audit is hunting.

## 3. Three Passes

### Pass A: Module Existence and Doc Correspondence

**Question.** Does the file system match the doc-side picture of what modules exist and what they do?

**Inputs.** File system listing of `prolog/*.pl`. Doc references from `logic_index.md` (especially §"Architectural Principles" and §"Document Relationships"), `logic.md` (file-name references in Navigation), `logic_extensions.md` §6.4 (Module Dependency Graph).

**Per-file extraction.**
- File path
- Module name (Prolog `:- module(Name, Exports)` declaration)
- Exported predicates (count + list)
- Importers (which other in-scope modules `:- use_module` this one)
- Imported (which modules this one uses)
- One-line purpose from header comment or top-of-file documentation; flag if absent
- Doc references: where (if anywhere) the file is named in `logic.md`, `logic_extensions.md`, `logic_thresholds.md`, `logic_index.md`

**Outputs.**
1. A module inventory table (rows = files, columns = the per-file extraction fields).
2. A "named but missing" list: doc-named modules that don't exist as files.
3. An "exists but unnamed" list: in-scope-looking files that no doc names.
4. A "purpose mismatch" list: files where the doc-stated purpose visibly diverges from the file's actual contents (signature audit, not deep audit).
5. Ambiguous-modules resolution: each ambiguous file from §2 categorized in-scope or out-of-scope with one-line justification grounded in its exports and importers.

**Verdict criteria.** Pass A is descriptive, not diagnostic — its outputs feed B and C. The "finding" shape is structural: each entry on lists 2, 3, 4 is a candidate cross-abstraction drift. A clean pass would have empty lists 2, 3, 4. The hypothesis predicts non-empty lists.

**Estimated session scope.** One Claude Code session, probably half. Mostly file traversal and predicate extraction.

### Pass B: Architectural Pattern Verification

**Question.** Do the named architectural patterns in the docs still hold in the code?

**Patterns to test.**

1. *Shadow mode (Stages 7-9 don't modify `classify_from_metrics/6`).* Specifiable test: does `classify_from_metrics/6` still exist? Is it called from any Stages 7-9 module? Does any Stages 7-9 module mutate state that `classify_from_metrics/6` reads? Verdict: holds / partial / does-not-hold, with the specific call sites if partial or doesn't-hold.

2. *Two-regime classification (metrics-first, then signature-override).* Specifiable test: does the classification path in `drl_core.pl` route through `classify_from_metrics/6` first and `structural_signatures:integrate_signature_with_modal/3` second? Are there other routes that bypass either layer? Verdict: holds / has-bypasses (list them) / has-been-replaced.

3. *Network contamination is one-hop only.* Specifiable test: in the contamination propagation code (`drl_modal_logic.pl` per recon-1, possibly elsewhere), is there a recursion bound or fixed-point cap that enforces one hop? Verdict: enforced-by-structure / enforced-by-parameter / not-enforced.

4. *Priority ordering in `dr_type/3`* (per `logic_index.md`: Mountain > Piton(dead-coord) > Snare > Scaffold > Rope > Tangled Rope > Piton(fallback) > Naturalized > unknown). Specifiable test: does the implementation match this exact ordering? What does the gate cascade look like in practice?

5. *Single-source-of-truth flow* (per `logic_index.md`: "Changes flow spec → registry → implementation, never backward"). *Not testable from a code snapshot* — this is a process claim about commit history. Flag explicitly: this claim is not testable in this pass, requires a separate commit-history audit, and was *visibly violated* by recon-1's own pattern (code values changed, docs not updated — recon-1 resolved by updating docs *to match code*, the backward direction).

**Outputs.** Pattern verification table: one row per pattern, with verdict, evidence (call sites, predicate names), and notes.

**Verdict criteria.** Each pattern returns one of: *holds*, *partial* (with documented exceptions or known limits), *violated* (with specifics), *untestable from snapshot* (with reason). The hypothesis predicts at least one partial or violated finding.

**Estimated session scope.** One Claude Code session. Requires Pass A's inventory to know which files implement which patterns.

### Pass C: Cross-Module Concept Inventory

**Question.** For each load-bearing concept named in the docs, do its implementations across modules agree?

**Concepts to track.**

1. *Purity zone* (already known divergent per recon-1 C13 — three implementations: `logical_fingerprint.pl`, `fpn_report.pl`, `giant_component_analysis.pl`). Confirm current state and check for fourth+ implementations.
2. *Structural signature* (NL, FNL, CI_Rope, FCR per `logic_extensions.md` §1 and `logic.md` §V). One canonical definition? Multiple?
3. *Classification gate* (Mountain, Rope, Snare, Tangled Rope, Scaffold, Piton per `logic.md` §II.B). Centralized via `classify_from_metrics/6` per recon-1, but does any module re-implement the gate logic for its own purposes?
4. *Drift event type* (Types 1-11 per `logic.md` §III and `logic_extensions.md` §4). Centralized in `drift_events.pl` and `drl_lifecycle.pl`? Or dispersed?
5. *Sigmoid / directionality (`d(P, E)`).* Recon-1 noted the v6.11 paper-vs-code drift on this formula was corrected. Is the corrected formula now consistently implemented? Are there multiple sigmoid computations?
6. *Effective extractiveness χ.* The formula `χ = ε × f(d(P, E)) × σ(S)` — single implementation point, or distributed?
7. *Power modifier π(P) and scope modifier σ(S).* Sourced from `config.pl` per `logic_thresholds.md`, but does any module shortcut or hardcode? (Recon-1 found instances in `drl_modal_logic.pl` lines 1477-1496 for contamination strengths — concept-level analog likely exists for π and σ.)

**Per-concept extraction.**
- Concept name and doc reference
- Modules touching the concept (defining, reading, or computing on it)
- For each touching module: the operative definition or usage
- Verdict: *unified* (one definition, others import), *convergent* (multiple definitions, same outputs on same inputs), *divergent* (multiple definitions, different outputs)
- Evidence: file:line references

**Outputs.** Concept inventory table. For divergent concepts, additional notes on which definition is operationally authoritative (called most, called by the canonical path) versus which are stale or scoped.

**Verdict criteria.** Concept-level. The hypothesis predicts divergent findings beyond purity zone — the prediction is specifically that load-bearing concepts will show the C13 pattern wherever the implementation has been touched by multiple subsystems.

**Estimated session scope.** One Claude Code session, possibly with overflow if many concepts have wide module reach. Stage if needed.

## 4. Side Study: The Abductive Subsystem

**Question.** Does a four-month-old subsystem (constraint logic) show the same drift patterns as a recent one (abductive, four files developed in the last few months)?

**Scope.** `abductive_engine.pl`, `abductive_helpers.pl`, `abductive_report.pl`, `abductive_triggers.pl`. Plus `config.pl` §12 (abductive parameters).

**Three checks, each smaller than the main passes.**

1. *Within-subsystem concept drift.* Are there concepts the abductive system defines that have multiple implementations across its four files? (C13 pattern internal to the subsystem.)
2. *Config-consumption check.* Does the subsystem read the `abductive_*` params from `config.pl` via `param/2`, or hardcode the values?
3. *Architectural boundary check.* What does the abductive subsystem call from the constraint logic side? Does it respect identifiable architectural patterns, or has it grown its own conventions?

**Outputs.** Short comparison writeup (≤2 pages): findings, then explicit comparison to the main audit's results. Does timescale shift the drift profile? Does substrate (subsystem size, single-author vs. multi-session development, recency) explain more than timescale?

**Verdict criteria.** The interesting finding isn't a verdict on the abductive system in isolation; it's the comparison. Three possible outcomes, each informative:

- *Recent work is cleaner.* The drift is timescale-driven; recon-1's findings reflect accumulated time-since-touch.
- *Recent work shows the same drift profile.* The drift mechanism is substrate-driven (complexity, abstraction); timescale doesn't matter. This strengthens the cross-abstraction-drift hypothesis.
- *Recent work shows different drift patterns.* Some patterns are timescale-driven, others substrate-driven. The comparison tells us which is which.

**Estimated session scope.** Half a Claude Code session, after the main passes.

## 5. Ordering and Staging

Pass A first (produces inventory both B and C consume). B and C can run sequentially or in parallel — Claude Code's choice. Side study last so the main passes' findings frame the comparison.

Total estimated sessions: 3-4 for Claude Code, with framework-author review pauses between each.

**Framework-author review checkpoints.**

1. *After this proposal.* Confirm scope, sharpen verdict criteria, approve handoff. The current document is the artifact for this review.
2. *After Pass A.* Review the inventory and the ambiguous-modules resolutions before Passes B and C consume them. This is the chance to add or remove files from scope based on what Pass A actually found.
3. *After Pass B.* Review pattern findings; decide if any pattern needs separate follow-up before Pass C runs.
4. *After Pass C.* Review concept findings; decide whether to proceed to side study or pause for synthesis.
5. *After side study.* Decide whether the audit set produces a writeup, and what form (analog of `metric_audit_writeup.md`, or composed differently).

## 6. Sanity Check Against the Four-Question Test

Per `project_orientation_web.md` §7:

- *Does it test something that hasn't been tested?* Yes. Recon-1 tested same-abstraction-level values. Recon-2 tests label-content correspondence across abstraction levels, architectural patterns as patterns, and concept-level implementation agreement. None of these were in recon-1's scope.
- *Does it test it on a substrate that supports the claim?* Yes. The substrate is the Prolog implementation of the constraint logic, which is what the docs describe and where the hypothesis predicts drift.
- *Are verdict criteria specifiable in advance, including what constitutes a negative finding?* Yes. Each pass has its verdict shape stated. Negative findings are empty drift lists (Pass A), all-holds patterns (Pass B), all-unified concepts (Pass C), and recent-work-cleaner outcome (side study). All would weaken the hypothesis.
- *Is it within one session's scope, or does it need to be staged?* Staged. 3-4 Claude Code sessions, plus framework-author review at each handoff. Within standard project methodology.

## 7. What Recon-2 Does Not Do

Worth stating explicitly so the boundary is clear at writeup time.

- *Does not audit the abductive system as a complete subject.* Side study only.
- *Does not audit MaxEnt, Dirac, trajectory mining, isomorphism, or downstream bridges.* Each is its own audit subject if warranted.
- *Does not verify the spec → registry → implementation flow direction over time.* That requires commit-history audit. Flagged in Pass B as untestable from a code snapshot.
- *Does not produce a v7 of the framework paper or revise threshold values.* Recon-1 already resolved value-level drift; recon-2's findings, if positive, would feed a synthesis paper or doc rewrite, not parameter changes.
- *Does not catch drift in `scs_*.pl` overlays.* By design: those override config at load time as part of sensitivity-sweep tooling. They're a structural example of the cross-abstraction drift mechanism, not a finding to fix.

---

## Open Items for Framework-Author Review

Before drafting the Claude Code prompt:

1. *Is the in-scope list right?* Particularly: do you want `signature_config.pl` in or out? It's small and looks like a config registry for signatures, but Pass A could surface that fast.
2. *Are the four patterns in Pass B the right ones to test?* Other named patterns I might be missing: priority of signature override types (FNL > FCR > NL > CI_Rope), epistemic-access guard (≥3 classifications for Boltzmann compliance), the FPN Jacobi-style iteration.
3. *Are the seven concepts in Pass C the right ones?* Possible additions: complexity offset (Boltzmann context types), reformability score, separability score.
4. *Is the side study sized right?* It could be smaller (single check on config-consumption) or larger (full Pass A/B/C on the abductive subsystem). Current size: comparison-shaped.
5. *Should the staging include a writeup stage by default, or only if findings warrant?* Recon-1 produced a writeup as a single artifact. Recon-2 could produce one consolidated writeup or three small ones plus a synthesis.

---

*Drafted for review prior to Claude Code handoff. The Claude Code prompt itself is a separate artifact, drafted after this scope document is approved.*
