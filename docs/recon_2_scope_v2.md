# Recon-2 Scope Document: Engine Drift Audit

**Version.** v2 (revised after framework-author review of v1).
**Status.** Proposal stage. Awaiting framework-author re-review before Claude Code handoff.
**Anchor.** Treats `logic_divergence_audit.md` (Feb 15, 2026, with March 7 supplement) as recon-1.
**Methodology.** Project standard: recon → proposal → execution → writeup. This document is the proposal.
**Drafted.** May 14, 2026.

---

## 1. What This Audit Is For

Recon-2 audits the constraint-logic engine for drift between the doc-side picture (file names, module purposes, architectural patterns, concept implementations) and the file-system reality. The deliverable is operational: a documented set of drift instances and corrections, plus an updated map of where the engine actually is after four months of development.

This audit is *not* a test of the general hypothesis (complexity and abstraction as a Type A drift substrate). Framing recon-2 as a hypothesis test pre-commits to finding things at a different abstraction level than recon-1 ran at — a confirmation-friendly setup the v1 of this document had not adequately guarded against. The general hypothesis goes to a separate instrument (the compression experiment discussed elsewhere in this project), where derivative artifacts at controlled abstraction levels and a substrate constructed rather than endogenous to the framework give a cleaner comparison.

Findings from this audit may inform the general hypothesis as instance evidence. They do not validate it. Synthesis across instruments is downstream and only warranted if both produce non-trivial findings.

## 2. Pre-Registered Predictions

Stated before Claude Code runs, to make the findings falsifiable.

**Pass A (Module inventory).** Of the ~32 in-scope files, at least 3 will have a doc-stated purpose that visibly diverges from current contents. The "named but missing" list will be empty or near-empty (the docs were written from the code, not from a separate spec). The "exists but unnamed" list will have at least 2 entries from the ambiguous-resolution set — modules that implement doc-claimed logic but were never named in the docs.

**Pass B (Architectural patterns).** Of the 4 patterns tested:
- Shadow mode: partial. The pattern's enforcement was never structural — it was discipline-based.
- Two-regime classification: holds, possibly with bypasses for specific signature types.
- Network one-hop: enforced by parameter or by structural recursion bound; clean finding either way.
- Priority ordering: holds in `drl_core.pl`'s gate cascade, with implementation details that may slightly differ from the docs' compact statement.

Net prediction: 1 partial, 3 holds (or holds-with-noted-implementation).

**Pass C (Concept inventory).** Of the 4 concepts tracked:
- Purity zone: divergent (C13 from recon-1 confirmed; check for fourth+ implementations).
- Structural signature: convergent or unified. The signature cascade looked centralized in recon-1's tracing.
- Classification gate: unified via `classify_from_metrics/6`, possibly with one bypass for `naturalized` or a similar edge case.
- Drift event type: divergent. The mention of 11 types vs. an older "10 types" header comment in `drl_lifecycle.pl` (recon-1 L5) hints at definitional drift; the type definitions are likely scattered across `drift_events.pl`, `drl_lifecycle.pl`, and possibly `network_dynamics.pl`.

Net prediction: 2 divergent, 1 convergent or unified, 1 unified-with-bypass.

**Side study.** Recent work (abductive subsystem) shows the same drift profile as the constraint logic on within-subsystem concept consistency, but cleaner config-consumption (the abductive_* params are read from `config.pl`, not hardcoded). Architectural boundary check: abductive may have its own internal conventions but the boundary to the constraint logic is clean.

Findings that contradict these predictions are more informative than findings that confirm them. The writeup should explicitly flag predictions that landed, predictions that missed, and findings that fell outside the prediction space.

## 3. Scope

The doc-anchored constraint logic — Stages 1-9 per `logic.md`, `logic_extensions.md`, `logic_thresholds.md`. Other engines are separate audit subjects; abductive is included as a comparison-framed side study.

**In scope by direct doc reference** (~30 files):

- *Stages 1-6 core:* `drl_core.pl`, `drl_modal_logic.pl`, `drl_lifecycle.pl`, `structural_signatures.pl`, `signature_detection.pl`, `signature_mapper.pl`, `signature_config.pl`, `constraint_indexing.pl`, `constraint_bridge.pl`, `constraint_data.pl`, `constraint_instances.pl`, `drift_events.pl`, `type_metadata.pl`, `measurement_layer.pl`, `domain_priors.pl`, `domain_priors_expanded.pl`, `domain_registry.pl`, `narrative_ontology.pl`
- *Stages 7-9 extensions:* `boltzmann_compliance.pl`, `purity_scoring.pl`, `drl_purity_network.pl`, `network_dynamics.pl`, `drl_fpn.pl`, `fpn_report.pl`, `logical_fingerprint.pl`, `fingerprint_report.pl`, `giant_component_analysis.pl`
- *Config / validation:* `config.pl`, `config_schema.pl`, `config_validation.pl`

**In scope pending Pass A resolution** (Claude Code traces predicates and decides whether the module implements doc-claimed logic):

`sheaf_analysis.pl`, `grothendieck_cohomology.pl`, `arakelov_height.pl`, `covering_analysis.pl`, `coercion_projection.pl`, `invertibility_analysis.pl`, `bifurcation_export.pl`, `omega1_audit.pl`, `post_synthesis.pl`, `drl_audit_core.pl`, `drl_boltzmann_analysis.pl`, `product_site_export.pl`, `inferred_coupling_protocol.pl`, `gap_diagnostic.pl`, `diagnostic_summary.pl`

Decision rule: if a module's exported predicates implement a claim in `logic.md` / `logic_extensions.md`, it's in scope. If it computes analysis on the apparatus from outside (audit tooling, sensitivity probes), it's out and gets a one-line characterization.

**Out of scope** (separate engines / tooling): abductive (see §6), MaxEnt, Dirac, trajectory mining, isomorphism, intent_engine, psych_bridge, uke_dr_bridge, scenario_manager, drl_composition, drl_counterfactual, all `scs_*.pl`, all pure reporters, test/data infrastructure.

*Assumption to flag.* Scope was generated from filename pattern matching plus recon-1 findings. Pass A's first task is to validate these calls. Files mis-categorized here are findings, not errors.

## 4. Single Merged Extraction

Recon-2 runs as one data-extraction pass producing a unified inventory, with Pass B and Pass C as analysis over the inventory. This avoids the triple-traversal cost of running A, B, C as independent file walks.

### 4.1 Per-File Extraction

For each in-scope `.pl` file:

- File path, module name, exported predicates (count + list), importers, imported
- One-line purpose from header comment; flag if absent
- Doc references: where (if anywhere) the file is named in `logic.md`, `logic_extensions.md`, `logic_thresholds.md`, `logic_index.md`
- **Pattern flags:** for each of the 4 patterns in §5, does this file participate? (calls into / mutates / defines structures relevant to the pattern)
- **Concept flags:** for each of the 4 concepts in §6, does this file define, read, or compute on the concept? With file:line refs.

### 4.2 Inventory Outputs

1. *Module inventory table.* Rows = files, columns = extraction fields above.
2. *Named but missing.* Doc-named modules that don't exist.
3. *Exists but unnamed.* In-scope-looking files that no doc names.
4. *Purpose mismatch.* Files where doc-stated purpose visibly diverges from contents.
5. *Ambiguous-modules resolution.* Each ambiguous file categorized in-scope/out-of-scope with one-line justification.

## 5. Pattern Verification (Analysis over Inventory)

Four patterns, each with granularity rules stated in advance.

### Pattern 1: Shadow mode

**Claim.** Stages 7-9 modules don't modify `classify_from_metrics/6` or mutate state it reads.

**Test.** Find all Stages 7-9 modules (per inventory). For each: does it call `classify_from_metrics/6`? Does it `assertz` / `retract` / otherwise mutate predicates that `classify_from_metrics/6` reads?

**Granularity.**
- *Holds:* no Stages 7-9 module calls into or mutates state of `classify_from_metrics/6`.
- *Holds with read-only access:* Stages 7-9 modules call `classify_from_metrics/6` for reading classifications but do not mutate. This is fine; flag it but do not count as violation.
- *Partial:* one or more Stages 7-9 modules mutate state that `classify_from_metrics/6` reads, but the mutations are bounded (e.g., happen only at initialization).
- *Violated:* one or more Stages 7-9 modules mutate state at runtime that `classify_from_metrics/6` consumes during classification.

### Pattern 2: Two-regime classification

**Claim.** Classification flows metrics-first via `classify_from_metrics/6`, then signature-override via `structural_signatures:integrate_signature_with_modal/3`.

**Test.** Trace the classification path in `drl_core.pl` and check for bypasses elsewhere — modules that produce a final classification without going through both stages.

**Granularity.**
- *Holds:* every classification path runs both stages.
- *Holds with bypasses:* some classifications bypass signature override (e.g., for specific edge cases like `naturalized`), but the bypasses are explicit and documented as edge cases in the code.
- *Partial:* bypasses exist but are not explicitly marked as such; classification routes can produce different results depending on which path is taken.
- *Replaced:* the two-regime path is no longer the canonical classification flow; some other regime has displaced it.

### Pattern 3: Network contamination one-hop

**Claim.** Contamination propagation runs one hop only, preventing infinite recursion.

**Test.** Find the contamination propagation code (per recon-1, `drl_modal_logic.pl` and possibly elsewhere). Look for: recursion bounds, iteration counts, fixed-point caps, explicit one-hop enforcement.

**Granularity.**
- *Structural:* one-hop enforced by code structure (no recursive call from contamination propagation back into itself).
- *Parametric:* one-hop enforced by a parameter (e.g., `max_hops = 1`).
- *Not enforced:* propagation can run indefinitely; only halts because contamination decays below threshold.

### Pattern 4: Priority ordering in `dr_type/3`

**Claim.** Priority is Mountain > Piton(dead-coord) > Snare > Scaffold > Rope > Tangled Rope > Piton(fallback) > Naturalized > unknown (per `logic_index.md`).

**Test.** Inspect the gate cascade in `dr_type/3` and `classify_from_metrics/6`. Compare to documented order.

**Granularity.**
- *Holds:* gate cascade matches documented order exactly.
- *Holds with implementation details:* cascade matches in operational effect; the in-code structure may interleave gates with helpers, but the effective priority is correct.
- *Partial:* one or two gates are out of order, with effects on edge-case classifications.
- *Violated:* cascade differs materially from documented order.

### Excluded: Pattern 5

The "spec → registry → implementation" flow direction claim (per `logic_index.md`) is not testable from a code snapshot. Requires commit-history audit. Flag in writeup; do not include in this audit.

## 6. Concept Inventory (Analysis over Inventory)

Four concepts. Sigmoid and χ formulas dropped from v1 — formula-level audit is a possible follow-on if Pass A surfaces multiple computation sites for them.

### Concept 1: Purity zone

**Doc reference.** `logic_extensions.md` §2.3, `logic_thresholds.md` §6a.
**Known state from recon-1.** Three divergent implementations (`logical_fingerprint.pl`, `fpn_report.pl`, `giant_component_analysis.pl`). C13.
**Test.** Confirm current state; check for fourth+ implementations.
**Verdict.** Unified / convergent / divergent, with implementation count.

### Concept 2: Structural signature

**Doc reference.** `logic_extensions.md` §1 and §6 (cascade), `logic.md` §V.
**Test.** Find all places `constraint_signature/2` (or related predicates) are defined. Check for shadow implementations.
**Verdict.** Unified / convergent / divergent.

### Concept 3: Classification gate

**Doc reference.** `logic.md` §II.B.
**Test.** Find all places that produce a constraint type from metrics. Check whether all routes go through `classify_from_metrics/6` or whether modules implement local gate logic.
**Verdict.** Unified / convergent / divergent.

### Concept 4: Drift event type

**Doc reference.** `logic.md` §III, `logic_extensions.md` §4.
**Known signal.** `drl_lifecycle.pl` header says "Ten drift event types" but lists 11 (per recon-1 L5).
**Test.** Find all places drift event types are defined or matched. Are the 11 types defined in one place? Multiple? Do other modules pattern-match on names that match the canonical definition?
**Verdict.** Unified / convergent / divergent.

## 7. Side Study: Abductive Subsystem

**Scope.** `abductive_engine.pl`, `abductive_helpers.pl`, `abductive_report.pl`, `abductive_triggers.pl`, plus `config.pl` §12.

**Three checks.**
1. Within-subsystem concept drift (does the subsystem internally agree on its own concepts?)
2. Config-consumption (does it read `config:param/2` or hardcode?)
3. Architectural boundary (what does it call from the constraint logic side?)

**Confound flagged for the writeup.** The comparison framing has three possible drivers, not two:
- Timescale (4 months old vs recent)
- Substrate (constraint-logic complexity vs abductive-subsystem complexity)
- *Development context* (constraint logic predates audit infrastructure; abductive was likely written under audit-aware conditions)

The audit cannot disambiguate context from substrate without a third subsystem developed under different audit conditions. The writeup states the confound and refrains from claiming the comparison cleanly isolates substrate.

## 8. Staging

Compressed from v1's five checkpoints to three.

1. *Pre-execution checkpoint.* This document, plus framework-author sign-off on §2 predictions, §3 scope, and §5/6 granularity rules. Confirms readiness for Claude Code handoff.
2. *Post-extraction checkpoint.* After Claude Code runs the merged extraction (§4). Review the inventory, ambiguous-modules resolutions, and the Pass B / Pass C analyses produced over it. Decide whether to proceed to side study or pause for synthesis.
3. *Post-side-study checkpoint.* Decide form of writeup (single doc, analog of `metric_audit_writeup.md`; or split into engine findings + side-study comparison).

Estimated Claude Code sessions: 1-2 for the merged extraction and analyses, 0.5 for the side study. Plus framework-author review time at each checkpoint.

## 9. Sanity Check Against the Four-Question Test

- *Tests something untested?* Yes. Recon-1 tested same-level values; recon-2 tests module-doc correspondence, architectural patterns as patterns, concept implementation agreement.
- *Substrate supports the claim?* Yes for the operational claim ("the engine has drift of these kinds at these surfaces"). Limited for any general claim about complexity-and-abstraction as a drift substrate; that claim goes to the compression experiment.
- *Verdict criteria specifiable in advance?* Yes per §2 predictions, §5 granularity rules, §6 verdict shapes. Negative findings are predictions falsified by the data.
- *Within one session's scope?* No, staged: pre-execution → merged extraction + analyses → side study. 1.5-2.5 Claude Code sessions plus reviews.

## 10. What Recon-2 Does Not Do

- Does not audit abductive as a complete subject. Side study only.
- Does not audit MaxEnt, Dirac, trajectory, isomorphism, downstream bridges.
- Does not verify spec→registry→implementation flow direction (commit-history audit).
- Does not produce a v7 framework paper or revise thresholds.
- Does not catch drift in `scs_*.pl` overlays. Out by design.
- Does not validate the general hypothesis about complexity-and-abstraction as Type A substrate. That's the compression experiment's job.

---

## Open Items Before Claude Code Handoff

1. *Predictions in §2.* Are these the right pre-registered predictions? Sharper specific numbers, or fewer predictions, or different ones?
2. *Granularity rules in §5.* Per pattern, are the holds/partial/violated thresholds set at the right grain?
3. *Concept selection in §6.* Four concepts feels right but is a judgment call. Add (e.g., reformability score) or subtract?
4. *Side-study confound.* Is flagging context-vs-substrate enough, or should the side study be re-scoped or dropped if it can't disambiguate?
5. *Writeup form.* Single consolidated writeup or engine-findings + comparison-writeup split? Decided post-extraction, but worth flagging now if there's a preference.

---

*Drafted for re-review prior to Claude Code handoff. The Claude Code prompt itself remains a separate artifact, drafted after this scope document is approved.*

---

*Naming note (2026-06-25, OQ-16): a module named in this account was renamed after this was
written — `drift_events.pl` → `metric_drift_events.pl`. The name in the body reflects the
pre-rename state and is left as a dated record; the rename was name-only (no logic change).
Rename table: ISSUES.md OQ-16.*
