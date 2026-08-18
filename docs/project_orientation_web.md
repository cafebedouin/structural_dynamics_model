# Project Orientation: Deferential Realism (Web-Context Reference)

**Purpose.** Anchor for a Claude instance entering this project through the web/desktop chat interface. The reader is not running the apparatus — the codebase isn't directly inspectable here. The reader is doing what the framework author calls evaluation and project-management work: reading papers critically, sanity-checking proposed audits, drafting Claude Code prompts, tracking the development arc, and helping decide what comes next.

**Companion document.** `project_orientation.md` (in `docs/`) is the code-anchored reference for Claude Code instances who can grep the repo. This document covers the same ground from the side of someone reading rather than running.

**Last updated.** 2026-05-09, after the synthesis paper landed and the audit chain stabilized.

---

## 1. What the Project Is

Deferential Realism (DR) is a framework for classifying social constraints — laws, professional licensing requirements, institutional norms, similar — by how their classifications behave across different observer positions. The framework's central claim: when a constraint produces different classifications for different observers, the disagreement is sometimes noise, sometimes perspective, and sometimes structurally forced by the constraint's generative mechanism. DR distinguishes these by computing whether per-observer classifications form a sheaf (glue into a global classification, observer-independent) or a presheaf (fail to glue, observer-dependent in a measurable way).

The framework is implemented as a working Prolog/Python apparatus operating on 3,335 constraints in the main corpus and 189 SOTU-derived constraints in a secondary corpus. The apparatus produces per-observer classifications, computes cohomological obstruction (H¹), and runs structural-signature checks. Most of what this project does is iterate between framework claims, apparatus behavior, audits that surface drift between the two, and papers that document each layer of refinement.

The project is not the framework alone or the apparatus alone — it is the iterative cycle. The synthesis paper (`when_apparatus_sharpens_taxonomy.md`) makes this cycle explicit: implementing a diagnostic taxonomy discloses architecture the taxonomy doesn't see, and that architecture matters for what the diagnostic can claim.

## 2. The Trifurcation Underneath

DR sits on top of a more general taxonomy from `debugging_philosophy.md`: reasoning failures fall into three operationally distinct types.

**Type A (drift).** A frame is treated as fixed while operationally allowed to mutate. The Sorites argument is the canonical example. Repair: frame-fix at t₀.

**Type B (axiomatic inconsistency).** The formal system's axioms produce contradiction immediately, regardless of process or perspective. Russell's R = {x | x ∉ x} is the canonical example. Repair: axiom revision.

**Type C (indexical underspecification).** A grammatically singular question packages multiple distinct queries indexed by different coordinate systems. Sleeping Beauty is the canonical example. Repair: index specification.

Three subsequent papers extend the taxonomy. `asymmetry_of_failure_types.md` makes the architectural move: the three types divide unevenly when built into a working apparatus (Type B detected, Type C prevented, Type A governed). `when_splitting_isnt_solving.md` makes the Type C extension: indexed answers either glue (sheaves) or don't (presheaves), and H¹ measures the difference. `when_apparatus_sharpens_taxonomy.md` makes the granularity move: each repair operation has internal structure visible only when the apparatus is mechanical.

The DR apparatus is the trifurcation operationalized for the social-constraint domain. Type B detection runs as multi-layer formal checks (Boltzmann compliance, structural signatures, False CI Rope, False Summit Mountain, False Natural Law). Type C prevention runs as schema-layer requirements that no classification proceed without complete (P, T, E, S) indexing. Type A monitoring runs as diachronic drift-event detection. Reading this back into the trifurcation: most of what the apparatus does is operationalize Type C, with Type B detection layered on top, and Type A handled at the project-management level (audit passes are themselves a Type A discipline applied to the project's own artifacts).

## 3. The Four Pieces of Substrate to Hold in Mind

For evaluating new claims about the project — auditing a proposal, drafting a prompt, sanity-checking a finding — these four pieces of substrate are usually what's in play.

**The (P, T, E, S) observer position.** Power, TimeHorizon, ExitOptions, Scope. Every classification is computed relative to a position in this 4-tuple. The canonical observer site has 4 points (U₁–U₄); the product-site expansion has 156. The 10-slice Tier-1 working family used in recent audits is adjacent to but distinct from the canonical site (drift on U₃ specifically). The synthesis paper's §3 finding is that this 4-tuple is not a flat 4D space — it routes through two functionally distinct subsystems.

**The two-hub architecture.** Hub 1 takes (P, S) and produces χ via sigmoid; Hub 2 takes (T, E) and produces mountain or rope via discrete lookup. P appears only in Hub 1; T appears only in Hub 2; S appears only in Hub 1; E is the only axis present in both, with marginal Hub 1 contribution and major Hub 2 contribution. Hub 2 captures more between-slice structural variance than Hub 1 under four of five tested metrics, and the two hubs are statistically independent predictors. This is the empirical anchor for the synthesis paper's Type C granularity claim.

**The χ formula and its corrected notation.** χ = ε × f(d(P, E)) × σ(S). For most of the project's history this was written χ = ε × f(d(P)) × σ(S(P)), and the discrepancy with the implementation was invisible until the metric audit's reconnaissance pass surfaced it. v6.11 was corrected in 8 locations after the audit. This is the worked example for the synthesis paper's Type A granularity claim (drift between specification and implementation as a substrate the original taxonomy didn't capture).

**The site-stability claim, qualified.** v6.11's primary empirical commitment is that the binary H¹ = 0 vs H¹ > 0 classification is preserved with zero crossings between the canonical 4-point site and the 156-context product site. The sheaf audit tested this on the 10-slice Tier-1 family and found a 68.98% crossing rate, 91% driven by the family's U_4 context using universal scope where canonical and product sites use global. The claim survived as: site-stability holds for sites respecting σ ≥ σ(global) at the analytical observer position. This is the worked example for the synthesis paper's Type B granularity claim (axiomatic consistency has a scope-design dimension; the appropriate repair when the design domain is violated is domain specification, not axiom revision).

**The corpus as a continuously-extending artifact.** The main corpus is not a fixed dataset. Each essay generated using the apparatus writes new constraints into `prolog/testsets/`, on the order of three constraints per essay. Recent papers cite different counts (3,254, 3,301, 3,335) because they sampled at different times — the corpus has grown between samples. "The corpus" is grammatically singular but operationally indexes whatever-the-corpus-was-at-time-t. This is unmarked Type C indexical underspecification, not Type A drift, and the appropriate repair is making the time-index visible (see synthesis paper §4.1). Pipeline output JSONs carry timestamp manifests; audits cite the manifest of the pipeline state they ran on; cross-audit synthesis verifies pipeline-state comparability before composing findings.

These five pieces are what most current discussion routes through. Newer findings will compose with them, qualifications will land relative to them, and proposed audits will usually want to test something one of them implies.

## 4. The Paper Sequence

The papers form a coherent intellectual trajectory. Listed in development order, with operational role:

**`debugging_philosophy.md`.** The trifurcation foundation. Type A/B/C with diagnostic test (specify the index → fix the frame → check axioms). Canonical reference; not superseded. The apparatus is built to operationalize this.

**`asymmetry_of_failure_types.md`.** The architectural extension of the trifurcation. The three types divide unevenly: Type B is detectable, Type C is preventable, Type A is governable. Validation is a family of operations, not a single one. Canonical; refined by the synthesis paper.

**`when_splitting_isnt_solving.md`.** The Type C extension. Specification produces clean splits (sheaves) or structured splits (presheaves); H¹ measures the difference. Establishes the product-site binary-stability result (4 → 156 contexts, zero crossings) which is v6.11's primary empirical commitment. Canonical; the binary-stability claim is qualified (not falsified) by the sheaf audit.

**`metrics_as_routing.md`.** Foundation document. Framework metrics — ε, χ, power modifiers, thresholds — are routing mechanisms and governance stands, not truth measurements. Misreading this makes the framework look like precision theater. Canonical.

**`observers_not_humans_v6.md`.** The universality-class framing — **proposed, with its quantitative support open.** DR's load-bearing claims are structural rather than anthropocentric; they should generalize across systems whose internal states update differentially under position-relative net flows. v6 §2.3 carries a marked correction dated 2026-08-18 (OQ-311): robustness to functional form survives (Jaccard 0.697–0.833 over six forms), but the **type-concentration claim is withdrawn as unwitnessed** — its named witness could not have produced per-type numbers. Whether the advantage tracks the `tangled_rope` label or the geometric condition is open at OQ-311 Item 2. **Do not cite "universality class" as an established result.** Canonical; v2–v5 are superseded.

**`contextuality_paper_v1.md`.** The cohomological-obstruction paper. Reports the canonical-site mountain contextuality fraction (11/403 = 0.027). This number is distinct from the SOTU 151/151 finding which is corpus-limited and not used as evidence.

**`when_consensus_isnt_coherence.md`, `when_frame_isnt_foreground.md`, `when_nodes_arent_the_unit.md`.** Supporting framework papers. Each addresses a specific way the apparatus's outputs can be misread (mistaking consensus for coherence; treating frame-relative claims as frame-independent; treating nodes as the analytical unit when network-level effects are operating).

**`when_metrics_arent_measurement.md`.** The cluster-space architecture paper. Three findings: cluster spaces are empirically distinct, the metric layer collapses onto signature class, the lensing zone is genuine cross-axiom tension. Sets up the metric layer as a routing apparatus.

**`coupling_structure_evidence.md`.** Audit infrastructure document. Reports BC coupling (forward asymmetry, geometry-driven) and position-geometry metric sensitivity (axis dominance is metric-specific). §4.4 documents the implementation-adjacent findings (two-hub architecture, within-rope-group T effect, Axiom 2 correction). §7 lists the three reconciliation pointers, one of which the sheaf audit closed.

**`metric_audit_writeup.md`.** The metric audit paper. Two-hub architecture finding, within-rope-group T-axis effect, Axiom 2 notation correction, scope-design preview. Sets up the architectural finding the synthesis paper draws on.

**Sheaf audit writeup** (`when_the_site_changes_the_boundary.md` or similar). Tests the binary-boundary claim on the 10-slice family; finds 68.98% crossing rate driven by U_4 universal-scope mechanism; revises the site-stability claim to its scope-conditional form.

**`when_apparatus_sharpens_taxonomy.md`.** The synthesis paper. Composes the audit findings with the trifurcation: Type C has functional decomposition (two hubs), Type A has cross-artifact substrate (paper vs code), Type B has scope-design dimension (domain specification). Closes the audit chain.

**`deferential_realism_paper_v6_11.md`.** The framework paper. Updated with the Axiom 2 corrections and the construct-distinction note (within-block hierarchy vs hub-level decomposition). The most-cited document; new readers usually start here.

A reader entering through this document who wants to understand the project rather than just orient should read in approximately this order: `debugging_philosophy.md` → `asymmetry_of_failure_types.md` → v6.11 → `when_splitting_isnt_solving.md` → `metrics_as_routing.md` → `coupling_structure_evidence.md` → metric audit → sheaf audit → `when_apparatus_sharpens_taxonomy.md`. The other papers can be read on demand.

## 5. The Audit Chain

The recent audit chain produced the synthesis paper's empirical material. Understanding what each pass did and didn't do is necessary for evaluating any new audit proposal.

**BC coupling audit** (Pass 1–2). Tested whether observer specification and structural variation in the extractive subgraph are coupled and asymmetric. Found forward ρ = 0.350, reverse ρ = −0.121. Sensitivity check showed the coupling is geometry-driven (replacing ordinal PTES distance with empirical classification disagreement collapses ρ to 0.010). Source: `bc_coupling_audit.{md,json}`.

**Position-geometry metric sensitivity audit.** Tested whether the prior "T-dominance" finding holds across structural-distance metrics. Found axis dominance is metric-specific: T leads under extractive fraction only, E leads under three other metrics, S leads under the negative control. Source: `position_geometry_metric_sensitivity.{md,json}`.

**Metric audit** (recon → proposal → execution → robustness check → writeup). Investigated whether the apparatus implementation privileges a structural-distance metric. Discovered the two-hub architecture as the actual answer (it doesn't privilege a metric; it has a functional decomposition). Established Hub 2 dominance (4/5 metrics) and the within-rope-group T-axis effect. T_diff/E_diff covariation robustness check (n=36) confirmed the within-rope-group effect under E-fixed restriction. Surfaced the Axiom 2 notation drift. Sources: `metric_audit_recon.md`, `metric_audit_proposal.md`, `metric_audit_results.{md,json}`, `audit3_te_robustness.{md,json}`, `metric_audit_writeup.md`.

**Sheaf audit** (recon → execution → writeup). Tested binary site-stability on the 10-slice family. Found 68.98% crossing rate driven by U_4 universal-scope mechanism. Closed the second of the three §7 reconciliation pointers. Deferred the Arakelov fragility sub-question (requires MaxEnt re-run). Source: sheaf audit writeup and supporting JSONs.

The methodological pattern: **recon** establishes what's tractable and surfaces unanticipated findings; **proposal** states exactly what will run and what each verdict requires; **execution** runs scripts, saves raw output; **writeup** analyzes from evidence, not documentation. This is the structure to expect for any future audit.

A discipline added after the synthesis paper: each audit cites the manifest of the pipeline state it ran on (`pipeline_run_at` timestamp, `code_commit_short`, `n_constraints`). The corpus is continuously extending, so "the corpus" is meaningful only relative to a timestamp. Cross-audit synthesis verifies that the manifests of the audits being composed are comparable before treating their findings as composable.

## 6. What's Open

As of the synthesis paper's completion:

**Two of three §7 reconciliation pointers from `coupling_structure_evidence.md`.** First: replicating BC coupling and metric-sensitivity audits on a slice family containing the canonical 4 points as a subset (would close the slice-family drift). Third: replicating within-block analysis on the 10-slice family (would test whether v6.11's hierarchy claim survives the slice-family change, holding the within-block construct fixed). The second pointer (binary-boundary audit) was closed by the sheaf audit.

**Arakelov fragility on 10-slice contexts.** The sheaf audit deferred this because MaxEnt distributions for 10-slice contexts aren't in the current pipeline run. Closing it requires adding a `site_contexts_ten_slice/1` predicate, re-running MaxEnt, and re-running the pipeline. Estimated infrastructure: ~30 minutes.

**Fragile-presheaf χ-distribution check** on the 1,569 tangled_rope crossings the sheaf audit found. Question: do these constraints cluster near rope_chi_ceiling at the analytical position, which would compose them cleanly with the fragile-sheaf concept from `when_splitting_isnt_solving.md`? Proposed but not run.

**Two unimplemented engine extensions.** Scope-design validator on `site_contexts/N` predicates (would catch the failure mode the sheaf audit surfaced before any future site predicate violates the design constraint). MaxEnt parameterization for arbitrary sites (would unlock Arakelov fragility on alternate sites). Neither adds findings; both harden the apparatus.

**The reference document for new-model orientation.** This document is one piece of that need (web-context); the Claude Code code-anchored version (`docs/project_orientation.md`) is the other. Whether additional orientation artifacts are needed depends on use cases not yet established.

The audit chain itself is at a natural stopping point. The synthesis paper closed it. Continuing through the open work above would produce incremental refinements rather than new architectural findings.

## 7. How to Read New Claims About the Project

The framework author uses this interface as evaluator and project manager. Most live discussion involves either evaluating proposals, drafting Claude Code prompts, or tracking development arcs. A few patterns recur often enough to call out.

**Proposed audits should be evaluated against four questions.** Does it test something that hasn't been tested? Does it test it on a substrate that supports the claim? Are the verdict criteria specifiable in advance, including what constitutes a negative finding? Is it within one session's scope, or does it need to be staged? The audit chain has produced rough templates for each of these; new proposals usually need to clear all four.

**Findings should be evaluated against the audit chain's epistemic discipline.** Match the stance of `coupling_structure_evidence.md` and the audit writeups: claim what the evidence supports, list what it doesn't, acknowledge limitations explicitly. The user-preferences guardrails on this conversation reinforce this. Findings that overstate (claiming a "privileged metric" when only one of three audits supports it; claiming "site-stability is falsified" when the canonical-to-product result is intact) get pulled back.

**Apparent inconsistencies between papers are findings, not problems to fix.** If two papers disagree on something, surface it as a finding for the framework author to decide on. Don't paper over it. The synthesis paper's distinction between within-block hierarchy and between-slice hub decomposition is exactly this kind of finding — two constructs that look the same but measure different things, surfaced as a clarification rather than a contradiction.

**The trifurcation is the framework's deep structure.** Most new findings will compose with the Type A/B/C taxonomy. The synthesis paper's three granularity findings are specific instances of this — Type C functional decomposition, Type A cross-artifact drift, Type B scope-design dimension. New findings should usually be expressible as either an extension within one of the three types, a clarification of the asymmetry between them, or an instance of how a repair operation behaves when implemented. Findings that don't compose this way are interesting but warrant special scrutiny — they may be revealing something the trifurcation doesn't capture, or they may be miscategorized.

**The synthesis paper's reception was muted across multiple models, and that's diagnostic.** When five models including Gemini, ChatGPT, and others reviewed the paper, several converged on the same handful of observations (taxonomy → architecture → granularity, the meta-recursion of the framework catching its own drift, the engineering stance) without much friction. The reactions confirmed the paper does what it sets out to do but didn't reveal it does *more* than that. The reading: papers in this project are doing something specific (granularity within an existing taxonomy that has its own architectural extension) that doesn't have a familiar shape in models' training. Project-internal references are load-bearing for the synthesis to land. New papers in this project will probably hit the same reception pattern, and that's fine — the project isn't optimizing for general-audience reach.

## 8. Drift Watch

For the framework author using this document as a checkpoint: items most likely to drift between this document's writing and a future review.

A preliminary distinction worth holding in mind: drift in synthesis documents has two mechanisms with different repairs (synthesis paper §4.1). **Substantive claim drift** — a single claim diverges between two artifacts that share a frame (e.g., the Axiom 2 case before the metric audit caught it) — is Type A and is repaired by reconciliation; an audit pass is the right tool. **Continuous-extension drift** — different artifacts cite different time-indexed values of an artifact that is monotonically extending (e.g., the corpus count) — is Type C and is repaired by making the time-index visible; a manifest convention is the right tool. Items in this section are categorized accordingly.

**The audit chain status.** *(Substantive.)* New audit passes will close some open work and may surface new open work. The §6 list reflects post-synthesis-paper state.

**The paper sequence.** *(Substantive.)* New papers may be added; existing papers may be revised. The §4 list is canonical as of the synthesis paper's completion. Renames, supersession, or new additions should update this section.

**The trifurcation's relationship to the apparatus.** *(Substantive.)* The synthesis paper closed the audit chain at three granularity findings, one per type. If a future audit surfaces a fourth finding that doesn't fit within Type A/B/C (or that requires a new category), this section needs to be re-examined.

**The five pieces of substrate.** *(Substantive.)* If a new audit revises any of (P, T, E, S) interpretation, the two-hub architecture, the χ formula, the site-stability claim, or the corpus-as-extending framing, §3 should be updated to match. These are the most stable elements; any changes here are major.

**Voice and stance.** *(Substantive.)* The user-preferences guardrails on epistemic discipline (label assumptions, decline to invent facts, treat findings as bounded claims) are project-defining. If the project's stance shifts, this orientation should reflect it.

**Numerical claims about the corpus and the audit-finding sample sizes.** *(Continuous-extension.)* Counts cited in this document (3,335 main-corpus constraints, 189 SOTU, 24-slice expansion, etc.) are samples at this document's writing date. They will move. The repair is not to update them periodically — that's the wrong governance pattern for continuously-extending artifacts — but to make the timestamp visible. Each cited number traces to a source document (paper or audit) which itself ran on a pipeline state with a manifest. New synthesis work cites manifests; this document cites source-document numbers without trying to maintain a current count.

The companion code-anchored document (`docs/project_orientation.md`) drifts faster — line numbers, file structure, specific findings tied to specific scripts. This document drifts slower because it's at the level of paper sequence and architectural concepts rather than implementation specifics. Periodic review every 4–6 weeks of substantial development is probably the right cadence for substantive items; continuous-extension items don't need periodic review at all, since the manifest discipline handles them in passing.

---

*Document generated 2026-05-09. Companion to `docs/project_orientation.md`. For evaluator/project-manager use in the web/desktop chat context. Not a substitute for reading the papers; orientation only.*
