% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Derivative Work Trigger — Narrow Linking Reading
 *   domain: legal/software/copyright
 *
 * SUMMARY:
 *   The GPL's copyleft mechanism depends on the concept of 'derivative work':
 *   when you link to GPL code, are you creating a derivative work subject to
 *   GPL obligations, or are you aggregating two independent works? The narrow
 *   linking reading answers: linking alone is aggregation, not derivation —
 *   only if you modify the GPL code itself do obligations trigger. This
 *   reading benefits proprietary vendors (they can link freely without
 *   disclosure) and frustrates the GPL's propagation goal (copyleft doesn't
 *   flow through linking). The claimed type is tangled_rope because the
 *   reading simultaneously provides a coordination function (clear
 *   bright-line rule for modular composition) and enables extraction
 *   (proprietary vendors gain a wall). The measured metrics show substantial
 *   extraction (0.68) backed by active suppression (0.72) — courts and
 *   licensing bodies must continuously defend the narrowness against broader
 *   copyleft readings.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors (powerful, beneficiary) — gain the right to link without disclosure obligations
 *   - gpl_users and open_source_downstream_developers (moderate, victims) — lose source-availability guarantees for coupled proprietary modules
 *   - fsf_and_copyleft_advocates (organized, agenda_setter + victim) — defend GPL but are frustrated by the narrowing
 *   - software_licensing_courts (institutional, agenda_setter) — choose and enforce the reading across jurisdictions
 *   - end_users (powerless, observer) — cannot repair or audit proprietary modules even when tightly coupled to GPL code
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.68).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.72).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative Work Trigger — Narrow Linking Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "legal/software/copyright").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '1f21daf9-c25b-457e-b536-f734e728edeb').
narrative_ontology:cs_kernel_codification('1f21daf9-c25b-457e-b536-f734e728edeb', fixed_text).
narrative_ontology:cs_authority_grounding('1f21daf9-c25b-457e-b536-f734e728edeb', lineage).
narrative_ontology:cs_interpretation_layer_present('1f21daf9-c25b-457e-b536-f734e728edeb').
narrative_ontology:cs_reading_relation('1f21daf9-c25b-457e-b536-f734e728edeb', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f21daf9-c25b-457e-b536-f734e728edeb', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('1f21daf9-c25b-457e-b536-f734e728edeb', foundational, linking_is_aggregation_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_aggregation_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('1f21daf9-c25b-457e-b536-f734e728edeb', linking_is_aggregation_not_derivation, conventional).
narrative_ontology:cs_axiom('1f21daf9-c25b-457e-b536-f734e728edeb', foundational, code_modification_triggers_copyleft_obligations).
narrative_ontology:cs_axiom_status(code_modification_triggers_copyleft_obligations, holdable).
narrative_ontology:cs_axiom_grounding('1f21daf9-c25b-457e-b536-f734e728edeb', code_modification_triggers_copyleft_obligations, conventional).
narrative_ontology:cs_reference_frame('1f21daf9-c25b-457e-b536-f734e728edeb', gpl_modular_composition_license).
narrative_ontology:cs_drift_state('1f21daf9-c25b-457e-b536-f734e728edeb', contemporary_proprietary_layering_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f21daf9-c25b-457e-b536-f734e728edeb', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, closed_source_module_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_downstream_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_and_copyleft_advocates).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, software_linking_is_aggregation).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, derivative_work_requires_code_modification).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, api_boundary_preserves_independence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Link against GPL libraries without triggering copyleft obligations on their own code. Under this reading, dynamic linking to unmodified GPL code does not create a derivative work, so proprietary modules remain closed source while accessing GPL functionality. They benefit from free infrastructure without obligation to disclose their own innovations or distribution methods.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Develop proprietary extensions and modules around unmodified GPL libraries. This reading permits them to build closed-source layers without triggering GPL source-disclosure obligations, enabling a two-tier ecosystem where GPL provides the base and proprietary vendors add value in secrecy.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, closed_source_module_developers, beneficiary,
    organized, biographical, mobile, global).

% Use integrated systems combining GPL libraries with proprietary modules. Under this reading, they lose the source-availability guarantee for the proprietary portions, even though those portions are tightly coupled to GPL code. They cannot demand disclosure of the proprietary module's source, limiting their ability to repair, modify, or audit the full system they rely on. Their exclusion from the copyleft guarantee is structural to this reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_users, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_users, excluded).

% Distribute GPL-licensed code and depend on copyleft to ensure that derivative systems remain open. This reading allows proprietary vendors to link against their work and distribute combined binaries without triggering GPL obligations, fracturing the copyleft guarantee downstream. They bear the cost of GPL maintenance without receiving corresponding propagation of source-availability rights.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_downstream_developers, payer,
    moderate, biographical, constrained, global).

% Defend the GPL's copyleft mechanism as a tool to preserve software freedom. This reading directly frustrates their propagation goal: it permits licensing walls that allow proprietary code to bundle with GPL code without disclosing source. They must enforce the GPL through licenses and courts, but this reading narrows what 'derivative work' means, reducing their enforcement leverage. They are simultaneously the constraint's administrator and its victim.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_and_copyleft_advocates, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_and_copyleft_advocates, payer).

% Interpret and adjudicate GPL compliance disputes. This reading represents one coherent but narrow interpretation of 'derivative work' and 'linking.' Courts must choose which reading (narrow linking, broad copyleft, or interface boundary) governs their jurisdictions, and their choices cascade to the ecosystem.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, software_licensing_courts, agenda_setter,
    institutional, generational, analytical, national).

% Would be constrained by a broad copyleft reading that treats API design as derivative-work triggering; excluded from the GPL reading negotiation itself. They develop interface standards that permit modular composition, and a narrow linking reading protects their flexibility to define boundaries; a broad reading would force them to choose between copyleft compliance and modularity.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, technical_standards_bodies, excluded,
    organized, generational, constrained, global).

% Run software combining GPL and proprietary modules. They observe the constraint's effects (whether they receive source or not) but have no formal voice in the licensing interpretation. Their freedom to repair, audit, or modify the full system they depend on is at stake in the reading choice.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users, observer,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear bright-line rule for software licensing: linking to unmodified libraries does not create a derivative work, permitting clean module boundaries and modular composition without cascading licensing obligations. This reading solves the coordination problem of how to build layered software systems with heterogeneous licenses.
% TRANSFER_FUNCTION: Moves the right to keep source code closed from GPL library users (and downstream developers) to proprietary vendors and closed-source module developers. The constraint transfers copyleft propagation rights from the GPL authors to the proprietary integrators — the proprietary vendors gain the ability to distribute combined binaries without source disclosure, while GPL users lose the source-availability guarantee for the coupled system.
% ABSENT_VOICES: GPL library maintainers who would argue the linking creates a derivative work and should trigger copyleft obligations are minimized by the narrow reading; end users who would demand source access to proprietary modules bundled with GPL code are structurally excluded from the licensing conversation; technical standards bodies that would argue API clarity should not determine derivativeness are not at the table.
% DISAPPEARANCE_RATIONALE: If this reading disappeared (and the broad copyleft reading took over), proprietary vendors would face source-disclosure obligations for any system linking to GPL libraries. The entire commercial software ecosystem that has built layered systems on unmodified GPL bases would reorganize — either proprietary vendors would refactor to avoid linking, or they would convert their modules to open source, or licensing litigation would escalate sharply. The two-tier proprietary-on-GPL architecture depends on this reading's enforcement.
% FOUNDING_PROBLEM: Early GPL enforcement action treated any linking as derivativeness, creating an all-or-nothing licensing trap: vendors could either use only GPL code (and open-source everything) or avoid GPL entirely. The narrow linking reading was developed to permit modular software composition while preserving developer choice — a middle path between blanket copyleft and blanket proprietary.
% FOUNDING_PROBLEM_CORROBORATION: Commercial software vendors attest the problem is live — they argue that broad copyleft would chill innovation and force reimplementation of GPL functionality. The FSF and copyleft advocates attest the founding problem is either solved by explicit re-licensing or is a false problem — they argue that if vendors want to use GPL code, they should accept GPL terms, and the narrow reading was developed not to solve a coordination problem but to protect vendor interests. Independent software licensing scholars document both positions in academic literature, and courts have not yet definitively settled the reading across major jurisdictions.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.48 to 0.68 over the interval as proprietary vendors increasingly build layered systems on GPL bases and the value of the protection (the wall) becomes clear. Theater rises from 0.18 to 0.41 because enforcement increasingly relies on legal argumentation and licensing classification rather than technical implementation — courts must continuously reaffirm the boundary via case law. Suppression rises from 0.55 to 0.72 as vendors internalize legal uncertainty and self-censor, and as licensing bodies build enforcement infrastructure (license templates, compliance tools, litigation precedent) to defend the boundary. The measurements are authored on one shared grid (t=0, 5, 10, 15, 20, 25, 30, 35) to ensure every metric samples at every time point. Early observed points (0–15) show rapid escalation as the ecosystem realizes the reading's implications; later projected points (20–35) assume legal stability once a major court ruling clarifies the boundary (or resolves the contest in favor of a broader reading). The plateau in extractiveness and suppression after t=20 reflects maturation — the reading becomes settled practice or is overturned, ending the escalation.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary vendor perspective, the reading is a genuine coordination solution enabling modular development; they compute high benefit (no disclosure obligations) and moderate cost (need to keep linking boundaries clean). From the GPL user and FSF perspective, the reading is a licensing wall that frustrates copyleft; they compute high cost (loss of source access) and minimal benefit. Courts and licensing bodies sit at an analytical seat: they see the coordination function as real but incomplete — it solves the vendor's modular-development problem at the cost of GPL propagation. The engine computes per-seat types from the beneficiary/victim declarations and power atoms: powerful proprietary vendors see coordination (rope-like), moderate GPL users see extraction (snare-like), institutional courts see hybrid enforcement (tangled_rope, the overall claim).
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors benefit unambiguously (d near 0.0, beneficiary end) — they get the wall without modification obligations. GPL users and downstream developers are victims (d near 1.0, target end) — they bear the cost of closed proprietary layers coupled to GPL code. FSF and copyleft advocates are ambiguously positioned: they set the GPL's terms (agenda-setter role, d shifted toward 0.5) but also bear the cost of copyleft frustration (victim pressure, d shifted toward 0.7). Courts are analytical observers setting the constraint itself, not trading within it. No directionality overrides are needed: the structural derivation from beneficiary/victim declarations and power atoms captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early GPL enforcement was all-or-nothing; vendors needed modularity) is contested — FSF argues it was solved by licensing clarity and proprietary vendors' choice to accept GPL terms or avoid GPL code, not by narrowing the reading. The disappearance verdict (world_rearranges) correctly identifies that the ecosystem depends on this reading's persistence. If overturned in favor of the broad copyleft reading, proprietary layering would reorganize sharply — this constraint is not mandatrophic (dead problem, continued function). The constraint persists because courts and vendors defend it as a legitimate boundary-drawing mechanism, not because it solves an unsolved problem. The theater ratio rising from 0.18 to 0.41 indicates growing reliance on legal and rhetorical maintenance (case law, licensing templates, community norms) rather than technical enforcement — this is not yet piton-grade performance, but the trend is toward inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_derivativeness_boundary,
    'Does linking to an unmodified GPL library create a derivative work under copyright law, or does it constitute aggregation of independent works?',
    'Binding judicial interpretation in major jurisdictions (US, EU) setting precedent on whether the GPL''s ''combined work'' language includes dynamic linking. Legislative clarification of ''derivative work'' in copyright statutes.',
    'If linking creates derivativeness, the narrow reading collapses into the broad copyleft reading and proprietary vendors lose their wall; extractiveness transitions from 0.68 to near-snare levels (0.80+). If linking is pure aggregation, the narrow reading is affirmed and the extraction persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(linking_derivativeness_boundary, empirical, 'The foundational legal question the reading rests on — a question of copyright law, not software engineering or intent.').

omega_variable(
    reading_motivation_ambiguity,
    'Was the narrow linking reading developed as a genuine technical/coordination solution to enable modular composition, or was it developed to create a licensing wall that benefits proprietary vendors?',
    'Historical documentation: early GPL drafting memos, FSF emails, court filings, and academic commentary from the 1990s–2010s. Analysis of whether the reading was chosen for its modularity benefits or for its vendor-friendly consequences.',
    'If developed primarily for coordination, the constraint is authentically a hybrid (genuine boundary-drawing function + extraction side effect). If developed primarily to benefit vendors, the constraint is better classified as a snare dressed in technical language. This shapes whether the reading can be defended as ''natural'' or is transparently constructive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_motivation_ambiguity, empirical, 'Whether the narrow reading''s origin and motivation align with its stated coordinating function.').

omega_variable(
    api_sufficiency_ambiguity,
    'Is a clean API boundary alone sufficient to establish non-derivativeness, or does tightness of coupling (message frequency, semantic dependencies, internal-library exposure) matter for the derivativeness analysis?',
    'Technical and legal analysis of real-world systems: how much of a proprietary module''s implementation depends on internals vs. public API? Can a proprietary module be cleanly extracted and reimplemented by a different vendor?',
    'If APIs alone suffice (interface boundary reading affirmed), the wall holds. If coupling tightness matters (broad reading pressure), courts may find that ''tight coupling'' creates derivative works even with unmodified GPL code, reducing effective protection for proprietary modules and collapsing the narrow reading''s edge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(api_sufficiency_ambiguity, conceptual, 'Whether API boundaries are the decisive test or whether semantic/technical coupling matters for derivativeness.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (enforced through licensing ambiguity and litigation threat) or internalized (proprietary vendors self-censor and avoid GPL linking to reduce legal risk)?',
    'Post-clarification trajectory: if a binding court ruling clarifies the narrow reading, does suppression stay high (structural enforcement persists) or drop sharply (vendors had been internalizing uncertainty)? Survey data on vendor decision-making rationale.',
    'If internalized, vendors'' export of proprietary source to avoid GPL entanglement persists even after legal clarity, and the suppression has deeper roots in legal uncertainty than in the reading itself. If purely structural, legal clarity immediately reduces suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the measured suppression is structural or internalized by vendors managing legal risk.').

omega_variable(
    committer_kernel_context,
    'This constraint is one reading of a contested kernel (gpl_derivative_work_trigger). The kernel is the GPL''s founding commitment: that code modifications trigger source-disclosure obligations. This reading narrows when modifications trigger the obligation — only modifications to GPL code itself, not linking to it. Does this narrowing violate the GPL''s core legitimacy claim, or is it a coherent reinterpretation within the GPL''s own framework?',
    'FSF''s original intent statements and subsequent clarifications; GPL license text interpretation; case law on GPL enforceability and scope. Whether FSF has accepted or rejected the narrow reading across its own projects (Linux kernel vs. proprietary drivers is the battleground).',
    'If the narrow reading violates GPL''s core (propagation of copyleft through linking), the reading is foreclosed by the kernel''s own axioms and courts would likely reject it in disputes involving FSF. If it is a coherent reinterpretation, both readings coexist as competing party positions, and the outcome depends on jurisdiction and litigation power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_context, conceptual, 'Whether this reading is compatible with the GPL kernel''s own foundational claims about copyleft and derivative works.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(gpl__tr_t20, projected).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(gpl__tr_t25, projected).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(gpl__tr_t30, projected).
narrative_ontology:measurement(gpl__tr_t35, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(gpl__tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(gpl__be_t20, projected).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(gpl__be_t25, projected).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(gpl__be_t30, projected).
narrative_ontology:measurement(gpl__be_t35, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(gpl__be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(gpl__su_t20, projected).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(gpl__su_t25, projected).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(gpl__su_t30, projected).
narrative_ontology:measurement(gpl__su_t35, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(gpl__su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_licensing_walls).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_ecosystem_propagation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the GPL derivative-work-trigger kernel. The broad_copyleft_reading treats all linking as derivative (high extraction, high suppression, snare-like). The interface_boundary_reading uses API clarity as the boundary test (moderate extraction, rope-like coordination). The narrow_linking_permissive_reading (this story) asserts that linking is aggregation, not derivation (moderate-high extraction, tangled_rope, coordinates modularity while extracting proprietary vendor benefit). All three readings affect the same downstream constraints (proprietary licensing walls, open source propagation). Each reading is a distinct ε-invariant constraint with its own stakeholders, metrics, and classification; the three are linked via network.affects_constraints to show the constraint family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
