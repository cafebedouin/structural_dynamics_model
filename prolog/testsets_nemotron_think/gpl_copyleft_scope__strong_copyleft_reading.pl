% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Section 2(b) Strong Copyleft — Derivative Work Boundary Extends to All Code Coupling
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   GPL Section 2(b) requires that any work based on the Program — including
 *   combined or dynamically linked works — be licensed under the GPL. The
 *   strong copyleft reading interprets 'work based on the Program' to extend
 *   to all forms of code coupling: dynamic linking, shared libraries, plugin
 *   architectures, and any technical integration that creates a combined work
 *   at runtime. This reading structurally excludes proprietary vendors from
 *   incorporating GPL-licensed components without releasing their entire
 *   combined work under GPL. Free software communities gain a structural
 *   guarantee that code combining with GPL components remains free.
 *   Enforcement threats against dynamic linking patterns are credible through
 *   FSF litigation capacity and community pressure. The constraint operates
 *   as a high-extraction snare: proprietary developers who need GPL
 *   components face a binary choice (release source or don't use), with no
 *   middle-ground licensing option.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.82).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.78).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) Strong Copyleft — Derivative Work Boundary Extends to All Code Coupling").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '56f0d6db-2a78-4ce8-8fca-ed7466e58c65').
narrative_ontology:cs_kernel_codification('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', formalized).
narrative_ontology:cs_authority_grounding('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', lineage).
narrative_ontology:cs_interpretation_layer_present('56f0d6db-2a78-4ce8-8fca-ed7466e58c65').
narrative_ontology:cs_reading_relation('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', gpl_copyleft_scope__narrow_scope_reading, forecloses).
narrative_ontology:cs_reading_relation('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', foundational, copyleft_propagates_through_all_coupling).
narrative_ontology:cs_axiom_status(copyleft_propagates_through_all_coupling, holdable).
narrative_ontology:cs_axiom_grounding('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', copyleft_propagates_through_all_coupling, deontological).
narrative_ontology:cs_axiom('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', foundational, dynamic_linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(dynamic_linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', dynamic_linking_creates_derivative_work, deontological).
narrative_ontology:cs_reference_frame('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', gpl_copyleft_scope_strong).
narrative_ontology:cs_drift_state('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', contemporary_legal_uncertainty, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('56f0d6db-2a78-4ce8-8fca-ed7466e58c65', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_communities).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_projects).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_developers).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, software_freedom_requires_copyleft_propagation).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__strong_copyleft_reading, dynamic_linking_creates_derivative_work).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot legally incorporate GPL components into proprietary products without releasing the combined work under GPL. Face binary choice: open-source their proprietary codebase (surrendering competitive advantage) or forgo GPL components (incurring rewrite costs or inferior alternatives). Exit requires massive reengineering investment or business model change. FSF compliance program targets commercial distributors.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_vendors, payer,
    powerful, biographical, constrained, global).

% Individual developers or small firms building proprietary applications who need GPL-licensed libraries. Same binary choice as vendors but with fewer resources for clean-room reimplementation or legal defense. Often pressured by employers to avoid GPL dependencies entirely, narrowing their technical options.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_developers, payer,
    moderate, biographical, constrained, global).

% Receive structural guarantee that any code combining with GPL components remains free. The constraint protects the commons from enclosure: improvements, extensions, and integrations flow back. They can exit by migrating to permissive licenses, but the GPL network effect makes exit costly for established projects. Benefit is the ecosystem integrity itself, not monetary rent.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_communities, beneficiary,
    organized, generational, mobile, global).

% Individual GPL-licensed projects (Linux kernel, GCC, coreutils, etc.) benefit from the constraint because any proprietary fork or integration must contribute back. They administer the license terms through their own governance. Exit would mean relicensing (requires contributor agreement) or abandoning the project.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_projects, beneficiary,
    organized, generational, mobile, global).

% Free Software Foundation stewards the GPL license text, publishes authoritative interpretations (FAQ, compliance guides), and operates the compliance program (enforcement notices, litigation support, settlement negotiation). They set the interpretive agenda for what 'derivative work' means. They do not directly collect monetary rents but gain institutional relevance and donor support from enforcement activity.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, fsf_enforcement, agenda_setter,
    institutional, generational, analytical, global).

% Adjudicate specific disputes over GPL scope: whether dynamic linking creates derivative works, whether plugin architectures are 'mere aggregation,' whether RPC boundaries matter. No jurisdiction has produced definitive precedent on all coupling forms. Rulings create localized precedent but global uniformity is absent. They bear no direct cost/benefit from the constraint's operation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, courts_legal_system, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of preventing proprietary enclosure of free software: without copyleft, any contributor's code could be absorbed into proprietary products without reciprocation, undermining the incentive to contribute to the commons. The constraint ensures that improvements and integrations flow back to the community.
% TRANSFER_FUNCTION: Moves source code availability from proprietary vendors/developers (who would keep combined works closed) to the public domain/commons (where GPL requires combined works to be released). The transfer is binary: either the proprietary party releases source, or they cannot legally distribute the combined work.
% ABSENT_VOICES: Proprietary developers who want to use GPL libraries in closed products without releasing source — they are structurally excluded from the licensing conversation because the GPL's terms deny them the permission they seek. Also absent: users who would benefit from proprietary-GPL integration products that never get built due to the constraint.
% DISAPPEARANCE_RATIONALE: If the strong copyleft constraint vanished overnight, proprietary vendors would immediately incorporate GPL components into closed products without source release. The free software ecosystem would lose its structural guarantee of reciprocity. Linux kernel, GCC, and core infrastructure would face proprietary forks capturing improvements. The entire free software economy would reorganize around permissive licenses or collapse into open-core models.
% FOUNDING_PROBLEM: Preventing proprietary enclosure of free software through combination and linking: early free software (Emacs, GCC) was being incorporated into proprietary Unix variants without source release, breaking the reciprocity that sustained the commons.
% FOUNDING_PROBLEM_CORROBORATION: FSF and free software community attest the problem is live — proprietary incentives to enclose free code persist (evidenced by ongoing compliance cases and open-core business models). Industry representatives (proprietary vendors, BSA, etc.) attest the founding problem is substantially solved or that the constraint now overreaches — they argue modern software architecture makes the derivative work boundary ambiguous and the constraint captures innocent coupling. Independent legal scholars (e.g., Pamela Samuelson, Lawrence Rosen) corroborate the boundary ambiguity but differ on whether the strong reading is correct.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint forces proprietary vendors to either surrender their proprietary codebase (full source release) or forgo GPL components entirely — a binary extraction with no proportional alternative. Suppression is high (0.78) because the constraint actively suppresses proprietary alternatives: vendors cannot legally distribute combined works, and the FSF's compliance program creates enforcement pressure. Theater ratio is moderate (0.35) — the copyleft mechanism is genuinely functional for its stated purpose, but compliance theater (dual licensing, LGPL workarounds, clean-room reimplementation) absorbs increasing effort. Accessibility collapse (0.68) reflects that proprietary alternatives to GPL components exist but are often technically inferior or economically nonviable. Resistance (0.75) captures sustained industry pushback: LGPL creation, permissive license migration, legal challenges, and the rise of 'open core' models to circumvent copyleft.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF/free-software seat, the constraint is a genuine coordination mechanism (rope-like) that solves the collective-action problem of preventing proprietary capture of communal code. From the proprietary vendor seat, the same structure operates as pure extraction (snare) — a binary tax on using standard infrastructure. The engine computes this divergence from the declared structural positions; the claimed_type (snare) reflects the authoring seat's assessment that the extraction is asymmetric and enforcement-dependent, not that the coordination function is absent.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors and proprietary developers are structural targets (d near 1.0): they bear the full cost of the constraint — either releasing proprietary code or losing access to GPL components. Their exit is constrained: rewriting GPL components is costly, and market pressure to use standard libraries is high. Free software communities and GPL projects are structural beneficiaries (d near 0.0): they receive the guarantee that improvements to GPL code remain free, and their ecosystem is protected from proprietary enclosure. FSF/enforcement entities are agenda_setters: they administer the license, set interpretation, and initiate enforcement. Courts are observers: they adjudicate boundary disputes but do not bear the constraint's costs or benefits. The directionality derivation from beneficiary/victim declarations + exit options produces this structure without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing proprietary enclosure through combination) remains live — proprietary incentives to enclose free code have not diminished. The constraint is not a piton (atrophied function); enforcement activity and compliance infrastructure are actively maintained. The snare classification correctly identifies asymmetric extraction without mislabeling the coordination function as absent — the coordination function exists but operates asymmetrically: free software communities coordinate through the constraint, while proprietary vendors are excluded from that coordination unless they surrender their proprietary position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the GPL''s derivative work boundary a single settled legal fact, or an irreducibly contested kernel where strong_copyleft_reading, narrow_scope_reading, and enforcement_vacuum_reading instantiate different constraints with different ε values?',
    'Authoritative judicial precedent on specific coupling forms (dynamic linking, plugin architectures, RPC boundaries) that resolves the interpretive dispute, or persistent pluralism where enforcement capacity determines which reading operates in which context.',
    'If single settled fact, one reading is correct and others are errors; if contested kernel, each reading is a distinct constraint with its own beneficiary/victim structure and classification — the corpus must model all three as a constraint family linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether GPL scope is one constraint or a kernel generating multiple constraints').

omega_variable(
    enforcement_credibility,
    'Are FSF enforcement threats against dynamic linking patterns structurally credible (backed by litigation capacity and community pressure), or performative (dependent on voluntary compliance)?',
    'Track actual litigation outcomes, settlement terms, and compliance behavior changes after enforcement notices — not merely the volume of threats issued.',
    'If credible, suppression is structural and the snare classification holds; if performative, the constraint''s effective suppression is lower and the classification may drift toward tangled_rope (coordination with weak enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_credibility, empirical, 'Whether enforcement machinery makes the copyleft threat structurally real').

omega_variable(
    derivative_work_boundary_ambiguity,
    'Where does the derivative work boundary actually fall for modern coupling forms: dynamic linking, shared address space, RPC/IPC, plugin architectures, containerized microservices?',
    'Case law on specific technical coupling forms, or legislative clarification of ''derivative work'' in software context.',
    'Boundary location directly determines victim set: each coupling form ruled ''not derivative'' removes a class of proprietary developers from the victim pool and reduces ε; each form ruled ''derivative'' expands the snare''s reach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, empirical, 'Technical boundary of what counts as derivative work under GPL 2(b)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1991, 0.15).
narrative_ontology:measurement(gpl__tr_t1999, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1999, 0.22).
narrative_ontology:measurement(gpl__tr_t2007, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2007, 0.28).
narrative_ontology:measurement(gpl__tr_t2015, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(gpl__tr_t2020, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(gpl__tr_t2024, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1991, 0.65).
narrative_ontology:measurement(gpl__be_t1999, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1999, 0.72).
narrative_ontology:measurement(gpl__be_t2007, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2007, 0.78).
narrative_ontology:measurement(gpl__be_t2015, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(gpl__be_t2020, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement(gpl__be_t2024, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1991, 0.55).
narrative_ontology:measurement(gpl__su_t1999, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1999, 0.62).
narrative_ontology:measurement(gpl__su_t2007, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2007, 0.7).
narrative_ontology:measurement(gpl__su_t2015, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(gpl__su_t2020, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(gpl__su_t2024, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__strong_copyleft_reading, 0.1).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, lgpl_library_exception).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, agpl_network_copyleft).

% DUAL FORMULATION NOTE:
% This constraint decomposes the colloquial label 'GPL copyleft' into a constraint family. strong_copyleft_reading has high ε (snare) because it extends derivative work boundary to all coupling forms. narrow_scope_reading would have lower ε (tangled_rope or rope) because its boundary is narrower and coordination function dominates. enforcement_vacuum_reading would have context-dependent ε. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, powerful, 0.9).
constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, moderate, 0.85).
constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, organized, 0.1).
constraint_indexing:directionality_override(gpl_copyleft_scope__strong_copyleft_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
