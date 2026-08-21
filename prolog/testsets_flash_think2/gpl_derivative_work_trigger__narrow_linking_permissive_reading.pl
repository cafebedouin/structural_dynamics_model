% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Derivative Work Trigger: Narrow Linking Permissive Reading
 *   domain: Software Licensing / Copyright Law / Open Source Governance
 *
 * SUMMARY:
 *   This constraint represents the 'narrow linking permissive reading' of the
 *   GPL's derivative work trigger. It asserts that merely linking proprietary
 *   code to GPL-licensed software does not automatically create a derivative
 *   work, and thus does not trigger the GPL's source code sharing
 *   obligations. This interpretation creates a 'wall' protecting proprietary
 *   modules, frustrating the Free Software Foundation's (FSF) propagation
 *   goal and denying users source-availability guarantees for the proprietary
 *   components. The constraint is claimed as a 'tangled_rope' because it
 *   coordinates software interoperability while extracting from the copyleft
 *   principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.65).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.7).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative Work Trigger: Narrow Linking Permissive Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Software Licensing / Copyright Law / Open Source Governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '0260e065-9538-4c8e-88cb-2c525d6981ad').
narrative_ontology:cs_kernel_codification('0260e065-9538-4c8e-88cb-2c525d6981ad', fixed_text).
narrative_ontology:cs_authority_grounding('0260e065-9538-4c8e-88cb-2c525d6981ad', lineage).
narrative_ontology:cs_interpretation_layer_present('0260e065-9538-4c8e-88cb-2c525d6981ad').
narrative_ontology:cs_reading_relation('0260e065-9538-4c8e-88cb-2c525d6981ad', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('0260e065-9538-4c8e-88cb-2c525d6981ad', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('0260e065-9538-4c8e-88cb-2c525d6981ad', foundational, linking_is_aggregation_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_aggregation_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('0260e065-9538-4c8e-88cb-2c525d6981ad', linking_is_aggregation_not_derivation, conventional).
narrative_ontology:cs_axiom('0260e065-9538-4c8e-88cb-2c525d6981ad', foundational, copyright_requires_modification_for_derivation).
narrative_ontology:cs_axiom_status(copyright_requires_modification_for_derivation, holdable).
narrative_ontology:cs_axiom_grounding('0260e065-9538-4c8e-88cb-2c525d6981ad', copyright_requires_modification_for_derivation, conventional).
narrative_ontology:cs_reference_frame('0260e065-9538-4c8e-88cb-2c525d6981ad', copyright_aggregation_principle).
narrative_ontology:cs_drift_state('0260e065-9538-4c8e-88cb-2c525d6981ad', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0260e065-9538-4c8e-88cb-2c525d6981ad', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These developers benefit from this interpretation as it allows them to link their proprietary code with GPL-licensed libraries without being obligated to release their own source code under the GPL. This protects their intellectual property and business models.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers, beneficiary,
    powerful, biographical, mobile, global).

% Users of software that links GPL components with proprietary modules under this interpretation lose the guarantee of source code availability for the proprietary parts, which frustrates their right to study, modify, and share the complete software.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_users, payer,
    moderate, biographical, constrained, global).

% The FSF, as the primary advocate and enforcer of the GPL, views this interpretation as undermining the core 'copyleft' principle of ensuring software freedom. Their goal of propagating free software is frustrated by the creation of proprietary 'walls' around GPL code.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, payer,
    organized, generational, constrained, global).

% These actors are responsible for interpreting copyright law and the GPL, shaping the legal landscape that defines 'derivative work' and the obligations that follow. Their rulings and analyses establish or challenge this permissive reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, courts_and_legal_scholars, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for other open-source licenses (e.g., MIT, Apache) observe this debate as it influences the broader perception of open-source licensing and the legal risks associated with using open-source components in commercial products.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_advocates_non_gpl, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This interpretation coordinates the legal interaction between proprietary and GPL-licensed software components, providing a framework for developers to combine them without triggering broad copyleft obligations.
% TRANSFER_FUNCTION: It transfers the obligation to share source code (under the GPL) from proprietary developers who link to GPL libraries, effectively allowing them to retain proprietary control over their linked modules.
% ABSENT_VOICES: The original authors of the GPL and strict copyleft advocates, who would argue that any form of linking creates a derivative work, are structurally marginalized by this permissive interpretation in legal contexts where it is adopted.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, proprietary developers would face immense legal uncertainty regarding linking to GPL code, potentially forcing them to either open-source their entire projects or cease using GPL components, fundamentally reorganizing the software development ecosystem.
% FOUNDING_PROBLEM: To define the precise boundary of what constitutes a 'derivative work' under copyright law when proprietary software links to GPL-licensed components, providing legal clarity for software developers.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, industry associations, and proprietary software companies (outside the FSF) attest to the ongoing need for clear definitions of derivative works in complex software systems, supporting the claim that this problem remains live.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is high because this reading significantly curtails the GPL's intended scope, allowing proprietary developers to benefit from GPL code without contributing back. Suppression (0.70) is also high as it actively suppresses the alternative, broader interpretation of 'derivative work' through legal precedent and industry practice. The theater ratio is low (0.10) as this is a genuine legal interpretation, not a performative one. Resistance is high (0.75) due to strong opposition from the FSF and copyleft advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of proprietary developers, this interpretation is a beneficial 'rope' that enables interoperability while protecting their assets. From the perspective of GPL users and the FSF, it functions as a 'snare' that extracts the core value of the GPL by allowing proprietary enclosures. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software developers are clear beneficiaries, gaining legal certainty and protection for their code. GPL users and the FSF are victims, as the core promise of source code availability and propagation is undermined. Courts and legal scholars act as agenda-setters, shaping and enforcing this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'Is the legal definition of ''derivative work'' under copyright law sufficiently clear to definitively resolve the linking question, or is it inherently ambiguous?',
    'A landmark Supreme Court ruling or international treaty explicitly defining ''derivative work'' in the context of software linking.',
    'If resolved as clear, this reading''s persistence would depend on its alignment with that clarity; if ambiguous, the contest between readings is structural and ongoing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, conceptual, 'Ambiguity in the core legal concept of ''derivative work''.').

omega_variable(
    fsf_propagation_goal_impact,
    'To what extent does this permissive linking interpretation genuinely frustrate the FSF''s goal of propagating free software, versus merely altering the strategy for achieving it?',
    'Empirical studies tracking the growth of proprietary software linking GPL components versus the growth of fully free software projects over time.',
    'If the frustration is severe, the extractiveness of this reading is confirmed; if the impact is minor, the extractiveness might be overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fsf_propagation_goal_impact, empirical, 'Measuring the actual impact on free software propagation.').

omega_variable(
    conceptual_contest_with_broad_copyleft,
    'Is the ''narrow linking permissive reading'' fundamentally compatible with the underlying principles of the GPL, or does it represent a conceptual departure?',
    'Philosophical and legal analysis of the GPL''s intent and its historical interpretations, particularly from its original authors.',
    'If deemed incompatible, this reading''s legitimacy would be severely challenged from within the free software movement; if compatible, it would gain broader acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_contest_with_broad_copyleft, conceptual, 'The core conceptual contest between permissive and strict copyleft interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t6, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(gpl__tr_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(gpl__tr_t18, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gpl__be_t6, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(gpl__be_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(gpl__be_t18, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(gpl__su_t6, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(gpl__su_t12, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(gpl__su_t18, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(gpl__su_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_derivative_work_trigger' kernel, alongside 'broad_copyleft_reading' and 'interface_boundary_reading'. Each reading defines the boundary of derivative works differently, leading to distinct structural outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
