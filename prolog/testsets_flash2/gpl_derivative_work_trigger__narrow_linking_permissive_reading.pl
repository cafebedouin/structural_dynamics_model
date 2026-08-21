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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Derivative Work Trigger: Narrow Linking Permissive Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.6).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.4).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative Work Trigger: Narrow Linking Permissive Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '639bbb19-ea85-46dc-a0db-2457b9388dff').
narrative_ontology:cs_kernel_codification('639bbb19-ea85-46dc-a0db-2457b9388dff', fixed_text).
narrative_ontology:cs_authority_grounding('639bbb19-ea85-46dc-a0db-2457b9388dff', lineage).
narrative_ontology:cs_interpretation_layer_present('639bbb19-ea85-46dc-a0db-2457b9388dff').
narrative_ontology:cs_reading_relation('639bbb19-ea85-46dc-a0db-2457b9388dff', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('639bbb19-ea85-46dc-a0db-2457b9388dff', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('639bbb19-ea85-46dc-a0db-2457b9388dff', foundational, linking_is_aggregation_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_aggregation_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('639bbb19-ea85-46dc-a0db-2457b9388dff', linking_is_aggregation_not_derivation, conventional).
narrative_ontology:cs_axiom('639bbb19-ea85-46dc-a0db-2457b9388dff', foundational, only_modification_triggers_copyleft).
narrative_ontology:cs_axiom_status(only_modification_triggers_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('639bbb19-ea85-46dc-a0db-2457b9388dff', only_modification_triggers_copyleft, conventional).
narrative_ontology:cs_reference_frame('639bbb19-ea85-46dc-a0db-2457b9388dff', copyright_law_traditional_aggregation_view).
narrative_ontology:cs_drift_state('639bbb19-ea85-46dc-a0db-2457b9388dff', contemporary_software_ecosystem, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('639bbb19-ea85-46dc-a0db-2457b9388dff', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_users_of_gpl_libraries).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_licensed_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_linked_software).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from being able to link GPL-licensed libraries without incurring copyleft obligations, allowing them to keep their proprietary code closed source and monetize it without sharing modifications. This reading provides a legal 'safe harbor'.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers, beneficiary,
    powerful, biographical, mobile, global).

% Benefits from using GPL components in their commercial products without the burden of open-sourcing their entire application. This reduces legal risk and development costs, but they are still constrained by the need to avoid direct modification.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_users_of_gpl_libraries, beneficiary,
    organized, biographical, constrained, global).

% Bears the cost of reduced copyleft propagation. This reading frustrates their goal of ensuring software freedom for all users, as proprietary modules can 'wall off' GPL code. They actively litigate against this interpretation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, payer,
    institutional, generational, trapped, global).

% Their work, intended to propagate freedom, is used in ways that do not extend copyleft to derivative works, limiting the impact of their licensing choice. They are often reliant on the FSF for enforcement.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_licensed_developers, payer,
    moderate, biographical, constrained, global).

% Loses the source-availability guarantee for the proprietary modules linked to GPL code, which they would have received under a broader interpretation. Their ability to modify or understand the full software stack is curtailed.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_linked_software, payer,
    powerless, immediate, trapped, global).

% These actors interpret and apply copyright law to software licenses, shaping the legal landscape for derivative works. Their rulings and opinions determine the practical effect of this reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, courts_and_legal_scholars, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit narrow, boundary for proprietary software developers to integrate GPL components without triggering full copyleft, facilitating some level of interoperability and reuse.
% TRANSFER_FUNCTION: Transfers the right to keep proprietary code closed-source (and thus monetize it exclusively) to proprietary developers, at the cost of reduced software freedom and source availability for users and GPL developers.
% ABSENT_VOICES: The original authors of GPL code, who intended maximum propagation of software freedom, are often not directly involved in the legal interpretations that narrow its scope. Their intent is often overridden by commercial interests.
% DISAPPEARANCE_RATIONALE: If this permissive reading vanished, proprietary developers would face immediate copyleft obligations for linked code, forcing them to either open-source their products, re-architect to avoid GPL, or cease using GPL components. This would fundamentally alter the software industry's approach to open-source integration.
% FOUNDING_PROBLEM: The GPL was created to ensure software freedom and prevent proprietary enclosure of free software, defining 'derivative work' broadly to achieve this goal.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and many GPL developers attest that the problem of proprietary enclosure remains live. Proprietary developers and some legal scholars argue that the original problem is adequately addressed by other means, or that the GPL's scope is overly broad, leading to this contested status.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'What constitutes a ''derivative work'' under copyright law in the context of software linking (dynamic vs. static)?',
    'Definitive court rulings or legislative action specifically addressing software linking and derivative works.',
    'A broad definition would strengthen copyleft, increasing extractiveness for proprietary developers and decreasing it for GPL advocates. A narrow definition would reinforce the current permissive reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, conceptual, 'Ambiguity in the legal definition of ''derivative work'' for software.').

omega_variable(
    gpl_enforcement_effectiveness,
    'How effective is the FSF''s legal enforcement in challenging this permissive reading and upholding broader copyleft interpretations?',
    'Analysis of litigation outcomes, settlement terms, and industry compliance trends over time.',
    'Increased enforcement effectiveness would shift the balance of power, making the constraint more extractive for proprietary developers and less so for GPL advocates, potentially reclassifying it towards a ''rope'' or ''scaffold'' for the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpl_enforcement_effectiveness, empirical, 'The practical impact of legal challenges on the permissive linking interpretation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, market dominance) or internalized (developers'' fear of litigation, lack of legal resources)?',
    'Surveys of developer behavior and legal counsel decisions; analysis of the financial and legal burden of challenging the permissive reading.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as developers self-censor even without direct legal action. If structural, legal remedies are more direct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in software licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 1991, 0.05).
narrative_ontology:measurement(gpl__tr_t1998, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(gpl__tr_t2005, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(gpl__tr_t2012, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(gpl__tr_t2018, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 1991, 0.4).
narrative_ontology:measurement(gpl__be_t1998, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 1998, 0.48).
narrative_ontology:measurement(gpl__be_t2005, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(gpl__be_t2012, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2012, 0.58).
narrative_ontology:measurement(gpl__be_t2018, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2018, 0.59).
narrative_ontology:measurement(gpl__be_t2024, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 1991, 0.3).
narrative_ontology:measurement(gpl__su_t1998, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 1998, 0.35).
narrative_ontology:measurement(gpl__su_t2005, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(gpl__su_t2012, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2012, 0.4).
narrative_ontology:measurement(gpl__su_t2018, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(gpl__su_t2024, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_licensing_models).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gpl_derivative_work_trigger' kernel. Its interpretation directly impacts the scope and enforceability of other GPL readings and proprietary licensing models.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
