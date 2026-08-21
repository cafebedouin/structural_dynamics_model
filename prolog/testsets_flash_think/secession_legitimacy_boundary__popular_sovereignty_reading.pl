% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Provincial Popular Sovereignty for Secession
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'popular_sovereignty_reading' of
 *   the 'secession_legitimacy_boundary' kernel. It describes a situation
 *   where a democratic majority within a provincial boundary asserts its
 *   ultimate sovereignty, claiming that a referendum result is
 *   self-legitimating for secession. From this reading's perspective, the
 *   federal constraint preventing unilateral secession is highly extractive
 *   and suppressive, as it denies the fundamental right of
 *   self-determination. The claimed type is 'rope' because it represents a
 *   coordination mechanism for the provincial majority's collective action
 *   towards independence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.85).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.9).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Provincial Popular Sovereignty for Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '51c28740-e132-4790-8d0a-eb9c82b96f5e').
narrative_ontology:cs_kernel_codification('51c28740-e132-4790-8d0a-eb9c82b96f5e', formalized).
narrative_ontology:cs_authority_grounding('51c28740-e132-4790-8d0a-eb9c82b96f5e', practice).
narrative_ontology:cs_interpretation_layer_present('51c28740-e132-4790-8d0a-eb9c82b96f5e').
narrative_ontology:cs_reading_relation('51c28740-e132-4790-8d0a-eb9c82b96f5e', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('51c28740-e132-4790-8d0a-eb9c82b96f5e', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('51c28740-e132-4790-8d0a-eb9c82b96f5e', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('51c28740-e132-4790-8d0a-eb9c82b96f5e', foundational, popular_sovereignty_is_ultimate).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_ultimate, holdable).
narrative_ontology:cs_axiom_grounding('51c28740-e132-4790-8d0a-eb9c82b96f5e', popular_sovereignty_is_ultimate, deontological).
narrative_ontology:cs_axiom('51c28740-e132-4790-8d0a-eb9c82b96f5e', foundational, referendum_is_legitimate_expression_of_will).
narrative_ontology:cs_axiom_status(referendum_is_legitimate_expression_of_will, holdable).
narrative_ontology:cs_axiom_grounding('51c28740-e132-4790-8d0a-eb9c82b96f5e', referendum_is_legitimate_expression_of_will, conventional).
narrative_ontology:cs_reference_frame('51c28740-e132-4790-8d0a-eb9c82b96f5e', unfettered_popular_will_as_source_of_legitimacy).
narrative_ontology:cs_drift_state('51c28740-e132-4790-8d0a-eb9c82b96f5e', contemporary_federal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51c28740-e132-4790-8d0a-eb9c82b96f5e', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_loyalists_in_province).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts its collective right to self-determination, viewing a referendum result as the ultimate source of legitimacy for secession. Seeks to overcome federal constraints to achieve independent statehood.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority, agenda_setter,
    powerful, biographical, constrained, regional).

% Bears the political, economic, and territorial costs of potential unilateral secession. Actively asserts the constitutional and legal primacy of the federal state over provincial popular will for matters of national integrity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Oppose secession, fearing loss of federal protections, economic stability, and national identity. Their interests are directly targeted by the provincial majority's assertion of unilateral sovereignty.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_loyalists_in_province, payer,
    moderate, biographical, constrained, local).

% Hold pre-existing treaty rights and inherent sovereignty claims that predate both federal and provincial authority. They are often excluded from the primary debate over provincial secession, yet their lands and rights would be directly impacted by any change in sovereignty.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_nations_within_province, excluded,
    organized, generational, trapped, local).

% Monitor the situation for compliance with international norms regarding self-determination, human rights, and territorial integrity. Their pronouncements can influence the legitimacy and feasibility of secession.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_law_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for a provincial majority to coordinate its collective will and assert its right to self-determination, aiming to establish a new sovereign entity.
% TRANSFER_FUNCTION: Seeks to transfer ultimate sovereignty, territorial control, and allegiance from the federal state to the newly independent provincial entity, along with associated resources and responsibilities.
% ABSENT_VOICES: Indigenous nations within the provincial boundaries, whose pre-existing sovereignty and treaty rights are often sidelined or unaddressed by both federal and provincial claims to ultimate authority. Their consent is critical for any legitimate territorial change.
% DISAPPEARANCE_RATIONALE: If the principle of provincial popular sovereignty for secession were universally accepted and acted upon, it would fundamentally alter the structure of federal states, leading to potential fragmentation and a reordering of national and international political boundaries and allegiances.
% FOUNDING_PROBLEM: To resolve fundamental disputes over the locus of ultimate sovereignty (federal vs. popular/provincial) and the legitimate means for a sub-state unit to achieve self-determination, particularly when a significant provincial population desires independence.
% FOUNDING_PROBLEM_CORROBORATION: Historical and ongoing secessionist movements globally, international legal debates on self-determination, and academic discourse on federalism and sovereignty attest to the enduring nature of this problem. The provincial majority's political parties and advocacy groups consistently articulate this problem as live and unresolved.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the provincial majority's perception that the federal system extracts their right to self-determination and potentially their resources without full consent. Suppression (0.90) is high due to the significant legal, political, and potentially coercive measures the federal government would employ to prevent unilateral secession. The theater ratio is low (0.10) because the conflict is a genuine, high-stakes political struggle, not a performative one. Accessibility collapse (0.75) is high for the provincial majority, as alternatives to full independence are seen as insufficient or foreclosed. Resistance (0.80) is high, reflecting active political and social movements advocating for secession. The metrics reflect the provincial majority's experience of the federal constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial majority's perspective, the federal system's constraint on secession is an illegitimate imposition, making this reading a 'rope' for their self-determination. From the federal government's perspective, this same assertion of popular sovereignty is a threat to national integrity, and the federal constraint is a legitimate 'mountain' or 'rope' for national unity. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial_majority is the primary beneficiary of this reading, as it legitimizes their claim to self-determination (d near 0.0). The federal_government and federal_loyalists_in_province are the targets, as they bear the costs of potential dismemberment and loss of national unity (d near 1.0). Indigenous_nations_within_province are excluded, as their distinct sovereignty claims are often not central to this particular framing of the secession debate.
 *
 * MANDATROPHY ANALYSIS:
 *   From the perspective of the provincial majority, the federal government's mandate to govern them has substantially atrophied, particularly regarding the question of ultimate sovereignty. They perceive the federal structure as having outlived its legitimate function for their region, transforming into a mechanism of extraction and suppression that prevents their self-determination. The persistence of the federal constraint is seen as due to federal power and constitutional inertia, rather than a live, consensual mandate from the provincial population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_majority,
    'What constitutes a legitimate ''majority'' for self-determination in a secession referendum: a simple majority, a supermajority, or a majority of all eligible voters?',
    'International legal precedent, domestic constitutional amendment, or a negotiated political agreement between federal and provincial authorities.',
    'A higher threshold for ''majority'' would significantly constrain the provincial majority''s ability to claim self-legitimating secession, shifting the balance of power towards federal authority or requiring broader consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_majority, conceptual, 'Ambiguity regarding the quantitative threshold for legitimate popular will in secession.').

omega_variable(
    territorial_integrity_vs_self_determination,
    'How does the principle of a sub-state unit''s right to self-determination (popular sovereignty) reconcile with the principle of the territorial integrity of the existing federal state?',
    'Adjudication by international courts, a new constitutional framework, or a political settlement that redefines the relationship between internal and external self-determination.',
    'If territorial integrity is prioritized, the provincial majority''s claim is significantly weakened; if self-determination is prioritized, the federal state''s integrity is challenged, potentially leading to reclassification of the federal constraint as a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_integrity_vs_self_determination, conceptual, 'Fundamental tension between two competing principles of international law and political theory.').

omega_variable(
    indigenous_consent_requirement,
    'Does the provincial majority''s right to self-determination for secession supersede or require the explicit consent of Indigenous nations whose traditional territories lie within the provincial boundaries?',
    'Direct negotiations with Indigenous nations, legal rulings on the scope of Indigenous sovereignty, or a new constitutional framework that explicitly addresses multi-layered sovereignty.',
    'If Indigenous consent is required, the provincial majority''s claim to unilateral secession is fundamentally constrained, potentially leading to a re-evaluation of the ''self-legitimating'' aspect of a provincial referendum.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_consent_requirement, preference, 'The moral and legal priority of Indigenous rights in a secession context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 2000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(sece_tr_t2004, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2004, 0.11).
narrative_ontology:measurement(sece_tr_t2008, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(sece_tr_t2012, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(sece_tr_t2016, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2016, 0.09).
narrative_ontology:measurement(sece_tr_t2020, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(sece_be_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(sece_be_t2004, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2004, 0.75).
narrative_ontology:measurement(sece_be_t2008, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2008, 0.8).
narrative_ontology:measurement(sece_be_t2012, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2012, 0.82).
narrative_ontology:measurement(sece_be_t2016, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2016, 0.84).
narrative_ontology:measurement(sece_be_t2020, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(sece_su_t2004, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2004, 0.8).
narrative_ontology:measurement(sece_su_t2008, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2008, 0.85).
narrative_ontology:measurement(sece_su_t2012, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2012, 0.87).
narrative_ontology:measurement(sece_su_t2016, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2016, 0.89).
narrative_ontology:measurement(sece_su_t2020, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'secession_legitimacy_boundary' kernel, focusing on the popular sovereignty argument. It is linked to sibling readings that offer alternative framings of secession legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
