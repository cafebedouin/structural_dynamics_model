% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Commons Governance
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint models the perspective that software control is
 *   fundamentally a question of commons governance, requiring collective
 *   management rather than absolute individual freedom or absolute property
 *   rights. It positions both 'absolute freedom' and 'absolute property'
 *   positions as 'victims' in the sense that their absolutist claims are
 *   curtailed for the sake of collective sustainability. The constraint is
 *   claimed as a 'rope' because it aims for genuine coordination and mutual
 *   benefit, with moderate extraction arising from the necessary compromises
 *   of collective action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.35).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.2).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Commons Governance").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, 'bd744375-ef45-41cb-bc2b-7aaa253f8fb8').
narrative_ontology:cs_kernel_codification('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', distributed).
narrative_ontology:cs_authority_grounding('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', practice).
narrative_ontology:cs_interpretation_layer_present('bd744375-ef45-41cb-bc2b-7aaa253f8fb8').
narrative_ontology:cs_reading_relation('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', foundational, digital_resources_are_shared_commons).
narrative_ontology:cs_axiom_status(digital_resources_are_shared_commons, holdable).
narrative_ontology:cs_axiom_grounding('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', digital_resources_are_shared_commons, deontological).
narrative_ontology:cs_axiom('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', foundational, collective_governance_ensures_sustainability).
narrative_ontology:cs_axiom_status(collective_governance_ensures_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', collective_governance_ensures_sustainability, instrumental).
narrative_ontology:cs_reference_frame('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', ostrom_principles_for_commons).
narrative_ontology:cs_drift_state('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', contemporary_digital_enclosures, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('bd744375-ef45-41cb-bc2b-7aaa253f8fb8', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, digital_infrastructure_users).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_freedom_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_property_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities actively participate in defining and enforcing the rules for shared digital infrastructure, balancing individual contributions with collective benefit. They benefit from sustainable, collectively managed resources but are constrained by the need for consensus.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, agenda_setter,
    organized, generational, constrained, global).

% Users benefit from stable, well-maintained, and equitably governed digital commons. Their access and participation are facilitated by the commons rules, but they may face limitations on individual 'freedom' or 'property' in favor of collective good.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, digital_infrastructure_users, beneficiary,
    moderate, biographical, mobile, global).

% Advocates for absolute user freedom find their ideals constrained by the need for collective governance and rules. They 'pay' by accepting limitations on their ability to modify or distribute software without regard for community norms or shared infrastructure stability.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_freedom_advocates, payer,
    moderate, generational, identity_locked, global).

% Advocates for absolute property rights find their ability to exclusively control and monetize software limited by the commons framework. They 'pay' by ceding some proprietary control to collective management, which they may perceive as an infringement on their rights.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_property_advocates, payer,
    powerful, generational, identity_locked, global).

% These advocates view software control as a practical choice for development. While they may appreciate the benefits of open collaboration, they observe the commons framework from a utilitarian perspective, assessing its effectiveness rather than its foundational principles.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, pragmatic_openness_advocates, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for collective decision-making and resource allocation for shared digital infrastructure, preventing 'tragedy of the commons' scenarios and ensuring sustainable development and maintenance.
% TRANSFER_FUNCTION: Transfers decision-making power and some individual control (from both 'freedom' and 'property' absolutists) to a collective governance body, in exchange for shared benefits and sustainable infrastructure.
% ABSENT_VOICES: Those who believe software should be entirely unregulated or entirely privatized are marginalized in this framework, as their foundational premises are incompatible with collective governance. They would argue for their respective absolutist positions.
% DISAPPEARANCE_RATIONALE: If the commons governance framework vanished, the shared digital infrastructure would likely fragment, leading to increased 'tragedy of the commons' issues, unsustainable development, and a return to unmanaged conflicts between absolutist positions, forcing a reorganization of how digital resources are managed.
% FOUNDING_PROBLEM: The problem of managing shared digital resources sustainably, preventing monopolization or fragmentation, and ensuring equitable access and participation in the face of competing absolutist claims (absolute freedom vs. absolute property).
% FOUNDING_PROBLEM_CORROBORATION: Academic research in commons theory, successful open-source projects with strong governance models, and policy discussions around digital public goods corroborate the ongoing need for such a framework, from outside the immediate beneficiary group.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).
:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the inherent costs and compromises of collective governance, where individual desires are balanced against community needs. Suppression (0.20) is low, as the framework relies more on participation and shared understanding than coercion, though it does suppress absolutist claims. Theater ratio (0.10) is low, indicating that the governance functions are largely genuine and effective. Accessibility collapse (0.40) is moderate, as alternatives (pure freedom or pure property) are conceptually available but structurally disfavored by the framework. Resistance (0.30) is also moderate, coming from those who prefer absolutist approaches.
 *
 * PERSPECTIVAL GAP:
 *   The core tension lies between the collective benefit emphasized by this reading and the individualistic claims of the 'freedom' and 'property' readings. While this reading sees its compromises as necessary for coordination, the absolutist readings perceive them as illegitimate extraction or suppression of fundamental rights. The engine's per-seat classification will highlight how the same governance structure is experienced as beneficial coordination by some and as a cost by others.
 *
 * DIRECTIONALITY LOGIC:
 *   Stakeholder communities and digital infrastructure users are beneficiaries, gaining from stable and managed shared resources. Advocates for absolute freedom and absolute property are 'payers' in this framework, as their maximalist positions are necessarily curtailed for the commons to function. Pragmatic openness advocates are observers, evaluating the system's efficacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a valid and distinct reading of the ''software_control_legitimacy'' kernel, or does it conflate aspects of other readings?',
    'Detailed comparative analysis of the normative foundations and practical implications of each reading, ensuring unique structural deltas and non-overlapping core axioms.',
    'If not distinct, this reading would be subsumed into a sibling, altering its classification and the overall structure of the ''software_control_legitimacy'' constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the distinctness of the ''commons_reading'' within the kernel.').

omega_variable(
    commons_rules_extractiveness_variability,
    'How much does the extractiveness of ''commons governance'' vary depending on the specific rules and enforcement mechanisms adopted by different stakeholder communities?',
    'Empirical study of diverse digital commons projects, correlating specific governance models (e.g., strict vs. permissive licensing, centralized vs. distributed decision-making) with measured extractiveness from individual contributors/users.',
    'If extractiveness varies widely, the ''commons_reading'' itself might encompass a range of constraint types (from Rope to Tangled Rope), requiring further decomposition or a more nuanced extractiveness metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_rules_extractiveness_variability, empirical, 'Variability of extraction within the commons governance model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__commons_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__commons_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__commons_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__commons_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__commons_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__commons_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__commons_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__commons_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__commons_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel. Each reading offers a distinct structural interpretation of how software should be controlled, leading to different classifications and stakeholder dynamics. This 'commons_reading' emphasizes collective management over individual absolutism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
