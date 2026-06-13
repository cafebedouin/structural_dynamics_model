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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Commons Governance (Commons Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'commons reading' of software control,
 *   where software is viewed as a shared digital infrastructure requiring
 *   negotiated collective management, rather than absolute freedom or
 *   absolute property. It seeks to establish governance mechanisms that
 *   balance diverse stakeholder interests for long-term sustainability. The
 *   claimed type is 'rope' because it aims for genuine coordination and
 *   mutual benefit, though it requires active enforcement to manage the
 *   tension between competing absolutist claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.3).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.2).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Commons Governance (Commons Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, 'cb051bde-bd0b-45b8-acfa-5600fa06968b').
narrative_ontology:cs_kernel_codification('cb051bde-bd0b-45b8-acfa-5600fa06968b', distributed).
narrative_ontology:cs_authority_grounding('cb051bde-bd0b-45b8-acfa-5600fa06968b', practice).
narrative_ontology:cs_interpretation_layer_present('cb051bde-bd0b-45b8-acfa-5600fa06968b').
narrative_ontology:cs_reading_relation('cb051bde-bd0b-45b8-acfa-5600fa06968b', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb051bde-bd0b-45b8-acfa-5600fa06968b', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb051bde-bd0b-45b8-acfa-5600fa06968b', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('cb051bde-bd0b-45b8-acfa-5600fa06968b', foundational, software_as_shared_infrastructure).
narrative_ontology:cs_axiom_status(software_as_shared_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('cb051bde-bd0b-45b8-acfa-5600fa06968b', software_as_shared_infrastructure, conventional).
narrative_ontology:cs_axiom('cb051bde-bd0b-45b8-acfa-5600fa06968b', foundational, negotiated_collective_management_is_optimal).
narrative_ontology:cs_axiom_status(negotiated_collective_management_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('cb051bde-bd0b-45b8-acfa-5600fa06968b', negotiated_collective_management_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('cb051bde-bd0b-45b8-acfa-5600fa06968b', sustainable_digital_commons).
narrative_ontology:cs_drift_state('cb051bde-bd0b-45b8-acfa-5600fa06968b', contemporary_digital_economy, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('cb051bde-bd0b-45b8-acfa-5600fa06968b', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, public_digital_infrastructure).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_freedom_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_property_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from collective management, shared decision-making, and the long-term sustainability of digital infrastructure. Participates in governance but is constrained by the need for consensus and the complexity of managing diverse interests.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, beneficiary,
    organized, generational, constrained, global).

% The abstract entity representing shared digital resources, whose health and longevity are enhanced by a commons-based governance model. It is not an agent but a concept whose 'benefit' is its continued existence and utility.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, public_digital_infrastructure, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(software_control_legitimacy__commons_reading, public_digital_infrastructure).

% Bears the cost of accepting negotiated limits on software use and modification, which they view as an infringement on fundamental user rights. Their identity is often tied to the principle of absolute freedom, making compromise difficult.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_freedom_advocates, payer,
    moderate, biographical, identity_locked, global).

% Bears the cost of relinquishing absolute control over their software creations, accepting that some aspects must be managed collectively. They see this as an erosion of property rights and a disincentive for investment.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_property_advocates, payer,
    powerful, biographical, constrained, global).

% Observes the debate from a position of practical utility, recognizing the benefits of both open and proprietary models. They are not directly extracted from but may be influenced by the outcome of the governance model.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, pragmatic_openness_advocates, observer,
    organized, biographical, mobile, global).

% Responsible for creating legal and regulatory frameworks that define software control. They navigate competing claims and attempt to establish a governance model that balances various interests, often under pressure from different advocacy groups.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for collective decision-making and resource allocation for shared digital infrastructure, preventing fragmentation and ensuring long-term sustainability by balancing diverse interests.
% TRANSFER_FUNCTION: Transfers some individual control (from both absolute freedom and absolute property positions) into a shared governance pool, managed by stakeholder communities for the benefit of the collective digital commons.
% ABSENT_VOICES: Future generations of software users and developers, who would advocate for sustainable, collectively managed digital infrastructure, are not directly represented in current debates but are implicitly considered by the commons reading.
% DISAPPEARANCE_RATIONALE: If the commons reading vanished, the debate would revert to absolutist positions (absolute freedom vs. absolute property), leading to increased conflict, fragmentation of digital infrastructure, and potentially less sustainable software ecosystems. The current negotiated management would collapse.
% FOUNDING_PROBLEM: The problem of managing shared digital resources in a way that avoids both the tyranny of proprietary monopolies and the chaos of unmanaged 'free' access, ensuring long-term health and utility for all stakeholders.
% FOUNDING_PROBLEM_CORROBORATION: Academics in digital commons studies, open-source governance experts, and some public interest technologists corroborate that this problem is live and ongoing, citing historical failures of both purely proprietary and purely 'free' models to sustain shared infrastructure. This corroboration comes from outside the immediate beneficiaries of any single control model.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).

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
 *   Extractiveness (0.3) is moderate, reflecting the necessary compromises and overheads of collective governance, but it is not primarily extractive. Suppression (0.2) is low, as the model relies on participation and negotiation rather than coercion, though some enforcement is needed to prevent absolutist positions from dominating. Theater ratio (0.1) is low, indicating a genuine effort towards functional governance. Accessibility collapse (0.4) is moderate, as it creates a new, managed access model that is neither fully open nor fully closed. Resistance (0.3) is moderate, stemming from those who prefer absolutist approaches.
 *
 * PERSPECTIVAL GAP:
 *   Advocates of absolute freedom or property rights would experience this constraint as extractive, as it limits their preferred mode of control. Stakeholder communities, however, would perceive it as a beneficial coordination mechanism. Policy makers would see it as a necessary framework for managing a complex public good.
 *
 * DIRECTIONALITY LOGIC:
 *   Stakeholder communities and public digital infrastructure are beneficiaries, as the constraint aims to serve their collective interests. Absolute freedom and property advocates are payers, as they must cede some control to the collective. Policy makers act as agenda-setters, mediating and enforcing the commons rules. Pragmatic openness advocates are observers, as their position is more about methodology than fundamental rights or property.
 *
 * MANDATROPHY ANALYSIS:
 *   The commons reading directly addresses the potential for mandatrophy by asserting that the 'founding problem' of managing shared digital infrastructure is 'live'. It prevents mislabeling coordination as extraction by emphasizing the collective benefit and the active, ongoing nature of governance, rather than allowing the constraint to persist due to inertia or hidden rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_overhead_vs_benefit,
    'Does the overhead and complexity of collective governance (transaction costs, decision paralysis) outweigh the benefits of shared management for specific software projects?',
    'Empirical studies comparing the efficiency and innovation outcomes of commons-governed software projects against purely open or proprietary alternatives.',
    'If overhead consistently outweighs benefits, the extractiveness metric for this reading might need to be adjusted upward, potentially shifting its classification towards a ''tangled rope'' for some contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_overhead_vs_benefit, empirical, 'Assessing the true cost-benefit ratio of commons governance in practice.').

omega_variable(
    boundary_definition_ambiguity,
    'Where is the precise boundary between ''shared digital infrastructure'' (subject to commons governance) and ''private innovation'' (subject to property rights or individual freedom)?',
    'Legal precedents, policy guidelines, and community consensus-building processes that explicitly define the scope of commons governance for different types of software components.',
    'Ambiguity in this boundary can lead to ''scope creep'' where the commons model overreaches, increasing extraction from private innovators, or ''enclosure'' where shared resources are privatized, increasing extraction from the public. Resolution would stabilize the extractiveness and suppression metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_definition_ambiguity, conceptual, 'Defining the scope of collective management versus individual control.').

omega_variable(
    natural_vs_constructed_commons,
    'To what extent is the ''digital commons'' a natural emergent property of networked software, versus a constructed legal and social artifact requiring continuous active maintenance?',
    'Historical analysis of digital infrastructure development and comparative studies of different legal regimes for digital resources. If it requires constant legal and social construction, its ''naturalness'' is lower.',
    'If more constructed, the ''emerges_naturally'' aspect (if ever claimed) would be false, and the ''requires_active_enforcement'' would be more central to its persistence, potentially increasing the suppression metric''s weight in classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_commons, conceptual, 'The degree to which the digital commons is an inherent feature or a human construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t2000, software_control_legitimacy__commons_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(soft_tr_t2008, software_control_legitimacy__commons_reading, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(soft_tr_t2016, software_control_legitimacy__commons_reading, theater_ratio, 2016, 0.09).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__commons_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t2000, software_control_legitimacy__commons_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(soft_be_t2008, software_control_legitimacy__commons_reading, base_extractiveness, 2008, 0.3).
narrative_ontology:measurement(soft_be_t2016, software_control_legitimacy__commons_reading, base_extractiveness, 2016, 0.28).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__commons_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t2000, software_control_legitimacy__commons_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(soft_su_t2008, software_control_legitimacy__commons_reading, suppression_requirement, 2008, 0.2).
narrative_ontology:measurement(soft_su_t2016, software_control_legitimacy__commons_reading, suppression_requirement, 2016, 0.18).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__commons_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.15).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel, focusing on a commons governance approach. It contrasts with readings emphasizing absolute freedom, pragmatic openness, or property rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
