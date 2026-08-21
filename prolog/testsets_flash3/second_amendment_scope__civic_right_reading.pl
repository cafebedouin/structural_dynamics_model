% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment: Individual Right Conditioned on Civic Militia Participation
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the 'civic right' reading of the Second
 *   Amendment, which interprets the right to bear arms as an individual
 *   right, but one that is conditioned on or closely tied to participation in
 *   a well-regulated militia. This reading seeks to balance individual
 *   liberty with civic duty and state authority, often emphasizing the
 *   historical context of the amendment. It stands in contrast to purely
 *   'individual right' or 'collective right' interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.45).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.3).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment: Individual Right Conditioned on Civic Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '1e72e5d1-664b-44b5-af8d-9c13b9d37dd9').
narrative_ontology:cs_kernel_codification('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', fixed_text).
narrative_ontology:cs_authority_grounding('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', lineage).
narrative_ontology:cs_interpretation_layer_present('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9').
narrative_ontology:cs_reading_relation('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', foundational, individual_right_tied_to_civic_duty).
narrative_ontology:cs_axiom_status(individual_right_tied_to_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', individual_right_tied_to_civic_duty, deontological).
narrative_ontology:cs_axiom('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', secondary, state_sovereignty_over_militia_affirmed).
narrative_ontology:cs_axiom_status(state_sovereignty_over_militia_affirmed, holdable).
narrative_ontology:cs_axiom_grounding('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', state_sovereignty_over_militia_affirmed, conventional).
narrative_ontology:cs_reference_frame('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1e72e5d1-664b-44b5-af8d-9c13b9d37dd9', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_governments).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, individuals_unwilling_to_serve).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, federal_government).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republicanism).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, states_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals possess the right to bear arms, but this right is understood to be contingent on their potential or actual participation in a civic militia. They benefit from the right but bear the implicit obligation of service.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, biographical, constrained, national).

% States retain significant authority to regulate firearms and organize militias, seeing the Second Amendment as primarily protecting their capacity for self-defense and public order through a citizen militia. They administer militia laws and firearms regulations.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who do not wish to participate in a militia, or who are deemed ineligible, may face restrictions on their right to bear arms that would not apply under an 'individual right' interpretation. They bear the cost of conditional access to firearms.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, individuals_unwilling_to_serve, payer,
    powerless, immediate, constrained, local).

% The federal government's power to regulate firearms is constrained by the states' authority to maintain militias and the individual's conditional right. It faces resistance when attempting to enact broad federal gun control measures.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Advocates for an expansive individual right to bear arms, often without militia conditioning, find this reading restrictive. They would argue for fewer regulations and a broader scope of ownership, but their arguments are often sidelined by the civic militia focus.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, gun_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Advocates for stricter gun control measures find this reading too permissive compared to a purely collective right, as it still grants individuals a conditional right. They would argue for greater federal regulatory power, but this reading limits that scope.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, gun_control_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the right to bear arms with the civic duty of militia participation, aiming to ensure a well-regulated citizenry capable of self-defense and public order, particularly at the state level.
% TRANSFER_FUNCTION: Transfers the responsibility for maintaining public order and defense, in part, to an armed citizenry organized through state militias, while transferring some regulatory authority from the federal government to the states.
% ABSENT_VOICES: Advocates for both an unconditioned individual right and a purely collective state right are marginalized, as this reading attempts to balance both, satisfying neither extreme. Their arguments for broader or narrower interpretations are not fully accommodated.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the legal landscape around firearms would immediately shift. Either an unconditioned individual right or a purely collective state right would likely become dominant, leading to significant changes in gun ownership laws, state-federal power dynamics, and the role of armed citizens in public life.
% FOUNDING_PROBLEM: The founding problem was to balance individual liberty with the need for collective security, particularly in the context of a nascent republic wary of a standing army and reliant on citizen militias for defense.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, alongside state governments, corroborate that the tension between individual rights and collective security, and the role of militias, remains a live issue in constitutional interpretation and public policy, especially concerning federal vs. state power.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).
:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while individuals gain a right, it comes with a civic obligation or potential restriction for those unwilling to serve. Suppression (0.30) is also moderate, as states retain significant regulatory power, but cannot entirely disarm citizens. Theater ratio is low (0.10) because the civic militia concept, while debated, is a genuine functional component of this interpretation, not merely a cover. The metrics reflect a constraint that coordinates rights and duties, but with costs for those who do not fit the civic militia model.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments and those who value civic republicanism, this is a balanced and functional constraint. From the perspective of individuals seeking an unconditioned right, it is an extractive limitation. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens are beneficiaries (low d) as they gain a conditional right. State governments are also beneficiaries/agenda-setters (low d) as their authority to regulate and organize militias is affirmed. Individuals unwilling to serve are targets (high d) as their right is curtailed. The federal government is a target (high d) as its power to enact broad gun control is limited by this reading. Gun rights and gun control advocates are 'excluded' as their preferred interpretations are not fully realized.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_relevance_ambiguity,
    'Is the concept of a ''well-regulated militia'' still functionally relevant in contemporary society, or has its meaning atrophied?',
    'Empirical analysis of modern state militia structures and their actual role in defense and public order, compared to the historical context of the Second Amendment''s drafting.',
    'If the militia concept is deemed functionally irrelevant, the ''civic right'' reading''s justification weakens, potentially shifting its classification towards a more extractive ''snare'' for individuals, or a ''piton'' if the enforcement becomes purely theatrical. If it remains relevant, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_relevance_ambiguity, empirical, 'Uncertainty regarding the contemporary functional relevance of the ''well-regulated militia'' clause.').

omega_variable(
    conditional_right_enforcement_scope,
    'What is the precise scope and mechanism by which the ''conditioned'' aspect of the individual right is enforced? Is it through training, registration, or eligibility requirements?',
    'Judicial rulings clarifying the permissible extent of state regulation tied to militia service, and legislative action defining ''well-regulated militia'' and ''participation'' in modern terms.',
    'A broad and stringent enforcement mechanism would increase extractiveness and suppression for individuals, potentially pushing the classification towards a ''snare''. A narrow or unenforced conditioning would reduce extractiveness, moving it closer to a ''rope'' or even a ''piton'' if the condition becomes purely nominal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_right_enforcement_scope, conceptual, 'Ambiguity in the practical enforcement and scope of the ''conditioned'' aspect of the individual right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__civic_right_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__civic_right_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__civic_right_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__civic_right_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__civic_right_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__civic_right_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__civic_right_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__civic_right_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__civic_right_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__civic_right_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__civic_right_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__civic_right_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__civic_right_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__civic_right_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__civic_right_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, state_gun_control_laws).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, federal_gun_control_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment's scope. It emphasizes the individual right conditioned on militia participation, influencing and being influenced by the purely individual and collective rights readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
