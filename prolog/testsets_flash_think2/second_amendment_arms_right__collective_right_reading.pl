% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment: Collective Right Reading (State Militia Authority)
 *   domain: Constitutional Law / Political Philosophy / Legal Interpretation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'collective right' reading of the
 *   Second Amendment, which interprets the right to keep and bear arms as
 *   primarily protecting the authority of state governments to maintain
 *   well-regulated militias, rather than an individual right to own firearms
 *   for any purpose. Under this reading, individual ownership is largely
 *   subject to state regulation. The constraint itself, as interpreted,
 *   functions as a coordination mechanism for state defense, with low
 *   inherent extraction, but it enables state-level regulations that can be
 *   highly extractive for individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.2).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.6).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment: Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "Constitutional Law / Political Philosophy / Legal Interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, 'd487c4d2-aa4d-445b-94d5-f2fbd269fb24').
narrative_ontology:cs_kernel_codification('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', fixed_text).
narrative_ontology:cs_authority_grounding('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', lineage).
narrative_ontology:cs_interpretation_layer_present('d487c4d2-aa4d-445b-94d5-f2fbd269fb24').
narrative_ontology:cs_reading_relation('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', foundational, militia_clause_governs_entire_amendment).
narrative_ontology:cs_axiom_status(militia_clause_governs_entire_amendment, holdable).
narrative_ontology:cs_axiom_grounding('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', militia_clause_governs_entire_amendment, conventional).
narrative_ontology:cs_axiom('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', foundational, right_of_the_people_is_collective_not_individual).
narrative_ontology:cs_axiom_status(right_of_the_people_is_collective_not_individual, holdable).
narrative_ontology:cs_axiom_grounding('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', right_of_the_people_is_collective_not_individual, conventional).
narrative_ontology:cs_reference_frame('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', original_state_sovereignty_framework).
narrative_ontology:cs_drift_state('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', contemporary_individual_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d487c4d2-aa4d-445b-94d5-f2fbd269fb24', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militias).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, gun_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, states possess the authority to organize and regulate militias, and to control individual arms ownership outside of that organized context. They benefit from the ability to maintain public order and defense without constitutional impediment to regulation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, agenda_setter,
    institutional, generational, mobile, national).

% As the primary focus of the Second Amendment under this interpretation, organized militias (e.g., National Guard) are constitutionally protected and empowered to fulfill their role in state defense. Their existence and function are secured.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militias, beneficiary,
    organized, biographical, constrained, national).

% Individuals who own firearms for personal use, outside of organized militia service, are subject to plenary state regulation. Their right to bear arms is not constitutionally protected against state or federal infringement under this reading, leading to potential costs of compliance or forfeiture.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia, payer,
    powerless, immediate, constrained, local).

% Organizations and individuals who advocate for an expansive individual right to bear arms find their core claims undermined by this interpretation. They bear the cost of having to argue against a prevailing constitutional understanding that limits individual gun ownership.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_rights_advocates, payer,
    organized, biographical, constrained, national).

% The federal government's power to regulate arms is also less constrained under this reading, particularly regarding individual ownership. It benefits from a broader scope for national security and public safety legislation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Scholars who interpret the Second Amendment as primarily protecting a collective right to maintain militias. They analyze historical texts and legal precedents to support this view, often engaging in debates with proponents of other readings.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, constitutional_scholars_collective_right, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate state defense capacity and public safety by ensuring states can maintain well-regulated militias and control individual arms ownership, preventing a fragmented or anarchic approach to armed force.
% TRANSFER_FUNCTION: Transfers primary authority over arms from individuals to state governments, and transfers the burden of collective defense to organized, state-controlled militias.
% ABSENT_VOICES: Proponents of an individual right to bear arms, particularly those who believe it pre-exists government, are structurally excluded from the core interpretive framework of this reading. They would argue that this interpretation disarms law-abiding citizens and undermines a fundamental liberty.
% DISAPPEARANCE_RATIONALE: If the Second Amendment, as interpreted by the collective right reading, vanished overnight, the constitutional basis for state militia authority and state regulation of firearms would be severely undermined. This would lead to a chaotic reorganization of state defense capabilities and public safety frameworks, potentially resulting in a proliferation of unregulated private arms and a weakening of state control over armed force.
% FOUNDING_PROBLEM: The founding problem was to ensure the security of free states by allowing them to maintain well-regulated militias, while also addressing concerns about a powerful federal standing army and preventing the federal government from disarming the states' primary defense forces.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the founding era debates, early state militia laws, and a significant body of legal scholarship from outside the immediate beneficiaries (e.g., some historians, legal scholars not directly involved in state government) corroborate that ensuring state defense capacity was a central concern, though the extent to which it excluded individual rights is heavily debated.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `claimed_type` is 'rope' because, from the perspective of this reading, the Second Amendment primarily serves a coordination function for states to ensure their security. `extractiveness` is low (0.2) because the constitutional constraint itself, under this interpretation, is not seen as extracting from its primary beneficiaries (states); rather, it empowers them. `suppression` is moderate (0.6) because this reading enables states to suppress individual arms ownership, which is a significant coercive power. `theater_ratio` is low (0.1) as this is a functional, rather than performative, interpretation of constitutional text. `resistance` is high (0.7) due to strong opposition from proponents of individual rights readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this reading of the Second Amendment is a vital coordination mechanism for public safety and defense. From the perspective of individual gun owners and their advocates, it is a highly suppressive constraint that enables the state to disarm them. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias are the primary beneficiaries, as this reading secures their authority and existence. Individual gun owners outside of militia contexts and gun rights advocates are the payers/targets, as their claims are subordinated to state power. The federal government also benefits from a less constrained regulatory scope. This structural asymmetry drives the divergence in perceived extractiveness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_intent_ambiguity,
    'Was the original intent of the Second Amendment primarily to protect state militias, an individual right, or a civic duty?',
    'Further historical and linguistic analysis of founding-era documents, state constitutions, and contemporary legal commentaries, potentially leading to a consensus among historians.',
    'Resolution would either strengthen the ''collective right'' interpretation (if intent was primarily state-centric) or weaken it (if intent was primarily individual or civic), potentially shifting its perceived legitimacy and extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_intent_ambiguity, empirical, 'Ambiguity regarding the historical intent behind the Second Amendment.').

omega_variable(
    militia_concept_modern_relevance,
    'How does the concept of a ''well-regulated militia'' apply in the context of modern military and law enforcement structures, and advanced weaponry?',
    'Legal and political consensus on the contemporary definition and role of a ''militia'' in constitutional law, potentially through Supreme Court rulings or legislative action.',
    'If the militia concept is deemed largely anachronistic, the ''collective right'' reading''s foundational premise weakens, potentially leading to reclassification or a shift towards other readings. If it retains strong modern relevance, the reading''s stability is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_concept_modern_relevance, conceptual, 'The evolving relevance of the ''militia'' concept.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__collective_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__collective_right_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__collective_right_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__collective_right_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__collective_right_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(seco_tr_t50, second_amendment_arms_right__collective_right_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__collective_right_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__collective_right_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__collective_right_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__collective_right_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__collective_right_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement(seco_be_t50, second_amendment_arms_right__collective_right_reading, base_extractiveness, 50, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__collective_right_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__collective_right_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__collective_right_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__collective_right_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__collective_right_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(seco_su_t50, second_amendment_arms_right__collective_right_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment, each with different structural properties and classifications. They are linked as a constraint family, with each reading representing a different interpretation of the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
