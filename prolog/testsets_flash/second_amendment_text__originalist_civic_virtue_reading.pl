% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment: Originalist Civic Virtue Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents an originalist reading of the Second
 *   Amendment, focusing on the civic republican ideal of a 'well-regulated
 *   militia' as a universal armed citizenry. The right to keep and bear arms
 *   is understood primarily in the context of this collective capacity for
 *   defense and as a check on government power, rather than as an individual
 *   right for personal self-defense or a right subject to extensive state
 *   regulation. It is claimed as a Rope because it coordinates collective
 *   security with minimal extraction, imposing civic duty rather than rent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.1).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.05).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment: Originalist Civic Virtue Reading").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '9dd78331-4522-4807-b309-06bb41109453').
narrative_ontology:cs_kernel_codification('9dd78331-4522-4807-b309-06bb41109453', fixed_text).
narrative_ontology:cs_authority_grounding('9dd78331-4522-4807-b309-06bb41109453', lineage).
narrative_ontology:cs_interpretation_layer_present('9dd78331-4522-4807-b309-06bb41109453').
narrative_ontology:cs_reading_relation('9dd78331-4522-4807-b309-06bb41109453', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('9dd78331-4522-4807-b309-06bb41109453', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('9dd78331-4522-4807-b309-06bb41109453', foundational, armed_citizenry_secures_free_state).
narrative_ontology:cs_axiom_status(armed_citizenry_secures_free_state, holdable).
narrative_ontology:cs_axiom_grounding('9dd78331-4522-4807-b309-06bb41109453', armed_citizenry_secures_free_state, deontological).
narrative_ontology:cs_axiom('9dd78331-4522-4807-b309-06bb41109453', foundational, militia_is_whole_body_of_people).
narrative_ontology:cs_axiom_status(militia_is_whole_body_of_people, holdable).
narrative_ontology:cs_axiom_grounding('9dd78331-4522-4807-b309-06bb41109453', militia_is_whole_body_of_people, conventional).
narrative_ontology:cs_reference_frame('9dd78331-4522-4807-b309-06bb41109453', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('9dd78331-4522-4807-b309-06bb41109453', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9dd78331-4522-4807-b309-06bb41109453', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, the_citizenry_as_political_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, individual_citizens).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republicanism_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, popular_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of a well-regulated militia, understood as the entire body of the people capable of bearing arms, ensuring the capacity for collective defense and resistance against tyranny. The right is tied to the civic duty and capacity of citizens to participate in the common defense.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, the_citizenry_as_political_community, beneficiary,
    institutional, generational, identity_locked, national).

% Have the responsibility to organize and train the militia, but are constrained by the understanding that the right of the people to keep and bear arms is fundamental to the militia's existence. Their regulatory power is limited to ensuring the militia's effectiveness, not disarming the populace.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Bear the civic duty and responsibility of maintaining arms and being prepared for militia service. While they possess the right, it is primarily understood through the lens of their contribution to the collective good, rather than purely personal self-defense. The 'cost' is the civic obligation.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, individual_citizens, payer,
    moderate, biographical, constrained, local).

% Observes and interprets the Second Amendment, with its role primarily to ensure states can maintain a militia and that the people's right to bear arms for that purpose is not infringed. Its power to regulate is secondary to the civic purpose of the right.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the capacity for collective defense by ensuring a broad, armed citizenry capable of forming a militia, thereby providing a check on potential government overreach and securing a free state.
% TRANSFER_FUNCTION: Transfers the responsibility for collective security and civic virtue to the armed citizenry, ensuring a distributed capacity for defense rather than relying solely on a standing army.
% ABSENT_VOICES: Those who advocate for a purely individualistic interpretation of the right, detached from civic duty, or those who seek extensive federal control over firearms, would object to the emphasis on the collective, civic-republican purpose.
% DISAPPEARANCE_RATIONALE: If this understanding vanished, the foundational concept of an armed citizenry as a check on power would erode, potentially leading to a more centralized and less accountable state, and a shift in the balance of power between the government and the people.
% FOUNDING_PROBLEM: The problem of ensuring a free state by preventing both foreign invasion and domestic tyranny, relying on a citizen militia rather than a standing army, which was viewed with suspicion.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era, political theorists of civic republicanism, and some constitutional scholars attest to the centrality of the citizen-soldier concept to the Second Amendment's original understanding. This corroboration comes from academic and historical analysis, not directly from benefiting parties.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.1) because the constraint primarily imposes a civic duty rather than extracting rents; the 'cost' is the responsibility of being armed and ready for service. Suppression is low (0.05) as the constraint is meant to empower, not restrict, the citizenry. Theater ratio is low (0.1) as the civic function was genuinely understood and practiced in the founding era. The metrics remain stable over the interval as this reading's structural properties were largely consistent during this period.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the citizenry, this is a foundational right enabling self-governance and security. From the perspective of state governments, it is a framework for organizing defense. There is little 'gap' in the sense of opposed interests, but rather different roles within a shared civic understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry as a political community is the primary beneficiary, gaining collective security and a check on tyranny. State governments are agenda-setters, responsible for organizing the militia within the bounds of this right. Individual citizens are 'payers' in the sense of bearing the civic duty, but are also beneficiaries of the collective security. The federal government is an observer, ensuring the right is not infringed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the civic duty as extraction by emphasizing the collective benefit and the non-coercive nature of the 'payment' (civic responsibility). It highlights the coordination function of an armed citizenry for a free state, rather than a purely extractive or purely individualistic interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_duty_vs_individual_right,
    'Is the Second Amendment primarily a right tied to civic duty and collective defense, or an individual right for personal self-defense?',
    'Further historical and legal scholarship on founding-era intent, and judicial rulings that explicitly prioritize one interpretation over the other.',
    'If resolved as primarily civic duty, regulations aimed at militia effectiveness would be permissible; if individual right, regulations would be more strictly scrutinized against personal liberty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civic_duty_vs_individual_right, conceptual, 'Ambiguity in the primary purpose of the Second Amendment right.').

omega_variable(
    militia_definition_evolution,
    'How has the ''well-regulated militia'' concept evolved from the founding era to contemporary understandings, and does this reading remain applicable?',
    'Analysis of legislative history, military organization changes, and judicial interpretations of the militia clause over time.',
    'If the militia concept has fundamentally changed, this reading''s relevance might diminish, potentially shifting classification towards a more ''piton'' or ''snare'' if the original function is lost but the constraint persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_evolution, empirical, 'Evolution of the ''militia'' concept and its impact on the reading''s applicability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1791, 1865).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1810, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1810, 0.1).
narrative_ontology:measurement(seco_tr_t1830, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1830, 0.1).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(seco_tr_t1865, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1865, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1810, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1810, 0.1).
narrative_ontology:measurement(seco_be_t1830, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1830, 0.1).
narrative_ontology:measurement(seco_be_t1850, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1850, 0.1).
narrative_ontology:measurement(seco_be_t1865, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1865, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1791, 0.05).
narrative_ontology:measurement(seco_su_t1810, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1810, 0.05).
narrative_ontology:measurement(seco_su_t1830, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1830, 0.05).
narrative_ontology:measurement(seco_su_t1850, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1850, 0.05).
narrative_ontology:measurement(seco_su_t1865, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1865, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment text, each with different structural properties and implications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
