% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Sovereignty as Conditional Responsibility (R2P Doctrine)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint represents the 'conditional responsibility' reading of
 *   Westphalian sovereignty, often associated with the Responsibility to
 *   Protect (R2P) doctrine. It posits that states forfeit their traditional
 *   territorial inviolability when they fail to protect their own populations
 *   from mass atrocities. This reading emerged in response to failures to
 *   prevent genocides and mass killings, challenging the absolute
 *   non-intervention principle. It grants the international community a
 *   legitimate basis for intervention, thereby lowering the threshold for
 *   external interference in domestic affairs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.75).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Sovereignty as Conditional Responsibility (R2P Doctrine)").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, 'cc99afd8-c32c-40fe-9b45-4a155eab61e5').
narrative_ontology:cs_kernel_codification('cc99afd8-c32c-40fe-9b45-4a155eab61e5', formalized).
narrative_ontology:cs_authority_grounding('cc99afd8-c32c-40fe-9b45-4a155eab61e5', lineage).
narrative_ontology:cs_interpretation_layer_present('cc99afd8-c32c-40fe-9b45-4a155eab61e5').
narrative_ontology:cs_reading_relation('cc99afd8-c32c-40fe-9b45-4a155eab61e5', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('cc99afd8-c32c-40fe-9b45-4a155eab61e5', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('cc99afd8-c32c-40fe-9b45-4a155eab61e5', foundational, state_sovereignty_is_conditional).
narrative_ontology:cs_axiom_status(state_sovereignty_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('cc99afd8-c32c-40fe-9b45-4a155eab61e5', state_sovereignty_is_conditional, deontological).
narrative_ontology:cs_axiom('cc99afd8-c32c-40fe-9b45-4a155eab61e5', foundational, international_community_has_responsibility_to_protect).
narrative_ontology:cs_axiom_status(international_community_has_responsibility_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('cc99afd8-c32c-40fe-9b45-4a155eab61e5', international_community_has_responsibility_to_protect, deontological).
narrative_ontology:cs_reference_frame('cc99afd8-c32c-40fe-9b45-4a155eab61e5', post_rwanda_srebrenica_consensus).
narrative_ontology:cs_drift_state('cc99afd8-c32c-40fe-9b45-4a155eab61e5', contemporary_geopolitical_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('cc99afd8-c32c-40fe-9b45-4a155eab61e5', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, states_failing_to_protect_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, sovereignty_absolutists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates when states have failed their responsibility to protect and authorizes interventions. Bears the political and material costs of intervention, but gains moral authority and a framework for action.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_community, agenda_setter,
    institutional, generational, constrained, global).

% Forfeits territorial inviolability and faces potential external intervention, losing control over its domestic affairs and potentially its regime. Its options are to comply with international demands or face military action.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, states_failing_to_protect_populations, payer,
    powerless, immediate, trapped, national).

% Receives the potential benefit of international protection from mass atrocities, which can be life-saving. Their agency is limited to being the object of protection.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity, beneficiary,
    powerless, immediate, trapped, local).

% Gains a legitimate framework and mandate for military intervention in situations of mass atrocities, reducing the political and legal risks associated with such actions. They are the operational arm of the doctrine.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    organized, biographical, mobile, global).

% Expands its adjudicative authority and mandate in international affairs, particularly regarding human rights and state sovereignty. This strengthens its role in shaping global norms and security.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, generational, constrained, global).

% Bears the cost of the erosion of the principle of absolute state sovereignty and non-intervention. They resist the doctrine ideologically and politically, viewing it as a dangerous precedent for external interference.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, sovereignty_absolutists, payer,
    organized, generational, identity_locked, global).

% Their traditional view of state sovereignty as categorical territorial inviolability is sidelined by this doctrine. They are often excluded from the decision-making processes that authorize interventions, despite their strong objections.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, absolute_non_intervention_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international action to protect populations from mass atrocities when states fail to do so, providing a framework for collective security and humanitarian response.
% TRANSFER_FUNCTION: Transfers adjudicative authority over state conduct and the right to territorial inviolability from states failing to protect their populations to the international community, enabling external intervention.
% ABSENT_VOICES: States that prioritize absolute sovereignty and non-intervention, fearing that the doctrine will be abused for geopolitical interests or regime change. Their concerns are often voiced but not structurally integrated into the decision-making process for intervention.
% DISAPPEARANCE_RATIONALE: If the conditional responsibility doctrine vanished overnight, the international community would lose a key, albeit contested, legitimate framework for responding to mass atrocities. This would likely lead to more unchecked atrocities, or to ad-hoc, less legitimate, and potentially more destabilizing interventions, fundamentally reorganizing international security dynamics.
% FOUNDING_PROBLEM: The failure of the international community to prevent or stop genocides and mass atrocities (e.g., Rwanda, Srebrenica) in the late 20th century due to strict adherence to the principle of non-intervention.
% FOUNDING_PROBLEM_CORROBORATION: UN reports, human rights organizations, and international legal scholars (outside of states directly benefiting from intervention) corroborate the ongoing need for such a framework, citing continued instances of mass atrocities and the moral imperative to act, despite the doctrine's controversial application.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because it imposes a significant cost on states that fail their responsibility, potentially leading to loss of sovereignty and regime change. Suppression (0.75) is also high, as the doctrine implies the international community can override state authority, backed by military force. The theater ratio (0.20) is relatively low, reflecting a genuine, albeit often controversial, intent to protect populations, though geopolitical interests can sometimes influence its application. Resistance (0.80) is high due to strong opposition from states prioritizing absolute sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the international community and populations under atrocity, this doctrine is a necessary coordination mechanism for human protection. However, from the perspective of states facing potential intervention or those advocating for absolute sovereignty, it is a highly extractive and suppressive mechanism that erodes fundamental principles of international law. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The international community, humanitarian coalitions, and global governance institutions are beneficiaries, gaining expanded authority and legitimacy. Populations under atrocity are direct beneficiaries of potential protection. States failing to protect their populations are clear targets/payers, facing the loss of inviolability and potential intervention. Sovereignty absolutists are also targets, as their core ideological commitment is challenged and undermined by this doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The conditional nature of this constraint prevents it from being a pure snare, as the extraction (intervention) is theoretically triggered by a specific failure of responsibility, not arbitrary will. However, its active enforcement and the significant costs imposed on target states, coupled with the contested nature of its application, make it a Tangled Rope. The coordination function (protecting populations) is intertwined with asymmetric extraction from failing states, requiring continuous justification and enforcement to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_abuse_risk,
    'Is the conditional responsibility doctrine genuinely applied for human protection, or is it susceptible to abuse as a pretext for geopolitical interests or regime change?',
    'Empirical analysis of intervention outcomes, including post-intervention stability, human rights records, and the presence of non-humanitarian strategic interests of intervening powers.',
    'If consistently abused, the doctrine''s legitimacy would collapse, and its effective extractiveness would be higher, reclassifying it closer to a Snare. If applied consistently for protection, its coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_abuse_risk, empirical, 'Risk of R2P doctrine being used for non-humanitarian ends.').

omega_variable(
    legitimacy_of_adjudication,
    'Who legitimately adjudicates when a state has failed its responsibility to protect, and is that authority universally accepted?',
    'Analysis of international legal consensus, UN Security Council voting patterns, and the acceptance of International Criminal Court (ICC) jurisdiction by various states.',
    'If adjudication is widely contested or seen as illegitimate, the constraint''s suppression is less effective, and its persistence relies more on raw power than normative consensus. This would push it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_adjudication, conceptual, 'Contestation over the authority to determine R2P triggers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__conditional_responsibility, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(west_tr_t1997, westphalia_sovereignty__conditional_responsibility, theater_ratio, 1997, 0.15).
narrative_ontology:measurement(west_tr_t2004, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2004, 0.2).
narrative_ontology:measurement(west_tr_t2011, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2011, 0.22).
narrative_ontology:measurement(west_tr_t2018, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2018, 0.21).
narrative_ontology:measurement(west_tr_t2025, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(west_be_t1997, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 1997, 0.55).
narrative_ontology:measurement(west_be_t2004, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2004, 0.62).
narrative_ontology:measurement(west_be_t2011, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2011, 0.65).
narrative_ontology:measurement(west_be_t2018, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement(west_be_t2025, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(west_su_t1997, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 1997, 0.6).
narrative_ontology:measurement(west_su_t2004, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2004, 0.68).
narrative_ontology:measurement(west_su_t2011, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2011, 0.72).
narrative_ontology:measurement(west_su_t2018, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2018, 0.74).
narrative_ontology:measurement(west_su_t2025, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, un_security_council_veto_power).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, international_criminal_court_jurisdiction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
