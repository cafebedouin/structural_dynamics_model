% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: UN Charter Article 51: Narrow Armed Attack Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'narrow armed attack' reading of
 *   UN Charter Article 51, which limits the right of self-defense to
 *   responses to an actual or imminent armed attack by a state, attributable
 *   under international law. This reading emphasizes state sovereignty and
 *   the prohibition on the use of force, preserving the authority of the UN
 *   Security Council. It is actively enforced by international institutions
 *   and supported by many states, particularly weaker ones, but faces
 *   significant resistance from powerful states seeking broader
 *   interpretations to address non-state actor threats.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.68).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.75).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "UN Charter Article 51: Narrow Armed Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '6396c518-780a-4fe5-ab69-64fe5016b06b').
narrative_ontology:cs_kernel_codification('6396c518-780a-4fe5-ab69-64fe5016b06b', fixed_text).
narrative_ontology:cs_authority_grounding('6396c518-780a-4fe5-ab69-64fe5016b06b', lineage).
narrative_ontology:cs_interpretation_layer_present('6396c518-780a-4fe5-ab69-64fe5016b06b').
narrative_ontology:cs_reading_relation('6396c518-780a-4fe5-ab69-64fe5016b06b', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_reading_relation('6396c518-780a-4fe5-ab69-64fe5016b06b', article_51_self_defense__unable_unwilling_doctrine_reading, forecloses).
narrative_ontology:cs_axiom('6396c518-780a-4fe5-ab69-64fe5016b06b', foundational, prohibition_on_use_of_force_is_norm).
narrative_ontology:cs_axiom_status(prohibition_on_use_of_force_is_norm, holdable).
narrative_ontology:cs_axiom_grounding('6396c518-780a-4fe5-ab69-64fe5016b06b', prohibition_on_use_of_force_is_norm, deontological).
narrative_ontology:cs_axiom('6396c518-780a-4fe5-ab69-64fe5016b06b', foundational, self_defense_exception_is_narrow).
narrative_ontology:cs_axiom_status(self_defense_exception_is_narrow, holdable).
narrative_ontology:cs_axiom_grounding('6396c518-780a-4fe5-ab69-64fe5016b06b', self_defense_exception_is_narrow, conventional).
narrative_ontology:cs_reference_frame('6396c518-780a-4fe5-ab69-64fe5016b06b', post_wwii_un_charter_framework).
narrative_ontology:cs_drift_state('6396c518-780a-4fe5-ab69-64fe5016b06b', post_9_11_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6396c518-780a-4fe5-ab69-64fe5016b06b', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, un_security_council).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, international_law_regime).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states_seeking_unilateral_force).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_actor_threats).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary body responsible for maintaining international peace and security, whose authority to authorize force is preserved by this narrow interpretation of self-defense. It actively enforces this interpretation through resolutions and diplomatic pressure.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, un_security_council, agenda_setter,
    institutional, civilizational, analytical, global).

% Benefit from the constraint on unilateral force by powerful states, as it reduces the likelihood of intervention against them and strengthens the collective security framework. Their security is enhanced by the requirement for state attribution and actual armed attack.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_states, beneficiary,
    organized, generational, constrained, global).

% Bear the cost of constrained strategic freedom, as their ability to use force unilaterally, especially against non-state actors or perceived emerging threats, is limited by this interpretation. They often advocate for broader readings of Article 51.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, powerful_states_seeking_unilateral_force, payer,
    institutional, biographical, constrained, global).

% Interprets and applies international law, including Article 51, through its advisory opinions and contentious cases. Its jurisprudence often reinforces a narrow reading of self-defense.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% Analyze and debate the interpretation of Article 51, contributing to the normative framework. Many support the narrow reading as essential for the stability of the international legal order.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_law_scholars, observer,
    analytical, biographical, analytical, global).

% Find their ability to respond to non-state actor attacks originating from other territories constrained if the attack cannot be attributed to the host state. This forces them to seek UNSC authorization or rely on the host state's (potentially inadequate) response.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_actor_threats, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior regarding the use of force, preventing unilateral military action and preserving the UN Charter's collective security framework by strictly limiting the self-defense exception.
% TRANSFER_FUNCTION: Transfers strategic freedom (to use force unilaterally) from powerful states to the collective security framework, and legitimacy to multilateral institutions like the UN Security Council. It also transfers the burden of responding to non-state actor threats to the host state or the UNSC.
% ABSENT_VOICES: States advocating for a broader interpretation of self-defense (e.g., against non-state actors without state attribution, or for preemptive strikes) are often present in international legal debates but their arguments are rejected by this reading, which prioritizes state sovereignty and the prohibition on the use of force.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished overnight, powerful states would likely assert broader rights to unilateral force, leading to increased military interventions, erosion of UN authority, and a more anarchic international system where the use of force is less constrained by law.
% FOUNDING_PROBLEM: To prevent unilateral uses of force by states and establish a collective security system after WWII, limiting self-defense to clear cases of armed attack by a state to avoid pretexts for aggression.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UN General Assembly resolutions, and the consistent practice of many states (especially weaker ones) corroborate the original intent and ongoing relevance of this narrow interpretation. However, powerful states and some security analysts contest its adequacy for modern threats, particularly from non-state actors.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is substantial because this reading significantly curtails the strategic freedom of powerful states to use force unilaterally. Suppression (0.75) is high due to the active diplomatic, legal, and political efforts to uphold this interpretation against challenges. Resistance (0.8) is also high, reflecting the ongoing push by some states for more expansive interpretations. Theater ratio (0.15) is low, as the debate and enforcement are genuine, not merely performative. The claimed type is Tangled Rope because it coordinates collective security (benefiting weaker states and the UN) while simultaneously extracting strategic freedom from powerful states through active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of weaker states and the UN Security Council, this constraint is a vital Rope, coordinating collective security and preventing aggression. From the perspective of powerful states seeking unilateral action, it is a Snare, unduly restricting their ability to protect national interests against evolving threats. The engine's per-seat classification will reflect these divergent experiences based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   The UN Security Council and weaker states are beneficiaries, as this reading reinforces their authority and security. Powerful states, especially those seeking to use force unilaterally or against non-state actors without clear state attribution, are the primary targets/payers. States facing non-state actor threats also bear costs if they cannot attribute the attack to a host state, limiting their response options. The international legal regime itself is a beneficiary, as its coherence and authority are maintained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminence_definition_ambiguity,
    'What constitutes ''imminent'' armed attack, and how does it apply to evolving threats, particularly from non-state actors?',
    'Further ICJ jurisprudence, consistent state practice, or a UN General Assembly resolution clarifying the temporal and evidentiary thresholds for imminence.',
    'A stricter definition of imminence would increase the constraint''s suppressive force on potential unilateral action; a looser definition would move it closer to a preventive right, blurring the line with the expansive reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_definition_ambiguity, conceptual, 'Ambiguity in the temporal scope of ''imminent armed attack''.').

omega_variable(
    state_attribution_threshold,
    'What level of state involvement or control is required for a non-state actor''s actions to be ''attributable'' to a state under international law, thereby triggering Article 51 self-defense?',
    'Further ICJ rulings (e.g., Nicaragua case clarification), or a new international convention establishing clear criteria for attribution in the context of non-state actor violence.',
    'A high attribution threshold reinforces the narrow reading, increasing the constraint on states responding to non-state actors. A lower threshold would weaken the narrow reading, moving it towards the ''unable or unwilling'' doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_attribution_threshold, empirical, 'Ambiguity in the criteria for attributing non-state actor actions to a state.').

omega_variable(
    response_to_non_state_actors_gap,
    'Does the narrow reading of Article 51 leave a critical gap in international law regarding effective responses to significant non-state actor threats that cannot be attributed to a state?',
    'Empirical analysis of state security outcomes under the narrow reading versus alternative approaches, or a global consensus on new legal frameworks for non-state actor threats.',
    'If a critical gap is demonstrated, it would increase pressure for the narrow reading to be reinterpreted or for new legal norms to emerge, potentially shifting the constraint''s type towards a more permissive framework for powerful states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(response_to_non_state_actors_gap, preference, 'Whether the narrow reading adequately addresses modern non-state actor threats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(arti_tr_t1970, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(arti_tr_t1990, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.14).
narrative_ontology:measurement(arti_tr_t2010, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(arti_be_t1970, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1970, 0.62).
narrative_ontology:measurement(arti_be_t1990, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.67).
narrative_ontology:measurement(arti_be_t2010, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(arti_su_t1970, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1970, 0.72).
narrative_ontology:measurement(arti_su_t1990, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.74).
narrative_ontology:measurement(arti_su_t2010, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, un_security_council_veto_power).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, prohibition_on_use_of_force).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 51 self-defense kernel, alongside the 'expansive preventive' and 'unable or unwilling doctrine' readings. Each reading instantiates a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
