% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Insurrectionist Reading: Armed Resistance Capacity Against Tyranny
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The insurrectionist reading of the Second Amendment claims the right to
 *   possess military-grade arms in order to preserve citizen capacity for
 *   armed resistance against governmental tyranny. This is ONE READING of the
 *   contested Second Amendment kernel — distinct from the individual-right
 *   reading (which grounds the right in pre-existing personal liberty, not
 *   resistance capacity) and the militia-conditioned reading (which treats
 *   the prefatory militia clause as limiting scope). The insurrectionist
 *   reading's structural claim is that individual armed capacity serves as an
 *   implicit constitutional check on state power consolidation. The cost
 *   structure is asymmetric: armed citizens and scholars advocating the
 *   reading benefit from constitutional legitimacy for their armed status;
 *   state security apparatus, public safety stakeholders, and potential
 *   conflict civilians bear the costs. The reading treats any state
 *   disarmament effort as a tyranny precursor and delegitimizes it within its
 *   own framework, creating a constraint that is substantially extractive
 *   toward safety constituencies while being justified as deterrent
 *   coordination for the beneficiary set.
 *
 * KEY AGENTS:
 *   - armed_citizens_claiming_deterrent: Organized gun-owner networks, militia movements, constitutional advocates — benefit from the reading through legitimation of armed status and deterrent framing. Identity-locked (exiting means abandoning the liberty narrative entirely).
 *   - state_security_apparatus: Federal and state law enforcement, military oversight bodies — bear costs through threat assessment, constraint on disarmament policy, delegitimization of security operations. Constrained exit (cannot abandon security mission; cannot reframe the threat without losing authority).
 *   - potential_conflict_civilians: Would-be victims of armed conflict — trapped and powerless, bearing externalized risk from the reading's logical endpoint without voice in its creation.
 *   - public_safety_stakeholders: Crime prevention advocates, public health researchers, communities experiencing gun violence — victim set bearing empirical harm while excluded from constitutional framing.
 *   - constitutional_scholars_insurrectionist: Institutional agenda-setters transmitting the reading through law schools, amicus briefs, historical reinterpretation. Mobile exit but professionally invested in the reading's coherence.
 *   - supreme_court_majority: Authoritative interpreter of the Second Amendment; gatekeeps whether the insurrectionist reading gains constitutional legitimacy. Analytical position.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.72).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Insurrectionist Reading: Armed Resistance Capacity Against Tyranny").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'abe8ba21-f345-4f3c-8968-c3aea91b932f').
narrative_ontology:cs_kernel_codification('abe8ba21-f345-4f3c-8968-c3aea91b932f', fixed_text).
narrative_ontology:cs_authority_grounding('abe8ba21-f345-4f3c-8968-c3aea91b932f', lineage).
narrative_ontology:cs_interpretation_layer_present('abe8ba21-f345-4f3c-8968-c3aea91b932f').
narrative_ontology:cs_reading_relation('abe8ba21-f345-4f3c-8968-c3aea91b932f', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('abe8ba21-f345-4f3c-8968-c3aea91b932f', second_amendment_boundary__militia_conditioned_reading, coexists_with).
narrative_ontology:cs_axiom('abe8ba21-f345-4f3c-8968-c3aea91b932f', foundational, armed_resistance_capacity_deters_tyranny).
narrative_ontology:cs_axiom_status(armed_resistance_capacity_deters_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('abe8ba21-f345-4f3c-8968-c3aea91b932f', armed_resistance_capacity_deters_tyranny, empirically_contingent).
narrative_ontology:cs_axiom('abe8ba21-f345-4f3c-8968-c3aea91b932f', foundational, individual_possession_instrumental_to_collective_deterrent).
narrative_ontology:cs_axiom_status(individual_possession_instrumental_to_collective_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('abe8ba21-f345-4f3c-8968-c3aea91b932f', individual_possession_instrumental_to_collective_deterrent, deontological).
narrative_ontology:cs_reference_frame('abe8ba21-f345-4f3c-8968-c3aea91b932f', citizen_armed_deterrent_against_state_consolidation).
narrative_ontology:cs_drift_state('abe8ba21-f345-4f3c-8968-c3aea91b932f', contemporary_uncontested_federal_power_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('abe8ba21-f345-4f3c-8968-c3aea91b932f', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, anti_tyranny_narrative_holders).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, potential_conflict_civilians).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, public_safety_stakeholders).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the reading transfers constitutional authority from democratically accountable safety policy toward armed-citizen constituencies and anti-tyranny narrative holders, against the demonstrated preferences of public safety majorities. The transfer is justified by appeal to a hypothetical (permanent governmental tyranny risk) that has no empirical instantiation in contemporary American governance. Suppression is substantial (0.72) because the reading suppresses alternative constitutional narratives (militia-conditioned, narrower individual-right) and delegitimizes safety-based regulation as tyranny precursor. Theater is moderate (0.48) because the reading's deterrent function is largely performative — the actual deterrence capacity is untested and the constraint's persistence depends on political advocacy rather than on empirical demonstration of the deterrent effect. Accessibility collapse is moderate (0.61) because alternatives exist (militia-conditioned reading, public safety framing) but require constitutional reclassification, which is institutionally difficult. Resistance is high (0.78) because the reading meets substantial organized opposition from public safety constituencies, legal scholars, and state security apparatus, which continue to contest the reading's premises and empirical claims.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (state security apparatus, public safety stakeholders) experience this as extraction justified by hypothetical rather than empirical coordination benefit. The beneficiary seat (armed citizens) experiences it as legitimate deterrent coordination. The excluded seats (militia-conditioned scholars) experience it as constitutional misinterpretation. The engine's per-seat computation should reveal that the reading is tangled_rope from the beneficiary perspective (genuine coordination + extraction of authority) and snare from the victim perspectives (extraction without compensation or empirical deterrent benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens claiming deterrent benefit: d near 0.0 (beneficiary). They receive constitutional legitimation and deterrent framing without bearing the safety costs; their exit is identity-locked (the reading is inseparable from their self-concept as liberty-defenders). State security apparatus: d near 1.0 (target). They are delegitimized by the reading's tyranny-precursor framing and constrained in policy options; they cannot exit without abandoning their authority claim. Public safety stakeholders: d near 1.0 (victim). They bear empirical harm (elevated homicide, suicide, accident rates) while excluded from the constitutional framing. Potential conflict civilians: d at 1.0 (pure target). They face externalized risk of armed conflict with zero voice. Constitutional scholars (insurrectionist): d near 0.2 (slight beneficiary). They gain institutional authority through the reading without bearing the direct safety costs. Supreme court: d near 0.5 (analytical, symmetric). They interpret but do not collect from the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The insurrectionist reading exhibits incipient mandatrophy: it claims to solve a permanent founding problem (tyranny prevention) but operates in a constitutional context where tyranny has never materialized despite massive increases in state power and federal enforcement capacity. The empirical content of the founding problem is contested — scholars disagree on whether the founders intended the Second Amendment as a general anti-tyranny check or as a specific militia-regulation provision. The reading persists by framing any state disarmament effort as a tyranny precursor, which prevents empirical falsification: if tyranny never happens, the reading claims this proves the deterrent worked; if state power grows unchecked, the reading claims this proves disarmament enables tyranny. This circular epistemic structure is characteristic of mandatrophy — the reading's persistence no longer depends on solving the founding problem but on maintaining the perpetual threat narrative. Theater ratio rising from 0.35 to 0.48 reflects increasing performativity: political advocacy defending the reading grows while actual deterrent efficacy (the coordination justification) remains unmeasured and untestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_intent_ambiguity,
    'Did the founders explicitly intend the Second Amendment as a general anti-tyranny check (insurrectionist reading) or as a militia-regulation provision (militia-conditioned reading)?',
    'Historical scholarship comparing founding-era writings, state constitutions, anti-federalist opposition to standing armies, and the contextual placement of the right in a bill of individual rights vs. revolutionary powers.',
    'If founders intended general anti-tyranny check, the insurrectionist reading gains historical legitimacy and the military-grade arms extension becomes more defensible. If founders intended militia-regulation, the militia-conditioned reading gains primacy and state regulation becomes constitutionally permissible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_intent_ambiguity, empirical, 'Whether founding intent supports insurrectionist or militia-conditioned reading.').

omega_variable(
    deterrent_efficacy_untestability,
    'Is the claim that armed citizen capacity deters governmental tyranny empirically testable, or is it inherently unfalsifiable (absence of tyranny proves the deterrent worked; presence of unchecked state power proves disarmament enables tyranny)?',
    'Comparative historical analysis of tyranny emergence in disarmed vs. armed populations; theoretical analysis of whether deterrence can be measured when the adverse event does not occur.',
    'If the deterrent claim is unfalsifiable, the reading exhibits mandatrophy: it persists through circular epistemic structure rather than empirical validation. If testable, the claim becomes falsifiable by evidence of tyranny emergence despite armed capacity or absence of tyranny despite disarmament.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrent_efficacy_untestability, conceptual, 'Whether the deterrent claim is empirically testable or structurally unfalsifiable.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the preservation of armed resistance capacity a genuine coordination problem (solving a public good) or is it extractive positioning that redistributes constitutional authority from democratic safety majorities to armed constituencies?',
    'Comparison of the empirical harm from armed capacity (measurable: homicide, suicide, accident rates) against the empirical benefit from deterrence (unmeasured and untestable). If harm >> benefit, the constraint is extractive even if the beneficiary motivation is genuine.',
    'If the constraint is net-extractive, it should be classified as snare or tangled_rope from victim perspectives, despite the beneficiary''s genuine belief in the deterrent function. If the harm-benefit analysis shows net coordination, the rope or coordination framing becomes defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the constraint''s coordination benefit exceeds its empirical safety costs.').

omega_variable(
    identity_lock_mechanism,
    'For armed citizens claiming deterrent, is the exit option truly identity-locked (the reading is inseparable from self-concept as liberty-defender) or is it constrained by political and social pressure?',
    'Ethnographic study of gun-owner communities examining whether rejection of the insurrectionist reading is experienced as existential identity threat or as pragmatic policy disagreement.',
    'If truly identity-locked, the armed-citizen seat has near-zero exit mobility and the directionality computation should reflect high d-value persistence. If constrained-but-not-locked, exit is possible with political cost, which would lower the effective d-value and suggest higher extraction than currently computed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether the identity-lock is structural or socially contingent.').

omega_variable(
    reading_foreclosure_test,
    'Does the insurrectionist reading logically foreclose the militia-conditioned reading, or do both remain live positions across different constitutional coalitions?',
    'Test whether a party could simultaneously hold both readings (insurrectionist premise that armed capacity deters tyranny AND militia-conditioned premise that the prefatory clause limits scope to organized defense). If logically incompatible, the relation is forecloses; if different parties can hold both, the relation is coexists_with.',
    'If forecloses, one reading must eventually be rejected and the Second Amendment will resolve into a single binding constraint. If coexists_with, the constitutional contest is permanent and the three readings will persist as competing constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether insurrectionist reading logically forecloses militia-conditioned reading or both remain live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(seco_tr_t8, second_amendment_boundary__insurrectionist_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(seco_tr_t16, second_amendment_boundary__insurrectionist_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(seco_tr_t24, second_amendment_boundary__insurrectionist_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(seco_tr_t32, second_amendment_boundary__insurrectionist_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__insurrectionist_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(seco_be_t8, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(seco_be_t16, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(seco_be_t24, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(seco_be_t32, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(seco_su_t8, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(seco_su_t16, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(seco_su_t24, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(seco_su_t32, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__insurrectionist_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, tyranny_precursor_framing__state_disarmament).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, public_safety_regulation__federal_firearms).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel decomposes into three structurally distinct constraints based on reading the prefatory militia clause as (1) a non-limiting purpose clause (individual_right_reading), (2) a resistance-capacity preservation clause (insurrectionist_reading — THIS CONSTRAINT), or (3) a scope-limiting militia-conditioned clause (militia_conditioned_reading). Each reading instantiates a different constraint with different beneficiary/victim structures, different ε values, and different constitutional implications. All three readings coexist as live political positions; none has achieved definitive judicial adoption. The insurrectionist_reading links upstream to individual_right_reading (shares the core premise that the operative clause protects individual possession) and downstream to tyranny_precursor_framing (the reading's logical endpoint that any state disarmament effort is a tyranny precursor). The constraint family is linked bidirectionally: the individual_right_reading influences the insurrectionist_reading by establishing that individual possession is constitutionally protected; the insurrectionist_reading influences tyranny_precursor_framing by assigning moral weight to armed deterrence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
