% ============================================================================
% CONSTRAINT STORY: asean_ceasefire_2011
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asean_ceasefire_2011, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: asean_ceasefire_2011
 *   human_readable: 2011 ASEAN-mediated Thai-Cambodian Ceasefire Agreement
 *   domain: geopolitical
 *
 * SUMMARY:
 *   Following deadly border clashes near the Preah Vihear temple, Thailand
 *   and Cambodia agreed to an ASEAN-mediated ceasefire in February 2011. This
 *   agreement aimed to de-escalate tensions and prevent further conflict
 *   along the disputed border region. The agreement's effectiveness was
 *   limited, as sporadic clashes continued despite the ceasefire.
 *
 * KEY AGENTS:
 *   - Thai Border Residents: Primary victims (powerless/trapped)
 *   - Cambodian Border Residents: Primary victims (powerless/trapped)
 *   - Thailand: Moderate actor (moderate/constrained)
 *   - Cambodia: Moderate actor (moderate/constrained)
 *   - ASEAN Member States: Primary beneficiaries (institutional/arbitrage)
 *   - International Community: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asean_ceasefire_2011, 0.55).
domain_priors:suppression_score(asean_ceasefire_2011, 0.65).
domain_priors:theater_ratio(asean_ceasefire_2011, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asean_ceasefire_2011, extractiveness, 0.55).
narrative_ontology:constraint_metric(asean_ceasefire_2011, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(asean_ceasefire_2011, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asean_ceasefire_2011, tangled_rope).
narrative_ontology:human_readable(asean_ceasefire_2011, "2011 ASEAN-mediated Thai-Cambodian Ceasefire Agreement").
narrative_ontology:topic_domain(asean_ceasefire_2011, "geopolitical").

domain_priors:requires_active_enforcement(asean_ceasefire_2011).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, asean_member_states).
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, international_community).
narrative_ontology:constraint_victim(asean_ceasefire_2011, thai_border_residents).
narrative_ontology:constraint_victim(asean_ceasefire_2011, cambodian_border_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Thai border residents near Preah Vihear experienced the agreement as a snare. They were trapped in the conflict zone and remained vulnerable despite the ceasefire, as sporadic clashes continued.  They have limited power to influence the agreement.
constraint_indexing:constraint_classification(asean_ceasefire_2011, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Cambodian border residents near Preah Vihear experienced the agreement as a snare. They were trapped in the conflict zone and remained vulnerable despite the ceasefire, as sporadic clashes continued.  They have limited power to influence the agreement.
constraint_indexing:constraint_classification(asean_ceasefire_2011, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Thailand experiences the agreement as a tangled rope. It constrains their military actions and limits their ability to unilaterally resolve the border dispute. However, it also benefits from the agreement by maintaining a semblance of stability and avoiding further escalation of the conflict, bolstering its international image.
constraint_indexing:constraint_classification(asean_ceasefire_2011, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Cambodia experiences the agreement as a tangled rope. It constrains their military actions and limits their ability to unilaterally resolve the border dispute. However, it also benefits from the agreement by maintaining a semblance of stability and avoiding further escalation of the conflict, garnering international support.
constraint_indexing:constraint_classification(asean_ceasefire_2011, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% ASEAN member states benefit from the agreement as it reinforces ASEAN's role as a regional mediator and promotes regional stability. They can arbitrage their position by enhancing their diplomatic influence. While they expend resources, the coordination yields a net positive.
constraint_indexing:constraint_classification(asean_ceasefire_2011, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% The international community benefits from the agreement, as it prevents a larger conflict and maintains regional stability. It promotes adherence to international norms and laws. It observes the situation analytically.
constraint_indexing:constraint_classification(asean_ceasefire_2011, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asean_ceasefire_2011_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asean_ceasefire_2011, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asean_ceasefire_2011, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asean_ceasefire_2011, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(asean_ceasefire_2011_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.55) reflects the continued suffering and displacement of border residents, who bear the brunt of the conflict despite the ceasefire. The suppression score (0.65) reflects the limitations on both Thailand and Cambodia's ability to act unilaterally, as well as the limits on the border residents' ability to influence the situation. The theater ratio (0.30) is relatively low, as the ceasefire was more than just a symbolic gesture, but did not fully resolve the conflict.
 *
 * PERSPECTIVAL GAP:
 *   The Thai and Cambodian border residents experience the agreement as a snare, as they remain vulnerable to sporadic clashes. Thailand and Cambodia experience the agreement as a tangled rope, as it constrains their actions but also benefits them by preventing further escalation. ASEAN member states and the international community benefit from the agreement as it reinforces regional stability and promotes international norms.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the agent's structural position relative to the agreement. Border residents bear the costs and have limited exit options, resulting in a high d value. Thailand and Cambodia experience both costs and benefits, resulting in a moderate d value. ASEAN member states and the international community primarily benefit from the agreement, resulting in a low d value.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating how the agreement can be viewed differently depending on the perspective. It is not simply a case of coordination or extraction, but rather a complex interplay of both. The agreement's effectiveness is limited, and it does not fully address the underlying causes of the border dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_strength,
    'How effective are the enforcement mechanisms of the ceasefire agreement?',
    'Monitoring of border incidents and investigation of violations.',
    'Strong enforcement: Agreement is more likely to be a rope. Weak enforcement: Agreement is more likely to be a piton or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_strength, empirical, 'Strength of ceasefire enforcement mechanisms').

omega_variable(
    underlying_dispute_resolution,
    'To what extent does the agreement address the underlying causes of the border dispute?',
    'Analysis of diplomatic efforts and progress in border demarcation negotiations.',
    'Addresses underlying causes: Agreement is more likely to be a scaffold. Fails to address causes: Agreement is more likely to be a tangled rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(underlying_dispute_resolution, conceptual, 'Degree to which underlying border dispute is resolved').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asean_ceasefire_2011, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asea_tr_t0, asean_ceasefire_2011, theater_ratio, 0, 0.2).
narrative_ontology:measurement(asea_tr_t2, asean_ceasefire_2011, theater_ratio, 2, 0.3).
narrative_ontology:measurement(asea_tr_t5, asean_ceasefire_2011, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(asea_be_t0, asean_ceasefire_2011, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(asea_be_t2, asean_ceasefire_2011, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(asea_be_t5, asean_ceasefire_2011, base_extractiveness, 5, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asean_ceasefire_2011, enforcement_mechanism).
narrative_ontology:affects_constraint(asean_ceasefire_2011, preah_vihear_temple_dispute).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
