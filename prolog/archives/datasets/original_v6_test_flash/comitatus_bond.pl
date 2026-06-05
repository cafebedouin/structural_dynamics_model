% ============================================================================
% CONSTRAINT STORY: comitatus_bond
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_comitatus_bond, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: comitatus_bond
 *   human_readable: The Germanic Comitatus Code
 *   domain: social/political
 *
 * SUMMARY:
 *   The comitatus is the foundational socio-political constraint governing
 *   the relationship between a lord (the "ring-giver") and his thanes in
 *   early Germanic society. It creates a system of mutual obligation, wherein
 *   the lord provides protection, resources, and leadership, while the thanes
 *   offer loyalty, military service, and counsel. The comitatus shaped
 *   Germanic social and political structures and influenced later feudal
 *   systems. It exemplifies a blend of coordination and extraction, with
 *   benefits distributed unevenly between parties.
 *
 * KEY AGENTS:
 *   - Ring-Giver: Primary beneficiary (institutional/arbitrage) - Benefits from loyalty and military strength.
 *   - Individual Thane: Primary target (powerless/trapped) - Individual autonomy is suppressed.
 *   - Thanes as a Collective: Organized (constrained) - Group reputation and security, limited individual power.
 *   - Vassal Families (Long-Term): Trapped - Families become enmeshed in comitatus structure
 *   - Analytical Observer: Analytical (analytical) - Considers coordination/extraction balance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(comitatus_bond, 0.6).
domain_priors:suppression_score(comitatus_bond, 0.7).
domain_priors:theater_ratio(comitatus_bond, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(comitatus_bond, extractiveness, 0.6).
narrative_ontology:constraint_metric(comitatus_bond, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(comitatus_bond, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(comitatus_bond, tangled_rope).
narrative_ontology:human_readable(comitatus_bond, "The Germanic Comitatus Code").
narrative_ontology:topic_domain(comitatus_bond, "social/political").

domain_priors:requires_active_enforcement(comitatus_bond).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(comitatus_bond, ring_giver).
narrative_ontology:constraint_beneficiary(comitatus_bond, thane_collective_reputation).
narrative_ontology:constraint_victim(comitatus_bond, individual_thane_autonomy).
narrative_ontology:constraint_victim(comitatus_bond, vassal_families_long_term).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual thane, bound by oath, finds his autonomy heavily suppressed. Desertion brings dishonor and loss of protection.
constraint_indexing:constraint_classification(comitatus_bond, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The ring-giver, as the institutional leader, benefits from the comitatus in terms of loyalty, military strength, and social order. They can choose to move or replace vassals.
constraint_indexing:constraint_classification(comitatus_bond, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% From a civilizational perspective, the comitatus showcases a blend of coordination (mutual defense) and extraction (limited individual freedom). The long-term impact shapes social structures and power dynamics.
constraint_indexing:constraint_classification(comitatus_bond, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Thanes as a group benefit from collective reputation and security but are constrained by the need to maintain loyalty and perform in battle. A strong band of warriors provides a powerful advantage, even if it limits their individual power.
constraint_indexing:constraint_classification(comitatus_bond, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% Families become deeply enmeshed in the comitatus's power structure, making them vulnerable to consequences from broken oaths or battlefield deaths.
constraint_indexing:constraint_classification(comitatus_bond, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(comitatus_bond_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(comitatus_bond, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(comitatus_bond, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(comitatus_bond, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(comitatus_bond_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness value (0.6) is high because the system extracts significant personal autonomy from the thanes. Suppression (0.7) is also high due to limited exit options and social consequences for breaking the oath. The theater ratio (0.2) is low because the primary function of the comitatus is military strength, not performative display. This is a low theater ratio relative to systems where fealty rituals overwhelm substantive obligations.
 *
 * PERSPECTIVAL GAP:
 *   The lord sees the comitatus as a rope (coordination mechanism), enabling military strength and societal order. The individual thane experiences it as a snare, limiting personal freedom and mandating obedience. The analytical observer recognizes the tangled rope, a system blending cooperation with asymmetric extraction. The Thane collective gains reputational benefits but accepts constrained freedom to operate.
 *
 * DIRECTIONALITY LOGIC:
 *   The ring-giver benefits, resulting in low directionality. The individual thane experiences extraction, leading to high directionality. The analytical observer understands the blend, which results in moderate overall directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oath_enforcement_power,
    'How consistently and effectively are oaths enforced within the comitatus?',
    'Archaeological evidence of punishment, literary accounts of oath-breaking and retribution, comparative legal studies',
    'Strong enforcement shifts classification towards Snare; weak enforcement allows drift towards Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oath_enforcement_power, empirical, 'Strength of oath enforcement within the comitatus structure').

omega_variable(
    lord_generosity_verification,
    'To what extent is the ''ring-giver'' truly generous versus self-serving?',
    'Historical analysis of gift-giving patterns, resource distribution records, and critiques of specific rulers',
    'Verified generosity strengthens Rope classification; consistent self-serving behavior reinforces Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lord_generosity_verification, empirical, 'Analysis of the ring-giver''s true generosity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(comitatus_bond, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comi_tr_t0, comitatus_bond, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comi_tr_t50, comitatus_bond, theater_ratio, 50, 0.2).
narrative_ontology:measurement(comi_tr_t100, comitatus_bond, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(comi_be_t0, comitatus_bond, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(comi_be_t50, comitatus_bond, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(comi_be_t100, comitatus_bond, base_extractiveness, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(comitatus_bond, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
