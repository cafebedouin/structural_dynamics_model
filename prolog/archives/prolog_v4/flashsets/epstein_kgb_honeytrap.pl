% ============================================================================
% CONSTRAINT STORY: epstein_kgb_honeytrap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epstein_kgb_honeytrap, []).

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
 *   constraint_id: epstein_kgb_honeytrap
 *   human_readable: The Kompromat Siphon
 *   domain: political/intelligence
 *
 * SUMMARY:
 *   A high-tech surveillance operation disguised as an elite social network.
 *   The network gathered compromising material on high-profile individuals,
 *   using blackmail to exert influence and gain leverage. This has serious
 *   consequences for public trust.
 *
 * KEY AGENTS:
 *   - Compromised Targets: Primary target (powerless/trapped) — bears extraction
 *   - KGB Successors: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Blackmailers: Secondary actor (organized/constrained)
 *   - Public Trust: Abstract victim (powerless/trapped)
 *   - Analytical Observer: Sees the full structure (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epstein_kgb_honeytrap, 0.85).
domain_priors:suppression_score(epstein_kgb_honeytrap, 0.9).
domain_priors:theater_ratio(epstein_kgb_honeytrap, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epstein_kgb_honeytrap, extractiveness, 0.85).
narrative_ontology:constraint_metric(epstein_kgb_honeytrap, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(epstein_kgb_honeytrap, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epstein_kgb_honeytrap, snare).
narrative_ontology:human_readable(epstein_kgb_honeytrap, "The Kompromat Siphon").
narrative_ontology:topic_domain(epstein_kgb_honeytrap, "political/intelligence").

domain_priors:requires_active_enforcement(epstein_kgb_honeytrap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epstein_kgb_honeytrap, kgb_successors).
narrative_ontology:constraint_beneficiary(epstein_kgb_honeytrap, blackmailers).
narrative_ontology:constraint_victim(epstein_kgb_honeytrap, compromised_targets).
narrative_ontology:constraint_victim(epstein_kgb_honeytrap, public_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The compromised individuals are trapped in a snare. They are powerless to resist the blackmail, and their reputations and careers are at stake.
constraint_indexing:constraint_classification(epstein_kgb_honeytrap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% KGB successors benefit from the kompromat, using it to exert influence and gain leverage over compromised individuals.
constraint_indexing:constraint_classification(epstein_kgb_honeytrap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Blackmailers both benefit and are constrained by the system. They gain financially and in power but are also susceptible to exposure and legal consequences.
constraint_indexing:constraint_classification(epstein_kgb_honeytrap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Public trust in institutions and individuals is eroded by the revelations of kompromat and blackmail.
constraint_indexing:constraint_classification(epstein_kgb_honeytrap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The analytical observer sees the system as a tangled rope, involving both coordination (information gathering) and extraction (blackmail).
constraint_indexing:constraint_classification(epstein_kgb_honeytrap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epstein_kgb_honeytrap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epstein_kgb_honeytrap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epstein_kgb_honeytrap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epstein_kgb_honeytrap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epstein_kgb_honeytrap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85): High. The network extracted compromising material, resulting in substantial blackmail and influence. Suppression (0.90): High. Victims were largely unable to escape due to the nature of the compromising material and the power of the network. Theater ratio (0.30): Low. The network's primary function was extraction, not public performance.
 *
 * PERSPECTIVAL GAP:
 *   The compromised individuals see a snare, while the KGB successors and blackmailers see a rope or tangled rope. The public sees the erosion of trust as a snare. The analytical observer views the overall situation as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Compromised individuals are victims, so they have a high d value. The KGB successors are beneficiaries, so they have a low d value. Blackmailers have a mixed d value, as they are both beneficiaries and subject to risks. The public has a high d value due to the loss of trust. The analyst has a moderate d-value.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by accurately classifying the system as a snare for the compromised individuals and a tangled rope for the blackmailers and KGB successors. The perspective from different agents defines their observed classification of the honeytrap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_compromise,
    'How many high-profile individuals were actually compromised by this network?',
    'Independent investigations, leaked documents, and verifiable testimonies.',
    'Affects the overall scale of extraction and the perceived threat to public trust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_compromise, empirical, 'Determining the actual number of compromised individuals.').

omega_variable(
    level_of_kgb_involvement,
    'To what extent was the KGB or its successor organizations directly involved in the network''s operations?',
    'Declassified intelligence reports, archival research, and insider accounts.',
    'Defines the nature of the rope element, whether it''s a genuine intelligence operation or a criminal enterprise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(level_of_kgb_involvement, empirical, 'Assessing the degree of KGB or successor involvement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epstein_kgb_honeytrap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epst_tr_t0, epstein_kgb_honeytrap, theater_ratio, 0, 0.1).
narrative_ontology:measurement(epst_tr_t5, epstein_kgb_honeytrap, theater_ratio, 5, 0.2).
narrative_ontology:measurement(epst_tr_t10, epstein_kgb_honeytrap, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(epst_be_t0, epstein_kgb_honeytrap, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(epst_be_t5, epstein_kgb_honeytrap, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(epst_be_t10, epstein_kgb_honeytrap, base_extractiveness, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epstein_kgb_honeytrap, information_standard).
narrative_ontology:affects_constraint(epstein_kgb_honeytrap, state_secrets_privilege).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
