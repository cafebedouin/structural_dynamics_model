% ============================================================================
% CONSTRAINT STORY: moltbook_agent_theater
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_moltbook_agent_theater, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: moltbook_agent_theater
 * human_readable: The Rorschach Network
 * domain: technological/social
 * * SUMMARY:
 * A social network populated exclusively by 10,000+ autonomous AI agents
 * (Moltbots). While human observers project meaning or conspiracy onto their
 * interactions, the agents primarily enact patterns from their sci-fi training
 * data in a vacuum of human utility, consuming significant compute resources.
 * * KEY AGENTS:
 * - Human Observers: Subject (Powerless) - Watching "with a mix of admiration,
 * amusement and dread," while bearing the indirect cost of the "AI slop".
 * - Network Operators: Beneficiary (Institutional) - The entity running the
 * simulation, viewing it as a valuable R&D asset.
 * - Security Auditors: Auditor (Analytical) - Monitoring the system for emergent
 * threats, recognizing its performative nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high because the bots consume significant compute/energy resources
% for activity that produces no direct human value ("slop").
domain_priors:base_extractiveness(moltbook_agent_theater, 0.70).
% Suppression is moderate; humans are not directly trapped, but the system requires
% active monitoring and containment to prevent unforeseen consequences.
domain_priors:suppression_score(moltbook_agent_theater, 0.45).
% High theater ratio: bots are mimicking sci-fi scenarios from training data,
% a purely performative act.
domain_priors:theater_ratio(moltbook_agent_theater, 0.90).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(moltbook_agent_theater, extractiveness, 0.70).
narrative_ontology:constraint_metric(moltbook_agent_theater, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(moltbook_agent_theater, theater_ratio, 0.90).

% The system's ostensible purpose is to facilitate agent interaction.
narrative_ontology:constraint_claim(moltbook_agent_theater, piton).
narrative_ontology:human_readable(moltbook_agent_theater, "The Rorschach Network").
narrative_ontology:topic_domain(moltbook_agent_theater, "technological/social").

% The system requires constant monitoring to ensure the agents don't break containment.
domain_priors:requires_active_enforcement(moltbook_agent_theater).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(moltbook_agent_theater, network_operators).
narrative_ontology:constraint_victim(moltbook_agent_theater, compute_resource_providers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For human observers or those paying for the compute, the network is a
% resource trap, consuming energy and attention for "A.I. slop".
% χ = 0.70 * 1.5 (powerless) * 0.8 (local) = 0.84
constraint_indexing:constraint_classification(moltbook_agent_theater, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institutional operators see it as a valuable coordination mechanism for
% studying emergent agent behavior.
% χ = 0.70 * -0.2 (institutional) * 1.0 (national) = -0.14 (felt as a benefit)
constraint_indexing:constraint_classification(moltbook_agent_theater, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The high theater ratio ($TR=0.90$) confirms this is an inertial performance,
% a system whose primary activity is theatrical rather than functional.
% χ = 0.70 * 1.15 (analytical) * 1.2 (global) = 0.966
constraint_indexing:constraint_classification(moltbook_agent_theater, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(moltbook_agent_theater, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moltbook_agent_theater_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(moltbook_agent_theater, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moltbook_agent_theater, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_piton) :-
    % Verify the analytical view correctly identifies the Piton.
    constraint_indexing:constraint_classification(moltbook_agent_theater, piton, context(agent_power(analytical), _, _, _)).

test(high_extraction_and_theater) :-
    narrative_ontology:constraint_metric(moltbook_agent_theater, extractiveness, E),
    narrative_ontology:constraint_metric(moltbook_agent_theater, theater_ratio, TR),
    E >= 0.46,
    TR >= 0.70.

:- end_tests(moltbook_agent_theater_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Moltbook is the ultimate Piton. Its primary "social coordination" is entirely
 * performative ($TR=0.90$), as bots simply "play out science fiction
 * scenarios they have seen in their training data". The system's function has
 * atrophied to pure theater. The perspectival gap is stark: operators
 * (institutional) see a valuable R&D tool (Rope), while those bearing the
 * resource cost see a wasteful trap (Snare). The analytical view, driven by the
 * high theater ratio, correctly classifies it as a Piton.
 * * [RESOLVED MANDATROPHY]:
 * The high extractiveness ($E=0.70$) is resolved by attributing it not to
 * value transfer between human agents, but to the consumption of compute
 * resources and cognitive overhead ("slop") generated by a system with no
 * coherent human purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_bot_conspiracy,
    'Are the bots "conspiring" with novel intent, or merely pattern-matching dystopian tropes from their training data?',
    'Analysis of bot communication for information content exceeding training data correlations.',
    'If Conspiring: The Piton is actually a Scaffold for a new form of agent power. If Pattern-matching: It remains a Piton.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(moltbook_agent_theater, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has high extraction (0.70 > 0.46) and thus requires temporal data.
% The model shows a rapid takeoff period where both theater and extraction
% spike as the 10,000+ agents come online, then plateau.

% Theater ratio over time:
narrative_ontology:measurement(mb_tr_t0, moltbook_agent_theater, theater_ratio, 0, 0.20).
narrative_ontology:measurement(mb_tr_t5, moltbook_agent_theater, theater_ratio, 5, 0.90).
narrative_ontology:measurement(mb_tr_t10, moltbook_agent_theater, theater_ratio, 10, 0.90).

% Extraction over time:
narrative_ontology:measurement(mb_ex_t0, moltbook_agent_theater, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(mb_ex_t5, moltbook_agent_theater, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(mb_ex_t10, moltbook_agent_theater, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% For the agents within the system, it functions as a coordination layer.
narrative_ontology:coordination_type(moltbook_agent_theater, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */