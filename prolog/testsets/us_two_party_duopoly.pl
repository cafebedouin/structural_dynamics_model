% ============================================================================
% CONSTRAINT STORY: us_two_party_duopoly
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_us_two_party_duopoly, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: us_two_party_duopoly
 * human_readable: The U.S. Two-Party Political Duopoly
 * domain: political
 * * SUMMARY:
 * The U.S. electoral landscape is structurally locked into a two-party system
 * via "winner-take-all" mechanics, high ballot access barriers, and campaign
 * finance laws. While it provides a mechanism for stable governance (a coordination
 * function), it extracts political representation from a significant portion of
 * the electorate who desire more options, creating a structural trap for
 * alternative political movements.
 * * KEY AGENTS:
 * - Independent/Third-Party Voters: Subject (Powerless)
 * - Major Party Leadership/Strategists: Beneficiary (Institutional)
 * - Political Scientists/Systems Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Base extractiveness (0.75): Captures the vast majority of political power and
% representation, despite significant voter dissatisfaction with the binary choice.
domain_priors:base_extractiveness(us_two_party_duopoly, 0.75).

% Suppression (0.80): High ballot access barriers, restrictive debate inclusion
% criteria (e.g., 15% polling threshold), and campaign finance structures
% actively suppress viable alternatives.
domain_priors:suppression_score(us_two_party_duopoly, 0.80).

% Theater Ratio (0.15): While political theater is high, the core functions of
% governance and power consolidation are very real. The ratio of performative
% to functional activity is relatively low.
domain_priors:theater_ratio(us_two_party_duopoly, 0.15).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(us_two_party_duopoly, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_two_party_duopoly, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(us_two_party_duopoly, theater_ratio, 0.15).

% Constraint self-claim: The system is presented as a necessary mechanism for
% stable governance and preventing fragmentation.
narrative_ontology:constraint_claim(us_two_party_duopoly, tangled_rope).
narrative_ontology:human_readable(us_two_party_duopoly, "The U.S. Two-Party Political Duopoly").

% Binary flags
domain_priors:requires_active_enforcement(us_two_party_duopoly). % Ballot access laws, FEC regulations.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(us_two_party_duopoly, major_political_parties).
narrative_ontology:constraint_victim(us_two_party_duopoly, independent_voters).
narrative_ontology:constraint_victim(us_two_party_duopoly, third_party_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISILLUSIONED VOTER (SNARE)
% From the perspective of a voter desiring alternative representation, the
% system is a trap. Their vote feels wasted or forced into a "lesser of two
% evils" choice, with high effective extraction of their political agency.
% χ = 0.75 * 1.5 (powerless) * 1.0 (national) = 1.125
constraint_indexing:constraint_classification(us_two_party_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PARTY STRATEGIST (ROPE)
% For an institutional actor within one of the major parties, the system is a
% pure coordination mechanism. It simplifies the political landscape, channels
% resources efficiently, and provides a stable path to power.
% χ = 0.75 * -0.2 (institutional) * 1.0 (national) = -0.15
constraint_indexing:constraint_classification(us_two_party_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both functions simultaneously. The system coordinates
% governance (Rope aspect) but does so via coercive suppression of alternatives,
% leading to asymmetric extraction of political capital (Snare aspect).
% χ = 0.75 * 1.15 (analytical) * 1.2 (global) = 1.035
constraint_indexing:constraint_classification(us_two_party_duopoly, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_two_party_duopoly_tests).

test(perspectival_gap_voter_vs_strategist) :-
    constraint_indexing:constraint_classification(us_two_party_duopoly, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_two_party_duopoly, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_observer_sees_tangled_rope) :-
    constraint_indexing:constraint_classification(us_two_party_duopoly, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(us_two_party_duopoly, extractiveness, E),
    assertion(E >= 0.46).

:- end_tests(us_two_party_duopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The U.S. two-party system is a canonical example of a Tangled Rope. Its
 * scores reflect a high degree of functional extraction (0.75) and active
 * suppression of alternatives (0.80).
 *
 * The Perspectival Gap is stark:
 * - For the powerless voter, the system is a Snare, extracting their
 *   representative will by making alternative choices non-viable.
 * - For the institutional party member, it's a Rope, a tool for coordinating
 *   power and ensuring stability.
 * - The analytical view, which requires acknowledging both the coordination
 *   function and the asymmetric extraction, correctly identifies it as a
 *   Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 * This constraint is marked as having [RESOLVED MANDATROPHY] because the
 * Tangled Rope classification itself is the resolution. It prevents the system
 * from making a binary choice between "it's all coordination" (the institutional
 * view) and "it's all extraction" (the powerless view). The classification
 * forces an acknowledgment that both are true simultaneously, which is the
 * core insight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_us_two_party_duopoly,
    'Will systemic reforms like Ranked Choice Voting (RCV) or the abolition of the Electoral College fundamentally reduce the duopoly''s extractive power?',
    'Monitor state-level adoption of RCV and federal electoral reform bills over the next decade. Track changes in third-party vote share and candidate viability in reformed jurisdictions.',
    'If reforms are effective, base_extractiveness and suppression_score would decrease, potentially shifting the analytical classification from Tangled Rope to Rope. If ineffective, the constraint remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_two_party_duopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The duopoly has become more entrenched and extractive over time due to
% increasing polarization and legal fortifications.
%
% Theater ratio over time:
narrative_ontology:measurement(us_two_party_duopoly_tr_t0, us_two_party_duopoly, theater_ratio, 0, 0.10).
narrative_ontology:measurement(us_two_party_duopoly_tr_t5, us_two_party_duopoly, theater_ratio, 5, 0.12).
narrative_ontology:measurement(us_two_party_duopoly_tr_t10, us_two_party_duopoly, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(us_two_party_duopoly_ex_t0, us_two_party_duopoly, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(us_two_party_duopoly_ex_t5, us_two_party_duopoly, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(us_two_party_duopoly_ex_t10, us_two_party_duopoly, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The duopoly functions as a mechanism to enforce a specific political structure.
narrative_ontology:coordination_type(us_two_party_duopoly, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */