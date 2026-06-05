% ============================================================================
% CONSTRAINT STORY: israel_surplus_vote_agreements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_surplus_vote_agreements, []).

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
 *   constraint_id: israel_surplus_vote_agreements
 *   human_readable: Surplus-Vote Agreements (Bader-Ofer Method) in Israeli Elections
 *   domain: political
 *
 * SUMMARY:
 *   Surplus-vote agreements in Israeli elections, also known as the
 *   Bader-Ofer method, allow two parties to pool their surplus votes to
 *   increase their chances of obtaining extra seats in the Knesset. This
 *   system creates a complex interplay between coordination among parties and
 *   extraction from smaller parties and voters whose preferences may be
 *   indirectly represented. The agreements serve as a strategic tool for
 *   parties to overcome electoral thresholds, but can also distort
 *   representation and disadvantage those excluded from the arrangements.
 *
 * KEY AGENTS:
 *   - Partner Party A: Primary beneficiary (institutional/arbitrage) - Benefits from increased seat allocation.
 *   - Partner Party B: Secondary beneficiary (institutional/arbitrage) - Shares in the benefit of increased seat allocation.
 *   - Smaller Parties: Primary target (powerless/trapped) - Disadvantaged by exclusion from agreements.
 *   - Voters of Partner Parties: Secondary target (moderate/constrained) - Votes may indirectly support a different party.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_surplus_vote_agreements, 0.55).
domain_priors:suppression_score(israel_surplus_vote_agreements, 0.45).
domain_priors:theater_ratio(israel_surplus_vote_agreements, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, extractiveness, 0.55).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_surplus_vote_agreements, tangled_rope).
narrative_ontology:human_readable(israel_surplus_vote_agreements, "Surplus-Vote Agreements (Bader-Ofer Method) in Israeli Elections").
narrative_ontology:topic_domain(israel_surplus_vote_agreements, "political").

domain_priors:requires_active_enforcement(israel_surplus_vote_agreements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_surplus_vote_agreements, partner_party_a).
narrative_ontology:constraint_beneficiary(israel_surplus_vote_agreements, partner_party_b).
narrative_ontology:constraint_victim(israel_surplus_vote_agreements, smaller_parties).
narrative_ontology:constraint_victim(israel_surplus_vote_agreements, voters_of_partner_parties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLER PARTIES (SNARE) - Smaller parties are often excluded or disadvantaged by surplus vote agreements between larger parties, limiting their ability to gain representation in the Knesset. They are trapped because they lack the size or influence to form such agreements themselves, bearing the full negative impact.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VOTERS OF PARTNER PARTIES (TANGLED ROPE) - Voters of parties entering surplus vote agreements may find their vote indirectly supporting a different party than they intended, creating a tension between their individual preferences and the strategic advantage gained by the agreement. They are constrained because their vote contributes to the agreement as a whole, whether they approve of the specific partner party or not.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARTNER PARTY A (ROPE) - Parties entering surplus vote agreements benefit by increasing their chances of obtaining surplus seats, even if they do not win enough votes independently. They can arbitrage between different political partnerships to maximize their gains, seeing the agreement as pure coordination to achieve a shared goal.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARTNER PARTY B (ROPE) - Similar to Party A, Party B benefits from the increased likelihood of obtaining surplus seats through the agreement. The arrangement is viewed as a coordinated strategy to overcome electoral thresholds.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - From a broader perspective, surplus vote agreements represent a mixed system of coordination and extraction. They facilitate coalition building and stability but can also distort representation and disadvantage smaller parties. The observer sees the active enforcement through electoral rules and strategic party behavior.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_surplus_vote_agreements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_surplus_vote_agreements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_surplus_vote_agreements, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_surplus_vote_agreements, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(israel_surplus_vote_agreements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - Moderate to high. Surplus vote agreements primarily benefit the participating parties, increasing their representation at the expense of smaller parties and potentially misrepresenting the preferences of some voters. Suppression: 0.45 - Moderate. Smaller parties are often excluded from forming such agreements due to their size, which limits their ability to compete effectively. The agreements also suppress the direct expression of voter preferences. Theater ratio: 0.30 - Low. The agreements are largely functional in achieving their intended goal of increasing seat allocation and are not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing experiences and interests of the agents involved. Partner parties view the agreement as a purely beneficial coordination mechanism, whereas smaller parties experience it as a system that suppresses their representation. Voters of partner parties face a mixed experience, as their votes contribute to the overall success of the agreement, but may not directly align with their individual preferences. The analytical observer recognizes both the coordination and extraction aspects of the system, leading to a tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the structural relationships of the agents to the constraint. Partner parties are the beneficiaries, as they actively gain seats through the agreements (d close to 0). Smaller parties are the victims, as they are disadvantaged by the system (d close to 1). Voters are indirectly affected (d closer to 0.5). The analytical observer views the entire system with a balanced perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the same agreement can be viewed as both beneficial coordination (for partner parties) and extractive suppression (for smaller parties). The classification as a tangled rope captures the mixed nature of the system, reflecting the coordination and extraction aspects observed from different perspectives. It's not solely a rope or a snare but a hybrid that combines elements of both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electoral_threshold_effect,
    'How does the electoral threshold influence the strategic value and impact of surplus-vote agreements?',
    'Comparative analysis of electoral outcomes with and without agreements, considering different threshold levels.',
    'Higher thresholds amplify the benefits for participating parties but also increase the disadvantages for smaller parties, making the constraint more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_threshold_effect, empirical, 'Impact of electoral threshold on agreement value.').

omega_variable(
    coalition_formation_impact,
    'To what extent do surplus-vote agreements affect the overall stability and composition of governing coalitions?',
    'Analysis of historical coalition formations and the role of parties entering surplus-vote agreements.',
    'Increased coalition stability would classify the agreement as more of a rope. Distorted or limited coalition options would classify it as more of a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_formation_impact, empirical, 'Influence on coalition stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_surplus_vote_agreements, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isra_tr_t0, israel_surplus_vote_agreements, theater_ratio, 0, 0.2).
narrative_ontology:measurement(isra_tr_t10, israel_surplus_vote_agreements, theater_ratio, 10, 0.3).
narrative_ontology:measurement(isra_tr_t20, israel_surplus_vote_agreements, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(isra_be_t0, israel_surplus_vote_agreements, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(isra_be_t10, israel_surplus_vote_agreements, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(isra_be_t20, israel_surplus_vote_agreements, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_surplus_vote_agreements, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
