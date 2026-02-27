% ============================================================================
% CONSTRAINT STORY: columbia_2026_elections
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_columbia_2026_elections, []).

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
 *   constraint_id: columbia_2026_elections
 *   human_readable: 2026 Colombian Presidential Election Structure
 *   domain: political
 *
 * SUMMARY:
 *   The 2026 Colombian presidential election presents a structural constraint
 *   due to the constitutional one-term limit, forcing a transition and
 *   opening opportunities for new political actors. This dynamic interplay
 *   results in a complex scenario where different actors experience the
 *   election structure in varied ways. The incumbent administration is at a
 *   disadvantage, while traditional and emerging parties vie for power, each
 *   facing their own constraints and opportunities. The process will be a
 *   test of democratic stability in a country with a history of political
 *   violence.
 *
 * KEY AGENTS:
 *   - Incumbent Administration: Primary target (powerless/trapped)
 *   - Traditional Political Parties: Secondary actor (moderate/constrained)
 *   - Emerging Political Factions: Primary beneficiary (powerful/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(columbia_2026_elections, 0.5).
domain_priors:suppression_score(columbia_2026_elections, 0.4).
domain_priors:theater_ratio(columbia_2026_elections, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(columbia_2026_elections, extractiveness, 0.5).
narrative_ontology:constraint_metric(columbia_2026_elections, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(columbia_2026_elections, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(columbia_2026_elections, tangled_rope).
narrative_ontology:human_readable(columbia_2026_elections, "2026 Colombian Presidential Election Structure").
narrative_ontology:topic_domain(columbia_2026_elections, "political").

domain_priors:requires_active_enforcement(columbia_2026_elections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(columbia_2026_elections, emerging_political_factions).
narrative_ontology:constraint_beneficiary(columbia_2026_elections, traditional_political_parties).
narrative_ontology:constraint_victim(columbia_2026_elections, incumbent_administration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The incumbent administration is structurally disadvantaged by the one-term limit, essentially trapped within a system that necessitates transition. They are the primary target of this constraint.
constraint_indexing:constraint_classification(columbia_2026_elections, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Traditional political parties are constrained by the changing political landscape but also benefit from the election structure as a means to regain power. They can participate in the election, but they are also somewhat limited by new emerging factions.
constraint_indexing:constraint_classification(columbia_2026_elections, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Emerging political factions benefit from the opportunity to challenge established powers. The election structure provides an arbitrage opportunity for them to gain influence and power. The constitutionally mandated transition provides a chance for these parties to gain power.
constraint_indexing:constraint_classification(columbia_2026_elections, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective, the election structure is a complex interplay of coordination and extraction. It serves to maintain democratic transitions, but also creates opportunities for power struggles and potential instability. The time horizon is civilizational because the electoral system is intended to persist indefinitely.
constraint_indexing:constraint_classification(columbia_2026_elections, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(columbia_2026_elections_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(columbia_2026_elections, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(columbia_2026_elections, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(columbia_2026_elections, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(columbia_2026_elections_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The election is a tangled rope because it provides a coordination mechanism (transfer of power) but also allows for extraction (power struggles and corruption). Extractiveness: 0.5. Suppression: 0.4. Theater Ratio: 0.3.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent administration sees the one-term limit as a snare, preventing them from consolidating power. Traditional parties see a tangled rope, constrained by new actors but also presented with an opportunity. Emerging factions see a rope, facilitating their rise to power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (emerging and some traditional parties) gain influence and potentially power through the election process (low directionality value). Victims (incumbent administration) are structurally disadvantaged by the term limit, leading to a higher directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope is appropriate since it provides a mechanism for coordination, but also provides incentives for power struggles and corrupt activities that prevent equal access to power for all participants. The mandate of a single-term ensures that power is somewhat balanced but also allows for potential chaos as new leaders emerge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    influence_of_organized_crime,
    'To what extent will organized crime influence the elections?',
    'Monitoring and reporting by international observers and investigative journalism.',
    'High influence could undermine democratic legitimacy and increase the risk of violence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(influence_of_organized_crime, empirical, 'The level of interference and influence of criminal organizations will influence the outcome and legitimacy of the elections.').

omega_variable(
    peace_process_stability,
    'Will the ongoing peace process hold?',
    'Negotiation outcomes and compliance monitoring by peace accords.',
    'Instability could lead to a resurgence of violence and displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peace_process_stability, conceptual, 'The success of the peace process with rebel groups could destabilize elections, or give more parties the opportunity to participate in elections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(columbia_2026_elections, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colu_tr_t0, columbia_2026_elections, theater_ratio, 0, 0.2).
narrative_ontology:measurement(colu_tr_t1, columbia_2026_elections, theater_ratio, 1, 0.3).
narrative_ontology:measurement(colu_tr_t2, columbia_2026_elections, theater_ratio, 2, 0.4).

% Extraction over time
narrative_ontology:measurement(colu_be_t0, columbia_2026_elections, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(colu_be_t1, columbia_2026_elections, base_extractiveness, 1, 0.4).
narrative_ontology:measurement(colu_be_t2, columbia_2026_elections, base_extractiveness, 2, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(columbia_2026_elections, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
