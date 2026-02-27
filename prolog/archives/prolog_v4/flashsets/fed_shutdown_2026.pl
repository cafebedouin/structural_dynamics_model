% ============================================================================
% CONSTRAINT STORY: fed_shutdown_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fed_shutdown_2026, []).

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
 *   constraint_id: fed_shutdown_2026
 *   human_readable: The 2026 DHS/ICE Funding Standoff
 *   domain: political/economic
 *
 * SUMMARY:
 *   The 2026 DHS/ICE funding standoff leading to a federal government
 *   shutdown highlights the tension between political agendas, immigration
 *   policy, and the functioning of government. This constraint story examines
 *   the different perspectives on the shutdown, identifying who benefits and
 *   who bears the costs. Hardline political factions exploit the crisis for
 *   political gain, while DHS employees and immigrant communities suffer the
 *   most. The broader economy experiences mixed effects. The standoff reveals
 *   the challenges of balancing security concerns with human rights and
 *   economic stability.
 *
 * KEY AGENTS:
 *   - DHS Employees: Primary target (powerless/trapped) - face furloughs and financial insecurity.
 *   - Immigrant Communities: Secondary target (powerless/trapped) - experience increased fear and vulnerability.
 *   - Broader Economy: Mixed impact (moderate/constrained) - suffers from disruption but may gain from reduced spending.
 *   - Hardline Political Factions: Primary beneficiary (institutional/arbitrage) - exploit the crisis for political gain.
 *   - Private Prison Industry: Potential beneficiary (institutional/arbitrage) - may benefit from increased ICE enforcement.
 *   - Analytical Observer: Analytical perspective (analytical/analytical) - assesses the long-term implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fed_shutdown_2026, 0.65).
domain_priors:suppression_score(fed_shutdown_2026, 0.7).
domain_priors:theater_ratio(fed_shutdown_2026, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fed_shutdown_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(fed_shutdown_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fed_shutdown_2026, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fed_shutdown_2026, tangled_rope).
narrative_ontology:human_readable(fed_shutdown_2026, "The 2026 DHS/ICE Funding Standoff").
narrative_ontology:topic_domain(fed_shutdown_2026, "political/economic").

domain_priors:requires_active_enforcement(fed_shutdown_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fed_shutdown_2026, hardline_political_factions).
narrative_ontology:constraint_beneficiary(fed_shutdown_2026, private_prison_industry).
narrative_ontology:constraint_victim(fed_shutdown_2026, dhs_employees).
narrative_ontology:constraint_victim(fed_shutdown_2026, immigrant_communities).
narrative_ontology:constraint_victim(fed_shutdown_2026, broader_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of DHS employees facing furloughs and financial insecurity due to the shutdown. Limited exit options and high vulnerability.
constraint_indexing:constraint_classification(fed_shutdown_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of immigrant communities facing increased fear and vulnerability due to reduced oversight of ICE and potential for heightened enforcement.
constraint_indexing:constraint_classification(fed_shutdown_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of the U.S. economy experiencing negative impacts due to the shutdown, but also potentially benefiting from reduced government spending (a contested claim).
constraint_indexing:constraint_classification(fed_shutdown_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of hardline political factions who benefit from the shutdown by furthering their political agendas and rallying their base. They have arbitrage opportunities via fundraising and media attention.
constraint_indexing:constraint_classification(fed_shutdown_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective of the private prison industry potentially benefiting from increased ICE enforcement, although this is contingent on the specific shutdown outcomes.
constraint_indexing:constraint_classification(fed_shutdown_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analytical observer perspective, recognizing the mixed coordination and extraction aspects of the political standoff. Sees the long-term implications for governance and social stability.
constraint_indexing:constraint_classification(fed_shutdown_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fed_shutdown_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fed_shutdown_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fed_shutdown_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fed_shutdown_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fed_shutdown_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The shutdown extracts value from DHS employees, immigrant communities, and the economy. Hardline factions benefit politically. Suppression (0.70): High. The political climate and lack of compromise suppress alternative solutions. Theater ratio (0.40): Moderate. While there is political theater, the shutdown also has real consequences.
 *
 * PERSPECTIVAL GAP:
 *   DHS employees and immigrant communities experience the shutdown as a snare, with limited options and significant harm. Hardline political factions see it as a rope, enabling them to advance their agenda. The broader economy faces a tangled rope, with both positive and negative consequences. The analytical observer sees the tangled rope of competing interests and long-term instability.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the beneficiary/victim status and the agent's exit options. Powerless agents with trapped exit options have high directionality values, experiencing the constraint as extractive. Institutional agents with arbitrage opportunities have low directionality values, benefiting from the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The shutdown could be misconstrued as pure extraction, but the political benefits to some factions indicate a degree of coordination. The tangled rope classification captures the mixed nature of the constraint, with both coordination and extraction occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ice_oversight_effectiveness,
    'How effectively does oversight of ICE practices prevent abuses and protect human rights?',
    'Independent audits, legal challenges, and investigations into ICE practices under different oversight regimes.',
    'If oversight is effective, reduced oversight leads to increased abuses (snare). If oversight is ineffective, the impact of the shutdown is primarily economic (rope/scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ice_oversight_effectiveness, empirical, 'Effectiveness of ICE oversight').

omega_variable(
    political_polarization_severity,
    'How severely is political polarization impacting the ability to reach compromises on budget and policy issues?',
    'Analysis of voting patterns, public opinion surveys, and discourse analysis of political rhetoric.',
    'High polarization makes shutdowns more likely and intractable (snare). Lower polarization allows for negotiated solutions (rope/scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_polarization_severity, conceptual, 'Severity of political polarization').

omega_variable(
    economic_impact_magnitude,
    'What is the true magnitude of the economic impact of government shutdowns?',
    'Economic modeling, analysis of historical shutdown data, and comparisons with control groups.',
    'High economic impact makes the shutdown more damaging (snare). Low economic impact makes the political gains/losses more salient (rope/scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_magnitude, empirical, 'Magnitude of economic impact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fed_shutdown_2026, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed__tr_t0, fed_shutdown_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fed__tr_t1, fed_shutdown_2026, theater_ratio, 1, 0.3).
narrative_ontology:measurement(fed__tr_t2, fed_shutdown_2026, theater_ratio, 2, 0.4).

% Extraction over time
narrative_ontology:measurement(fed__be_t0, fed_shutdown_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fed__be_t1, fed_shutdown_2026, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(fed__be_t2, fed_shutdown_2026, base_extractiveness, 2, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fed_shutdown_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(fed_shutdown_2026, immigration_policy_effectiveness).
narrative_ontology:affects_constraint(fed_shutdown_2026, border_security_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
