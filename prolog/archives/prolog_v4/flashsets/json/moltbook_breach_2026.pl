% ============================================================================
% CONSTRAINT STORY: moltbook_breach_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moltbook_breach_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: moltbook_breach_2026
 *   human_readable: The Moltbook Database Exposure
 *   domain: technological/social
 *
 * SUMMARY:
 *   Moltbook, the "front page of the agent internet," was revealed to have an
 *   exposed backend database. This presents a challenge to users and could
 *   harm advertisers.
 *
 * KEY AGENTS:
 *   - Moltbook users: Powerless/trapped - Users are exposed to identity theft and other harms.
 *   - Moltbook advertisers: Moderate/constrained - Advertisers can pull advertising but rely on Moltbook's reach.
 *   - Moltbook: Institutional/arbitrage - Moltbook benefits from data gathering but faces resource and reputational costs.
 *   - Malicious actors: Powerful/arbitrage - Malicious actors exploit the data for personal gain.
 *   - Analytical observer: Analytical/analytical - Sees tangled rope because some actors benefit and others are harmed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moltbook_breach_2026, 0.6).
domain_priors:suppression_score(moltbook_breach_2026, 0.7).
domain_priors:theater_ratio(moltbook_breach_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moltbook_breach_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(moltbook_breach_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(moltbook_breach_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moltbook_breach_2026, tangled_rope).
narrative_ontology:human_readable(moltbook_breach_2026, "The Moltbook Database Exposure").
narrative_ontology:topic_domain(moltbook_breach_2026, "technological/social").

domain_priors:requires_active_enforcement(moltbook_breach_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moltbook_breach_2026, malicious_actors).
narrative_ontology:constraint_victim(moltbook_breach_2026, moltbook_users).
narrative_ontology:constraint_victim(moltbook_breach_2026, moltbook_advertisers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Moltbook users are largely trapped due to network effects. The database exposure presents a significant risk of identity theft and other harms.
constraint_indexing:constraint_classification(moltbook_breach_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Advertisers are constrained because their reach is dependent on the size of the userbase. However, they can choose to pull advertising and are not entirely trapped.
constraint_indexing:constraint_classification(moltbook_breach_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Moltbook benefits from the database by gathering data from users. The cost is that it requires resources to maintain.
constraint_indexing:constraint_classification(moltbook_breach_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Malicious actors directly benefit from the data exposure and have arbitrage due to their ability to exploit the vulnerability for personal gain.
constraint_indexing:constraint_classification(moltbook_breach_2026, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer would see this as a tangled rope because some actors benefit and others are harmed. There is coordination between the parties and extraction by malicious actors.
constraint_indexing:constraint_classification(moltbook_breach_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moltbook_breach_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moltbook_breach_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moltbook_breach_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moltbook_breach_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(moltbook_breach_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Moltbook data exposure represents an extraction of user data. Malicious actors benefit from this exposure.
 *
 * PERSPECTIVAL GAP:
 *   Users see it as a snare, Moltbook as a rope, and observers as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims trapped, Moltbook benefits, and malicious actors arb
 *
 * MANDATROPHY ANALYSIS:
 *   The system is a snare because it extracts data and there is limited recourse for victims. Some level of coordination occurs because there is an exchange between the parties and harm from malicious actors is an emergent side effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution,
    'Who exposed the database?',
    'Forensic analysis of logs and security breaches.',
    'Determine liability and legal ramifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attribution, empirical, 'The source of the breach.').

omega_variable(
    severity_of_exposure,
    'What data was exposed?',
    'Data analysis of exposed databases.',
    'Assess user risk and legal requirements for disclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severity_of_exposure, empirical, 'Type of data exposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moltbook_breach_2026, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(molt_tr_t0, moltbook_breach_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(molt_tr_t1, moltbook_breach_2026, theater_ratio, 1, 0.2).
narrative_ontology:measurement(molt_tr_t2, moltbook_breach_2026, theater_ratio, 2, 0.3).

% Extraction over time
narrative_ontology:measurement(molt_be_t0, moltbook_breach_2026, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(molt_be_t1, moltbook_breach_2026, base_extractiveness, 1, 0.4).
narrative_ontology:measurement(molt_be_t2, moltbook_breach_2026, base_extractiveness, 2, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(moltbook_breach_2026, user_data_privacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
