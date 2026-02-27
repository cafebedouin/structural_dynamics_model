% ============================================================================
% CONSTRAINT STORY: greshams_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_greshams_law, []).

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
 *   constraint_id: greshams_law
 *   human_readable: Gresham's Law ("Bad money drives out good")
 *   domain: economic
 *
 * SUMMARY:
 *   Gresham's Law describes how, when two forms of currency are legally
 *   stipulated to be of equal value but differ in intrinsic worth, the
 *   currency with higher intrinsic value tends to be hoarded while the 'bad
 *   money' circulates more freely. This dynamic benefits the issuer of
 *   debased currency (typically governments) in the short term but harms
 *   holders of 'good money' and undermines long-term economic stability. It
 *   leads to suppressed alternatives for those seeking to store value.
 *
 * KEY AGENTS:
 *   - Governments issuing debased currency: Beneficiary (institutional/arbitrage)
 *   - Holders of 'good money': Victim (powerless/trapped)
 *   - General Populace: Mixed effects (moderate/constrained)
 *   - Long term savers: Victim (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(greshams_law, 0.55).
domain_priors:suppression_score(greshams_law, 0.7).
domain_priors:theater_ratio(greshams_law, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(greshams_law, extractiveness, 0.55).
narrative_ontology:constraint_metric(greshams_law, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(greshams_law, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(greshams_law, tangled_rope).
narrative_ontology:human_readable(greshams_law, "Gresham's Law (\"Bad money drives out good\")").
narrative_ontology:topic_domain(greshams_law, "economic").

domain_priors:requires_active_enforcement(greshams_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(greshams_law, governments_issuing_debased_currency).
narrative_ontology:constraint_beneficiary(greshams_law, individuals_making_payments).
narrative_ontology:constraint_victim(greshams_law, holders_of_good_money).
narrative_ontology:constraint_victim(greshams_law, long_term_savers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Holders of 'good money' with limited options to arbitrage outside the national scope are trapped, facing suppressed alternatives as the debased currency dominates transactions.
constraint_indexing:constraint_classification(greshams_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The general populace is constrained. They benefit from having money to transact with, even if debased, but are harmed by the long-term effects of inflation and the disappearance of sound money.
constraint_indexing:constraint_classification(greshams_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Governments benefit in the immediate term by being able to inflate the money supply and pay debts with cheaper currency. They can also extract seigniorage.
constraint_indexing:constraint_classification(greshams_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational perspective, Gresham's Law reflects a recurring pattern where short-term incentives to debase currency undermine long-term economic stability.
constraint_indexing:constraint_classification(greshams_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Long-term savers are unable to protect their wealth and are effectively targeted by the effects of inflation over the long term. This may trap them in poverty over their lifetimes.
constraint_indexing:constraint_classification(greshams_law, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(greshams_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(greshams_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(greshams_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(greshams_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(greshams_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is rated at 0.55 due to the moderate loss of value to holders of 'good money.' Suppression is rated 0.70 because alternatives may be limited or expensive and government enforcement is required.
 *
 * PERSPECTIVAL GAP:
 *   Holders of 'good money' see a snare because their wealth is devalued. Governments see a coordination mechanism because they can manage the money supply. The general populace experiences a mix of benefits and costs, resulting in a tangled rope classification. The analytical observer takes a long-term view and recognizes the inherent instability and wealth transfer associated with Gresham's Law.
 *
 * DIRECTIONALITY LOGIC:
 *   Governments benefit from seigniorage and short-term economic stimulus (low d). Holders of good money are forced to transact with debased currency (high d). General populace experiences a mixed effect (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification correctly captures the dual nature of Gresham's Law. It's not purely extractive because there are some benefits to the government issuing the debased currency and the populace at large when there is no other option, nor a straightforward coordination mechanism due to wealth extraction. The extraction and enforcement elements justify the chosen classification. The rope like properties stem from a government trying to solve the problem of needing more money at a cost of devaluing what is already in circulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_currency_substitution,
    'To what extent can ''good money'' be substituted with other currencies or assets, mitigating the effects of Gresham''s Law?',
    'Econometric analysis of currency substitution rates and asset diversification strategies.',
    'High substitutability reduces the snare effect and makes it easier to escape the bad money. Low substitutability strengthens the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_currency_substitution, empirical, 'The ability to escape the effects of Gresham''s Law').

omega_variable(
    government_commitment_to_sound_money,
    'How credible is the government''s commitment to maintaining the value of its currency?',
    'Analysis of monetary policy decisions, central bank independence, and historical track record.',
    'Strong commitment reduces extractiveness; weak commitment increases it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(government_commitment_to_sound_money, empirical, 'Government''s commitment to sound money').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(greshams_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gres_tr_t0, greshams_law, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gres_tr_t5, greshams_law, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gres_tr_t10, greshams_law, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(gres_be_t0, greshams_law, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gres_be_t5, greshams_law, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(gres_be_t10, greshams_law, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(greshams_law, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
