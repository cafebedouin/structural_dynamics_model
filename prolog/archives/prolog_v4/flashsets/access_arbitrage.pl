% ============================================================================
% CONSTRAINT STORY: access_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_access_arbitrage, []).

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
 *   constraint_id: access_arbitrage
 *   human_readable: Access Arbitrage: Reporters Pay in Framing for Institutional Access
 *   domain: media/political_economy
 *
 * SUMMARY:
 *   Reporters at major news organizations require access to elite
 *   institutions (The White House, The Fed, Congress) in order to report on
 *   them. This access can create a structural incentive for reporters to
 *   frame their reporting in a way that is favorable to these institutions,
 *   even if it is not in the public interest. This is access arbitrage:
 *   institutions extract favorable coverage in exchange for granting access.
 *   The public discourse suffers from biased framing, and reporters'
 *   integrity is compromised.
 *
 * KEY AGENTS:
 *   - Elite Institutions: Primary beneficiary (institutional/arbitrage) — extracts favorable framing in exchange for access.
 *   - Reporters: Secondary actor (moderate/constrained) — constrained by need for access but benefit from career advancement.
 *   - Public Discourse: Primary victim (powerless/trapped) — trapped by biased framing with no exit option.
 *   - Reporters_integrity: Secondary victim (moderate/constrained) - compromised by need for access, limited exist options due to career concerns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(access_arbitrage, 0.6).
domain_priors:suppression_score(access_arbitrage, 0.7).
domain_priors:theater_ratio(access_arbitrage, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(access_arbitrage, extractiveness, 0.6).
narrative_ontology:constraint_metric(access_arbitrage, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(access_arbitrage, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(access_arbitrage, tangled_rope).
narrative_ontology:human_readable(access_arbitrage, "Access Arbitrage: Reporters Pay in Framing for Institutional Access").
narrative_ontology:topic_domain(access_arbitrage, "media/political_economy").

domain_priors:requires_active_enforcement(access_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(access_arbitrage, elite_institutions).
narrative_ontology:constraint_victim(access_arbitrage, public_discourse).
narrative_ontology:constraint_victim(access_arbitrage, reporters_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The public discourse is trapped; it has no agency and bears the full cost of biased framing. No exit option. Extraction is maximized.
constraint_indexing:constraint_classification(access_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Reporters are constrained by institutional pressure but also benefit from access; they have limited exit options due to career concerns. Experiences both extraction and coordination, giving a tangled rope classification. Their personal integrity is eroded.
constraint_indexing:constraint_classification(access_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Elite institutions benefit from favorable framing. They experience the constraint as coordination. Institutions can arbitrage access for favorable coverage.
constraint_indexing:constraint_classification(access_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analytical observer sees the entire structure over a civilizational time horizon and recognizes the intertwined nature of extraction and coordination. The observer can choose to exit via alternative analysis and reporting.
constraint_indexing:constraint_classification(access_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(access_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(access_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(access_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(access_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(access_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.6) because institutions gain favorable coverage. Suppression is high (0.7) because dissenting voices are suppressed or marginalized. Theater ratio is low (0.3) because the exchange of framing for access is largely implicit, not explicit.
 *
 * PERSPECTIVAL GAP:
 *   The public discourse is unaware that it is being framed in a certain way, so it experiences the constraint as a snare. Reporters are aware of the pressure to frame their reporting in a way that is favorable to elite institutions, so they experience the constraint as a tangled rope. Elite institutions benefit from the exchange of framing for access, so they experience the constraint as a rope. Analytical observer sees the structure and understands how the extraction works.
 *
 * DIRECTIONALITY LOGIC:
 *   Elite institutions benefit and have arbitrage; d close to 0.0. Reporters are victims but have constrained exit; d between 0.5 and 1.0. Public is victim and trapped; d close to 1.0. The engine derives d from these structural relationships. Beneficiaries + arbitrage exit → low d → low/negative χ. Victims + trapped exit → high d → high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by acknowledging the mutual benefit of access and coverage. However, the asymmetric power relationship and the suppression of alternative framings justify the tangled rope classification, rather than pure rope. The access creates a coordination mechanism (reporting) which is being used to provide extraction (positive framing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_funding_models,
    'Can alternative funding models for journalism reduce reliance on institutional access and influence?',
    'Compare reporting quality and bias across different funding models (e.g., public funding, non-profit journalism, subscription models).',
    'If effective, reliance on access may decrease, shifting the constraint towards a rope. If ineffective, the tangled rope or snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_models, empirical, 'Do alternative funding models for journalism reduce reliance on access?').

omega_variable(
    measuring_framing_bias,
    'How can framing bias be objectively measured and quantified?',
    'Develop metrics for analyzing language, tone, and emphasis in news reporting to detect systematic bias in favor of certain institutions or viewpoints.',
    'Better measurement of bias would clarify the extent of extraction from public discourse, potentially reclassifying the public discourse perspective as snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measuring_framing_bias, conceptual, 'How to measure and quantify framing bias?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(access_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, access_arbitrage, theater_ratio, 0, 0.1).
narrative_ontology:measurement(acce_tr_t5, access_arbitrage, theater_ratio, 5, 0.2).
narrative_ontology:measurement(acce_tr_t10, access_arbitrage, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, access_arbitrage, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(acce_be_t5, access_arbitrage, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(acce_be_t10, access_arbitrage, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(access_arbitrage, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
