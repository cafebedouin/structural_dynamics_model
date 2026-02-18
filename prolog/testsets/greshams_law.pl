% ============================================================================
% CONSTRAINT STORY: greshams_law
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-09
% ============================================================================

:- module(constraint_greshams_law, []).

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
 * * constraint_id: greshams_law
 * human_readable: Gresham's Law ("Bad money drives out good")
 * domain: economic
 * * SUMMARY:
 * Gresham's Law is a monetary principle stating that if two forms of commodity
 * money in circulation have the same face value but different intrinsic values,
 * the "bad money" (debased) will circulate while the "good money"
 * (high intrinsic value) will be hoarded and disappear from circulation. This
 * dynamic is enforced by legal tender laws that suppress the market's ability
 * to price the two currencies differently.
 * * KEY AGENTS:
 * - Wage Earners: Subject (Powerless), forced to accept debased currency at face value.
 * - Currency Issuing Authority: Beneficiary (Institutional), extracts seigniorage by debasing the currency.
 * - Economic Historian: Auditor (Analytical), observes the systemic effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(greshams_law, 0.6). % High extraction via seigniorage (an invisible tax).
domain_priors:suppression_score(greshams_law, 0.7).   % High suppression via legal tender laws.
domain_priors:theater_ratio(greshams_law, 0.1).       % Low theater; this is a highly functional extractive process.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(greshams_law, extractiveness, 0.6).
narrative_ontology:constraint_metric(greshams_law, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(greshams_law, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% It is often presented as an immutable, natural law of economics.
narrative_ontology:constraint_claim(greshams_law, tangled_rope).
narrative_ontology:topic_domain(greshams_law, "economic").
narrative_ontology:human_readable(greshams_law, "Gresham's Law (\"Bad money drives out good\")").

% Binary flags
domain_priors:requires_active_enforcement(greshams_law). % Requires legal tender laws.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(greshams_law, currency_issuing_authority).
narrative_ontology:constraint_victim(greshams_law, wage_earners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a wage earner forced to accept debased currency, the law is a trap
% that systematically erodes their purchasing power.
constraint_indexing:constraint_classification(greshams_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (TANGLED ROPE)
% For the sovereign, debasement is a powerful tool for raising funds (coordination)
% but it simultaneously degrades the currency, risking long-term economic
% instability (asymmetric extraction from the public).
constraint_indexing:constraint_classification(greshams_law, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, the system has a clear coordination function (funding the state)
% and clear asymmetric extraction, enforced by suppression. This is the
% definition of a Tangled Rope.
constraint_indexing:constraint_classification(greshams_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(greshams_law_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(greshams_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(greshams_law, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == tangled_rope.

test(threshold_validation) :-
    % Verify that the base extractiveness meets the high-extraction threshold.
    narrative_ontology:constraint_metric(greshams_law, extractiveness, E),
    E >= 0.46.

:- end_tests(greshams_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the dual nature of currency debasement. The base extractiveness
 * (0.6) is high because seigniorage is a direct, albeit hidden, tax on currency
 * holders. The suppression score (0.7) is high because the entire mechanism
 * relies on legal tender laws preventing citizens from refusing the bad money or
 * pricing it differently from the good money.
 *
 * The Perspectival Gap is stark:
 * - The `powerless` wage earner sees a Snare. Their savings and wages are devalued,
 *   and they have no alternative.
 * - The `institutional` authority sees a Tangled Rope. It's a useful tool for
 *   funding state activities (a coordination function) but carries the risk of
 *   destroying public trust in the currency (the tangled element).
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint could be misclassified as a pure Snare if one only considers
 * the victim's perspective. The Tangled Rope classification resolves this by
 * correctly identifying that the constraint *does* serve a coordination function
 * for the beneficiary (funding the state), even while it asymmetrically extracts
 * value from the public. This prevents the system from dismissing the constraint's
 * functional purpose for its creators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_greshams_law,
    'At what point of debasement does social trust collapse, flipping Gresham''s Law into Thiers'' Law (where good money drives out bad because the bad is worthless)?',
    'Historical analysis of hyperinflationary events and monitoring the velocity of debased currency.',
    'If trust collapses, the Snare breaks, and the system reverts to barter or a new currency standard (a new Mountain).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(greshams_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models a currency system that starts stable and is progressively
% debased over time as the issuing authority increases its extraction.
%
% Theater ratio over time (remains low):
narrative_ontology:measurement(greshams_law_tr_t0, greshams_law, theater_ratio, 0, 0.05).
narrative_ontology:measurement(greshams_law_tr_t5, greshams_law, theater_ratio, 5, 0.08).
narrative_ontology:measurement(greshams_law_tr_t10, greshams_law, theater_ratio, 10, 0.1).

% Extraction over time (increases significantly):
narrative_ontology:measurement(greshams_law_ex_t0, greshams_law, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(greshams_law_ex_t5, greshams_law, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(greshams_law_ex_t10, greshams_law, base_extractiveness, 10, 0.6).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint functions as a way to enforce the state's monetary policy.
narrative_ontology:coordination_type(greshams_law, enforcement_mechanism).

% Gresham's Law is structurally coupled with and dependent on legal tender laws.
narrative_ontology:affects_constraint(legal_tender_laws, greshams_law).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */