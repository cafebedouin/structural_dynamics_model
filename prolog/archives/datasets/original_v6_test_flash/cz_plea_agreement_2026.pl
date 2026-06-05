% ============================================================================
% CONSTRAINT STORY: cz_plea_agreement_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cz_plea_agreement_2026, []).

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
 *   constraint_id: cz_plea_agreement_2026
 *   human_readable: CZ and Binance Global Regulatory Settlement
 *   domain: economic/political/legal
 *
 * SUMMARY:
 *   The plea agreement between Changpeng Zhao (CZ) and the US Department of
 *   Justice (DOJ) represents a significant event in the cryptocurrency
 *   industry. It addresses Binance's failure to maintain an effective
 *   anti-money laundering (AML) program, leading to a settlement that
 *   involves both extraction and coordination. The agreement aims to
 *   establish better regulatory compliance within the cryptocurrency space,
 *   preventing further illicit activity. The plea deal represents a shift of
 *   Binance adhering to legal standards.
 *
 * KEY AGENTS:
 *   - US Department of Justice: Institutional beneficiary (institutional/arbitrage) - benefits from establishing regulatory compliance and sending a message to the industry.
 *   - CZ: Moderate actor (moderate/constrained) - constrained by the plea agreement, facing potential jail time and restrictions.
 *   - Binance Users: Primary victims (powerless/trapped) - face uncertainty and potential losses due to the settlement.
 *   - Financial Markets: Powerful beneficiaries (powerful/mobile) - benefit from increased stability and legitimacy in the cryptocurrency industry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cz_plea_agreement_2026, 0.6).
domain_priors:suppression_score(cz_plea_agreement_2026, 0.5).
domain_priors:theater_ratio(cz_plea_agreement_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cz_plea_agreement_2026, extractiveness, 0.6).
narrative_ontology:constraint_metric(cz_plea_agreement_2026, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(cz_plea_agreement_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cz_plea_agreement_2026, tangled_rope).
narrative_ontology:human_readable(cz_plea_agreement_2026, "CZ and Binance Global Regulatory Settlement").
narrative_ontology:topic_domain(cz_plea_agreement_2026, "economic/political/legal").

domain_priors:requires_active_enforcement(cz_plea_agreement_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cz_plea_agreement_2026, us_department_of_justice).
narrative_ontology:constraint_beneficiary(cz_plea_agreement_2026, financial_markets).
narrative_ontology:constraint_victim(cz_plea_agreement_2026, binance_users).
narrative_ontology:constraint_victim(cz_plea_agreement_2026, cz).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Binance users who may have unknowingly used the platform for illicit activities or who now face uncertainty regarding the platform's future operations. Users are trapped as they are unable to easily recover funds. Experienced extraction is very high due to potential loss of funds or inability to access the platform.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% CZ is constrained by the plea agreement, facing potential jail time and restrictions on his involvement with Binance. However, the agreement also allows CZ to avoid a potentially harsher outcome, demonstrating elements of both extraction and coordination. He benefits from reduced uncertainty compared to fighting charges.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The DOJ benefits from the plea agreement as it sends a message to other cryptocurrency exchanges and individuals about the consequences of non-compliance with AML regulations. It coordinates the adherence to legal standards and regulations within the cryptocurrency space, preventing further illicit activity.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Financial markets benefit as the settlement removes a source of uncertainty regarding Binance's operations and helps legitimize the cryptocurrency industry. Though there are costs associated with Binance users moving their capital to other exchanges, the markets as a whole are able to continue operating.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% From an analytical perspective, the plea agreement represents a tangled rope. While it coordinates better regulatory compliance and provides some stability to the cryptocurrency market, it also involves extraction from Binance and its users. Long-term stability is facilitated while immediate extraction is present.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cz_plea_agreement_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cz_plea_agreement_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cz_plea_agreement_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cz_plea_agreement_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cz_plea_agreement_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): The plea agreement involves significant financial penalties for Binance and restrictions on CZ's involvement, representing a high level of extraction. Additionally, the restrictions placed on Binance can affect it's profitability and operations. Suppression (0.50): The agreement suppresses certain activities and behaviors within the cryptocurrency industry, imposing new regulations and oversight. However, it does not completely eliminate the possibility of illicit activities shifting to other platforms. The markets aren't significantly impacted as new trading entities can appear. Theater ratio (0.30): The theater ratio is low since the agreement has real consequences, with minimal performative compliance measures being implemented. The settlement represents a real regulatory action against Binance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions and interests of the involved parties. Binance users experience the settlement as a snare due to potential losses and uncertainty. The DOJ views it as a coordination mechanism for ensuring regulatory compliance. CZ sees it as a tangled rope, facing both constraints and a path to resolution. Financial markets view this as a net positive for markets stability. The analytical observer classifies the agreement as a tangled rope, acknowledging both the extraction and coordination elements.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality of the settlement is determined by the structural relationships between the agents. The DOJ benefits from increased regulatory control, while Binance and its users bear the costs of the agreement. Users are trapped. CZ is constrained but also benefits from a resolution. The financial markets are in favor of this. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_market_impact,
    'Will the settlement result in a more regulated and stable cryptocurrency market in the long term, or will it simply push illicit activities to other, less regulated platforms?',
    'Monitoring the flow of illicit funds and the rise of new cryptocurrency exchanges in less regulated jurisdictions.',
    'If the market stabilizes, the agreement is a significant step towards legitimacy. If illicit activities increase elsewhere, the agreement is a limited success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_market_impact, empirical, 'Long-term impact of the settlement on the cryptocurrency market.').

omega_variable(
    user_fund_recovery,
    'Will Binance users be able to fully recover their funds, and what measures will be taken to compensate them for potential losses?',
    'Auditing Binance''s fund management practices and tracking the progress of user compensation programs.',
    'Full fund recovery would mitigate the extraction experienced by users. Significant uncompensated losses would reinforce the Snare perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_fund_recovery, empirical, 'The level of the success for user fund recovery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cz_plea_agreement_2026, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cz_p_tr_t0, cz_plea_agreement_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cz_p_tr_t1, cz_plea_agreement_2026, theater_ratio, 1, 0.2).
narrative_ontology:measurement(cz_p_tr_t2, cz_plea_agreement_2026, theater_ratio, 2, 0.3).

% Extraction over time
narrative_ontology:measurement(cz_p_be_t0, cz_plea_agreement_2026, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cz_p_be_t1, cz_plea_agreement_2026, base_extractiveness, 1, 0.55).
narrative_ontology:measurement(cz_p_be_t2, cz_plea_agreement_2026, base_extractiveness, 2, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cz_plea_agreement_2026, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
