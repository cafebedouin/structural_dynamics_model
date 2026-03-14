% ============================================================================
% CONSTRAINT STORY: dividend_policy_optionality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dividend_policy_optionality, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dividend_policy_optionality
 *   human_readable: Dividend Policy Optionality in Corporate Governance
 *   domain: corporate_finance/governance
 *
 * SUMMARY:
 *   Dividend policy optionality creates a structural tension between
 *   management's legitimate discretion to allocate capital efficiently and
 *   shareholders' need for credible constraints on rent extraction. The
 *   constraint exhibits multiple classification types from different agent
 *   perspectives: minority shareholders experience it as pure extraction
 *   (snare); management experiences it as coordination; institutional
 *   investors see a problem being solved through reform (scaffold); and the
 *   agency cost theory risks naturalizing a contingent institutional
 *   arrangement as an immutable law of corporate structure. The
 *   extractiveness trajectory shows an initial rise (0.38 → 0.52 over the
 *   interval) reflecting increasing instances of dividend suppression without
 *   credible growth justification, followed by early signs of decline (0.49
 *   at T=15) as regulatory reforms and activist pressure begin constraining
 *   discretion. The theater ratio (0.61) reflects the prevalence of 'growth
 *   story' narratives that justify dividend cuts despite weak return
 *   expectations — analysts and management consistently invoke strategic
 *   investment justifications that are post-hoc rationalizations for
 *   discretionary cash retention.
 *
 * KEY AGENTS:
 *   - Minority Shareholders: Primary victims (powerless/trapped) — cannot exit without losses; face full extraction through discretionary dividend suppression
 *   - Management Team: Primary beneficiary (institutional/arbitrage) — retains discretion to deploy capital for empire-building, compensation increases, or value-destroying acquisitions
 *   - Institutional Investor Coalition: Organized agents (organized/constrained) — coordinate through voting and stewardship to enforce dividend discipline; benefit from coordination mechanism but constrained by distributed holdings
 *   - Regulatory Reform Movement: Powerful actors (powerful/mobile) — driving mandatory disclosure, clawback provisions, and stakeholder representation to create sunset to discretionary management control
 *   - Retained Earnings Doctrine: Institutional actor (institutional/arbitrage) — maintains performative legitimacy for dividend suppression despite weak empirical foundation; maintains through business school curriculum and analyst expectations
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing agency costs as immutable property of corporate form rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dividend_policy_optionality, 0.52).
domain_priors:suppression_score(dividend_policy_optionality, 0.48).
domain_priors:theater_ratio(dividend_policy_optionality, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dividend_policy_optionality, extractiveness, 0.52).
narrative_ontology:constraint_metric(dividend_policy_optionality, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dividend_policy_optionality, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dividend_policy_optionality, tangled_rope).
narrative_ontology:human_readable(dividend_policy_optionality, "Dividend Policy Optionality in Corporate Governance").
narrative_ontology:topic_domain(dividend_policy_optionality, "corporate_finance/governance").

domain_priors:requires_active_enforcement(dividend_policy_optionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dividend_policy_optionality, management_discretion).
narrative_ontology:constraint_beneficiary(dividend_policy_optionality, controlling_shareholders).
narrative_ontology:constraint_victim(dividend_policy_optionality, minority_shareholders).
narrative_ontology:constraint_victim(dividend_policy_optionality, capital_allocation_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY SHAREHOLDER (SNARE) — Cannot exit without realizing losses; faces full extraction through dividend suppression or opportunistic allocation. Management retains discretion to defer dividends indefinitely while pursuing private benefit projects. No meaningful exit option — selling into a suppressed market locks in losses. Maximum extraction experienced by the powerless agent.
constraint_indexing:constraint_classification(dividend_policy_optionality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL INVESTOR COALITION (TANGLED ROPE) — Organized agents (pension funds, asset managers, voting blocs) coordinate through shareholder activism and stewardship codes to enforce dividend policies. Benefits from coordination mechanism that protects capital returns. Constrained by reputational costs of forced actions and by the distributed nature of holdings. Genuine coordination function (enforcing capital discipline) coexists with asymmetric extraction (management discretion to delay or redirect dividends).
constraint_indexing:constraint_classification(dividend_policy_optionality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANAGEMENT TEAM (ROPE) — Experiences dividend policy as pure coordination mechanism: regular payouts discipline capital allocation and enable growth planning. Arbitrage exit option — can shift to debt financing or alternative capital structures if dividend constraints become binding. Benefits from the constraint's legitimacy function (dividend policy makes management credible). Net beneficiary experiencing coordination rather than extraction.
constraint_indexing:constraint_classification(dividend_policy_optionality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM MOVEMENT (SCAFFOLD) — Powerful actors (regulators, governance reformers, index-linked activist investors) see dividend optionality as a temporary coordination failure with a sunset. Mandatory dividend disclosure, clawback provisions, and stakeholder representation on compensation committees are building alternative mechanisms that reduce management discretion. Mobile exit option — regulatory pressure is shifting norms. Extraction levels declining as sunset approaches through institutional reform.
constraint_indexing:constraint_classification(dividend_policy_optionality, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RETAINED EARNINGS DOCTRINE (PITON) — The theoretical principle that retained earnings optimize long-term shareholder value persists despite empirical evidence that discretionary retention often funds managerial empire-building or entrenchment. The doctrine is maintained through institutional inertia — business school curriculum, analyst expectations, corporate governance norms — not because it demonstrably works. Theater ratio reflects the performative 'growth story' that justifies discretionary dividend suppression. Piton classification derives from the high theater of growth narratives masking extraction.
constraint_indexing:constraint_classification(dividend_policy_optionality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / AGENCY COST VIEW (MOUNTAIN) — From a civilizational/universal perspective, the separation of ownership and control creates an inherent agency cost: managers have structural incentives to retain cash for size/control/compensation rather than return it to shareholders. This perspective sees dividend discretion as an immutable property of the corporate form itself. However, the structural data (extractiveness 0.52, suppression 0.48, theater 0.61) contradicts the mountain classification — empirical constraints on management (shareholder votes, regulatory oversight, market discipline, activist pressure) demonstrate that agency costs are not laws of nature but contingent institutional arrangements. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(dividend_policy_optionality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dividend_policy_optionality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dividend_policy_optionality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dividend_policy_optionality, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dividend_policy_optionality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dividend_policy_optionality, TR),
    TR >= 0.70.

:- end_tests(dividend_policy_optionality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Management has substantial discretion to suppress dividends in the name of 'growth,' and minority shareholders bear the cost through diluted capital returns. The extraction is not total (some constraints exist through voting and regulatory oversight) but significant. The trajectory shows rise during period of light governance (0.38 → 0.52) with early reversal due to regulatory pressure. Suppression (0.48): Moderate. Minority shareholders face material barriers to exit (illiquidity, transaction costs, lock-in through index funds) but not absolute barriers — they can sell at some cost or vote. Institutional investors can coordinate voting and stewardship pressure. Neither exit is costless; both are possible. Theater ratio (0.61): Moderate-high. The dominant narrative justifying dividend suppression is the 'growth story' — management claims strategic investments justify capital retention despite weak return evidence. This narrative persists through institutional inertia despite frequent failure to deliver promised returns. Analysts mechanically accept the growth story; business schools teach that retained earnings optimize value despite empirical evidence otherwise.
 *
 * PERSPECTIVAL GAP:
 *   The gap between management (rope), institutional investors (tangled_rope), and minority shareholders (snare) reflects their different structural positions relative to the dividend discretion flow. Management controls the discretion and benefits from optionality; minorities bear the cost with no exit option; organized investors coordinate to create constraints that benefit minorities but acknowledge management's legitimate discretion need. The scaffold perspective reflects a genuine structural shift — regulatory mechanisms are reducing the discretion gap. The piton perspective reflects institutional inertia: retained earnings doctrine maintains theoretical legitimacy despite weak empirical support. The mountain perspective is a false summit: agency costs appear immutable from the civilizational view but are actually contingent on institutional structures (dispersed ownership, weak minority protections, compensation concentrated in stock/options).
 *
 * DIRECTIONALITY LOGIC:
 *   Management team benefits from discretion and experiences low directionality (d ≈ 0.15) — they are the beneficiary, arbitrage exit option, institutional power. Minority shareholders experience high directionality (d ≈ 0.90) — they are victims, trapped exit option, powerless position. Institutional investors coordinate on behalf of minorities but retain some optionality (d ≈ 0.50) — organized power, constrained exit, mixed beneficiary/victim status as coordinators rather than principals. The engine derives d from these structural positions and applies sigmoid f(d) to compute experienced extractiveness chi. Management's arbitrage exit produces negative f(d), dampening their experienced extraction (they see coordination). Trapped minority shareholders produce high f(d), amplifying experienced extraction (they see snare). Organized institutional investors with constrained exit produce moderate f(d), consistent with their tangled_rope experience.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: The mandatrophy is resolved through the perspectival gap itself. All six types are valid readings of the same structural data. Management genuinely experiences the constraint as pure coordination (rope) because they benefit from it and can arbitrage into alternative capital structures. Minorities genuinely experience it as pure extraction (snare) because they bear all costs with no exit. Institutional investors genuinely experience it as tangled_rope because they coordinate to enforce some constraints while acknowledging management's legitimate discretion need. The regulatory reform movement genuinely experiences it as scaffold because new governance mechanisms are creating sunset to discretionary management control. The retained earnings doctrine genuinely experiences it as piton because the theoretical justification persists despite weak empirical foundation. The civilizational analytical observer risks a false summit (mountain) by naturalizing agency costs, but the structural data (extractiveness 0.52, suppression 0.48, theater 0.61) and the perspectival diversity confirm that agency costs are institutional artifacts, not laws of nature. The presheaf over the observation site — the full set of perspectives with their different classifications and directionalities — is the answer, not any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_investment_authenticity,
    'Are discretionary retained earnings genuinely funding high-return growth investments, or are they funding value-destroying managerial projects?',
    'Return on incremental invested capital (ROIC) analysis; comparison of post-retention value creation to cost of capital; econometric studies linking discretionary dividend cuts to subsequent acquisition and compensation choices',
    'If ROIC > cost of capital: retention is coordination mechanism, extraction is minimal. If ROIC < cost of capital: retention is extraction mechanism, dividend discretion is snare. Classification shifts from tangled_rope toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(growth_investment_authenticity, empirical, 'Whether retained earnings fund value-creating or value-destroying investments').

omega_variable(
    minority_shareholder_exit_feasibility,
    'Do minority shareholders have genuinely available exit options (sale, portfolio diversification, voting with feet) or are they structurally trapped?',
    'Liquidity analysis of minority share blocks; transaction cost measurement; correlation between minority shareholding size and exit capacity; empirical study of minority exit patterns in response to dividend suppression',
    'If exits are available at reasonable cost: exit_options should be constrained or mobile, not trapped. If exits are blocked by illiquidity or lock-in: trapped classification confirmed, snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_shareholder_exit_feasibility, empirical, 'Whether minority shareholders have viable exit options').

omega_variable(
    institutional_investor_enforcement_effectiveness,
    'Do institutional investor voting blocs and stewardship codes actually constrain management discretion, or do they represent performative activism?',
    'Dividend policy change rates post-voting pressure; management response to institutional investor proposals; correlation between active institutional ownership and dividend payout ratios; longitudinal tracking of institutional investor threats vs actual enforcement',
    'If effective: institutional investor perspective is genuine tangled_rope with real coordination function. If performative: organized agent experiences theater, classification shifts toward piton. Theater ratio would increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_investor_enforcement_effectiveness, empirical, 'Whether institutional investor enforcement actually constrains management discretion').

omega_variable(
    regulatory_sunset_plausibility,
    'Are mandatory dividend disclosure, clawback provisions, and stakeholder representation mechanisms actually reducing management discretion, or are these reforms merely symbolic?',
    'Comparison of executive compensation structures pre/post-reform; dividend policy stability and predictability in reformed vs unreformed regimes; empirical measurement of discretionary dividend suppression rates before/after regulatory implementation',
    'If sunset is real: scaffold perspective confirmed, theater declining, extraction should show temporal decline. If reforms are symbolic: scaffold perspective is aspirational, no sunset occurs, extraction plateaus or rises. Measurement interval trajectory becomes diagnostic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sunset_plausibility, empirical, 'Whether regulatory reform is reducing management discretion over dividend policy').

omega_variable(
    agency_cost_universality,
    'Is the agency cost inherent to all corporate structures, or is it contingent on specific institutional arrangements (dispersed ownership, weak governance, concentrated control)?',
    'Comparative institutional analysis: dividend discretion in widely-held vs family-controlled vs state-owned corporations; variation across legal regimes (civil law vs common law, strong vs weak minority protections); empirical measurement of agency costs under different ownership structures',
    'If inherent: mountain classification confirmed (agency costs are immutable). If contingent: mountain is false summit (agency costs are institutional artifacts). Classification shifts from mountain toward tangled_rope or snare depending on structural controls present.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agency_cost_universality, empirical, 'Whether agency costs are universal or contingent on institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dividend_policy_optionality, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divopt_tr_t0, dividend_policy_optionality, theater_ratio, 0, 0.52).
narrative_ontology:measurement(divopt_tr_t5, dividend_policy_optionality, theater_ratio, 5, 0.58).
narrative_ontology:measurement(divopt_tr_t10, dividend_policy_optionality, theater_ratio, 10, 0.61).
narrative_ontology:measurement(divopt_tr_t15, dividend_policy_optionality, theater_ratio, 15, 0.57).

% Extraction over time
narrative_ontology:measurement(divopt_be_t0, dividend_policy_optionality, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(divopt_be_t5, dividend_policy_optionality, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(divopt_be_t10, dividend_policy_optionality, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(divopt_be_t15, dividend_policy_optionality, base_extractiveness, 15, 0.49).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dividend_policy_optionality, resource_allocation).
narrative_ontology:affects_constraint(dividend_policy_optionality, executive_compensation_asymmetry).
narrative_ontology:affects_constraint(dividend_policy_optionality, minority_shareholder_protection_gaps).
narrative_ontology:affects_constraint(dividend_policy_optionality, capital_market_information_asymmetry).

% DUAL FORMULATION NOTE:
% Dividend policy optionality decomposes into three distinct constraints with different ε values: (1) discretionary capital allocation (ε=0.52, tangled_rope) — management control over retained earnings for investment/acquisition decisions; (2) minority shareholder exit barriers (ε=0.68, snare) — liquidity constraints and lock-in preventing exit when dividends are suppressed; (3) growth narrative theater (ε=0.45, piton) — institutional maintenance of 'retained earnings optimize value' doctrine despite weak empirical foundation. This story addresses the primary coordination-extraction hybrid. The minority exit constraint (downstream) has higher extractiveness; the narrative theater constraint (downstream) has higher performativity. All three are linked through management discretion flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dividend_policy_optionality, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
