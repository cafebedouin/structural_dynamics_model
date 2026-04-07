% ============================================================================
% CONSTRAINT STORY: carried_interest_tax_preference
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carried_interest_tax_preference, []).

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
 *   constraint_id: carried_interest_tax_preference
 *   human_readable: Carried Interest Tax Preference in Private Investment
 *   domain: tax_policy/financial_services
 *
 * SUMMARY:
 *   The carried interest tax preference allows private equity, hedge fund,
 *   and venture capital managers to classify compensation (carried interest,
 *   typically 20% of profits) as long-term capital gains rather than ordinary
 *   income, generating tax savings of 15-20 percentage points (capital gains
 *   rates vs. top ordinary income rates of 37%). This constraint demonstrates
 *   the Tangled Rope structure: it exhibits genuine coordination function
 *   (aligning manager incentives with capital provider returns, enabling
 *   efficient capital deployment in illiquid assets) alongside significant
 *   asymmetric extraction (estimated $10-15B annual revenue cost, borne
 *   primarily by wage earners and competing investment sectors). The
 *   constraint has been controversial since the 1950s when it was formulated
 *   for small partnerships; its persistence despite repeated reform attempts
 *   reflects both its coordinative function (the industry lobbies
 *   extensively) and its extractive mechanism (powerful institutional agents
 *   have arbitrage capacity to resist change). Theater_ratio (0.68) reflects
 *   the constraint's increasingly performative status: the original 1950s
 *   rationale (partnership alignment in small firms) no longer applies to
 *   billion-dollar mega-funds with professional management structures
 *   equivalent to traditional corporations, yet the tax treatment persists.
 *   The constraint exhibits lifecycle drift: extractiveness has increased
 *   over the 16-year measurement interval as fund sizes have grown and the
 *   original rationale has become more obviously a cover story for what is
 *   now clearly income reallocation.
 *
 * KEY AGENTS:
 *   - Wage Earners: Powerless/trapped (national scope) — bear increased tax burden as revenue is shifted away from capital income taxation. No mechanism for exit; cannot restructure income into carried interest form.
 *   - Competitive Investment Sectors: Moderate/constrained (national scope) — workers in mutual funds, index funds, traditional asset management pay ordinary income tax on equivalent labor compensation. Can exit only through costly career switching.
 *   - Private Equity Managers: Institutional/arbitrage (national scope) — primary beneficiaries with maximum agency. Can arbitrage: shift fund structures, lobby Congress, relocate management to favorable tax jurisdictions. Experience constraint as legitimate incentive alignment.
 *   - Tax Reform Coalition: Organized/constrained (national scope) — labor unions, progressive advocacy, revenue-focused legislators. Perceive both coordination and extraction. Constrained by legislative gridlock and industry lobbying power.
 *   - Federal Tax Code: Institutional/arbitrage (national scope) — maintains provision through inertia; original function has degraded as fund structures have changed.
 *   - Analytical Observer: Sees genuine coordination-extraction coupling, neither separable from the other.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carried_interest_tax_preference, 0.58).
domain_priors:suppression_score(carried_interest_tax_preference, 0.65).
domain_priors:theater_ratio(carried_interest_tax_preference, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carried_interest_tax_preference, extractiveness, 0.58).
narrative_ontology:constraint_metric(carried_interest_tax_preference, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(carried_interest_tax_preference, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carried_interest_tax_preference, tangled_rope).
narrative_ontology:human_readable(carried_interest_tax_preference, "Carried Interest Tax Preference in Private Investment").
narrative_ontology:topic_domain(carried_interest_tax_preference, "tax_policy/financial_services").

domain_priors:requires_active_enforcement(carried_interest_tax_preference).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carried_interest_tax_preference, private_equity_managers).
narrative_ontology:constraint_beneficiary(carried_interest_tax_preference, hedge_fund_managers).
narrative_ontology:constraint_beneficiary(carried_interest_tax_preference, venture_capital_managers).
narrative_ontology:constraint_victim(carried_interest_tax_preference, wage_earner_tax_base).
narrative_ontology:constraint_victim(carried_interest_tax_preference, public_revenue_base).
narrative_ontology:constraint_victim(carried_interest_tax_preference, competitive_investment_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Bears disproportionate tax burden as carried interest tax avoidance shifts revenue collection to wage income and consumption taxes. No exit mechanism; cannot reorganize income structure to capture preferential treatment. Maximum extraction from structurally immobile position.
constraint_indexing:constraint_classification(carried_interest_tax_preference, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETITIVE INVESTMENT SECTOR (MODERATE/CONSTRAINED) — Workers in mutual funds, index funds, and traditional investment management earn equivalent compensation but pay ordinary income tax. Face career switching costs and retraining barriers to enter carried interest structures. Constrained by practical mobility rather than trapped.
constraint_indexing:constraint_classification(carried_interest_tax_preference, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIVATE EQUITY MANAGER (ROPE) — Net beneficiary experiencing the constraint as coordination mechanism. Carried interest aligns manager incentives with investor returns (legitimate coordination function). Capital gains treatment is incentive alignment tool. Can arbitrage: shift investment structures, relocate to favorable jurisdictions, or organize lobbying coalitions. Maximum agency.
constraint_indexing:constraint_classification(carried_interest_tax_preference, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TAX REFORM COALITION (TANGLED ROPE) — Organized agents (labor unions, progressive advocacy groups, revenue-focused legislators) perceive both coordination and extraction. The constraint has genuine incentive alignment function (attracts capital, compensates risk-taking) alongside asymmetric extraction (estimated $10-15B annual revenue cost). Coalition is constrained by legislative gridlock and industry lobbying power but retains some organizing capacity.
constraint_indexing:constraint_classification(carried_interest_tax_preference, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL TAX CODE (PITON) — The carried interest provision persists through institutional inertia. Original 1950s rationale (aligning manager-investor interests in small partnerships) is largely obsolete given modern fund structures, yet the rule persists. Theater_ratio reflects that the tax code maintains the provision while acknowledging its problematic status — the code itself treats carried interest as a doctrinal anomaly requiring constant legislative scrutiny, revealing that its function has degraded relative to its continued existence.
constraint_indexing:constraint_classification(carried_interest_tax_preference, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint exhibits genuine coordination function (aligning manager interests with capital provider interests, enabling efficient deployment of risk capital) alongside significant asymmetric extraction (capital gains taxation of labor income, estimated 15-20% effective rate advantage vs 37% top ordinary income rate). Both functions are structurally real and interdependent — the tax benefit is the mechanism that enables incentive alignment. This is the defining characteristic of Tangled Rope: genuine coordination cannot be untangled from asymmetric extraction.
constraint_indexing:constraint_classification(carried_interest_tax_preference, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carried_interest_tax_preference_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carried_interest_tax_preference, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carried_interest_tax_preference, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carried_interest_tax_preference, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carried_interest_tax_preference, TR),
    TR >= 0.70.

:- end_tests(carried_interest_tax_preference_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The capital gains preference generates documented tax savings of 15-20 percentage points for carried interest income. Revenue cost estimates range from $10-15B annually (conservative) to $20B (inclusive). This is material extraction from the tax base. However, extractiveness is not higher (0.75+) because there is genuine economic coordination occurring: the tax incentive does alter manager behavior and capital allocation patterns. The extractiveness value reflects the empirical blend of real coordination (40%) and pure income reallocation (60%). Suppression (0.65): High. Barriers to exit include: (a) career/skill specificity — wage earners cannot enter private equity structures without years of retraining; (b) institutional power asymmetry — managers have lobbying resources that constrained actors lack; (c) cognitive capture — the original partnership rationale persists in tax code language despite obsolescence, naturalizing what is actually a policy choice; (d) legislative gridlock — multiple reform attempts have failed since 2010. Theater_ratio (0.68): High. The constraint's theater has increased over the interval as fund economics have changed. Original 1950s rationale (alignment in small partnerships) is increasingly inaccurate for modern mega-funds with institutional management structures. Yet the provision persists through doctrinal inertia and legislative stalemate, revealing that the function has degraded relative to the institutional maintenance required to keep it alive.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the gap between objective extraction magnitude (15B+ annually) and experienced extraction (chi), which varies dramatically by agent position. The constraint exhibits all mechanisms of the tangled rope simultaneously: (1) Genuine coordination — manager incentive alignment with capital provider returns is real and economically consequential; (2) Asymmetric extraction — capital gains treatment creates 15-20pp tax advantage that is pure income reallocation, not payment for coordinative service; (3) Locked interdependence — you cannot remove the tax preference without losing some genuine coordination benefit, because the tax incentive is precisely what enables the incentive alignment to function psychologically; (4) Active enforcement — requires continuous congressional action (or inaction) and IRS regulations to maintain the distinction between ordinary income and capital gains treatment. The gap between beneficiary and victim perspectives is extreme: PE managers classify the constraint as legitimate coordination (Rope), while wage earners classify it as extraction with no coordination benefit (Snare). Both are observing the same structure; the gap reflects that the coordinative function benefits the beneficiary while the extractive cost falls on the victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by their structural relationship to the extraction flow. Private equity managers: d ≈ 0.10 (beneficiary + arbitrage exit → low d → low f(d) → low chi, experiencing constraint as coordination). Wage earners: d ≈ 0.92 (victim + trapped exit → high d → high f(d) → high chi, experiencing maximum extraction). Tax reform coalition: d ≈ 0.58 (victim + constrained exit → moderate-high d → moderate f(d) → moderate chi, mix of extraction and ineffective resistance). The beneficiary's arbitrage exit capacity significantly dampens their experienced extractiveness despite the objective extraction magnitude. The trapped agent's lack of exit amplifies their experienced extractiveness even if the absolute tax cost per individual is smaller. This reveals why the constraint persists despite broad political consensus (polling shows 60-70% public support for carried interest reform): the beneficiaries experience low chi due to their arbitrage capacity, enabling them to lobby effectively, while the victims experience high chi but lack organizing capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (the tension between classifying coordination vs. extraction) by showing that the classification is perspective-dependent and that both readings are structurally accurate from their respective positions. The beneficiary's Rope perspective is not wrong — the constraint does solve a real alignment problem. The victim's Snare perspective is not wrong — the constraint does extract pure economic rent through tax arbitrage. The analytical observer's Tangled Rope perspective is the encompassing view: both functions are real, both are structural, and they are locked together such that eliminating one destroys the other. The mandatrophy is resolved not by choosing one type but by recognizing that the presheaf of perspectives over the observation site is the complete answer. The increasing theater_ratio (0.42 → 0.68 over the interval) reveals one edge of the mandatrophy: the constraint's original rationale has degraded (original purpose no longer applies to mega-funds), yet the rule persists, creating a gap between stated function and actual effect. This is diagnostic for Piton degradation from an institutional perspective, but simultaneously the Tangled Rope classification holds because the actual effect (incentive alignment) continues to function despite the degraded rationale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incentive_alignment_necessity,
    'Is capital gains treatment for carried interest necessary for efficient incentive alignment in investment partnerships, or is it a contingent choice among multiple viable alignment mechanisms?',
    'Comparative analysis: performance of private equity funds with carried interest vs. those using fee-only or equity-only compensation; cross-national comparison of fund performance in jurisdictions with and without carried interest preferences.',
    'If necessary: coordination function is structural, supporting Tangled Rope classification. If contingent: the tax preference is pure extraction and should classify as Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_alignment_necessity, empirical, 'Whether capital gains treatment is structurally necessary for manager-investor alignment').

omega_variable(
    magnitude_of_revenue_loss,
    'What is the true annual revenue cost of carried interest tax preference? Estimates range from $3B to $20B depending on definitional scope and accounting method.',
    'IRS/Treasury analysis of individual tax returns and fund structures claiming carried interest treatment; dynamic scoring models accounting for behavioral responses to potential rule changes.',
    'Higher estimated cost (>15B) suggests larger extraction magnitude, supporting Snare classification from wage-earner perspective. Lower cost (<5B) would reduce extraction perception, shifting perspectives toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magnitude_of_revenue_loss, empirical, 'Annual revenue cost of carried interest tax preference').

omega_variable(
    real_vs_nominal_income_shift,
    'Does carried interest treatment shift income classification without changing actual economic reality (nominal shift only), or does it materially alter investment behavior and capital allocation (real economic effect)?',
    'Econometric analysis of fund formation, capital deployment, and return patterns before/after proposed carried interest rule changes; comparison of fund behavior under different tax regimes.',
    'If real effect: the tax preference enables economically efficient allocation that would not occur under ordinary income taxation, supporting coordination function. If nominal only: the preference is pure redistribution without economic benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_vs_nominal_income_shift, empirical, 'Whether carried interest treatment produces real or nominal economic effects').

omega_variable(
    alternative_alignment_mechanisms,
    'What alternative compensation and incentive structures (restricted equity, performance fees, claw-back provisions, deferred compensation) could replicate the alignment function without capital gains preference?',
    'Survey of global fund structures and compensation practices; analysis of fund structures in high-tax jurisdictions that have eliminated or restricted carried interest treatment.',
    'If viable alternatives exist: the capital gains preference is not unique to alignment, supporting extraction-dominant view. If no viable alternatives: the tax treatment is legitimately coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_alignment_mechanisms, empirical, 'Availability of alternative manager-investor alignment mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carried_interest_tax_preference, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carr_tr_t0, carried_interest_tax_preference, theater_ratio, 0, 0.42).
narrative_ontology:measurement(carr_tr_t8, carried_interest_tax_preference, theater_ratio, 8, 0.58).
narrative_ontology:measurement(carr_tr_t16, carried_interest_tax_preference, theater_ratio, 16, 0.68).

% Extraction over time
narrative_ontology:measurement(carr_be_t0, carried_interest_tax_preference, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(carr_be_t8, carried_interest_tax_preference, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(carr_be_t16, carried_interest_tax_preference, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carried_interest_tax_preference, resource_allocation).
narrative_ontology:affects_constraint(carried_interest_tax_preference, hedge_fund_structural_arbitrage).
narrative_ontology:affects_constraint(carried_interest_tax_preference, progressive_tax_base_erosion).

% DUAL FORMULATION NOTE:
% Carried interest preference is upstream of sector-specific tax arbitrage dynamics. It enables structural arbitrage that affects hedge fund compensation patterns and contributes to broader progressive tax base erosion. The constraint family links carried interest (ε=0.58, Tangled Rope) to downstream institutional behaviors in fund compensation structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carried_interest_tax_preference, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
