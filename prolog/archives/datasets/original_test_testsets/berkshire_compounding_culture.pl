% ============================================================================
% CONSTRAINT STORY: berkshire_compounding_culture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_berkshire_compounding_culture, []).

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
 *   constraint_id: berkshire_compounding_culture
 *   human_readable: The Berkshire Hathaway Culture of Compounding
 *   domain: economic/corporate_finance
 *
 * SUMMARY:
 *   The Berkshire Hathaway culture of compounding is a corporate policy and
 *   philosophy that mandates the reinvestment of all earnings rather than
 *   their distribution as dividends. This creates a permanent, ever-growing
 *   capital base centrally allocated by top management. The constraint is the
 *   absolute prohibition on dividends, which coordinates long-term investors
 *   while systematically excluding and extracting from income-oriented
 *   investors and subsidiary managers.
 *
 * KEY AGENTS:
 *   - Long-Term Shareholders: Primary beneficiaries (powerful/arbitrage) who benefit from decades of tax-deferred capital growth.
 *   - Berkshire Management: Primary beneficiaries (institutional/arbitrage) who control the allocation of a vast and permanent capital base.
 *   - Income-Oriented Investors: Primary victims (powerless/trapped) who are denied dividends and must sell shares to generate income.
 *   - Subsidiary Managers: Secondary victims (organized/constrained) who benefit from stability but lose control over the capital their businesses generate.
 *   - Activist Investors: Targets (organized/mobile) whose strategies for unlocking value are structurally suppressed by the culture and voting control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(berkshire_compounding_culture, 0.55).
domain_priors:suppression_score(berkshire_compounding_culture, 0.65).
domain_priors:theater_ratio(berkshire_compounding_culture, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(berkshire_compounding_culture, extractiveness, 0.55).
narrative_ontology:constraint_metric(berkshire_compounding_culture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(berkshire_compounding_culture, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(berkshire_compounding_culture, tangled_rope).
narrative_ontology:human_readable(berkshire_compounding_culture, "The Berkshire Hathaway Culture of Compounding").
narrative_ontology:topic_domain(berkshire_compounding_culture, "economic/corporate_finance").

domain_priors:requires_active_enforcement(berkshire_compounding_culture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(berkshire_compounding_culture, long_term_shareholders).
narrative_ontology:constraint_beneficiary(berkshire_compounding_culture, berkshire_management).
narrative_ontology:constraint_victim(berkshire_compounding_culture, income_oriented_investors).
narrative_ontology:constraint_victim(berkshire_compounding_culture, activist_investors).
narrative_ontology:constraint_victim(berkshire_compounding_culture, subsidiary_capital_planners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCOME INVESTOR (SNARE) — This agent requires current income from investments. Berkshire's absolute no-dividend policy traps their capital, forcing a sale of shares (incurring taxes and transaction costs) to generate cash. From this view, the retained earnings are a form of coercive extraction. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(berkshire_compounding_culture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LONG-TERM SHAREHOLDER (ROPE) — This agent benefits from tax-deferred compounding over decades. They see the no-dividend policy as a pure coordination mechanism to maximize long-term value, willingly deferring gratification. They can arbitrage the market's short-term focus. d≈0.10, f(d)≈-0.07, σ=1.2 → χ≈-0.05. Negative effective extraction indicates a net subsidy.
constraint_indexing:constraint_classification(berkshire_compounding_culture, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SUBSIDIARY MANAGER (TANGLED ROPE) — Managers of acquired companies benefit from the stability and prestige of the Berkshire brand (coordination) but must remit their earnings to Omaha for central reallocation, losing control over their own capital (extraction). Their exit options are constrained. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.47.
constraint_indexing:constraint_classification(berkshire_compounding_culture, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The analyst sees both the powerful coordination function (aligning capital for immense long-term value creation) and the asymmetric extraction (centralizing capital control and denying dividends to those who need them). The structure is a hybrid by definition. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(berkshire_compounding_culture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE TRUE BELIEVER (MOUNTAIN) — From the perspective of a devoted follower of the Buffett/Munger philosophy, the principle of compounding is an immutable, natural law of finance. They see the Berkshire culture not as a set of choices but as the optimal, inevitable expression of this law. The engine will flag this as a false summit, as the high ε and suppression values are inconsistent with a natural law.
constraint_indexing:constraint_classification(berkshire_compounding_culture, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(berkshire_compounding_culture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(berkshire_compounding_culture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(berkshire_compounding_culture, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(berkshire_compounding_culture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(berkshire_compounding_culture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55) is rated as moderate-high because the decision to retain 100% of earnings represents a significant, non-negotiable transfer of capital control from shareholders and subsidiaries to a central authority. Suppression (0.65) is high; the 'no dividend' rule is absolute, enforced by majority voting control and a powerful, self-reinforcing corporate culture that resists any external pressure for alternative capital allocation. Theater Ratio (0.15) is very low, as the compounding mechanism is highly functional and has produced tangible, world-leading results for decades.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a long-term 'partner' in the enterprise, the culture is a perfect Rope, a brilliant coordination tool for wealth creation. For an outsider who needs income, it is a Snare that holds their rightful earnings hostage. For an insider manager of a subsidiary, it is a Tangled Rope, providing the benefits of the Berkshire ecosystem while extracting the fruits of their labor for reallocation. This demonstrates how a single, fixed set of rules can be perceived as benevolent coordination, coercive extraction, or a hybrid of the two, depending entirely on the observer's structural relationship to the cash flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (long-term shareholders, management) have arbitrage exit options and a generational time horizon, leading to a very low derived directionality (d) and a Rope classification. Victims (income investors) are powerless and trapped, leading to a very high 'd' and a Snare classification. Agents in the middle (subsidiary managers) are organized but constrained, experiencing both coordination and extraction, resulting in a moderate 'd' and a Tangled Rope classification. The system's structure sorts participants into these distinct experiences.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy of labeling Berkshire's culture as either purely 'good' (a Rope of genius) or 'bad' (a Snare for tax avoidance). The Deferential Realism framework shows both are valid perspectival truths. The analytical classification of Tangled Rope correctly identifies the core structure: a genuine, powerful coordination function (compounding) that is inextricably linked to an asymmetric extraction mechanism (centralized capital control and dividend suppression). It avoids mislabeling the coercive element as pure coordination or the coordination element as pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    succession_culture_persistence,
    'Can the culture of centralized, disciplined capital allocation survive its founders?',
    'Observing capital allocation decisions (acquisitions, buybacks vs dividends) under the post-Buffett leadership over a 5-10 year period.',
    'If the culture degrades (e.g., initiates a dividend), the constraint''s suppression and extractiveness would decrease, potentially shifting it towards a Rope or Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(succession_culture_persistence, empirical, 'Whether the compounding culture persists after its founders.').

omega_variable(
    capital_allocation_drag,
    'Does Berkshire''s enormous size prevent it from finding investment opportunities that can generate returns exceeding what shareholders could achieve themselves?',
    'Comparing Berkshire''s return on equity (ROE) and book value growth against S&P 500 returns over the next decade.',
    'If returns lag significantly, the core justification for retaining earnings collapses, revealing the mechanism as pure extraction (Snare) rather than value-creating coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_allocation_drag, empirical, 'Whether Berkshire''s size creates a drag on returns.').

omega_variable(
    tax_regime_inversion,
    'How would the constraint''s logic change if capital gains taxes were set significantly higher than taxes on qualified dividends?',
    'Modeling shareholder after-tax returns under a hypothetical inverted tax regime.',
    'An inversion would weaken the tax-deferral benefit, a key part of the coordination function. This would increase the perceived extractiveness for all shareholders, shifting more perspectives toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tax_regime_inversion, conceptual, 'Impact of a hypothetical inversion of dividend vs. capital gains tax rates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(berkshire_compounding_culture, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(berk_tr_t1980, berkshire_compounding_culture, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(berk_tr_t2000, berkshire_compounding_culture, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(berk_tr_t2025, berkshire_compounding_culture, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(berk_be_t1980, berkshire_compounding_culture, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(berk_be_t2000, berkshire_compounding_culture, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(berk_be_t2025, berkshire_compounding_culture, base_extractiveness, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(berkshire_compounding_culture, resource_allocation).
narrative_ontology:affects_constraint(berkshire_compounding_culture, long_term_value_investing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
