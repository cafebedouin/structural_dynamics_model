% ============================================================================
% CONSTRAINT STORY: bnpl_payment_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bnpl_payment_systems, []).

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
 *   constraint_id: bnpl_payment_systems
 *   human_readable: "Buy Now, Pay Later" (BNPL) Payment Systems
 *   domain: economic
 *
 * SUMMARY:
 *   "Buy Now, Pay Later" (BNPL) systems represent a financial innovation that
 *   functions as both a low-friction payment coordination tool and a highly
 *   effective mechanism for extracting value from vulnerable consumers. By
 *   integrating seamlessly into retail checkouts and marketing themselves as
 *   an "interest-free" way to manage cash flow, they obscure their nature as
 *   short-term loans. The business model relies on fees from merchants (who
 *   benefit from increased sales) and late fees from consumers who miss
 *   payments (a frequent occurrence, especially among the target demographic
 *   of lower-income individuals).
 *
 * KEY AGENTS:
 *   - BNPL Providers (e.g., Klarna, Afterpay): Primary beneficiary (institutional/arbitrage) - Profit from merchant fees and consumer late fees.
 *   - Online Merchants: Secondary beneficiary (organized/mobile) - Benefit from increased conversion rates and average order values.
 *   - Low-Income Consumers: Primary victim (powerless/trapped) - Attracted by easy credit, they are most likely to incur late fees and accumulate debt.
 *   - Financially Stable Consumers: Incidental user (moderate/mobile) - Use the service for convenience and experience it as a pure coordination mechanism (Rope).
 *   - Financial Regulators: Institutional observer (institutional/constrained) - Tasked with balancing financial innovation against consumer protection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bnpl_payment_systems, 0.62).
domain_priors:suppression_score(bnpl_payment_systems, 0.75).
domain_priors:theater_ratio(bnpl_payment_systems, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bnpl_payment_systems, extractiveness, 0.62).
narrative_ontology:constraint_metric(bnpl_payment_systems, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bnpl_payment_systems, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bnpl_payment_systems, tangled_rope).
narrative_ontology:human_readable(bnpl_payment_systems, "\"Buy Now, Pay Later\" (BNPL) Payment Systems").
narrative_ontology:topic_domain(bnpl_payment_systems, "economic").

domain_priors:requires_active_enforcement(bnpl_payment_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bnpl_payment_systems, bnpl_providers).
narrative_ontology:constraint_beneficiary(bnpl_payment_systems, online_merchants).
narrative_ontology:constraint_victim(bnpl_payment_systems, low_income_consumers).
narrative_ontology:constraint_victim(bnpl_payment_systems, consumers_with_poor_credit).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEBTED CONSUMER (SNARE) — For consumers with low income or poor credit, BNPL is one of few available financing options. Once late fees accumulate, it becomes a debt trap that is difficult to exit. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.70. This exceeds the snare threshold (χ≥0.66).
constraint_indexing:constraint_classification(bnpl_payment_systems, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: FINANCIALLY STABLE CONSUMER (ROPE) — This user experiences BNPL as a pure convenience tool for cash flow management, paying on time and never incurring fees. They can easily exit to other payment methods like credit cards or cash. For them, it is a pure coordination mechanism. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.32. This is below the rope threshold (χ≤0.35).
constraint_indexing:constraint_classification(bnpl_payment_systems, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: BNPL PROVIDER (ROPE) — The provider views the system as a valuable coordination service connecting merchants with consumers, increasing sales and providing liquidity. From their position, extraction is just revenue. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Negative effective extraction signifies a net beneficiary.
constraint_indexing:constraint_classification(bnpl_payment_systems, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL REGULATOR (TANGLED ROPE) — Regulators are constrained by their mandate to both foster innovation and protect consumers. They see the system's dual nature: a legitimate payment innovation (coordination) that creates significant consumer harm (extraction). d≈0.60, f(d)≈0.88, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(bnpl_payment_systems, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical view recognizes both the genuine coordination function for merchants and financially stable users, and the severe, asymmetric extraction from vulnerable users via late fees and induced overspending. The high extraction and suppression combined with a real coordination function is the definition of a Tangled Rope. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(bnpl_payment_systems, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bnpl_payment_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bnpl_payment_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bnpl_payment_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bnpl_payment_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bnpl_payment_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.62): High. This score reflects the dual revenue streams: significant fees charged to merchants (which are likely passed on to all consumers via higher prices) and steep late fees levied on a substantial portion (30-40%) of users. The model is designed to encourage higher spending, which amplifies the total value extracted. Suppression (0.75): High. BNPL options are aggressively integrated into online checkouts, creating a low-friction path to debt. For the target demographic with poor credit, traditional alternatives are heavily suppressed, making BNPL one of the only accessible options. Theater Ratio (0.60): The "interest-free" framing is highly theatrical, masking the true cost which is realized through late fees and diffuse price inflation. The seamless, instant approval process creates a theater of financial empowerment while obscuring the creation of debt.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark. For BNPL providers and merchants, the system is a Rope, a brilliant coordination tool that boosts commerce. For a financially stable user who pays on time, it also appears as a Rope. However, for the consumer who misses a payment and gets trapped in a cycle of fees, it is a Snare. Regulators and analytical observers, who can see both the coordination function and the asymmetric extraction, correctly classify it as a Tangled Rope. The system's identity is fundamentally dependent on the observer's position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Providers, Merchants) with high exit options (arbitrage, mobile) derive a low 'd' value, resulting in a Rope classification with low or negative effective extraction (χ). Victims (Low-Income Consumers) with no exit (trapped) derive a high 'd' value, leading to high χ and a Snare classification. Agents with a mixed or analytical view (Regulators, Analysts) occupy the middle ground, perceiving the dual functions that define a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   BNPL is a classic case where mandatrophy would occur if a single classification were enforced. Labeling it purely as a 'Snare' would ignore its genuine coordination function for millions of users and merchants. Conversely, labeling it a 'Rope' (as its proponents do) would willfully ignore the immense, targeted extraction from vulnerable populations. The Deferential Realism framework resolves this by showing that Rope, Snare, and Tangled Rope are all valid, simultaneous classifications from different structural positions, with Tangled Rope being the most complete analytical description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_intervention_impact,
    'How will future consumer protection regulations (e.g., mandatory credit reporting, caps on late fees) alter the system''s extractive potential?',
    'Observing market changes in revenue models and late fee incidence in jurisdictions after new regulations are implemented.',
    'Strong regulation could reduce base extractiveness, shifting the constraint towards a Rope. Weak regulation might entrench it as a Tangled Rope or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_intervention_impact, empirical, 'Impact of future regulation on the BNPL business model and its extractiveness.').

omega_variable(
    price_inflation_effect,
    'To what extent are merchant fees (typically 3-6%) passed on to all consumers as higher prices, creating a diffuse subsidy from non-BNPL users to BNPL users?',
    'Econometric studies comparing product pricing at merchants with and without BNPL options, controlling for other variables.',
    'High price pass-through would mean the ''victim'' pool is much larger than just those who pay late fees, strengthening the Tangled Rope classification by revealing a hidden, broad-based extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_inflation_effect, empirical, 'Whether merchant fees are passed on to all consumers via price inflation.').

omega_variable(
    behavioral_vs_rational_overspending,
    'Is consumer overspending via BNPL a result of rational short-term utility maximization or the exploitation of cognitive biases like present bias?',
    'Behavioral economics experiments and longitudinal studies of user financial health after adopting BNPL.',
    'If primarily driven by exploiting cognitive biases, the ''suppression'' metric is understated, as the choice architecture itself is coercive. This would push the classification closer to a Snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_rational_overspending, conceptual, 'Whether BNPL''s model relies on rational choice or exploiting cognitive biases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bnpl_payment_systems, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bnpl_tr_t2015, bnpl_payment_systems, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(bnpl_tr_t2020, bnpl_payment_systems, theater_ratio, 2020, 0.6).
narrative_ontology:measurement(bnpl_tr_t2025, bnpl_payment_systems, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(bnpl_be_t2015, bnpl_payment_systems, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(bnpl_be_t2020, bnpl_payment_systems, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(bnpl_be_t2025, bnpl_payment_systems, base_extractiveness, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bnpl_payment_systems, resource_allocation).
narrative_ontology:affects_constraint(bnpl_payment_systems, consumer_credit_scoring).
narrative_ontology:affects_constraint(bnpl_payment_systems, retail_market_competition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
