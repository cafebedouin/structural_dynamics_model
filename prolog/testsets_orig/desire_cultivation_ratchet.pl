% ============================================================================
% CONSTRAINT STORY: desire_cultivation_ratchet
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_desire_cultivation_ratchet, []).

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
 *   constraint_id: desire_cultivation_ratchet
 *   human_readable: Desire Cultivation Ratchet in Consumer Finance
 *   domain: political_economy/consumer_finance/social_control
 *
 * SUMMARY:
 *   The desire cultivation ratchet operates through the systematic
 *   cultivation of wants that require debt financing to satisfy, creating a
 *   self-reinforcing cycle where aspirational consumption raises the
 *   perceived obligation floor for social participation. Advertising, social
 *   media algorithms, and peer comparison mechanisms work together to
 *   normalize lifestyle standards that exceed median income capacity, making
 *   consumer credit appear necessary rather than optional. The constraint
 *   exhibits genuine coordination functions (lifecycle consumption smoothing,
 *   access to durable goods) alongside extractive mechanisms (interest
 *   accumulation on discretionary purchases, debt service burden that
 *   constrains future choices). The ratchet effect appears in the asymmetry:
 *   lifestyle expectations adjust upward quickly through social comparison
 *   but resist downward adjustment due to identity fusion and status loss
 *   aversion. Theater ratio (0.48) reflects the gap between the stated
 *   function of consumer credit (enabling productive investment and emergency
 *   smoothing) and its actual dominant use (financing discretionary
 *   consumption to meet socially-constructed standards). The constraint has
 *   intensified over the measurement interval (1980-2020) as advertising
 *   platforms have become more sophisticated, social comparison has been
 *   amplified by digital media, and financial products have proliferated to
 *   capture the manufactured demand.
 *
 * KEY AGENTS:
 *   - Aspirational Consumers: Primary victim (powerless/identity_locked) — identity fused with consumption patterns that require debt financing; cannot exit without perceived status loss and social exclusion
 *   - Median Income Households: Secondary victim (moderate/constrained) — face high costs to exit the consumption ratchet but retain some agency; experience both coordination benefits (access to credit) and extraction (debt service burden)
 *   - Consumer Finance Industry: Primary beneficiary (institutional/arbitrage) — captures interest payments and fees from debt-financed consumption; experiences constraint as coordination mechanism enabling market expansion
 *   - Luxury Goods Manufacturers: Secondary beneficiary (institutional/arbitrage) — benefit from expanded market as aspirational goods become normalized necessities
 *   - Advertising Platforms: Secondary beneficiary (institutional/arbitrage) — monetize attention through desire cultivation; algorithmic curation amplifies social comparison
 *   - Financial Literacy Movement: Organized resistance (organized/mobile) — building alternative narratives and tools to break identity lock; sees constraint as temporary problem with sunset through education and norm change
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and extractive overlay; tangled rope classification reflects irreducible ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(desire_cultivation_ratchet, 0.52).
domain_priors:suppression_score(desire_cultivation_ratchet, 0.58).
domain_priors:theater_ratio(desire_cultivation_ratchet, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(desire_cultivation_ratchet, extractiveness, 0.52).
narrative_ontology:constraint_metric(desire_cultivation_ratchet, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(desire_cultivation_ratchet, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(desire_cultivation_ratchet, tangled_rope).
narrative_ontology:human_readable(desire_cultivation_ratchet, "Desire Cultivation Ratchet in Consumer Finance").
narrative_ontology:topic_domain(desire_cultivation_ratchet, "political_economy/consumer_finance/social_control").

domain_priors:requires_active_enforcement(desire_cultivation_ratchet).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(desire_cultivation_ratchet, consumer_finance_industry).
narrative_ontology:constraint_beneficiary(desire_cultivation_ratchet, luxury_goods_manufacturers).
narrative_ontology:constraint_beneficiary(desire_cultivation_ratchet, advertising_platforms).
narrative_ontology:constraint_victim(desire_cultivation_ratchet, aspirational_consumers).
narrative_ontology:constraint_victim(desire_cultivation_ratchet, median_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(desire_cultivation_ratchet, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

constraint_indexing:constraint_classification(desire_cultivation_ratchet, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(desire_cultivation_ratchet, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(desire_cultivation_ratchet, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(desire_cultivation_ratchet, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(desire_cultivation_ratchet_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(desire_cultivation_ratchet, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(desire_cultivation_ratchet, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(desire_cultivation_ratchet, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(desire_cultivation_ratchet_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Consumer credit serves genuine coordination functions (lifecycle smoothing, emergency access, productive investment in education/housing) but the dominant use has shifted toward discretionary consumption financing. The extractiveness reflects the interest burden on debt used to meet socially-constructed rather than material needs. The value has increased over the interval as financial products have proliferated and social comparison mechanisms have intensified. Suppression (0.58): Moderate-high. Exit barriers include identity fusion with consumption patterns (aspirational consumers cannot downshift without perceived status loss), algorithmic amplification of social comparison (platforms curate aspirational content to maximize engagement), normalization of debt financing (credit cards and BNPL products make debt invisible), and coordination trap dynamics (individual restraint is punished by relative status loss unless collective norms shift). Suppression is not total — some agents successfully exit through intentional lifestyle design, financial independence movements, or geographic arbitrage to lower-cost regions. Theater ratio (0.48): Moderate. The stated function of consumer credit (enabling productive investment, smoothing consumption over lifecycle, providing emergency access) is genuine but secondary to the actual dominant function (financing discretionary consumption to meet aspirational standards). Financial literacy education and responsible lending rhetoric are partly performative — they address information gaps but not the structural mechanisms (advertising, algorithmic curation, peer comparison) that cultivate the desires in the first place.
 *
 * PERSPECTIVAL GAP:
 *   The consumer finance industry sees pure coordination (Rope) — they are solving the legitimate problem of providing access to goods and smoothing consumption over time. The financial literacy movement sees a temporary problem with a sunset (Scaffold) — education and norm change will break the ratchet. Median income households see mixed coordination and extraction (Tangled Rope) — credit access enables important purchases but debt service constrains future choices. Aspirational consumers see a trap they cannot escape (Snare) — their identity is fused with consumption patterns that require ongoing debt financing, and exit would mean social exclusion and status loss. The analytical observer sees tangled rope at the civilizational scale — the coordination function is real (credit markets do enable lifecycle smoothing and productive investment) but the extractive overlay is also real (interest accumulation on discretionary purchases captures future income). The perspectival gap reveals that the constraint's type depends on the agent's structural position: beneficiaries see coordination, identity-locked victims see entrapment, constrained victims see mixed benefits and costs, and organized resistance sees a solvable problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Aspirational consumers are identity-locked victims — their self-concept is constituted through consumption patterns that require debt financing. They cannot exit without abandoning the identity they have constructed, making the constraint appear as a snare (high d, high chi). Median income households are constrained victims — they face high costs to exit but retain agency and experience some coordination benefits, producing tangled rope classification (moderate d, moderate chi). The consumer finance industry is the primary beneficiary with arbitrage exit options — they experience the constraint as pure coordination, a mechanism that enables market expansion and captures predictable returns (low d, negative chi). The financial literacy movement sees a scaffold — they are organized agents building alternative pathways (budgeting tools, debt payoff communities, anti-consumerism norms) with an explicit sunset logic: as financial education spreads and alternative status signals emerge, the ratchet's grip weakens. The analytical observer sees tangled rope — genuine coordination function (credit access) with extractive overlay (interest burden on manufactured desires), producing moderate chi at the analytical context.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that consumer credit has both genuine coordination functions and extractive mechanisms operating simultaneously. The coordination function is real: credit markets enable lifecycle consumption smoothing (borrowing when young, repaying when earning more), emergency access (medical expenses, car repairs), and productive investment (education, housing). The extractive function is also real: advertising and social comparison cultivate wants that exceed income capacity, normalizing debt financing for discretionary consumption, and interest accumulation on these purchases captures future income. The tangled rope classification at the analytical level reflects this irreducible duality. The constraint is not 'really' a rope (pure coordination) or 'really' a snare (pure extraction) — it is structurally both, and which aspect dominates depends on the specific use case and the agent's position. The mandatrophy resolution comes from recognizing that the perspectival gap is not a measurement error but a structural feature: different agents experience different mixtures of coordination and extraction based on their power, exit options, and relationship to the constraint. The identity-locked aspirational consumer experiences primarily extraction (snare). The constrained median household experiences both (tangled rope). The institutional beneficiary experiences primarily coordination (rope). All three readings are correct from their respective positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aspiration_vs_manipulation_threshold,
    'At what point does legitimate aspiration become manufactured desire that serves primarily extractive rather than coordinative functions?',
    'Longitudinal analysis of consumer satisfaction trajectories post-purchase; correlation between advertising exposure and debt accumulation controlling for income; comparison of self-reported vs. advertiser-induced purchase motivations',
    'If threshold is low (most aspiration is manufactured): constraint is primarily snare. If threshold is high (most aspiration is authentic): constraint is primarily rope with legitimate coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aspiration_vs_manipulation_threshold, conceptual, 'Boundary between authentic aspiration and manufactured desire').

omega_variable(
    identity_lock_permanence,
    'Is the identity fusion with consumption patterns reversible through education and awareness, or does it represent a stable attractor state that persists even after the mechanisms are understood?',
    'Post-intervention tracking of consumers who complete financial literacy programs; measurement of lifestyle inflation reversal rates; longitudinal studies of consumption pattern changes after debt crisis resolution',
    'If reversible: identity_locked classification overstates permanence, and constrained is more accurate. If stable: identity lock is structural, not just informational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_permanence, empirical, 'Reversibility of consumption-based identity fusion').

omega_variable(
    social_comparison_mechanism,
    'Is the social comparison mechanism primarily peer-driven (organic status competition) or platform-mediated (algorithmic curation of aspirational content)?',
    'A/B testing of social media feed algorithms; comparison of lifestyle inflation rates in high vs. low social media usage cohorts; analysis of consumption pattern changes following platform design changes',
    'If peer-driven: suppression is lower (organic social dynamics, harder to regulate). If platform-mediated: suppression is higher (algorithmic amplification, potentially regulable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_comparison_mechanism, empirical, 'Primary driver of social comparison effects').

omega_variable(
    coordination_function_necessity,
    'Does consumer credit serve a genuine coordination function (smoothing consumption over lifecycle, enabling productive investment) or is it primarily an extraction mechanism that captures future income?',
    'Decomposition of consumer debt by use case (education, medical, productive capital vs. discretionary consumption); analysis of debt service burden relative to income mobility; comparison of lifecycle consumption smoothing in credit-rich vs. credit-constrained economies',
    'If genuine coordination dominates: tangled_rope classification confirmed. If extraction dominates: reclassify toward snare for most agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Proportion of genuine coordination vs. extraction in consumer credit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(desire_cultivation_ratchet, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(desire_cult_tr_t0, desire_cultivation_ratchet, theater_ratio, 0, 0.35).
narrative_ontology:measurement(desire_cult_tr_t10, desire_cultivation_ratchet, theater_ratio, 10, 0.42).
narrative_ontology:measurement(desire_cult_tr_t20, desire_cultivation_ratchet, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(desire_cult_be_t0, desire_cultivation_ratchet, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(desire_cult_be_t10, desire_cultivation_ratchet, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(desire_cult_be_t20, desire_cultivation_ratchet, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(desire_cultivation_ratchet, resource_allocation).
narrative_ontology:affects_constraint(desire_cultivation_ratchet, housing_cost_burden).
narrative_ontology:affects_constraint(desire_cultivation_ratchet, educational_debt_trap).
narrative_ontology:affects_constraint(desire_cultivation_ratchet, healthcare_cost_exposure).

% DUAL FORMULATION NOTE:
% The desire cultivation ratchet is upstream of specific debt categories (housing, education, healthcare) but represents a distinct structural constraint. The downstream constraints have their own extractiveness values reflecting the specific market dynamics in each sector; the desire cultivation ratchet has its own extractiveness reflecting the general mechanism by which advertising and social comparison raise the obligation floor across all consumption categories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
