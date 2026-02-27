% ============================================================================
% CONSTRAINT STORY: broke_vs_poor_grocery_math
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_broke_vs_poor_grocery_math, []).

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
 *   constraint_id: broke_vs_poor_grocery_math
 *   human_readable: The Cognitive Load of Poverty (Grocery Math)
 *   domain: economic/social
 *
 * SUMMARY:
 *   The cognitive load of poverty—exemplified by 'grocery math,' the
 *   mandatory real-time mental tallying of every item's cost to avoid
 *   exceeding available funds—represents a structural constraint that
 *   extracts cognitive capacity from low-income households. This constraint
 *   operates across six distinct perspectives: a Snare for the shopper
 *   (powerless, trapped, no exit), a Rope for the grocer (institutional,
 *   arbitrage options, benefits from pricing structure), a Tangled Rope for
 *   the affluent (powerful, mobile, can exit but sometimes benefits from
 *   optimization), a Piton for market ideology (institutional, rationalizes
 *   the constraint as efficient pricing despite evidence of atrophy), a false
 *   Mountain for the analytical observer (risks naturalizing a design choice
 *   as inevitable scarcity), and a Scaffold for safety net programs
 *   (organized, constrained, temporary support without structural
 *   displacement). The constraint's extractiveness (0.58) reflects that the
 *   cognitive tax on the poor is real and severe, but is not as total as pure
 *   exploitation—the shopper is still making choices, just under extreme
 *   constraint. The low theater ratio (0.35) indicates that grocery math is
 *   functionally real (not purely performative), distinguishing this from
 *   fully degraded pitons, but the historical data shows theater increasing
 *   as retail complexity grows and justifications become more rationalized.
 *   The constraint is mandatrophy-resolved through this multiperspectival
 *   analysis: it is not a coordination mechanism disguised as extraction (no
 *   hidden Rope underneath), nor is it an immutable law of scarcity (the
 *   mountain is false). It is a Snare maintained by institutional design
 *   choices in retail structure and validated by ideology that naturalizes
 *   those choices.
 *
 * KEY AGENTS:
 *   - Low-Income Households: Primary victim (powerless/trapped) — mandatory grocery math extraction; no exit from food purchasing; cognitive load spills over to other decision-making domains
 *   - Retail Grocers: Primary beneficiary (institutional/arbitrage) — profit from price discrimination enabled by item-by-item pricing; can exit the constraint (use technology, price uniformly) but choose not to
 *   - Affluent Households: Secondary actor (powerful/mobile) — can exit grocery math through abstention, bulk purchasing, or convenience; experience mild constraint but benefit from optimization options
 *   - Market Ideology / Efficient Pricing Doctrine: Institutional actor (institutional/arbitrage) — justifies item-by-item pricing as 'transparent, individual-choice respecting' (piton rationalization); functional purpose (market discovery) has atrophied
 *   - Social Safety Net Programs (SNAP, food banks, co-ops): Organized agent (organized/constrained) — provides temporary support (scaffold) but has not yet displaced the underlying retail constraint
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a design choice (retail structure) as an immutable law of scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(broke_vs_poor_grocery_math, 0.58).
domain_priors:suppression_score(broke_vs_poor_grocery_math, 0.72).
domain_priors:theater_ratio(broke_vs_poor_grocery_math, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, extractiveness, 0.58).
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(broke_vs_poor_grocery_math, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(broke_vs_poor_grocery_math, snare).
narrative_ontology:human_readable(broke_vs_poor_grocery_math, "The Cognitive Load of Poverty (Grocery Math)").
narrative_ontology:topic_domain(broke_vs_poor_grocery_math, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_victim(broke_vs_poor_grocery_math, low_income_households).
narrative_ontology:constraint_victim(broke_vs_poor_grocery_math, cognitive_capacity_poor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME GROCERY SHOPPER (SNARE) — No exit from mandatory cost calculation; every shopping trip requires real-time mental arithmetic to avoid overdraft, shame, or impossible choices. Suppression is total: no viable alternative to shopping for food, no way to avoid the cognitive tax. Extractiveness is experienced maximally — the constraint extracts cognitive capacity that could be applied to job training, child development, or financial planning.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RETAIL GROCER (ROPE) — Benefits from the constraint: item-by-item pricing (vs bulk discounts, vs transparent average-cost shopping) enables price discrimination and captures surplus from budget-constrained customers. Price variance across locations and timing creates arbitrage opportunities the grocer exploits but the poor customer cannot. Yet the grocer also sees the constraint as coordinating their supply chain and pricing system — they've internalized the constraint as a normal feature of retail operations. This perspective experiences the constraint as coordination (Rope) because the grocer has agency and benefits.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: AFFLUENT HOUSEHOLD (TANGLED ROPE) — Experiences grocery math as optional, not mandatory. Can afford to not calculate (use credit, don't check balances, buy premium convenience items). But still benefits from item-by-item pricing for purchasing optimization. Also bears a hidden cost: the cognitive framing of 'poor shopping' vs 'normal shopping' creates social distance and stigma, which maintains the constraint's psychological force. Moderate extraction because the affluent have exit options (abstention from counting, bulk purchases, online shopping with cart visibility) but also experience some mild benefit (fine-tuning their budget).
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: MARKET IDEOLOGY / EFFICIENT PRICING (PITON) — The constraint is justified as 'transparent, individual-choice-respecting retail pricing' — a good coordination mechanism (Rope). But empirical evidence shows that item-by-item pricing, while efficient for aggregate markets, extracts disproportionately from cognitively constrained actors and creates deadweight loss (bad decisions due to calculation errors, skipped nutrition, rushed purchases). The functional justification (market efficiency) has largely atrophied; the constraint persists through institutional inertia and vendor lock-in. Theater ratio (0.35) is moderate — some genuine coordination function remains, but much performative justification obscures the extraction mechanism. This is a piton: the original purpose (efficient market discovery) has been superseded by digital pricing tools, yet the constraint persists because alternatives would require institutional redesign.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / COGNITIVE BOTTLENECK (MOUNTAIN) — From a universal, civilizational perspective, there is an irreducible cognitive load to managing scarce resources: any finite being with limited funds must perform *some* mental accounting to avoid ruin. The constraint appears to be a natural law of scarcity economics. However, the analytical observer recognizes that this is a false summit. The magnitude of the load (and its behavioral consequences) is contingent on the choice of retail structure. Real-time individual item enumeration vs. batch shopping with pre-set budgets vs. cooperative bulk purchasing vs. algorithmic shopping assistants are all technologically feasible. The 'mountain' framing naturalizes what is actually a vendor and institutional design choice.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SOCIAL SAFETY NET / POLICY INTERVENTION (SCAFFOLD) — Programs like SNAP (food stamps with pre-set benefit limits), community food banks, and subsidized cooperative grocery models introduce temporary support mechanisms that reduce cognitive load: fixed monthly budgets, community pooling, and opt-out pricing. These are explicitly scaffolded as temporary or voluntary. However, the scaffold has not yet displaced the snare — existing retail infrastructure still imposes the grocery math calculation even for beneficiaries. The sunset logic is present (policy makers envision these programs as transitional until broader economic mobility improves) but the actual functional displacement is partial. Extraction χ is moderate in this perspective because constrained organizational actors can push back (advocacy, policy change, cooperative formation) but cannot simply exit the market system.
constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(broke_vs_poor_grocery_math_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(broke_vs_poor_grocery_math, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(broke_vs_poor_grocery_math, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(broke_vs_poor_grocery_math, TR),
    TR >= 0.70.

:- end_tests(broke_vs_poor_grocery_math_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated, reflecting that grocery math imposes a genuine cognitive tax on budget-constrained actors. The value is not as high as pure predatory extraction (0.70+) because the constraint leaves shopper agency intact—they can still optimize, compare, and make choices. The 0.58 value captures that the extraction is severe enough to impair other decision-making (opportunity cost) and creates barrier to upward mobility, but is not total enslavement. Historical progression from 0.35 to 0.58 reflects that retail complexity has increased (more SKUs, more pricing variations, more complex discount structures) without corresponding increases in shopper cognitive support—extraction has intensified. Suppression (0.72): High. There is no viable exit from food shopping (biological necessity), no way to delegate the mental calculation without trusting someone else (social risk for poor households), and no institutional alternative that fully replaces the retail system (food banks and cooperatives are supplementary, not primary). The suppression captures that poor households face genuine lock-in. Theater ratio (0.35): Moderate, indicating that grocery math has real functional content (prices do vary, comparing is rational, the calculation does affect budget adherence) but also increasingly performative justification. The low theater distinguishes this from fully degraded pitons (like meaningless compliance rituals), but the ratio reflects that market efficiency ideology increasingly exceeds the actual coordination benefit.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a textbook perspectival gap between powerless and institutional actors. The shopper sees Snare (maximum extraction, no escape). The grocer sees Rope (coordination mechanism, profit opportunity). The gap derives from exit options: the shopper cannot exit; the grocer can exit any time but chooses not to (exit cost < benefit, so exit is voluntary). The affluent household sees a third perspective (Tangled Rope, mixed experience, exit available). The false Mountain from the analytical observer reveals how ideology can naturalize institutional design choices as laws of scarcity. The Scaffold from the policy perspective shows that alternative institutional structures (fixed-budget shopping, cooperative pricing, algorithmic assistance) exist and are partly implemented, but have not displaced the snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the low-income shopper: d ≈ 0.95 (nearly full target). Powerless agent with trapped exit options and victim status (bears cognitive cost) yields maximum experienced extraction. The sigmoid f(d) converts this to highest effective extractiveness. Directionality for the grocer: d ≈ 0.05 (nearly full beneficiary). Institutional agent with arbitrage options and beneficiary status (profits from price structure) yields minimum or negative experienced extractiveness—the grocer benefits. Directionality for the affluent household: d ≈ 0.50 (symmetric, mobile exit). Powerful agent with mobile exit options can experience the constraint as either beneficial (if they choose to optimize) or absent (if they choose to abstain). Directionality for market ideology: d ≈ 0.35 (moderate beneficiary with some cost). The ideology benefits from naturalizing the constraint but bears the cost of cognitive dissonance when reality contradicts the efficiency narrative—piton classification reflects this inertial maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is not mislabeled. It is genuinely a Snare from the perspective of the low-income household—extractive (0.58), suppressive (0.72), with no coordination function that could justify calling it a Rope. The apparent coordination function (retail pricing discovers market equilibrium) is secondary to the extraction mechanism for the poorest actors. The constraint could theoretically be classified as a Tangled Rope if one emphasized the grocer's coordination benefit, but the victim group (low-income households) is so clearly powerless and trapped that snare is the authoritative classification. The mandatrophy resolution demonstrates that the constraint is not ambiguously hybrid—it is a clear snare from the perspective of the agent bearing the actual cognitive cost. The piton perspective (market ideology) and the false mountain (scarcity naturalization) are important for understanding how the constraint is *justified*, not for classifying its actual structure. The constraint maintains its snare classification because the victims have no exit and the extraction is real, regardless of the rationalizations offered by other perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_load_quantification,
    'What is the actual magnitude of cognitive load imposed by grocery math, and how does it vary with budget constraint severity?',
    'Empirical measurement: eye-tracking studies during shopping, reaction-time tasks pre/post-shopping, error rates in budget calculations by income group, cortisol levels during shopping under budget pressure',
    'If load is mild (< 5% of working memory): constraint is closer to Rope (acceptable coordination cost). If load is severe (> 20% of working memory): constraint is clearly Snare (extraction dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_load_quantification, empirical, 'Magnitude of cognitive load from budget-constrained grocery shopping').

omega_variable(
    alternative_retail_sufficiency,
    'Would alternative retail structures (pre-packaged budget bundles, cooperative bulk pricing, algorithmic shopping assistants, transparent average-cost checkout) eliminate the grocery math constraint or merely shift its form?',
    'Pilot programs testing alternative retail models with low-income populations; measurement of cognitive load, purchase satisfaction, and budget adherence under each model',
    'If alternatives work: constraint is a design choice (snare), not a natural necessity (mountain). If alternatives fail: constraint may reflect deeper scarcity management problems (mountain candidates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_retail_sufficiency, empirical, 'Whether alternative retail structures can reduce cognitive load from budget constraints').

omega_variable(
    spillover_to_decision_making,
    'Does the cognitive load of grocery math actually impair decision-making quality in other domains (financial planning, job search, health behavior), or is the spillover effect modest?',
    'Longitudinal studies measuring cognitive task performance, financial decision quality, and opportunity pursuit rates conditional on grocery math load; randomized interventions removing budget pressure and measuring spillover effects',
    'If spillover is large: constraint extracts significant opportunity cost (snare is correct). If spillover is negligible: constraint''s harm is localized (closer to Rope or Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spillover_to_decision_making, empirical, 'Whether grocery math cognitive load impairs decision-making in other domains').

omega_variable(
    institutional_intent_vs_outcome,
    'Is the cognitive load a *side effect* of retail pricing (Rope with externality), an *enabling mechanism* for price discrimination (Snare by design), or an *inevitable artifact* of scarcity (Mountain)?',
    'Historical analysis of retail technology adoption and pricing design choices; interviews with retail designers and economists about explicit vs. implicit intent; comparison of cognitive load across countries with different pricing regulations',
    'If side effect: constraint may be remediable without major institutional change. If enabling mechanism: constraint is actively maintained (clear snare). If inevitable: constraint is harder to escape (mountain candidates).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_intent_vs_outcome, conceptual, 'Whether cognitive load from grocery math is side effect, design feature, or inevitable artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(broke_vs_poor_grocery_math, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bvpgm_tr_t0, broke_vs_poor_grocery_math, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bvpgm_tr_t5, broke_vs_poor_grocery_math, theater_ratio, 5, 0.32).
narrative_ontology:measurement(bvpgm_tr_t10, broke_vs_poor_grocery_math, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(bvpgm_be_t0, broke_vs_poor_grocery_math, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bvpgm_be_t5, broke_vs_poor_grocery_math, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bvpgm_be_t10, broke_vs_poor_grocery_math, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(broke_vs_poor_grocery_math, resource_allocation).
narrative_ontology:affects_constraint(broke_vs_poor_grocery_math, poverty_trap_asset_accumulation).
narrative_ontology:affects_constraint(broke_vs_poor_grocery_math, decision_fatigue_opportunity_cost).
narrative_ontology:affects_constraint(broke_vs_poor_grocery_math, financial_literacy_investment_barrier).

% DUAL FORMULATION NOTE:
% The grocery math constraint is downstream of the broader poverty trap but represents a distinct structural constraint. The cognitive load is specifically generated by item-by-item retail pricing structure, not by scarcity alone. Alternative institutional structures (bulk cooperatives, fixed-budget assisted shopping, algorithmic carts) could reduce the constraint's extractiveness without changing the underlying scarcity condition. This decomposition separates the unavoidable cognitive load of resource scarcity (mountain) from the avoidable cognitive load of retail design choices (snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
