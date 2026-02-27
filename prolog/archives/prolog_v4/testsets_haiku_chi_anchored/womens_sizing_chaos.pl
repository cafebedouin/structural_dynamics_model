% ============================================================================
% CONSTRAINT STORY: womens_sizing_chaos
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_womens_sizing_chaos, []).

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
 *   constraint_id: womens_sizing_chaos
 *   human_readable: Chaotic and non-standardized system of US women's clothing sizes
 *   domain: consumer_retail/economic_regulation
 *
 * SUMMARY:
 *   The US women's clothing sizing system is a fragmented, non-standardized
 *   ecosystem where manufacturers maintain proprietary sizing schemes despite
 *   the existence of government size standards (NIST PS 42, PS 42.4)
 *   established in 1958 and 1971. This fragmentation creates systematic
 *   extraction: consumers bear cognitive and financial costs (repeated fit
 *   failures, alteration expenses), plus-size women face both higher prices
 *   and reduced inventory, and small retailers cannot compete on
 *   standardization. The constraint exhibits snare characteristics — high
 *   suppression (no unified exit pathway), high extraction (fragmentation
 *   benefits manufacturers through price discrimination and reduced inventory
 *   risk), and moderate theater (legacy standards exist but are
 *   performatively ignored). The chaos has deepened over the past 50 years as
 *   'vanity sizing' (progressively inflating nominal sizes while keeping
 *   actual dimensions stable) became a deliberate profit strategy, allowing
 *   retailers to signal lower prices while extracting margin through
 *   psychological pricing. The theatrical dimension reflects the persistence
 *   of NIST standards as regulatory fiction — manufacturers nominally
 *   reference them while systematically violating them. Industry resistance
 *   to standardization (exemplified by ASTM Committee F6.02 proposals) is
 *   structural: non-standardization is directly profitable through inventory
 *   optimization, price discrimination, and reduction of return rates caused
 *   by standardized reference.
 *
 * KEY AGENTS:
 *   - Women consumers (general): Primary victim (powerless/trapped) — forced to navigate fragmented sizing with no stable reference, bearing search costs and fit friction
 *   - Plus-size women: Primary victim (powerless/trapped) — secondary victimization with higher prices (20-30% markup), limited inventory, and inconsistent vanity sizing, compounding trapped exit
 *   - Apparel manufacturers and large retailers (Gap, Target, Amazon): Primary beneficiary (institutional/arbitrage) — profit from non-standardization through vanity sizing, price discrimination, and reduced inventory risk
 *   - NIST and legacy government standards: Institutional theater (institutional/constrained) — standards persist on paper but lack enforcement; manufacturers ignore without consequence
 *   - Consumer advocacy groups and ASTM Committee: Organized victim-agents (organized/mobile) — perceive coordination solution but face active resistance from beneficiary manufacturers
 *   - Independent tailors and alteration shops: Secondary victim (moderate/constrained) — depend on fit friction for demand but are not primary extractors; trapped in supply chain
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing fragmentation as inevitable consequence of body diversity, missing that standardization is both technically feasible and implemented in other markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(womens_sizing_chaos, 0.52).
domain_priors:suppression_score(womens_sizing_chaos, 0.68).
domain_priors:theater_ratio(womens_sizing_chaos, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(womens_sizing_chaos, extractiveness, 0.52).
narrative_ontology:constraint_metric(womens_sizing_chaos, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(womens_sizing_chaos, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(womens_sizing_chaos, snare).
narrative_ontology:human_readable(womens_sizing_chaos, "Chaotic and non-standardized system of US women's clothing sizes").
narrative_ontology:topic_domain(womens_sizing_chaos, "consumer_retail/economic_regulation").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(womens_sizing_chaos, apparel_manufacturers).
narrative_ontology:constraint_beneficiary(womens_sizing_chaos, fast_fashion_retailers).
narrative_ontology:constraint_victim(womens_sizing_chaos, women_consumers).
narrative_ontology:constraint_victim(womens_sizing_chaos, plus_size_women).
narrative_ontology:constraint_victim(womens_sizing_chaos, independent_tailors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN CONSUMER (SNARE) — Trapped within a fragmented sizing ecosystem with no standardized reference. Cannot exit clothing purchases; forced to navigate inconsistent labeling, trial-and-error fitting, and psychological friction from vanity sizing disparities. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(womens_sizing_chaos, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PLUS-SIZE WOMAN CONSUMER (SNARE) — Doubly trapped: smaller inventory, higher prices (often 20-30% markup), limited style options, and inconsistent vanity sizing compression across brands. No exit from needing clothing; suppression particularly severe. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(womens_sizing_chaos, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: APPAREL MANUFACTURERS/RETAILERS (ROPE) — Benefit from non-standardization. Fragmentation allows vanity sizing (size 6 in Brand A = size 8 in Brand B) to mask inflation and preserve profit margins without raising nominal prices. Leverage consumer confusion for inventory optimization and price discrimination. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary through reduced effective extraction.
constraint_indexing:constraint_classification(womens_sizing_chaos, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY GOVERNMENT STANDARDS (PITON) — NIST PS 42 (1958) and PS 42.4 (1971) defined women's clothing size standards based on body measurements. These standards were ignored by the industry upon adoption — manufacturers continued proprietary systems for competitive reasons. The standards persist as regulatory theater (manufacturers claim compliance while violating them), but have zero functional enforcement. theater_ratio=0.61. Institutional inertia maintains the appearance of standardization without its substance.
constraint_indexing:constraint_classification(womens_sizing_chaos, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSUMER ADVOCACY/ASTM COALITION (TANGLED ROPE) — Organized actors (consumer groups, size-diversity advocates, ASTM Committee F6.02) perceive both coordination opportunity and extraction resistance. Pursuing standardization (ASTM D5219) could reduce consumer friction and expand market access for smaller brands. But large retailers actively resist (coordination failure is profitable for them). The constraint provides mixed coordination function (potential standardization pathway) and active suppression (industry opposition). d≈0.52, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(womens_sizing_chaos, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INDEPENDENT TAILORS (SNARE) — Depend on customer willingness to pay for alterations driven by sizing misfit. Chaotic sizing increases demand for tailoring but also creates a secondary extraction mechanism: consumers bear alteration costs (~$50-150 per garment) that would be unnecessary under standardization. Tailors have constrained mobility (geographically bound, small scale) and are victims of the constraint they superficially benefit from. d≈0.80, f(d)≈1.22, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(womens_sizing_chaos, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN RISK) — The constraint risks being naturalized as 'human body diversity makes standardization impossible.' But this is structurally false: body diversity exists everywhere, yet most global clothing markets (EU, Japan, Australia) have either government-enforced or industry-wide standards. The chaos is not inherent to female bodies; it is enforced by manufacturer fragmentation strategies. The engine detects this as a false summit: ε=0.52 and suppression=0.68 contradict the natural-law narrative.
constraint_indexing:constraint_classification(womens_sizing_chaos, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(womens_sizing_chaos_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(womens_sizing_chaos, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(womens_sizing_chaos, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(womens_sizing_chaos, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(womens_sizing_chaos, TR),
    TR >= 0.70.

:- end_tests(womens_sizing_chaos_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Manufacturers extract through multiple mechanisms: (1) vanity sizing inflates nominal sizes, disguising inflation and extraction of margin, (2) inventory optimization under fragmentation reduces risk to retailer but increases friction cost to consumer, (3) price discrimination via plus-size markup (20-30% higher price, 30-40% lower volume) captures surplus. The extraction is not as severe as a pure monopolistic snare (consumers can still purchase, fit problems are inconvenient rather than survival-level), but it exceeds pure coordination failure. Measurement trajectory shows increasing extractiveness (0.35 → 0.52 over 50 years) reflecting the deliberate deployment of vanity sizing as a profit strategy. Suppression (0.68): High. Limited alternatives exist: consumers cannot easily abandon clothing purchase; fragmentation prevents formation of a unified standard; manufacturers actively block standardization efforts; no enforceable standards exist despite government versions. Plus-size women face particularly severe suppression (constrained inventory, higher prices, limited alternative retailers). Theater ratio (0.61): Moderate-high. NIST standards and manufacturer claims of compliance are performative theater — standards are violated systematically with no enforcement. The theatrical dimension reflects the regulatory fiction that standardization exists when it does not. Theater has increased over the interval (0.38 → 0.61) as the gap between claimed and actual compliance has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows strong perspectival divergence: the general consumer sees a snare (trapped by fragmentation, bearing search and fit costs), the plus-size consumer sees an intensified snare (trapped plus price-discriminated), the beneficiary retailer sees coordination-like benefits (profitable inventory optimization), the government standard sees piton degradation (persisting without function), and the analytical observer risks seeing natural law (falsely naturalizing fragmentation as inevitable given body diversity). The gap is particularly sharp between the consumer victim and the retailer beneficiary: the same structural phenomenon (size variation between brands) appears as an extractive trap to one agent and as a profitable coordination mechanism to another. The organized advocacy coalition perceives both: they see the coordination solution (standardization would solve the problem) but face active suppression (retailer resistance is explicit and organized).
 *
 * DIRECTIONALITY LOGIC:
 *   General women consumers: Victim + trapped → d≈0.92, f(d)≈1.40. Near-maximal extraction. Plus-size women: Victim + trapped (with secondary victimization via price markup) → d≈0.95, f(d)≈1.42. Maximum extraction due to dual victimization. Apparel manufacturers/retailers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; fragmentation directly profits them. Independent tailors: Victim + constrained → d≈0.80, f(d)≈1.22. High extraction; they depend on fit friction but do not control it. Consumer advocacy/ASTM: Organized + mobile → d≈0.52, f(d)≈0.65. Moderate extraction; they have agency and see a path forward (standardization) but face active resistance. Legacy standards: Institutional + constrained → d≈0.15 (piton classification comes from theater gate, not directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: The constraint exhibits snare characteristics across multiple victim perspectives (general consumer, plus-size consumer, tailors) while showing clear beneficiary extraction (retailers). The extractiveness (0.52) and suppression (0.68) exceed the rope thresholds (ε ≤ 0.45, suppression ≤ 0.40), confirming snare rather than coordination failure. The constraint is not a market coordination problem that would spontaneously resolve through price signals or voluntary standardization — it is a stable extraction equilibrium because non-standardization is directly profitable for the dominant retailers. The presence of existing government standards (NIST) that are systematically ignored demonstrates that standardization is technically feasible but suppressed for profit reasons. The theatrical dimension (theater ratio = 0.61) reflects manufacturers' use of nominal compliance (citing standards they violate) to preempt regulatory intervention while maintaining the profitable fragmentation. This is a textbook snare: high suppression (no viable exit), high extraction (asymmetric benefit to retailers), existence dependent on preventing the obvious solution (standardization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    standardization_adoption_threshold,
    'What market share of brands must adopt standardized sizing before the constraint collapses through network effects?',
    'Historical analysis of standardization success in other markets (EU EN 13402); modeling of consumer switching behavior when multiple standards coexist; empirical tracking of brand adoption rates in pilot programs',
    'If threshold < 30%: standardization achievable through coordinated retailer agreement. If threshold > 60%: network lock-in is too strong; only regulatory mandate can force transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standardization_adoption_threshold, empirical, 'Market share threshold for standardization network effects').

omega_variable(
    vanity_sizing_profit_dependency,
    'How much of retailer margin and consumer lifetime value depends on vanity sizing as a mechanism for psychological price discrimination?',
    'Financial analysis of profit per unit under standardized vs non-standardized sizing; A/B testing with standardized sizing cohorts; analysis of return rates and size-related abandonment under different labeling schemes',
    'If dependency > 15% of margin: retailers will fight standardization fiercely (snare confirmed). If dependency < 5%: standardization could be negotiated without significant margin loss (constraint is more coordination problem than extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vanity_sizing_profit_dependency, empirical, 'Financial dependency on vanity sizing for retailer profitability').

omega_variable(
    body_measurement_representation,
    'Do existing size standards (NIST PS 42, ASTM D5219, EU EN 13402) adequately represent the racial, ethnic, and age diversity of women''s bodies, or do they encode historical bias that would perpetuate exclusion under standardization?',
    'Comparative anthropometric analysis of sizing basis samples; examination of original standard-setting populations (who was measured?); testing of proposed standards against diverse body populations; analysis of fit data across demographic groups under different standardization schemes',
    'If standards are racially/ethnically biased: standardization could worsen fit for non-majority populations (snare extends to standard-enforcement). If standards are representative: standardization improves fit universally and enables comparison (rope/coordination confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(body_measurement_representation, empirical, 'Whether size standards encode demographic bias in body representation').

omega_variable(
    international_standard_harmonization,
    'Could US adoption of ISO 3635 or EU EN 13402 (international standards) create a regulatory coordination pathway that bypasses domestic industry resistance?',
    'Analysis of trade implications; modeling of competitive advantage/disadvantage under international standard adoption; study of similar transitions in other regulated industries (automotive, electronics); assessment of retaliatory pressure from large retailers',
    'If harmonization is feasible: creates external anchor for standard-setting (scaffold perspective strengthens). If retaliatory pressure is severe: international pathway is blocked and domestic coordination remains necessary (snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_standard_harmonization, conceptual, 'Whether international standard adoption can bypass domestic resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(womens_sizing_chaos, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsc_tr_t0, womens_sizing_chaos, theater_ratio, 0, 0.38).
narrative_ontology:measurement(wsc_tr_t25, womens_sizing_chaos, theater_ratio, 25, 0.52).
narrative_ontology:measurement(wsc_tr_t50, womens_sizing_chaos, theater_ratio, 50, 0.61).

% Extraction over time
narrative_ontology:measurement(wsc_be_t0, womens_sizing_chaos, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wsc_be_t25, womens_sizing_chaos, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(wsc_be_t50, womens_sizing_chaos, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(womens_sizing_chaos, information_standard).
narrative_ontology:affects_constraint(womens_sizing_chaos, plus_size_market_extraction).
narrative_ontology:affects_constraint(womens_sizing_chaos, fast_fashion_inventory_opacity).

% DUAL FORMULATION NOTE:
% The women's sizing chaos is downstream of profit-maximization strategies in apparel retail and upstream of secondary extraction mechanisms (plus-size markup, tailoring demand, return/exchange friction). Treated as a single constraint because the root mechanism (manufacturer fragmentation for profit) is structurally distinct from downstream manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
