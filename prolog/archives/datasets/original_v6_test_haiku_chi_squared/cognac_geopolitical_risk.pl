% ============================================================================
% CONSTRAINT STORY: cognac_geopolitical_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognac_geopolitical_risk, []).

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
 *   constraint_id: cognac_geopolitical_risk
 *   human_readable: Geopolitical Risk to Cognac Sales
 *   domain: economic/trade/commodities
 *
 * SUMMARY:
 *   Cognac production represents one of France's iconic luxury exports, with
 *   deep regional historical roots in Aquitaine and Charente. Since 2018,
 *   producers have faced escalating geopolitical risk: US tariffs on French
 *   wines (2018-2021), China trade tensions, UK Brexit disruptions, and
 *   ongoing tensions between NATO/EU and Russia have created unpredictable
 *   market access restrictions. This constraint exhibits the snare structure
 *   where producers are trapped by immobile capital (land, distillery
 *   equipment, brand geographic specificity) while facing extraction through
 *   tariffs, sanctions, and retaliatory trade measures imposed by
 *   geopolitical actors. The constraint is NOT a purely natural economic
 *   phenomenon but a manufactured extraction mechanism using trade policy as
 *   enforcement. However, significant ongoing coordination efforts to
 *   diversify markets and build trade alliance cohesion create overlapping
 *   scaffold and tangled_rope perspectives. Small family distillers face
 *   maximal extraction with no exit; large multinational producers have more
 *   mobility through portfolio diversification; geopolitical states
 *   experience the constraint as a functional coordination tool with minimal
 *   cost; organized European coalitions see both coordination benefits and
 *   extraction costs.
 *
 * KEY AGENTS:
 *   - Cognac Producers (Large): Moderate power through multinational portfolio diversification; constrained within Cognac category but mobile across regions and product categories
 *   - Small/Family Distillers: Powerless; geographically and capital-locked; bear full extraction; typical size 20-100 employees
 *   - Regional Economy (Charente/Aquitaine): Moderate power; organized through chambers of commerce; constrained by geographic specialization; employment/tax/cultural identity at stake
 *   - French State: Institutional actor; both beneficiary (tariff revenue, retaliatory leverage) and protector (subsidies, trade negotiations); arbitrage position
 *   - European Union: Institutional actor; coordinates retaliatory tariffs and trade policy; exercises arbitrage options
 *   - Sanctioning States (US, China, Russia): Institutional actors; use Cognac tariffs as symbolic/practical leverage in broader geopolitical strategy; beneficiaries through tariff extraction
 *   - Diplomatic Corps: Institutional actor; performs trade negotiations with low functional effect on underlying geopolitical tensions; piton perspective
 *   - Alternative Markets (Asia, Africa, Middle East): Emerging beneficiaries of market diversification; enable scaffold/mobile perspectives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognac_geopolitical_risk, 0.52).
domain_priors:suppression_score(cognac_geopolitical_risk, 0.65).
domain_priors:theater_ratio(cognac_geopolitical_risk, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognac_geopolitical_risk, extractiveness, 0.52).
narrative_ontology:constraint_metric(cognac_geopolitical_risk, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cognac_geopolitical_risk, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognac_geopolitical_risk, snare).
narrative_ontology:human_readable(cognac_geopolitical_risk, "Geopolitical Risk to Cognac Sales").
narrative_ontology:topic_domain(cognac_geopolitical_risk, "economic/trade/commodities").

% --- Structural relationships ---
narrative_ontology:constraint_victim(cognac_geopolitical_risk, cognac_producers).
narrative_ontology:constraint_victim(cognac_geopolitical_risk, french_regional_economy).
narrative_ontology:constraint_victim(cognac_geopolitical_risk, small_distillers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COGNAC PRODUCERS (SNARE) — Face extraction through tariffs, sanctions, and market access restrictions imposed by geopolitical actors beyond their control. Cannot exit global markets without abandoning centuries of supply chains and brand equity. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.81.
constraint_indexing:constraint_classification(cognac_geopolitical_risk, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GEOPOLITICAL ACTOR / SANCTIONING STATE (ROPE) — Uses trade restrictions as coordination mechanism to signal displeasure and enforce geopolitical objectives. Experiences constraint as effective coordination tool with minimal internal cost (tariff collection, symbolic enforcement). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(cognac_geopolitical_risk, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL ECONOMY (SNARE) — Cognac production represents significant employment, tax revenue, and cultural heritage for French regions. Constrained by geographic specialization and cannot relocate production without destroying regional identity and brand. d≈0.88, f(d)≈1.32, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(cognac_geopolitical_risk, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EUROPEAN TRADE ALLIANCE (TANGLED ROPE) — Organized response to geopolitical risk includes retaliatory tariffs, diversification agreements, and trade bloc coordination. Benefits from collective bargaining power (coordination) but faces extraction through volatility and resource costs of maintaining alliance discipline. d≈0.52, f(d)≈0.65, σ=1.1 → χ≈0.37.
constraint_indexing:constraint_classification(cognac_geopolitical_risk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: SMALL DISTILLER (SNARE) — Family-owned producers with no capital reserves to weather market disruptions, no political voice, and no ability to relocate. Completely extracted by geopolitical volatility with no exit option. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.60.
constraint_indexing:constraint_classification(cognac_geopolitical_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: DIVERSIFICATION INITIATIVE (SCAFFOLD) — Coordinated effort to reduce geopolitical dependency through new market development, product diversification (cognac-based goods, hospitality tourism), and supply chain resilience building. Has sunset clause: once new markets mature and diversification reaches critical mass, geopolitical risk exposure decreases structurally. d≈0.45, f(d)≈0.42, σ=1.2 → χ≈0.24. Theater ratio for this perspective is low (0.15) because diversification activities are functionally productive, not performative.
constraint_indexing:constraint_classification(cognac_geopolitical_risk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: DIPLOMATIC THEATER (PITON) — Trade negotiations, tariff discussions, and retaliatory threat cycles are largely ritualistic. Real decisions are made by defense and geopolitical strategists; trade negotiations are performative displays of state strength. theater_ratio=0.62 reflects substantial but not dominant theatrical content. Maintains institutional inertia through repeated cycles of crisis and negotiation despite low functional effect on structural geopolitical tensions.
constraint_indexing:constraint_classification(cognac_geopolitical_risk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognac_geopolitical_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognac_geopolitical_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognac_geopolitical_risk, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cognac_geopolitical_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognac_geopolitical_risk, TR),
    TR >= 0.70.

:- end_tests(cognac_geopolitical_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. Initial value (0.28) reflects pre-2018 baseline when Cognac faced normal commodity price volatility but not geopolitical targeting. By year 10, extractiveness reaches 0.52 reflecting cumulative impact of multiple tariff cycles, market access restrictions, and uncertainty premium on supply chain costs. Suppression (0.65): High. The mechanism is coercive (tariffs, sanctions) with minimal legitimacy from the producers' perspective. Alternative market access (arbitrage/mobile option) exists in principle but faces barriers: brand preference for traditional markets, logistical infrastructure built around established routes, price-sensitivity differences. Producers cannot easily 'shop' for better geopolitical contexts. Theater ratio (0.38): Moderate-low. Trade negotiations and diplomatic cycles contain performance elements (public posturing, reciprocal threat displays) but underlying tariff policies are functionally real and impose material extraction. The theatrical content is lower than true piton constraints because extraction mechanisms work; trade policy is not purely symbolic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows dramatic perspectival divergence. The geopolitical actor (state/coalition) sees Rope — an effective coordination mechanism for signaling and enforcement. The small distiller sees Snare — pure extraction with no agency. The regional economy sees Snare — extraction of employment and tax revenue. The large producer sees Tangled Rope / Scaffold — mixed coordination and extraction, with some mobility options. The diversification initiative sees Scaffold — a temporary problem solvable through market adaptation. The diplomatic corps sees Piton — ritualistic negotiation maintaining institutional forms despite low functional effect. The European trade alliance sees Tangled Rope — benefits from collective coordination but faces extraction through compliance costs and volatility. These gaps are not measurement artifacts but genuine structural differences: producers experience powerlessness; states experience agency; alliances experience mixed constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Cognac Producers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit; bear costs of tariffs, market access loss, and uncertainty. Large producers have slightly lower d (≈0.82) through portfolio diversification, but cannot exit Cognac category without destroying brand asset. French Regional Economy: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction. Geographically locked to region; cannot relocate employment; constrained by heritage/identity factors. Geopolitical Actor: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can unilaterally impose restrictions; faces no counter-extraction; experiences tariff revenue benefits. European Union: Beneficiary + arbitrage (mixed victim in secondary way) → d≈0.12, f(d)≈-0.08. Slight beneficiary through coordination role, but also faces extraction through retaliation cycles. Trade Alliance: Victim + mobile (organized) → d≈0.52, f(d)≈0.65. Intermediate. Organized response reduces effective extraction but does not eliminate it; mobility through collective action is partial. Diversification Initiative: Agent + mobile → d≈0.45, f(d)≈0.42. Low effective extraction due to agency and exit pathway (new markets); represents adaptive response converting snare to scaffold.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Extractiveness ε=0.52 > 0.46 requires mandatrophy closure. The constraint is classified as SNARE from primary victims' perspective (d≈0.92, f(d)≈1.38, χ≈0.81), satisfying snare thresholds (ε≥0.46, suppression≥0.60, χ≥0.66). However, the alternative scaffold and tangled_rope perspectives are not misclassifications but legitimate observational positions. The mandatrophy is resolved by recognizing that (1) the primary extraction mechanism (geopolitical tariffs/sanctions) is functionally real, not theater; (2) the constraint exists regardless of whether victims resist or accept it; (3) beneficiaries (sanctioning states) experience genuine coordination gain with minimal cost; (4) escape routes exist (diversification, market mobility, state subsidies) but are constrained/costly. This prevents the mischaracterization of the constraint as pure coordination (false Rope) while acknowledging that organized responses and market adaptation can convert portions of the snare into temporary scaffold structures. The classification remains SNARE as claimed_type because the baseline structural relationship is extraction, not coordination; the scaffold and tangled_rope are overlays that reduce but do not eliminate the snare structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctions_permanence_threshold,
    'Are geopolitical restrictions on Cognac sales temporary tariff cycles or permanent structural shifts in global trade patterns?',
    'Historical analysis of trade restrictions: duration, reversal rates, and pattern of escalation/de-escalation across commodity categories over 20-year horizon',
    'If temporary: constraint is primarily Scaffold with realistic sunset. If permanent: constraint is primarily Snare with no exit mechanism. Changes classification distribution across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_permanence_threshold, empirical, 'Whether geopolitical restrictions are temporary or structural').

omega_variable(
    alternative_market_viability,
    'Can Cognac successfully develop sufficient market depth in non-sanctioned regions (Asia, Africa, Middle East, Latin America) to offset loss of traditional markets (US, EU)?',
    'Market analysis of demand elasticity in alternative regions; brand reputation maintenance in geopolitically neutral markets; price-point sustainability without traditional premium markets',
    'If viable: diversification scaffold is real and sunset is achievable. If not viable: producers face permanent revenue contraction and snare classifications persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_market_viability, empirical, 'Viability of alternative market development').

omega_variable(
    state_subsidy_effectiveness,
    'Can French/EU state subsidies and trade protections effectively shield Cognac producers from geopolitical extraction, or does state intervention create moral hazard and productivity loss?',
    'Comparative analysis: subsidy-dependent producers vs market-adapted producers; correlation between subsidy level and innovation/efficiency metrics; long-term market share retention post-subsidy removal',
    'If effective: constraint is partially converted to Rope (state coordination function dominates). If ineffective: state intervention masks fundamental snare structure and delays adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_subsidy_effectiveness, empirical, 'Effectiveness of state subsidies in mitigating geopolitical risk').

omega_variable(
    brand_equity_decay_rate,
    'How quickly does Cognac brand equity depreciate under sustained market access disruption? Can hundred-year brand reputation weather multi-year sanctions?',
    'Brand valuation modeling; consumer preference tracking in disruption periods; comparison with other luxury goods (wine, champagne) under sanctions; market recovery rates post-sanctions removal',
    'If decay is slow: constraints are temporary, scaffold reasoning valid. If decay is fast: multi-year disruptions cause permanent value destruction, converting constraint to pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brand_equity_decay_rate, empirical, 'Rate of brand equity depreciation under market access disruption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognac_geopolitical_risk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cognac_tr_t0, cognac_geopolitical_risk, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cognac_tr_t5, cognac_geopolitical_risk, theater_ratio, 5, 0.32).
narrative_ontology:measurement(cognac_tr_t10, cognac_geopolitical_risk, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(cognac_be_t0, cognac_geopolitical_risk, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cognac_be_t5, cognac_geopolitical_risk, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(cognac_be_t10, cognac_geopolitical_risk, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognac_geopolitical_risk, enforcement_mechanism).
narrative_ontology:affects_constraint(cognac_geopolitical_risk, luxury_goods_supply_chain_resilience).
narrative_ontology:affects_constraint(cognac_geopolitical_risk, french_agricultural_export_dependency).
narrative_ontology:affects_constraint(cognac_geopolitical_risk, trade_retaliation_cycles).

% DUAL FORMULATION NOTE:
% This constraint is a specific instantiation of broader geopolitical extraction mechanisms in trade policy. It affects and is affected by constraints on wine, agricultural exports, and luxury goods supply chains generally. The constraint family includes: trade_retaliation_cycles (ε≈0.35, Tangled Rope) as the upstream macro-structural constraint, cognac_geopolitical_risk (ε=0.52, Snare) as the sector-specific manifestation, and French_agricultural_export_dependency (ε≈0.48, Snare) as a parallel vulnerability. The ε values differ because sectoral specificity (Cognac) faces higher extraction than broader category effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cognac_geopolitical_risk, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
