% ============================================================================
% CONSTRAINT STORY: sanctions_regime_economic_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sanctions_regime_economic_extraction, []).

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
 *   constraint_id: sanctions_regime_economic_extraction
 *   human_readable: Sanctions Regime Economic Extraction
 *   domain: international_relations/economic_policy
 *
 * SUMMARY:
 *   Sanctions regimes represent a hybrid coordination-extraction constraint:
 *   the sanctioning coalition coordinates unified action to impose costs on a
 *   target state, ostensibly to coerce policy change. However, the extraction
 *   flow is systematically asymmetric — punishment falls disproportionately
 *   on powerless civilian populations rather than decision-making elites,
 *   while beneficiaries include both the sanctioning coalition and
 *   alternative supply networks that profit from market concentration. The
 *   constraint's extractiveness has increased over the measurement interval
 *   (0.35 → 0.58) as evasion networks have matured and sanctions have become
 *   de facto permanent rather than conditional on measurable policy change.
 *   Theater ratio has also risen (0.38 → 0.55) as sanctions administration
 *   has shifted from direct enforcement toward procedural legitimation
 *   (humanitarian exemptions, UN panels, compliance certifications) that mask
 *   the underlying extraction mechanism. The regime exhibits all six
 *   constraint types from different structural perspectives: pure extraction
 *   for trapped civilian populations (snare), mixed coordination-extraction
 *   for the sanctioning coalition and neighboring states (tangled_rope),
 *   coordination for alternative suppliers (rope), degraded enforcement
 *   ritual for bureaucratic actors (piton), and temporary measures for
 *   humanitarian coalitions (scaffold). From the analytical perspective,
 *   naturalizing sanctions as inherent to international relations (mountain)
 *   risks concealing the policy choices that perpetuate them.
 *
 * KEY AGENTS:
 *   - Sanctioning State Coalition: Primary beneficiary (institutional/arbitrage) — coordinates unified action to impose costs; captures political concessions, market share, and geopolitical leverage
 *   - Sanctioned State Civilian Population: Primary victim (powerless/trapped) — bears extraction through inflation, medicine scarcity, fuel rationing; no exit capacity or negotiating power
 *   - Sanctioned State Government: Secondary actor (organized/constrained) — experiences extraction and coordination asymmetry; some agency to negotiate relief but under duress
 *   - Third-Party Trading Partners: Powerful secondary victim (powerful/mobile) — face secondary sanctions and compliance cost; can relocate but with friction and reputational damage
 *   - Alternative Supply Networks: Beneficiary (institutional/arbitrage) — profit from market concentration and sanctions-driven price premiums; have perverse incentive to perpetuate regime
 *   - Neighboring States: Mixed actor (organized/constrained) — experience spillover costs and security guarantees; constrained by coalition pressure and geographic dependence
 *   - International Sanctions Administration: Institutional actor (institutional/arbitrage) — maintains enforcement bureaucracy with degraded function; sustains through procedural theater
 *   - Humanitarian Exemption Coalition: Organized actor (organized/constrained) — see sanctions as temporary with built-in sunset logic; attempt to carve out genuine exceptions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sanctions_regime_economic_extraction, 0.58).
domain_priors:suppression_score(sanctions_regime_economic_extraction, 0.65).
domain_priors:theater_ratio(sanctions_regime_economic_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sanctions_regime_economic_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(sanctions_regime_economic_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sanctions_regime_economic_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sanctions_regime_economic_extraction, tangled_rope).
narrative_ontology:human_readable(sanctions_regime_economic_extraction, "Sanctions Regime Economic Extraction").
narrative_ontology:topic_domain(sanctions_regime_economic_extraction, "international_relations/economic_policy").

domain_priors:requires_active_enforcement(sanctions_regime_economic_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sanctions_regime_economic_extraction, sanctioning_state_coalition).
narrative_ontology:constraint_beneficiary(sanctions_regime_economic_extraction, alternative_supply_networks).
narrative_ontology:constraint_beneficiary(sanctions_regime_economic_extraction, domestic_substitute_producers).
narrative_ontology:constraint_victim(sanctions_regime_economic_extraction, sanctioned_state_economy).
narrative_ontology:constraint_victim(sanctions_regime_economic_extraction, third_party_trading_partners).
narrative_ontology:constraint_victim(sanctions_regime_economic_extraction, sanctioned_state_civilian_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SANCTIONED STATE CIVILIAN POPULATION (SNARE) — No exit capacity. Trapped within borders of sanctioned state with restricted access to trade, credit, and technology. Bears full extraction through inflation, medicine scarcity, fuel rationing. Cannot negotiate, relocate, or opt out of the constraint. Maximum experienced extraction — powerless agents with zero degrees of freedom.
constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SANCTIONED STATE GOVERNMENT (TANGLED ROPE) — Constrained by coordinated external enforcement but retains some agency: can negotiate sanctions relief, build alternative supply networks, adjust domestic policy. Experiences both extraction (revenue loss, capital flight) and coordination cost (international negotiation required to exit). High suppression but partial agency distinguishes from pure snare.
constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SANCTIONING STATE COALITION (ROPE) — Primary beneficiary with high agency. Experiences sanctions as coordination mechanism: unified action enforces political demands with minimal internal cost to coalition members. Arbitrage exit option: can alter sanctions scope and timing. Net coordination function — coalition members solve collective action problem of coercing target state behavior.
constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THIRD-PARTY TRADING PARTNERS (TANGLED ROPE) — Powerful actors (multinational corporations, financial institutions, logistics networks) with mobile exit options can relocate trade relationships, but face secondary sanctions and compliance cost. Experience mixed extraction (forced compliance, limited market access) and coordination benefit (cleared to do business with sanctioning coalition). Constrained mobility distinguishes from pure rope.
constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE SUPPLY NETWORKS (ROPE) — Beneficiaries with high agency (shadow suppliers, informal trade channels, gray-market logistics). Arbitrage exit option: can shift between markets and regulatory regimes. Low experienced extraction — they benefit from market concentration in sanctioned goods and have competitive advantage. Genuine coordination function for parallel economic systems.
constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NEIGHBORING STATES (TANGLED ROPE) — Organized actors with constrained exits. Experience both extraction (spillover economic damage, refugee pressure, security risks) and coordination benefit (security guarantee from sanctioning coalition, trade preferences if aligned). Cannot fully exit due to geographic dependence and coalition pressure. Moderate extraction, genuine coordination content.
constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: INTERNATIONAL SANCTIONS ADMINISTRATION (PITON) — Institutional actors (UN panels, Treasury offices, export control agencies) maintain sanctions bureaucracy with theater_ratio near 0.55. Enforcement has degraded: evasion networks circumvent controls, compliance costs exceed deterrent effects, sanctions persist through institutional inertia beyond original policy goals. Sees own enforcement as degraded (piton characteristic) while justifying maintenance through procedural ritual.
constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: HUMANITARIAN EXEMPTION COALITION (SCAFFOLD) — Organized actors (NGOs, medical groups, humanitarian agencies) see sanctions as temporary coordinated measure with built-in sunset logic: humanitarian carve-outs, medical exemptions, and negotiation timelines are designed to ease toward relief. Low theater because exemptions require genuine material differentiation and documentation. Coordination function dominates extraction function from this perspective.
constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, some extraction is inherent to any enforcement system: coordinated action requires credible threat, threat requires asymmetric cost distribution, cost distribution requires targeting. This perspective risks naturalizing the contingent institutional arrangement (state-level collective punishment) as an immutable feature of international relations. Engine's false summit detector will flag this as naturalization of a policy choice.
constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sanctions_regime_economic_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sanctions_regime_economic_extraction, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sanctions_regime_economic_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sanctions_regime_economic_extraction, TR),
    TR >= 0.70.

:- end_tests(sanctions_regime_economic_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The baseline extraction reflects the asymmetric cost distribution — costs fall on powerless civilian populations with no exit capacity while benefits accrue to coalition members with arbitrage options. The rising trajectory (0.35 → 0.58 over 15 years) indicates that sanctions have become increasingly extractive as they have shifted from targeted measures with conditional sunset logic to de facto permanent regimes justified through ritual compliance and humanitarian carve-outs. Suppression (0.65): Moderate-high. Significant barriers to exit and alternatives include coordinated enforcement across sanctioning coalition, secondary sanctions on third parties, restrictions on capital flight, trade network disruption, and geographic/material dependence of target state on specific imported goods. But suppression is not total — shadow supply networks exist, humanitarian channels operate, and some third parties evade compliance. Theater ratio (0.55): Moderate. Sanctions administration has shifted toward procedural legitimation — UN panels, humanitarian exemptions, compliance certifications — that maintains the appearance of precision and conditionality while the underlying extraction mechanism operates with less specificity. The rising theater trajectory reflects bureaucratic ritual growth as direct enforcement has faced evasion pressure. The theater is genuine institutional performance but increasingly decoupled from measurable policy change.
 *
 * PERSPECTIVAL GAP:
 *   The gap between sanctioning coalition perspective and sanctioned civilian perspective is maximum: rope (low extraction, coordination, arbitrage exit) versus snare (high extraction, no coordination benefit, trapped exit). This gap is not an artifact of observational position but a structural fact about the constraint — the extraction mechanism is designed to be asymmetric, with costs concentrated on powerless populations and benefits concentrated on institutional beneficiaries. The gap cannot be closed by better information or negotiation frameworks because the asymmetry is intentional. The gap can only be closed by changing the constraint structure: either by genuinely enforcing targeted sanctions against elites (reducing civilian extraction) or by lifting sanctions entirely (removing extraction). The humanitarian exemption coalition's scaffold perspective attempts a third path: reducing suppression through carve-outs while maintaining coordination. But if exemptions are theater rather than genuine, this path also fails. The piton classification of sanctions bureaucracy reveals institutional capture: the enforcement apparatus has become invested in its own continuation rather than in measurable policy change, creating a structural lock-in where extraction persists beyond the original coordinating goal.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value is derived from: (1) beneficiary/victim status + power level + exit options. Beneficiaries with high exit capacity (sanctioning coalition, alternative networks) derive low d (≈0.05–0.15) producing negative or minimal chi. Victims with trapped or constrained exits derive high d (0.60–0.95) producing high chi. Mixed actors (third-party traders, neighboring states) derive mid-range d (0.55–0.65) producing moderate chi. The analytical observer (d ≈ 0.72 by canonical fallback) sees the constraint at high chi but acknowledges all perspectives as valid. The directed flow of extraction is clear: sanctions coalition → sanctioned state government → sanctioned state civilians (downward pressure) with intermediate beneficiaries (alternative networks) capturing rents along the way. This directionality is not hidden or ambiguous — it is the deliberate design of sanctions policy — which makes the mountain perspective (naturalizing it as law) particularly dangerous: it conceals a policy choice as an immutable feature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through perspectival multiplexing: no single type is 'correct' — the six types are simultaneous valid readings of the same structural phenomenon from different observational contexts. The mandatrophy emerges from the gap between coordinating-coalition perspective (rope) and trapped-civilian perspective (snare). The resolution is not to choose one but to recognize that the constraint is structurally tangled_rope — it genuinely coordinates coalition action to enforce behavioral change AND it genuinely extracts asymmetrically from powerless populations with no exit. The tension is not resolvable by choosing a 'true' type; it is the defining feature. The piton classification of the sanctions bureaucracy reveals that the coordinating mechanism has degraded: enforcement now relies on procedural theater (humanitarian exemptions, compliance certifications) rather than direct coercion. This degradation does not eliminate extraction — it redistributes it upward through bureaucratic cost and downward through civilian deprivation via reduced aid and supply fluctuation. The scaffold perspective (humanitarian coalitions seeing temporary measures) is aspirational — it represents the intended exit path — but is currently not materially instantiated because the duration omega (sanctions permanence vs conditional sunset) remains unresolved. If humanitarian exceptions functioned as genuine exceptions and sanctions had explicit sunset tied to measurable concessions, the scaffold would be structurally real. Currently it is hoped-for rather than actual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_exception_enforcement,
    'Do humanitarian carve-outs function as genuine exceptions or theater that legitimizes broader extraction?',
    'Empirical audit: what percentage of humanitarian goods successfully reach sanctioned populations? What delays occur between exception approval and delivery? Do exceptions reduce civilian cost measurably?',
    'If genuine: scaffold perspective confirmed; sanctions have real sunset logic. If theater: humanitarianism becomes cover story; extract/suppression ratios increase; reclassify from tangled_rope to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_exception_enforcement, empirical, 'Whether humanitarian carve-outs function as genuine exceptions or theater').

omega_variable(
    third_party_secondary_sanctions_enforcement,
    'How effectively do secondary sanctions enforce compliance on third-party actors who lack direct dependence on sanctioning coalition?',
    'Track trade volume, pricing, and substitution rates for third parties under secondary sanctions. Identify threshold: sanctions duration vs compliance erosion.',
    'If effective: tangled_rope classification of third parties stands. If ineffective: third parties experience lower extraction; reclassify toward rope; sanctions regime''s true enforceability is overestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_secondary_sanctions_enforcement, empirical, 'Effectiveness of secondary sanctions on third-party compliance').

omega_variable(
    targeted_vs_collective_punishment_boundary,
    'What fraction of measured extraction targets regime elites vs civilian population? Is there enforceable differentiation, or does extraction flow inevitably to powerless populations?',
    'Economic impact analysis: GDP loss, inflation, medicine/food access by income quintile. Compare elite asset availability (black-market access, capital flight) vs civilian deprivation.',
    'If targeting works: extraction mechanism may be precision-aligned with political objective; snare classification of civilians may degrade toward tangled_rope with constrained exit. If targeting fails: extraction flows to powerless; snare is structural inevitability; classify as pure extortionate regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeted_vs_collective_punishment_boundary, empirical, 'Distribution of extraction burden between regime elites and civilian population').

omega_variable(
    sanctions_duration_time_horizon_mismatch,
    'Do sanctions regimes have implicit sunset clauses (tied to specific behavioral changes) or de facto permanence (conditioned on immeasurable political concessions)?',
    'Historical analysis of sanctions duration and termination: what triggers relief? How many sanctions regimes that lasted >10 years were eventually lifted? What were the stated exit criteria?',
    'If genuine sunset logic: scaffold classification is justified. If de facto permanence: sanctions become pure extraction mechanism (snare); reclassify suppression upward; piton theater ratio increases as enforcement becomes ritualized without exit path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctions_duration_time_horizon_mismatch, empirical, 'Whether sanctions regimes have genuine sunset clauses or de facto permanence').

omega_variable(
    alternative_supply_network_symbiosis,
    'Do shadow supply networks and gray-market logistics depend on sanctions regime for competitive advantage, creating symbiotic relationship that prevents sanctions termination?',
    'Economic analysis: would shadow networks'' profitability collapse if sanctions were lifted? Do they have incentive to perpetuate sanctions? Track lobbying, political influence by alternative supply actors.',
    'If symbiotic: beneficiaries have perverse incentive to maintain regime; extraction mechanism is structurally self-perpetuating; snare and tangled_rope classifications become more entrenched; duration omega resolves toward permanent regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_network_symbiosis, empirical, 'Whether shadow networks depend on sanctions for competitive advantage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sanctions_regime_economic_extraction, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sanc_tr_t0, sanctions_regime_economic_extraction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sanc_tr_t5, sanctions_regime_economic_extraction, theater_ratio, 5, 0.48).
narrative_ontology:measurement(sanc_tr_t10, sanctions_regime_economic_extraction, theater_ratio, 10, 0.55).
narrative_ontology:measurement(sanc_tr_t15, sanctions_regime_economic_extraction, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(sanc_be_t0, sanctions_regime_economic_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sanc_be_t5, sanctions_regime_economic_extraction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sanc_be_t10, sanctions_regime_economic_extraction, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sanc_be_t15, sanctions_regime_economic_extraction, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sanctions_regime_economic_extraction, enforcement_mechanism).
narrative_ontology:affects_constraint(sanctions_regime_economic_extraction, geopolitical_zero_sum_competition).
narrative_ontology:affects_constraint(sanctions_regime_economic_extraction, secondary_sanctions_compliance).
narrative_ontology:affects_constraint(sanctions_regime_economic_extraction, shadow_supply_network_profitability).

% DUAL FORMULATION NOTE:
% Sanctions regime decomposition: the primary regime (this story) models coordination + extraction at state level; it affects three downstream constraints representing the detailed mechanisms: geopolitical positioning, secondary enforcement, and shadow profit channels. Each has its own ε value reflecting different structural logics. See network edges for dependency graph.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sanctions_regime_economic_extraction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
