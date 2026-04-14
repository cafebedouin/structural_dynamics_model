% ============================================================================
% CONSTRAINT STORY: secondary_sanctions_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secondary_sanctions_regime, []).

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
 *   constraint_id: secondary_sanctions_regime
 *   human_readable: Secondary Sanctions Regime
 *   domain: geopolitical/economic_coercion
 *
 * SUMMARY:
 *   Secondary sanctions regimes impose coercive restrictions on third parties
 *   who engage with designated targets, extending the sanctioning state's
 *   enforcement power globally. The constraint exhibits core properties of
 *   tangled rope: genuine coordination function (unified allied enforcement
 *   prevents targeted states from exploiting regulatory gaps) combined with
 *   asymmetric extraction (compliance costs concentrated on third-party
 *   actors and allied states with economic exposure). The regime has
 *   escalated over the measurement interval (extractiveness rising from 0.38
 *   to 0.61) as enforcement mechanisms have hardened and financial
 *   architecture integration has deepened. Theater ratio (rising from 0.35 to
 *   0.58) reflects increasing performative compliance: financial institutions
 *   maintain facade of transaction facilitation while processing only
 *   sanctioning-regime-approved flows. The constraint operates at global
 *   scope with uneven spatial distribution — enforcement is near-total in
 *   allied jurisdictions, partial in non-aligned states, and circumvented in
 *   specifically designated zones. Multiple institutional actors (sanctioning
 *   state, allied powers, financial institutions, targeted state) experience
 *   structurally distinct extraction patterns, making secondary sanctions a
 *   diagnostic exemplar for inter-institutional constraint analysis.
 *
 * KEY AGENTS:
 *   - Sanctioning State (institutional/arbitrage): Designer and primary beneficiary — captures coercive leverage at minimal institutional cost
 *   - Targeted State (powerless/trapped): Primary victim — faces total financial isolation with no viable exit path
 *   - Third-Party Economic Actors (moderate/constrained): Secondary victims — face compliance costs, legal liability, market restrictions; can reduce scale but not exit entirely
 *   - Allied States with Economic Exposure (institutional/constrained): Secondary victims — benefit from unified enforcement but absorb trade loss and market access penalties
 *   - Financial Institutions (institutional/constrained): Enforcement infrastructure — maintain coordination appearance while performing theater; trapped by political pressure and regulatory mandate
 *   - Non-Aligned States Coalition (organized/constrained): Organized opposition — build alternative financial channels; face pressure but maintain degrees of freedom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secondary_sanctions_regime, 0.58).
domain_priors:suppression_score(secondary_sanctions_regime, 0.68).
domain_priors:theater_ratio(secondary_sanctions_regime, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secondary_sanctions_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(secondary_sanctions_regime, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(secondary_sanctions_regime, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secondary_sanctions_regime, tangled_rope).
narrative_ontology:human_readable(secondary_sanctions_regime, "Secondary Sanctions Regime").
narrative_ontology:topic_domain(secondary_sanctions_regime, "geopolitical/economic_coercion").

domain_priors:requires_active_enforcement(secondary_sanctions_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secondary_sanctions_regime, sanctioning_state).
narrative_ontology:constraint_beneficiary(secondary_sanctions_regime, allied_financial_institutions).
narrative_ontology:constraint_victim(secondary_sanctions_regime, targeted_state).
narrative_ontology:constraint_victim(secondary_sanctions_regime, third_party_economic_actors).
narrative_ontology:constraint_victim(secondary_sanctions_regime, global_financial_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED STATE (SNARE) — A sanctioned state faces no exit path from secondary sanctions without abandoning core policy objectives or sovereignty. The regime operates globally — any state wishing to trade with the sanctioning power must comply. Maximum suppression (0.68) through financial isolation, trade restrictions, and asset freezes. No coordination benefit visible from this position; pure extraction via coercive denial. Trapped agents experience maximum effective extraction.
constraint_indexing:constraint_classification(secondary_sanctions_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THIRD-PARTY ECONOMIC ACTORS (TANGLED ROPE) — Multinational corporations, financial institutions, and trading partners face constrained exit. The regime coordinates international enforcement (genuine coordination function: unified sanctions prevent evasion) while extracting compliance costs (legal liability, transaction restrictions, market access reduction). Exit costs are high (market loss, regulatory penalties) but not absolute — some actors maintain targeted state relationships at reduced scale or find arbitrage routes. Mixed extraction and coordination characterizes this position.
constraint_indexing:constraint_classification(secondary_sanctions_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SANCTIONING STATE AND ALLIED POWERS (ROPE) — The regime's designer experiences it as a coordination mechanism: unified allied enforcement prevents targeted states from exploiting gaps between jurisdictions. Exit cost is negligible (the sanctioning state can adjust terms unilaterally). Net beneficiary — extraction flows toward this agent. The regime delivers coercive coordination at low institutional cost. From this perspective, the constraint solves a collective action problem: how to maintain coercive pressure without any single ally absorbing the full economic penalty.
constraint_indexing:constraint_classification(secondary_sanctions_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIED STATES WITH ECONOMIC EXPOSURE (TANGLED ROPE) — Allied states that depend on trade with the targeted state face genuine tension. They benefit from the coordination logic (unified front prevents targeted states from exploiting divisions) but absorb significant extraction: lost trade revenue, market access penalties, domestic industry disruption. Exit costs are moderate (diplomatic fallout, sanctioning state retaliation) but tolerable — some allies reduce compliance or broker exemptions. This institutional actor experiences genuine mixed coordination-extraction.
constraint_indexing:constraint_classification(secondary_sanctions_regime, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL TRADE AND FINANCIAL INSTITUTIONS (PITON) — The IMF, World Bank, and SWIFT system maintain formal neutrality but function as enforcement infrastructure for secondary sanctions. Their original coordination role (facilitate legitimate cross-border trade and finance) has atrophied — they now perform theater: processing compliant transactions while maintaining the appearance of impartial intermediaries. Theater ratio (0.52) reflects that much institutional activity around sanctions compliance is procedural rather than functionally necessary. The institutions are trapped by institutional inertia and political pressure to appear compliant without breaking.
constraint_indexing:constraint_classification(secondary_sanctions_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-ALIGNED STATES COALITION (TANGLED ROPE) — States outside the sanctioning regime (Global South, BRICS, etc.) face organized collective pressure. They benefit from coordination within the non-aligned movement (united markets, alternative financial channels via parallel payment systems) but face extraction through market access reduction, financial de-risking, and pressure to choose allegiance. Exit costs are moderate — some states maintain targeted state relations at political and economic cost. This organized agent experiences coordinated extraction through the regime's secondary pressure mechanisms.
constraint_indexing:constraint_classification(secondary_sanctions_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SCAFFOLD) — From a generational/analytical perspective, secondary sanctions regimes contain structural sunset mechanisms. As alternative financial infrastructure matures (digital currencies, parallel payment systems, non-SWIFT channels), the regime's enforcement power decays. The technology shift from centralized financial choke points (SWIFT, correspondent banking) to decentralized alternatives reduces the regime's functional capacity. The regime's extraction mechanism (financial isolation) becomes obsolete as exit routes proliferate. This perspective sees secondary sanctions as temporary institutional forms with declining extractive power over generational timescales.
constraint_indexing:constraint_classification(secondary_sanctions_regime, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secondary_sanctions_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(secondary_sanctions_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(secondary_sanctions_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(secondary_sanctions_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(secondary_sanctions_regime, TR),
    TR >= 0.70.

:- end_tests(secondary_sanctions_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The regime extracts through financial isolation mechanisms but retains genuine coordination function (preventing targeted states from exploiting regulatory arbitrage). The value (0.58) reflects that extraction is real but not total — partial substitution routes exist (barter, non-SWIFT channels, cryptocurrency). The rising trajectory (0.38 → 0.61) shows the regime intensifying over time as enforcement infrastructure consolidates. Suppression (0.68): High. Multiple barriers prevent exit: legal liability for violations, market access denial, asset freezes, and information asymmetry (compliance verification requires transparency the targeted state resists). The suppression is structural (external enforcement) rather than internalized. Theater ratio (0.52): Moderate. Financial institutions perform visible compliance (regulatory filings, transaction blocking, audit trails) while actual enforcement relies on centralized choke points (correspondent banking, SWIFT infrastructure). The theater is not total — some enforcement is real (actual transaction blocking) — but much is procedural display. The rising trajectory (0.35 → 0.58) reflects intensifying performative compliance as political pressure increases.
 *
 * PERSPECTIVAL GAP:
 *   The sanctioning state sees coordination (rope) — unified enforcement prevents evasion. The targeted state sees pure extraction (snare) — no exit path, total suppression. Allied states see mixed extraction-coordination (tangled rope) — unified front benefits them politically but costs them economically. Financial institutions see degradation (piton) — their coordination role (facilitate legitimate cross-border finance) has atrophied into theater (process only regime-approved flows). The non-aligned coalition sees organized extraction (tangled rope) — alternative financial channels provide escape routes but at political and efficiency cost. The analytical observer sees institutional sunset (scaffold) — as financial decentralization matures, the regime's enforcement capacity decays. The perspectival gap reveals that 'secondary sanctions' is not a single constraint but a presheaf: multiple structural realities depending on the observer's position. The tension between perspectives is not perspectival error but legitimate structural difference.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is computed from base extractiveness (0.58), their structural position relative to the regime, and their exit capacity. The sanctioning state (beneficiary + arbitrage exit) experiences negative chi — the regime subsidizes their coercive power. Allied powers (mixed beneficiary/victim + constrained exit) experience low positive chi — coordination benefits are real but extraction costs are moderate. Third-party actors (victims + constrained exit) and the targeted state (victim + trapped exit) experience high chi — extraction flows from them. The targeted state's d-value approaches 1.0 (full target) because it is the regime's primary victim with no exit options. The analytical observer at generational/global scope derives d ≈ 0.72, producing moderate-high chi that flags the regime as temporally limited (scaffold perspective) rather than permanent.
 *
 * MANDATROPHY ANALYSIS:
 *   Secondary sanctions resolve the mandatrophy by demonstrating how a constraint can be simultaneously coordinative (from the sanctioning state's view: prevent regulatory arbitrage) and purely extractive (from the targeted state's view: total financial isolation). The regime is NOT a mistake classification — both perspectives are correct from their respective structural positions. What appears as pure extraction to a powerless agent (snare) appears as necessary coordination to an institutional beneficiary (rope). The mandatrophy dissolves when we recognize that the classification is observer-dependent, not constraint-dependent. The regime's claimed type (tangled_rope) averages across these perspectives: it has genuine coordination function (ε = 0.58 reflects coordination cost baseline) but uses that coordination to extract asymmetrically (suppression = 0.68 reflects that extraction is enforced, not voluntary). The analytical/generational perspective (scaffold) adds temporal resolution: the regime's extractive power is degrading as alternative financial infrastructure matures, meaning the current extraction is temporary. This doesn't make the current snare less real for the targeted state — it means the snare is institutionally unstable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    financial_decentralization_timeline,
    'How quickly will decentralized financial infrastructure (CBDCs, blockchain settlement, parallel payment networks) reduce the sanctioning regime''s enforcement capacity?',
    'Tracking adoption rates of non-SWIFT settlement mechanisms, CBDC rollout timelines, and sanctions evasion success rates via alternative channels',
    'Fast decentralization (5-10 years): scaffold classification confirmed, regime becomes piton. Slow decentralization (30+ years): regime remains snare/tangled_rope for full interval. This determines mandatrophy urgency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_decentralization_timeline, empirical, 'Timeline for financial decentralization reducing sanctions enforcement').

omega_variable(
    coalition_stability_and_fragmentation,
    'Will the sanctioning coalition maintain unified enforcement, or will economic costs and geopolitical realignment cause fragmentation?',
    'Monitoring allied state compliance trajectories, exemption requests, sanctions evasion via allies, and bilateral trade patterns with targeted states',
    'High fragmentation: regime degrades to piton (performative theater). High stability: regime remains tangled_rope/snare. Determines whether this is a stable extractive constraint or a degrading one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_stability_and_fragmentation, empirical, 'Sanctioning coalition cohesion and fragmentation trajectory').

omega_variable(
    extraction_versus_containment_intent,
    'Is the regime primarily extractive (punitive/coercive wealth transfer) or coordinative (containment/leverage for behavioral change)?',
    'Historical analysis of sanction removal timelines, target state behavior modification success rates, sanctions versus negotiated settlements',
    'If primarily extractive: snare/piton classifications dominate. If primarily coordinative: rope/scaffold classifications dominate. This is partially a preference question (policymakers'' stated intent) but empirically resolvable via outcome tracking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_versus_containment_intent, preference, 'Whether regime functions as extraction or containment mechanism').

omega_variable(
    targeted_state_regime_change_feasibility,
    'Can the targeted state modify policies sufficiently to trigger sanctions relief, or are the regime''s political demands structurally impossible to meet?',
    'Tracking policy modification offers from targeted states and sanctioning regime responses; assessing whether sanctions removal is genuinely available or performative',
    'If exit is available: tangled_rope confirmation. If exit is blocked: snare confirmation. Determines whether the suppression is structural (external barriers) or total (no exit path exists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeted_state_regime_change_feasibility, empirical, 'Whether targeted state policy modification can trigger sanctions relief').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secondary_sanctions_regime, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sanc_tr_t0, secondary_sanctions_regime, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sanc_tr_t5, secondary_sanctions_regime, theater_ratio, 5, 0.46).
narrative_ontology:measurement(sanc_tr_t10, secondary_sanctions_regime, theater_ratio, 10, 0.52).
narrative_ontology:measurement(sanc_tr_t15, secondary_sanctions_regime, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(sanc_be_t0, secondary_sanctions_regime, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sanc_be_t5, secondary_sanctions_regime, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sanc_be_t10, secondary_sanctions_regime, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sanc_be_t15, secondary_sanctions_regime, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secondary_sanctions_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(secondary_sanctions_regime, currency_settlement_system_lock_in).
narrative_ontology:affects_constraint(secondary_sanctions_regime, allied_state_trade_dependency).
narrative_ontology:affects_constraint(secondary_sanctions_regime, financial_institution_regulatory_capture).

% DUAL FORMULATION NOTE:
% Secondary sanctions represent the enforcement layer of a broader geopolitical constraint family. Upstream constraints (currency system dominance, allied state coalition formation) enable secondary sanctions; downstream constraints (non-aligned financial architecture, cryptocurrency adoption) circumvent or degrade secondary sanctions. This story focuses on the regime's structural properties; decomposition into enforcement mechanisms vs. policy objectives would require separate stories with different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secondary_sanctions_regime, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
