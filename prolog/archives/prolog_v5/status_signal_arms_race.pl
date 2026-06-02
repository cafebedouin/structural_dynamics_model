% ============================================================================
% CONSTRAINT STORY: status_signal_arms_race
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_status_signal_arms_race, []).

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
 *   constraint_id: status_signal_arms_race
 *   human_readable: Status Signal Arms Race
 *   domain: social/economic/behavioral
 *
 * SUMMARY:
 *   The status signal arms race is a dynamic constraint in which agents
 *   engage in escalating consumption and visible display to maintain relative
 *   status position within hierarchical social structures. This constraint
 *   exhibits genuine coordination function (status markers reduce information
 *   asymmetry and enable efficient matching) simultaneously with extraction
 *   dynamics (arms race escalation extracts resources without corresponding
 *   coordination benefit). The constraint operates across all scales:
 *   interpersonal status maintenance within small groups, organizational
 *   prestige hierarchies, national consumption patterns, and global luxury
 *   markets. The theater ratio (0.68) reflects that much status signaling is
 *   performative — the signal's function is to be visible rather than to
 *   convey reliable information. Over the 40-year measurement interval, both
 *   extractiveness and theater have increased, indicating degradation of the
 *   coordination function relative to the arms race mechanism. Luxury goods
 *   producers, attention merchants, and status-signal intermediaries benefit
 *   from the constraint through monopoly pricing and market segmentation.
 *   Status seekers and economically constrained populations bear the costs
 *   through competitive consumption pressure and opportunity costs.
 *   Alternative status systems (minimalism, sustainability metrics, local
 *   reputation) are emerging as counterbalancing forces with sunset logic —
 *   they represent organizational scaffolds that could reduce dependence on
 *   consumption-based signaling if they scale.
 *
 * KEY AGENTS:
 *   - Status Seekers: Primary victims (powerless/trapped) — competitive consumption requirements with identity fusion, facing extraction through escalating display costs with no exit option
 *   - Status-Conscious Communities: Secondary victims (moderate/constrained) — face social pressure and economic costs; benefit from coordination function but bear arms race costs
 *   - Luxury Goods Producers: Primary beneficiaries (institutional/arbitrage) — capture monopoly rents from status-conscious consumption; experience constraint as enabling market segmentation
 *   - Attention Merchants: Secondary beneficiaries (institutional/arbitrage) — extract value from status signaling demand through advertising, influencer networks, and attention monopolies
 *   - Anti-Consumption Coalition: Organized counterforce (organized/constrained) — minimalism, sustainability movements, intentional communities building alternative status metrics and signaling systems
 *   - Heritage Status System: Institutional actor (institutional/arbitrage) — traditional status markers (titles, membership, old money aesthetics) persist through inertia despite reduced functional signaling
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent arms race as immutable feature of human hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(status_signal_arms_race, 0.58).
domain_priors:suppression_score(status_signal_arms_race, 0.65).
domain_priors:theater_ratio(status_signal_arms_race, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(status_signal_arms_race, extractiveness, 0.58).
narrative_ontology:constraint_metric(status_signal_arms_race, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(status_signal_arms_race, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(status_signal_arms_race, tangled_rope).
narrative_ontology:human_readable(status_signal_arms_race, "Status Signal Arms Race").
narrative_ontology:topic_domain(status_signal_arms_race, "social/economic/behavioral").

domain_priors:requires_active_enforcement(status_signal_arms_race).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(status_signal_arms_race, luxury_goods_producers).
narrative_ontology:constraint_beneficiary(status_signal_arms_race, status_signal_intermediaries).
narrative_ontology:constraint_beneficiary(status_signal_arms_race, attention_merchants).
narrative_ontology:constraint_victim(status_signal_arms_race, status_seekers).
narrative_ontology:constraint_victim(status_signal_arms_race, economic_inequality_amplification).
narrative_ontology:constraint_victim(status_signal_arms_race, resource_allocation_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATUS SEEKER (SNARE) — Individual trapped in escalating consumption to maintain relative status. No exit without loss of social position. Faces continuous extraction through competitive signaling requirements with no coordination benefit. Maximum suppression: status is internalized as essential to identity and belonging.
constraint_indexing:constraint_classification(status_signal_arms_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STATUS-CONSCIOUS COMMUNITY (TANGLED ROPE) — Community coordinates on visible status markers (education, housing, apparel) that do solve real coordination problems (ability signaling, trustworthiness verification) while simultaneously driving extractive arms race dynamics. Constrained by social pressure and economic cost; also benefits from coordination function. Moderate extraction with genuine mixed function.
constraint_indexing:constraint_classification(status_signal_arms_race, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LUXURY GOODS PRODUCERS (ROPE) — Institutional actors who benefit from status signaling arms race. Experience the constraint as coordination mechanism enabling market segmentation and price discrimination. Arbitrage positions allow navigation of luxury market globally. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(status_signal_arms_race, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANTI-CONSUMPTION COALITION (SCAFFOLD) — Organized movements (minimalism, sustainable fashion, value-signaling via non-consumption) represent alternative status signaling systems with inherent sunset logic. As these coalitions grow and redefine status criteria, traditional consumption-based signaling loses power. Constrained by mainstream social norming but building exit pathways through alternative status metrics (environmental consciousness, simplicity, authenticity).
constraint_indexing:constraint_classification(status_signal_arms_race, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HERITAGE STATUS SYSTEM (PITON) — Traditional markers of status (class, hereditary titles, land ownership) persist through institutional inertia despite reduced functional signaling capacity. Modern consumption-based signaling has largely replaced heritage status, yet vestigial status rituals (aristocratic titles, country club memberships, old money aesthetics) persist performatively. Theater ratio indicates maintenance of form without functional content.
constraint_indexing:constraint_classification(status_signal_arms_race, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, status signaling is inherent to human social hierarchy and relative position comparison. Some level of status differentiation appears unavoidable in large-scale societies with division of labor and specialization. However, the structural data reveals this as naturalization: the arms race escalation, the suppression of alternatives, the extraction mechanism are contingent institutional features, not immutable laws of human nature.
constraint_indexing:constraint_classification(status_signal_arms_race, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(status_signal_arms_race_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(status_signal_arms_race, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(status_signal_arms_race, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(status_signal_arms_race, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(status_signal_arms_race, TR),
    TR >= 0.70.

:- end_tests(status_signal_arms_race_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint combines genuine coordination function (status markers do convey real information about ability, trustworthiness, aesthetic sophistication) with clear extraction dynamics (arms race escalation forces continuous consumption increases with diminishing coordination value). Initial extractiveness (0.38) reflected stronger coordination function in earlier periods when status markers were more stable and less frequently updated; current value (0.58) reflects 40-year deterioration as fashion cycles accelerate and luxury goods inflation outpaces quality improvement. Suppression (0.65): High. Multiple suppression mechanisms operate: (1) internalized identity fusion with status markers (identity_locked for many agents); (2) material economic barriers to non-consumption (constrained for moderate agents); (3) information asymmetry preventing agents from coordinating on alternative status systems; (4) institutional monopolization of status definition by luxury goods producers. Theater ratio (0.68): High and increasing. Much status signaling is performative — the signal's value derives from visibility rather than functional content. High-end fashion, luxury automobiles, and prestige consumption categories show minimal functional improvement despite massive price increases; the theater (visual differentiation) has become the primary product. This indicates piton-level degradation where the performance persists despite reduced functional foundation.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximum. Luxury producers see coordination function and market efficiency; status seekers see mandatory extraction with no exit. The gap reveals that the constraint's structure maps onto wealth distribution — agents with capital can participate in status competition strategically (arbitrage), while constrained agents experience it as mandatory escalation (trapped or constrained). The anti-consumption coalition perspective bridges the gap by proposing alternative status metrics, which threatens the constraint's extraction mechanism and explains why institutional actors invest in maintaining consumption-based status definitions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Beneficiaries with arbitrage options (luxury producers, attention merchants) derive low d values from their ability to navigate status markets globally and capture value. Status seekers with trapped exit options derive high d values — they cannot escape competitive consumption without social cost and identity loss, placing them fully in the extraction target position. Communities with constrained options face moderate d values reflecting their mixed position: they benefit from status coordination but bear costs of arms race escalation. Organized coalitions with access to alternative systems (constrained exit but with coalition power) derive moderate d values reflecting agency within constraint. The analytical observer derives d from observer position (0.73 canonical) reflecting distance from extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination (status signals do convey information about ability and trustworthiness) from extractive overlay (arms race escalation extracts resources without coordination benefit). The Tangled Rope classification correctly captures this hybrid: the constraint would classify as Rope if extractiveness were lower and theater were lower (pure coordination with minimal overhead), or as Snare if extractiveness were higher and suppression more total (pure extraction with minimal coordination). At current parameters, both functions are substantial and structural — beneficiaries genuinely benefit from coordination infrastructure while simultaneously extracting through arms race monopolization. The measurement trajectory (increasing extractiveness, increasing theater) shows the constraint degrading from genuine coordination toward pure extraction, which would trigger Snare reclassification if the trend continues past ε ≥ 0.66.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_function_vs_extraction_mechanism,
    'Do status signals coordinate genuine information (ability, trustworthiness, taste, values) or primarily function as zero-sum extraction vehicles?',
    'Empirical analysis of signal reliability: correlation between status markers and actual competence/trustworthiness; assessment of whether status information improves matching efficiency or primarily enables premium pricing and hierarchy maintenance',
    'If primarily informational: constraint reclassifies toward Rope from multiple perspectives. If primarily zero-sum: Snare and Tangled Rope classifications confirmed; extraction mechanism is dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_vs_extraction_mechanism, empirical, 'Whether status signals provide genuine coordination function or are primarily extraction mechanisms').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.65) structural (material barriers to non-status consumption) or internalized (agent''s identity fused with status signaling)?',
    'Post-exit analysis: tracking agents who exit status competition permanently (monastic movements, intentional communities, extreme minimalists) to assess whether suppression persists after structural barriers are removed; identity-lock detection through post-exit identity reconstruction',
    'If structural: suppression reflects real economic/social costs; exit possible at measurable price. If internalized: agents carry suppression forward even after barriers removed; identity_locked exit option may apply to multiple perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Suppression mechanism: structural vs internalized').

omega_variable(
    alternative_status_metric_viability,
    'Can alternative status systems (sustainability metrics, local reputation, skill-based markers, values-alignment signaling) scale to population-wide status coordination, or are they fundamentally limited to niche communities?',
    'Longitudinal tracking of alternative status systems adoption rates; measurement of status coherence across diverse social contexts; analysis of whether alternative metrics degrade under growth pressure',
    'If scalable: scaffold perspective is correct; sunset timeline is real. If niche-limited: alternative systems cannot replace consumption-based signaling; arms race is structural rather than contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_status_metric_viability, empirical, 'Scalability of alternative status signaling systems').

omega_variable(
    extraction_vs_efficiency_loss,
    'Is the net effect extraction (beneficiaries gain more than victims lose) or efficiency loss (total output declines due to zero-sum status competition)?',
    'Macroeconomic accounting: measurement of consumption volatility, savings rate degradation, labor allocation to status-signaling sectors; comparison of resource flows to luxury goods vs productive investment',
    'If extraction: constraint redistributes resources from victims to beneficiaries. If efficiency loss: constraint reduces total welfare; may reclassify toward snare for entire population rather than tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_efficiency_loss, empirical, 'Whether arms race produces extraction or efficiency loss').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(status_signal_arms_race, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, status_signal_arms_race, theater_ratio, 0, 0.52).
narrative_ontology:measurement(stat_tr_t20, status_signal_arms_race, theater_ratio, 20, 0.6).
narrative_ontology:measurement(stat_tr_t40, status_signal_arms_race, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, status_signal_arms_race, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(stat_be_t20, status_signal_arms_race, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(stat_be_t40, status_signal_arms_race, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(status_signal_arms_race, identity_coordination).
narrative_ontology:affects_constraint(status_signal_arms_race, positional_goods_monopoly).
narrative_ontology:affects_constraint(status_signal_arms_race, conspicuous_consumption_feedback_loop).
narrative_ontology:affects_constraint(status_signal_arms_race, attention_market_extraction).

% DUAL FORMULATION NOTE:
% Status signaling coordinates group membership and status hierarchy (identity_coordination function) while simultaneously driving extraction through competitive escalation. Upstream constraints (positional goods monopoly, attention markets) enable this hybrid by controlling the supply of status signal channels and the information flows that define status criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(status_signal_arms_race, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
