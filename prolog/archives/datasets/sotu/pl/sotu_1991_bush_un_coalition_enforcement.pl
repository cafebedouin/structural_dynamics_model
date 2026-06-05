% ============================================================================
% CONSTRAINT STORY: sotu_1991_bush_un_coalition_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1991_bush_un_coalition_enforcement, []).

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
 *   constraint_id: sotu_1991_bush_un_coalition_enforcement
 *   human_readable: UN-Backed Multilateral Coalition Enforcement Mechanism for Territorial Sovereignty
 *   domain: foreign_policy/international_law
 *
 * SUMMARY:
 *   The UN-backed multilateral coalition enforcement mechanism represents a
 *   structural constraint that emerges when territorial aggression triggers
 *   coordinated international response through UN Security Council
 *   resolutions and military forces from multiple nations. The 1991 coalition
 *   against Iraq involved 28 countries from 6 continents, establishing a
 *   precedent for collective enforcement of the territorial integrity
 *   principle. This constraint exhibits tangled rope structure: it
 *   coordinates the upholding of international legal norms that prevent
 *   race-to-the-bottom in territorial conquest (genuine coordination
 *   function) while simultaneously extracting costs from aggressor states,
 *   coalition member nations, and regional civilian populations. The
 *   mechanism benefits the international legal order, smaller states
 *   vulnerable to larger neighbors, and coalition leadership (particularly
 *   the U.S.); it costs the aggressor state, coalition military personnel,
 *   and populations in the conflict zone. The constraint's effectiveness as
 *   deterrent depends on credible enforcement (demonstrating that territorial
 *   conquest will be resisted collectively), but credibility itself requires
 *   bearing costs. The theater ratio shows degradation over the generational
 *   horizon as enforcement selectivity becomes apparent (applied to mid-size
 *   aggressors but not to major powers due to Security Council P5 structure),
 *   reducing the mechanism's perceived legitimacy and increasing theatrical
 *   performance relative to functional enforcement.
 *
 * KEY AGENTS:
 *   - International Legal Order: Primary beneficiary (institutional/arbitrage) — the constraint protects the territorial integrity principle that underpins state sovereignty system
 *   - Small Vulnerable States: Primary beneficiary (institutional/arbitrage) — rely on enforcement credibility for protection against larger neighbors; benefit without bearing proportional military cost
 *   - United States (Coalition Leader): Primary beneficiary (institutional/arbitrage) — demonstrates credible commitment to allies, extends deterrent umbrella, maintains post-Cold War unipolarity; bears largest absolute cost but gains strategic advantage
 *   - Mid-Power Coalition Members: Mixed beneficiary/victim (moderate/constrained) — benefit from deterrent effect and participation in shaping international response, but bear military casualties and economic costs
 *   - Aggressor State: Primary victim (powerful/trapped) — targeted by coordinated military and economic extraction; no exit except capitulation
 *   - Invaded State: Victim (powerless/trapped) — benefits from coalition support but experiences catastrophic costs from initial conquest and conflict; bears extraction regardless of coalition presence
 *   - Regional Civilian Populations: Victim (powerless/trapped) — bear costs of military operations, displacement, infrastructure destruction
 *   - UN Security Council: Institutional actor (institutional/constrained) — enforces mechanism but constrained by P5 veto structure that prevents application to major powers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1991_bush_un_coalition_enforcement, 0.38).
domain_priors:suppression_score(sotu_1991_bush_un_coalition_enforcement, 0.42).
domain_priors:theater_ratio(sotu_1991_bush_un_coalition_enforcement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1991_bush_un_coalition_enforcement, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1991_bush_un_coalition_enforcement, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1991_bush_un_coalition_enforcement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1991_bush_un_coalition_enforcement, tangled_rope).
narrative_ontology:human_readable(sotu_1991_bush_un_coalition_enforcement, "UN-Backed Multilateral Coalition Enforcement Mechanism for Territorial Sovereignty").
narrative_ontology:topic_domain(sotu_1991_bush_un_coalition_enforcement, "foreign_policy/international_law").

domain_priors:requires_active_enforcement(sotu_1991_bush_un_coalition_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1991_bush_un_coalition_enforcement, international_legal_order).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_un_coalition_enforcement, small_vulnerable_states).
narrative_ontology:constraint_beneficiary(sotu_1991_bush_un_coalition_enforcement, coalition_leadership).
narrative_ontology:constraint_victim(sotu_1991_bush_un_coalition_enforcement, aggressor_state).
narrative_ontology:constraint_victim(sotu_1991_bush_un_coalition_enforcement, coalition_military_personnel).
narrative_ontology:constraint_victim(sotu_1991_bush_un_coalition_enforcement, regional_civilians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INVADED STATE (SNARE) — Trapped by geographic vulnerability. Even with UN coalition support, the invaded state bears catastrophic costs (military casualties, infrastructure destruction, displacement, long-term reconstruction burden). The coalition mechanism coordinates international response but does not prevent the initial invasion's extraction. The invaded state experiences maximum suppression during the conflict and residual extraction through post-war dependency on coalition states for reconstruction. No meaningful exit option during the crisis window.
constraint_indexing:constraint_classification(sotu_1991_bush_un_coalition_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COALITION MEMBER NATIONS (TANGLED ROPE) — Mid-power states (Canada, UK, France, Germany, Saudi Arabia, etc.) benefit from upholding the international legal order and deterring future aggression against themselves, but face direct extraction: military casualties, economic costs of deployment, disruption of trade relations with the aggressor state. Constrained exit — withdrawing undermines deterrent credibility and invites isolation, but staying imposes material burden. Genuine coordination function (collective security) coupled with asymmetric extraction (burden-sharing is unequal; wealthier states subsidize military while others provide symbolic participation).
constraint_indexing:constraint_classification(sotu_1991_bush_un_coalition_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: UNITED STATES (ROPE) — Primary beneficiary with arbitrage options. Enforces the international legal order that underscores U.S. security and economic interests; demonstrates credible commitment to alliance obligations (NATO, Japan, South Korea) to deter other potential aggressors. Bears significant military and economic costs but achieves deterrent value that extends far beyond this specific conflict. Can withdraw from coalition without legal consequences (arbitrage exit). Experiences the constraint as coordination mechanism — leadership role enables shaping the international response and maintaining post-Cold War unipolarity.
constraint_indexing:constraint_classification(sotu_1991_bush_un_coalition_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SMALL VULNERABLE STATES (ROPE) — States like Kuwait, smaller Gulf states, Baltic states, and island nations benefit from enforcement credibility. The coalition mechanism provides deterrent protection against larger neighbors without requiring them to maintain prohibitively expensive militaries. Arbitrage exit — these states can align with aggressors to avoid extraction, but doing so negates the protection benefit. Net beneficiary position over the generational horizon: the mechanism that extracts from this coalition member provides insurance against future predation. Low effective extraction despite some costs.
constraint_indexing:constraint_classification(sotu_1991_bush_un_coalition_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AGGRESSOR STATE (SNARE) — Experiences maximum extraction through military defeat, economic sanctions, territorial loss, and post-war occupation/monitoring regimes. Trapped by the coalition's coordinated response — no exit option except capitulation. The enforcement mechanism's primary function is to impose costs on territorial aggression sufficient to deter repetition. The aggressor is the focal point of the constraint's extraction. However, this classification is time-bounded: once the acute conflict ends, the aggressor's relationship to the constraint shifts (see piton perspective).
constraint_indexing:constraint_classification(sotu_1991_bush_un_coalition_enforcement, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: UN INSTITUTIONAL FRAMEWORK (PITON) — Over the generational horizon, the UN enforcement mechanism becomes substantially performative. The UN Security Council structure (P5 veto) prevents enforcement against major powers; enforcement is applied only to mid-sized aggressors. The mechanism's theater increases as it becomes clear that enforcement is contingent on geopolitical alignment rather than principled application of international law. By the generational timescale, the constraint shows classic piton degradation: the institutional architecture persists through inertia and legitimacy claims, but its functional enforcement capacity has declined. Theater ratio rises from the biographical commitment phase (high functional need) to the generational maintenance phase (normative theater sustaining diminished capability).
constraint_indexing:constraint_classification(sotu_1991_bush_un_coalition_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational/global perspective, the UN coalition enforcement mechanism is a hybrid that coordinates the upholding of territorial integrity norms (genuine collective action problem requiring coordination) while simultaneously extracting from aggressor states and coalition member nations through military and economic costs. The constraint has a real coordination function (preventing race-to-the-bottom in territorial conquest dynamics) that cannot be solved without enforcement mechanisms. The extraction (military casualties, economic burden) is the price of that coordination. The mechanism is not pure extraction (snare) because many beneficiaries genuinely depend on it; it is not pure coordination (rope) because the burdens are asymmetrically distributed and some actors are trapped. Tangled rope captures this hybrid nature across the civilizational horizon.
constraint_indexing:constraint_classification(sotu_1991_bush_un_coalition_enforcement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1991_bush_un_coalition_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1991_bush_un_coalition_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1991_bush_un_coalition_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1991_bush_un_coalition_enforcement, TR),
    TR >= 0.70.

:- end_tests(sotu_1991_bush_un_coalition_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high in biographical horizon but decreases over generational horizon. During the acute conflict (t=2), extractiveness reaches 0.58 as military and economic costs peak. In the postwar reconstruction phase (t=5), extractiveness declines to 0.42 as immediate costs are absorbed and deterrent benefit begins to accrue. Over the generational horizon (t=10), extractiveness stabilizes at 0.38 as the mechanism becomes normalized and the deterrent value is realized. The mechanism is not a snare (pure extraction) because genuine coordination benefit accrues to multiple parties — enforcement of territorial integrity norms prevents costly arms races and encourages economic integration. The mechanism is not pure rope (coordination) because extraction flows to the aggressor and coalition members bear unequal burdens. Suppression (0.42): Moderate. Coalition member nations face significant but surmountable barriers to exit — withdrawing imposes reputation costs and reduces personal security, but exit is technically possible. The invaded state faces near-total suppression (trapped exit option) during the conflict but reduced suppression post-war. The aggressor state faces maximum suppression during the enforcement phase (trapped exit) but some agency returns post-war (constrained exit as sanctions regime stabilizes). Theater ratio (0.48): Moderate, trending upward. In the pre-war phase (t=0), theater is low (0.25) because the mechanism is abstract and hypothetical. During acute conflict (t=2), theater remains moderate (0.35) as actual enforcement validates the mechanism's functionality. In the postwar phase (t=5), theater rises (0.48) as the institutional framework asserts its authority through monitoring regimes and reconstruction conditionality. Over the generational horizon (t=10), theater rises further (0.62) as selective enforcement becomes apparent — the mechanism is applied to Iraq (mid-size aggressor) but would not be applied to P5 members engaging in territorial conquest (China in Tibet/Xinjiang, Russia in Crimea). The rising theater reflects decreasing functional enforcement coupled with increasing institutional performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap between the aggressor state and coalition beneficiaries. The snare perspective (aggressor, powerless/trapped, biographical horizon) sees the constraint as pure coercive extraction with maximum suppression. The rope perspective (U.S. leadership, institutional/arbitrage, generational horizon) sees the constraint as coordination mechanism with minimal experienced extraction despite bearing absolute costs. The tangled rope perspective (coalition members, moderate/constrained, biographical horizon) sees the constraint as both coordination (collective security) and extraction (unequal burden-sharing). The piton perspective (UN framework, institutional/constrained, generational horizon) sees the constraint as degraded institutional theater — enforcement selectivity undermines the principle. This perspectival diversity is diagnostic of tangled rope: multiple actors experience the same structural phenomenon as different types depending on their position in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure differentiates actors by their power level, exit options, and beneficiary/victim status. Beneficiaries (international legal order, small states, U.S.) have arbitrage exit options, yielding low d values (0.10-0.20) and low or negative χ. Victims (aggressor state, coalition personnel, regional civilians) have trapped or constrained exit options, yielding high d values (0.60-0.95) and high χ. The U.S. position is structurally ambiguous — it bears the largest absolute cost (military personnel, economic burden) but has maximum agency and gain (deterrent extension, geopolitical dominance), making its d value moderate despite high cost. Mid-power coalition members occupy the clearest victim position relative to the mechanism's structure — they provide military personnel and economic resources for a collective good whose benefits they don't capture proportionally. No directionality overrides are required because the canonical derivation from beneficiary/victim data + power + exit options produces the correct structural picture.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled rope across multiple observation sites, not a case of mislabeled pure extraction masquerading as coordination or vice versa. The constraint has a real coordination function — it solves a genuine collective action problem (deterring territorial conquest without requiring each small state to maintain independent military deterrence). The constraint has real extraction — costs are borne by aggressor states and coalition members, with benefits concentrated on international legal order and coalition leadership. The hybrid structure is not resolvable by redefining one component: both the coordination function and the extraction are structurally essential. If enforcement were removed (solving the extraction problem), the coordination function would collapse and territorial conquest would become more attractive. If coordination were removed and the mechanism became pure punishment (solving the coordination problem), the mechanism would become indistinguishable from imperial domination. The tangled rope classification is mandatrophy-resolved because it accurately captures this hybrid structure across all observation contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_legitimacy,
    'Does selective enforcement of UN resolutions against medium powers while tolerating P5 members'' territorial actions undermine the mechanism''s credibility as a neutral international law principle, or is the mechanism simply contingent on the geopolitical capacity to enforce?',
    'Historical analysis of enforcement patterns across multiple territorial conflicts; assessment of whether stated principle (territorial integrity is inviolable) is applied consistently or only when enforcement is geopolitically feasible',
    'If selective enforcement is seen as illegitimate: mechanism reclassifies from tangled_rope to snare (enforcement becomes tool of major powers, not neutral law). If contingency is accepted as intrinsic to collective action: mechanism remains tangled_rope with reduced legitimacy floor but structural validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_legitimacy, conceptual, 'Whether selective enforcement undermines or is inherent to the mechanism').

omega_variable(
    deterrent_efficacy_threshold,
    'What cost threshold is required for the coalition enforcement mechanism to deter future aggression? Is the 1991 response sufficient to establish credible deterrence, or does credibility require repeated costly enforcement?',
    'Post-hoc analysis of subsequent territorial conflicts (2000-2026): Do states refrain from territorial conquest at rates higher than pre-1991 baseline? Control for other factors (nuclear proliferation, economic integration, regional stability).',
    'If single enforcement establishes deterrent credibility: mechanism''s suppression value is high and extraction cost is justified. If repeated enforcement is required: mechanism requires ongoing costs and the deterrent is weaker than claimed, lowering the coordination benefit relative to extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrent_efficacy_threshold, empirical, 'Whether single enforcement establishes sufficient deterrent credibility').

omega_variable(
    coalition_burden_distribution_fairness,
    'Is the unequal burden distribution across coalition members (U.S. bears majority of military/economic cost while some nations provide symbolic participation) justified by asymmetric benefit (U.S. gains greatest deterrent advantage), or does it constitute hidden extraction from mid-power contributors?',
    'Structural accounting: cost-benefit analysis per nation type (hegemon vs mid-power vs small state); long-term security dividend accrual; comparison to counterfactual (cost of maintaining independent deterrence without coalition)',
    'If burden distribution reflects asymmetric benefits: mechanism is legitimate tangled_rope with justified distribution. If burden is decoupled from benefit: mid-power nations are victims of extraction disguised as burden-sharing, and the mechanism is a snare from their perspective despite tangled_rope analytics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_burden_distribution_fairness, empirical, 'Whether burden distribution aligns with asymmetric benefits or constitutes hidden extraction').

omega_variable(
    civilian_extraction_temporal_scope,
    'Are civilian casualties and infrastructure destruction in the theater of conflict appropriately classified as suppression/extraction imposed by the enforcement mechanism, or are they externalities of the aggressor''s action that precede the coalition response?',
    'Causal attribution analysis: deaths/damage during aggressor''s conquest phase vs during coalition enforcement phase; assessment of coalition''s operational choices (targeting precision, civilian protection measures) that affect distribution of civilian costs',
    'If civilian costs are primarily aggressor-inflicted: the mechanism''s suppression value is lower (costs are already incurred by the time enforcement begins). If coalition operations add significant civilian casualties: mechanism''s suppression and extraction values increase, worsening the tangled_rope classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_extraction_temporal_scope, empirical, 'Attribution of civilian casualties to aggressor vs coalition enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1991_bush_un_coalition_enforcement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(un_coalition_theater_t0_prewar, sotu_1991_bush_un_coalition_enforcement, theater_ratio, 0, 0.25).
narrative_ontology:measurement(un_coalition_theater_t2_conflict, sotu_1991_bush_un_coalition_enforcement, theater_ratio, 2, 0.35).
narrative_ontology:measurement(un_coalition_theater_t5_postwar, sotu_1991_bush_un_coalition_enforcement, theater_ratio, 5, 0.48).
narrative_ontology:measurement(un_coalition_theater_t10_maintenance, sotu_1991_bush_un_coalition_enforcement, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(un_coalition_extractiveness_t0_prewar, sotu_1991_bush_un_coalition_enforcement, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(un_coalition_extractiveness_t2_conflict, sotu_1991_bush_un_coalition_enforcement, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(un_coalition_extractiveness_t5_postwar, sotu_1991_bush_un_coalition_enforcement, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(un_coalition_extractiveness_t10_maintenance, sotu_1991_bush_un_coalition_enforcement, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1991_bush_un_coalition_enforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1991_bush_un_coalition_enforcement, post_cold_war_unipolarity).
narrative_ontology:affects_constraint(sotu_1991_bush_un_coalition_enforcement, international_law_legitimacy).
narrative_ontology:affects_constraint(sotu_1991_bush_un_coalition_enforcement, regional_arms_race_dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
