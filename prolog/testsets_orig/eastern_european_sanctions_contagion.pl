% ============================================================================
% CONSTRAINT STORY: eastern_european_sanctions_contagion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eastern_european_sanctions_contagion, []).

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
 *   constraint_id: eastern_european_sanctions_contagion
 *   human_readable: Eastern European Sanctions Contagion
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Eastern European sanctions contagion describes the structural constraint
 *   whereby buffer states between Western sanctioning coalitions and
 *   sanctioned actors are forced to choose alignment while bearing
 *   disproportionate economic costs. The constraint exhibits genuine
 *   coordination function (unified sanctions enhance deterrence, prevent
 *   evasion through third-party routes) alongside asymmetric extraction
 *   (periphery absorbs energy shocks, currency volatility, and trade
 *   disruption while core Western economies buffer through diversification
 *   and capital reallocation). The contagion emerges from geographic
 *   proximity, energy infrastructure legacies (Russian pipeline dependency),
 *   and post-Cold War institutional commitments that have become
 *   identity-fused for Eastern European state elites. The constraint is
 *   neither purely extractive nor purely coordinating — it is a Tangled Rope
 *   where legitimate deterrence overlays rent-seeking by Western capital
 *   seeking supply-chain consolidation, where collective defense rationales
 *   intertwine with political-class alignment interests.
 *
 * KEY AGENTS:
 *   - Eastern European Buffer States: Primary victims (powerless/trapped, also institutional/identity_locked) — trapped by geography and alliance identity; bear disproportionate costs while having minimal voice in coalition decision-making
 *   - Western Sanctioning Coalition (US, EU core, G7): Primary beneficiary (institutional/arbitrage) — sets coalition terms, can absorb or externalize costs, maintains deterrence multiplier benefits
 *   - Multinational Corporate Capital: Secondary beneficiary (powerful/mobile) — captures supply-chain consolidation gains, arbitrages sanctions-compliant premium pricing
 *   - Civilian Populations in Buffer States: Victims (powerless/trapped) — bear direct costs through energy insecurity, inflation, currency depreciation; no meaningful exit or political voice
 *   - EU Collective Action: Organizational actor (organized/constrained) — coordinates sanctions but distributes burden asymmetrically; periphery states' exit is constrained by consensus requirement
 *   - Sanctioned Actor (Russia): Counterparty (powerful/constrained) — experiences extraction but retains agency through autarky and alternative partnerships; not powerless
 *   - International Trade Rules Framework: Institutional (institutional/arbitrage) — maintains symbolic authority while being superseded by geopolitical alliance structures; theater ratio high
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eastern_european_sanctions_contagion, 0.58).
domain_priors:suppression_score(eastern_european_sanctions_contagion, 0.65).
domain_priors:theater_ratio(eastern_european_sanctions_contagion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eastern_european_sanctions_contagion, extractiveness, 0.58).
narrative_ontology:constraint_metric(eastern_european_sanctions_contagion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eastern_european_sanctions_contagion, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eastern_european_sanctions_contagion, tangled_rope).
narrative_ontology:human_readable(eastern_european_sanctions_contagion, "Eastern European Sanctions Contagion").
narrative_ontology:topic_domain(eastern_european_sanctions_contagion, "geopolitical/economic").

domain_priors:requires_active_enforcement(eastern_european_sanctions_contagion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eastern_european_sanctions_contagion, western_sanctioning_coalition).
narrative_ontology:constraint_beneficiary(eastern_european_sanctions_contagion, us_foreign_policy_apparatus).
narrative_ontology:constraint_victim(eastern_european_sanctions_contagion, eastern_european_buffer_states).
narrative_ontology:constraint_victim(eastern_european_sanctions_contagion, trade_dependent_economies).
narrative_ontology:constraint_victim(eastern_european_sanctions_contagion, civilian_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BUFFER STATE ECONOMIES (SNARE) — Trapped between sanctioning coalitions and sanctioned actors. Cannot exit without severe geopolitical consequences; bear disproportionate costs of sanctions enforcement (lost trade, currency volatility, energy disruption). Maximum extraction from a powerless position with no legitimate exit route.
constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EU COLLECTIVE ACTION (TANGLED ROPE) — Genuine coordination function: unified sanctions enhance deterrence value and prevent sanctions evasion via third-party routes. Simultaneously exhibits asymmetric extraction: burden falls disproportionately on periphery states (Poland, Hungary, Baltic states) while core Western European states maintain economic insulation through diversified supply chains and capital flows. Exit is constrained — EU member states risk isolation and security withdrawal if they defect from sanctions consensus.
constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WESTERN SANCTIONING COALITION (ROPE) — Benefits from first-mover coordination advantage; sanctions mechanism functions as coordination among allies (unified message, deterrence multiplier, sanctions coherence). Coalition members (US, EU core, G7) can arbitrage into alternative supply chains or absorb costs through capital reallocation. Low experienced extraction — coordination benefits exceed burden for coalition members.
constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL CORPORATE CAPITAL (TANGLED ROPE) — Experiences sanctions as coordinating constraint (reduces competitive uncertainty, clarifies regulatory environment) while also providing extraction opportunity (sanctions-compliant supply chains command premium; first-movers gain market consolidation). Capital is mobile — firms can relocate supply chains, repatriate profits, access capital markets. Mixed experience: genuine coordination benefit with embedded extractive opportunity.
constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVILIAN POPULATIONS (SNARE) — Trapped by geography and citizenship; bear costs of sanctions through energy insecurity, inflation, supply disruption, currency depreciation. No meaningful exit (emigration requires capital and legal status). Maximum extraction with zero agency. Suppression operates through geographic immobility and state control of key commodities.
constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: INTERNATIONAL TRADE RULES (PITON) — WTO and bilateral trade agreements persist despite sanctions regime contradicting stated rules-based order principles. Theater ratio high: sanctions presented as rules-based enforcement while simultaneously violating most-favored-nation principles and transparent dispute resolution. Trade framework maintains symbolic authority but has lost functional coordination capacity — superseded by geopolitical alliance structures.
constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ENERGY TRANSITION COALITION (SCAFFOLD) — European Green transition programs provide temporary support structure for buffer states to reduce Russian energy dependency. Sunset logic: as renewable capacity and LNG infrastructure mature (15-20 year horizon), sanctions-imposed energy decoupling becomes permanent technical decoupling. Current extraction mechanism (high energy costs, supply insecurity) declines as alternative infrastructure reaches critical mass. High suppression now justified by temporary structural transformation.
constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: EASTERN EUROPEAN STATES / INSTITUTIONAL IDENTITY (TANGLED ROPE) — States have internalized post-Cold War institutional identity as 'Western-allied democracies' whose legitimacy depends on NATO/EU alignment. Structural mobility exists (could pursue neutrality, diversified supply chains) but identity frame makes this politically unthinkable within domestic elite consensus. Exit is blocked not by external barriers but by fused institutional identity. Coordination function genuine (deterrence multiplier, alliance cohesion); asymmetric extraction embedded (periphery absorbs disproportionate costs while core buffers from shocks).
constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational view, sanctions represent an immutable feature of geopolitical structure: great powers always pressure subordinate powers through economic coercion, and geographic proximity to rival powers creates inherent vulnerability. This perspective naturalizes contingent alliance politics as unchangeable law. However, structural data reveals false summit: sanctions are institutional choices, not natural law. Historical contingency (post-WWII order choice) masquerades as immutability.
constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eastern_european_sanctions_contagion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eastern_european_sanctions_contagion, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eastern_european_sanctions_contagion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eastern_european_sanctions_contagion, TR),
    TR >= 0.70.

:- end_tests(eastern_european_sanctions_contagion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits genuine coordination value (unified sanctions prevent evasion, enhance deterrence credibility) but extraction is substantial and asymmetric. Buffer states face energy cost spikes (15-30% inflation attributable to sanctions), supply chain disruption, and currency depreciation. Western coalition members insulate themselves through capital mobility and diversified supply chains. The 0.58 value reflects that extraction is significant but not total — buffer states retain some trade options and receive EU infrastructure support; the constraint is not absolute predation. Suppression (0.65): Moderate-high. Geographic immobility (buffer states cannot relocate), geopolitical alliance constraints (defection from coalition carries security costs), infrastructure lock-in (pipeline dependencies created over decades), and state commodity control (energy markets are state-regulated) all limit exit options. However, suppression is not total — informal sanctions evasion networks exist, energy transition options exist (LNG, renewables), and some coalition defection has occurred (Hungary). Theater ratio (0.48): Moderate. Early-stage constraint. The sanctions regime is presented as rules-based deterrence but contradicts stated rules-based order principles. Theater ratio increases over time as coalition narrative must work harder to justify distributional asymmetry. Energy transition coalition frame (Scaffold) is increasingly invoked to justify current costs as temporary — theater grows as this narrative becomes central to legitimation.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the Western coalition's rope experience and buffer states' snare experience is the diagnostic signal for the Tangled Rope classification. Coalition members genuinely experience coordination (deterrence multiplier, strategic clarity, sanctions coherence). Buffer states genuinely experience extraction (asymmetric cost distribution, currency depreciation, energy insecurity). Both experiences are true — the constraint IS a coordination mechanism AND an extraction mechanism. The classification as Tangled Rope (not Rope, not Snare, but both) requires this gap. The institutional identity-lock perspective reveals an additional gap: Eastern European states as institutional actors have structural options (neutrality, diversified partnerships, energy independence) but their post-Cold War identity frame makes these options politically unthinkable within elite consensus. This reveals that the binding mechanism is partially cognitive — the constraint persists not only through material barriers but through internalized framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation of d values reflects structural position via beneficiary/victim + exit options. Western coalition: beneficiary + arbitrage → d ≈ 0.15 → f(d) ≈ -0.01 → χ negative (beneficiary experienced). Buffer states (powerless perspective): victim + trapped → d ≈ 0.85 → f(d) ≈ 1.15 → χ high (victim experienced). EU collective: enforcer role generates d differentiation by member position — core members approximate coalition d; periphery members approximate buffer state d, but constrained exit prevents true victim status. Eastern European institutional states (identity-locked): d ≈ 0.68 derived from victim status (bear costs) + identity-locked exit (cannot perceive alternatives from within frame) → f(d) ≈ 1.08 → χ ≈ 0.63. This is higher χ than the powerless/trapped perspective despite higher agent power, because the identity lock prevents exercising mobility options. The overrides are not needed — the derivation chain captures the structure correctly via identity_locked exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The Tangled Rope classification resolves the mandatrophy by disambiguating the constraint into its genuine coordination function and its embedded asymmetric extraction. The coordinating function is real: unified sanctions prevent evasion, enhance deterrence credibility, and solve a genuine collective action problem among Western states. The extraction is also real: buffer states and civilian populations bear disproportionate costs while having minimal voice in coalition decision-making. The constraint would be misclassified as pure Rope if the analysis focused only on coalition cohesion benefits or as pure Snare if focused only on buffer state victimization. The truth is both. The Tangled Rope resolution requires all three gates: (1) beneficiaries declared (Western coalition), (2) victims declared (buffer states, civilian populations), (3) active enforcement required (yes — sanctions enforcement apparatus). Without all three, the classification would degrade toward false purity. The identity-locked exit option adds a fourth diagnostic layer: the constraint persists not only through material cost asymmetry but through fused institutional identity. Breaking the constraint would require Eastern European states to abandon their post-Cold War identity frame — a transformation of perceived interest, not just a cost-benefit recalculation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    buffer_state_exit_threshold,
    'At what cost threshold do Eastern European states perceive exit from Western sanctions coalition as feasible despite identity-lock?',
    'Historical comparison: post-Soviet state behavior under sanctions; correlation between energy cost spikes and political realignment pressure; analysis of populist backlash timing relative to sanctions burden escalation',
    'If threshold is low (< 15% inflation): identity-lock is weaker than classified; exit pressure mounts faster. If threshold is high (> 30%): identity lock is deeply institutionalized; contagion persists through crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(buffer_state_exit_threshold, empirical, 'Cost threshold for Eastern European exit from sanctions coalition').

omega_variable(
    sanctions_effectiveness_paradox,
    'Do sanctions actually deter target behavior or do they deepen commitment through rally-around-flag effects and supply-chain autarky?',
    'Longitudinal behavioral analysis of sanctioned state: compare pre-sanctions stated objectives vs post-sanctions strategy changes; measure rally effects in public opinion; track sanctions evasion capacity growth',
    'If sanctions ineffective at deterrence: contagion costs are pure extraction with no coordination payoff (Snare not Tangled Rope). If effectiveness variable by context: classification depends on geopolitical assumptions embedded in coalition narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_effectiveness_paradox, empirical, 'Whether sanctions achieve stated deterrence objectives').

omega_variable(
    energy_transition_timeline_credibility,
    'Will European renewable/LNG infrastructure reach critical mass within stated 15-20 year horizon, or does political/technical risk extend timeline indefinitely?',
    'Scenario modeling: grid stability analysis, LNG terminal capacity buildout tracking, renewable generation ramp-up vs demand growth, political support durability over electoral cycles',
    'If timeline credible: Scaffold perspective is structural reality. If timeline extends beyond 30 years: constraint becomes permanent Snare for buffer states; sunset is aspirational theater, not real structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_transition_timeline_credibility, empirical, 'Credibility of European energy transition sunset timeline').

omega_variable(
    identity_lock_mechanism_interpenetration,
    'To what degree is the institutional identity-lock (post-Cold War Western alignment) created by genuine security threat vs. top-down political elite consensus unrepresentative of publics?',
    'Public opinion polling: support for NATO/EU membership vs sanctions burden; comparison of elite vs public preferences for neutrality options; analysis of anti-establishment party success in buffer states',
    'If threat-driven: identity-lock reflects genuine perceived interests; exit remains unthinkable even under cost pressure. If elite-imposed: identity-lock is more fragile; political disruption could enable rapid exit, converting Tangled Rope to Snare in biographical timeframe.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpenetration, conceptual, 'Threat-driven vs elite-consensus basis for institutional identity lock').

omega_variable(
    sanctions_evasion_capacity_trajectory,
    'Are buffer states and target states developing effective sanctions evasion networks faster than coalition can close loopholes?',
    'Tracking: third-country transshipment volumes, cryptocurrency transaction patterns, informal barter networks, corporate shell company sophistication; measurement of sanctions enforcement resource allocation vs evasion innovation rate',
    'If evasion outpaces enforcement: extraction mechanism weakens; suppression declines; constraint migrates toward Rope over biographical timeframe. If enforcement containment succeeds: suppression and extraction remain elevated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_evasion_capacity_trajectory, empirical, 'Sanctions evasion capacity trajectory vs enforcement capability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eastern_european_sanctions_contagion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eesc_tr_t0, eastern_european_sanctions_contagion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eesc_tr_t2, eastern_european_sanctions_contagion, theater_ratio, 2, 0.42).
narrative_ontology:measurement(eesc_tr_t4, eastern_european_sanctions_contagion, theater_ratio, 4, 0.48).
narrative_ontology:measurement(eesc_tr_t6, eastern_european_sanctions_contagion, theater_ratio, 6, 0.51).

% Extraction over time
narrative_ontology:measurement(eesc_be_t0, eastern_european_sanctions_contagion, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(eesc_be_t2, eastern_european_sanctions_contagion, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(eesc_be_t4, eastern_european_sanctions_contagion, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(eesc_be_t6, eastern_european_sanctions_contagion, base_extractiveness, 6, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eastern_european_sanctions_contagion, enforcement_mechanism).
narrative_ontology:affects_constraint(eastern_european_sanctions_contagion, energy_infrastructure_lock_in).
narrative_ontology:affects_constraint(eastern_european_sanctions_contagion, european_green_transition).
narrative_ontology:affects_constraint(eastern_european_sanctions_contagion, nato_expansion_commitment).
narrative_ontology:affects_constraint(eastern_european_sanctions_contagion, sanctions_evasion_networks).

% DUAL FORMULATION NOTE:
% Eastern European sanctions contagion is downstream of geopolitical conflict (Russia-Ukraine war) and upstream of energy transition infrastructure investments. The constraint represents the institutional chokepoint where coordination (sanctions deterrence) and extraction (cost asymmetry) are structurally inseparable. Three related constraints form a constraint family: (1) energy_infrastructure_lock_in (ε=0.25, structurally constraining but lower extraction), (2) sanctions_contagion (ε=0.58, current story), (3) energy_transition_mechanism (ε=0.30, Scaffold with sunset). The identity-lock mechanism appears primarily in this story but relates to nato_expansion_commitment story where institutional identity-fusion is the primary binding mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eastern_european_sanctions_contagion, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
