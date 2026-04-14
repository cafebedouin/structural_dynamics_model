% ============================================================================
% CONSTRAINT STORY: indo_pacific_strategic_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_pacific_strategic_competition, []).

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
 *   constraint_id: indo_pacific_strategic_competition
 *   human_readable: Indo-Pacific Strategic Competition Framework
 *   domain: geopolitical/economic/military
 *
 * SUMMARY:
 *   The Indo-Pacific strategic competition constraint describes the
 *   institutional and military architecture through which great powers manage
 *   (or fail to manage) overlapping claims to influence in a region of rising
 *   economic and military importance. The constraint exhibits properties of
 *   both coordination and extraction: it solves the collective action problem
 *   of preventing uncontrolled conflict escalation while simultaneously
 *   concentrating strategic autonomy and economic benefits in the hands of
 *   alliance hub states and established powers. The extractiveness trajectory
 *   shows accumulation from 0.38 to 0.58 over the interval, reflecting rising
 *   military spending, tightening alliance commitments, and expansion of
 *   exclusive security frameworks. Theater ratio increases from 0.40 to 0.55,
 *   indicating growing performative content in alliance signaling,
 *   freedom-of-navigation operations, and military exercises conducted
 *   primarily for strategic messaging rather than capability development. The
 *   constraint is structurally a tangled rope at the analytical level because
 *   both the coordination function (preventing great-power war) and the
 *   extraction mechanisms (coercing strategic alignment, creating zones of
 *   vulnerability) are irreducible and persistent.
 *
 * KEY AGENTS:
 *   - United States: Alliance hub state (institutional/arbitrage) — benefits from network effects, negotiating leverage, primary beneficiary of alliance cost-sharing asymmetries
 *   - China: Emerging power (organized/identity_locked) — organizes military and economic capacity to challenge status quo; identity constituted through rising-power challenger narrative
 *   - India: Middle power (powerful/constrained) — derives security benefits from alliance participation while constrained by strategic autonomy limitations and forced alignment choices
 *   - Small Island States: Powerless region-local (powerless/trapped) — geographic and economic dependency creates forced alignment; bears costs of regional instability without strategic options
 *   - ASEAN Coalition: Organized regional integrators (organized/mobile) — pursuing regional scaffolding through institutional deepening; agency through coordination rather than coercion
 *   - Advanced Manufacturing Nations: Institutional beneficiaries (institutional/arbitrage) — extract through rare earth control, military-industrial base leverage, and export control regimes
 *   - Cold War Alliance Structures: Institutional inertia (institutional/arbitrage) — persist through path dependency and military-industrial demand despite contested functional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_pacific_strategic_competition, 0.58).
domain_priors:suppression_score(indo_pacific_strategic_competition, 0.68).
domain_priors:theater_ratio(indo_pacific_strategic_competition, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_pacific_strategic_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(indo_pacific_strategic_competition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(indo_pacific_strategic_competition, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_pacific_strategic_competition, tangled_rope).
narrative_ontology:human_readable(indo_pacific_strategic_competition, "Indo-Pacific Strategic Competition Framework").
narrative_ontology:topic_domain(indo_pacific_strategic_competition, "geopolitical/economic/military").

domain_priors:requires_active_enforcement(indo_pacific_strategic_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_pacific_strategic_competition, advanced_manufacturing_nations).
narrative_ontology:constraint_beneficiary(indo_pacific_strategic_competition, security_alliance_hub_states).
narrative_ontology:constraint_beneficiary(indo_pacific_strategic_competition, arms_exporters).
narrative_ontology:constraint_beneficiary(indo_pacific_strategic_competition, rare_earth_processors).
narrative_ontology:constraint_victim(indo_pacific_strategic_competition, small_island_states).
narrative_ontology:constraint_victim(indo_pacific_strategic_competition, resource_exporters_without_allies).
narrative_ontology:constraint_victim(indo_pacific_strategic_competition, middle_powers_in_gray_zone).
narrative_ontology:constraint_victim(indo_pacific_strategic_competition, civilian_populations_in_contested_zones).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL ISLAND STATE (SNARE) — Geographic dependency on shipping lanes controlled by competing powers, no credible security option outside patron relationships, economic vulnerability to supply chain interruptions. Trapped within strategic competition not of their making; extraction takes forms of forced alliance choice, vulnerability to coercion, and asymmetric costs of regional instability.
constraint_indexing:constraint_classification(indo_pacific_strategic_competition, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MIDDLE POWER (TANGLED ROPE) — Derives genuine coordination benefits from alliance architecture (security guarantees, economic integration, technology access) while also bearing extraction costs through military alignment constraints, limited strategic autonomy, and vulnerability to great-power conflict. Constrained: capable of independent action but at significant geopolitical cost. Mixed experience of constraint as both enabling and limiting.
constraint_indexing:constraint_classification(indo_pacific_strategic_competition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ALLIANCE HUB STATE (ROPE) — Primarily experiences coordination function: alliance system solves collective action problem of balancing an emerging peer competitor. Hub state benefits from alliance network effects, negotiating leverage, and military-industrial base demand. Arbitrage access: can threaten to reposition but has higher switching costs for competitors due to network entrenchment. Extraction runs toward this actor.
constraint_indexing:constraint_classification(indo_pacific_strategic_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL INTEGRATION COALITION (SCAFFOLD) — ASEAN-centered groupings and regional forums (EAS, RCEP) represent temporary scaffolding for reducing strategic competition through economic integration and multilateral dialogue. Coalition members have agency and see an exit path via deepening interdependence and regional institutions replacing great-power arbitration. Theater ratio lower here — genuine negotiation structures, not performative ones. Sunset logic: as regional institutions mature and economic integration deepens, the scaffolding transitions to permanent architecture.
constraint_indexing:constraint_classification(indo_pacific_strategic_competition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: EMERGING POWER (TANGLED ROPE / IDENTITY_LOCKED) — Organizes military and economic capacity to challenge the existing alliance hierarchy. Genuine coordination need: rising powers require institutional mechanisms to accommodate new capabilities within a system. But also extraction mechanism: status quo powers extract through institutional lock-in (UN veto, alliance exclusion, export control regimes) that constrains the rising power's options. Identity_locked: the emerging power's institutional identity and strategic doctrine are constituted through the framework of challenging the status quo order. Organizational capacity is real; exit would require abandoning the identity of rising power seeking rightful position.
constraint_indexing:constraint_classification(indo_pacific_strategic_competition, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR ALLIANCE ARCHITECTURE (PITON) — NATO Article 5, bilateral defense treaties, and hub-and-spoke alliance system were designed for Soviet containment. Their functional role in addressing current Indo-Pacific strategic competition is contested: these structures persist through institutional inertia and military-industrial base dependency rather than functional necessity for current threat environment. Theater ratio elevated: extended deterrence rhetoric, alliance burden-sharing theater, freedom-of-navigation operations performed for domestic audiences. The structures work (constraining adversary options) but largely through performative commitment signaling.
constraint_indexing:constraint_classification(indo_pacific_strategic_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED_ROPE / CIVILIZATIONAL) — The Indo-Pacific strategic competition constraint exhibits genuine coordination function (balancing multiple great powers in a shared region, managing power transitions, preventing wars through alliance credibility) alongside asymmetric extraction (coercing strategic choice, creating zones of vulnerability, concentrating extraction on the powerless and middle powers). Chi remains in the Tangled Rope range across civilizational time horizon because the coordination function is irreducible while extraction mechanisms persist.
constraint_indexing:constraint_classification(indo_pacific_strategic_competition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_pacific_strategic_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_pacific_strategic_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_pacific_strategic_competition, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_pacific_strategic_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indo_pacific_strategic_competition, TR),
    TR >= 0.70.

:- end_tests(indo_pacific_strategic_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts from small states through forced alliance choice and vulnerability to coercion. It extracts from middle powers through autonomy constraints and alignment costs. But it also enables coordination (balancing multiple competitors, preventing wars, enabling regional order). The measurement trajectory shows extraction accumulating as military spending rises and exclusive security frameworks tighten, suggesting the coordination function is increasingly overlaid with extraction mechanisms. Suppression (0.68): High. Significant barriers to independent action include: geographic constraints on small states, alliance entrapment costs for middle powers, exclusion mechanisms (export controls, technology access denial) for rising powers, and military superiority differentials that make non-alignment costly. Suppression is not total but substantial. Theater ratio (0.55): Moderate-high and rising. Alliance commitment signaling, freedom-of-navigation operations, and extended deterrence rhetoric serve partly genuine security functions and partly performative international messaging for domestic audiences. The increase reflects growing emphasis on signaling as competition intensifies.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence across all six types. Hub states' Rope reflects genuine coordination benefit. Small states' Snare reflects genuine extraction vulnerability. Middle powers' Tangled Rope reflects mixed experience. Regional coalition's Scaffold reflects institutional alternative under construction. Emerging power's Tangled Rope with identity_lock reflects the specific mechanism binding them (cognitive identity fusion with challenger status). Cold War alliance's Piton reflects institutional inertia. The analytical observer's Tangled Rope reflects the irreducible hybridity. The gap arises because the same structural phenomenon (alliance architecture organizing region-scale competition) simultaneously enables coordination and enables extraction, with the distribution of each depending entirely on the agent's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows beneficiary/victim declarations and exit options. Hub states (beneficiary, arbitrage) derive d ≈ 0.15-0.20, producing negative χ. Small island states (victim, trapped) derive d ≈ 0.90-0.95, producing maximum χ ≈ 1.42. Middle powers (mixed, constrained) derive d ≈ 0.55-0.65, producing moderate χ ≈ 0.75-0.90. Emerging power (victim, identity_locked, organized) derives d ≈ 0.65-0.75 through the organizational capacity partially offsetting victim status. The identity_locked exit modulation prevents d from reaching powerless levels (which would produce d > 0.85), reflecting that the emerging power has organized capacity but cognitive constraints on exercising exit options. No directionality overrides needed; the beneficiary/victim declarations and exit options produce appropriate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The Indo-Pacific constraint resolves mandatrophy by maintaining that both coordination and extraction are structurally real. This is not a case of mislabeled extraction (snare misclassified as rope) or mislabeled coordination (rope misclassified as snare). The constraint genuinely coordinates (prevents great-power war through alliance credibility and balance-of-power mechanisms) while genuinely extracting (coerces strategic alignment, concentrates benefits, creates zones of vulnerability). The tangled rope classification reflects this irreducible hybridity. The perspectival gap reinforces the mandatrophy resolution: from the beneficiary perspective the constraint appears pure coordination (Rope); from the victim perspective pure extraction (Snare); from mixed-position perspectives the hybrid nature becomes visible (Tangled Rope). The analytical observer classifies as Tangled Rope at civilizational time horizon because neither the coordination nor extraction mechanisms will resolve within historical timeframes. Alternative institutional architectures (regional integration, great-power co-accommodation, hegemonic accommodation) could redistribute the coordination-to-extraction ratio, but the constraint as currently configured maintains chi in the 0.58-0.75 range (Tangled Rope bounds).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alliance_credibility_vs_entrapment,
    'Does alliance architecture primarily solve a coordination problem (balancing competitors to prevent war) or primarily create entrapment that extracts strategic autonomy from middle powers?',
    'Historical counterfactual: would regional conflicts be more or less frequent absent alliance structures? Comparative analysis of alliance members'' strategic autonomy vs non-aligned states'' vulnerability.',
    'If coordination-primary: constraint classifies higher as rope from middle power perspective. If entrapment-primary: constraint classifies higher as snare from middle power perspective. This omega explains the perspectival gap most clearly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_credibility_vs_entrapment, empirical, 'Whether alliances primarily coordinate or primarily entrap').

omega_variable(
    great_power_competition_inevitability,
    'Is great-power competition in the Indo-Pacific structurally inevitable (rising power + status quo power in shared region) or contingent on institutional choices and strategic narratives?',
    'Comparative analysis of historical power transitions; examination of periods of great-power coexistence without conflict; assessment of whether alternative institutional frameworks could accommodate rising-power interests.',
    'If inevitable: constraint appears mountain from civilizational view (immutable feature of power transition). If contingent: constraint is tangled rope (embedded in institutions that could be restructured).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(great_power_competition_inevitability, conceptual, 'Whether great-power competition is structurally inevitable or contingent').

omega_variable(
    extraction_mechanism_beneficiary_clarity,
    'Do advanced manufacturing nations and security hub states consciously extract from the constraint architecture, or do they primarily benefit through structural position while pursuing other goals?',
    'Policy discourse analysis; examination of whether alliance burden-sharing demands are adjusted when extraction becomes visible; assessment of whether beneficiaries resist institutional reforms that would reduce their leverage.',
    'If conscious extraction: classification as snare from small-state perspective is stronger; beneficiary actor sees extraction opportunity and defends the structure. If structural benefit without intentional extraction: the mechanism is better classified as unequal coordination (tangled rope) rather than predatory (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_beneficiary_clarity, empirical, 'Whether extraction is conscious beneficiary strategy or structural side effect').

omega_variable(
    asean_centrality_functionality,
    'Can ASEAN-centered regional institutions (EAS, RCEP) genuinely reduce great-power competition and provide structural alternative to alliance-based balancing, or are these mechanisms subordinate theater?',
    'Longitudinal assessment of ASEAN institutional effectiveness at conflict prevention and economic integration; comparison of outcomes in ASEAN-led multilateral framework vs alliance-based bilateral arrangements; analysis of whether major powers treat ASEAN consensus as binding.',
    'If functional: regional integration represents genuine scaffold with realistic sunset. If subordinate: scaffold classification is aspirational and the constraint''s theater ratio is actually higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asean_centrality_functionality, empirical, 'Whether ASEAN-centered institutions provide genuine alternative to alliance balancing').

omega_variable(
    identity_lock_breakability_emerging_power,
    'Can the emerging power''s institutional identity as ''challenger to status quo order'' decouple from its strategic doctrine, or is the identity so fused that institutional accommodation would require existential role redefinition?',
    'Analysis of institutional discourse and identity claims; examination of whether alternative status narratives (regional peer, responsible stakeholder, system participant) are entertained by decision-making elites; assessment of whether security doctrine could shift if political identity shifted.',
    'If identity_locked: emerging power''s choices are constrained at the cognitive level; exit from competition would require abandoning institutional identity. If merely constrained: high-cost exit is possible and the power could theoretically reposition. The difference determines whether the emerging power experiences the constraint as mountain or tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_breakability_emerging_power, conceptual, 'Whether emerging power''s identity is locked into challenger status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_pacific_strategic_competition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indopac_tr_t0, indo_pacific_strategic_competition, theater_ratio, 0, 0.4).
narrative_ontology:measurement(indopac_tr_t5, indo_pacific_strategic_competition, theater_ratio, 5, 0.48).
narrative_ontology:measurement(indopac_tr_t10, indo_pacific_strategic_competition, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(indopac_be_t0, indo_pacific_strategic_competition, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(indopac_be_t5, indo_pacific_strategic_competition, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(indopac_be_t10, indo_pacific_strategic_competition, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_pacific_strategic_competition, enforcement_mechanism).
narrative_ontology:affects_constraint(indo_pacific_strategic_competition, semiconductor_supply_chain_security).
narrative_ontology:affects_constraint(indo_pacific_strategic_competition, rare_earth_supply_chain_dependency).
narrative_ontology:affects_constraint(indo_pacific_strategic_competition, south_china_sea_freedom_of_navigation).
narrative_ontology:affects_constraint(indo_pacific_strategic_competition, taiwan_strait_military_balance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indo_pacific_strategic_competition, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
