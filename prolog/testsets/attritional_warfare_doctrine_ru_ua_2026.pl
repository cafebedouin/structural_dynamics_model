% ============================================================================
% CONSTRAINT STORY: attritional_warfare_doctrine_ru_ua_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attritional_warfare_doctrine_ru_ua_2026, []).

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
 *   constraint_id: attritional_warfare_doctrine_ru_ua_2026
 *   human_readable: Russian Attritional Warfare Doctrine in Ukraine (2026)
 *   domain: geopolitical/military
 *
 * SUMMARY:
 *   The Russian military's attritional warfare doctrine in Ukraine,
 *   exemplified by the deployment of Storm-Z convict units in frontal
 *   assaults with casualty rates exceeding 50-70%, represents a structural
 *   snare affecting multiple victim classes: conscripted soldiers with zero
 *   exit options, Ukrainian combatants and civilians under sustained
 *   bombardment, and ultimately Russian military effectiveness itself. The
 *   doctrine benefits the Russian state apparatus through territorial
 *   consolidation and enemy force degradation without requiring technological
 *   parity, but the extraction mechanism is increasingly sustained by
 *   institutional inertia rather than military rationality. Theater ratio
 *   (0.55) reflects that the attritional approach is partly performative — it
 *   maintains Cold War-era doctrine despite clear evidence of tactical
 *   ineffectiveness against Ukrainian defensive tactics and NATO support. The
 *   constraint exhibits all six DR types across different perspectives: pure
 *   snare for trapped conscripts, tangled rope for organized Ukrainian
 *   military (mixed coordination and extraction), rope for the Russian state
 *   (experiencing it as coordination), piton for the inherited Cold War
 *   doctrine, and snare again for the analytical observer viewing the mutual
 *   destructiveness of the structure itself.
 *
 * KEY AGENTS:
 *   - Russian Military Command: Primary beneficiary (institutional/arbitrage) — captures territorial gains and force degradation without technological superiority
 *   - Russian State Apparatus: Institutional beneficiary (institutional/arbitrage) — maintains strategic position in eastern Ukraine despite economic costs
 *   - Storm-Z Conscripts and Convicts: Primary victims (powerless/trapped) — mobilized soldiers with zero exit options; casualty rates 40-70% in frontal assaults
 *   - Ukrainian Military Command: Secondary beneficiary and victim (organized/constrained) — coordinated resistance with NATO support but faces enforced participation in degrading combat
 *   - Ukrainian Civilian Population: Victim (powerless/trapped) — internally displaced or under sustained bombardment with no exit mechanism
 *   - Russian Military Doctrine System: Institutional actor (institutional/arbitrage) — maintains Cold War-era attritional approach through institutional inertia despite effectiveness degradation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a mutual snare where both sides are trapped by sunk costs and signaling requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attritional_warfare_doctrine_ru_ua_2026, 0.78).
domain_priors:suppression_score(attritional_warfare_doctrine_ru_ua_2026, 0.85).
domain_priors:theater_ratio(attritional_warfare_doctrine_ru_ua_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attritional_warfare_doctrine_ru_ua_2026, extractiveness, 0.78).
narrative_ontology:constraint_metric(attritional_warfare_doctrine_ru_ua_2026, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attritional_warfare_doctrine_ru_ua_2026, snare).
narrative_ontology:human_readable(attritional_warfare_doctrine_ru_ua_2026, "Russian Attritional Warfare Doctrine in Ukraine (2026)").
narrative_ontology:topic_domain(attritional_warfare_doctrine_ru_ua_2026, "geopolitical/military").

domain_priors:requires_active_enforcement(attritional_warfare_doctrine_ru_ua_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attritional_warfare_doctrine_ru_ua_2026, russian_military_command).
narrative_ontology:constraint_beneficiary(attritional_warfare_doctrine_ru_ua_2026, russian_state_apparatus).
narrative_ontology:constraint_victim(attritional_warfare_doctrine_ru_ua_2026, russian_conscripts_and_convicts).
narrative_ontology:constraint_victim(attritional_warfare_doctrine_ru_ua_2026, ukrainian_military_and_civilians).
narrative_ontology:constraint_victim(attritional_warfare_doctrine_ru_ua_2026, russian_combat_effectiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STORM-Z CONSCRIPT (SNARE) — Mobilized or imprisoned soldiers face maximum extraction with zero exit options. Conscription is mandatory; desertion is punishable by execution. These units suffer 40-70% casualty rates in frontal assaults. No choice of participation, no exit mechanism, full bearing of extraction cost. This is the canonical snare perspective — trapped agent experiencing maximal effective extraction.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UKRAINIAN CIVILIAN POPULATION (SNARE) — Trapped within the conflict zone. Attritional doctrine produces sustained artillery and drone strikes targeting infrastructure. Civilians cannot exit Ukraine en masse; internally displaced persons face extreme hardship. No exit option, maximum bearing of collateral extraction costs. Structurally equivalent to conscripts despite different causal mechanism.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: UKRAINIAN MILITARY COMMAND (TANGLED ROPE) — Constrained by resource scarcity and NATO ammunition supply bottlenecks, but gains organizational cohesion and international support through organized resistance. Experiences the constraint as both coordination (shared defense objective) and extraction (enforced participation in degrading combat conditions). Can theoretically exit through negotiated settlement, but political costs are severe. Mixed extraction and genuine coordination function.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: RUSSIAN STATE APPARATUS (ROPE) — Benefits from attritional strategy through territorial consolidation and degradation of Ukrainian military capacity without requiring technological parity. Experiences the constraint as a coordination mechanism: directing mass manpower into grinding attrition solves the problem of how to prosecute war without superior equipment. Can exit through negotiated settlement with territorial gains. Net beneficiary of the extraction — extraction flows toward this agent.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: COLD WAR DOCTRINE INHERITANCE (PITON) — Russian military doctrine emphasizes mass manpower and attritional grinding as a legacy of Soviet-era doctrine. This approach is increasingly performative in a modern warfare context: attritional tactics are maintained despite their evident ineffectiveness against superior Ukrainian tactics and NATO support. The doctrine persists through institutional inertia and the absence of institutional reform, not because it optimizes military outcomes. Theater ratio reflects that the 'meat grinder' is maintained as ritual despite clear degradation of military effectiveness.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, attritional warfare is a snare for all participants. The extraction mechanism (human life, material resources, economic disruption) persists because no participant can exit without accepting major strategic losses. The constraint is mutually destructive but structurally self-sustaining — each side's cost of continued conflict is less than the perceived cost of withdrawal. This is not a natural law but a structural trap created by sunk costs and signaling requirements.
constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attritional_warfare_doctrine_ru_ua_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attritional_warfare_doctrine_ru_ua_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attritional_warfare_doctrine_ru_ua_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attritional_warfare_doctrine_ru_ua_2026, TR),
    TR >= 0.70.

:- end_tests(attritional_warfare_doctrine_ru_ua_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high, reflecting the severity of casualty extraction from conscripted personnel and the forced subordination of civilian economy to military production. The value has increased from 0.62 to 0.78 over the measured interval, indicating accumulation of extraction as mobilization waves intensify and casualty replacement demands grow. This is not sustainable indefinitely. Suppression (0.85): Extremely high. Conscription is mandatory; desertion is punishable by execution or military tribunal. Exit options are nonexistent for conscripted soldiers. Civilian population faces similar nonexistence of exit mechanisms (cannot flee Ukraine en masse). Suppression reflects the coercive infrastructure maintaining the constraint. Theater ratio (0.55): Moderate, indicating that the attritional doctrine contains both real functional extraction (genuine military effects on enemy forces) and performative elements (maintenance of Cold War doctrine despite tactical ineffectiveness). The ratio has increased from 0.40 to 0.55, suggesting that as the doctrine fails to produce major breakthroughs, its performance component has grown — the 'meat grinder' is increasingly maintained as ritual rather than optimized tactic.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim is extreme. The Russian state apparatus experiences the constraint as pure coordination (Rope) — a mechanism for translating mass manpower into military effect. The conscripted soldier experiences it as pure extraction (Snare) — mandatory participation with zero agency or exit. The Ukrainian military experiences it as mixed (Tangled Rope) — they are constrained by resource scarcity and NATO supply dependencies but also coordinated through shared defense objectives. The Cold War doctrine inheritance perspective reveals the constraint as degraded ritual (Piton) — maintained through institutional momentum rather than effectiveness. The analytical observer sees the entire structure as a mutual snare: both sides are trapped by sunk costs, domestic political signaling requirements (neither can withdraw without accepting humiliation), and the absence of credible exit mechanisms. No perspective sees this as beneficial coordination or as an immutable natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The Russian military command and state apparatus derive d values near 0.0-0.15 (beneficiary + arbitrage exit), producing negative effective extraction chi — extraction flows toward them. Conscripted soldiers derive d values near 0.95-1.0 (victims + trapped exit), producing maximum f(d) and maximum experienced extraction. Ukrainian military experiences d around 0.65-0.75 (mixed victim and beneficiary + constrained exit), producing moderate-to-high experienced extraction despite some coordination benefits. The constraint's directionality is highly asymmetric: the primary beneficiary has low d, primary victims have extreme d. This asymmetry is the defining feature of the snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_ci_rope.
 *   MANDATROPHY RESOLVED: This constraint demonstrates the critical distinction between extraction and coercion on one axis and coordination function on the other. The attritional doctrine is NOT a coordination mechanism — it does not solve a collective action problem that benefits all participants. The Russian state benefits; Ukrainian and Russian conscripts bear costs. The doctrine persists because (a) the Russian state apparatus can enforce conscription and (b) Ukraine cannot exit the conflict without accepting territorial loss. This is pure snare: high extraction (0.78), high suppression (0.85), no coordination function for the majority of participants. The constraint could theoretically be reframed as 'coordination of military strategy' but this is false mandatrophy: the actual structure is extraction of human and material resources from trapped populations for the benefit of state apparatus. The theater ratio (0.55) prevents complete naturalization as immutable military law — the constraint is partly performative, sustained by institutional momentum rather than pure strategic necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    russian_mobilization_capacity_ceiling,
    'What is the sustainable upper limit of Russian mobilization and casualty replacement capacity before economic or social collapse?',
    'Longitudinal tracking of mobilization waves, casualty rates, demographic data, and economic capacity indices. Comparison with historical parallels (WWI Russian collapse, Soviet-Afghan war fatigue).',
    'If ceiling < 1.5M total mobilized: constraint becomes unsustainable within 18-24 months. If ceiling > 3M: constraint could persist 5+ years. This determines whether the snare is temporary or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(russian_mobilization_capacity_ceiling, empirical, 'Maximum sustainable Russian mobilization and casualty replacement').

omega_variable(
    ukrainian_nato_support_persistence,
    'Will NATO military support to Ukraine persist at sufficient levels to sustain Ukrainian resistance, or will domestic political shifts reduce support?',
    'Monitoring NATO member parliamentary votes, arms shipment levels, declared commitment timelines. Assessment of U.S. domestic political constraints on Ukraine aid.',
    'If support drops below critical threshold: Ukraine cannot sustain organized defense, snare deepens. If support sustains: constraint remains structured but with Ukrainian agency (tangled_rope rather than pure snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ukrainian_nato_support_persistence, preference, 'Persistence of NATO military support to Ukraine').

omega_variable(
    attritional_doctrine_replacement_viability,
    'Can Russian military command adopt higher-skill, lower-manpower tactics, or is institutional rigidity preventing doctrine reform?',
    'Analysis of Russian military leadership statements, tactical evolution in theater, investment in professional NCO corps and advanced equipment. Institutional assessment of reform barriers.',
    'If reform possible: attritional constraint could be replaced with lower-extraction coordination mechanism. If institutional rigidity is binding: attritional snare persists independent of military rationality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attritional_doctrine_replacement_viability, conceptual, 'Whether Russian military can adopt higher-skill, lower-manpower tactics').

omega_variable(
    extract_flow_reversibility,
    'Is the extraction mechanism (conscription, casualty tolerance, economic subordination of civilian economy) reversible without major institutional collapse?',
    'Historical analysis of how attritional wars terminate. Assessment of reversibility of conscription apparatus, casualty normalization, and economic war-footing.',
    'If reversible: constraint could be terminated through negotiated settlement or military stalemate. If irreversible: structure locks participants into continued grinding despite mutual destruction costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extract_flow_reversibility, conceptual, 'Reversibility of attritional extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attritional_warfare_doctrine_ru_ua_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attrit_tr_t0, attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(attrit_tr_t6, attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 6, 0.48).
narrative_ontology:measurement(attrit_tr_t12, attritional_warfare_doctrine_ru_ua_2026, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(attrit_be_t0, attritional_warfare_doctrine_ru_ua_2026, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(attrit_be_t6, attritional_warfare_doctrine_ru_ua_2026, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(attrit_be_t12, attritional_warfare_doctrine_ru_ua_2026, base_extractiveness, 12, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attritional_warfare_doctrine_ru_ua_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(attritional_warfare_doctrine_ru_ua_2026, ukrainian_conscription_sustainability).
narrative_ontology:affects_constraint(attritional_warfare_doctrine_ru_ua_2026, russian_economic_war_footing).
narrative_ontology:affects_constraint(attritional_warfare_doctrine_ru_ua_2026, nato_arms_supply_bottleneck).

% DUAL FORMULATION NOTE:
% Attritional warfare doctrine represents a structural constraint on military strategy with immediate effects on conscript populations. Upstream constraints include the doctrine inheritance from Cold War military education systems; downstream constraints include the sustainability of Ukrainian and Russian mobilization capacity and the NATO support structure enabling Ukrainian resistance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
