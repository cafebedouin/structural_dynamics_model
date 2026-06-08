% ============================================================================
% CONSTRAINT STORY: iron_wall_strategy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iron_wall_strategy, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: iron_wall_strategy
 *   human_readable: Iron Wall Strategy: Military Supremacy as Prerequisite for Political Settlement
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The Iron Wall strategy, articulated by Ze'ev Jabotinsky in 1923 and
 *   adopted by Labor Zionism after the 1936-39 Arab Revolt, established
 *   military supremacy as the explicit prerequisite for any political
 *   settlement with the Arab population. The doctrine's core logic: only when
 *   the Arabs despair of defeating the Yishuv militarily will they accept its
 *   political existence. This constraint exhibits the full range of DR
 *   classification depending on structural position. Zionist military
 *   institutions experience it as coordination (Rope) — it solves the genuine
 *   strategic problem of state-building against regional opposition.
 *   Palestinian Arabs experience it as extraction (Snare) — their despair is
 *   the doctrine's stated goal. Regional Arab states experience it as hybrid
 *   (Tangled Rope) — both coordination (clear rules of engagement) and
 *   extraction (permanent inferiority as basis for settlement). Early peace
 *   movements saw it as temporary (Scaffold) with an expected sunset that
 *   never arrived. Oslo-era peace process institutions maintained it as
 *   degraded performance (Piton) — the military asymmetry persisted ritually
 *   while the functional link to negotiated outcomes atrophied. The
 *   analytical observer sees structural fusion of coordination and
 *   extraction: the doctrine solved a real problem through a mechanism that
 *   deliberately induces despair in the target population. Theater ratio
 *   (0.35) reflects that the doctrine retained substantial functional content
 *   even as peace process rhetoric increased — military expenditure and
 *   settlement expansion continued regardless of negotiation status, but the
 *   performance element grew during Oslo as the gap between stated peace aims
 *   and maintained military supremacy widened.
 *
 * KEY AGENTS:
 *   - Zionist Military Institutions (Haganah, later IDF): Primary beneficiary (institutional/arbitrage) — doctrine coordinates resource allocation and strategic planning; captures state-building success
 *   - Jewish Settlement Enterprise: Primary beneficiary (institutional/arbitrage) — military supremacy enables territorial expansion and demographic engineering
 *   - Labor Zionist Leadership: Primary beneficiary (institutional/arbitrage) — adopted doctrine post-1936 despite ideological opposition to Jabotinsky; institutional interests aligned with military-first strategy
 *   - Palestinian Arab Population: Primary victim (powerless/trapped) — doctrine explicitly targets their despair as prerequisite for political recognition; no exit from military asymmetry
 *   - Regional Arab States: Secondary victim (moderate/constrained) — military inferiority maintained as basis for any settlement; some diplomatic leverage but constrained by asymmetry
 *   - Peace Advocacy Movements: Secondary victim (organized/mobile early period, then constrained) — Brit Shalom, Ihud, and later peace groups marginalized by doctrine's success; expected sunset that never arrived
 *   - Peace Process Institutions (Oslo era): Institutional actor (institutional/constrained) — maintained military asymmetry ritually while claiming to pursue settlement; piton perspective reflects degraded function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iron_wall_strategy, 0.78).
domain_priors:suppression_score(iron_wall_strategy, 0.82).
domain_priors:theater_ratio(iron_wall_strategy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iron_wall_strategy, extractiveness, 0.78).
narrative_ontology:constraint_metric(iron_wall_strategy, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(iron_wall_strategy, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iron_wall_strategy, tangled_rope).
narrative_ontology:human_readable(iron_wall_strategy, "Iron Wall Strategy: Military Supremacy as Prerequisite for Political Settlement").
narrative_ontology:topic_domain(iron_wall_strategy, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(iron_wall_strategy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(iron_wall_strategy, '25be5d7b-ace3-4711-8d1f-160960f8b70d').
narrative_ontology:cs_kernel_codification('25be5d7b-ace3-4711-8d1f-160960f8b70d', formalized).
narrative_ontology:cs_authority_grounding('25be5d7b-ace3-4711-8d1f-160960f8b70d', lineage).
narrative_ontology:cs_interpretation_layer_present('25be5d7b-ace3-4711-8d1f-160960f8b70d').
narrative_ontology:cs_reading_relation('25be5d7b-ace3-4711-8d1f-160960f8b70d', iron_wall_strategy__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('25be5d7b-ace3-4711-8d1f-160960f8b70d', iron_wall_strategy__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('25be5d7b-ace3-4711-8d1f-160960f8b70d', foundational, persecution_justifies_displacement).
narrative_ontology:cs_axiom_status(persecution_justifies_displacement, holdable).
narrative_ontology:cs_axiom_grounding('25be5d7b-ace3-4711-8d1f-160960f8b70d', persecution_justifies_displacement, deontological).
narrative_ontology:cs_axiom('25be5d7b-ace3-4711-8d1f-160960f8b70d', foundational, historical_connection_grounds_sovereignty).
narrative_ontology:cs_axiom_status(historical_connection_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('25be5d7b-ace3-4711-8d1f-160960f8b70d', historical_connection_grounds_sovereignty, conventional).
narrative_ontology:cs_axiom('25be5d7b-ace3-4711-8d1f-160960f8b70d', secondary, arab_opposition_illegitimate).
narrative_ontology:cs_axiom_status(arab_opposition_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('25be5d7b-ace3-4711-8d1f-160960f8b70d', arab_opposition_illegitimate, deontological).
narrative_ontology:cs_reference_frame('25be5d7b-ace3-4711-8d1f-160960f8b70d', defensive_necessity_framework).
narrative_ontology:cs_drift_state('25be5d7b-ace3-4711-8d1f-160960f8b70d', post_1967_territorial_maximalism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25be5d7b-ace3-4711-8d1f-160960f8b70d', '2026-06-06T03:36:37.624676+00:00').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iron_wall_strategy, zionist_military_institutions).
narrative_ontology:constraint_beneficiary(iron_wall_strategy, jewish_settlement_enterprise).
narrative_ontology:constraint_beneficiary(iron_wall_strategy, labor_zionist_leadership).
narrative_ontology:constraint_victim(iron_wall_strategy, palestinian_arab_population).
narrative_ontology:constraint_victim(iron_wall_strategy, regional_arab_states).
narrative_ontology:constraint_victim(iron_wall_strategy, peace_advocacy_movements).
narrative_ontology:constraint_vindicates(iron_wall_strategy, force_precedes_negotiation_doctrine).
narrative_ontology:constraint_vindicates(iron_wall_strategy, despair_inducement_theory).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN ARAB POPULATION (SNARE) — Trapped within a doctrine that makes their despair the explicit prerequisite for political recognition. No exit from the military asymmetry; suppression increases with each phase of Haganah development. The constraint extracts land, autonomy, and political agency while presenting the extraction as necessary preparation for eventual negotiation. Maximum experienced extraction — the doctrine's stated goal is to induce hopelessness as the condition for acceptance.
constraint_indexing:constraint_classification(iron_wall_strategy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL ARAB STATES (TANGLED ROPE) — Constrained by the military asymmetry but not entirely powerless; possess some diplomatic leverage and coalition capacity. Experience both coordination (the doctrine creates clear rules of engagement and predictable Israeli behavior) and extraction (military inferiority is deliberately maintained as the basis for any future settlement). Can exit through acceptance of Israeli terms but at severe cost to regional standing and Palestinian solidarity obligations.
constraint_indexing:constraint_classification(iron_wall_strategy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ZIONIST MILITARY INSTITUTIONS (ROPE) — Primary beneficiaries. The doctrine coordinates settlement defense, resource allocation, and strategic planning around a clear principle: build overwhelming force first, negotiate from strength later. Experiences the constraint as pure coordination — it solves the genuine problem of how to establish a state against regional opposition. Net beneficiary with arbitrage-level exit options (could pivot to alternative strategies but chooses not to because this one succeeds).
constraint_indexing:constraint_classification(iron_wall_strategy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PEACE ADVOCACY MOVEMENTS / EARLY PERIOD (SCAFFOLD) — Organized groups (Brit Shalom, Ihud, early peace advocates) saw the Iron Wall as a temporary necessity that would sunset once military security was established. Expected the doctrine to transition from military supremacy to genuine negotiation within a generation. This perspective held the constraint as transitional coordination with a built-in exit: once the 'wall' was established, political settlement would follow. The sunset never arrived — the doctrine became permanent policy.
constraint_indexing:constraint_classification(iron_wall_strategy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: PEACE PROCESS INSTITUTIONS / OSLO ERA (PITON) — By the 1990s, the Iron Wall doctrine had become largely theatrical within the peace process framework. Negotiations proceeded while military supremacy was maintained, but the doctrine's original logic (despair precedes acceptance) had atrophied into performance. The peace process institutions maintained the military asymmetry ritually while claiming to pursue political settlement, but the functional link between military pressure and negotiated outcomes had degraded. What remained was institutional inertia and the performance of 'negotiating from strength' without the doctrine's original clarity about what that strength was meant to achieve.
constraint_indexing:constraint_classification(iron_wall_strategy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the Iron Wall doctrine exhibits both genuine coordination (it solved the Yishuv's strategic problem of how to establish a state against regional opposition) and substantial extraction (it deliberately induces despair in the Palestinian population as a prerequisite for their political recognition). The doctrine's extractiveness is not incidental to its coordination function — the two are structurally fused. The analytical observer sees the constraint as a hybrid: a real strategic solution that operates through asymmetric extraction, maintained by active enforcement (military expenditure, settlement expansion, diplomatic isolation of alternatives).
constraint_indexing:constraint_classification(iron_wall_strategy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iron_wall_strategy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iron_wall_strategy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iron_wall_strategy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iron_wall_strategy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iron_wall_strategy, TR),
    TR >= 0.70.

:- end_tests(iron_wall_strategy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The doctrine deliberately induces Palestinian despair as its stated mechanism. Military supremacy is not incidental to political aims but constitutive — the extraction (land, autonomy, political agency) is the means by which the coordination function (state-building) operates. The value reflects that extraction is structural rather than incidental, but not total (some negotiation has occurred, some Palestinian agency persists). Suppression (0.82): Very high. The doctrine suppresses alternatives by design: binational proposals, immediate negotiation, territorial compromise are all foreclosed until military supremacy is absolute. Suppression increased dramatically from 1920 (0.45) through 1967 (0.88) as military capacity grew and alternative strategies were marginalized. Modest decline post-1967 reflects some diplomatic engagement, but suppression remains very high. Theater ratio (0.35): Moderate. The doctrine retained substantial functional content throughout — military expenditure and settlement expansion were not performative. However, theater increased during Oslo (0.48 in 1993) as peace process rhetoric diverged from maintained military asymmetry. The 2020 value (0.35) reflects return to more explicit military-first policy with reduced peace process performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon appears as six different types depending on observer position. Zionist military institutions see pure coordination (Rope) — the doctrine solves their strategic problem. Palestinian Arabs see pure extraction (Snare) — their despair is the explicit goal. Regional Arab states see hybrid (Tangled Rope) — both coordination (clear rules) and extraction (permanent inferiority). Early peace movements saw temporary coordination (Scaffold) with expected sunset. Oslo-era institutions see degraded performance (Piton) — ritual maintenance without functional link to outcomes. The analytical observer sees structural fusion (Tangled Rope) — coordination and extraction are inseparable. The gap is not about who is correct but about which structural position you measure from. The doctrine IS coordination from the beneficiary's seat and IS extraction from the victim's seat. Both are true simultaneously because the constraint operates through their fusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Zionist military institutions are primary beneficiaries with arbitrage exit — they experience low or negative effective extraction (the constraint subsidizes them). Palestinian Arabs are primary victims with trapped exit — they experience maximum effective extraction (the doctrine's stated goal is their despair). Regional Arab states are secondary victims with constrained exit — they experience high but not maximal extraction (some diplomatic leverage exists). Peace advocacy movements shifted from organized/mobile (early period, expected sunset) to organized/constrained (post-1948, marginalized but not eliminated). Peace process institutions are institutional/constrained (maintain asymmetry ritually but cannot exit without abandoning institutional role). The analytical observer sees the structural fusion: coordination and extraction are not separable — the doctrine solves the Yishuv's strategic problem through a mechanism that extracts from Palestinians.
 *
 * MANDATROPHY ANALYSIS:
 *   The Iron Wall doctrine resolves the mandatrophy by demonstrating that Tangled Rope is not a compromise classification but a structural reality: some constraints coordinate and extract through the same mechanism, and the two functions cannot be separated without destroying the constraint. The doctrine genuinely solved the Yishuv's strategic problem (coordination function is real) and genuinely induces Palestinian despair as its stated mechanism (extraction function is real). Attempts to separate them fail: remove the extraction (stop inducing despair) and the coordination collapses (no basis for military supremacy); remove the coordination (abandon state-building) and the extraction becomes purposeless. The constraint's persistence depends on maintaining both functions simultaneously. This is not a bug in the classification system but a feature of the constraint itself — it is structurally hybrid, and any single-type classification from a single perspective would miss the fusion that defines it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    despair_threshold_ambiguity,
    'What level of Palestinian despair constitutes the ''acceptance'' threshold the doctrine aims to induce? Is the threshold empirically specifiable or does it shift to justify indefinite military supremacy?',
    'Historical analysis of Israeli negotiation behavior: does military advantage correlate with genuine concessions, or does the threshold for ''sufficient despair'' rise with each increase in military capacity? Comparison of settlement patterns and military expenditure during negotiation periods vs conflict periods.',
    'If threshold is fixed and empirically reachable: doctrine is scaffold (temporary coordination with sunset). If threshold shifts to maintain permanent asymmetry: doctrine is snare (extraction mechanism with coordination cover story).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(despair_threshold_ambiguity, empirical, 'Whether the despair threshold is fixed or indefinitely deferred').

omega_variable(
    jabotinsky_labor_adoption_mechanism,
    'Did Labor Zionism adopt the Iron Wall doctrine post-1936 because it proved strategically correct (coordination logic) or because it served institutional interests in maintaining military budgets and settlement expansion (extraction logic)?',
    'Archival analysis of internal Labor Zionist debates 1936-1948; comparison of stated rationales vs resource allocation patterns; examination of whether adoption followed military necessity or preceded it.',
    'If adoption followed demonstrated necessity: coordination function is primary. If adoption preceded necessity and shaped resource allocation to justify itself: extraction function is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jabotinsky_labor_adoption_mechanism, empirical, 'Whether Labor Zionist adoption was strategic or institutional').

omega_variable(
    alternative_strategy_suppression,
    'Were alternative strategies (binational state proposals, immediate negotiation, territorial compromise) genuinely tested and found wanting, or were they suppressed before they could demonstrate viability?',
    'Historical analysis of Brit Shalom, Ihud, and other peace advocacy movements: were they marginalized through demonstrated failure or through institutional exclusion? Examination of whether military strategy shaped political possibilities or vice versa.',
    'If alternatives were tested and failed: Iron Wall is coordination (solved a problem alternatives couldn''t). If alternatives were suppressed without testing: Iron Wall is extraction (maintained through suppression of exits).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_strategy_suppression, empirical, 'Whether alternative strategies were tested or suppressed').

omega_variable(
    reading_under_determination,
    'Does the national_liberation_reading''s framing of the Iron Wall as defensive necessity versus the settler_colonial_reading''s framing as offensive displacement strategy represent two coherent interpretations of the same historical record, or does one reading suppress evidence the other requires?',
    'Comparative analysis of land acquisition patterns, demographic engineering policies, and military expenditure allocation under the Iron Wall doctrine. Does the evidence support both ''defensive return to ancestral homeland'' and ''offensive displacement of indigenous population'' as equally coherent framings, or does one reading require ignoring structural features the other reading centers?',
    'If both readings are equally coherent given the evidence: the kernel is genuinely contested and readings coexist. If one reading requires suppressing evidence the other reading depends on: the readings are not symmetric — one is a cover story for the structure the other reveals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_under_determination, conceptual, 'Whether the national_liberation and settler_colonial readings are symmetric interpretations or asymmetric (one is cover story for the other''s revealed structure)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iron_wall_strategy, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iron_wall_theater_1920, iron_wall_strategy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(iron_wall_theater_1936, iron_wall_strategy, theater_ratio, 16, 0.18).
narrative_ontology:measurement(iron_wall_theater_1948, iron_wall_strategy, theater_ratio, 28, 0.22).
narrative_ontology:measurement(iron_wall_theater_1967, iron_wall_strategy, theater_ratio, 47, 0.28).
narrative_ontology:measurement(iron_wall_theater_1993, iron_wall_strategy, theater_ratio, 73, 0.48).
narrative_ontology:measurement(iron_wall_theater_2020, iron_wall_strategy, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(iron_wall_extract_1920, iron_wall_strategy, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(iron_wall_extract_1936, iron_wall_strategy, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(iron_wall_extract_1948, iron_wall_strategy, base_extractiveness, 28, 0.78).
narrative_ontology:measurement(iron_wall_extract_1967, iron_wall_strategy, base_extractiveness, 47, 0.82).
narrative_ontology:measurement(iron_wall_extract_1993, iron_wall_strategy, base_extractiveness, 73, 0.8).
narrative_ontology:measurement(iron_wall_extract_2020, iron_wall_strategy, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(iron_wall_suppress_1920, iron_wall_strategy, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(iron_wall_suppress_1936, iron_wall_strategy, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(iron_wall_suppress_1948, iron_wall_strategy, suppression_requirement, 28, 0.78).
narrative_ontology:measurement(iron_wall_suppress_1967, iron_wall_strategy, suppression_requirement, 47, 0.88).
narrative_ontology:measurement(iron_wall_suppress_1993, iron_wall_strategy, suppression_requirement, 73, 0.85).
narrative_ontology:measurement(iron_wall_suppress_2020, iron_wall_strategy, suppression_requirement, 100, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iron_wall_strategy, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The Iron Wall strategy is downstream of british_mandate_scaffolding (the British Mandate created the institutional framework within which Zionist military development occurred) but represents a distinct structural constraint with its own extractiveness profile. The Mandate's extractiveness reflected colonial administration overhead; the Iron Wall's extractiveness reflects the doctrine's deliberate induction of Palestinian despair as prerequisite for political settlement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
