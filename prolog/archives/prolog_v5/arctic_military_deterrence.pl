% ============================================================================
% CONSTRAINT STORY: arctic_military_deterrence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_military_deterrence, []).

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
 *   constraint_id: arctic_military_deterrence
 *   human_readable: Arctic Military Deterrence Regime
 *   domain: geopolitical/military_coordination
 *
 * SUMMARY:
 *   Arctic military deterrence represents a geopolitical constraint that
 *   simultaneously enables regional stability and extracts from those with no
 *   power to negotiate the bargain. Since the post-Cold War period, Arctic
 *   access and resource competition have created incentives for major powers
 *   (NATO and Russia) to maintain military deterrence frameworks that prevent
 *   major-power conflict while militarizing the region and imposing costs on
 *   smaller states and indigenous populations. The constraint exhibits
 *   characteristics of pure coordination (deterrence prevents catastrophic
 *   war), pure extraction (indigenous populations bear costs without
 *   consent), and institutional inertia (Cold War doctrines persist beyond
 *   their original justification). The extractiveness metric (0.58) reflects
 *   that the regime is neither a stable coordination mechanism nor simple
 *   exploitation — it is an enforced equilibrium whose legitimacy rests on
 *   shared major-power interest in stability, not on consent from affected
 *   populations.
 *
 * KEY AGENTS:
 *   - NATO Militaries: Primary beneficiary (institutional/arbitrage) — maintains Arctic strategic position, sea lane access, and forward deterrence posture without major-power conflict
 *   - Russian Military: Co-beneficiary (institutional/arbitrage) — reciprocal deterrence partner; maintains Arctic sovereignty and military positioning
 *   - Arctic Indigenous Populations: Primary victim (powerless/trapped) — bears militarization costs, environmental degradation, sovereignty limitations with no exit option
 *   - Smaller Arctic States (Canada, Norway, Denmark/Greenland): Secondary victim (moderate/constrained) — benefit from deterrence preventing major-war spillover but constrained by great-power military presence and limited sovereignty in their own waters
 *   - Arctic Environmental Systems: Structural victim (powerless/trapped) — military activity degrades ecosystems, climate impact amplified by Arctic processes, no voice in deterrence bargaining
 *   - Arctic Council and Environmental Coalitions: Organized agent (organized/mobile) — advocates for alternative frameworks based on climate cooperation and resource transition
 *   - Cold War Strategic Doctrine Institutions: Institutional actor maintaining its own continuity (institutional/arbitrage) — perpetuates deterrence structures through organizational inertia and doctrinal justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_military_deterrence, 0.58).
domain_priors:suppression_score(arctic_military_deterrence, 0.68).
domain_priors:theater_ratio(arctic_military_deterrence, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_military_deterrence, extractiveness, 0.58).
narrative_ontology:constraint_metric(arctic_military_deterrence, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(arctic_military_deterrence, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_military_deterrence, tangled_rope).
narrative_ontology:human_readable(arctic_military_deterrence, "Arctic Military Deterrence Regime").
narrative_ontology:topic_domain(arctic_military_deterrence, "geopolitical/military_coordination").

domain_priors:requires_active_enforcement(arctic_military_deterrence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_military_deterrence, nato_militaries).
narrative_ontology:constraint_beneficiary(arctic_military_deterrence, russian_military).
narrative_ontology:constraint_beneficiary(arctic_military_deterrence, stable_arctic_access).
narrative_ontology:constraint_victim(arctic_military_deterrence, arctic_indigenous_populations).
narrative_ontology:constraint_victim(arctic_military_deterrence, arctic_environmental_stability).
narrative_ontology:constraint_victim(arctic_military_deterrence, smaller_arctic_states).
narrative_ontology:constraint_victim(arctic_military_deterrence, regional_economic_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARCTIC INDIGENOUS POPULATIONS (SNARE) — Cannot exit the military deterrence structure; forced to live under escalating armament, militarization of traditional lands, and environmental degradation from military activity. Bears extraction costs with no voice in deterrence bargaining. Trapped by geography and colonial legacy.
constraint_indexing:constraint_classification(arctic_military_deterrence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALLER ARCTIC STATES (TANGLED ROPE) — Experience both coordination benefit (deterrence prevents large-power conflict that would devastate region) and extraction cost (military presence, resource control, sovereignty limitations). Constrained exit — cannot unilaterally withdraw from deterrence without strategic vulnerability. Mixed experience of genuine coordination need and asymmetric extraction by major powers.
constraint_indexing:constraint_classification(arctic_military_deterrence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NATO MILITARY ESTABLISHMENTS (ROPE) — Primary beneficiary experiencing constraint as coordination mechanism. Deterrence framework enables NATO to maintain Arctic posture, access, and strategic positioning without direct large-scale conflict. Arbitrage exit available — NATO can adjust force posture dynamically. Net benefit through deterrence credibility.
constraint_indexing:constraint_classification(arctic_military_deterrence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RUSSIAN MILITARY ESTABLISHMENTS (ROPE) — Co-beneficiary of deterrence regime. Reciprocal arrangement: Russia maintains Arctic military position, NATO respects threshold, both benefit from predictability and avoided major conflict. Arbitrage exit available — Russia can adjust force posture. Co-creators of the constraint itself.
constraint_indexing:constraint_classification(arctic_military_deterrence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ARCTIC COUNCIL AND ENVIRONMENTAL MOVEMENTS (SCAFFOLD) — Organized agents perceiving the deterrence constraint as temporary, with potential sunset through decarbonization, climate adaptation, and shifting geopolitical incentives. Mobile exit through alternative frameworks (climate cooperation, resource economics shifting toward renewable energy). Deterrence seen as phase that can be transcended rather than permanent feature.
constraint_indexing:constraint_classification(arctic_military_deterrence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR STRATEGIC DOCTRINE LEGACY (PITON) — The deterrence framework derives much of its institutional maintenance from inherited Cold War military structures, doctrines, and organizational inertia. Theater ratio (0.55) reflects that significant energy goes into maintaining strategic narratives, military ritual, and doctrinal justification rather than direct tactical necessity. The constraint persists partly through institutional momentum — military establishments maintain Arctic postures because the systems exist, not because current threat calculations strictly require them.
constraint_indexing:constraint_classification(arctic_military_deterrence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The deterrence regime simultaneously solves a genuine coordination problem (preventing major-power conflict) and extracts from those with no power to participate in the bargain (indigenous populations, smaller states, future generations bearing climate costs). Both dimensions are structural, not contingent. The constraint is neither pure extraction nor pure coordination — it is their synthesis.
constraint_indexing:constraint_classification(arctic_military_deterrence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_military_deterrence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_military_deterrence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_military_deterrence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_military_deterrence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arctic_military_deterrence, TR),
    TR >= 0.70.

:- end_tests(arctic_military_deterrence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The regime enforces a major-power settlement that prevents large-scale conflict but concentrates costs on actors without bargaining power. Measurement interval shows rising extractiveness from 0.32 (immediate post-Cold War, deterrence seen as stabilizing) to 0.58 (present, as environmental costs mount and indigenous populations formalize sovereignty claims). The increase reflects not higher explicit extraction but surfacing of previously externalized costs. Suppression (0.68): High but incomplete. Smaller states can voice objections through Arctic Council; indigenous populations organize and litigate; environmental groups mobilize. But all face structural barriers: military establishments control key information, strategic doctrine is treated as technical/apolitical, and exit costs for any actor are prohibitive. Arctic Council has no enforcement power over military matters. Theater ratio (0.55): Moderate. Significant portion of deterrence activity is genuine tactical necessity (submarine patrols, monitoring, logistics), but institutional component is substantial — military establishments maintain deterrence postures partly through doctrine continuity, strategic narratives, and organizational self-preservation rather than current threat calculation. Theater has increased over the interval as Cold War strategic rationales have become less credible; institutional maintenance now explains more of the constraint's persistence.
 *
 * PERSPECTIVAL GAP:
 *   NATO and Russia experience the same constraint as Rope (coordination enabling stability). Indigenous populations experience it as Snare (extraction without benefit). Smaller states experience it as Tangled Rope (mixed coordination and extraction). The gap reveals the constraint's fundamental structure: major-power benefit from deterrence is real and reciprocal, but the constraint persists through the willing cooperation of major powers who benefit, not through consent or participation from those who bear costs. The regime's legitimacy rests entirely on major-power satisfaction with the bargain — if either major power decided deterrence was no longer stabilizing, the constraint would collapse, but indigenous populations would have no power to force that reckoning.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives, reflecting power asymmetry. NATO and Russian institutional beneficiaries have low d (~0.15-0.20) — they benefit from the constraint, have arbitrage exit options (can adjust posture unilaterally), and drive the constraint's persistence. Smaller states have moderate d (~0.55-0.65) — they experience both benefit (deterrence prevents major-war spillover) and cost (sovereignty limitations, military presence), with constrained exit. Indigenous populations have maximum d (~0.95) — they are victims with no benefits and no exit, structurally trapped. The chi formula shows that indigenous experienced extractiveness is highest (high d + high ε + high suppression = very high χ), while NATO experienced extractiveness is negative or minimal (low d, low f(d), high σ(S) dampened by arbitrage exit). The constraint maintains itself because major powers have structurally low experienced extraction and credible exit options, creating no incentive to change it.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint simultaneously solves a real coordination problem and creates structural extraction. Deterrence prevents major-power war (genuine coordination benefit) but does so by imposing costs on third parties without their consent (extractive mechanism). The regime is neither 'really just' coordination hidden by hostile framing nor 'really just' extraction hidden by stability rhetoric — it is a hybrid whose persistence depends on the coordination benefit being large enough to motivate major powers to maintain it despite extractive effects on third parties. If deterrence credibility degraded (if major powers lost confidence in mutual deterrence), the regime would shift toward pure snare (coercive maintenance without coordination). If Arctic geopolitics shifted to reduce major-power tensions (through climate transition or resource economics change), the regime would shift toward piton (institutional maintenance without either coordination or extraction function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_credibility_asymmetry,
    'Does the deterrence constraint maintain stability because threat levels are truly symmetrical or because each side rationally fears the asymmetries they cannot verify?',
    'Declassified military assessments; analysis of force asymmetries (nuclear vs conventional, defense vs power-projection, Arctic-specific vs global); comparison of official deterrence rhetoric with internal strategic assessments',
    'If symmetric: deterrence is genuine pure coordination (upgrade perspectives to Rope). If asymmetric: deterrence is posturing obscuring dominance seeking (classify as Snare from weaker party''s perspective). Current classification assumes partial information asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_credibility_asymmetry, empirical, 'Whether deterrence credibility is based on symmetrical threat assessment or mutual uncertainty').

omega_variable(
    indigenous_land_sovereignty_incommensurability,
    'Can indigenous population interests in territorial sovereignty and environmental integrity be reconciled with NATO/Russian deterrence requirements, or are they fundamentally incommensurable?',
    'Historical analysis of indigenous consent and participation in Arctic governance; mapping of military activity footprints against indigenous territory claims; assessment of whether alternative deterrence architectures (non-Arctic-based, naval-only, etc.) could reduce indigenous impact',
    'If reconcilable: the snare classification is too severe; reclassify indigenous perspective as constrained tangled rope. If incommensurable: indigenous populations are not secondary victims but structurally excluded from the constraint''s bargaining structure — the regime is fundamentally extractive toward them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_land_sovereignty_incommensurability, conceptual, 'Whether indigenous interests are reconcilable with deterrence requirements').

omega_variable(
    climate_transition_sunset_feasibility,
    'Will decarbonization and climate adaptation shift Arctic geopolitical incentives enough to enable deterrence sunset, or will resource competition intensify deterrence requirements?',
    'Modeling of Arctic economic transitions (shipping, fishing, resource extraction); analysis of whether renewable energy dominance reduces strategic resource competition; assessment of whether climate refuge migration creates new tensions',
    'If sunset feasible: scaffold perspective is confirmed — deterrence is temporary. If not: deterrence persists and becomes piton (institutional inertia) rather than temporary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_transition_sunset_feasibility, empirical, 'Whether climate transition will enable deterrence regime sunset').

omega_variable(
    extractiveness_measurement_sensitivity,
    'Does extractiveness (0.58) measure the immediate military/diplomatic burden or the intergenerational environmental and sovereignty costs? How much of the measured extraction reflects present-day costs vs. deferred future burden?',
    'Comparative analysis of immediate deterrence costs (military spending, sovereignty limitations) vs. intergenerational costs (environmental damage, Arctic ecosystem degradation, indigenous culture impact); discounting framework for future generations'' losses',
    'If measurement includes intergenerational costs: extractiveness should rise to 0.70+, triggering mandatrophy resolution requirement. If measurement is present-only: extractiveness remains ~0.58 but mask is placed on future analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_measurement_sensitivity, conceptual, 'Whether extractiveness measurement includes intergenerational costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_military_deterrence, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arct_tr_t0, arctic_military_deterrence, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arct_tr_t15, arctic_military_deterrence, theater_ratio, 15, 0.48).
narrative_ontology:measurement(arct_tr_t30, arctic_military_deterrence, theater_ratio, 30, 0.55).
narrative_ontology:measurement(arct_tr_t45, arctic_military_deterrence, theater_ratio, 45, 0.61).

% Extraction over time
narrative_ontology:measurement(arct_be_t0, arctic_military_deterrence, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(arct_be_t15, arctic_military_deterrence, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(arct_be_t30, arctic_military_deterrence, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(arct_be_t45, arctic_military_deterrence, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_military_deterrence, enforcement_mechanism).
narrative_ontology:affects_constraint(arctic_military_deterrence, arctic_resource_extraction_conflict).
narrative_ontology:affects_constraint(arctic_military_deterrence, northwest_passage_sovereignty).
narrative_ontology:affects_constraint(arctic_military_deterrence, arctic_environmental_degradation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arctic_military_deterrence, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
