% ============================================================================
% CONSTRAINT STORY: military_defeat_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_military_defeat_cascade, []).

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
 *   constraint_id: military_defeat_cascade
 *   human_readable: Military Defeat Cascade in Russo-Ukrainian War
 *   domain: political_economy/regime_stability/military_conflict
 *
 * SUMMARY:
 *   The military defeat cascade in the Russo-Ukrainian War describes the
 *   widening structural gap between the Kremlin's maximalist war aims (full
 *   Ukrainian capitulation, territorial recognition, regime change) and its
 *   observable military incapacity (unable to occupy Donbas after four years,
 *   losing territory, casualty-to-advance ratio tripled from 61 to 206
 *   casualties per square mile). This constraint exhibits multiple DR types
 *   from different structural positions: the Kremlin inner circle experiences
 *   it as coordination (consolidates domestic control), conscripts experience
 *   it as pure extraction (trapped, bearing tripled casualty costs), the
 *   defense industrial complex experiences it as mixed
 *   coordination-extraction (genuine production plus rent extraction), and
 *   the analytical observer risks naturalizing it as immutable geopolitics
 *   (false summit). The constraint's theater_ratio (0.85) reflects that
 *   military planning has become substantially performative: General Staff
 *   briefs show imminent breakthrough despite observable incapacity
 *   (Ukrainian operations exceeded Russian in May 2026). The rising
 *   suppression_requirement (0.65 → 0.82) tracks enforcement intensification:
 *   mobilization law tightened, exit restrictions increased, domestic
 *   opposition suppressed as the capability gap widened. The constraint is
 *   downstream of manpower_exhaustion_trap (mountain) — the demographic and
 *   economic limits on Russian mobilization capacity are immutable, but the
 *   decision to continue the war despite those limits is a constructed policy
 *   choice.
 *
 * KEY AGENTS:
 *   - Russian Conscripts: Primary victim (powerless/trapped) — bear tripled casualty costs with no exit; extraction maximized by widening capability gap
 *   - Kremlin Inner Circle: Primary beneficiary (institutional/arbitrage) — consolidates domestic control, extracts resources, retains exit options (offshore assets, dual citizenship)
 *   - Defense Industrial Complex: Secondary beneficiary (moderate/constrained) — coordinates genuine military production while extracting rents through cost-plus contracts and import substitution theater
 *   - Occupied Population (Donbas): Secondary victim (powerless/identity_locked) — identity-locked by passportization, bears indefinite occupation costs with no coordination benefit
 *   - Nationalist Ideological Bloc: Mixed actor (organized/constrained) — coordinates genuine ideological commitment while extracting state funding and career advancement; constrained by inability to deliver promised victory
 *   - Russian General Staff: Institutional actor (institutional/constrained) — maintains performative operational planning despite observable incapacity (piton perspective)
 *   - Western Sanctions Coalition: Organized external actor (organized/mobile) — sees constraint as temporary (scaffold logic); assumes widening gap will force Russian concessions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choice as immutable geopolitics (false summit candidate)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(military_defeat_cascade, 0.68).
domain_priors:suppression_score(military_defeat_cascade, 0.82).
domain_priors:theater_ratio(military_defeat_cascade, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(military_defeat_cascade, extractiveness, 0.68).
narrative_ontology:constraint_metric(military_defeat_cascade, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(military_defeat_cascade, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(military_defeat_cascade, tangled_rope).
narrative_ontology:human_readable(military_defeat_cascade, "Military Defeat Cascade in Russo-Ukrainian War").
narrative_ontology:topic_domain(military_defeat_cascade, "political_economy/regime_stability/military_conflict").

domain_priors:requires_active_enforcement(military_defeat_cascade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(military_defeat_cascade, kremlin_inner_circle).
narrative_ontology:constraint_beneficiary(military_defeat_cascade, defense_industrial_complex).
narrative_ontology:constraint_beneficiary(military_defeat_cascade, nationalist_ideological_bloc).
narrative_ontology:constraint_victim(military_defeat_cascade, russian_conscripts).
narrative_ontology:constraint_victim(military_defeat_cascade, occupied_population).
narrative_ontology:constraint_victim(military_defeat_cascade, russian_civil_society).
narrative_ontology:constraint_victim(military_defeat_cascade, ukrainian_civilians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RUSSIAN CONSCRIPT (SNARE) — Trapped by mobilization law with no legal exit; faces immediate death or injury for territorial gains the state cannot hold. Casualty-to-advance ratio (206/sq mile vs 61 summer 2025) means extraction has tripled. Maximum experienced extraction — no agency, no exit, bears full cost of the widening capability gap.
constraint_indexing:constraint_classification(military_defeat_cascade, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEFENSE INDUSTRIAL MANAGER (TANGLED ROPE) — Constrained by sanctions and supply chain disruption but benefits from wartime production contracts and regime patronage. Coordinates genuine military-industrial output (ammunition, vehicles) while extracting rents through cost-plus contracts and import substitution theater. Mixed coordination-extraction: the war economy both enables and exploits this agent.
constraint_indexing:constraint_classification(military_defeat_cascade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KREMLIN INNER CIRCLE (ROPE) — Primary beneficiary with arbitrage-level exit (offshore assets, dual citizenship, exit visas). Experiences the constraint as coordination: the war narrative consolidates domestic control, suppresses opposition, and justifies resource extraction. Net beneficiary — the widening military gap is a problem for conscripts, not for the decision-making elite who can exit if the regime collapses.
constraint_indexing:constraint_classification(military_defeat_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OCCUPIED POPULATION (SNARE) — Identity-locked by enforced passportization and severed Ukrainian ties; cannot exit occupied territory without regime permission. Bears cost of military occupation (conscription, resource extraction, suppression of Ukrainian identity) with no coordination benefit. The widening gap means longer occupation with no resolution — extraction persists indefinitely.
constraint_indexing:constraint_classification(military_defeat_cascade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 5: NATIONALIST IDEOLOGICAL BLOC (TANGLED ROPE) — Organized agents (state media, patriotic organizations, Z-movement) coordinate genuine ideological commitment to territorial maximalism while extracting state funding and career advancement. Constrained by the widening capability gap (cannot deliver promised victory) but benefits from wartime mobilization of nationalist sentiment. Mixed: the ideology is both sincere coordination and extractive performance.
constraint_indexing:constraint_classification(military_defeat_cascade, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: RUSSIAN GENERAL STAFF (PITON) — Institutional actor maintaining performative operational planning (maps showing Ukrainian collapse, victory timelines) despite observable incapacity. The planning function has atrophied into theater: casualty-to-advance ratio tripled, Ukrainian operations exceed Russian (May 2026), yet staff briefs show imminent breakthrough. Piton classification derives from theater gate — the military planning apparatus persists through institutional inertia, not functional capacity.
constraint_indexing:constraint_classification(military_defeat_cascade, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALIST IR VIEW (MOUNTAIN) — From a civilizational/universal perspective, great power competition over buffer zones is an immutable feature of international relations: Russia's security concerns about NATO expansion are structural, and the war is an inevitable response to encirclement. This perspective naturalizes the conflict as a law of geopolitics. However, the structural data contradicts the mountain classification — the widening gap between aims and capacity, the rising casualty ratio, and the regime's inability to exit reveal this as a contingent policy choice, not a geopolitical necessity. The engine's false summit detector will identify this as naturalization of a constructed constraint.
constraint_indexing:constraint_classification(military_defeat_cascade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: WESTERN SANCTIONS COALITION (SCAFFOLD) — Organized agents (EU, US, allied states) see the constraint as temporary: sanctions and military aid are transitional mechanisms to impose costs until Russia accepts a negotiated settlement. Low effective extraction because the coalition has agency, mobile exit (can lift sanctions), and sees a sunset (war termination). The scaffold logic assumes the widening capability gap will force Russian concessions within a generational timeframe.
constraint_indexing:constraint_classification(military_defeat_cascade, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(military_defeat_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(military_defeat_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(military_defeat_cascade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(military_defeat_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(military_defeat_cascade, TR),
    TR >= 0.70.

:- end_tests(military_defeat_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The widening gap between maximalist aims and military capacity means conscripts bear tripled casualty costs (206 vs 61 casualties/sq mile) for territorial gains the state cannot hold. The Kremlin inner circle captures regime-stabilization benefits (domestic control, resource extraction, suppression of opposition) while retaining exit options. The defense industrial complex extracts rents through wartime contracts. Extraction has risen monotonically from 0.45 (Feb 2022 invasion) to 0.68 (June 2026) as the capability gap widened. Suppression (0.82): Very high. Mobilization law eliminates legal exit for conscripts; domestic opposition suppressed; occupied populations cannot leave without regime permission; exit restrictions tightened as the war continued. Suppression rose from 0.65 (initial invasion, partial mobilization) to 0.82 (full mobilization, exit restrictions, domestic crackdown) as the regime compensated for military incapacity with enforcement intensification. Theater ratio (0.85): Very high. Military planning is substantially performative: General Staff briefs show imminent Ukrainian collapse despite observable data (Ukrainian operations exceed Russian, casualty ratio tripled, territorial gains negligible). State media maintains victory narrative despite widening capability gap. Theater rose from 0.35 (early war, genuine operational planning) to 0.85 (mid-2025 onward, planning divorced from capacity) as the gap between aims and reality became undeniable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from different structural positions. The Kremlin inner circle sees coordination (Rope) — the war consolidates domestic control and justifies resource extraction; they are net beneficiaries with exit options. Russian conscripts see pure extraction (Snare) — trapped by mobilization law, bearing tripled casualty costs with no agency or exit. The defense industrial complex sees mixed coordination-extraction (Tangled Rope) — genuine military production plus rent extraction through cost-plus contracts. The occupied population sees pure extraction (Snare) — identity-locked by passportization, bearing indefinite occupation costs. The nationalist ideological bloc sees mixed coordination-extraction (Tangled Rope) — genuine ideological commitment plus state funding and career advancement. The General Staff sees degraded planning (Piton) — operational briefs are performative, divorced from capacity. The Western sanctions coalition sees a temporary problem (Scaffold) — assumes the widening gap will force Russian concessions within a generational timeframe. The analytical observer risks seeing immutable geopolitics (Mountain) — great power competition over buffer zones as a natural law — but the structural data (high extraction, high suppression, identifiable beneficiaries) reveals this as a false summit: the constraint is a constructed policy choice, not a geopolitical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from the agent's structural position. Kremlin inner circle: institutional power + arbitrage exit + primary beneficiary → d ≈ 0.1 (low, near full beneficiary). Defense industrial complex: moderate power + constrained exit + mixed beneficiary/victim → d ≈ 0.35 (moderate, mixed extraction). Nationalist ideological bloc: organized power + constrained exit + mixed beneficiary/victim → d ≈ 0.40 (moderate, mixed extraction). General Staff: institutional power + constrained exit + neither clear beneficiary nor victim → d ≈ 0.50 (symmetric, piton derives from theater gate not high chi). Western sanctions coalition: organized power + mobile exit + external actor (not directly targeted) → d ≈ 0.25 (low, scaffold logic). Russian conscripts: powerless + trapped exit + primary victim → d ≈ 0.95 (very high, near full target). Occupied population: powerless + identity_locked exit + primary victim → d ≈ 0.90 (very high, near full target). Analytical observer: analytical power + analytical exit → d ≈ 0.50 (symmetric, mountain is perspectival naturalization). The directionality spread (0.1 to 0.95) reflects the extreme asymmetry: elite beneficiaries with exit options experience the constraint as coordination or low extraction, while powerless victims with no exit bear maximum extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR FOR INTER-INSTITUTIONAL AND SAME-LEVEL DYNAMICS: This constraint resolves the mandatrophy by showing that the same structural phenomenon (widening gap between war aims and military capacity) produces different constraint types from different structural positions. The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?' The Kremlin inner circle's rope is their genuine experience (net beneficiary, exit options). The conscript's snare is their structural reality (trapped, maximum extraction). The defense industrial complex's tangled rope is their mixed experience (genuine coordination plus rent extraction). The General Staff's piton is their institutional reality (performative planning, atrophied function). The Western coalition's scaffold is their strategic logic (temporary mechanism with sunset). The analytical observer's mountain is a false summit (naturalization of contingent policy choice). No single type is 'the' answer — the presheaf over the observation site IS the answer. The constraint also demonstrates how inter-institutional dynamics (Kremlin vs General Staff, defense industry vs state) and same-level lateral dynamics (elite beneficiaries vs powerless victims at the same nominal national scope) produce perspectival gaps through differentiated exit options and beneficiary/victim status, not through adding axes to the tuple.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_collapse_threshold,
    'At what casualty-to-advance ratio or territorial loss rate does the constraint shift from regime-stabilizing (consolidates domestic control) to regime-threatening (triggers elite defection or mass unrest)?',
    'Historical analysis of authoritarian regime survival under military defeat; identification of tipping points in prior cases (USSR Afghanistan, Tsarist WWI, Argentine Falklands)',
    'If threshold already crossed: constraint is in terminal phase, regime collapse imminent. If threshold distant: constraint persists as tangled rope for years. If threshold nonexistent: regime can sustain indefinite extraction (pure snare from all non-elite perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_collapse_threshold, empirical, 'Casualty threshold for regime-threatening vs regime-stabilizing dynamics').

omega_variable(
    maximalist_aims_sincerity,
    'Are the Kremlin''s stated maximalist aims (full Ukrainian capitulation, regime change, territorial recognition) genuine strategic objectives or negotiating theater to anchor expectations for a more limited settlement?',
    'Declassified internal planning documents; post-conflict memoirs; revealed preference analysis (resource allocation to occupation infrastructure vs mobile offensive capability)',
    'If genuine: widening gap is catastrophic failure, regime cannot exit without existential loss. If theater: widening gap is manageable — regime can declare victory at any territorial line and exit. Determines whether the constraint has a sunset or persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maximalist_aims_sincerity, conceptual, 'Whether maximalist aims are strategic objectives or negotiating theater').

omega_variable(
    ukrainian_operations_sustainability,
    'Can Ukraine sustain monthly operations exceeding Russia''s (May 2026 data point) or is this a temporary capability spike dependent on Western aid flows?',
    'Longitudinal tracking of Ukrainian vs Russian operations count; correlation with Western aid delivery timelines; assessment of Ukrainian domestic production capacity vs import dependency',
    'If sustainable: the capability gap widens further, increasing extraction on Russian conscripts and occupied populations. If temporary: the gap stabilizes or narrows, reducing extraction and potentially enabling Russian territorial consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ukrainian_operations_sustainability, empirical, 'Sustainability of Ukrainian operational tempo advantage').

omega_variable(
    false_summit_naturalization,
    'Is the analytical observer''s mountain classification (great power competition as immutable geopolitics) a genuine natural law or a false summit that naturalizes contingent policy choices?',
    'Cross-position analysis: if the constraint''s extractiveness and suppression are high from non-elite perspectives, and identifiable beneficiaries exist (Kremlin inner circle, defense industrial complex), the mountain is a false summit. The engine''s FSM detector will evaluate this automatically.',
    'If genuine mountain: the war is an inevitable structural feature of the international system, and no policy intervention can resolve it. If false summit: the constraint is a constructed policy choice that benefits specific agents, and alternative policies (negotiated settlement, regime change, sanctions relief) could dissolve it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether realist IR framing naturalizes a contingent constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(military_defeat_cascade, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mdc_theater_2022_02, military_defeat_cascade, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mdc_theater_2022_08, military_defeat_cascade, theater_ratio, 6, 0.48).
narrative_ontology:measurement(mdc_theater_2023_02, military_defeat_cascade, theater_ratio, 12, 0.62).
narrative_ontology:measurement(mdc_theater_2023_08, military_defeat_cascade, theater_ratio, 18, 0.71).
narrative_ontology:measurement(mdc_theater_2024_02, military_defeat_cascade, theater_ratio, 24, 0.78).
narrative_ontology:measurement(mdc_theater_2024_08, military_defeat_cascade, theater_ratio, 30, 0.82).
narrative_ontology:measurement(mdc_theater_2025_02, military_defeat_cascade, theater_ratio, 36, 0.84).
narrative_ontology:measurement(mdc_theater_2025_08, military_defeat_cascade, theater_ratio, 42, 0.85).
narrative_ontology:measurement(mdc_theater_2026_02, military_defeat_cascade, theater_ratio, 48, 0.85).

% Extraction over time
narrative_ontology:measurement(mdc_extract_2022_02, military_defeat_cascade, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mdc_extract_2022_08, military_defeat_cascade, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(mdc_extract_2023_02, military_defeat_cascade, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(mdc_extract_2023_08, military_defeat_cascade, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(mdc_extract_2024_02, military_defeat_cascade, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(mdc_extract_2024_08, military_defeat_cascade, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(mdc_extract_2025_02, military_defeat_cascade, base_extractiveness, 36, 0.68).
narrative_ontology:measurement(mdc_extract_2025_08, military_defeat_cascade, base_extractiveness, 42, 0.68).
narrative_ontology:measurement(mdc_extract_2026_02, military_defeat_cascade, base_extractiveness, 48, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mdc_suppress_2022_02, military_defeat_cascade, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(mdc_suppress_2023_02, military_defeat_cascade, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(mdc_suppress_2024_02, military_defeat_cascade, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(mdc_suppress_2025_02, military_defeat_cascade, suppression_requirement, 36, 0.81).
narrative_ontology:measurement(mdc_suppress_2026_02, military_defeat_cascade, suppression_requirement, 48, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(military_defeat_cascade, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of manpower_exhaustion_trap (mountain) — the demographic and economic limits on Russian mobilization capacity are immutable structural features. The military defeat cascade is the constructed policy choice to continue the war despite those limits. The upstream mountain (manpower exhaustion) sets the boundary conditions; the downstream constraint (defeat cascade) is the regime's response to those conditions. Decomposition: manpower_exhaustion_trap has negligible extraction (it is a genuine resource limit, not a policy choice); military_defeat_cascade has high extraction (the decision to continue extracting from conscripts despite the resource limit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(military_defeat_cascade, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
