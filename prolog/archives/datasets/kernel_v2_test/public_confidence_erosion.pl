% ============================================================================
% CONSTRAINT STORY: public_confidence_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_confidence_erosion, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_confidence_erosion
 *   human_readable: Public Confidence Erosion in Russian War Legitimacy
 *   domain: political_economy/regime_stability/military_conflict
 *
 * SUMMARY:
 *   The erosion of public confidence in the Russian war effort represents a
 *   structural constraint where the regime's coordination function (national
 *   governance, identity provision, stability maintenance) is increasingly
 *   contaminated by visible extraction (casualties, economic hardship,
 *   military failure). The constraint began in February 2022 with high
 *   initial support (rallying effect) but has degraded as war costs became
 *   undeniable: Ukrainian drone attacks reaching Moscow and St. Petersburg,
 *   overflowing military cemeteries, 30% increase in alcoholism, rising
 *   mental health disorders, and economic strain. The regime responds with
 *   intensifying suppression (censorship, arrests, propaganda) but this
 *   creates a feedback loop: more suppression is required to maintain the
 *   same level of public compliance, revealing that the coordination story is
 *   failing. The constraint is a tangled rope because genuine coordination
 *   functions remain (the state still provides services, pensions, regional
 *   administration) even as extraction intensifies. The theater ratio (0.58)
 *   reflects the gap between performative patriotism (required public
 *   displays of support) and actual belief (64% favor negotiations, only 24%
 *   support war continuation). State TV viewership decline (60% to 47%) and
 *   Putin trust collapse (29.5%) indicate the propaganda apparatus is losing
 *   effectiveness despite maintaining institutional power.
 *
 * KEY AGENTS:
 *   - Russian General Population: Primary victim (powerless/trapped) — bears direct costs of war (conscription, casualties, economic hardship) with no exit; trapped between lived reality and required performance
 *   - Regime Legitimacy: Abstract victim (powerless/trapped) — the regime's authority structure itself is being extracted from as confidence erodes; cannot exit or organize
 *   - State Media Apparatus: Primary beneficiary (institutional/arbitrage) — captures budget allocations, editorial control, and immunity from accountability; extraction flows toward this agent
 *   - Regional Administrators: Mixed position (moderate/constrained) — experience both coordination function (governance tasks) and extraction (must enforce failing narratives, bear blame for federal failures)
 *   - Opposition Coalition: Organized victim (organized/constrained) — benefits from regime's legitimacy crisis but bears severe suppression costs; constrained exit through exile
 *   - Patriotic Believers: Identity-locked victims (moderate/identity_locked) — structurally mobile but cognitively trapped; identity fused with regime narratives; experience maximum extraction as identity framework collapses
 *   - Polling Infrastructure: Institutional piton (institutional/arbitrage) — maintains ritual of opinion measurement but function has atrophied into performance; produces regime-legitimating statistics rather than valid data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_confidence_erosion, 0.68).
domain_priors:suppression_score(public_confidence_erosion, 0.72).
domain_priors:theater_ratio(public_confidence_erosion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_confidence_erosion, extractiveness, 0.68).
narrative_ontology:constraint_metric(public_confidence_erosion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(public_confidence_erosion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_confidence_erosion, tangled_rope).
narrative_ontology:human_readable(public_confidence_erosion, "Public Confidence Erosion in Russian War Legitimacy").
narrative_ontology:topic_domain(public_confidence_erosion, "political_economy/regime_stability/military_conflict").

domain_priors:requires_active_enforcement(public_confidence_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_confidence_erosion, regime_propaganda_apparatus).
narrative_ontology:constraint_beneficiary(public_confidence_erosion, state_media_institutions).
narrative_ontology:constraint_victim(public_confidence_erosion, russian_general_population).
narrative_ontology:constraint_victim(public_confidence_erosion, regime_legitimacy).
narrative_ontology:constraint_victim(public_confidence_erosion, social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RUSSIAN GENERAL POPULATION (SNARE) — Trapped within national borders with limited exit options, facing conscription risk, economic hardship, and social pressure. Cannot escape the constraint's extraction: visible war costs (cemeteries, drone attacks, inflation) directly contradict state narratives they are compelled to publicly affirm. The coordination story (national defense) has collapsed into pure extraction as the gap between lived reality and required performance widens.
constraint_indexing:constraint_classification(public_confidence_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL ADMINISTRATOR (TANGLED ROPE) — Constrained by career dependency on regime loyalty but also tasked with genuine coordination functions (managing local services, economic stability, social order). Experiences both the coordination function (maintaining regional governance) and asymmetric extraction (must enforce narratives they know are failing, bear blame for federal policy failures). Can exit through resignation but at severe career cost.
constraint_indexing:constraint_classification(public_confidence_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE MEDIA APPARATUS (ROPE) — Primary beneficiary with arbitrage-level exit options (senior figures can relocate, have foreign assets). Experiences the constraint as coordination: the propaganda system provides employment, status, and resource access. Extraction flows toward this agent through budget allocations, editorial control, and immunity from accountability. The declining viewership (60% to 47%) is a problem for effectiveness but not for their structural position.
constraint_indexing:constraint_classification(public_confidence_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPPOSITION COALITION (TANGLED ROPE) — Organized but heavily suppressed (imprisonment, exile, censorship). The erosion of public confidence creates coordination opportunities (shared recognition of narrative failure) but also extraction (increased repression, surveillance, legal persecution). Benefits from the regime's legitimacy crisis but bears severe costs for organizing. Constrained exit: can flee to exile but loses domestic organizing capacity.
constraint_indexing:constraint_classification(public_confidence_erosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PATRIOTIC BELIEVER (SNARE) — Identity-locked agent whose self-concept is fused with regime narratives and national greatness mythology. Structurally has moderate power and could exit (not conscription-age, has resources) but cannot because their identity is constituted through the war's legitimacy. The visible failure (drone attacks, casualties, economic decline) creates cognitive dissonance that intensifies rather than breaks the identity lock. Experiences maximum extraction because the constraint is destroying the identity framework they cannot abandon.
constraint_indexing:constraint_classification(public_confidence_erosion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 6: POLLING INFRASTRUCTURE (PITON) — State polling agencies (VTsIOM, FOM, Levada) maintain the ritual of measuring public opinion but the function has atrophied into performance. Real opinion is unmeasurable under repression; published polls serve regime legitimation rather than information gathering. The infrastructure persists through institutional inertia and budget allocation, not because it produces valid data. High theater ratio reflects the gap between claimed function (measuring opinion) and actual function (manufacturing consent statistics).
constraint_indexing:constraint_classification(public_confidence_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both genuine coordination function (the regime does provide some stability, services, and national identity framework) and substantial extraction (the war imposes massive costs on the population for elite benefit). The constraint coordinates national governance while extracting blood and treasure for a failing military adventure. The erosion is a structural feature: as war costs become undeniable, the coordination story loses force and extraction becomes visible.
constraint_indexing:constraint_classification(public_confidence_erosion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_confidence_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_confidence_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_confidence_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_confidence_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_confidence_erosion, TR),
    TR >= 0.70.

:- end_tests(public_confidence_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts blood (casualties, conscription), treasure (economic hardship, inflation, sanctions impact), and cognitive coherence (gap between lived reality and required belief) from the general population. The regime captures legitimacy, resource control, and geopolitical positioning while the population bears costs. Extraction has increased from 0.35 (initial rallying effect, genuine national defense framing) to 0.68 (visible failure, undeniable costs, coordination story collapse). The value reflects that substantial extraction is occurring but some coordination function remains (the state still governs, provides services, maintains order). Suppression (0.72): High and rising. Censorship laws (15-year prison terms for discrediting the military), media shutdowns, VPN bans, arrest of opposition figures, surveillance intensification, and propaganda saturation. Suppression has increased from 0.55 to 0.72 over the interval as the regime requires more coercion to maintain the same level of compliance. The rising trajectory indicates the constraint is becoming more coercive as voluntary compliance erodes. Theater ratio (0.58): Moderate-high. Substantial gap between performative patriotism (required public displays, Z-symbols, rally attendance) and actual belief (private polling shows 64% favor negotiations). State TV viewership decline (60% to 47%) indicates the performance is losing audience even as it maintains institutional presence. The ratio has increased from 0.38 to 0.58 as the gap between performance and function widens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon — public confidence erosion — appears differently based on the observer's position. The general population experiences a snare: trapped, facing direct extraction, with the coordination story (national defense) revealed as cover. Regional administrators experience tangled rope: genuine governance tasks mixed with forced narrative enforcement. The state media apparatus experiences rope: they are the beneficiaries, extraction flows toward them. The opposition sees tangled rope: the erosion creates organizing opportunities but also triggers intensified repression. Patriotic believers experience snare through identity lock: their cognitive fusion with regime narratives makes them unable to exit even as the framework collapses around them. The polling infrastructure sees piton: their function has degraded into theater but persists through inertia. The analytical observer sees tangled rope: genuine coordination functions remain even as extraction intensifies. The perspectival gap reveals that 'public confidence erosion' is not a single phenomenon but a presheaf over observation sites — the constraint's type depends on where you stand relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The state media apparatus is declared as beneficiary with arbitrage exit → low d → negative or low chi (they experience the constraint as coordination or mild extraction). The general population is declared as victim with trapped exit → high d → high chi (they experience maximum extraction). Regional administrators are victims with constrained exit → moderate-high d → moderate-high chi (mixed experience). The opposition coalition is victim with constrained exit but organized power → moderate d → moderate chi (extraction is real but agency reduces experienced severity). Patriotic believers are victims with identity_locked exit → very high d → very high chi (cognitive trap amplifies extraction beyond even the trapped population's experience). The polling infrastructure is beneficiary with arbitrage exit → low d → low chi (they benefit from the constraint through budget allocation and institutional position). The analytical observer uses analytical exit → context-dependent d based on the structural assessment of coordination vs extraction balance. The directionality derivation captures that the same base extractiveness (0.68) is experienced very differently depending on structural position: beneficiaries see coordination, trapped victims see pure extraction, identity-locked victims see identity dissolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled_rope is the correct classification at the analytical level: genuine coordination functions (governance, service delivery, national identity provision, regional administration) coexist with substantial extraction (war costs, suppression, cognitive dissonance). The regime is not pure extraction (snare) because it still governs — services are delivered, pensions paid, infrastructure maintained. But it is not pure coordination (rope) because the war imposes massive costs on the population for elite benefit. The tangled_rope classification captures this structural reality: the coordination function is real but contaminated by extraction, and the extraction is substantial but not total. The perspectival variation (snare from trapped victims, rope from beneficiaries, piton from degraded institutions) reflects genuine differences in experienced extraction based on structural position, not classification error. The mandate (national defense, regime stability) has not fully outlived its function (the state still provides governance) but is increasingly extractive as war costs mount and coordination stories fail.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_collapse_threshold,
    'At what threshold of visible failure does public confidence erosion trigger regime instability rather than merely reducing support?',
    'Historical analysis of authoritarian regime collapses; identification of tipping points where passive non-compliance becomes active resistance; correlation between economic hardship, military defeat visibility, and regime survival',
    'If threshold is high (regime can sustain 70%+ opposition): erosion is extraction but not existential. If threshold is low (regime vulnerable at 50% opposition): erosion becomes a scaffold with regime-change sunset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_collapse_threshold, empirical, 'Threshold at which confidence erosion triggers regime instability').

omega_variable(
    suppression_effectiveness_decay,
    'Does increasing suppression (censorship, arrests, propaganda intensity) stabilize or accelerate confidence erosion when war costs are directly visible?',
    'Longitudinal tracking of suppression intensity vs. confidence metrics; comparison with historical cases where visible war failure met increased repression (USSR Afghanistan, US Vietnam)',
    'If suppression stabilizes: regime can sustain the constraint indefinitely through coercion. If suppression accelerates erosion: the constraint has a structural sunset as repression delegitimizes faster than it controls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_effectiveness_decay, empirical, 'Whether suppression stabilizes or accelerates confidence erosion under visible failure').

omega_variable(
    identity_lock_breaking_mechanism,
    'What mechanism breaks identity-locked patriotic belief when lived reality contradicts regime narratives — gradual accumulation or sudden shock?',
    'Psychological research on belief perseverance under disconfirmation; interviews with former regime supporters; identification of critical incidents (personal loss, economic crisis, direct contradiction) that broke identity fusion',
    'If gradual: identity-locked agents remain locked until cumulative evidence overwhelms. If shock-based: specific events (major military defeat, leadership crisis) can rapidly shift large populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_breaking_mechanism, empirical, 'Mechanism by which identity-locked belief breaks under narrative failure').

omega_variable(
    coordination_function_residual,
    'How much genuine coordination function remains in the regime''s governance after confidence erosion — is the state still providing services, stability, and identity framework, or has it become pure extraction?',
    'Assessment of state capacity: service delivery, infrastructure maintenance, pension payments, regional administration effectiveness; comparison of state performance in 2022 vs 2024',
    'If substantial coordination remains: tangled_rope classification holds (mixed extraction and coordination). If coordination has collapsed: reclassify toward snare (pure extraction with coordination cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_residual, empirical, 'Residual genuine coordination function in regime governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_confidence_erosion, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pub_conf_theater_2022_02, public_confidence_erosion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pub_conf_theater_2022_08, public_confidence_erosion, theater_ratio, 6, 0.45).
narrative_ontology:measurement(pub_conf_theater_2023_02, public_confidence_erosion, theater_ratio, 12, 0.52).
narrative_ontology:measurement(pub_conf_theater_2023_08, public_confidence_erosion, theater_ratio, 18, 0.56).
narrative_ontology:measurement(pub_conf_theater_2024_02, public_confidence_erosion, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(pub_conf_extract_2022_02, public_confidence_erosion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pub_conf_extract_2022_05, public_confidence_erosion, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(pub_conf_extract_2022_08, public_confidence_erosion, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(pub_conf_extract_2023_02, public_confidence_erosion, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(pub_conf_extract_2023_08, public_confidence_erosion, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(pub_conf_extract_2024_02, public_confidence_erosion, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pub_conf_suppress_2022_02, public_confidence_erosion, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(pub_conf_suppress_2022_08, public_confidence_erosion, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(pub_conf_suppress_2023_02, public_confidence_erosion, suppression_requirement, 12, 0.67).
narrative_ontology:measurement(pub_conf_suppress_2023_08, public_confidence_erosion, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(pub_conf_suppress_2024_02, public_confidence_erosion, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_confidence_erosion, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of military_defeat_cascade (military failures make war costs visible), deathonomics_collapse (economic strain from war spending and sanctions), and control_mechanism_backfire (suppression intensification reveals narrative failure). The confidence erosion is a distinct constraint with its own extractiveness reflecting the cognitive and social costs of maintaining required belief in the face of visible failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
