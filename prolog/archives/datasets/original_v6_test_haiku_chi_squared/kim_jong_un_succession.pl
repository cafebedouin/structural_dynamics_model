% ============================================================================
% CONSTRAINT STORY: kim_jong_un_succession
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kim_jong_un_succession, []).

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
 *   constraint_id: kim_jong_un_succession
 *   human_readable: Kim Jong-un's Succession Plan and Dynastic Perpetuation
 *   domain: political/authoritarian_systems
 *
 * SUMMARY:
 *   Kim Jong-un's apparent grooming of his daughter as a potential successor
 *   crystallizes a fundamental constraint on North Korean political
 *   development: the intersection of dynastic perpetuation, authoritarian
 *   suppression, and geopolitical lock-in. The succession plan is not merely
 *   a personnel decision but a structural mechanism that forecloses
 *   institutional reform, meritocratic advancement, and alternative political
 *   futures. The constraint exhibits high extractiveness (0.68) and extreme
 *   suppression (0.78) because it operates through both overt coercion
 *   (execution, family punishment under songbun system) and performative
 *   ideology (cult of personality, Kimilsungism as quasireligion). Theater
 *   ratio (0.65) reflects the heavy investment in propaganda machinery,
 *   public performances of loyalty, and ritualized submission — the
 *   performance of dynastic legitimacy masks the underlying extraction of
 *   human potential and political voice from the entire population. From the
 *   perspective of the North Korean population, this is a pure Snare: trapped
 *   with no exit, bearing full cost of regime perpetuation. From the military
 *   and party cadre, it is also Snare: career blocked, meritocratic paths
 *   closed, exit constrained by collective punishment rules. From the
 *   security apparatus, it becomes Tangled Rope: constrained but also
 *   benefits from succession predictability (reduces coup risk, enables
 *   planning). From regional powers, it is Snare: structurally locked into
 *   managing a perpetually hostile, nuclear-armed authoritarian regime. From
 *   China, it is Rope: arbitrage capacity through aid leverage and
 *   geopolitical positioning. From liberal observers, it risks appearing as a
 *   Mountain (naturalized totalitarianism) when it is actually a
 *   contingent-but-deeply-entrenched Snare.
 *
 * KEY AGENTS:
 *   - Kim Jong-un: Institutional patriarch (institutional/arbitrage) — defines succession, benefits from perpetual regime control, exit option is maintenance of power only
 *   - North Korean Population: Primary victims (powerless/trapped) — no exit, full extraction cost, locked into songbun hereditary hierarchy
 *   - Military and Party Leadership: Secondary victims (moderate/constrained) — career ceilings imposed by dynastic requirement, exit constrained by collective punishment, forced meritocratic suspension
 *   - Regime Security Apparatus (State Security Department, secret police): Beneficiary (institutional/constrained) — enforces Snare, benefits from order but constrained by loyalty requirements to dynasty
 *   - Regional Powers (South Korea, Japan, US, China): Organized victims (organized/constrained) — structurally locked into managing permanent North Korean threat; China has arbitrage capacity
 *   - Chinese Leadership: Strategic beneficiary (institutional/arbitrage) — can manage North Korea through succession, maintains buffer state, extracts geopolitical leverage from DPRK dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kim_jong_un_succession, 0.68).
domain_priors:suppression_score(kim_jong_un_succession, 0.78).
domain_priors:theater_ratio(kim_jong_un_succession, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kim_jong_un_succession, extractiveness, 0.68).
narrative_ontology:constraint_metric(kim_jong_un_succession, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(kim_jong_un_succession, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kim_jong_un_succession, snare).
narrative_ontology:human_readable(kim_jong_un_succession, "Kim Jong-un's Succession Plan and Dynastic Perpetuation").
narrative_ontology:topic_domain(kim_jong_un_succession, "political/authoritarian_systems").

domain_priors:requires_active_enforcement(kim_jong_un_succession).

% --- Structural relationships ---
narrative_ontology:constraint_victim(kim_jong_un_succession, north_korean_population).
narrative_ontology:constraint_victim(kim_jong_un_succession, military_meritocracy).
narrative_ontology:constraint_victim(kim_jong_un_succession, party_cadre_mobility).
narrative_ontology:constraint_victim(kim_jong_un_succession, regime_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NORTH KOREAN POPULATION (SNARE) — No exit. Citizens cannot leave without severe penalty (execution, imprisonment, collective family punishment under songbun system). Bound by dynastic succession constraint that guarantees regime perpetuation and blocks institutional reform. d≈0.98, f(d)≈1.43, σ=1.0 → χ≈0.97. Maximum extraction with complete suppression of alternatives.
constraint_indexing:constraint_classification(kim_jong_un_succession, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY AND PARTY LEADERSHIP (SNARE) — Career advancement blocked by dynastic requirement. Merit-based promotion becomes meaningless once succession is predetermined. Exit constrained: defection means family destruction, treason execution. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.90. Senior military/cadre trapped by songbun descent rules and personality cult enforcement.
constraint_indexing:constraint_classification(kim_jong_un_succession, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIME SECURITY APPARATUS (TANGLED ROPE) — Constrained by dynasty but also benefits from succession clarity: predictable power transfer reduces factionalism and enables long-term repressive planning. Sees daughter-grooming as coordination mechanism (dynastic continuity) AND as enforcement requirement (controlling expectations). d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.46. Mixed: benefits from predictability, constrained by cult-of-personality performance requirements.
constraint_indexing:constraint_classification(kim_jong_un_succession, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL GEOPOLITICAL ACTORS (SNARE) — South Korea, China, Japan, US are structurally constrained by dynastic perpetuation in North Korea. Regime stability (however authoritarian) is somewhat predictable; dynastic succession reduces coup-risk instability but locks in perpetual authoritarianism, nuclear proliferation, regional tension. Cannot exit the constraint. d≈0.65, f(d)≈1.03, σ=1.1 → χ≈0.76. Organized actors bear structural cost of permanent regime hostility and proliferation risk.
constraint_indexing:constraint_classification(kim_jong_un_succession, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CHINESE LEADERSHIP (ROPE) — Can arbitrage the succession. Dynastic predictability enables China to manage North Korean alignment, resource extraction, and buffer-state utility. Exit option: shift alliances, reduce aid. Benefits from succession clarity (reduces coup chaos) while maintaining leverage through resources. d≈0.10, f(d)≈0.07, σ=1.1 → χ≈0.05. Net beneficiary through arbitrage capacity.
constraint_indexing:constraint_classification(kim_jong_un_succession, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVERS / LIBERAL DEMOCRACY ADVOCATES (PITON) — View the succession as embodying 'hereditary authoritarianism,' a category that describes the constraint but does not capture its function. The frame naturalizes dynastic rule as if it were immutable (mountain), when in fact it is maintained through performative ideology (Kimilsungism, divine leadership mythology) and brute suppression. theater_ratio=0.65 reflects the heavy performative content (cult of personality, propaganda apparatus) masking the extractive mechanism. From analytical distance, appears as degraded principle (meritocratic socialism → dynastic theocracy). But false summit risk: observers may naturalize 'totalitarianism is permanent' when the succession is actually contingent on continuous enforcement.
constraint_indexing:constraint_classification(kim_jong_un_succession, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kim_jong_un_succession_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kim_jong_un_succession, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kim_jong_un_succession, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kim_jong_un_succession, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kim_jong_un_succession, TR),
    TR >= 0.70.

:- end_tests(kim_jong_un_succession_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The succession plan extracts political voice, economic resources, and human potential from the population to sustain dynastic perpetuation. The extraction grows over time (0.52 → 0.68 over interval) as succession becomes more explicit and the regime hardens ideological enforcement. Suppression (0.78): Extreme. Exit is blocked by songbun system (hereditary caste), execution for defection, collective family punishment, total information control. Individuals have zero degrees of freedom. Theater ratio (0.65): Moderate-high. Significant portion of regime activity is performative (propaganda, public loyalty rituals, cult ceremonies) maintaining ideological legitimacy mask for what is structural extraction. As succession becomes explicit, theater increases (public displays of daughter's prominence, readjustments to Kimilsungist doctrine to accommodate female heir). Mandatrophy resolution: This is unambiguously a Snare (high ε, high suppression, high χ) from the population's perspective. From security apparatus it is Tangled Rope (coordination of succession + extraction of loyalty). The classification depends entirely on structural position. The Snare is the base reality; the Tangled Rope is the institutional actor's partial-benefit perspective. No ambiguity between categories — all perspectives agree on high extractiveness, just differing on coordination component.
 *
 * PERSPECTIVAL GAP:
 *   The North Korean population sees pure extraction (Snare): dynastic succession guarantees perpetual authoritarianism, blocks any institutional path to reform, and forecloses alternatives. The military and party cadre also see Snare: succession ends meritocratic career advancement for anyone outside the dynasty. The security apparatus sees Tangled Rope: they are constrained by loyalty requirements but also benefit from the predictability and clarity of succession (reduces internal factional conflict, enables long-term repressive planning). China sees Rope with arbitrage: can leverage North Korean dependence, benefits from stability, has exit option (shift alliances). Liberal observers risk a false Mountain: naturalizing 'totalitarianism is permanent' when the system is actually contingently enforced and could collapse rapidly under sufficient shock. The critical perspectival gap is between the powerless (locked in Snare) and the institutional beneficiary (China seeing Rope). This gap reflects asymmetric exit: the population has zero exit, China has selective exit, creating divergent classifications from structurally identical phenomena.
 *
 * DIRECTIONALITY LOGIC:
 *   North Korean population: Victims (identified in victims array) + trapped exit → d≈0.98, f(d)≈1.43. Maximum d because population has zero exit options and bears all costs. Military/cadre: Victims + constrained exit → d≈0.88, f(d)≈1.32. High d because exit is possible (defection, coup) but extremely costly. Security apparatus: Mixed (constrained but beneficiary of order) + constrained exit → d≈0.52, f(d)≈0.68. Neutral d reflecting mixed extraction/coordination. Regional powers: Victims (of perpetual threat) + constrained exit → d≈0.65, f(d)≈1.03. Organized but locked in by geopolitics. China: Beneficiary (strategic leverage) + arbitrage exit → d≈0.10, f(d)≈0.07. Low d because China can adjust its North Korea strategy without fatal cost. The directionality chain shows that population and military experience high extraction (high d, high f(d)) while institutional actors (security, China) experience lower effective extraction through their greater exit options and strategic positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint clearly classifies as SNARE from the structural reality of the North Korean population. Extractiveness (0.68) > 0.46 threshold met. Suppression (0.78) > 0.60 threshold met. Effective extraction χ for the population: d≈0.98, f(d)≈1.43, σ=1.0 → χ≈0.97, well above 0.66 Snare gate. The mandate problem ('Is this really extraction or just immutable authoritarianism?') is resolved by demonstrating that: (1) alternative institutional forms exist (South Korea transitioned; Taiwan; Japan; South Vietnam did transition). (2) The succession constraint is contingent on continuous enforcement — it requires the cult of personality machinery, the security apparatus, external support (China), and ideological socialization. (3) Historical analysis shows authoritarian systems can collapse rapidly when enforcement falters (USSR, East Germany, Yugoslavia). Therefore the constraint is not a Mountain (immutable natural law) but a contingent Snare. The mandate is resolved: North Korea's perpetual authoritarianism is structurally enforced extraction, not natural law. Mandatrophy_resolved: true, because the system demonstrates all three gates for high-extractiveness classification (ε, suppression, χ all exceed thresholds) and analysis shows it is not a false summit naturalizing contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    female_succession_acceptance,
    'Will the military and party cadre accept a female heir, or does patriarchal assumption within the Kimilsungist cult of personality create hidden resistance that could trigger succession crisis?',
    'Tracking of military statements, purges of potential dissenters, intelligence analysis of faction positioning during transition window. Comparison with historical female-leader outcomes in East Asian authoritarian systems.',
    'If accepted: succession logic holds (Snare classification confirmed). If rejected: hidden factionalism could trigger coup attempt, reclassifying constraint as unstable Tangled Rope with active contestation and higher exit probabilities for some cadre.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_succession_acceptance, empirical, 'Whether military patriarchy blocks female succession').

omega_variable(
    youth_legitimacy_deficit,
    'Can a generationally younger successor (or very young daughter) claim sufficient cult-of-personality capital without the military credentials or survival record of Kim Jong-un? Does legitimacy erosion create opening for constitutional constraint or meritocratic reform?',
    'Analysis of succession rhetoric; monitoring of ideological readjustment (how propaganda pivots from military performance to dynastic right); assessment of cadre morale during transition period.',
    'If legitimacy holds: Snare persists. If deficit creates opening: constraint becomes temporarily mobile (higher exit_options for some cadre), reclassifying transition period as Tangled Rope with exit pressure, potentially enabling institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(youth_legitimacy_deficit, empirical, 'Whether successor''s youth creates legitimacy deficit').

omega_variable(
    chinese_realignment_risk,
    'Does succession uncertainty create window for China to shift its North Korea strategy, potentially moving from propping up dynasty to supporting reform or transition? Or is Chinese interest in stability so strong that it enforces the succession through military/economic pressure?',
    'Tracking Chinese public statements, aid flows, military deployments near DPRK border. Historical analysis of how China handled succession in other client states.',
    'If China enforces dynasty: Snare deepens (external enforcement, even stronger suppression). If China exploits uncertainty: constraint becomes momentarily contestable, exit options expand for reform factions, classification shifts toward high-extractiveness Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chinese_realignment_risk, empirical, 'Whether China manipulates or secures the succession').

omega_variable(
    internal_reform_possibility,
    'Is the succession constraint ultimately contingent on continuous enforcement and ideology, or has the system calcified to the point where perpetual authoritarianism is genuinely immutable (mountain)? Could a shock (foreign pressure, economic collapse, military mutiny) rapidly shift regime type?',
    'Historical comparison with other collapsed dictatorships (USSR, East Germany, Yugoslavia); analysis of regime fragility indicators (defection rates, resource constraints, generational cadre turnover). Scenarios modeling transition pressure thresholds.',
    'If mountain (truly immutable): constraint is eternal. If Snare (contingent on enforcement): shock could collapse it rapidly. Classification consequence: determines whether we project 50-year regime stability or 5-year vulnerability window.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_reform_possibility, conceptual, 'Whether authoritarianism is immutable or contingently enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kim_jong_un_succession, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kju_succ_tr_t0, kim_jong_un_succession, theater_ratio, 0, 0.58).
narrative_ontology:measurement(kju_succ_tr_t10, kim_jong_un_succession, theater_ratio, 10, 0.62).
narrative_ontology:measurement(kju_succ_tr_t20, kim_jong_un_succession, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(kju_succ_be_t0, kim_jong_un_succession, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(kju_succ_be_t10, kim_jong_un_succession, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(kju_succ_be_t20, kim_jong_un_succession, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kim_jong_un_succession, enforcement_mechanism).
narrative_ontology:affects_constraint(kim_jong_un_succession, songbun_hereditary_caste).
narrative_ontology:affects_constraint(kim_jong_un_succession, north_korea_centralized_planning).
narrative_ontology:affects_constraint(kim_jong_un_succession, kim_dynasty_cult_of_personality).
narrative_ontology:affects_constraint(kim_jong_un_succession, sanctions_regime_north_korea).

% DUAL FORMULATION NOTE:
% Succession constraint is downstream of the broader Kim dynasty perpetuation mechanism and songbun caste system. The daughter-grooming specifically crystallizes the dynasty's structural choice to perpetuate authoritarian control rather than enable institutional reform. Related constraints: songbun (hereditary ε≈0.74), cult of personality (enforcement ε≈0.62), centralized planning (coordination failure ε≈0.55).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
