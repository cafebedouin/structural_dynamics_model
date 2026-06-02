% ============================================================================
% CONSTRAINT STORY: cultural_backlash_mobilization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_backlash_mobilization, []).

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
 *   constraint_id: cultural_backlash_mobilization
 *   human_readable: Cultural Backlash Mobilization Against Progressive Social Change
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   Cultural backlash mobilization against progressive social change (LGBTQ+
 *   rights, immigration, gender equality) represents a complex political
 *   constraint that exhibits different structural properties from different
 *   perspectives. The constraint coordinates genuine political preferences
 *   (aggregating diffuse cultural anxiety into electoral coalitions) while
 *   simultaneously extracting from minority groups (legal barriers, social
 *   hostility, institutional exclusion) and potentially from the backlash
 *   coalition itself (through bundled economic policies that harm material
 *   interests). The theater_ratio (0.58) reflects that much of the
 *   mobilization is performative culture war signaling rather than functional
 *   policy-making: legislative focus on symbolic issues (bathroom bills, flag
 *   controversies) rather than addressing underlying economic precarity or
 *   institutional dysfunction. The constraint is downstream of
 *   generational_economic_decline: material insecurity amplifies cultural
 *   threat perception, and political entrepreneurs channel economic anxiety
 *   into cultural grievance. The measurements show rising theater and
 *   extraction over the interval as the mobilization becomes more
 *   performative and the policy bundling becomes more extractive.
 *
 * KEY AGENTS:
 *   - LGBTQ+ Individuals: Primary victim (powerless/identity_locked) — cannot exit without abandoning identity authenticity; face legal barriers and social hostility
 *   - Immigrant Communities: Secondary victim (moderate/constrained) — constrained by legal status and economic dependency; experience mixed coordination and extraction
 *   - Conservative Political Entrepreneurs: Primary beneficiary (institutional/arbitrage) — capture electoral advantage and career benefits from mobilizing cultural anxiety
 *   - Traditional Status Hierarchy Incumbents: Secondary beneficiary (powerful/mobile) — benefit from backlash's defense of existing social hierarchies
 *   - Culturally Anxious Majority Group: Mixed position (moderate/constrained) — benefit from political representation of cultural preferences but bear costs from bundled economic policies
 *   - Progressive Counter-Mobilization: Organized opposition (organized/mobile) — see backlash as temporary reaction with demographic sunset
 *   - Traditional Media Gatekeepers: Degraded institutional actor (institutional/arbitrage) — maintain theatrical culture war framing but have lost agenda-setting power
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees recurring pattern of backlash against rapid social change with mixed coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_backlash_mobilization, 0.38).
domain_priors:suppression_score(cultural_backlash_mobilization, 0.52).
domain_priors:theater_ratio(cultural_backlash_mobilization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_backlash_mobilization, extractiveness, 0.38).
narrative_ontology:constraint_metric(cultural_backlash_mobilization, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(cultural_backlash_mobilization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_backlash_mobilization, tangled_rope).
narrative_ontology:human_readable(cultural_backlash_mobilization, "Cultural Backlash Mobilization Against Progressive Social Change").
narrative_ontology:topic_domain(cultural_backlash_mobilization, "political_economy/comparative_politics/democratic_theory").

domain_priors:requires_active_enforcement(cultural_backlash_mobilization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_backlash_mobilization, conservative_political_entrepreneurs).
narrative_ontology:constraint_beneficiary(cultural_backlash_mobilization, traditional_status_hierarchy_incumbents).
narrative_ontology:constraint_beneficiary(cultural_backlash_mobilization, culturally_anxious_majority_group_members).
narrative_ontology:constraint_victim(cultural_backlash_mobilization, lgbtq_individuals).
narrative_ontology:constraint_victim(cultural_backlash_mobilization, immigrant_communities).
narrative_ontology:constraint_victim(cultural_backlash_mobilization, gender_equality_advocates).
narrative_ontology:constraint_victim(cultural_backlash_mobilization, cosmopolitan_urban_professionals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LGBTQ+ INDIVIDUALS (SNARE) — Identity-locked rather than trapped: structurally mobile (could relocate, could closet) but identity is constituted through authentic self-expression. The backlash mobilization creates legal barriers, social hostility, and institutional exclusion. Exit would require either abandoning identity authenticity or geographic displacement from community/family networks. High extraction, high suppression.
constraint_indexing:constraint_classification(cultural_backlash_mobilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: IMMIGRANT COMMUNITIES (TANGLED ROPE) — Constrained by legal status, economic dependency, and language barriers. The backlash mobilization both extracts (through hostile policy, social exclusion, labor exploitation) and coordinates (immigration policy does solve genuine collective action problems around resource allocation, labor market integration, cultural adaptation). Mixed experience: real coordination function exists alongside asymmetric extraction.
constraint_indexing:constraint_classification(cultural_backlash_mobilization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSERVATIVE POLITICAL ENTREPRENEURS (ROPE) — Primary beneficiaries with arbitrage exit options. The backlash mobilization is a coordination mechanism: it aggregates diffuse cultural anxiety into electoral coalitions, solves the collective action problem of organizing voters around non-economic issues, and provides a stable platform for political careers. Low experienced extraction — the constraint subsidizes this agent.
constraint_indexing:constraint_classification(cultural_backlash_mobilization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CULTURALLY ANXIOUS MAJORITY GROUP (TANGLED ROPE) — Moderate power, constrained exit. These voters experience genuine status threat from rapid social change and benefit from the backlash's coordination function (political representation of their preferences, community solidarity). But they also bear costs: economic policies bundled with cultural backlash often harm their material interests, and the mobilization channels anxiety into performative culture war rather than addressing underlying economic precarity. Mixed coordination and extraction.
constraint_indexing:constraint_classification(cultural_backlash_mobilization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PROGRESSIVE COUNTER-MOBILIZATION (SCAFFOLD) — Organized agents (civil rights organizations, progressive parties, urban coalitions) see the backlash as a temporary reaction to demographic and cultural shifts. The sunset logic: as generational replacement proceeds and urban-rural population rebalances, the backlash coalition's electoral viability declines. The constraint is real now but has a structural expiration date. Low effective extraction because the coalition has agency and sees an exit path through demographic change.
constraint_indexing:constraint_classification(cultural_backlash_mobilization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL MEDIA GATEKEEPERS (PITON) — The cultural backlash narrative is maintained through media amplification, but the gatekeeping function has atrophied. Social media and fragmented information ecosystems have bypassed traditional editorial control. The backlash mobilization persists through the media's theatrical maintenance of culture war framing, but the actual agenda-setting power has degraded. High theater ratio, low functional coordination.
constraint_indexing:constraint_classification(cultural_backlash_mobilization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, cultural backlash mobilization is a recurring pattern in democratic transitions: rapid social liberalization triggers organized resistance from status hierarchy incumbents. The pattern has both coordination elements (aggregating preferences, representing constituencies) and extraction elements (scapegoating minorities, blocking material redistribution through culture war distraction). The analytical classification is Tangled Rope because the structural data shows genuine coordination function alongside asymmetric extraction.
constraint_indexing:constraint_classification(cultural_backlash_mobilization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_backlash_mobilization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cultural_backlash_mobilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cultural_backlash_mobilization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cultural_backlash_mobilization, TR),
    TR >= 0.70.

:- end_tests(cultural_backlash_mobilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The backlash mobilization extracts from minority groups through legal barriers, social hostility, and institutional exclusion. It also extracts from the culturally anxious majority group through bundled economic policies that harm their material interests while providing symbolic cultural representation. But extraction is not as severe as pure scapegoating (Snare) because genuine coordination function exists: the mobilization does aggregate real preferences and solve collective action problems around political representation. Suppression (0.52): Moderate-high. Significant barriers to exit for identity-locked minorities (LGBTQ+ individuals cannot exit without abandoning authenticity) and constrained immigrants (legal status, economic dependency). But suppression is not total — some geographic mobility exists, some legal protections remain, and counter-mobilization provides alternative political pathways. Theater ratio (0.58): Moderate-high. Much of the backlash mobilization is performative: legislative focus on symbolic culture war issues rather than functional policy, media amplification of outrage rather than substantive debate, political signaling to base rather than governance. The theater has increased over the interval as social media fragmentation has enabled more performative politics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same political mobilization appears as coordination from the beneficiary perspective (conservative entrepreneurs see preference aggregation), extraction from the victim perspective (LGBTQ+ individuals see hostility and exclusion), mixed coordination-extraction from moderate perspectives (immigrants and culturally anxious voters see both benefits and costs), temporary problem from the organized opposition perspective (progressives see demographic sunset), and degraded ritual from the institutional perspective (media sees performative culture war). The analytical observer's Tangled Rope classification reflects the structural reality: genuine coordination function (political representation, preference aggregation) coexists with asymmetric extraction (minority scapegoating, economic policy bundling that harms backlash supporters). The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?'
 *
 * DIRECTIONALITY LOGIC:
 *   Conservative political entrepreneurs are primary beneficiaries with arbitrage exit options — they can shift to other political strategies if backlash mobilization becomes electorally unviable. The engine derives low d (beneficiary + arbitrage) → low/negative f(d) → low/negative chi. They experience the constraint as coordination (Rope). LGBTQ+ individuals are primary victims with identity_locked exit — structurally mobile but cannot exit without abandoning identity authenticity. The engine derives high d (victim + identity_locked) → high f(d) → high chi. They experience the constraint as extraction (Snare). Immigrant communities are secondary victims with constrained exit — high costs to exit (legal status, economic dependency) but not impossible. The engine derives moderately high d (victim + constrained) → moderately high f(d) → moderate chi. They experience mixed coordination and extraction (Tangled Rope). Culturally anxious majority group members have mixed beneficiary/victim status (benefit from cultural representation, harmed by bundled economic policies) with constrained exit. The engine derives moderate d → moderate f(d) → moderate chi. They experience mixed coordination and extraction (Tangled Rope). The progressive counter-mobilization is organized with mobile exit options and sees the constraint as temporary (Scaffold). Traditional media gatekeepers are institutional with arbitrage exit but see their own function as degraded (Piton).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that cultural backlash mobilization has BOTH genuine coordination elements (aggregating preferences, solving collective action problems around political representation) AND asymmetric extraction elements (scapegoating minorities, bundling harmful economic policies, channeling material anxiety into symbolic culture war). The Tangled Rope classification from the analytical perspective captures this duality. The Rope classification from the beneficiary perspective is their genuine experience (they benefit from the coordination). The Snare classification from the victim perspective is their genuine experience (they bear the extraction). The Scaffold classification from the progressive perspective reflects their structural position (organized agents with exit paths who see demographic sunset). No single type is 'the' answer — the presheaf over observation sites IS the answer. The constraint is not reducible to either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_vs_symbolic_driver,
    'Is cultural backlash primarily driven by material economic decline (status threat from downward mobility) or by symbolic threat from changing social norms independent of material conditions?',
    'Regression analysis controlling for economic indicators vs cultural change indicators; cross-national comparison of backlash strength vs economic trajectory; within-country analysis of backlash support across income/education strata',
    'If material: backlash is downstream of economic constraint (generational_economic_decline) and would dissolve with economic security. If symbolic: backlash is autonomous cultural phenomenon requiring different intervention. Classification remains Tangled Rope either way, but the coordination vs extraction balance shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_vs_symbolic_driver, empirical, 'Whether backlash is materially or symbolically driven').

omega_variable(
    elite_manipulation_vs_organic_demand,
    'Is cultural backlash mobilization primarily elite-driven (political entrepreneurs manufacturing grievance) or demand-driven (elites responding to organic voter preferences)?',
    'Analysis of issue salience before vs after elite messaging; comparison of voter priorities in surveys vs party platform emphasis; experimental studies of elite framing effects on cultural attitudes',
    'If elite-driven: higher extractiveness (manufactured grievance for political gain). If demand-driven: lower extractiveness (genuine preference aggregation). Affects whether the constraint is closer to Rope (coordination) or Snare (extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_manipulation_vs_organic_demand, empirical, 'Elite supply vs organic demand for backlash mobilization').

omega_variable(
    sunset_timeline_demographic_shift,
    'What is the timeline for demographic change to erode the backlash coalition''s electoral viability, and does this constitute a genuine sunset or merely a shift to new mobilization strategies?',
    'Cohort analysis of cultural attitudes; projection of voting-eligible population by demographic group; historical analysis of how previous backlash movements adapted vs dissolved',
    'If genuine sunset (10-20 years): Scaffold classification from progressive perspective is structurally accurate. If adaptation (backlash shifts targets/frames): Scaffold perspective is aspirational rather than structural, and the constraint is more persistent (Tangled Rope or Snare from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_timeline_demographic_shift, empirical, 'Timeline and mechanism of backlash coalition decline').

omega_variable(
    bundling_economic_harm,
    'To what extent do economic policies bundled with cultural backlash platforms (tax cuts for wealthy, deregulation, welfare retrenchment) materially harm the culturally anxious majority group that supports the backlash?',
    'Analysis of policy outcomes for backlash coalition voters; comparison of stated cultural preferences vs material policy consequences; longitudinal tracking of economic well-being for backlash supporters',
    'If substantial harm: higher extractiveness from the culturally anxious majority group perspective (they are victims of bundled extraction, not just beneficiaries of cultural representation). If minimal harm or net benefit: lower extractiveness (genuine preference satisfaction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundling_economic_harm, empirical, 'Material harm from bundled economic policies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_backlash_mobilization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_backlash_tr_t0, cultural_backlash_mobilization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cult_backlash_tr_t5, cultural_backlash_mobilization, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cult_backlash_tr_t10, cultural_backlash_mobilization, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cult_backlash_be_t0, cultural_backlash_mobilization, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cult_backlash_be_t5, cultural_backlash_mobilization, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(cult_backlash_be_t10, cultural_backlash_mobilization, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cult_backlash_su_t0, cultural_backlash_mobilization, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cult_backlash_su_t5, cultural_backlash_mobilization, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(cult_backlash_su_t10, cultural_backlash_mobilization, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_backlash_mobilization, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of generational_economic_decline (material insecurity amplifies cultural threat perception). The upstream constraint has its own extractiveness reflecting economic stagnation; this constraint has its own extractiveness reflecting political mobilization dynamics. They are linked but structurally distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
