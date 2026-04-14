% ============================================================================
% CONSTRAINT STORY: sotu_1951_truman_marshall_plan_economic_assistance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1951_truman_marshall_plan_economic_assistance, []).

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
 *   constraint_id: sotu_1951_truman_marshall_plan_economic_assistance
 *   human_readable: Marshall Plan Economic Assistance as Anti-Communist Stabilization
 *   domain: geopolitics/economic_policy
 *
 * SUMMARY:
 *   The Marshall Plan (1948-1952, extended through 1951 via the Mutual
 *   Security Act) represents a structural constraint deployed to prevent
 *   Soviet ideological subversion and communist political takeover in Western
 *   Europe. Truman frames the problem as material: populations with
 *   employment, homes, and consumer access resist communist recruitment. The
 *   constraint operates on three levels simultaneously: (1) as genuine
 *   coordination mechanism solving Western Europe's capital scarcity problem
 *   and preventing economic collapse; (2) as asymmetric extraction mechanism
 *   subordinating European economic policy to American strategic interest and
 *   foreclosing alternative development models; (3) as suppression mechanism
 *   that directs material grievance into pro-American political channels and
 *   away from communist organizing. The same structural feature — conditional
 *   capital transfer contingent on anti-communist political alignment and
 *   economic policy conformity — appears as rescue from all perspectives, and
 *   as sphere consolidation and autonomy extraction from others. The
 *   constraint's theater ratio (0.38 at measurement point 4) remains moderate
 *   because the plan's coordination function is genuine: actual
 *   infrastructure investment, measurable economic recovery, visible
 *   industrial restoration. However, the theater increases over the interval
 *   as the contingency conditions (anti-communist certification, currency
 *   stabilization, counterpart fund requirements) become more prominent than
 *   the aid itself.
 *
 * KEY AGENTS:
 *   - United States Government (institutional/arbitrage): Primary beneficiary — achieves strategic consolidation of Western bloc, secures export markets, prevents Soviet sphere expansion at relatively low fiscal cost to American productive capacity
 *   - Western European Unemployed and Dispossessed (powerless/trapped): Primary victim — material grievances that would otherwise fuel communist recruitment are redirected into pro-American capitalist wage economy; alternative organizing frameworks are ideologically foreclosed; exit from the American-defined reconstruction path is structurally impossible
 *   - Western European Political Leadership (moderate/constrained): Secondary beneficiary and secondary victim — restore legitimacy through economic recovery and material improvement, but surrender policy autonomy and become institutionally dependent on American capital and strategic approval
 *   - American Taxpayers (powerless/constrained): Distributed victim — bear financial cost of aid program; extraction is indirect and diffuse, masked by framing as geopolitical investment rather than redistribution
 *   - Soviet Union and Soviet Bloc (institutional/trapped): Victim — excluded from Marshall Plan participation, faces Western European consolidation as anti-Soviet bloc, loses sphere expansion opportunities in Western Europe
 *   - Non-Aligned Development Advocates (organized/constrained): Secondary victim — Marshall Plan demonstrates that capital-intensive reconstruction is dependent on American benevolence; forecloses truly independent development models; creates precedent for American strategic conditionality on aid
 *   - Western European Industrial and Financial Elites (institutional/arbitrage): Secondary beneficiary — American aid stabilizes property rights, prevents labor radicalization, enables profitable investment in reconstruction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1951_truman_marshall_plan_economic_assistance, 0.38).
domain_priors:suppression_score(sotu_1951_truman_marshall_plan_economic_assistance, 0.45).
domain_priors:theater_ratio(sotu_1951_truman_marshall_plan_economic_assistance, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1951_truman_marshall_plan_economic_assistance, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1951_truman_marshall_plan_economic_assistance, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sotu_1951_truman_marshall_plan_economic_assistance, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1951_truman_marshall_plan_economic_assistance, tangled_rope).
narrative_ontology:human_readable(sotu_1951_truman_marshall_plan_economic_assistance, "Marshall Plan Economic Assistance as Anti-Communist Stabilization").
narrative_ontology:topic_domain(sotu_1951_truman_marshall_plan_economic_assistance, "geopolitics/economic_policy").

domain_priors:requires_active_enforcement(sotu_1951_truman_marshall_plan_economic_assistance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1951_truman_marshall_plan_economic_assistance, united_states_strategic_interests).
narrative_ontology:constraint_beneficiary(sotu_1951_truman_marshall_plan_economic_assistance, western_european_governments).
narrative_ontology:constraint_beneficiary(sotu_1951_truman_marshall_plan_economic_assistance, dollar_export_markets).
narrative_ontology:constraint_victim(sotu_1951_truman_marshall_plan_economic_assistance, american_taxpayers).
narrative_ontology:constraint_victim(sotu_1951_truman_marshall_plan_economic_assistance, soviet_bloc_sphere_of_influence).
narrative_ontology:constraint_victim(sotu_1951_truman_marshall_plan_economic_assistance, alternative_european_development_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WESTERN EUROPEAN DISPOSSESSED (SNARE) — American aid redirects material grievances that would otherwise fuel communist recruitment. The beneficiaries (stable employed populations) experience the constraint as beneficial, but those excluded from the aid distribution (urban unemployed, displaced persons, agricultural laborers) face maximum extraction: they are simultaneously offered entry into the capitalist wage economy (the carrot) and denied alternative organizing frameworks (the stick). The constraint suppresses their capacity to articulate demands outside the framework of pro-American reconstruction. Exit is structurally impossible — the only alternative model (Soviet-style central planning) is ideologically foreclosed by American strategic interest.
constraint_indexing:constraint_classification(sotu_1951_truman_marshall_plan_economic_assistance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNITED STATES STRATEGIC INTEREST (ROPE) — The Marshall Plan is experienced by American institutional actors as pure coordination: preventing Soviet sphere expansion, securing Western Europe as an ally, stabilizing markets for American exports. The economic assistance solves a collective action problem (Western Europe cannot self-finance reconstruction; individual American firms cannot profitably export without solvent buyers; the Soviet alternative threatens both). The U.S. benefits from the arrangement through geopolitical position, export markets, and preserved alliance structure. Arbitrage exit is available — the U.S. could withdraw support, redirect resources elsewhere, renegotiate terms. The perceived extraction is minimal because the U.S. is beneficiary.
constraint_indexing:constraint_classification(sotu_1951_truman_marshall_plan_economic_assistance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: WESTERN EUROPEAN POLITICAL LEADERSHIP (TANGLED ROPE) — European governments experience the Marshall Plan as a hybrid of genuine coordination (restoring economic function, preventing state collapse, enabling political legitimacy through material recovery) and asymmetric extraction (institutional dependence on American capital, subordination of European industrial policy to American strategic interest, loss of policy autonomy). Exit is constrained — European leaders could refuse American aid, but at severe political cost (continued economic collapse, loss of electoral legitimacy, risk of communist electoral victory or Soviet military pressure). The beneficiaries (employed populations stabilized into pro-Western political coalitions) and victims (alternative development models foreclosed, European strategic autonomy surrendered) are both tangible. Active enforcement occurs through the condition-setting of aid (counterpart funds, currency stabilization, anti-communist certification requirements).
constraint_indexing:constraint_classification(sotu_1951_truman_marshall_plan_economic_assistance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: SOVIET BLOC AND NON-ALIGNED CRITICS (TANGLED ROPE) — The Marshall Plan functions as a coordination mechanism for Western reconstruction AND as an extractive barrier to alternative development paths. The Soviet Union experiences the plan as pure extraction — American aid consolidates Western Europe as a closed bloc, denies Soviet reconstruction participation, subordinates European economic policy to American strategic interest. Non-aligned development advocates (India, Yugoslavia, non-communist nationalist movements) experience the constraint as mixed: it stabilizes Western Europe but also demonstrates that capital-intensive reconstruction is dependent on American benevolence, foreclosing truly independent development models. The constraint both coordinates Western prosperity and extracts Eastern/non-aligned autonomy.
constraint_indexing:constraint_classification(sotu_1951_truman_marshall_plan_economic_assistance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-WAR RECONSTRUCTION PLANNERS (SCAFFOLD) — The Marshall Plan is experienced by planners and institutional architects as a temporary coordination mechanism with an implicit sunset: as Western European economies recover, aid dependence declines, and the geopolitical rationale (preventing Soviet takeover) weakens as democratic legitimacy replaces crisis survival. The theater ratio for this perspective is low — genuine infrastructure investment, visible economic recovery, measurable industrial restoration. However, the sunset is delayed and contingent: if Soviet threat perception remains high or European economies fail to recover, the aid mechanism persists beyond its functional necessity. Organized agents in this view see a clear exit path (economic recovery → aid withdrawal → normal alliance relationships).
constraint_indexing:constraint_classification(sotu_1951_truman_marshall_plan_economic_assistance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR INSTITUTIONAL APPARATUS (PITON) — The Marshall Plan, from the longue durée institutional perspective, represents a crystallized Cold War mechanism that persists through institutional inertia long after its functional necessity. The original extraction function (preventing Soviet sphere expansion, stabilizing Western Europe against communist revolution) gradually becomes embedded in permanent alliance structures, institutional hierarchies, and transatlantic dependency relationships. By the 1960s-1980s, the direct aid mechanism has largely sunset, but the structural subordination of European policy to American strategic interest remains through NATO, the IMF, Cold War military spending, and dollar-denominated trade. The theater ratio rises as the functional mechanism (emergency reconstruction) is replaced by performative rehearsals of alliance commitment. The piton classification derives from this degradation: the constraint persists not because it solves the original problem (Soviet takeover is no longer imminent) but because institutional actors have become fused with the Cold War framework.
constraint_indexing:constraint_classification(sotu_1951_truman_marshall_plan_economic_assistance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational/universal perspective, capital concentration (American postwar industrial supremacy) creates inevitable dependencies among lower-capital regions. Economic aid from capital-surplus to capital-deficit regions is a natural law of capitalist reconstruction: it flows according to accumulation logic, not strategic choice. This perspective naturalizes the Marshall Plan as an inevitable consequence of capital distribution, not as a contingent geopolitical strategy. However, the structural data contradicts this mountain classification: the beneficiaries (U.S. strategic interests, Western European governments, American export markets), victims (dispossessed populations, Soviet-aligned states), and active enforcement requirements reveal that the constraint is contingent institutional design, not natural law. The false summit signature fires here.
constraint_indexing:constraint_classification(sotu_1951_truman_marshall_plan_economic_assistance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1951_truman_marshall_plan_economic_assistance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1951_truman_marshall_plan_economic_assistance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1951_truman_marshall_plan_economic_assistance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1951_truman_marshall_plan_economic_assistance, TR),
    TR >= 0.70.

:- end_tests(sotu_1951_truman_marshall_plan_economic_assistance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Marshall Plan contains genuine coordination functions (solving capital scarcity, preventing economic collapse, enabling mutual benefit through trade) alongside asymmetric extraction (subordinating European policy autonomy, consolidating American sphere of influence, foreclosing alternative development models). The constraint extracts from American taxpayers (distributed burden), dispossessed Europeans (redirected grievance), and non-aligned states (foreclosed alternatives), while benefiting American strategic interests and Western European stabilized populations. The value reflects the hybrid nature: not pure coordination (which would be lower), but not maximum extraction (which would require no coordination function). Suppression (0.45): Moderate-high. Significant suppression operates through: (1) conditionality mechanisms (anti-communist certification, currency stabilization requirements, counterpart fund controls); (2) geopolitical vulnerability (Western Europe cannot refuse without risking Soviet pressure); (3) ideological closure (alternative development models are foreclosed as communist or nationalist, not presented as genuine options); (4) structural dependence (capital scarcity makes European governments structurally unable to exit). However, suppression is not total — European governments retain nominal policy choice, can negotiate aid terms, and some resistance to American directives is visible (Marshall Plan explicitly avoids administrative occupation, unlike Cold War occupation policies in later periods). Theater ratio (0.38): Moderate-low. The plan's functional content is substantial: actual capital transfers, documented economic recovery metrics, visible infrastructure investment, measurable output increases in European industrial production. Theater is not high because the coordination function is genuine. However, theater increases over the interval as the original emergency phase transitions to permanent institutional arrangement, and as the contingency conditions become ritualized (annual certification of anti-communist credentials becomes performative rather than substantive). The theater ratio captures this transition from functional mechanism to institutionalized structure.
 *
 * PERSPECTIVAL GAP:
 *   The Marshall Plan demonstrates maximum perspectival divergence: American strategic interests classify the constraint as pure Rope (coordination without experienced extraction), while dispossessed Europeans classify it as Snare (extraction with suppressed alternatives). Western European leadership experiences Tangled Rope (genuine coordination alongside asymmetric dependence). Post-war planners classify it as Scaffold (temporary emergency measure with implicit sunset). The Cold War institutional apparatus eventually degrades it into Piton (performative alliance ritual). The analytical observer risks classifying it as Mountain (natural law of capital concentration), but this is a false summit — the beneficiary declarations reveal that the constraint is contingent institutional design, not inevitable structural outcome. The perspectival gap is not between different facts but between different structural positions: beneficiaries experience minimal extraction because the constraint flows toward them; victims experience maximum extraction because the constraint flows away from them and their alternatives are suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from the structural position of each agent relative to the flow of extraction. American strategic interests (d ≈ 0.05): Full beneficiary with arbitrage exit — can withdraw support, redirect resources, renegotiate terms without strategic penalty. American taxpayers (d ≈ 0.72): Distributed targets with constrained exit — bear diffuse financial burden but cannot organize as collective political force; exit would require opposing geopolitical consensus. Western European leadership (d ≈ 0.58): Mixed position — constrained exit (refusal carries political cost of collapse and Soviet pressure), but also genuine beneficiaries of stabilized legitimacy and economic recovery. Western European dispossessed (d ≈ 0.88): Near-complete targets with trapped exit — material grievances redirected into pro-American channels; alternative frameworks ideologically foreclosed; refusal of aid means destitution. Soviet Union (d ≈ 0.92): Near-complete target with trapped exit — excluded from participation, faces Western consolidation against vital interests, cannot exit the geopolitical competition. Non-aligned states (d ≈ 0.75): High targets with constrained exit — observe that American aid requires political subordination; attempt independent development paths face pressure and resource scarcity. The dispersion of d values across perspectives explains why the same structural feature (capital transfer contingent on political alignment) appears as coordination from beneficiary views and as extraction from victim views.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION THROUGH STRUCTURAL DECOMPOSITION: The Marshall Plan resolves the mandatrophy by demonstrating that Tangled Rope is the analytically accurate classification because the constraint simultaneously contains (1) genuine coordination function (solving Western European capital scarcity, enabling mutual trade benefit, stabilizing democratic legitimacy against revolutionary threat) and (2) asymmetric extraction (subordinating European policy autonomy to American strategic interest, foreclosing alternative development models, redirecting material grievance into pro-American political channels). Neither pure Rope nor pure Snare correctly describes the constraint — the hybrid classification is required because the same institutional mechanism serves dual functions depending on the observer's structural position. The constraint is not misclassified Snare (extraction disguised as coordination); it genuinely solves coordination problems. The constraint is not misclassified Rope (pure coordination with incidental side effects); it systematically extracts European autonomy and forecloses alternatives. The Tangled Rope classification prevents the false natural-law reading (Mountain) by making beneficiaries and victims explicit: if the constraint is truly a natural law of capital concentration, no beneficiaries should be identifiable. The presence of clear beneficiaries (American strategic interests, Western European stabilized populations) and clear victims (American taxpayers, dispossessed Europeans, non-aligned states) reveals the constraint as contingent institutional design. The mandatrophy is resolved: the constraint is correctly classified as Tangled Rope because both the coordination and extraction dimensions are structurally necessary, not contingent or hidden.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communist_electoral_threat_counterfactual,
    'Would Western European communist movements have achieved electoral majority or revolutionary seizure without the Marshall Plan''s material stabilization?',
    'Comparative analysis of communist electoral performance in European regions with varying aid intensity; counterfactual modeling of aid withdrawal scenarios; archival evidence of communist recruitment dependence on grievance structures',
    'If high threat (aid prevented major communist takeover): plan functions as genuine coordination/stabilization (Rope/Scaffold from beneficiary perspective). If low threat (communist movements were marginal regardless): plan functions primarily as extraction/sphere consolidation (Snare/Tangled Rope from victim perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communist_electoral_threat_counterfactual, empirical, 'Whether communist electoral threat was materially high enough to justify aid as anti-communist mechanism').

omega_variable(
    alternative_reconstruction_sufficiency,
    'Could Western European recovery have occurred through internal accumulation, Soviet reparations participation, or non-aligned reconstruction models without American aid?',
    'Analysis of pre-Marshall Plan recovery trajectories in European nations with varying aid access; modeling of Soviet participation scenarios; comparison with post-WWII reconstruction models in non-aligned regions',
    'If internally sufficient: aid functions primarily as sphere consolidation and strategic subordination (extraction-dominant). If genuinely necessary: aid functions as coordinated stabilization (Rope/Scaffold dominant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reconstruction_sufficiency, conceptual, 'Whether reconstruction was dependent on American aid or merely accelerated by it').

omega_variable(
    subordination_mechanism_intentionality,
    'Was the structural subordination of European policy autonomy to American strategic interest a designed feature of the Marshall Plan or an emergent consequence of aid dependence?',
    'Analysis of State Department planning documents; comparison of design intentions with implemented conditions; interviews with American and European planners on strategic objectives vs operational constraints',
    'If designed: constraint is extracted by American institutional actors as core feature. If emergent: constraint arises from dependence dynamics, moderating the extraction characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_mechanism_intentionality, empirical, 'Whether European subordination was intentionally designed or emergent from aid dependence').

omega_variable(
    suppression_mechanism_internalization,
    'Did Western European elites internalize American strategic preferences (identity_locked) or respond to material constraints and explicit conditionality (constrained/trapped)?',
    'Analysis of European elite rhetoric and policy justifications; comparison of stated preferences to American directives; examination of cases where European leaders resisted American pressure',
    'If identity_locked: European agency is more severely suppressed through belief capture. If constrained: suppression is material/conditional, allowing potential exit at political cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Mechanism of European elite alignment: structural constraint vs identity fusion with American interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1951_truman_marshall_plan_economic_assistance, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marshall_tr_t0, sotu_1951_truman_marshall_plan_economic_assistance, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marshall_tr_t2, sotu_1951_truman_marshall_plan_economic_assistance, theater_ratio, 2, 0.32).
narrative_ontology:measurement(marshall_tr_t4, sotu_1951_truman_marshall_plan_economic_assistance, theater_ratio, 4, 0.38).

% Extraction over time
narrative_ontology:measurement(marshall_be_t0, sotu_1951_truman_marshall_plan_economic_assistance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(marshall_be_t2, sotu_1951_truman_marshall_plan_economic_assistance, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(marshall_be_t4, sotu_1951_truman_marshall_plan_economic_assistance, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1951_truman_marshall_plan_economic_assistance, resource_allocation).
narrative_ontology:affects_constraint(sotu_1951_truman_marshall_plan_economic_assistance, nato_alliance_subordination).
narrative_ontology:affects_constraint(sotu_1951_truman_marshall_plan_economic_assistance, dollar_hegemony_emergence).
narrative_ontology:affects_constraint(sotu_1951_truman_marshall_plan_economic_assistance, european_communist_party_marginalization).
narrative_ontology:affects_constraint(sotu_1951_truman_marshall_plan_economic_assistance, soviet_sphere_consolidation_eastern_europe).

% DUAL FORMULATION NOTE:
% The Marshall Plan decomposes into multiple structurally distinct constraints: (1) Western European capital scarcity (the coordination problem being solved), (2) American sphere consolidation in Western Europe (the extraction mechanism), (3) suppression of non-aligned development alternatives (ideological constraint). Each constraint has distinct ε and beneficiary/victim profiles. The Marshall Plan story addresses the hybrid institutional mechanism; downstream stories address the specific coordination problems and extraction consequences it produces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1951_truman_marshall_plan_economic_assistance, powerless, 0.88).
constraint_indexing:directionality_override(sotu_1951_truman_marshall_plan_economic_assistance, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
