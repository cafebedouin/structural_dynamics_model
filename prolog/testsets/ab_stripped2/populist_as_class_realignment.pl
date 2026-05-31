% ============================================================================
% CONSTRAINT STORY: populist_as_class_realignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_populist_as_class_realignment, []).

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
 *   constraint_id: populist_as_class_realignment
 *   human_readable: Populist Realignment as Education-Based Class Restructuring
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The populist realignment represents a fundamental restructuring of
 *   democratic class coalitions driven by education-based stratification in
 *   post-industrial economies. Beginning in the 1990s and accelerating
 *   through the 2010s, working-class voters (defined by non-college
 *   education) shifted from supporting left-of-center parties promising
 *   economic redistribution to supporting right-wing populist parties
 *   offering cultural protection and anti-elite framing. This realignment
 *   exhibits tangled rope structure: it coordinates a genuine political
 *   demand (working-class cultural grievances against cosmopolitan
 *   governance) while extracting from working-class material interests by
 *   decoupling cultural and economic policy axes. The constraint's
 *   extractiveness (0.28) reflects moderate asymmetric costs: working-class
 *   voters receive cultural performance and symbolic recognition but bear
 *   policy costs through reduced redistribution, weakened labor protections,
 *   and regressive taxation. Suppression (0.42) captures the barriers to
 *   alternative coalition formation: education polarization, union decline,
 *   media fragmentation, and identity fusion with cultural protection
 *   framing. Theater ratio (0.35) reflects the gap between populist parties'
 *   rhetorical defense of 'workers' and their actual policy delivery, though
 *   this is lower than pure piton constraints because some genuine
 *   coordination function persists. The realignment is downstream of
 *   post_industrial_spatial_extraction (the geographic concentration of
 *   economic opportunity in educated urban centers) but constitutes a
 *   distinct political constraint with its own extraction dynamics.
 *
 * KEY AGENTS:
 *   - Deindustrialized Worker: Primary victim (powerless/identity_locked) — identity-fused with cultural protection narrative after economic abandonment; structurally mobile but cognitively trapped
 *   - Traditional Union Member: Secondary victim (moderate/constrained) — declining institutional power; benefits from populist cultural framing but loses redistributive infrastructure
 *   - Right-Wing Populist Party: Primary beneficiary (institutional/arbitrage) — captures working-class votes through cultural appeals without承诺 costly redistribution
 *   - Social Democratic Party: Institutional victim (moderate/constrained) — loses working-class base to education polarization; retains educated middle-class supporters
 *   - Cross-Class Progressive Coalition: Organized agents (organized/mobile) — building alternative coalition through material redistribution + cultural inclusion; sees sunset path
 *   - Third Way Framework: Degraded coordination (institutional/arbitrage) — policy consensus persists despite losing legitimating coalition; piton perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function alongside asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(populist_as_class_realignment, 0.28).
domain_priors:suppression_score(populist_as_class_realignment, 0.42).
domain_priors:theater_ratio(populist_as_class_realignment, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(populist_as_class_realignment, extractiveness, 0.28).
narrative_ontology:constraint_metric(populist_as_class_realignment, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(populist_as_class_realignment, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(populist_as_class_realignment, tangled_rope).
narrative_ontology:human_readable(populist_as_class_realignment, "Populist Realignment as Education-Based Class Restructuring").
narrative_ontology:topic_domain(populist_as_class_realignment, "political_economy/comparative_politics/democratic_theory").

domain_priors:requires_active_enforcement(populist_as_class_realignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, right_wing_populist_parties).
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, cultural_protection_coalitions).
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, anti_elite_movement_entrepreneurs).
narrative_ontology:constraint_victim(populist_as_class_realignment, social_democratic_welfare_coalitions).
narrative_ontology:constraint_victim(populist_as_class_realignment, traditional_labor_unions).
narrative_ontology:constraint_victim(populist_as_class_realignment, redistributive_policy_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEINDUSTRIALIZED WORKER (SNARE) — Identity-locked into cultural protection framing after economic abandonment by traditional left parties. Structurally mobile (could support alternative coalitions) but identity-fused with anti-elite cultural narrative. Experiences the realignment as extraction: promised economic protection, receives cultural performance while material conditions deteriorate. The identity lock prevents recognition that the populist coalition extracts labor support without delivering redistributive policy.
constraint_indexing:constraint_classification(populist_as_class_realignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: TRADITIONAL UNION MEMBER (TANGLED ROPE) — Constrained by declining union density and weakened collective bargaining power. Benefits from populist coalition's rhetorical defense of 'workers' against cosmopolitan elites, but bears cost of losing redistributive policy infrastructure. Genuine coordination function (populist parties do aggregate working-class grievances) alongside asymmetric extraction (cultural framing substitutes for economic delivery). Can exit to social democratic parties but at cost of abandoning cultural protection claims.
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RIGHT-WING POPULIST PARTY (ROPE) — Primary beneficiary. Experiences realignment as pure coordination: successfully aggregates working-class cultural grievances into electoral coalition without承诺 costly redistributive policy. Arbitrage exit options: can shift between cultural protection, anti-immigration, and anti-elite frames as electoral conditions change. Low effective extraction because the party captures the benefit flow.
constraint_indexing:constraint_classification(populist_as_class_realignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SOCIAL DEMOCRATIC PARTY (TANGLED ROPE) — Victim of realignment but retains institutional resources and educated middle-class base. Constrained by education polarization: cannot credibly appeal to non-college voters without alienating professional-class supporters. Benefits from coordination function (realignment clarifies new coalition boundaries) but bears extraction cost (loss of working-class base undermines redistributive policy capacity). Generational time horizon: sees potential to rebuild coalition through economic crisis or populist failure.
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CROSS-CLASS PROGRESSIVE COALITION (SCAFFOLD) — Organized agents (community organizers, progressive unions, issue-based movements) building alternative coalition that bridges education divide through material redistribution + cultural inclusion. Sees realignment as temporary: education-based polarization is contingent on neoliberal policy consensus, not structural necessity. Sunset logic: as populist parties fail to deliver economic gains, working-class voters become available for redistribution-focused appeals. Mobile exit: can shift between electoral and movement strategies.
constraint_indexing:constraint_classification(populist_as_class_realignment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: THIRD WAY FRAMEWORK (PITON) — Degraded coordination mechanism. The 1990s-2000s synthesis (market liberalization + social investment) persists in policy discourse despite losing working-class electoral base. Theater ratio reflects gap between continued policy implementation (austerity, labor flexibility, means-testing) and loss of legitimating coalition. Institutional actors maintain framework through inertia, not because it coordinates working-class interests.
constraint_indexing:constraint_classification(populist_as_class_realignment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (populist parties do aggregate previously unrepresented cultural grievances; education-based stratification does create real cleavage) alongside asymmetric extraction (cultural framing substitutes for economic redistribution; working-class voters bear policy costs while populist elites capture rents). Analytical classification matches claimed type. The realignment coordinates a real political demand (cultural protection from cosmopolitan elite governance) but extracts from working-class material interests by decoupling cultural and economic axes.
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(populist_as_class_realignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(populist_as_class_realignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(populist_as_class_realignment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(populist_as_class_realignment, TR),
    TR >= 0.70.

:- end_tests(populist_as_class_realignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The realignment coordinates genuine working-class cultural grievances (anti-cosmopolitan elite governance, immigration concerns, national identity protection) but extracts through policy substitution: cultural performance replaces economic redistribution. Working-class voters receive symbolic recognition and cultural validation but bear material costs through regressive taxation, welfare retrenchment, and labor deregulation under populist governments. The extraction is real but not extreme — some populist parties do deliver protectionist trade policy or immigration restriction that working-class voters value. Suppression (0.42): Moderate. Significant barriers to alternative coalition formation include education-based identity polarization (college vs non-college cultural divide), union density decline (from 35% to 15% in many OECD countries), media fragmentation reinforcing cultural sorting, and identity fusion where working-class voters internalize anti-elite cultural framing. But suppression is not total — cross-class progressive coalitions are viable, economic crises can break identity locks, and some working-class voters do switch based on material appeals. Theater ratio (0.35): Moderate-low. Gap between populist rhetoric ('defending workers') and policy delivery (regressive tax cuts, welfare cuts, labor flexibility) is substantial but not total. Some populist parties do deliver on protectionist trade policy, immigration restriction, or symbolic cultural victories that their base values. The theater is lower than pure piton constraints because genuine coordination function persists — populist parties do aggregate previously unrepresented cultural demands, even if economic delivery is weak.
 *
 * PERSPECTIVAL GAP:
 *   The realignment demonstrates how the same structural phenomenon appears differently across power positions and time horizons. The deindustrialized worker sees a snare: promised economic protection, receives cultural performance, cannot exit due to identity fusion. The traditional union member sees tangled rope: genuine cultural coordination alongside material extraction. The right-wing populist party sees pure rope: successful aggregation of working-class grievances into electoral coalition. The social democratic party sees tangled rope from a different angle: coordination function (realignment clarifies new coalition boundaries) alongside extraction (loss of base undermines redistributive capacity). The cross-class progressive coalition sees scaffold: temporary polarization that will resolve as populist economic failure creates opening for redistribution-focused appeals. The Third Way framework sees piton from its own perspective: degraded policy consensus maintained through inertia. The analytical observer sees tangled rope: genuine coordination of cultural demands alongside asymmetric material extraction. The perspectival gap reveals that 'populism' is not a single phenomenon but a constraint that coordinates some interests while extracting from others, with the balance depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The deindustrialized worker (powerless/identity_locked) is a victim with identity-locked exit, yielding high d (≈0.89) and high experienced extraction. The identity lock is cognitive rather than material: the worker could support alternative coalitions but cannot see this option from within the cultural protection frame. Traditional union members (moderate/constrained) are victims with constrained exit, yielding moderate-high d (≈0.65). They face real barriers (declining union power, education polarization) but retain some agency. Right-wing populist parties (institutional/arbitrage) are beneficiaries with arbitrage exit, yielding very low d (≈0.05) and negative effective extraction — they capture the benefit flow. Social democratic parties (moderate/constrained) are victims with constrained exit, yielding moderate d (≈0.55) — they lose working-class base but retain institutional resources and educated supporters. The cross-class progressive coalition (organized/mobile) sees lower extraction because they have agency and exit paths. The analytical observer uses canonical d for analytical power (≈0.72), experiencing the constraint as moderate extraction with clear coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that populist realignment is neither pure coordination (rope) nor pure extraction (snare) but a hybrid that coordinates cultural protection demands while extracting from material redistribution interests. The tangled rope classification captures this duality: right-wing populist parties do aggregate genuine working-class grievances against cosmopolitan elite governance (coordination function), but they do so by decoupling cultural and economic axes, delivering symbolic recognition while implementing regressive economic policy (extraction function). The identity-locked perspective (snare from deindustrialized worker) reveals the extraction mechanism: cultural framing prevents recognition that populist coalition abandons material interests. The scaffold perspective (cross-class progressive coalition) reveals the contingency: education polarization is not structural necessity but consequence of specific policy choices (Third Way neoliberalization) that can be reversed. The piton perspective (Third Way framework) reveals the degraded coordination: policy consensus persists despite losing legitimating coalition. No single type captures the full structure — the presheaf over observation positions is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    education_polarization_mechanism,
    'Is education-based political polarization driven by (a) material divergence in economic interests between college and non-college workers, or (b) cultural socialization in higher education institutions, or (c) selection effects where pre-existing values determine educational attainment?',
    'Longitudinal panel data tracking political attitudes before, during, and after college; cross-national comparison of education polarization in systems with different higher education structures; within-family sibling comparisons controlling for shared background',
    'If (a) material: realignment is structural response to post-industrial economy (coordination). If (b) socialization: realignment is contingent on specific institutional configurations (extractive). If (c) selection: realignment reflects pre-existing cleavage made visible by education sorting (mixed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(education_polarization_mechanism, empirical, 'Mechanism driving education-based political polarization').

omega_variable(
    populist_policy_delivery,
    'Do right-wing populist parties in government deliver material economic benefits to working-class voters at rates comparable to social democratic parties, or does cultural performance substitute for redistributive policy?',
    'Comparative policy analysis: welfare spending, labor protections, tax progressivity, wage growth under populist vs social democratic governments; within-country before/after comparisons; voter income trajectory analysis',
    'If comparable delivery: realignment is genuine preference shift (coordination). If substitution: realignment is extraction mechanism where cultural framing masks policy abandonment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(populist_policy_delivery, empirical, 'Whether populist parties deliver material benefits to working-class base').

omega_variable(
    identity_lock_reversibility,
    'Is working-class identification with right-wing populist cultural framing reversible through material economic appeals, or has the identity fusion become self-sustaining independent of policy outcomes?',
    'Panel surveys tracking vote switching in response to economic shocks, policy changes, or populist government performance; experimental studies testing economic vs cultural message framing; historical comparison to previous realignment episodes',
    'If reversible: identity lock is contingent, scaffold perspective is viable. If self-sustaining: identity lock is structural, snare perspective dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of working-class populist identity fusion').

omega_variable(
    third_way_counterfactual,
    'Would social democratic parties have retained working-class support if they had rejected Third Way neoliberalization and maintained traditional redistributive platforms, or was realignment inevitable given post-industrial economic transformation?',
    'Cross-national comparison: countries where social democratic parties rejected Third Way (if any exist) vs those that adopted it; within-country regional variation in party positioning; historical comparison to interwar period social democratic coalitions facing similar structural pressures',
    'If avoidable: Third Way policy choice was extractive, realignment is contingent. If inevitable: post-industrial economy structurally undermines class-based redistribution coalitions regardless of party strategy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_way_counterfactual, conceptual, 'Counterfactual necessity of Third Way policy path').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(populist_as_class_realignment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1990, populist_as_class_realignment, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_2000, populist_as_class_realignment, theater_ratio, 10, 0.3).
narrative_ontology:measurement(theater_2010, populist_as_class_realignment, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(extract_1990, populist_as_class_realignment, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(extract_2000, populist_as_class_realignment, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(extract_2010, populist_as_class_realignment, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1990, populist_as_class_realignment, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(suppress_2000, populist_as_class_realignment, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(suppress_2010, populist_as_class_realignment, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(populist_as_class_realignment, identity_coordination).

% DUAL FORMULATION NOTE:
% Populist realignment is downstream of post_industrial_spatial_extraction (geographic concentration of opportunity in educated urban centers) but constitutes a distinct political constraint. The upstream constraint describes the material basis (spatial inequality, education returns, deindustrialization); this constraint describes the political response (coalition restructuring, cultural framing, policy substitution). They have different ε values because they measure different observables: spatial extraction measures geographic income divergence; populist realignment measures working-class vote share and policy delivery gaps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
