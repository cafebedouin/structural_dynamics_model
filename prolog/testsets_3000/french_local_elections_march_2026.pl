% ============================================================================
% CONSTRAINT STORY: french_local_elections_march_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_local_elections_march_2026, []).

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
 *   constraint_id: french_local_elections_march_2026
 *   human_readable: March 2026 French Municipal Elections
 *   domain: political/electoral
 *
 * SUMMARY:
 *   The March 15 and 22, 2026, French municipal elections serve as a Scaffold
 *   — a temporary institutional structure with genuine coordination function
 *   but rising theater ratio and embedded extraction mechanisms, designed
 *   with a concrete sunset clause through democratic experimentation. The
 *   constraint exhibits how electoral cycles function as coordination
 *   mechanisms that aggregate local preferences while simultaneously
 *   extracting consent from agents (protest voters, independent candidates,
 *   reform movements) who would prefer alternative institutional pathways.
 *   Unlike snares (which extract without coordination), the municipal
 *   election framework delivers real coordination benefits: local
 *   administrations genuinely reflect preference distributions, mayors gain
 *   legitimacy for executing local public goods, and community-level
 *   decision-making capacity is aggregated. However, the constraint also
 *   exhibits extraction: centrist coalitions benefit from incumbent advantage
 *   and media concentration; protest voters face forced choice between
 *   constrained options; independent candidates face ballot access barriers;
 *   and the administrative state maintains prefectural override authority
 *   despite electoral outcomes. The theater ratio has risen from 0.50 (2018,
 *   when local reform movements had real insurgent potential) to 0.65 (2026,
 *   as campaign spectacle has expanded while voter efficacy perception has
 *   declined). Critically, the constraint possesses a genuine sunset clause:
 *   proportional representation experimentation in Occitanie and Brittany,
 *   participatory budgeting pilots in major cities, and sortition proposals
 *   for regional councils are creating parallel institutional pathways.
 *   Within 10-15 years, if these experiments succeed, the reliance on
 *   five-year electoral cycles will diminish. This makes the 2026 election a
 *   window in which the constraint is Scaffold rather than entrenched Snare.
 *
 * KEY AGENTS:
 *   - Centrist Coalitions (institutional/arbitrage): Primary beneficiary — control media narratives, maintain coalition discipline, extract maximum electoral advantage from incumbent status
 *   - Incumbent Local Mayors (institutional/arbitrage): Secondary beneficiary — gain legitimacy for administrative action, control campaign narrative through incumbency, receive administrative resource concentration
 *   - Protest Voters (powerless/trapped): Primary victim — face forced choice between centrist consensus candidates or symbolic third-party votes with zero policy impact
 *   - Independent Candidates (moderate/constrained): Secondary victim — face ballot access barriers (10% signature threshold in most departments), media access disparities, campaign finance disadvantages vs party machinery
 *   - Reform Movements (organized/constrained): Strategic actor — view electoral constraint as temporary, invest in parallel institutional experiments (proportional representation pilots, participatory budgeting, sortition) to create exit paths
 *   - Administrative State / Prefectures (institutional/arbitrage): Institutional actor — maintains override authority regardless of electoral outcome, views election as legitimacy ritual for prefectural directives
 *   - Analytical Observer (analytical/analytical): Sees potential false summit — risks naturalizing contingent five-year electoral cycle as immutable feature of democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_local_elections_march_2026, 0.28).
domain_priors:suppression_score(french_local_elections_march_2026, 0.42).
domain_priors:theater_ratio(french_local_elections_march_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_local_elections_march_2026, extractiveness, 0.28).
narrative_ontology:constraint_metric(french_local_elections_march_2026, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(french_local_elections_march_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_local_elections_march_2026, scaffold).
narrative_ontology:human_readable(french_local_elections_march_2026, "March 2026 French Municipal Elections").
narrative_ontology:topic_domain(french_local_elections_march_2026, "political/electoral").

domain_priors:requires_active_enforcement(french_local_elections_march_2026).
narrative_ontology:has_sunset_clause(french_local_elections_march_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_local_elections_march_2026, centrist_coalitions).
narrative_ontology:constraint_beneficiary(french_local_elections_march_2026, incumbent_local_mayors).
narrative_ontology:constraint_beneficiary(french_local_elections_march_2026, administrative_continuity).
narrative_ontology:constraint_victim(french_local_elections_march_2026, radical_reform_movements).
narrative_ontology:constraint_victim(french_local_elections_march_2026, independent_candidates).
narrative_ontology:constraint_victim(french_local_elections_march_2026, protest_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTEST VOTER (SNARE) — Trapped within the electoral calendar with no exit option. Forced to choose between centrist consensus candidates or irrelevant third-party symbolic votes. Suppression is high: no recall mechanisms, no direct democracy tools, no bypass to administrative structures. The protest vote is absorbed into the system without structural consequence.
constraint_indexing:constraint_classification(french_local_elections_march_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INDEPENDENT CANDIDATE (TANGLED ROPE) — Constrained by ballot access rules, media access disparities, and campaign finance thresholds. But also benefits from the electoral framework: the municipal level permits grassroots organizing that higher-level elections do not. Mixed experience — coordination function exists (local voice aggregation) but asymmetric extraction persists (funding and media favor incumbents).
constraint_indexing:constraint_classification(french_local_elections_march_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT MAYOR NETWORK (ROPE) — Institutional actors with arbitrage options (career mobility, re-election probability, administrative continuity). The election is a coordination mechanism: it aggregates local preferences and legitimizes administrative action. The election benefits this agent group through resource control and electoral advantage. Low experienced extraction — beneficiary perspective.
constraint_indexing:constraint_classification(french_local_elections_march_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REFORM COALITION (SCAFFOLD) — Organized agents (environmental groups, regional autonomy movements, direct democracy advocates) view the March 2026 election as a temporary institutional structure. The constraint is a coordination mechanism with a genuine sunset: proportional representation experimentation (already adopted in some regions), sortition pilots, and participatory budgeting are creating parallel structures that will gradually reduce reliance on the five-year electoral cycle. The coalition has agency and sees an exit path. Theater ratio is moderate because the electoral campaign contains genuine debate alongside performative ritual.
constraint_indexing:constraint_classification(french_local_elections_march_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ADMINISTRATIVE STATE (PITON) — The centralized French administrative apparatus views municipal elections as a degraded mechanism for legitimizing prefectural directives. The election ritual persists (quinquennial theater) but the core function — translating local will into policy — has atrophied. Prefectures and national ministries execute most structural decisions; mayors execute or rubber-stamp. The election maintains theater but minimal functional verification of administrator competence. Theater ratio ≥ 0.70.
constraint_indexing:constraint_classification(french_local_elections_march_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN - FALSE SUMMIT) — From a civilizational perspective, periodic elections might appear as an immutable constraint of liberal democracy: communities must coordinate leadership succession, and periodic contests are a natural solution. However, the structural data reveals this as naturalization of a contingent institutional arrangement. Other democracies use sortition, multi-year administrations, or continuous recall mechanisms. The mountain classification is a false summit — the engine detects naturalization of institutional choice as law.
constraint_indexing:constraint_classification(french_local_elections_march_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_local_elections_march_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_local_elections_march_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_local_elections_march_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_local_elections_march_2026, TR),
    TR >= 0.70.

:- end_tests(french_local_elections_march_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The municipal election framework provides genuine coordination benefits (local preference aggregation, administrative legitimacy, community decision-making capacity). However, extraction mechanisms are clearly visible: incumbent advantage, media concentration favoring establishment parties, ballot access barriers for independents, and centralized administrative override. The extracted value is moderate because coordination benefits are real and widely distributed — it is not a pure Snare. The constraint is not extracting maximum value (which would be 0.46+). Suppression (0.42): Moderate. Barriers to alternative pathways are significant but not absolute: ballot access rules (10% signatures), campaign finance disparities, and media concentration create high suppression. However, proportional representation pilots and participatory budgeting mechanisms are lowering suppression by creating alternative verification pathways. Theater ratio (0.65): Moderate-high and rising. The electoral campaign contains genuine debate and preference aggregation, but campaign spectacle has expanded while voter efficacy perception has declined. Turnout dropped from 63.6% (2014) to 55.6% (2020), indicating rising theater perception. The election ritual persists partly through inertia and partly because alternatives are still nascent.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap: the powerless protest voter sees pure extraction (Snare — trapped with no functional voice), while the incumbent mayor sees pure coordination (Rope — the election legitimizes local governance). The independent candidate sees mixed experience (Tangled Rope — benefits from local organizing space but faces institutional barriers). The reform movement sees a temporary structure with declining force (Scaffold — open-science-style sunset as proportional representation and participatory mechanisms mature). The administrative state sees degraded theater (Piton — the election ritual persists while prefectural directives remain de facto supreme). The analytical observer risks seeing an immutable constraint (Mountain — periodic elections as natural law of democracy) but the structural data reveals this as naturalization of contingent institutional choice. The perspectival gaps are not merely observational differences — they reflect genuine structural asymmetries in how agents experience the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent mayors and centrist coalitions are structural beneficiaries (institutional power, arbitrage exit options, low d ≈ 0.15). They experience low effective extraction because the constraint coordinates voter preferences into support for their continued rule. Protest voters are structural victims (powerless, trapped exit) — they experience high d ≈ 0.95, maximum experienced extraction. Independent candidates occupy the middle (moderate power, constrained exit) — they benefit from local-level grassroots organizing but face institutional barriers, yielding d ≈ 0.60. Reform movements (organized power, constrained exit but with strategic agency) view the constraint through the sunset lens, yielding d ≈ 0.45 but with the critical caveat that they see d declining as parallel structures mature. The administrative state (institutional, arbitrage) experiences the election as a coordination mechanism for legitimizing its directives, not as a site of extraction — low d ≈ 0.10. The perspectival gap is large because the powerless agent (protest voter) experiences the constraint as a snare while the beneficiary (incumbent) experiences it as a rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clearly distinguishing coordination function from extraction mechanism. The election IS a coordination tool: it aggregates local preferences, produces legitimate administrations, and enables community-level decision-making. However, it ALSO exhibits extraction: incumbent advantage, media bias, ballot access barriers, and administrative override. This mixed character is precisely what makes it Scaffold rather than pure Rope or pure Snare. The sunset clause is credible because proportional representation pilots and participatory mechanisms are real structural alternatives being actively implemented. The scaffold classification resolves the ambiguity: the constraint is temporary coordination with declining extraction as alternatives mature, not a false binary between 'pure coordination' and 'pure extraction.' The key indicator is whether theater ratio continues rising (piton degradation) or whether alternative mechanisms actually reduce reliance on the five-year electoral cycle (scaffold sunset confirmation). Current trajectory suggests early-stage sunset confirmation — but the omega variables identify the decision points where trajectory could reverse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportional_representation_adoption,
    'Will France adopt proportional representation for municipal elections before 2030, creating genuine organizational space for third-force candidates?',
    'Legislative tracking; electoral commission reform proposals; regional experimentation outcomes from Occitanie and Brittany proportional pilots',
    'If adopted: scaffold sunset accelerates — extraction mechanism loses force as alternative pathways open. If rejected: constraint persists as snare/tangled rope beyond the nominal sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportional_representation_adoption, empirical, 'Whether proportional representation adoption accelerates the scaffold sunset').

omega_variable(
    direct_democracy_tool_effectiveness,
    'Do participatory budgeting and citizen assemblies at the municipal level actually devolve decision power or remain performative theater?',
    'Case studies of participatory budgeting outcomes in Grenoble, Lyon, and Marseille; tracking whether citizen recommendations are adopted vs symbolic',
    'If effective devolution: alternative institutional pathways truly reduce reliance on electoral constraint. If theater: reform movements face degraded alternatives, and scaffold sunset remains aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(direct_democracy_tool_effectiveness, empirical, 'Whether participatory mechanisms provide genuine power transfer or remain theater').

omega_variable(
    turnout_legitimacy_threshold,
    'What turnout level constitutes sufficient democratic legitimacy for municipal electoral outcomes? Does declining turnout (2014: 63.6%, 2020: 55.6%) signal constraint degradation or rational voter behavior under snare classification?',
    'Comparative analysis of turnout trends across European municipalities; survey data on voter satisfaction with electoral vs participatory mechanisms',
    'If declining turnout = rational protest: the snare classification is strengthened, and many protest voters are signaling accurate perception of powerlessness. If declining turnout = disengagement: the constraint may be shifting toward piton (maintained through inertia, not functional).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(turnout_legitimacy_threshold, empirical, 'Whether declining turnout indicates snare perception or piton-level inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_local_elections_march_2026, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flem_tr_t0, french_local_elections_march_2026, theater_ratio, 0, 0.5).
narrative_ontology:measurement(flem_tr_t4, french_local_elections_march_2026, theater_ratio, 4, 0.62).
narrative_ontology:measurement(flem_tr_t8, french_local_elections_march_2026, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(flem_be_t0, french_local_elections_march_2026, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(flem_be_t4, french_local_elections_march_2026, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(flem_be_t8, french_local_elections_march_2026, base_extractiveness, 8, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_local_elections_march_2026, resource_allocation).
narrative_ontology:affects_constraint(french_local_elections_march_2026, french_presidential_authority).
narrative_ontology:affects_constraint(french_local_elections_march_2026, prefectural_administrative_power).
narrative_ontology:affects_constraint(french_local_elections_march_2026, participatory_democracy_experimentation).

% DUAL FORMULATION NOTE:
% The municipal electoral cycle is downstream of the larger centralized French state structure (prefectural authority) but represents a distinct institutional constraint at the local coordination level. The upstream constraint (prefectural override authority) determines the scope within which municipal elections can operate; the municipal election constraint has its own extractiveness reflecting the incumbent-vs-challenger asymmetry. Participatory experimentation (sortition, proportional representation pilots) is a parallel constraint with lower extractiveness and shorter temporal horizon — it represents the structural alternative pathway that defines the scaffold sunset.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(french_local_elections_march_2026, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
