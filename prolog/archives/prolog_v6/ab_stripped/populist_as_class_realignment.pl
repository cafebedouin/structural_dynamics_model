% ============================================================================
% CONSTRAINT STORY: populist_as_class_realignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
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
 *   constraint_id: populist_as_class_realignment
 *   human_readable: Populist Realignment as Education-Based Class Stratification
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The populist realignment represents a fundamental restructuring of
 *   democratic class coalitions along education rather than income lines.
 *   Beginning in the 1980s and accelerating through the 2000s, working-class
 *   voters (defined by education rather than income) shifted from
 *   left-of-center parties advocating economic redistribution to right-wing
 *   populist parties offering cultural protection and anti-elite framing.
 *   This realignment exhibits tangled rope structure: it solves a genuine
 *   coordination problem (representing voters whose economic interests
 *   diverged from the cultural priorities of credentialed professionals who
 *   came to dominate left parties) while embedding asymmetric extraction
 *   (working-class voters receive symbolic recognition but minimal material
 *   redistribution, while right-wing parties and professional-class voters
 *   capture structural benefits). The constraint's rising theater_ratio
 *   reflects increasing performative anti-elitism that substitutes for
 *   material policy delivery. The rising suppression_requirement reflects the
 *   intensification of cultural wedge issues and identity-based framing that
 *   make exit from populist coalitions psychologically costly even as
 *   material conditions deteriorate.
 *
 * KEY AGENTS:
 *   - Non-College Working Class: Primary victim (powerless/identity_locked) — identity fused with cultural protection framing; votes against material self-interest
 *   - Right-Wing Populist Parties: Primary beneficiary (institutional/arbitrage) — captures working-class votes without redistributive commitment
 *   - Social Democratic Parties: Mixed victim/perpetrator (institutional/constrained) — loses historical base while serving credentialed professional interests
 *   - Credentialed Professional Class: Secondary beneficiary (institutional/arbitrage) — education stratification delivers material returns and political representation
 *   - Union Members: Secondary victim (moderate/constrained) — residual collective bargaining infrastructure fractured by cultural wedge issues
 *   - Redistributive Policy Infrastructure: Abstract victim (powerless/trapped) — welfare state coalitions collapse as cross-class solidarity erodes
 *   - Cross-Class Progressive Coalition: Organized agents (organized/mobile) — building alternative coalitions through universal material policies with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(populist_as_class_realignment, 0.38).
domain_priors:suppression_score(populist_as_class_realignment, 0.52).
domain_priors:theater_ratio(populist_as_class_realignment, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(populist_as_class_realignment, extractiveness, 0.38).
narrative_ontology:constraint_metric(populist_as_class_realignment, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(populist_as_class_realignment, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(populist_as_class_realignment, tangled_rope).
narrative_ontology:human_readable(populist_as_class_realignment, "Populist Realignment as Education-Based Class Stratification").
narrative_ontology:topic_domain(populist_as_class_realignment, "political_economy/comparative_politics/democratic_theory").

domain_priors:requires_active_enforcement(populist_as_class_realignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, right_wing_populist_parties).
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, credentialed_professional_class).
narrative_ontology:constraint_victim(populist_as_class_realignment, social_democratic_welfare_coalitions).
narrative_ontology:constraint_victim(populist_as_class_realignment, non_college_working_class).
narrative_ontology:constraint_victim(populist_as_class_realignment, redistributive_policy_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-COLLEGE WORKING CLASS (SNARE) — Identity-locked into cultural protection framing that delivers symbolic recognition but minimal material redistribution. The voter's class identity has been reframed from economic position to cultural status, making exit from the populist coalition psychologically costly even as material conditions deteriorate. Experiences high extraction: votes for parties that oppose the welfare policies that would materially benefit them.
constraint_indexing:constraint_classification(populist_as_class_realignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: UNION MEMBER (TANGLED ROPE) — Constrained by declining union density and institutional capture of left parties by credentialed professionals. Benefits from residual collective bargaining infrastructure but faces extraction through cultural wedge issues that fracture class solidarity. Can see both the coordination function (unions still deliver some wage premiums) and the extraction (leadership increasingly aligned with professional-class cultural priorities rather than shop-floor economic interests).
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RIGHT-WING POPULIST PARTY (ROPE) — Primary beneficiary. Captures working-class votes without committing to redistributive policy. Experiences the realignment as pure coordination: successfully aggregates voter discontent into electoral coalition. Low extraction because the party gains power and resources from the shift.
constraint_indexing:constraint_classification(populist_as_class_realignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SOCIAL DEMOCRATIC PARTY (TANGLED ROPE) — Victim of realignment but also complicit architect. Constrained by credentialed professional base that benefits from education stratification. The party still coordinates some welfare provision (genuine function) but extracts from its historical working-class base by prioritizing cultural liberalism over economic redistribution. Sees the constraint as mixed: losing electoral viability but retaining institutional position through professional-class support.
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDENTIALED PROFESSIONAL CLASS (ROPE) — Secondary beneficiary. Experiences the realignment as coordination: left parties now represent their cultural values and economic interests (meritocratic sorting, human capital investment, cosmopolitan identity). Low extraction because education stratification delivers material returns and the political system now reflects their preferences.
constraint_indexing:constraint_classification(populist_as_class_realignment, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CROSS-CLASS PROGRESSIVE COALITION (SCAFFOLD) — Organized agents building alternative coalitions that bridge education divides through universal material policies (healthcare, childcare, housing). Sees the realignment as temporary: education-based stratification can be overcome through policy that delivers tangible benefits across credential lines. Sunset logic: as material policies demonstrate cross-class appeal, cultural wedge issues lose salience.
constraint_indexing:constraint_classification(populist_as_class_realignment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both genuine coordination function (political system is aggregating real grievances about economic dislocation and cultural change) and asymmetric extraction (working-class voters receive symbolic recognition but minimal material redistribution while right-wing parties and credentialed professionals capture structural benefits). The realignment solves a real coordination problem (representing voters abandoned by left parties) while embedding extraction (cultural framing that obscures class-based redistribution).
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

:- end_tests(populist_as_class_realignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Working-class voters receive symbolic recognition and some non-redistributive benefits (trade protection, immigration restriction) but experience net extraction through foregone welfare state expansion and labor protections. The value reflects that extraction is substantial but not total — some material delivery occurs through populist industrial policy and cultural goods have real psychological value. Suppression (0.52): Moderate-high. Exit barriers include identity fusion with cultural framing, social network effects in working-class communities, media ecosystem reinforcement, and the absence of credible left alternatives offering both cultural respect and material redistribution. But suppression is not total — some voters do shift back, and cross-class coalitions remain possible. Theater ratio (0.45): Moderate. Anti-elite rhetoric and cultural performance increasingly substitute for material policy delivery, but populist parties do implement some tangible policies (immigration restriction, trade protection, symbolic cultural legislation). The theater has increased as initial policy momentum has given way to performative culture war maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The realignment appears as pure coordination (rope) from the perspective of right-wing populist parties and credentialed professionals — both groups benefit structurally. It appears as mixed coordination and extraction (tangled_rope) from the perspective of union members, social democratic parties, and the analytical observer — all see both the genuine coordination function (representing abandoned voters) and the asymmetric extraction (symbolic recognition substituting for material redistribution). It appears as pure extraction (snare) from the perspective of non-college working-class voters who are identity_locked into voting against their material interests. The cross-class progressive coalition sees it as temporary (scaffold) — a coordination failure that can be overcome through universal material policies. The gap reveals that the same structural phenomenon (education-based political stratification) serves different functions for different agents: coordination for beneficiaries, extraction for victims, and a solvable problem for organized alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   The non-college working class is identity_locked rather than trapped because the binding mechanism is cognitive/identity-based rather than material. These voters have structural mobility (they could vote for left parties offering redistribution, or organize independent working-class parties) but their identity has been constituted through the cultural protection frame, making exit psychologically costly. The identity lock is reinforced by social networks, media ecosystems, and the genuine cultural distance from credentialed professional-dominated left parties. Right-wing populist parties are institutional/arbitrage beneficiaries — they capture votes and power with minimal policy commitment and can exit to other coalitions if the populist wave recedes. Social democratic parties are institutional/constrained — they face high costs to recapture working-class voters (alienating professional base) but are not identity_locked (party leadership sees the strategic dilemma clearly). The credentialed professional class is institutional/arbitrage — education stratification delivers material returns regardless of which party governs, and they can shift between center-left and center-right options. Union members are moderate/constrained — they retain some organizational capacity and can see the extraction, but face high costs to exit (social pressure, declining union density, leadership capture).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the populist realignment is neither pure coordination (as right-wing parties claim) nor pure extraction (as left critics claim) but a tangled rope with both functions. The coordination function is real: working-class voters were genuinely abandoned by left parties that prioritized credentialed professional cultural preferences over economic redistribution. The extraction is also real: cultural framing delivers symbolic recognition but minimal material benefit while right-wing parties and professional-class voters capture structural advantages. The analytical perspective confirms tangled_rope: the realignment solves a genuine political representation problem (coordination) while embedding asymmetric material outcomes (extraction). The identity_locked classification for working-class voters is critical — it explains why extraction persists despite democratic exit options. The scaffold perspective from cross-class coalitions is not wishful thinking but a structural possibility: universal material policies can rebuild cross-education solidarity if they deliver tangible benefits that override cultural wedge issues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    education_stratification_permanence,
    'Is education-based political stratification a permanent feature of post-industrial democracies or a contingent outcome of specific policy choices?',
    'Cross-national comparison of countries with different education systems, credentialing regimes, and welfare state structures; longitudinal analysis of whether universal material policies can rebuild cross-education coalitions',
    'If permanent: realignment is structural feature of knowledge economies (closer to mountain from civilizational perspective). If contingent: realignment is policy-reversible (scaffold perspective gains strength).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(education_stratification_permanence, empirical, 'Whether education stratification is structural or contingent').

omega_variable(
    cultural_vs_material_primacy,
    'Do working-class voters prioritize cultural recognition over material redistribution, or does cultural framing substitute for unavailable economic alternatives?',
    'Survey experiments offering trade-offs between cultural policies and material benefits; analysis of voting behavior when credible redistributive options are available vs unavailable',
    'If cultural primacy: identity_locked classification is accurate and exit requires identity transformation. If material primacy: voters are constrained rather than identity_locked, and credible economic alternatives would shift behavior.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_material_primacy, empirical, 'Whether cultural voting reflects preference or constrained choice').

omega_variable(
    populist_policy_delivery,
    'Do right-wing populist parties deliver material benefits to working-class voters through non-redistributive means (trade protection, immigration restriction, industrial policy)?',
    'Policy analysis of populist governments: wage growth, employment rates, public service access for non-college workers under populist vs traditional left governance',
    'If material delivery occurs: extraction is lower than measured, coordination function is higher (closer to rope from working-class perspective). If no delivery: extraction is accurate or understated (closer to snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(populist_policy_delivery, empirical, 'Whether populist parties deliver material benefits to working-class base').

omega_variable(
    social_democratic_recapture_possibility,
    'Can social democratic parties recapture working-class voters without losing credentialed professional support, or is the coalition structurally incompatible?',
    'Analysis of parties attempting cross-class appeals: electoral outcomes, policy trade-offs, coalition stability; identification of successful bridging strategies vs failed attempts',
    'If recapture possible: scaffold perspective is realistic and sunset is achievable. If incompatible: realignment is stable equilibrium and social democratic parties face permanent extraction from historical base.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_democratic_recapture_possibility, empirical, 'Whether cross-education coalitions are structurally viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(populist_as_class_realignment, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pop_realign_theater_1980, populist_as_class_realignment, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pop_realign_theater_1990, populist_as_class_realignment, theater_ratio, 10, 0.35).
narrative_ontology:measurement(pop_realign_theater_2000, populist_as_class_realignment, theater_ratio, 20, 0.4).
narrative_ontology:measurement(pop_realign_theater_2010, populist_as_class_realignment, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(pop_realign_extract_1980, populist_as_class_realignment, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pop_realign_extract_1990, populist_as_class_realignment, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(pop_realign_extract_2000, populist_as_class_realignment, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(pop_realign_extract_2010, populist_as_class_realignment, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pop_realign_suppress_1980, populist_as_class_realignment, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(pop_realign_suppress_1990, populist_as_class_realignment, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(pop_realign_suppress_2000, populist_as_class_realignment, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(pop_realign_suppress_2010, populist_as_class_realignment, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(populist_as_class_realignment, identity_coordination).
narrative_ontology:affects_constraint(populist_as_class_realignment, welfare_state_retrenchment).
narrative_ontology:affects_constraint(populist_as_class_realignment, union_density_decline).
narrative_ontology:affects_constraint(populist_as_class_realignment, education_credentialing_expansion).
narrative_ontology:affects_constraint(populist_as_class_realignment, media_polarization_dynamics).

% DUAL FORMULATION NOTE:
% The populist realignment is downstream of education credentialing expansion (which created the education stratification) and upstream of welfare state retrenchment (which the realignment enables by fracturing redistributive coalitions). It is also mutually reinforcing with media polarization dynamics (cultural wedge issues drive media consumption patterns that reinforce identity lock).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(populist_as_class_realignment, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
