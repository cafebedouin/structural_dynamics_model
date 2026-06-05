% ============================================================================
% CONSTRAINT STORY: sotu_2006_bush_iraqi_inclusive_government_building
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2006_bush_iraqi_inclusive_government_building, []).

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
 *   constraint_id: sotu_2006_bush_iraqi_inclusive_government_building
 *   human_readable: Institutional Reconstruction of Iraqi Government Toward Inclusive Representation to Marginalize Insurgency
 *   domain: governance/counter-insurgency/institution-building
 *
 * SUMMARY:
 *   In 2006, the U.S. Bush administration adopted a counter-insurgency
 *   strategy centered on institutional reconstruction of the Iraqi government
 *   toward inclusive sectarian representation. The mechanism: by offering
 *   Sunni factions genuine political participation in government structures,
 *   the strategy aimed to remove political grievances driving recruitment
 *   into insurgent networks while simultaneously co-opting potential
 *   opposition into state-aligned institutions. This creates a structural
 *   constraint combining genuine coordination (solving the collective action
 *   problem of sectarian coexistence) with asymmetric extraction (suppressing
 *   military alternatives and enforcing compliance through coalition military
 *   presence). The constraint exhibits high theater—public narratives
 *   emphasize democratic institution-building while actual operations
 *   prioritize counter-insurgency objectives. Extractiveness increased from
 *   2004-2006 as institutional frameworks were formalized, peaked around 2006
 *   as political power-sharing was operationalized, then stabilized as
 *   institutional routines became established. Theater ratio increased
 *   throughout as the gap between stated institutional capacity and actual
 *   dependence on coalition enforcement widened.
 *
 * KEY AGENTS:
 *   - U.S. Military Counter-Insurgency Strategy: Primary beneficiary (institutional/arbitrage) — direct reduction in active opposition, creation of allied political structure, strategic success metric
 *   - Iraqi Civilian Population: Primary beneficiary (institutional/arbitrage) — reduction in sectarian violence, expanded political voice, access to distributed state resources
 *   - Insurgent Recruitment Networks: Primary victim (powerless/trapped) — political space closure, delegitimization of anti-government narrative, loss of recruitment grounds
 *   - Sunni Political Factions: Secondary victim/mixed (moderate/constrained) — offered political inclusion but required to fracture from insurgent-aligned hardliners, bearing cost of organizational division
 *   - International Coalition Presence: Enforcer/temporary (organized/constrained) — maintains institutional framework through military suppression of anti-government alternatives; sees own role as temporary but remains essential to function
 *   - Shiite-Dominated Government: Secondary beneficiary (institutional/arbitrage) — political legitimacy increased through inclusive structure; security burden shared across sectarian lines
 *   - Analytical Observer: Sees tangled mechanism (analytical/analytical) — genuine coordination function (sectarian coexistence) embedded in asymmetric power structure (external military enforcement)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2006_bush_iraqi_inclusive_government_building, 0.58).
domain_priors:suppression_score(sotu_2006_bush_iraqi_inclusive_government_building, 0.67).
domain_priors:theater_ratio(sotu_2006_bush_iraqi_inclusive_government_building, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2006_bush_iraqi_inclusive_government_building, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2006_bush_iraqi_inclusive_government_building, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(sotu_2006_bush_iraqi_inclusive_government_building, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2006_bush_iraqi_inclusive_government_building, tangled_rope).
narrative_ontology:human_readable(sotu_2006_bush_iraqi_inclusive_government_building, "Institutional Reconstruction of Iraqi Government Toward Inclusive Representation to Marginalize Insurgency").
narrative_ontology:topic_domain(sotu_2006_bush_iraqi_inclusive_government_building, "governance/counter-insurgency/institution-building").

domain_priors:requires_active_enforcement(sotu_2006_bush_iraqi_inclusive_government_building).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2006_bush_iraqi_inclusive_government_building, iraqi_civilians_violence_reduction).
narrative_ontology:constraint_beneficiary(sotu_2006_bush_iraqi_inclusive_government_building, us_military_counter_insurgency_strategy).
narrative_ontology:constraint_victim(sotu_2006_bush_iraqi_inclusive_government_building, insurgent_recruitment_networks).
narrative_ontology:constraint_victim(sotu_2006_bush_iraqi_inclusive_government_building, sunni_political_marginalization_risk).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSURGENT RECRUITMENT NETWORK (SNARE) — Faces structural collapse as political representation channels expand. Cannot exit or negotiate from position of shrinking legitimacy; confronts maximum suppression via state monopoly on violence and international coalition presence. The institutional co-optation mechanism directly targets recruitment narrative. No alternatives exist within constraint.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_inclusive_government_building, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUNNI POLITICAL FACTIONS (TANGLED ROPE) — Constrained by post-2003 demographic realities and institutional architecture but offered genuine political participation in expanded government. Coordination function: inclusive representation solves Shiite-majority domination fears. But extraction persists: Sunni factions sacrifice insurgent-aligned hardliners to enter government, bearing cost of organizational fracture and ideological compromise. Mixed position — some agency in negotiating terms, but significant structural constraints on exit from this bargain.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_inclusive_government_building, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. MILITARY COUNTER-INSURGENCY STRATEGY (ROPE) — Benefits directly from inclusive government architecture: reduces active opposition, legitimizes coalition presence, creates allied political structure. Experiences the constraint as coordination: building inclusive representation IS the counter-insurgency strategy. High agency (can modulate pressure, resources, timelines); arbitrage exit available via redeployment. Primary beneficiary.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_inclusive_government_building, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IRAQI CIVILIAN POPULATION — VIOLENCE REDUCTION FRAME (ROPE) — Genuine coordination function: inclusive government reduces sectarian recruitment incentives and distributes state resources across factions, lowering overall violence. Civilian casualties decline as insurgent networks lose recruitment legitimacy. Beneficiary with arbitrage-like agency: civilians can exit violence zones, participate in political processes. Low extraction — primary benefit flows toward this group.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_inclusive_government_building, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL COALITION PRESENCE (SCAFFOLD) — Temporary coordination mechanism with defined sunset: inclusive government is intended to eventually reduce coalition force levels and enable Iraqi state autonomy. Organized actors (military command, State Department, coalition partners) maintain the mechanism through active enforcement but see its own trajectory as temporary. Theater component: public statements about 'standing up' Iraqi security forces create performance layer above actual institutional capacity. Sunset logic: as Iraqi institutions mature, coalition presence phases out.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_inclusive_government_building, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LIBERAL INTERNATIONALISM NORM — NATION-BUILDING FAITH (PITON) — At civilizational scale, institutional reconstruction represents faith in democratic peace theory and inclusive governance as conflict solution. This perspective is largely performative: actual outcomes depend on local power dynamics, resource distribution, and sectarian loyalty more than institutional design. The norm persists through inertia and policy commitment despite historical evidence of degraded function in fragile states. Theater ratio is high at this scale — the public narrative (building democracy to solve conflict) persists despite institutional capacity gaps.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_inclusive_government_building, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (inclusive representation does reduce insurgent recruitment legitimacy) embedded within asymmetric power structure (U.S. coalition designs and enforces the institution-building process; local actors execute within externally-defined parameters). The constraint coordinates sectarian coexistence while extracting compliance from subordinate actors via suppression of military alternatives. Neither pure coordination nor pure extraction — the mechanism requires both. Classification reflects both genuine collective action problem solution AND asymmetric enforcement structure.
constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_inclusive_government_building, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2006_bush_iraqi_inclusive_government_building_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_inclusive_government_building, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2006_bush_iraqi_inclusive_government_building, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2006_bush_iraqi_inclusive_government_building, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2006_bush_iraqi_inclusive_government_building, TR),
    TR >= 0.70.

:- end_tests(sotu_2006_bush_iraqi_inclusive_government_building_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint combines genuine coordination benefits (reduction in sectarian violence through inclusive representation) with significant extraction (insurgent networks lose recruitment grounds via political co-optation; Sunni factions sacrifice military-aligned hardliners to gain political voice; all actors remain embedded in coalition-enforced institutional framework). The initial extractiveness (0.35) reflects that in 2004 the framework was nascent and incomplete. Peak extractiveness (0.60) at 2006 reflects full operationalization of institutional power-sharing arrangements—the mechanism is now maximally constraining insurgent alternatives while maximally extracting compliance from incorporated actors. The modest decline (0.58) by end of interval reflects stabilization of institutional routines and emerging local ownership. Suppression (0.67): High. Suppression is maintained through coalition military presence, the monopoly on legitimate coercive force, and the institutional closure of military channels—actors face both external military enforcement and internal institutional constraints. Insurgents confront maximum suppression: U.S. military combat operations continue throughout interval, Iraqi security forces are built as counter-insurgent organizations, and political pathways narrow to those approved by coalition-backed government. Theater ratio (0.58): Moderate-high. Significant gap exists between stated institutional capacity and actual dependence on coalition presence. Democratic elections are held (performative success) while security sectors remain coalition-directed. Institutional checks and balances are written into constitutions (theater) while sectarian distribution of power remains negotiated informally. Inclusive representation is announced (theater) while resource allocation follows coalition strategic priorities (function).
 *
 * PERSPECTIVAL GAP:
 *   The maximal perspectival gap appears between the beneficiary's experience (U.S. military sees Rope: strategic problem solved) and the victim's experience (insurgent networks see Snare: structural collapse with no exit). This gap is not observational ambiguity—it reflects genuine structural difference in how the constraint operates for different agents. The gap also appears between inter-institutional perspectives: the coalition-backed government sees the constraint as Rope (coordination mechanism), while the insurgent counter-government sees it as Snare (existential threat). The same institutional structure appears as coordination to those who designed it and gain from it, but as extraction and co-optation to those whose alternatives are being systematically closed. This is the diagnostic signature of a Tangled Rope: multiple institutional actors with incompatible experiences of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (U.S. military, Iraqi civilian violence reduction) have high agency (arbitrage exit available) and occupy the extraction-receiving end of the flow—the constraint transfers resources toward them (security, political legitimacy, reduced opposition costs). Their derived d values are low (0.15-0.20), yielding f(d) ≈ -0.01 to 0.05, producing negative or minimal effective extractiveness. Victims (insurgent networks, constrained Sunni factions) have minimal agency and occupy the extraction-paying end—the constraint removes their resources (recruitment grounds, organizational autonomy, military options). Insurgent networks have d ≈ 0.95 (trapped), yielding f(d) ≈ 1.42. Sunni political factions have d ≈ 0.72 (constrained), yielding f(d) ≈ 1.10. The analytical observer computing directionality from structural data sees: beneficiaries with arbitrage (low d), victims with trapped or constrained exit (high d), and the scope modifier σ(S)=1.0 (national scope) applied uniformly. The resulting χ = 0.58 × f(d) × 1.0 produces effective extractiveness values ranging from negative (beneficiaries) to 0.82 (trapped victims).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Tangled Rope is the analytically correct classification despite surface ambiguity. The mechanism genuinely coordinates sectarian coexistence (Rope function: reduces collective action problem of inter-group violence) while genuinely extracting compliance from subordinate actors through military suppression (Snare function: closes military alternatives, forces institutional embedding). Neither function is reducible to the other. A pure Rope classification would overstate the agency available to subordinate actors (they can negotiate terms but cannot exit the framework). A pure Snare classification would understate the genuine coordination benefit (violence reduction is real and substantial). The Tangled Rope classification captures both: there is a real collective action solution (inclusive representation does reduce insurgency recruitment) AND there is real asymmetric extraction (the solution is imposed by external military force, not negotiated among equals). The mandatrophy is resolved by recognizing that this is not observational ambiguity (can we see through the fog?) but structural reality (the constraint genuinely does both things). The perspectival gap (beneficiary sees Rope, victim sees Snare, observer sees Tangled Rope) reflects this structural truth, not analytical disagreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inclusive_government_co_optation_sincerity,
    'Is inclusive government architecture a genuine attempt to distribute political power, or a co-optation mechanism designed primarily to eliminate military threat by offering powerless actors token participation?',
    'Longitudinal analysis of resource distribution (budget allocations, ministry staffing, security force composition) across sectarian lines; comparison of promised power-sharing against actual decision-making authority; tracking of institutional veto points accessible to included factions',
    'If genuine: constraint is predominantly Rope from all perspectives — coordination function is primary. If co-optation theater: constraint is Tangled Rope → Snare gradient — asymmetric extraction dominates, with participation serving suppression logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusive_government_co_optation_sincerity, empirical, 'Whether inclusive government is genuine power-sharing or co-optation theater').

omega_variable(
    sunni_insurgent_supply_chain_interruption,
    'Does removing political grievance through inclusive government actually reduce insurgent recruitment, or do military-ideological factors dominate recruitment such that institutional inclusion is strategically irrelevant?',
    'Recruitment flow analysis: correlation between Sunni political inclusion milestones and active insurgent roster growth/decline; survey data from captured/defected insurgents on primary recruitment drivers (political marginalization vs religious ideology vs anti-occupation sentiment); comparison of recruitment rates in included vs excluded population segments',
    'If grievance-driven: inclusive government is functionally solving a real coordination problem; Rope classification dominates. If ideology-driven: inclusive government is theater blocking insurgent narrative without addressing actual recruitment mechanisms; extractiveness increases, classification shifts toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunni_insurgent_supply_chain_interruption, empirical, 'Whether insurgent recruitment is primarily grievance-driven or ideology-driven').

omega_variable(
    coalition_enforcement_capacity_sustainability,
    'Can inclusive government institutions sustain themselves without active coalition military enforcement, or does the constraint collapse once external suppression is removed?',
    'Institutional sustainability modeling: assess whether built institutions have independent legitimacy, revenue bases, enforcement capacity, or remain dependent on coalition presence for all three; timeline analysis of coalition withdrawal vs institutional maturation; post-withdrawal institutional performance (2011 onward)',
    'If sustainable: Scaffold classification is accurate — sunset is real. If dependent: constraint becomes Piton (maintained by external theater) or reverts to sectarian competition (Snare) once suppression removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_enforcement_capacity_sustainability, empirical, 'Whether inclusive government institutions can sustain autonomously').

omega_variable(
    sectarian_demographic_trajectory_permanence,
    'Does inclusive representation framework remain viable if sectarian demographic composition shifts during the reconstruction period due to internal displacement, migration, or violence?',
    'Population flow analysis: track displacement patterns, refugee movements, return rates across sectarian lines during 2004-2008 interval; model institutional power-sharing assumptions against actual demographic shifts; assess whether ''inclusive'' representation becomes fictional if underlying demographics diverge from constitutional assumptions',
    'If demographics remain relatively stable: institutional assumptions hold. If significant shifts occur: power-sharing formula becomes contested, extraction mechanism increases (suppression required to maintain framework that no longer reflects population), classification shifts Tangled Rope → Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sectarian_demographic_trajectory_permanence, empirical, 'Whether sectarian demographic stability persists throughout reconstruction period').

omega_variable(
    u_s_coalition_exit_conditionality,
    'Is coalition presence withdrawal genuinely conditional on Iraqi institutional maturity, or is withdrawal timeline driven by political costs in U.S. domestic politics, with institutional readiness narrative serving as cover story?',
    'Timeline analysis: compare stated institutional readiness benchmarks against actual coalition withdrawal timeline; assess whether benchmarks were adjusted retroactively to match political decisions; examine policy documents and decision records for primacy of institutional vs domestic political factors',
    'If institution-conditional: Scaffold classification holds — sunset is structural. If politically-driven: coalition presence is maintained or withdrawn regardless of institutional capacity; constraint becomes Piton (extractive framework maintained by political rather than functional logic) or collapses entirely upon withdrawal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(u_s_coalition_exit_conditionality, conceptual, 'Whether coalition exit is genuinely conditional on institutional readiness or driven by domestic politics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2006_bush_iraqi_inclusive_government_building, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iraqi_gov_tr_t0, sotu_2006_bush_iraqi_inclusive_government_building, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iraqi_gov_tr_t2, sotu_2006_bush_iraqi_inclusive_government_building, theater_ratio, 2, 0.51).
narrative_ontology:measurement(iraqi_gov_tr_t4, sotu_2006_bush_iraqi_inclusive_government_building, theater_ratio, 4, 0.62).
narrative_ontology:measurement(iraqi_gov_tr_t6, sotu_2006_bush_iraqi_inclusive_government_building, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(iraqi_gov_be_t0, sotu_2006_bush_iraqi_inclusive_government_building, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iraqi_gov_be_t2, sotu_2006_bush_iraqi_inclusive_government_building, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(iraqi_gov_be_t4, sotu_2006_bush_iraqi_inclusive_government_building, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(iraqi_gov_be_t6, sotu_2006_bush_iraqi_inclusive_government_building, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2006_bush_iraqi_inclusive_government_building, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2006_bush_iraqi_inclusive_government_building, iraqi_sunni_political_participation).
narrative_ontology:affects_constraint(sotu_2006_bush_iraqi_inclusive_government_building, us_military_counter_insurgency_strategy).
narrative_ontology:affects_constraint(sotu_2006_bush_iraqi_inclusive_government_building, sectarian_violence_dynamics_iraq_2004_2008).

% DUAL FORMULATION NOTE:
% This constraint is a structural mechanism embedding counter-insurgency strategy within institution-building narrative. It affects downstream constraints on Sunni political participation (which political pathways are available) and counter-insurgency effectiveness (what recruitment alternatives are available). It is affected by upstream dynamics of sectarian violence intensity and insurgent organizational capacity. All three form a causal family where institutional reconstruction is the lynchpin mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2006_bush_iraqi_inclusive_government_building, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
