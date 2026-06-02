% ============================================================================
% CONSTRAINT STORY: social_loafing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_loafing, []).

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
 *   constraint_id: social_loafing
 *   human_readable: Social Loafing (The Ringelmann Effect)
 *   domain: social/economic
 *
 * SUMMARY:
 *   Social loafing describes the systematic reduction in individual effort
 *   when working collectively compared to individual tasks, first documented
 *   by Ringelmann (1913) in rope-pulling experiments and formalized by Latané
 *   et al. (1979) as a function of group size, task clarity, and
 *   identifiability. The constraint reveals a Tangled Rope structure: the
 *   collective achieves real coordination benefits (shared workload,
 *   complementary skills, social safety), yet simultaneously enables
 *   extraction through effort reduction and free-riding. Individual loafers
 *   benefit from shared output while reducing personal cost; reliable
 *   contributors subsidize the group with excess effort; organizational
 *   leadership must enforce norms to maintain productivity. The constraint's
 *   extractiveness (0.38) reflects this hybrid: significant enough to matter
 *   for group output and individual fairness, but not so severe as to
 *   collapse coordination entirely. The theater ratio (0.58) captures the
 *   paradox that loafing discourse often treats behavioral defection as
 *   inevitable feature of human nature ('social loafing is real') when
 *   variation across team contexts (0-90% loafing rates) reveals it is highly
 *   contingent on institutional design choices: transparency, reputation
 *   systems, task saliency, psychological safety, and leadership quality. The
 *   constraint exhibits all six types from different structural positions,
 *   making it a diagnostic case for how the same phenomenon can be classified
 *   as both a natural feature of incentive misalignment (Mountain from
 *   universal view) and a contingent institutional problem (Tangled
 *   Rope/Scaffold from organizational redesign view).
 *
 * KEY AGENTS:
 *   - Reliable Contributors: Primary victims (powerless/trapped) — maintain high effort despite loafing, bear the subsidy cost, cannot exit without group failure
 *   - Free Riders: Primary beneficiaries (moderate/mobile) — reduce effort while capturing group output benefits, experience coordination without extraction pressure
 *   - Team Managers/Leaders: Institutional mediators (powerful/arbitrage) — observe loafing, must enforce norms through monitoring and incentive design, experience both coordination function and enforcement burden
 *   - Organizational Reform Coalition: Structured agents (organized/constrained) — implement transparency systems, reputation tracking, task rotation, and psychological safety protocols that create sunset for loafing through design rather than coercion
 *   - Industrial Efficiency Doctrine: Institutional narrative (institutional/arbitrage) — perpetuates 'loafing is inevitable' framing, maintains the constraint through theoretical inertia despite design solutions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (incentive misalignment, anonymity, low accountability) as immutable features of human motivation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_loafing, 0.38).
domain_priors:suppression_score(social_loafing, 0.45).
domain_priors:theater_ratio(social_loafing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_loafing, extractiveness, 0.38).
narrative_ontology:constraint_metric(social_loafing, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(social_loafing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_loafing, tangled_rope).
narrative_ontology:human_readable(social_loafing, "Social Loafing (The Ringelmann Effect)").
narrative_ontology:topic_domain(social_loafing, "social/economic").

domain_priors:requires_active_enforcement(social_loafing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_loafing, free_riders).
narrative_ontology:constraint_beneficiary(social_loafing, organization_leadership).
narrative_ontology:constraint_victim(social_loafing, high_effort_contributors).
narrative_ontology:constraint_victim(social_loafing, collective_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIABLE CONTRIBUTOR (SNARE) — Individual who consistently performs at high capacity discovers that effort is suppressed by free-rider presence. No mechanism to exit without bearing both social cost and performance collapse. Trapped within the group, witnessing their excess effort subsidizing others. Maximum experienced extraction relative to effort investment.
constraint_indexing:constraint_classification(social_loafing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MARGINAL FREE RIDER (ROPE) — Can reduce effort with minimal detection due to group anonymity and effort diffusion. Experiences constraint as pure coordination problem: the group is solving how to distribute collective task without precise individual monitoring. Has option to exit or defect; stays because costs of group membership (social sanction, reputational risk) are manageable. Experiences the constraint as workable.
constraint_indexing:constraint_classification(social_loafing, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: TEAM MANAGER (TANGLED ROPE) — Observes loafing but also benefits from group coordination. Must enforce effort norms to maintain output, which requires monitoring overhead and governance mechanisms. Experiences both coordination function (team achieves outputs together) and asymmetric extraction (must extract effort through enforcement mechanisms, threatening psychological safety). Has arbitrage options: can restructure teams, implement individual accountability, or accept lower output. Genuine hybrid.
constraint_indexing:constraint_classification(social_loafing, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ORGANIZATIONAL REFORM COALITION (SCAFFOLD) — Structured effort (team-based performance metrics, transparency tools, task rotation, reputation systems, trust-building practices) creates alternative pathways that reduce loafing without coercion. These interventions have a sunset: as psychological safety and transparent performance tracking mature, intrinsic motivation and social norm internalization can replace external enforcement. Extractiveness declines over implementation period as norms stabilize.
constraint_indexing:constraint_classification(social_loafing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INDUSTRIAL EFFICIENCY DOCTRINE (PITON) — Scientific management and groupwork literature extensively document loafing as unavoidable cost of collective action. The doctrine persists through repeated citation and institutional teaching despite evidence that well-designed teams (transparent metrics, shared goals, psychological safety) largely suppress loafing. The constraint is maintained through narrative inertia — 'social loafing is inevitable' — more than through actual structural necessity. High theater: the efficiency-doctrine framework treats loafing as natural law when it is actually contingent on team design.
constraint_indexing:constraint_classification(social_loafing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL INCENTIVE VIEW (MOUNTAIN) — From a civilizational/universal perspective, individual incentive divergence from collective outcome is an immutable feature of distributed action: when individual effort is not observable, rational agents will defect. This appears as a natural law of behavioral economics. However, the structural data contradicts the mountain classification — loafing rates vary dramatically (0-90% depending on group design, task saliency, reputation systems, leadership quality), revealing that incentive divergence is institutionally contingent, not a fixed limit. The engine will identify this as false summit: naturalization of a designable problem.
constraint_indexing:constraint_classification(social_loafing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_loafing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_loafing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_loafing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_loafing, TR),
    TR >= 0.70.

:- end_tests(social_loafing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Empirical loafing studies document 15-65% effort reduction depending on context, with average around 25-35% in unmodified conditions. This is meaningful but not catastrophic — groups retain 65-85% of potential output even with free-riding present. Suppression (0.45): Moderate. Loafing is partially suppressed by task identifiability, reputation systems, group cohesion, and social norms, but these suppressors are not maximally tight in most organizational contexts. Anonymous conditions remove suppressors entirely (loafing approaches 60%+); identifiable conditions with reputation tracking suppress loafing to 5-15%. Theater ratio (0.58): Elevated. The behavioral science literature on social loafing treats effort reduction as an inevitable consequence of incentive misalignment and diffusion of responsibility. Yet intervention studies show that well-designed teams (transparent metrics, clear task ownership, high psychological safety) achieve >90% of individual-task effort levels. The 'inevitability' framing is partially theatrical — it reflects the baseline case (anonymous conditions, low accountability, no reputation system) rather than an immutable property. As organizational design has matured (including tools like transparent task tracking, peer recognition systems, and structured accountability), the 'loafing is inevitable' narrative persists more through institutional inertia than structural necessity.
 *
 * PERSPECTIVAL GAP:
 *   The gap between free riders and reliable contributors is stark: the former experiences coordination (ability to reduce effort while still benefiting) while the latter experiences extraction (effort subsidy). The manager sees hybrid structure: real coordination benefit (team output often exceeds sum of carefully supervised individuals) plus enforcement burden. The reform coalition sees the constraint as contingent and solvable: team design variables (transparency, psychological safety, reputation systems) can shift the system toward Rope classification. The piton view recognizes that the 'loafing is inevitable' doctrine persists despite evidence that intervention reduces loafing by 50-70%. The natural law view risks claiming that incentive misalignment is immutable when the data show it depends on institutional design choices (anonymity, visibility, accountability, social distance, task saliency, group identity salience). The constraint exemplifies how the same behavioral phenomenon can classify as Mountain (inevitable human nature), Snare (extraction mechanism), Tangled Rope (hybrid coordination/extraction), or Scaffold (temporary problem with institutional solution), depending purely on the observer's structural relationship and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position and exit options. Free riders (moderate/mobile) experience low d: they have alternatives (can exit, can reduce effort without detection), benefit from the group, and experience the constraint as enabling rather than coercive. Their f(d) is low, making chi small or negative. Reliable contributors (powerless/trapped) experience high d: they cannot exit without bearing full output collapse, see others defecting, and subsidize group productivity with excess effort. Their f(d) is high, making chi large. Managers (powerful/arbitrage) experience intermediate d: they see the coordination value of teams but also the extraction cost of enforcement. They have options (restructure teams, change accountability mechanisms, implement transparency), so their d is moderate and chi is intermediate. The reform coalition (organized/constrained) experiences moderate d with declining trajectory: as transparency and reputation systems mature, the need for coercive enforcement decreases, so chi declines toward rope-like values. The piton perspective (institutional/arbitrage) experiences low d: the efficiency doctrine benefits from the existing narrative (academic publications citing loafing costs justify their own efficiency frameworks), but this is narrative extraction rather than structural extraction — hence piton classification despite arbitrage exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint exhibits genuine hybrid structure (Tangled Rope as claimed type) and avoids mandatrophy through three mechanisms: (1) Explicit beneficiary/victim declarations (free riders/loafers as beneficiaries, reliable contributors and collective productivity as victims) establish asymmetric extraction. (2) Real coordination function (teams achieve legitimate output beyond isolated individuals in many domains: innovation, problem-solving, task distribution, knowledge specialization) establishes coordination gate. (3) Active enforcement required: loafing suppression demands monitoring, transparency systems, reputation tracking, or psychological safety investments — not spontaneous emergence. The constraint is neither pure coordination (Rope) — reliable contributors are extracted — nor pure extraction (Snare) — real coordination benefits exist. The false natural law (Mountain perspective) is explicitly identified as such: the engine's analytical observer marks the 'incentive misalignment is immutable' framing as naturalization of contingent institutional design. The scaffold perspective (reform coalition) provides the mandatrophy path: as team design tools mature, the constraint's extractiveness declines and enforcement burden decreases, creating a genuine sunset where loafing transitions from structural extraction to residual individual choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effort_attribution_mechanism,
    'Can individual effort contributions be made visible without destroying psychological safety or introducing surveillance costs that exceed loafing suppression benefits?',
    'Empirical testing of reputation tracking systems, transparent task completion logging, and peer accountability mechanisms across diverse team contexts; measurement of effort attribution accuracy vs psychological safety outcomes',
    'If fully resolvable without cost: loafing becomes a design problem, not a constraint (approaches Rope/Scaffold from all perspectives). If surveillance costs exceed benefits: loafing becomes structural extraction mechanism (Snare/Tangled Rope from all perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effort_attribution_mechanism, empirical, 'Whether effort visibility can be achieved without psychological safety harm').

omega_variable(
    intrinsic_motivation_baseline,
    'In the absence of monetary incentives, reputation systems, or external enforcement, what fraction of individuals will naturally maintain high effort in anonymous collective tasks?',
    'Laboratory and field experiments with truly anonymous conditions, varying task saliency and social connection; cross-cultural comparative studies of collective action without formal monitoring',
    'If baseline > 50%: loafing is minority phenomenon, structurally addressable through norm-setting and minor design. If baseline < 20%: loafing is majority condition requiring active enforcement (Snare dominant). If baseline varies >30% with cultural/contextual factors: classification must decompose by context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_motivation_baseline, empirical, 'Baseline effort maintenance in anonymous collective work').

omega_variable(
    group_size_nonlinearity,
    'Is the loafing rate monotonically increasing with group size, or does it exhibit phase transitions (e.g., sharp drop in cooperative effort at n>5, then plateau)?',
    'Meta-analysis of group size studies; identification of inflection points and threshold effects; modeling of whether thresholds are cognitive limits or institutional design points',
    'If monotonic: suggests fundamental coordination problem (Mountain tendency). If threshold-based: suggests design points where small structural changes produce large behavioral shifts (Tangled Rope to Rope possible). If plateaus: suggests saturation where further growth adds no loafing (implication: problem solves itself at scale).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(group_size_nonlinearity, empirical, 'Nonlinearity and phase transitions in loafing with group size').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_loafing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(loaf_tr_t0, social_loafing, theater_ratio, 0, 0.4).
narrative_ontology:measurement(loaf_tr_t5, social_loafing, theater_ratio, 5, 0.5).
narrative_ontology:measurement(loaf_tr_t10, social_loafing, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(loaf_be_t0, social_loafing, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(loaf_be_t5, social_loafing, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(loaf_be_t10, social_loafing, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_loafing, resource_allocation).
narrative_ontology:affects_constraint(social_loafing, collective_action_problem).
narrative_ontology:affects_constraint(social_loafing, moral_hazard_in_teams).

% DUAL FORMULATION NOTE:
% Social loafing is downstream of more general free-rider problems in collective action, but represents a distinct constraint focused on effort reduction rather than resource contribution. The empirical extractiveness (0.38) is higher than the pure free-rider/collective action problem (which can approach 0.45-0.60 in unmonitored resource-pooling contexts) because loafing includes both output loss and motivational extraction. Related upstream constraint: incentive misalignment in team contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_loafing, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
