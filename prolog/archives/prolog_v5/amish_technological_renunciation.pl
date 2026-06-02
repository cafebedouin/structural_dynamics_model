% ============================================================================
% CONSTRAINT STORY: amish_technological_renunciation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amish_technological_renunciation, []).

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
 *   constraint_id: amish_technological_renunciation
 *   human_readable: The Television Test (Amish Renunciation)
 *   domain: social/technological/religious
 *
 * SUMMARY:
 *   The Amish technological renunciation constraint defines 'being Amish' as
 *   the collective discipline to refuse technologies deemed harmful to
 *   community integrity — most visibly television, but extending to
 *   electricity from the grid, automobiles, telephones in the home, and
 *   internet-connected devices. This constraint operates as both a
 *   coordination mechanism (binding ritual that preserves distinctive
 *   identity and enables decentralized governance) and an extraction
 *   mechanism (limiting individual opportunity, suppressing exit options,
 *   enforcing conformity through social and economic coercion). The tension
 *   between these functions creates a complex perspectival landscape. From
 *   the young person trapped between baptismal commitment and
 *   self-determination, the constraint is a pure snare — high suppression, no
 *   meaningful exit, maximum experienced extraction. From the church
 *   leadership, it is coordination — a mechanism for preserving cultural
 *   boundaries and decentralized authority without state interference. From
 *   the global observer viewing mass culture expansion, it is an extractive
 *   veto on human flourishing. The constraint exhibits high theater ratio
 *   (0.58) because the rule persists despite proliferating loopholes — cell
 *   phones in barns for business use, generators powered by diesel instead of
 *   grid electricity, computers in businesses while banned in homes —
 *   indicating the rule is increasingly maintained by social ritual rather
 *   than by genuine functional enforcement. The extractiveness value (0.62)
 *   reflects that the constraint does extract significant costs (limited
 *   education, constrained career options, suppressed information access)
 *   from trapped populations, but is not as severe as a pure snare (0.72+)
 *   because the community maintains genuine coordination benefits that
 *   partially legitimate the restriction.
 *
 * KEY AGENTS:
 *   - Rumspringa Adolescent: Primary victim (powerless/trapped) — faces maximum structural pressure to renounce technology with high cost of defection; bears extraction but has temporary escape window during rumspringa
 *   - Amish Youth Collective: Secondary victim (organized/constrained) — organized but with limited exit; experiences both coordination benefits (group identity) and extraction costs (opportunity suppression)
 *   - Baptized Adult Member: Moderate agent (moderate/constrained) — voluntarily committed to renunciation at baptism; experiences internalized coordination function but continues to bear suppression costs
 *   - Amish Church Leadership: Primary beneficiary (institutional/arbitrage) — maintains authority and cultural boundaries through renunciation discipline; experiences constraint as coordination mechanism with high exit optionality
 *   - Community Cohesion Mechanism: Abstract beneficiary — the collective good of distinctive identity and mutual aid that the constraint allegedly serves
 *   - Global Mass Culture: External observer (analytical/analytical) — views constraint as regressive suppression of information access and economic opportunity; maintains exclusion through normalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amish_technological_renunciation, 0.62).
domain_priors:suppression_score(amish_technological_renunciation, 0.68).
domain_priors:theater_ratio(amish_technological_renunciation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amish_technological_renunciation, extractiveness, 0.62).
narrative_ontology:constraint_metric(amish_technological_renunciation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(amish_technological_renunciation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amish_technological_renunciation, tangled_rope).
narrative_ontology:human_readable(amish_technological_renunciation, "The Television Test (Amish Renunciation)").
narrative_ontology:topic_domain(amish_technological_renunciation, "social/technological/religious").

domain_priors:requires_active_enforcement(amish_technological_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amish_technological_renunciation, community_cohesion_maintenance).
narrative_ontology:constraint_beneficiary(amish_technological_renunciation, amish_leadership_authority).
narrative_ontology:constraint_victim(amish_technological_renunciation, individual_autonomy).
narrative_ontology:constraint_victim(amish_technological_renunciation, youth_exit_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RUMSPRINGA ADOLESCENT (SNARE) — Faces the discipline of technological renunciation under maximum structural pressure. Trap is asymmetric: defection (adopting television, secular tech) means exit from family, church, inheritance, and community identity. Suppression is high because exit options are severely constrained — staying requires renouncing self-determination. Maximum experienced extraction from the perspective of the adolescent trapped between two worlds.
constraint_indexing:constraint_classification(amish_technological_renunciation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AMISH YOUTH COLLECTIVE (TANGLED ROPE) — Organized group with some agency but severely constrained exit. The constraint provides genuine coordination function (reinforces group identity, enables collective self-governance, maintains cultural boundary). But extraction is real: enforced renunciation limits individual opportunity exposure, suppresses alternative life paths, creates career/education constraints. Both benefits and costs present; enforcement is active.
constraint_indexing:constraint_classification(amish_technological_renunciation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: AMISH CHURCH LEADERSHIP (ROPE) — Experiences the constraint as a coordination mechanism: television renunciation is the binding ritual that preserves distinctive identity and enables decentralized self-governance without state interference. Leadership has exit options (modify rules, adapt policies) and experiences the constraint as serving its interests. High experienced benefit; extraction from church perspective runs inward from the community.
constraint_indexing:constraint_classification(amish_technological_renunciation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: BAPTIZED ADULT MEMBER (TANGLED ROPE) — Voluntarily accepted renunciation at baptism but continues to experience enforcement pressure. Has internalized the coordination function (community identity, shared values) and perceives genuine benefits. But also bears costs: suppressed business opportunities, limited access to information, educational constraints for children. Experienced extraction is lower than powerless youth because of voluntary commitment and partial agency, but suppression remains structural.
constraint_indexing:constraint_classification(amish_technological_renunciation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: EXTERNAL OBSERVER / GLOBAL MASS CULTURE VIEW (SNARE) — From outside, the constraint appears as high-suppression extraction: a regressive technology veto that limits human flourishing, information access, and economic opportunity for people born into the community. The observer sees the renunciation as imposed (born into it), not freely chosen, with coercive social enforcement. Theater ratio appears low from this view (the rule is genuinely enforced, not performative), but extractiveness and suppression are both high. Classification as Snare reflects the observer's judgment that community cohesion benefits accrue to leadership while costs fall on trapped youth.
constraint_indexing:constraint_classification(amish_technological_renunciation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: SECULAR MODERNITY APPARATUS (PITON) — The mass culture apparatus (television networks, consumer electronics industry, digital platforms) views Amish renunciation as a degraded and inertial constraint: Amish choose non-participation, but the apparatus itself maintains the exclusion through marketing, infrastructure, and normalization of mass media consumption. From the apparatus's view, the constraint is sustained by institutional inertia (Amish tradition) rather than by structural function for the broader system. It is a museum piece — theatrically preserved as exotic identity while the apparatus expands around it.
constraint_indexing:constraint_classification(amish_technological_renunciation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amish_technological_renunciation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amish_technological_renunciation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amish_technological_renunciation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(amish_technological_renunciation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(amish_technological_renunciation, TR),
    TR >= 0.70.

:- end_tests(amish_technological_renunciation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The constraint extracts significant costs from trapped youth — limited education (average 8th grade completion), constrained career options (limited to agriculture, construction, craft trades), suppressed information access, reduced economic mobility. However, this is not maximal extraction (0.72+) because: (1) the community provides genuine coordination benefits (mutual aid, social capital, cultural identity), (2) exit, while costly, is theoretically available during rumspringa, and (3) baptismal consent introduces an element of voluntary commitment for adults, even if the choice to be born Amish is not voluntary. The increasing theater ratio (0.35 → 0.58) reflects proliferating loopholes and the gap between stated rule (no electricity, no automobiles) and practiced enforcement (cell phones in barns, generators, business computers). Suppression (0.68): High. Suppression is structural and multi-channel: economic (limited occupational pathways), social (shunning/exclusion for defection), psychological (internalized shame around desire for technology), informational (limited access to mainstream education and media), and kinship-based (family separation for defectors). Youth have no legitimate exit path without bearing catastrophic costs. However, suppression is not total (0.80+) because rumspringa provides a temporary window, some communities practice less strict enforcement, and defection, while costly, is ultimately possible. Theater ratio (0.58): Moderate-high. The renunciation rule is functionally enforced — technology bans are real and persistently applied. But theater is rising because the enforcement is increasingly maintained through social ritual and selective interpretation (the loophole-studded middle ground between strict rule and complete adoption). The gap between the ideal rule (pure renunciation) and the practiced rule (context-dependent application) suggests growing functional atrophy masked by continued theatrical enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap between the trapped youth perspective (Snare) and the leadership perspective (Rope). The youth experience maximum suppression and extraction with minimal benefits — they bear the costs of information isolation while the community captures coordination value. The leadership experiences the constraint as coordination — a mechanism for preserving authority and cultural boundaries. The external global observer (Snare from their analytical vantage) judges the constraint as regressive extraction draped in coordination language. The baptized adult member occupies an intermediate position (Tangled Rope) — they have internalized the coordination function and participated in baptismal choice, reducing experienced extraction relative to the powerless youth, but they continue to bear suppression costs. This gap reveals the mandatrophy: is the constraint a genuine coordination mechanism (Rope) or an extraction mechanism (Snare) that justifies itself through coordination language? The answer is perspectival and structural: it is both. For the leadership, it genuinely coordinates. For trapped youth, it genuinely extracts. The classification as Tangled Rope from the organizational perspective captures this duality — the constraint has both a real coordination function (maintaining group identity and decentralized governance) and real asymmetric extraction (limiting youth opportunity), and enforcement is actively maintained by institutional actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural relationship between each agent and the renunciation discipline. The powerless youth has maximum d (≈0.95) because they are full targets: born into the constraint, trapped by family/community ties, bearing all costs of enforcement with no exit options. The church leadership has low d (≈0.10) because they are structural beneficiaries: they maintain authority, capture coordination value, and have escape velocity (ability to modify or relax rules). The organized youth collective has intermediate d (≈0.55) because they have some agency and some benefits (group identity) but remain constrained by enforcement and social pressure. The external observer has high d (≈0.75) in their analytical perspective because they see the constraint as externally imposed (born into it) with high suppression and extraction. The baptized adult's d is moderate (≈0.50) because voluntary commitment reduces experienced extraction relative to trapped youth, but ongoing suppression remains structural. These directionality values feed into the sigmoid function f(d) to produce experienced extractiveness (chi) — powerless youth with d=0.95 experience high chi (the constraint feels maximally extractive); leadership with d=0.10 experience negative chi (the constraint benefits them). The tangled rope classification emerges from the presence of both beneficiaries (leadership, community cohesion) and victims (youth, individual autonomy) with enforced boundary maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY: Is this Rope (pure coordination for community survival) or Snare (pure extraction disguised as coordination)? The constraint cannot be classified in a single type because it performs different structural functions for different agents. The mandatrophy is resolved by recognizing that the perspective from which one views the constraint determines its type: Leadership perspective yields Rope (genuine coordination). Youth perspective yields Snare (pure extraction). Analytical observer perspective yields Snare (regressive suppression). Adult member perspective yields Tangled Rope (mixed function). The engine should not resolve this to a single type but should report the perspectival presheaf. The risk of false natural law is HIGH: from the global observer perspective, the constraint classifies as Snare but with a temptation to naturalize it as 'inevitable cultural preservation' (false mountain). From the leadership perspective, the constraint classifies as Rope but with a temptation to naturalize it as 'necessary community survival mechanism' (false mountain). The structural data (increasing theater ratio, proliferating loopholes, rising youth exit rates) suggests the constraint is degrading toward Piton — maintained more by institutional inertia and social ritual than by genuine coordination function. The true story is that extractiveness and suppression are persistent and real, but are increasingly maintained through theater rather than through structural functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coercive_distinction,
    'To what extent is Amish technological renunciation voluntary choice vs. coercive social enforcement? Does baptismal consent render the constraint freely chosen?',
    'Longitudinal analysis of youth exit rates, cost of defection, comparative study of post-exit outcomes for Amish defectors vs. mainstream youth, interviews with youth during rumspringa regarding choice experience',
    'If genuinely voluntary: constraint shifts toward Rope/Scaffold from more perspectives. If coercive: shifts toward Snare/Tangled Rope. Determines whether beneficiary-victim relationship is symmetric or asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coercive_distinction, conceptual, 'Boundary between voluntary commitment and coercive enforcement in renunciation discipline').

omega_variable(
    community_cohesion_causation,
    'Does technological renunciation causally generate community cohesion, or does it proxy for deeper kinship/religious bonds that would persist without the technology ban?',
    'Comparison of Amish community metrics (social capital, mutual aid, intergenerational trust) across communities with varying strictness of technology restrictions. Cross-cultural comparison with other close-knit communities that use similar technology (Mennonites, Hutterites). Analysis of defectors'' reported loss of community after exit.',
    'If causal: renunciation is genuine coordination function (Rope element essential). If proxy: the constraint is pure extraction draped in coordination language (shifts toward Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_cohesion_causation, empirical, 'Whether technology renunciation causally produces community cohesion or merely correlates with it').

omega_variable(
    exit_cost_measurability,
    'What is the true economic and social cost of technological defection for Amish youth? Can this cost be quantified?',
    'Longitudinal tracking of economic outcomes (education level, income, wealth accumulation) for defectors vs. mainstream peers vs. stayers. Measurement of family relationship degradation, social network loss, inheritance impact. Survey of subjective psychological cost.',
    'High exit cost (>80th percentile vs. mainstream) → confirms Snare classification for powerless perspective. Low exit cost → reclassifies as Rope (renunciation is chosen, not enforced).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_measurability, empirical, 'True cost of technological defection for Amish youth and community members').

omega_variable(
    theater_degradation_direction,
    'Is the theater ratio increasing (rule becoming more performative, less functional) or stable (rule remains functionally enforced)? Are ''loopholes'' (cell phones in barns, business use of electricity) indicating rule decay?',
    'Historical analysis of enforcement strictness, documentation of loophole proliferation, community leadership commentary on rule modification pressure, comparison of youth perception of rule legitimacy over time',
    'If increasing theater: constraint is degrading toward Piton (functionally inertial). If stable/decreasing: constraint remains structurally enforced (remains Tangled Rope). Signals whether the constraint is sustainable across generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_degradation_direction, empirical, 'Trajectory of theater ratio and rule enforcement decay').

omega_variable(
    global_technology_inevitability,
    'Is the constraint''s persistence the result of organized discipline or merely the late-stage inertia of a declining population? Does continued renunciation represent genuine collective choice or demographic selection (only most committed remain)?',
    'Population dynamics analysis (birth rate, exit rate, retention rate by age cohort), comparison of constraint enforcement intensity to population decline trajectory, analysis of community sustainability models with and without technology restrictions',
    'If disciplined choice: constraint is structurally Tangled Rope with genuine coordination function. If demographic inertia: constraint is degrading Piton (selection bias making the remaining population appear more committed than they are).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_technology_inevitability, empirical, 'Whether constraint persistence reflects organizational discipline or demographic decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amish_technological_renunciation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amish_tr_t0, amish_technological_renunciation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(amish_tr_t20, amish_technological_renunciation, theater_ratio, 20, 0.48).
narrative_ontology:measurement(amish_tr_t40, amish_technological_renunciation, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(amish_be_t0, amish_technological_renunciation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(amish_be_t20, amish_technological_renunciation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(amish_be_t40, amish_technological_renunciation, base_extractiveness, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amish_technological_renunciation, global_infrastructure).
narrative_ontology:affects_constraint(amish_technological_renunciation, mennonite_selective_adoption).
narrative_ontology:affects_constraint(amish_technological_renunciation, hutterite_technology_governance).
narrative_ontology:affects_constraint(amish_technological_renunciation, religious_community_boundary_maintenance).

% DUAL FORMULATION NOTE:
% The Amish renunciation constraint decomposes into two related but distinct claims: (1) the television/technology ban as a coordination mechanism for preserving community identity (ε≈0.30, Rope/Scaffold at leadership level), and (2) the television/technology ban as an extraction mechanism limiting youth autonomy and opportunity (ε≈0.65, Snare at powerless level). These are not the same constraint viewed from different angles — they have different empirical signatures, different failure modes, and different causal mechanisms. However, the single constraint story captures both perspectives because they operate through the same institutional apparatus. If the analysis required separating them, two constraint files would be warranted: 'amish_community_preservation_via_renunciation' (ε≈0.30, Rope) and 'amish_youth_opportunity_suppression' (ε≈0.65, Snare). The present file treats them as unified because they are operationally inseparable in Amish practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amish_technological_renunciation, powerless, 0.95).
constraint_indexing:directionality_override(amish_technological_renunciation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
