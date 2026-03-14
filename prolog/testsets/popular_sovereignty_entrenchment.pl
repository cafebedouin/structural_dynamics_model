% ============================================================================
% CONSTRAINT STORY: popular_sovereignty_entrenchment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_popular_sovereignty_entrenchment, []).

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
 *   constraint_id: popular_sovereignty_entrenchment
 *   human_readable: Popular Sovereignty Entrenchment
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Popular Sovereignty Entrenchment describes the structural constraint
 *   whereby polities declare commitment to governance 'by the people' while
 *   systematically gatekeeping who counts as 'the people' and through what
 *   mechanisms they may exercise agency. The constraint exhibits a
 *   fundamental tension: popular sovereignty claims delegitimize coercive
 *   rule ('all legitimate power derives from the consent of the governed'),
 *   yet those holding power use entrenchment mechanisms—constitutional
 *   supermajority requirements, franchise restrictions, gerrymandering,
 *   ballot access rules, voter suppression, information asymmetries—to
 *   prevent the demos from actually overthrowing them. This creates a hybrid
 *   coordination-extraction mechanism. The sovereignty framework genuinely
 *   coordinates action (citizens can appeal to 'the people' as a legitimacy
 *   standard), yet it simultaneously extracts by making such appeals formally
 *   possible but practically futile. The constraint's extractiveness (0.58)
 *   reflects this hybrid: moderate, not total, because sovereignty rhetoric
 *   does create real constraints on power holders' behavior, even as
 *   gatekeeping prevents it from being fully realized. Theater ratio (0.65)
 *   captures the performative gap between declarations of popular sovereignty
 *   and actual decision-making architecture.
 *
 * KEY AGENTS:
 *   - Disenfranchised Populations: Primary victim (powerless/trapped) — excluded from suffrage, participation, or meaningful representation; bears full cost of closure
 *   - Established Political Elites: Primary beneficiary (institutional/arbitrage) — control implementation of sovereignty while claiming its authority; capture legitimacy of 'the people' while determining who that means
 *   - Organized Political Opposition: Secondary actor (moderate/constrained) — constrained by gatekeeping mechanisms but also benefits from sovereignty framework as appeal tool against current power holders
 *   - Civic Reform Movements: Organized secondary actor (organized/constrained) — face institutional obstruction but leverage legitimacy frameworks (constitutional amendment, international human rights law) to advocate for expansion
 *   - Constitutional Text as Institution: Institutional artifact (institutional/arbitrage) — the formal declaration persists through ritual invocation and inertia despite minimal functional constraint on actual power distribution
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing entrenchment mechanisms as coordination cost when they are actually designed extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(popular_sovereignty_entrenchment, 0.58).
domain_priors:suppression_score(popular_sovereignty_entrenchment, 0.62).
domain_priors:theater_ratio(popular_sovereignty_entrenchment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(popular_sovereignty_entrenchment, extractiveness, 0.58).
narrative_ontology:constraint_metric(popular_sovereignty_entrenchment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(popular_sovereignty_entrenchment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(popular_sovereignty_entrenchment, tangled_rope).
narrative_ontology:human_readable(popular_sovereignty_entrenchment, "Popular Sovereignty Entrenchment").
narrative_ontology:topic_domain(popular_sovereignty_entrenchment, "political/constitutional").

domain_priors:requires_active_enforcement(popular_sovereignty_entrenchment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(popular_sovereignty_entrenchment, established_political_elites).
narrative_ontology:constraint_beneficiary(popular_sovereignty_entrenchment, institutional_gatekeepers).
narrative_ontology:constraint_victim(popular_sovereignty_entrenchment, disenfranchised_populations).
narrative_ontology:constraint_victim(popular_sovereignty_entrenchment, excluded_political_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED MAJORITY (SNARE) — Trapped by constitutional and procedural barriers designed to exclude them from meaningful participation. No exit option; bears full cost of exclusion from self-governance. Maximum extraction through deprivation of agency.
constraint_indexing:constraint_classification(popular_sovereignty_entrenchment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED POLITICAL OPPOSITION (TANGLED ROPE) — Constrained by entrenchment mechanisms (gerrymandering, voter suppression, ballot access rules) but also benefits from the sovereignty framework itself — they can appeal to 'the people' against current power holders. Mixed extraction and coordination; agency limited but not eliminated.
constraint_indexing:constraint_classification(popular_sovereignty_entrenchment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL POWER HOLDERS (ROPE) — Benefits from popular sovereignty rhetoric while controlling its implementation. Can arbitrage between claimed commitment to sovereignty and actual gatekeeping. Experiences constraint as pure coordination mechanism that legitimizes their position.
constraint_indexing:constraint_classification(popular_sovereignty_entrenchment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CIVIC REFORM MOVEMENT (TANGLED ROPE) — Organized agents working to expand suffrage and participation mechanisms. Face suppression through counter-organizing and institutional obstruction, but benefit from legitimacy frameworks (constitutional amendments, international human rights norms). Genuine coordination function (expanding the demos) alongside extraction mechanism (current closure).
constraint_indexing:constraint_classification(popular_sovereignty_entrenchment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL TEXT AS INSTITUTIONAL PITON (PITON) — The formal declaration of popular sovereignty in constitutions worldwide (1.5+ billion people live under sovereignty-declaring frameworks) is performative theater with minimal functional constraint on actual decision-making. The text persists through institutional inertia and ritual invocation, not because it determines outcomes. Theater ratio 0.65 reflects widespread gap between rhetoric and gatekeeping reality.
constraint_indexing:constraint_classification(popular_sovereignty_entrenchment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some tension between popular sovereignty claims and actual governance is viewed as inherent: the logistics of consulting millions of people, coordination costs, and informational asymmetries are treated as immutable constraints on direct democracy. However, this perspective risks naturalizing what is actually a choice about institutional design — the mountain classification is a false summit.
constraint_indexing:constraint_classification(popular_sovereignty_entrenchment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(popular_sovereignty_entrenchment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(popular_sovereignty_entrenchment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(popular_sovereignty_entrenchment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(popular_sovereignty_entrenchment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(popular_sovereignty_entrenchment, TR),
    TR >= 0.70.

:- end_tests(popular_sovereignty_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through gatekeeping exclusion (determining who 'the people' are), but the extraction is partially constrained by sovereignty rhetoric itself—power holders cannot openly declare their gatekeeping legitimate without abandoning the sovereignty framework. This creates a hybrid: genuine coordination (the demos concept enables legitimate authority) alongside extraction (implementation narrows the demos to exclude disfavored populations). The measurement progression (0.42 → 0.50 → 0.58) reflects intensification as gatekeeping mechanisms multiply to counter democratization pressures. Suppression (0.62): Moderate-high. Significant barriers include constitutional supermajority requirements for franchise expansion, gerrymandering, voter ID laws, polling place closures, ballot access rules, and media capture. Suppression is high but not total—organized opposition can mobilize, and franchise historically has expanded. Theater ratio (0.65): Moderate-high. The performative content has increased over the measurement interval as the gap between sovereignty declarations and actual gatekeeping widens. Constitutional texts worldwide invoke 'the people' as sovereignty's source, yet implementation concentrates power in narrower constituencies. The theater persists because sovereignty rhetoric remains legitimacy's foundation—power holders cannot abandon it without delegitimizing their own authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR types from different observer positions. The disenfranchised see Snare (no exit, maximum extraction). Organized opposition sees Tangled Rope (mixed coordination benefit and extraction cost). Power holders see Rope (pure coordination of legitimate authority). Reformers see Tangled Rope (gatekeeping cost + sovereignty framework benefit). The institutional text itself is Piton (performative persistence). The analytical risk is a false Mountain (naturalizing entrenchment as coordination cost). The perspectival gap reveals the constraint's core function: sovereignty rhetoric legitimizes power holders while gatekeeping prevents it from actually constraining them. Each perspective sees a different classification because each occupies a different structural position in the flow of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position in the extraction pipeline. Disenfranchised populations have d ≈ 0.95 (trapped victims with no exit—maximum directionality toward extraction). Institutional power holders have d ≈ 0.10 (beneficiaries with arbitrage—low directionality, negative f(d), they experience negative chi as they extract positive value). Organized opposition has d ≈ 0.55 (moderate/constrained, neither pure victims nor pure beneficiaries—symmetric directionality). Civic reformers have d ≈ 0.60 (organized/constrained victims of current gatekeeping but beneficiaries of sovereignty framework itself). The constitutional text artifact has d ≈ 0.05 as an institutional beneficiary of the status quo. Suppression (0.62) is a raw structural property—it does not scale with directionality; only extractiveness (0.58) is scaled by f(d) and σ(S) per χ = ε × f(d) × σ(S). At national scope σ(S) = 1.0, so χ is determined primarily by ε × f(d), which varies dramatically across perspectives based on their exit options and victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint exemplifies mandatrophy resolution through perspectival proliferation. The original question 'Is popular sovereignty Rope or Snare?' is resolved by showing that it is BOTH, from different perspectives. From disenfranchised populations (trapped/powerless), it is pure Snare—gatekeeping without coordination benefit. From power holders (institutional/arbitrage), it is pure Rope—coordination mechanism for legitimate authority. From organized opposition (organized/constrained), it is Tangled Rope—mixed coordination (ability to appeal to 'the people') and extraction (gatekeeping prevents realization). The mandatrophy dissolves when we recognize that the classification depends entirely on which agent we are analyzing. No single universal type applies. The analytical observer's false Mountain (treating coordination barriers as immutable) is the most dangerous mislabeling—it naturalizes what is actually a political choice about institutional design. The constraint resolves mandatrophy by insisting on perspectival precision: for whom, at what time horizon, with what exit options, and at what spatial scope?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_expansion_mechanism,
    'Does expanding franchise (lowering voting age, removing property requirements, including non-citizens) genuinely enhance popular sovereignty or merely shift gatekeeping to new barriers?',
    'Empirical: Comparative analysis of voting accessibility across franchise-expanded polities; tracking of participation rates post-expansion vs. institutional obstruction escalation (voter ID, polling place closures, registration barriers)',
    'If expansion increases actual participation and outcome influence: sovereignty constraint relaxes to Rope. If new barriers emerge proportionally: constraint remains Snare/Tangled Rope with shifted target population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_expansion_mechanism, empirical, 'Whether franchise expansion reduces or relocates gatekeeping barriers').

omega_variable(
    rhetoric_reality_gap_mechanism,
    'Is the gap between popular sovereignty rhetoric and actual gatekeeping a feature of coordination complexity (coordination cost) or structural extraction (intentional entrenchment)?',
    'Historical/institutional: examine policy alternatives that reduce gap without proportional cost (e.g., Swiss direct democracy, sortition pilots); track whether power holders oppose gap-reducing reforms; analyze communication patterns (do institutional actors defend complexity or deny the gap exists?)',
    'If coordination cost: theater_ratio should decrease as technical capacity (digital voting, ballot summaries) improves. If intentional extraction: theater_ratio persists despite technical capacity gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetoric_reality_gap_mechanism, conceptual, 'Whether the rhetoric-reality gap is coordination cost or intentional extraction').

omega_variable(
    critical_demos_threshold,
    'Is there a critical constituency size (demos threshold) above which popular sovereignty claims automatically become false, regardless of institutional design?',
    'Empirical: Comparative outcomes across polities of different population sizes implementing similar direct democracy mechanisms; test whether governance quality/representativeness scales inversely with demos size',
    'If threshold exists (e.g., > 10M population): mountain classification gains validity—some entrenchment is unavoidable by design. If no threshold: constraints are institutional choices, not natural laws.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_demos_threshold, empirical, 'Whether population size creates unavoidable popular sovereignty limits').

omega_variable(
    identity_lock_entrenchment,
    'Do citizens internalize gatekeeping exclusion as identity, making them unable to perceive themselves as legitimate participants even when formal barriers lower?',
    'Psychological/sociological: survey data on self-perceived legitimacy to participate across populations with different franchise histories and current barriers; test whether removing formal barriers shifts participation without corresponding identity shift',
    'If identity lock is significant: removing formal barriers (constraints → mobile) will not increase participation — the constraint relocates to internalized suppression. Omega_variable impacts classification of victims'' exit_options: they remain identity_locked despite formal mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_entrenchment, empirical, 'Internalization of exclusion as identity barrier to participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(popular_sovereignty_entrenchment, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(popsov_tr_t0, popular_sovereignty_entrenchment, theater_ratio, 0, 0.4).
narrative_ontology:measurement(popsov_tr_t2, popular_sovereignty_entrenchment, theater_ratio, 2, 0.52).
narrative_ontology:measurement(popsov_tr_t4, popular_sovereignty_entrenchment, theater_ratio, 4, 0.6).
narrative_ontology:measurement(popsov_tr_t6, popular_sovereignty_entrenchment, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(popsov_be_t0, popular_sovereignty_entrenchment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(popsov_be_t2, popular_sovereignty_entrenchment, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(popsov_be_t4, popular_sovereignty_entrenchment, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(popsov_be_t6, popular_sovereignty_entrenchment, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(popular_sovereignty_entrenchment, enforcement_mechanism).
narrative_ontology:affects_constraint(popular_sovereignty_entrenchment, voting_access_barriers).
narrative_ontology:affects_constraint(popular_sovereignty_entrenchment, constitutional_amendment_supermajority).
narrative_ontology:affects_constraint(popular_sovereignty_entrenchment, electoral_system_entrenchment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(popular_sovereignty_entrenchment, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
