% ============================================================================
% CONSTRAINT STORY: coordination_threshold_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coordination_threshold_failure, []).

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
 *   constraint_id: coordination_threshold_failure
 *   human_readable: The Critical Mass Chasm
 *   domain: social/technological
 *
 * SUMMARY:
 *   The critical mass chasm is a structural coordination problem where a
 *   network or protocol provides zero utility to participants until a
 *   participation threshold is crossed. This creates a temporal valley: early
 *   adopters bear all costs (learning, setup, network externality loss) while
 *   receiving zero benefit, because the network cannot function until
 *   sufficient participants are present. Once the threshold is crossed, the
 *   network provides massive value to all participants, including those who
 *   joined after threshold. The constraint exhibits fundamental tension
 *   between necessity (the chasm is real — some mechanisms genuinely require
 *   critical mass) and extraction (the threshold mechanism can be, and often
 *   is, artificially maintained or exploited by incumbent operators to
 *   exclude competitors and lock in users). The extractiveness trajectory
 *   reflects this: severe extraction during bootstrap (0.72), moderate during
 *   threshold approach (0.55), reduced post-threshold stabilization (0.38).
 *   Theater ratio increases over time as the functional necessity of the
 *   threshold barrier declines but institutional gatekeeping persists.
 *
 * KEY AGENTS:
 *   - Stranded Bootstrappers: Primary victims (powerless/trapped) — early adopters who bear adoption costs before threshold; zero utility during bootstrap phase; maximum extraction because no exit option exists without losing investment
 *   - Threshold Strategists: Secondary participants (moderate/constrained) — time entry near critical mass inflection; benefit from coordination but face lock-in extraction; constrained exit due to network effects
 *   - Incumbent Operators: Primary beneficiaries (institutional/arbitrage) — network operators or dominant platforms benefit from threshold mechanism as coordination tool and competitive moat; threshold protects market share and extracts switching costs
 *   - Bootstrap Organizers: Organized agents (organized/mobile) — early-adopter communities, transition champions, foundations that solve chasm through structured campaigns; structured pressure has sunset logic (needed only until threshold crossed)
 *   - Legacy Threshold Guardians: Institutional maintainers (powerful/constrained) — defend threshold mechanisms through institutional inertia even after functional necessity expires; maintain gatekeeping theater
 *   - Analytical Observer: Systemic view (analytical/analytical) — integrates both coordination function (solves cold-start) and extraction (lock-in) perspectives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coordination_threshold_failure, 0.38).
domain_priors:suppression_score(coordination_threshold_failure, 0.52).
domain_priors:theater_ratio(coordination_threshold_failure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coordination_threshold_failure, extractiveness, 0.38).
narrative_ontology:constraint_metric(coordination_threshold_failure, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(coordination_threshold_failure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coordination_threshold_failure, tangled_rope).
narrative_ontology:human_readable(coordination_threshold_failure, "The Critical Mass Chasm").
narrative_ontology:topic_domain(coordination_threshold_failure, "social/technological").

domain_priors:requires_active_enforcement(coordination_threshold_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coordination_threshold_failure, incumbent_network_operators).
narrative_ontology:constraint_beneficiary(coordination_threshold_failure, early_adopters_post_threshold).
narrative_ontology:constraint_victim(coordination_threshold_failure, bootstrapping_participants).
narrative_ontology:constraint_victim(coordination_threshold_failure, network_switching_victims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRANDED BOOTSTRAPPER (SNARE) — Early participants who join before critical mass is reached experience zero utility while bearing all adoption costs (setup time, learning curve, social friction). No exit option exists without losing their sunk investment. The network provides no benefit until threshold, yet requires their participation to reach threshold. Maximum experienced extraction.
constraint_indexing:constraint_classification(coordination_threshold_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THRESHOLD STRATEGIST (TANGLED ROPE) — Participants who time their entry near the critical mass inflection point benefit from coordination (network becomes useful just as they join) but also face extraction through lock-in effects. They have constrained exit options — leaving after threshold crossing means losing network effects. Moderate experienced extraction balanced against genuine benefit.
constraint_indexing:constraint_classification(coordination_threshold_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT OPERATOR (ROPE) — Network operator or dominant platform benefits from the threshold mechanism as a coordination tool: the chasm creates a natural barrier to competition (competitors must also cross the threshold), protects market share, and extracts switching costs from locked-in users. Experiences the constraint as pure coordination — the chasm solves their problem of maintaining network position.
constraint_indexing:constraint_classification(coordination_threshold_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BOOTSTRAP ORGANIZER (SCAFFOLD) — Organized groups (early-adopter communities, transition champions, open-source foundations) solve the chasm through structured onboarding campaigns with built-in sunset logic: coordinated bootstrap events are designed to push past critical mass in finite time. Once threshold is crossed, the organizing pressure is no longer needed. Low effective extraction because the pressure declines post-threshold.
constraint_indexing:constraint_classification(coordination_threshold_failure, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: LEGACY THRESHOLD GUARDIAN (PITON) — Institutional defenders of old-network effects maintain the threshold mechanism long after its functional purpose has expired. The chasm persists through ritual and institutional inertia (device compatibility checks, membership vetting, protocol redundancy) even though the network has achieved critical mass and could handle lower barriers. Theater ratio high because the enforcement serves theatrical gatekeeping rather than genuine coordination.
constraint_indexing:constraint_classification(coordination_threshold_failure, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a systemic view, the threshold chasm exhibits both genuine coordination function (it solves the cold-start problem and protects from low-value participants pre-threshold) and structural extraction (it imposes costs on bootstrappers and creates lock-in for post-threshold participants). The constraint's functional necessity for bootstrapping is real, but the post-threshold lock-in mechanism is extraction. The analytical view integrates both.
constraint_indexing:constraint_classification(coordination_threshold_failure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_threshold_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coordination_threshold_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_threshold_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(coordination_threshold_failure, TR),
    TR >= 0.70.

:- end_tests(coordination_threshold_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 final): Moderate. The constraint exhibits genuine coordination function (networks require critical mass to provide value) balanced against genuine extraction (lock-in effects post-threshold, artificial threshold maintenance). The value reflects the mixed nature — extractiveness is neither low enough to be pure Rope nor high enough to be pure Snare. The trajectory (0.72→0.55→0.38) shows that extractiveness is highest during bootstrap (early adopters bear maximum cost for zero benefit) and decreases post-threshold as the coordination benefit becomes real. Suppression (0.52): Moderate-high. Significant barriers include: network effects (cannot use network without critical mass of peers), switching costs (leaving means losing social/technical investment), information asymmetries (early adopters cannot know if threshold will be reached), and institutional gatekeeping (incumbents actively maintain barriers). Suppression is not total because bootstrap organizers and federated alternatives can reduce barriers. Theater ratio (0.48 final, increasing from 0.25): Moderate and rising. Early in the interval, the threshold enforcement is functionally necessary (low theater) — the chasm solves a real bootstrapping problem. Over time, as the network matures past critical mass, the threshold enforcement becomes increasingly performative (rising theater) — barrier maintenance shifts from coordination solution to incumbent gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal between stranded bootstrappers and incumbent operators. The bootstrapper experiences the chasm as pure extraction (Snare): they pay costs (time, learning, setup friction) with zero return until a threshold beyond their control is reached. They perceive themselves as victims in an asymmetric coordination game where the operator sets the rules. The incumbent operator experiences the same structure as pure coordination (Rope): they see the threshold as a mechanism for solving the legitimate problem of cold-start network failure. They do not experience it as extraction because they are beneficiaries and have exit options (can lower threshold, can switch networks). The threshold strategist occupies the middle ground — they experience coordination value (join at the inflection point and immediately gain utility) but also lock-in extraction (constrained exit post-threshold), classifying the constraint as Tangled Rope from their perspective. The bootstrap organizer, through coordinated action, can reshape the chasm's severity, converting it from a Snare into a Scaffold with a built-in sunset. The legacy guardian represents institutional persistence of the mechanism beyond its functional necessity, shifting theater from functional necessity toward performative gatekeeping, reclassifying as Piton. The analytical observer integrates all perspectives and sees the constraint as genuinely hybrid — the bootstrapping chasm solves a real coordination problem AND creates extractive lock-in, making Tangled Rope the appropriate systemic classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the threshold mechanism. Stranded bootstrappers occupy the worst position: they are victims (the chasm exists to solve the coordination problem, and they bear its cost), they are trapped (no exit option without losing investment), and they have no power to change the threshold value. Their d-value approaches 1.0 (full target of extraction). Incumbent operators occupy the opposite position: they are beneficiaries (the threshold protects their market position), they have arbitrage options (can adjust or remove threshold if competitive advantage shifts), and they are powerful (can control threshold enforcement). Their d-value is near 0.0 (beneficiary with escape velocity). Threshold strategists occupy an intermediate position: they benefit from the coordination function (join near threshold and immediately gain network value) but face lock-in extraction (constrained exit post-threshold). Their d-value is near 0.50 (symmetric costs and benefits). The analytical observer has d=0.72 (approaching victim position but with full information optionality — can exit the analysis if desired, giving slight positive bias toward beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   The critical mass chasm resolves mandatrophy by demonstrating that the classification depends entirely on temporal position within the constraint's lifecycle. During bootstrap (T=0-2), the mechanism is functionally necessary: the chasm solves the cold-start problem, and some form of coordination threshold is unavoidable. From this phase, the constraint is Tangled Rope (genuine coordination function + unavoidable asymmetric costs). However, the post-threshold (T=3+) institutional persistence of the barrier is extraction: once network effects are established, the threshold enforcement becomes gatekeeping rather than coordination. From this phase, the constraint is Snare for bootstrappers and incumbent-protective Rope for operators. The mandatrophy is resolved by recognizing that the functional justification for extraction declines over the constraint's lifecycle — early extraction is justified by coordination necessity, later persistence is not. The constraint is transitioning from Tangled Rope (justified hybrid) toward Snare (unjustified extraction) as the theater ratio rises and the functional necessity of the barrier declines. A well-designed constraint would have built-in sunset logic (the Scaffold perspective) — a commitment to reduce the threshold as the network matures, converting the temporary extraction into a sunset mechanism. The rise in theater ratio (0.25→0.48) tracks this degradation: the constraint is moving from functional coordination toward performative gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_value_legitimacy,
    'Is the critical mass threshold value empirically necessary for network function, or is it artificially maintained for lock-in purposes?',
    'Comparative analysis across network implementations: identify networks that successfully function below the claimed threshold; measure actual minimum viable participant count vs declared threshold; analyze network protocol technical requirements vs threshold enforcement mechanisms',
    'If threshold is artificial: extractiveness increases to 0.55+, classification shifts toward Snare for bootstrappers. If threshold is technically necessary: extractiveness remains justified, Tangled Rope classification sustained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_value_legitimacy, empirical, 'Whether critical mass threshold is technically necessary or artificially maintained').

omega_variable(
    bootstrap_cost_internalization,
    'Who bears the cost of the bootstrapping chasm — early adopters, incumbent operators, or society broadly through missed coordination gains?',
    'Cost accounting: measure direct costs to early participants (learning, setup, opportunity cost); measure incumbent benefits (market protection, switching costs); measure social cost (delayed network value realization); longitudinal analysis of wealth transfer through network effects post-threshold',
    'If early adopters bear most costs: Snare classification confirmed, suppression >= 0.60. If costs distributed: Tangled Rope confirmed at moderate extraction. If network externalities dominate: suppression decreases, extraction becomes harder to justify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bootstrap_cost_internalization, empirical, 'Cost distribution in the bootstrapping phase').

omega_variable(
    alternative_bootstrap_mechanisms,
    'Can the coordination problem solved by the threshold chasm be solved through alternative mechanisms (subsidized early adoption, federation, protocol compatibility) that avoid extraction?',
    'Case study analysis of networks using graduated entry costs, federated joins, or interoperability. Measure success rates of smooth-barrier networks vs threshold-barrier networks. Analyze why threshold mechanism persists if alternatives exist.',
    'If smooth-barrier alternatives succeed: current threshold mechanism is revealed as Snare (incumbent extraction). If smooth barriers fail or are absent: threshold mechanism may be justified as the only workable coordination solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_bootstrap_mechanisms, empirical, 'Whether non-extraction bootstrap mechanisms are technically viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coordination_threshold_failure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coord_threshold_tr_t0, coordination_threshold_failure, theater_ratio, 0, 0.25).
narrative_ontology:measurement(coord_threshold_tr_t3, coordination_threshold_failure, theater_ratio, 3, 0.36).
narrative_ontology:measurement(coord_threshold_tr_t6, coordination_threshold_failure, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(coord_threshold_be_t0, coordination_threshold_failure, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(coord_threshold_be_t3, coordination_threshold_failure, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(coord_threshold_be_t6, coordination_threshold_failure, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coordination_threshold_failure, resource_allocation).
narrative_ontology:affects_constraint(coordination_threshold_failure, network_cold_start_problem).
narrative_ontology:affects_constraint(coordination_threshold_failure, switching_cost_lock_in).
narrative_ontology:affects_constraint(coordination_threshold_failure, incumbent_competitive_moat).

% DUAL FORMULATION NOTE:
% The critical mass chasm decomposes into two structurally distinct constraints: (1) the genuine cold-start coordination problem (why networks require critical mass, ε≈0.15, Mountain or Rope), and (2) the institutional persistence of threshold barriers post-threshold (why gatekeeping continues after coordination necessity expires, ε≈0.55, Snare). These are linked: the second constraint is justified by appeal to the first, but the justification becomes stale over time as theater ratio rises. This story captures both — it is the story of how justified temporary extraction (Tangled Rope) degrades into unjustified persistent extraction (Snare) as the functional necessity declines. The upstream constraints represent the coordination problem; this story tracks how the solution mechanism can outlive its justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coordination_threshold_failure, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
