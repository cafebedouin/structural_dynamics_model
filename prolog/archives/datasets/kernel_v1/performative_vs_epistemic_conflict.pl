% ============================================================================
% CONSTRAINT STORY: performative_vs_epistemic_conflict
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performative_vs_epistemic_conflict, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performative_vs_epistemic_conflict
 *   human_readable: Performative vs Epistemic Conflict in Discourse
 *   domain: epistemology/social_psychology/philosophy_of_discourse
 *
 * SUMMARY:
 *   The performative vs epistemic conflict constraint operates at the level
 *   of discourse norms and institutional design. It specifies that discourse
 *   can be oriented toward two incompatible primary goals: winning
 *   argument-territory (performative) or finding truth collaboratively
 *   (epistemic). When these orientations are conflated or when performative
 *   norms dominate epistemic ones, the result is a coordination failure:
 *   agents cannot jointly investigate because they are simultaneously
 *   competing for status. The constraint exhibits all six DR types depending
 *   on structural position. For subordinate truth-seekers, it is a snare:
 *   they cannot exit performative norms without social penalty. For
 *   professional epistemic actors (scientists, academics), it is a tangled
 *   rope: they must coordinate on joint truth-seeking while also defending
 *   intellectual territory and institutional position. For status defenders,
 *   it is pure rope: the constraint solves exactly their coordination
 *   problem. For epistemic reform movements, it is a scaffold: they are
 *   building alternative discourse containers with sunset dates as norms
 *   shift. For institutions like formal debate, it is a piton: the ritual
 *   persists through inertia even though its functional point (finding truth
 *   or allocating status fairly) has atrophied. For the civilizational
 *   observer, it risks appearing as a mountain: the apparent incompatibility
 *   between performative and epistemic modes can be naturalized as an
 *   ineliminable feature of human motivation. However, structural data
 *   reveals this as a false summit: performative dominance is maintained by
 *   identifiable beneficiaries (status defenders), enforced through
 *   measurable suppression (social penalty for admitting error), and produces
 *   measurable extraction (subordinates bear the cost of status competition
 *   while truth-seeking capacity declines). The theater ratio (0.68) reflects
 *   that much discourse is performative ritual: winning argument-position
 *   through rhetorical skill, maintaining face, avoiding admission of error,
 *   establishing dominance. True joint investigation requires norm-shift:
 *   explicit commitment to error-correction, willingness to revise position
 *   without status damage, collaborative rather than adversarial framing.
 *
 * KEY AGENTS:
 *   - Subordinate Truth-Seekers: Primary victims (powerless/trapped) — cannot exit performative norms without social penalty; forced to choose between truth-seeking and status maintenance
 *   - Professional Epistemic Actors: Secondary victims (moderate/constrained) — scientists, academics, mediators who value truth-seeking but depend on reputation and institutional standing; experience tangled coordination-extraction
 *   - Status Defenders: Primary beneficiaries (institutional/arbitrage) — political leaders, institutional power holders, ideological factions who benefit from performative norms that reward argument-winning; experience pure coordination
 *   - Epistemic Reform Movement: Organized agents (organized/constrained) — dialogue facilitators, academic integrity advocates, epistemic humility educators building alternative discourse containers
 *   - Debate Institution: Institutional degraded ritual (institutional/arbitrage) — formal debate, parliamentary procedure, adversarial traditions designed to expose error but serving primarily theatrical function
 *   - Analytical Observer: Civilizational risk position (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable feature of human cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performative_vs_epistemic_conflict, 0.52).
domain_priors:suppression_score(performative_vs_epistemic_conflict, 0.58).
domain_priors:theater_ratio(performative_vs_epistemic_conflict, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performative_vs_epistemic_conflict, extractiveness, 0.52).
narrative_ontology:constraint_metric(performative_vs_epistemic_conflict, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(performative_vs_epistemic_conflict, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performative_vs_epistemic_conflict, tangled_rope).
narrative_ontology:human_readable(performative_vs_epistemic_conflict, "Performative vs Epistemic Conflict in Discourse").
narrative_ontology:topic_domain(performative_vs_epistemic_conflict, "epistemology/social_psychology/philosophy_of_discourse").

domain_priors:requires_active_enforcement(performative_vs_epistemic_conflict).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performative_vs_epistemic_conflict, status_defenders).
narrative_ontology:constraint_beneficiary(performative_vs_epistemic_conflict, institutional_power_holders).
narrative_ontology:constraint_victim(performative_vs_epistemic_conflict, epistemic_commons).
narrative_ontology:constraint_victim(performative_vs_epistemic_conflict, joint_truth_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE TRUTH-SEEKER (SNARE) — Cannot exit performative norms without social penalty. Trapped in a system where admission of error signals weakness, where changing position appears as capitulation, where joint investigation is reframed as losing the argument. The subordinate experiences maximum extraction: forced to either abandon truth-seeking or accept status damage. No exit option available within the discourse frame.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROFESSIONAL EPISTEMIC ACTOR (TANGLED ROPE) — Academic, scientist, or professional mediator who genuinely values truth-seeking but also depends on reputation maintenance, grant competitiveness, and institutional standing. Experiences genuine coordination (joint investigation with peers) alongside genuine extraction (must defend territory to secure resources). Constrained exit: can advocate for epistemic norms within professional contexts but faces career risk if seen as too accommodating or insufficiently combative.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATUS DEFENDER (ROPE) — Institutional actor (political leader, CEO, ideological faction) who benefits from performative norms. For this agent, the constraint is pure coordination: prevailing in argument IS the coordination function. The system solves the problem of 'how do we maintain institutional position?' Experiences no extraction because winning argument-territory is their primary goal. Can exit (switch discourse context) at institutional will.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EPISTEMIC REFORM MOVEMENT (SCAFFOLD) — Organized agents (academic integrity movements, epistemic humility advocates, dialogue facilitation communities) see performative norms as a temporary coordination failure with a sunset. Conflict resolution training, epistemic humility pedagogy, forum-shift (replacing argument with collaborative problem-solving), and norm changes in academic publishing are building alternative discourse pathways. Constrained but organized: can shift discourse containers even if individual conversations remain performative.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEBATE INSTITUTION (PITON) — Formal debate, parliamentary procedure, and adversarial discourse traditions were designed to solve a genuine coordination problem: how to air differences, expose flaws, and make decisions when parties disagree. But the functional point (finding error, improving claims) has atrophied into pure theater (winning the match, scoring points, maintaining position). The institution persists through inertia despite serving neither function well — it neither reliably finds truth (too performative) nor fairly allocates status (too dependent on rhetorical skill rather than epistemic merit). Theater ratio high because the ritual is maintained by habit.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, performative and epistemic modes are fundamentally incompatible cognitive activities. Status defense is a baseline social reality; truth-seeking requires relaxing status concerns. These modes cannot be simultaneously optimized — they trade off at the level of human motivation and institutional design. This perspective sees the conflict as inevitable, immutable, an irreducible feature of social cognition. However, structural data (identifiable beneficiaries, measurable extraction, enforcement mechanisms) reveals this as a false summit: what is naturalized as 'human nature' is actually a contingent institutional arrangement that privileges performative norms.
constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performative_vs_epistemic_conflict_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performative_vs_epistemic_conflict, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performative_vs_epistemic_conflict, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performative_vs_epistemic_conflict, TR),
    TR >= 0.70.

:- end_tests(performative_vs_epistemic_conflict_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Status defenders capture significant benefit during discourse — they win argument, maintain position, accumulate intellectual authority. The extraction is not maximal because performative norms also provide genuine coordination (they do allocate discourse territory, make decisions, expose some errors). The 0.52 reflects that performative dominance produces real asymmetry (winners gain status, losers lose status) but that the mechanism is not pure exploitation — it coordinates discourse even while distorting truth-seeking. Rising from 0.38 to 0.52 over the interval suggests performative norms are tightening (more status-dependent, more argument-winning focused, less collaborative space for joint investigation). Suppression (0.58): Moderate-high. Social penalties for admitting error, changing position, or appearing indecisive create significant barriers to exit. The subordinate speaker who revises position in response to critique is perceived as weak, inconsistent, or defeated. These are not physical barriers but social ones, and they are real and enforced. Rising from 0.48 to 0.58 suggests suppression mechanisms strengthening (perhaps through social media amplification of argument-wins, polarization increasing stakes of debate losses, or institutional pressure intensifying status competition). Theater ratio (0.68): High and rising. Much discourse activity is performative ritual: posturing, scoring points, defending territory, maintaining face. The functional point (finding truth, improving claims through joint investigation) is often abandoned in favor of winning the match. Rising from 0.52 to 0.68 suggests theater is increasing relative to function — debate has become more about performance and less about collaborative problem-solving.
 *
 * PERSPECTIVAL GAP:
 *   Status defenders classify the constraint as rope (beneficial coordination); subordinate truth-seekers classify it as snare (pure extraction). Professional epistemic actors see tangled rope (mixed). Epistemic reformers see scaffold (temporary, solvable). Debate institutions see piton (degraded ritual). The analytical observer risks mountain (natural law). The perspectival gap reveals that performative norms are not neutral discourse procedures — they are structurally asymmetric. Agents for whom argument-winning is the goal experience them as beneficial coordination. Agents for whom truth-seeking is the goal experience them as extraction. The constraint's classification depends entirely on the observer's structural position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (status_defenders) are agents for whom argument-winning IS the coordination function — they experience low or negative d because the constraint solves their primary problem. Victims (epistemic_commons, joint_truth_seekers) are agents for whom joint investigation is the goal but performative norms prevent it — they experience high d because they bear extraction costs while the coordination benefit goes elsewhere. The professional epistemic actor occupies a mixed position: they benefit from status rewards (low d) but also bear extraction costs from suppressed error-admission (high d). The net d for this agent is intermediate, producing the tangled rope classification. The subordinate truth-seeker has high d (victim status) and no exit options (trapped), producing snare. No directionality overrides are needed — the structural data (beneficiary/victim declarations plus exit options) produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that performative dominance in discourse IS a real coordination mechanism (solving the problem of how to decide between competing claims, how to allocate discourse authority, how to maintain institutional order), AND simultaneously an extraction mechanism (directing resources toward argument-winners, suppressing error-admission, subordinating truth-seeking to status competition). The tangled rope classification captures both: genuine coordination function (rope aspects) alongside genuine asymmetric extraction (snare aspects). The constraint is NOT ambiguously rope or snare. It is BOTH, and different agents experience different proportions of coordination vs extraction based on their structural position. The false summit (mountain perspective) is where the analytical observer naturalizes what is actually a contingent institutional choice. The constraint is mutable: epistemic reform movements are demonstrating alternative discourse norms with lower performative content. The question is not which type is correct, but whether performative dominance is necessary or chosen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_and_truth_fungibility,
    'Can status rewards and epistemic merit be decoupled through institutional design, or are they fundamentally entangled in human motivation?',
    'Historical analysis of communities with different status/epistemic configurations (monastic scholarship, open-source software, peer review systems with anonymity vs attribution); measurement of truth-seeking behavior when status incentives are altered or removed',
    'If decoupled: scaffold perspective confirmed — epistemic norms can be shifted by redesigning reward structures. If entangled: mountain perspective gains credibility — performative norms may be ineliminable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_and_truth_fungibility, empirical, 'Whether status and epistemic merit can be structurally decoupled').

omega_variable(
    performative_norm_entrenchment,
    'Are performative norms enforced by explicit rules (suppression mechanism), by status asymmetries (structural extraction), or by internalized identity (cognitive lock)?',
    'Discourse analysis across contexts with different rule sets; measurement of norm adherence when explicit rules are removed vs when status structure remains; identification of agents who would exit if given costless escape route vs those who self-enforce performative norms',
    'If explicit rules: removing rules can shift norms (scaffold outcome). If status asymmetries: reform requires resource redistribution (tangled rope deepens). If cognitive lock: agents cannot perceive alternative norms even when available (identity_locked classification for more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_norm_entrenchment, empirical, 'Whether performative norms are externally enforced or internally locked').

omega_variable(
    collaborative_truth_seeking_sustainability,
    'Can joint epistemic investigation sustain as the primary discourse norm, or does performative competition inevitably resurface when stakes are real?',
    'Longitudinal observation of communities institutionalizing epistemic norms (research teams with explicit error-correction protocols, dialogue groups with facilitators, open-inquiry forums); measurement of norm persistence when status competition reintroduces high stakes; identification of failure modes when real disagreement emerges',
    'If sustainable: epistemic reform is not aspirational — scaffold has genuine sunset. If inevitable resurface: performative norms are attractors that require continuous active enforcement (tangled rope deepens toward snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collaborative_truth_seeking_sustainability, empirical, 'Whether collaborative epistemic norms can sustain under real stakes').

omega_variable(
    false_summit_naturalization,
    'Is the apparent incompatibility between performative and epistemic modes a feature of human cognition (mountain), or a feature of specific institutional arrangements that could be redesigned?',
    'Historical analysis of shifts in discourse norms (scientific revolution replacing scholasticism, civil rights discourse shifting rhetorical ground rules, online communities establishing novel norms); identification of cases where norm shifts produced genuine change in truthiness-seeking behavior; testing whether agents with epistemic identity (scientists, philosophers) show different trade-offs than agents with status identity',
    'If institutional: performative dominance is a contingent extraction mechanism, not natural law. If cognitive: the mountain classification is accurate and epistemic reform faces structural limits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether performative/epistemic incompatibility is natural law or institutional contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performative_vs_epistemic_conflict, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_epist_tr_t0, performative_vs_epistemic_conflict, theater_ratio, 0, 0.52).
narrative_ontology:measurement(perf_epist_tr_t3, performative_vs_epistemic_conflict, theater_ratio, 3, 0.59).
narrative_ontology:measurement(perf_epist_tr_t6, performative_vs_epistemic_conflict, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(perf_epist_be_t0, performative_vs_epistemic_conflict, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perf_epist_be_t3, performative_vs_epistemic_conflict, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(perf_epist_be_t6, performative_vs_epistemic_conflict, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(perf_epist_su_t0, performative_vs_epistemic_conflict, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(perf_epist_su_t3, performative_vs_epistemic_conflict, suppression_requirement, 3, 0.53).
narrative_ontology:measurement(perf_epist_su_t6, performative_vs_epistemic_conflict, suppression_requirement, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performative_vs_epistemic_conflict, information_standard).
narrative_ontology:affects_constraint(performative_vs_epistemic_conflict, scientific_priority_and_credit).
narrative_ontology:affects_constraint(performative_vs_epistemic_conflict, intellectual_property_asymmetry).
narrative_ontology:affects_constraint(performative_vs_epistemic_conflict, argumentative_burden_allocation).

% DUAL FORMULATION NOTE:
% Performative vs epistemic conflict is a meta-constraint on discourse itself, affecting how all domain-specific disagreements are processed. Upstream constraints (specific empirical claims, methodological standards) must pass through this constraint's frame to be adjudicated. The performative/epistemic framing affects how scientific priority is awarded (performative: whoever claims first wins; epistemic: whoever tested most thoroughly gets credit), how IP asymmetries persist (performative: defender of established position holds status advantage; epistemic: challenger with better evidence gets advantage), and how argumentative burden is allocated (performative: stronger rhetor bears lighter burden; epistemic: stronger evidence bearer bears lighter burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
