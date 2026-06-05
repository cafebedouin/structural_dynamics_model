% ============================================================================
% CONSTRAINT STORY: social_loafing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   Social loafing (Ringelmann effect) emerges when individuals reduce their
 *   effort in collective tasks relative to individual effort on the same
 *   task. This structural phenomenon combines genuine coordination function
 *   (collective tasks solve problems individuals cannot solve alone) with a
 *   systematic extraction mechanism (diffusion of responsibility enables
 *   effort reduction). The constraint exhibits different classifications
 *   across observer positions: free riders experience it as an opportunity
 *   (rope or neutral arbitrage), conscientious contributors experience it as
 *   a snare (trapped, bearing disproportionate burden), task designers
 *   experience it as a calibration challenge (rope, solvable through
 *   incentive design), and the analytical observer sees it as a fundamental
 *   tension between individual incentives and collective needs (tangled
 *   rope). The constraint's extractiveness has been rising over the past 50
 *   years as organizations have scaled up collective work while leaving
 *   diffusion-of-responsibility mechanisms intact. Theater ratio has also
 *   increased as performative team-building rituals have partially displaced
 *   actual task coordination, suggesting the constraint is acquiring
 *   piton-like characteristics in some institutional contexts. The emergence
 *   of digital attribution technologies (real-time contribution tracking,
 *   gamification, leaderboards) represents a potential sunset clause: as
 *   individual contribution becomes measurable, the free-rider advantage and
 *   diffusion-of-responsibility mechanism both decline, transitioning the
 *   constraint toward scaffold classification.
 *
 * KEY AGENTS:
 *   - Conscientious Contributors: Primary victims (powerless/trapped) — bear disproportionate share of work as collective effort declines; cannot exit without abandoning task or facing social friction
 *   - Free Riders / Effort Minimizers: Primary beneficiaries (moderate/constrained) — extract effort reduction through anonymity and diffusion of responsibility; constrained by peer monitoring and norms
 *   - Task Designers / Managers: Institutional actors (organized/arbitrage) — can modulate loafing through group size, incentive structure, identifiability, and task design; experience constraint as calibration problem
 *   - Conscientious-Conditional Hybrids: Secondary agents (moderate/constrained) — reduce effort when diffusion of responsibility increases but maintain effort under identifiability; show conditional participation
 *   - Digital Attribution Systems: Emerging institutional actors (institutional/arbitrage) — enable individual contribution tracking; represent technological scaffold pathway
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees fundamental tension between coordination benefit and individual incentive structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_loafing, 0.38).
domain_priors:suppression_score(social_loafing, 0.48).
domain_priors:theater_ratio(social_loafing, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_loafing, extractiveness, 0.38).
narrative_ontology:constraint_metric(social_loafing, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(social_loafing, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_loafing, tangled_rope).
narrative_ontology:human_readable(social_loafing, "Social Loafing (The Ringelmann Effect)").
narrative_ontology:topic_domain(social_loafing, "social/economic").

domain_priors:requires_active_enforcement(social_loafing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_loafing, free_riders).
narrative_ontology:constraint_beneficiary(social_loafing, effort_minimizers).
narrative_ontology:constraint_victim(social_loafing, collective_task_efficiency).
narrative_ontology:constraint_victim(social_loafing, conscientious_contributors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCIENTIOUS CONTRIBUTOR (SNARE) — Individual contributor is trapped in the collective task; bears disproportionate share of work as others reduce effort. Cannot exit without abandoning commitment or facing social friction. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.51.
constraint_indexing:constraint_classification(social_loafing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONDITIONAL FREE RIDER (TANGLED ROPE) — Constrained by social norms and peer monitoring; benefits from collective task's coordination function (shared goal achievement) while extracting through reduced effort when diffusion of responsibility increases. d≈0.70, f(d)≈1.05, σ=0.8 → χ≈0.32.
constraint_indexing:constraint_classification(social_loafing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: TASK DESIGNER / MANAGER (ROPE) — Institutional actor (organized power, arbitrage exit) who designs collective tasks to solve coordination problems and achieve outcomes. Experiences loafing as a calibration challenge rather than extraction — can modulate group size, incentive structure, identifiability, and task design. d≈0.25, f(d)≈0.10, σ=0.9 → χ≈0.00. Net-neutral; coordination function dominates.
constraint_indexing:constraint_classification(social_loafing, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational distance, social loafing reflects genuine tension between (a) coordination benefit of collective action and (b) individual incentive to minimize effort when contribution cannot be individually tracked. Both functions are structural. The constraint persists because task designers must balance group efficiency against contribution fairness. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.52. Effective extraction arises from the coordination function itself — effort reduction is the mechanism through which the coordination problem manifests.
constraint_indexing:constraint_classification(social_loafing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: TECHNOLOGY-ENABLED MEASURER (SCAFFOLD) — Digital systems, real-time contribution tracking, gamification, and transparent leaderboards represent a sunset clause for anonymous loafing. As individual contribution becomes measurable and attributable, the free-rider advantage declines. Organizations implementing these systems see social loafing as a temporary coordination problem with an engineering solution path. d≈0.30, f(d)≈0.20, σ=1.1 → χ≈0.08. Low effective extraction because the scaffold provides exit: measurability reduces loafing by eliminating the diffusion-of-responsibility mechanism.
constraint_indexing:constraint_classification(social_loafing, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PERFORMATIVE TEAM RITUAL (PITON) — In many organizational settings, 'team building' and collective work have become primarily theatrical: the stated coordination benefit (collective output) is subordinated to performative benefits (social cohesion markers, managerial visibility, diversity metrics). Actual task efficiency is degraded; the collective persists through inertia and institutional mandate. theater_ratio≈0.60, suggesting theater has not yet fully displaced function but is rising. This perspective sees the collective itself as degraded.
constraint_indexing:constraint_classification(social_loafing, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

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
 *   Extractiveness (0.38): Moderate. Social loafing involves real effort reduction that generates asymmetric outcomes (free riders save effort while conscientious contributors bear burden), but the magnitude is typically 20-30% in experimental settings, not total. The extraction is contingent on group size and anonymity; it disappears under identifiability or small-group contexts. Suppression (0.48): Moderate. Mechanisms suppressing exit include social norms against free-riding, peer monitoring (though imperfect), role expectations, and the coordinating function of the task itself — one cannot fully exit without abandoning the collective benefit. Theater ratio (0.55): Moderate-rising. In traditional team settings, performative elements (trust-building, morale rituals) are present but secondary to actual task work. However, in organizational contexts with high performativity (diversity initiatives, team-building exercises), theater has increased to 0.60+. The historical trajectory (0.35 → 0.55 over 50 years) reflects increasing professionalization and scaling of group work while relying on persistently opaque attribution.
 *
 * PERSPECTIVAL GAP:
 *   Free riders see loafing as a rational extraction mechanism (rope-like from their perspective: participate in the coordination benefit while minimizing effort cost). Conscientious contributors see a snare (trapped in a system where their effort subsidizes others). Task designers see a design problem solvable through incentive restructuring (rope, with coordination function maintained). The analytical observer sees tangled rope (genuine coordination function coupled with genuine extraction mechanism via diffusion of responsibility). Technology designers see a scaffold (measurable attribution will solve the problem). Organizational performers see piton (the team ritual persists through inertia, with actual task efficiency degraded). The perspectival gap arises because the same structural feature — anonymity and diffusion of responsibility — is experienced as opportunity by free riders and as victimization by conscientious contributors. Task designers can compress this gap by increasing identifiability, but this creates different extraction mechanisms (status anxiety, competitive hierarchy). No single perspective fully captures the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Conscientious contributors: Victims + trapped → d≈0.92, f(d)≈1.38. High directionality; bears costs with no exit. Free riders: Beneficiaries + constrained → d≈0.45, f(d)≈0.45. Moderate directionality; benefit from anonymity but constrained by norms. Task designers: Institutional + arbitrage → d≈0.25, f(d)≈0.10. Low directionality; can exit through redesign. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Medium-high directionality; sees both sides of the extraction. The derived d values cluster around the moderate range because loafing is not a totalizing extraction (like debt traps or surveillance) but rather a moderate efficiency loss with contingent beneficiaries and semi-trapped victims. Conscientious contributors occupy the trapped end; free riders occupy the constrained end (benefiting but with social limits); designers occupy the arbitrage end (can change the game).
 *
 * MANDATROPHY ANALYSIS:
 *   Social loafing resolves the mandatrophy by showing that the constraint is genuinely hybrid: it performs a coordination function (collective work solves problems individuals cannot solve) AND it extracts effort from conscientious contributors through diffusion of responsibility. Both properties are structural and cannot be eliminated without changing the collective work structure itself. The constraint is not 'really' a snare being mislabeled as rope (coordination is genuinely present), nor is it 'really' rope being mislabeled as snare (extraction via diffusion of responsibility is genuinely present). The mandatrophy resolves when the observer acknowledges that the same structural feature (anonymity in large groups) enables both: collective action becomes possible precisely because individuals are not individually accountable (reduces stigma, enables participation), AND this same anonymity enables free-riding (reduces incentive). The constraint cannot be classified as a pure type without losing precision. Tangled rope classification at the analytical level captures this: the coordination function is non-trivial (organizations clearly benefit from collective work over individual work for most tasks), the extraction is non-trivial (conscientious contributors clearly lose to free riders), and both arise from the same diffusion-of-responsibility mechanism. Technology (digital attribution) offers a potential pathway to scaffold classification by making the coordination function compatible with individual accountability, though this risks creating new extraction mechanisms (status hierarchies). The mandate resolves: social loafing is structurally tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diffusion_of_responsibility_threshold,
    'At what group size does diffusion of responsibility become the dominant mechanism suppressing individual effort?',
    'Experimental variation of group size (2-person, 5-person, 15-person, 50-person tasks) with identical task structure; measurement of effort as function of group size and attribution clarity',
    'If threshold is small (5-7 people): social loafing is a low-coordination problem, easily mitigated by small teams. If threshold is large (30+ people): loafing is intrinsic to mass coordination, suggesting fundamental constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diffusion_of_responsibility_threshold, empirical, 'Group size threshold for diffusion of responsibility dominance').

omega_variable(
    intrinsic_vs_instrumental_motivation,
    'Is reduced effort in collective tasks primarily a rational free-rider response (instrumental) or a psychologically deeper suppression of intrinsic motivation (non-instrumental)?',
    'Manipulation of extrinsic incentives (payment, public feedback) vs intrinsic appeal (task autonomy, meaning, competence); measurement of effort reduction in presence of extrinsic vs intrinsic motivation',
    'If instrumental dominates: loafing is a snare (extraction mechanism is explicit incentive misalignment). If psychological dominates: loafing may be a mountain (inherent to collective motivation structures), or a different snare (extraction via demotivation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_vs_instrumental_motivation, empirical, 'Whether effort reduction is rational response or deeper motivation suppression').

omega_variable(
    identifiability_as_structural_exit,
    'Does perfect identifiability (individual contribution tracking) eliminate social loafing entirely, or does it create new extraction mechanisms (status anxiety, competitive hierarchy)?',
    'Comparison of effort in opaque-collective vs transparent-attribution settings; measurement of both loafing reduction AND emergence of new coordination problems (excessive competitive effort, perfectionism, burnout)',
    'If identifiability eliminates loafing: scaffold perspective is correct — the constraint has a technical sunset. If identifiability creates new problems: loafing is not eliminated but redistributed, suggesting a deeper structural constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identifiability_as_structural_exit, empirical, 'Whether individual contribution tracking fully resolves social loafing').

omega_variable(
    cultural_variability_of_loafing,
    'Does social loafing emerge uniformly across cultures, or does it vary significantly with cultural emphasis on individualism vs collectivism?',
    'Cross-cultural experimental replication (Ringelmann-type rope tasks) in individualistic vs collectivistic cultures; measurement of effort reduction magnitude and attribution',
    'If culturally universal: loafing is a constraint on human coordination structures. If culturally variable: loafing is a contingent product of individualistic cultural frames, suggesting different constraint decomposition across cultures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_variability_of_loafing, empirical, 'Cultural variation in social loafing magnitude and mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_loafing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(loaf_tr_t0, social_loafing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(loaf_tr_t25, social_loafing, theater_ratio, 25, 0.5).
narrative_ontology:measurement(loaf_tr_t50, social_loafing, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(loaf_be_t0, social_loafing, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(loaf_be_t25, social_loafing, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(loaf_be_t50, social_loafing, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_loafing, resource_allocation).
narrative_ontology:affects_constraint(social_loafing, collective_action_problem).
narrative_ontology:affects_constraint(social_loafing, group_accountability).

% DUAL FORMULATION NOTE:
% Social loafing is upstream of broader collective action failures (public goods problems, tragedy of the commons) in organizational and community contexts. The constraint represents a specific mechanism (diffusion of responsibility) through which collective action problems manifest. Downstream constraints inherit the structural ambiguity between coordination function and extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
