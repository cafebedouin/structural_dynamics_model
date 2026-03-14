% ============================================================================
% CONSTRAINT STORY: moral_hazard_in_teams
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moral_hazard_in_teams, []).

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
 *   constraint_id: moral_hazard_in_teams
 *   human_readable: Moral Hazard in Team-Based Production
 *   domain: organizational/economic_behavior
 *
 * SUMMARY:
 *   Moral hazard in teams represents a fundamental tension between the
 *   efficiency of pooled production and the individual incentive to free-ride
 *   on the efforts of others. When team output is aggregated and compensation
 *   is partially shared or based on team performance, individual contributors
 *   have incentive to underperform relative to their capacity, secure in the
 *   knowledge that their shirking is obscured by the collective nature of the
 *   output. This constraint generates a full spectrum of DR classifications
 *   depending on the observer's structural position: it appears as pure
 *   extraction (snare) for diligent contributors bearing invisible labor, as
 *   a temporary coordination failure being solved through measurement
 *   technology (scaffold), as an institutional norm that has atrophied into
 *   theater (piton), or as an inherent feature of economic coordination under
 *   imperfect monitoring (mountain). The constraint's theater ratio (0.58)
 *   reflects the widespread reliance on performance reviews and peer
 *   evaluations that cannot independently verify individual effort — these
 *   review rituals are substantial but their actual contribution to
 *   distinguishing shirkers from diligent contributors is limited. As
 *   measurement technology improves (digital activity tracking, project
 *   management granularity, AI-assisted effort attribution), the theater
 *   ratio should decrease and the effective extraction visible to
 *   contributors should increase, because the obscurity that currently
 *   enables free-riding without detection will diminish.
 *
 * KEY AGENTS:
 *   - Diligent Contributors: Primary victims (powerless/trapped) — structurally forced to absorb free-riders' unperformed work; cannot exit without employment loss
 *   - Free Riders: Primary beneficiaries (moderate/constrained) — capture shirking benefits while social norms and detection risk provide partial suppression
 *   - Management/HR: Secondary beneficiary (institutional/arbitrage) — benefits from lower wage bills for same output; can arbitrage between teams or adjust comp structures
 *   - Labor Organizations: Organized actors (organized/constrained) — coordinate team-level enforcement and grievance processes; enforce quiet acceptance of some free-riding as coordination cost
 *   - Performance Incentive Systems: Institutional agents (institutional/arbitrage) — HR technology and compensation design aim to measure and differentiate individual contribution; see moral hazard as temporary problem being solved
 *   - Seniority Norms: Institutional legacy (institutional/mobile) — compensation based on tenure regardless of contribution; persists through pension obligations and culture despite acknowledged inefficiency
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks treating moral hazard as inherent law rather than policy choice about measurement investment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moral_hazard_in_teams, 0.52).
domain_priors:suppression_score(moral_hazard_in_teams, 0.65).
domain_priors:theater_ratio(moral_hazard_in_teams, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moral_hazard_in_teams, extractiveness, 0.52).
narrative_ontology:constraint_metric(moral_hazard_in_teams, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(moral_hazard_in_teams, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moral_hazard_in_teams, tangled_rope).
narrative_ontology:human_readable(moral_hazard_in_teams, "Moral Hazard in Team-Based Production").
narrative_ontology:topic_domain(moral_hazard_in_teams, "organizational/economic_behavior").

domain_priors:requires_active_enforcement(moral_hazard_in_teams).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moral_hazard_in_teams, free_riders).
narrative_ontology:constraint_beneficiary(moral_hazard_in_teams, management).
narrative_ontology:constraint_victim(moral_hazard_in_teams, contributors).
narrative_ontology:constraint_victim(moral_hazard_in_teams, team_output_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DILIGENT CONTRIBUTOR (SNARE) — Trapped within the team structure by employment contract and income dependency. Bears the full cost of free-riding through invisible labor absorption. Cannot exit without career penalty. Maximum experienced extraction: their effort subsidizes shirkers while compensation distribution obscures the asymmetry.
constraint_indexing:constraint_classification(moral_hazard_in_teams, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SITUATIONAL FREE RIDER (ROPE) — Constrained by detection risk and reputational concern but benefits from the coordination of shared output. Exit is costly (finding new team) but feasible. Experiences moderate extraction because their shirking is partially suppressed by visibility and social norms within the team.
constraint_indexing:constraint_classification(moral_hazard_in_teams, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: LABOR ORGANIZATION (TANGLED ROPE) — Organized agents see genuine coordination (collective bargaining, dispute resolution, work-sharing norms) alongside asymmetric extraction (members absorb other members' shirking). High suppression of individual alternatives (formal grievance processes, collective discipline) constrains exit. Effective extraction is moderate-high: the organization stabilizes the team but also enforces quiet tolerance of free-riding.
constraint_indexing:constraint_classification(moral_hazard_in_teams, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PERFORMANCE-BASED INCENTIVE REGIME (SCAFFOLD) — Institutional agents (HR departments, compensation committees) see moral hazard as a temporary coordination failure being solved through performance metrics, peer evaluation, and merit-based pay. High theater ratio: performance reviews are largely performative (raters cannot independently verify effort). But the regime has a sunset: as measurement technology improves and pay granularity increases, the regime transitions from theater toward genuine differentiation. Low effective extraction because institutions see an exit path and are actively investing in alternatives.
constraint_indexing:constraint_classification(moral_hazard_in_teams, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SENIORITY PROTECTION NORM (PITON) — Institutional inertia around seniority-based compensation (regardless of individual contribution) persists long after the efficiency rationale has faded. Modern organizations see the constraint as degraded — compensation based on tenure rather than performance is acknowledged as theater — but the norm persists through pension obligations, union contracts, and institutional culture. The seniority norm is a former Rope (once coordinated labor stability) that has atrophied into pure theater.
constraint_indexing:constraint_classification(moral_hazard_in_teams, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a fundamental economics perspective, some degree of moral hazard is inherent to any production arrangement with imperfect monitoring. The gap between effort and observation is a structural feature of economic coordination, not a contingent institutional artifact. Effort is unobservable; output is joint; incentive structures always involve trade-offs. However, this perspective risks naturalizing what is partly a policy choice about measurement granularity and transparency technology. The engine will likely detect this as a false summit.
constraint_indexing:constraint_classification(moral_hazard_in_teams, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_hazard_in_teams_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moral_hazard_in_teams, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_hazard_in_teams, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moral_hazard_in_teams, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moral_hazard_in_teams, TR),
    TR >= 0.70.

:- end_tests(moral_hazard_in_teams_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint involves real extraction — free-riders capture benefits they did not produce, and diligent contributors bear costs they did not cause. However, the extraction is not maximal because: (1) some free-riding is suppressed by peer visibility and reputation concerns; (2) diligent contributors often retain some intrinsic satisfaction from quality work; (3) compensation still correlates with observable factors even if individual effort is obscured. The upward trajectory (0.35 → 0.52) reflects that as teams scale and work complexity increases, individual effort becomes less observable, enabling more sophisticated free-riding. Suppression (0.65): Moderate-high. Significant barriers to exit include: employment contract, income dependency, reputational cost of leaving mid-project, cultural norms around team loyalty. However, suppression is not total because: (1) external job market provides some escape (constrained, not trapped, for most contributors); (2) peer visibility and informal sanctions provide partial self-regulation of free-riding; (3) voluntary organizations (open-source, academia) allow choice of team membership based on member quality. Theater ratio (0.58): Moderate. Performance reviews, peer evaluations, and 360-degree feedback are substantial institutional activity but have limited actual connection to individual effort. Reviewers cannot independently observe daily effort; they assess outputs, artifacts, and peer opinions, all of which can be manipulated by free-riders. The theater increases with team size (diffusion of responsibility makes individual assessment harder) and with temporal distance from review periods (shirking in non-review cycles is invisible). Modern measurement technology (keystroke tracking, Git commit analysis, project management tool granularity) aims to reduce theater, but adoption is uneven.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence from a single set of structural facts. The diligent contributor sees a snare: they are trapped, experiencing high extraction, with suppression preventing exit. The free rider sees a rope: they coordinate with their teammates (doing just enough to avoid detection) while benefiting from effort-pooling. Management sees a scaffold: the moral hazard is a temporary coordination problem being solved through better measurement and incentive alignment; they have agency and see an exit path (investment in technology, policy change). The labor organization sees a tangled rope: genuine coordination (collective bargaining, work-sharing norms) alongside asymmetric extraction (members tolerating shirkers for collective stability). The seniority norm is a piton: once coordinated labor stability and prevented wage competition; now pure theater—compensation by tenure despite acknowledged inefficiency. The civilizational observer risks a mountain: effort-output mismatch is inherent to economic coordination, monitoring is always imperfect, incentive misalignment is natural. But the structural data reveals this as false naturalization — the constraint's severity depends almost entirely on measurement technology and organizational choice about compensation transparency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from their structural relationship to the extraction flow: who benefits, who bears costs, what alternatives exist. Diligent contributors are trapped victims (d ≈ 0.95) with no arbitrage options — their effort subsidizes others, and exit costs are prohibitive. Free riders are beneficiaries with constrained exit (d ≈ 0.35) — they gain from shirking but fear detection and reputational damage. Management observes net benefit with arbitrage options (d ≈ 0.10) — they can reallocate teams, adjust comp, or outsource if moral hazard becomes severe. Labor organizations see mixed benefit (d ≈ 0.50) — they coordinate protections for members but also enforce tolerance of some free-riding as the cost of team stability. The scaffold perspective (institutional agents building performance metrics) derives low d ≈ 0.15 because they perceive themselves as solving the problem (builders of infrastructure), not extracting value. The piton perspective (seniority norms) derives high d ≈ 0.75 relative to merit contributors because the seniority rule explicitly shields low contributors from performance differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATHWAY: Moral hazard in teams resolves the mandatrophy by demonstrating that the constraint is best understood as a problem of information asymmetry and institutional design, not as an immutable feature of team production. The mountain perspective (inherent moral hazard) is revealed as a false summit once we observe that: (1) measurement granularity is a policy choice, not a natural limit; (2) teams with high-transparency cultures (open-source, pair programming, real-time task tracking) show dramatically lower free-riding than opaque hierarchical teams; (3) the constraint's severity correlates tightly with technology investment in monitoring, not with fundamental economic law. The scaffold perspective (temporary problem being solved) is the accurate structural reading. Performance-based incentive regimes are not solving the problem yet — theater ratio remains high — but the trajectory shows institutions are investing in solutions (better metrics, algorithmic peer review, transparent contribution tracking). The snare perspective (for contributors) is not a misclassification but a legitimate observation of *unequal institutional investment*: much institutional effort goes into detecting and penalizing free-riding, but little effort goes into making diligent contribution more visible or rewarded. The asymmetry (high suppression of shirking, low reward for diligence) is the core extraction mechanism. Resolving mandatrophy requires recognizing that moral hazard is partly a contingent institutional fact (replicable through measurement choices) and partly a coordination problem (genuinely difficult to solve without overhead). The piton classification (seniority protection) correctly identifies the false naturalization — the belief that compensation should be based on tenure regardless of contribution persists through institutional inertia, not economic necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_feasibility_boundary,
    'What is the technological boundary between unobservable effort and observable effort contributions?',
    'Historical analysis of measurement technology adoption (time-tracking, project management tools, keystroke monitoring, AI-based activity tracking) and correlation with measured free-riding rates; comparison of organizations with different measurement granularity',
    'If effort becomes highly observable: moral hazard classification shifts from snare/tangled_rope toward rope across all perspectives. If measurement remains expensive: constraint persists as snare for contributors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_feasibility_boundary, empirical, 'Technological boundary between observable and unobservable effort').

omega_variable(
    team_size_extraction_scaling,
    'Does free-riding increase monotonically with team size, or does it exhibit threshold effects?',
    'Empirical study of free-riding rates across team sizes (2-person to 50+ person teams); controlled experiments with varying group sizes and measurement transparency; analysis of Ringelmann effect literature and modern tech team data',
    'If monotonic scaling: larger teams show systematically higher extraction. If threshold at team size ~7-12: small teams self-regulate through visibility; larger teams require formal enforcement. Threshold effect would refine the suppression metric to be team-size-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(team_size_extraction_scaling, empirical, 'Relationship between team size and free-riding rates').

omega_variable(
    compensation_transparency_asymmetry,
    'Does individual contribution visibility reduce moral hazard, or does it enable strategic free-riding targeting (shirking on tasks others are known to value)?',
    'Comparison of teams with transparent vs opaque contribution metrics; analysis of behavioral shifts after transparency implementation; longitudinal tracking of shirking patterns post-disclosure',
    'If visibility reduces hazard: suppression metric decreases and classification shifts toward rope. If visibility enables strategic targeting: extraction mechanisms become more sophisticated but not less severe (tangled_rope persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_transparency_asymmetry, empirical, 'Whether contribution visibility reduces or redirects free-riding').

omega_variable(
    identity_locked_team_commitment,
    'For identity-locked contributors (whose professional identity is fused with team membership or craft commitment), does moral hazard operate differently?',
    'Comparison of exit-friction and free-riding rates for identity-locked contributors vs mobile contributors; qualitative analysis of identity-locked agents who do exit; study of craft communities (open-source projects, academic collaborations) with strong identity investment',
    'If identity lock reduces free-riding: some contributors experience lower suppression (their moral commitment acts as self-enforcement). If identity lock increases extraction: identity-fused agents are more exploitable, increasing victimhood and suppression from their perspective. The identity_locked exit option would appear in their perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_team_commitment, empirical, 'How professional identity fusion affects moral hazard dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moral_hazard_in_teams, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mh_teams_tr_t0, moral_hazard_in_teams, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mh_teams_tr_t3, moral_hazard_in_teams, theater_ratio, 3, 0.5).
narrative_ontology:measurement(mh_teams_tr_t6, moral_hazard_in_teams, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(mh_teams_be_t0, moral_hazard_in_teams, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mh_teams_be_t3, moral_hazard_in_teams, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(mh_teams_be_t6, moral_hazard_in_teams, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moral_hazard_in_teams, resource_allocation).
narrative_ontology:affects_constraint(moral_hazard_in_teams, principal_agent_misalignment).
narrative_ontology:affects_constraint(moral_hazard_in_teams, shirking_under_piece_rates).

% DUAL FORMULATION NOTE:
% Moral hazard in teams is downstream of the general principal-agent problem but represents a distinct structural constraint when production is aggregated. The principal-agent story models asymmetric information between owner and worker; the team moral hazard story adds the layer that worker effort is unobservable to peers, enabling free-riding within the collective. Each story has its own ε value reflecting different measurement domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(moral_hazard_in_teams, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
