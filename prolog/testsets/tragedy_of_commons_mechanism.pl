% ============================================================================
% CONSTRAINT STORY: tragedy_of_commons_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tragedy_of_commons_mechanism, []).

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
 *   constraint_id: tragedy_of_commons_mechanism
 *   human_readable: Tragedy of the Commons: Individual Incentive vs Collective Outcome
 *   domain: economic/environmental/social
 *
 * SUMMARY:
 *   The tragedy of the commons is a structural constraint where individually
 *   rational behavior aggregates into collectively irrational outcomes. A
 *   shared resource (pasture, fishery, atmosphere, groundwater, knowledge
 *   commons, attention commons) is degraded or depleted because each user
 *   benefits from extraction while bearing only a fraction of the resulting
 *   cost. The mechanism exhibits genuine coordination functions (users could
 *   benefit from mutual restraint) alongside extraction (those who defect
 *   benefit at others' expense). The constraint demonstrates how the same
 *   phenomenon appears as game-theoretic law (mountain), coordination failure
 *   (rope), managed extraction (tangled_rope), solvable through institutional
 *   design (scaffold), or contingent institutional feature (piton) depending
 *   on the observer's structural position. The theater ratio increases over
 *   time as degradation accelerates — initial open-access extraction appears
 *   straightforward, but as the commons degrades, users invest increasingly
 *   in justification rhetoric (claims of necessity, cultural rights,
 *   temporary hardship) rather than changing behavior.
 *
 * KEY AGENTS:
 *   - The Commons: Primary victim (powerless/trapped) — the shared resource itself, bearing full cost of degradation with no agency or exit
 *   - Individual User (Defection View): Moderate/mobile — sees coordination problem as solvable; rational defection is one individual actor among many
 *   - Organized Extractive User: Powerful/arbitrage — coordinates with other powerful actors to maximize extraction while suppressing smaller users' access
 *   - Commons Management Coalition: Organized/constrained — attempts to solve coordination through monitoring and enforcement but faces extraction by defectors
 *   - Institutional Transition System: Institutional/arbitrage — privatization, regulation, or technological alternatives offer sunset pathways with embedded asymmetries during transition
 *   - Analytical Observer: Analytical/analytical — risks naturalizing contingent institutional context as game-theoretic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tragedy_of_commons_mechanism, 0.38).
domain_priors:suppression_score(tragedy_of_commons_mechanism, 0.52).
domain_priors:theater_ratio(tragedy_of_commons_mechanism, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tragedy_of_commons_mechanism, extractiveness, 0.38).
narrative_ontology:constraint_metric(tragedy_of_commons_mechanism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(tragedy_of_commons_mechanism, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tragedy_of_commons_mechanism, tangled_rope).
narrative_ontology:human_readable(tragedy_of_commons_mechanism, "Tragedy of the Commons: Individual Incentive vs Collective Outcome").
narrative_ontology:topic_domain(tragedy_of_commons_mechanism, "economic/environmental/social").

domain_priors:requires_active_enforcement(tragedy_of_commons_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tragedy_of_commons_mechanism, individual_short_term_actors).
narrative_ontology:constraint_victim(tragedy_of_commons_mechanism, collective_long_term_commons).
narrative_ontology:constraint_victim(tragedy_of_commons_mechanism, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COMMONS (SNARE) — The shared resource (fishery, pasture, atmosphere, aquifer) cannot exit the degradation cycle. Bears full extraction cost with no agency. Maximum experienced coercion — the resource is locked into depletion trajectory by rational defection of individual users. Suppression is structural: depletion creates its own momentum independent of any actor's continued extraction.
constraint_indexing:constraint_classification(tragedy_of_commons_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL USER — DEFECTION VIEW (ROPE) — An individual user can see restraint as coordination: if everyone limits harvest, the commons sustains and everyone benefits long-term. But the actual constraint they face is that defection is individually rational even when mutual restraint would be collectively optimal. This is not extraction — it is a coordination failure. The individual experiences the constraint as a pure problem-solving mechanism, and from their perspective it appears solvable through communication and monitoring.
constraint_indexing:constraint_classification(tragedy_of_commons_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: ORGANIZED EXTRACTIVE USER (TANGLED ROPE) — A commercial operator or large-scale user coordinates with other users (through market mechanisms, informal cartels, or lobbying against regulation) to maximize collective extraction while preventing smaller users from accessing the commons. This perspective shows both genuine coordination (among the powerful users) and asymmetric extraction (against powerless users and future availability). Active enforcement of informal extraction rules maintains the asymmetry.
constraint_indexing:constraint_classification(tragedy_of_commons_mechanism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: COMMONS MANAGEMENT COALITION (TANGLED ROPE) — Organized agents (indigenous communities, environmental advocates, some states) attempt to establish sustainable management rules through monitoring and enforcement. They coordinate to solve the collective action problem but face extraction by defectors and powerful users. The constraint appears as a solvable coordination problem with enforcement mechanisms, but enforcement requires suppressing the defection incentive — creating an asymmetry between enforcers and those who bear the cost of restraint.
constraint_indexing:constraint_classification(tragedy_of_commons_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL TRANSITION (SCAFFOLD) — Privatization, state regulation, or technological alternatives (e.g., water markets, tradable fishing permits, renewable energy replacing fossil fuels) represent sunset pathways out of the tragedy. These systems have high theater initially (many rules, complex monitoring) but the theater ratio decreases as the mechanism stabilizes. Extraction exists during the transition (privatization benefits some, hurts others; regulation distributes costs unevenly) but with a clear sunset: once property rights or regulation equilibrate, the extraction mechanism can be adjusted or removed.
constraint_indexing:constraint_classification(tragedy_of_commons_mechanism, scaffold,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CLAIM) — From a game-theoretic perspective, the tragedy appears to be an immutable logical consequence of incentive structure: given rational self-interest, individual incentives diverge from collective welfare. The prisoner's dilemma structure appears universal — applicable to any common-pool resource. However, empirical data contradicts this mountain classification: Elinor Ostrom's research documented hundreds of communities that successfully managed commons for centuries through informal rules, monitoring, and graduated sanctions. This reveals the mountain as a false summit: the tragedy is contingent on institutional context, not a law of logic.
constraint_indexing:constraint_classification(tragedy_of_commons_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tragedy_of_commons_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tragedy_of_commons_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tragedy_of_commons_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(tragedy_of_commons_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The tragedy mechanism creates genuine extraction — those who harvest more than sustainable levels benefit, those who restrain bear costs. But extraction is not total because mutual restraint would benefit everyone, creating a coordination function. The value reflects the genuine mixed nature: users could exit through cooperation but face strong individual incentives against it. Suppression (0.52): Moderate-high. Barriers to exit include: (1) dependence on commons resources for livelihood with no alternatives, (2) knowledge barriers about sustainability tipping points, (3) coordination barriers against defection (monitoring other users' behavior is costly), (4) institutional barriers (no property rights to enforce individual restraint). Theater ratio (0.35): Moderate, increasing. Early-stage commons often show relatively low theater — extraction is direct and obvious. As degradation accelerates, theater rises as users justify extraction through cultural arguments, temporary hardship claims, and technical uncertainty about sustainability thresholds. The measurement trajectory shows extractiveness and theater both increasing, indicating Goodhart drift: as pressure to explain behavior grows, justification narratives substitute for actual restraint.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap appears between the individual user's rope (coordination solvable through communication) and the commons' snare (trapped in degradation). The user sees the constraint as a problem about information and monitoring; the commons experiences it as inexorable depletion. The powerful extractive user sees the constraint as rope (coordination with peers benefits everyone in their group) while the powerless users see snare (locked into scarcity). The analytical mountain perspective claims the tragedy is inevitable, yet the scaffold perspective documents exit pathways (Ostrom's work shows successful commons management). The gap reveals that the game-theoretic inevitability is conditional: it holds when institutional context prevents monitoring and graduated sanctions but fails when these mechanisms are present. The false summit detector should flag the mountain classification as naturalization of institutional context rather than logical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The individual short-term actor benefits from extraction while others bear collective costs — high d value (0.70+). The commons itself has d approaching 1.0 (full target, trapped, powerless). The organized extractive user has lower d because they successfully coordinate to distribute extraction asymmetrically, positioning themselves as beneficiary — d approximately 0.20-0.30. The commons management coalition has moderate d: they are partially in control (organized, constrained exit) but fighting against structural defection incentives, so they bear significant costs — d approximately 0.55-0.65. Future generations have d approaching 1.0 (completely trapped, bearing costs of present extraction with zero bargaining power). The institutional transition actor (privatization, regulation) experiences low d during transition if they control the distribution of property rights or regulatory authority, but higher d if they must defend against defectors — d approximately 0.25-0.50 depending on enforcement capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that 'the tragedy of the commons' as a natural-language concept conflates two distinct constraints with different ε values: (1) the coordination problem of aggregate resource depletion (ε ≈ 0.08-0.15, Rope) and (2) the extraction mechanism where organized powerful users benefit at the expense of powerless users and future commons (ε ≈ 0.45-0.55, Tangled Rope). Hardin's original 1968 framing emphasized the coordination problem (all users rationally defecting) but empirical commons management literature emphasizes the extraction mechanism (the tragedy as asymmetric power, not symmetric prisoner's dilemma). The constraint story presents both: decomposition into separate stories would be appropriate if the corpus requires ε-invariance precision, but the present story recognizes that both mechanisms operate simultaneously in observed commons degradation. The tangled_rope classification captures the hybrid: genuine coordination failure exists (rational defection is real) AND asymmetric extraction exists (powerful users coordinate while powerless users defect). The mandatrophy is resolved by declaring this explicitly: the tragedy is not pure coordination failure (that would be rope with ε ≤ 0.35) nor pure extraction (snare would have no beneficiary coordinating function), but genuinely tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_dependence_of_tragedy,
    'Is the tragedy of the commons an inevitable feature of resource governance or contingent on institutional scale and enforcement capacity?',
    'Comparative analysis of successful commons (Swiss alpine pastures, Indonesian subak irrigation, Philippine groundwater aquifers) vs failed commons (Atlantic cod fishery, Aral Sea, Amazon deforestation). Identify institutional design patterns that distinguish success from failure.',
    'If contingent: the constraint is not mountain but tangled_rope with management solutions available — classification shifts from inevitable law to solvable coordination problem. If inevitable: the tragedy persists across all institutional contexts regardless of design — mountain classification stands but is falsified by empirical evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_dependence_of_tragedy, empirical, 'Whether tragedy is inevitable or contingent on institutional design').

omega_variable(
    defection_threshold_heterogeneity,
    'Do individual actors have heterogeneous thresholds at which they will defect from cooperative restraint, and does population composition (proportion of cooperative vs defecting types) determine tragedy outcome?',
    'Agent-based modeling with heterogeneous preferences; empirical measurement of individual willingness-to-pay for restraint under varying threat conditions and inequality levels; cross-population comparison of defection rates against predicted thresholds.',
    'If thresholds vary significantly: trajectory depends on population composition, not structural inevitability — management can succeed by recruiting cooperative-type actors or enforcing against free-riders selectively. If thresholds cluster tightly: the tragedy is more inevitable but still not mountain (clustering is observable-dependent and policy-responsive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defection_threshold_heterogeneity, empirical, 'Whether defection thresholds vary or cluster uniformly').

omega_variable(
    technology_substitution_possibility,
    'Can technological development or institutional innovation (quotas, privatization, renewable alternatives) eliminate the extraction mechanism, creating a genuine scaffold sunset?',
    'Historical case studies of successful resource transitions (whale oil → petroleum → renewables; open-access fisheries → individual transferable quotas → ecosystem management); measurement of extraction reduction post-intervention; identification of persistent extractive features in supposedly resolved commons.',
    'If substitution is possible: scaffold perspective is real — the constraint has a terminal date. If substitution requires perpetual enforcement: the constraint degrades to piton (the sunset clause is aspirational, not structural). If new technologies create new common-pool problems: constraint families decompose into multiple stories rather than resolving singularly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitution_possibility, empirical, 'Whether technology or institutional innovation can eliminate the tragedy mechanism').

omega_variable(
    inequality_amplification_mechanism,
    'Does the tragedy of the commons mechanism inherently amplify inequality (powerful users extract more, commons degrades, powerless users lose access first), or is amplification contingent on initial inequality levels?',
    'Comparative analysis of commons outcomes in high-inequality vs egalitarian societies; measurement of extraction distribution before and after commons degradation; model simulations testing inequality as parameter vs outcome.',
    'If inherent amplification: the constraint generates its own suppression feedback (inequality increases → extraction increases → commons fails → inequality increases) creating a snare-like lock-in. If contingent: initial inequality distribution matters — relatively equal commons can stabilize, unequal ones degrade, decoupling the mechanism from its outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inequality_amplification_mechanism, empirical, 'Whether tragedy mechanism inherently amplifies inequality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tragedy_of_commons_mechanism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(toc_tr_t0, tragedy_of_commons_mechanism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(toc_tr_t10, tragedy_of_commons_mechanism, theater_ratio, 10, 0.35).
narrative_ontology:measurement(toc_tr_t20, tragedy_of_commons_mechanism, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(toc_be_t0, tragedy_of_commons_mechanism, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(toc_be_t10, tragedy_of_commons_mechanism, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(toc_be_t20, tragedy_of_commons_mechanism, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tragedy_of_commons_mechanism, resource_allocation).
narrative_ontology:affects_constraint(tragedy_of_commons_mechanism, prisoner_dilemma_mechanism).
narrative_ontology:affects_constraint(tragedy_of_commons_mechanism, collective_action_problem).
narrative_ontology:affects_constraint(tragedy_of_commons_mechanism, externality_mechanism).

% DUAL FORMULATION NOTE:
% The tragedy of the commons is upstream of multiple specific resource domains (fisheries, pastures, groundwater, atmosphere) and downstream of general game-theoretic coordination problems. The resource_allocation coordination type reflects that the commons mechanism fundamentally addresses how shared resources are distributed among users. Specific commons (Atlantic cod, Aral Sea, Amazon) should be modeled as separate stories with higher ε values reflecting observed extraction, linked to this general mechanism story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tragedy_of_commons_mechanism, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
