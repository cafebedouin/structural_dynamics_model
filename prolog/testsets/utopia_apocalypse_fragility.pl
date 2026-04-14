% ============================================================================
% CONSTRAINT STORY: utopia_apocalypse_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_utopia_apocalypse_fragility, []).

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
 *   constraint_id: utopia_apocalypse_fragility
 *   human_readable: The Utopia-Apocalypse Cliff-Edge
 *   domain: social/political/philosophical
 *
 * SUMMARY:
 *   The utopia-apocalypse cliff-edge describes the structural fragility
 *   inherent to complex social orders that depend on asymmetric suppression
 *   and concentrated institutional authority. This constraint operates across
 *   all six DR types from different observer positions. The 'utopian' order
 *   is presented as stable and inevitable; the apocalyptic alternative is
 *   presented as the only outcome of resistance or structural change. In
 *   reality, the cliff-edge is not a natural law but a contingent
 *   institutional artifact: the sharp bifurcation between order and chaos
 *   reflects the suppression infrastructure (0.72) that prevents intermediate
 *   paths, combined with the theatrical coupling (0.68) between slight
 *   structural perturbations and catastrophic outcomes. The constraint
 *   exhibits tangled rope characteristics: institutional custodians benefit
 *   from maintaining the utopian facade (beneficiaries), marginalized
 *   populations and structural dissidents bear the costs (victims), and
 *   enforcement is continuous and active. The extractiveness (0.58) reflects
 *   that the suppression load is substantial but not total — some agents
 *   retain agency, and alternative ordering mechanisms are emerging. The
 *   temporal trajectory shows mounting extractiveness and theater as
 *   institutional rigidity increases in response to perceived threats, a
 *   classic pattern of constraint degradation.
 *
 * KEY AGENTS:
 *   - Institutional Order Custodians: Primary beneficiaries (institutional/arbitrage) — maintain utopian narrative and suppress apocalyptic framings; capture rents from suppression infrastructure
 *   - Marginalized Populations: Primary victims (moderate/constrained) — bear asymmetric costs of maintaining order; cannot fully exit but have some collective agency
 *   - Structural Dissidents: Secondary victims (powerless/trapped) — perceive fragility directly; suppressed from articulating alternative orderings; face existential threat
 *   - Reform Movements: Organized agents (organized/constrained) — seek gradual structural change that reduces fragility and distributes extraction load; represent sunset mechanism
 *   - Epistemic Commons: Abstract victim (powerless/trapped) — prevented from analyzing true fragility sources; overwhelmed by apocalyptic theater
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutions as inherent constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(utopia_apocalypse_fragility, 0.58).
domain_priors:suppression_score(utopia_apocalypse_fragility, 0.72).
domain_priors:theater_ratio(utopia_apocalypse_fragility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, extractiveness, 0.58).
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(utopia_apocalypse_fragility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(utopia_apocalypse_fragility, tangled_rope).
narrative_ontology:human_readable(utopia_apocalypse_fragility, "The Utopia-Apocalypse Cliff-Edge").
narrative_ontology:topic_domain(utopia_apocalypse_fragility, "social/political/philosophical").

domain_priors:requires_active_enforcement(utopia_apocalypse_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(utopia_apocalypse_fragility, institutional_order_custodians).
narrative_ontology:constraint_beneficiary(utopia_apocalypse_fragility, status_quo_beneficiaries).
narrative_ontology:constraint_victim(utopia_apocalypse_fragility, marginalized_populations).
narrative_ontology:constraint_victim(utopia_apocalypse_fragility, structural_dissidents).
narrative_ontology:constraint_victim(utopia_apocalypse_fragility, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED DISSIDENTS (SNARE) — Those who perceive the fragility of the social order but lack exit mechanisms face maximum extraction. Coercive suppression (0.72) operates through surveillance, institutional barriers, and existential threat (apocalypse framing). Cannot exit the order without becoming 'apocalyptic' themselves. Pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED POPULATIONS (TANGLED ROPE) — Bear the costs of maintaining the 'utopian' order through structural inequality, yet also depend on institutional provisions (healthcare, education, minimal order). Cannot fully exit but have some agency through collective organization. Experience both extraction and coordination benefits.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CUSTODIANS OF ORDER (ROPE) — Institutional actors managing the social apparatus experience the constraint as pure coordination. Maintaining the utopian facade and preventing apocalyptic transition is their primary function. Net beneficiaries with high exit capacity (can arbitrage between institutional roles). Minimal perceived extraction.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM MOVEMENTS (SCAFFOLD) — Organized agents seeking gradual structural change perceive a sunset clause: incremental reforms that reduce fragility and distribute extraction load more equitably. Theater ratio declines as transparency increases. The constraint's extractive force dissipates as alternative ordering mechanisms emerge (redistributive policies, institutional pluralism, power diffusion).
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: IDEOLOGICAL APPARATUS (PITON) — The utopian/apocalyptic framing itself is substantially performative. The threat of apocalypse justifies institutional controls, but the coupling between structural fragility and catastrophic outcome is increasingly theatrical. Theater ratio (0.68) reflects this: extensive performative activity (dystopian rhetoric, security theater, civilizational discourse) maintains inertia despite degraded functional connection between actual fragility and apocalyptic risk.
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From a systems perspective, some degree of fragility and catastrophic risk is inherent to complex societies: any sufficiently intricate social order has bifurcation points where small perturbations produce discontinuous outcomes. This perspective risks naturalizing what is actually a contingent institutional arrangement — confusing the mathematical property (unstable equilibrium) with the structural cause (asymmetric power concentration and suppression mechanisms).
constraint_indexing:constraint_classification(utopia_apocalypse_fragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(utopia_apocalypse_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(utopia_apocalypse_fragility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(utopia_apocalypse_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(utopia_apocalypse_fragility, TR),
    TR >= 0.70.

:- end_tests(utopia_apocalypse_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The suppression intensity (0.72) indicates substantial coercive overhead, but the mechanism is not pure extraction like a Snare — it exists within a coordination function (maintaining social stability). The extractiveness value reflects that the order does provide real coordination benefits (rule of law, resource distribution, safety from total chaos) even as it concentrates rents and suppresses alternatives. The 0.58 value reflects the mixed nature: genuine coordination goods exist alongside asymmetric extraction. Suppression (0.72): High. The cliff-edge framing creates an effective monopoly on legitimate ordering — alternatives are delegitimized as apocalyptic, dissent is treated as catastrophic threat, and exit options are structurally eliminated. Theater ratio (0.68): Moderate-high. The apocalyptic/utopian narrative exhibits substantial performative content: the actual fragility may be much lower than the institutional rhetoric suggests, the coupling between small perturbations and catastrophic outcomes is often exaggerated, and the 'necessity' of suppression is theater-dependent. As institutional transparency increases, theater ratio should decline.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap emerges from differential access to exit options and structural position. Those with arbitrage exit (institutional actors) experience the constraint as coordination (Rope) — they can move between roles within the system. Those with constrained exit (moderate agents, organizations) experience mixed extraction-coordination (Tangled Rope) — they depend on the system while bearing its costs. Those with no exit (powerless, trapped dissidents) experience pure extraction (Snare) — the constraint apparatus exists only to suppress them. The analytical observer at the civilizational level risks mistaking a structural artifact (the suppression infrastructure that creates the cliff-edge) for a natural law (the inherent fragility of complex societies). This is the false summit that mandatrophy detection targets: the mountain is a naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position in the extraction flow. Custodians (institutional/arbitrage) have d≈0.10: they are net beneficiaries with exit capacity (can arbitrage between institutional roles). Marginalized populations (moderate/constrained) have d≈0.55: they bear costs but have some agency and some dependence on institutional benefits. Trapped dissidents (powerless/trapped) have d≈0.92: maximum exposure to extraction with no exit. The engine derives d from victim/beneficiary declarations plus exit options, producing the f(d) sigmoid multiplier that scales experienced chi. Custodians experience low effective extraction despite high base extractiveness because their structural position (beneficiary + arbitrage) lowers their d value. Trapped dissidents experience maximum χ because their position (victim + trapped) raises their d value to near 1.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by distinguishing the analytical observer's mountain classification (fragility as inherent to complexity) from the structural reality that the cliff-edge is manufactured through suppression asymmetry. True analytical work would ask: under what conditions does a complex society maintain stability WITHOUT the cliff-edge bifurcation? Answer: when decision-making is distributed (reducing single-point-of-failure risks), when suppression intensity is low (eliminating the need for apocalyptic narrative to justify coercion), and when exit options are genuinely available (preventing the bifurcation that emerges from trapped populations). Societies that maintain these properties show intermediate fragility, not cliff-edges. The cliff-edge itself is thus a contingent institutional artifact, not a law of nature. The mountain perspective is a false summit — it naturalizes what is actually a snare with institutional custodians as beneficiaries and trapped populations as victims. The real constraint structure is Tangled Rope: genuine coordination (maintaining social order) coupled with asymmetric extraction (concentrating rents and suppressing alternatives). As institutional transparency increases and alternative orderings become available, the rope component strengthens and the extraction component weakens — the scaffold perspective's sunset becomes real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragility_measurement_problem,
    'Is the cliff-edge fragility inherent to social complexity or manufactured by asymmetric suppression structures?',
    'Comparative analysis of societies with different power distributions and suppression intensities; identification of systems that maintain stability without cliff-edge dynamics; study of how transparency and distributed decision-making alter bifurcation topology',
    'If inherent: mountain classification strengthens (NL property). If manufactured: snare/tangled_rope classifications confirmed (contingent institutional extraction). If partially both: mountain-snare hybrid (exceptional case).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragility_measurement_problem, conceptual, 'Whether cliff-edge fragility is structural or constructed').

omega_variable(
    apocalypse_coupling_authenticity,
    'Do actual structural collapse mechanisms match the apocalyptic narratives used to justify suppression?',
    'Historical case studies of failed societies and actual failure modes; comparison between predicted apocalyptic scenarios and observed collapse patterns; analysis of rhetoric-reality gap in institutional threat narratives',
    'If authentic coupling: suppression (0.72) is functionally justified. If decoupled: suppression appears as pure coercion theater, extractiveness increases, piton component rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(apocalypse_coupling_authenticity, empirical, 'Authenticity of apocalypse narratives relative to actual collapse risks').

omega_variable(
    reform_exit_velocity,
    'At what pace of structural change does the scaffold perspective''s sunset clause become real rather than aspirational?',
    'Empirical tracking of reform implementation rates, institutional adoption of alternative ordering mechanisms, distribution of decision-making power, and corresponding changes in suppression intensity and extraction ratios',
    'If reforms accelerate past 2-5 year half-life: scaffold is real, theater ratio declines, extractiveness drops (tangled rope→rope). If reforms stall: scaffold is performative (organizational theater), extracted agents recognize snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_exit_velocity, empirical, 'Whether institutional reforms achieve sufficient velocity for sunset mechanism').

omega_variable(
    consciousness_bifurcation,
    'Does awareness of the constraint''s structure bifurcate toward either utopian faith or apocalyptic despair, preventing moderate perception?',
    'Psychological and sociological research on how exposure to fragility narratives affects risk perception, institutional trust, and agency; modeling of bifurcation dynamics in belief formation; analysis of whether nuanced understanding can persist under high-suppression regimes',
    'If bifurcation is real: piton theater is functionally necessary (prevents paralysis), extractiveness justified. If consciousness can remain modular: theater becomes pure coercion, snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consciousness_bifurcation, conceptual, 'Whether consciousness bifurcates toward extremes under cliff-edge framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(utopia_apocalypse_fragility, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(utap_tr_t0, utopia_apocalypse_fragility, theater_ratio, 0, 0.52).
narrative_ontology:measurement(utap_tr_t25, utopia_apocalypse_fragility, theater_ratio, 25, 0.62).
narrative_ontology:measurement(utap_tr_t50, utopia_apocalypse_fragility, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(utap_be_t0, utopia_apocalypse_fragility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(utap_be_t25, utopia_apocalypse_fragility, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(utap_be_t50, utopia_apocalypse_fragility, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(utopia_apocalypse_fragility, enforcement_mechanism).
narrative_ontology:affects_constraint(utopia_apocalypse_fragility, legitimacy_bifurcation).
narrative_ontology:affects_constraint(utopia_apocalypse_fragility, suppression_escalation_cycle).
narrative_ontology:affects_constraint(utopia_apocalypse_fragility, distributed_ordering_alternatives).

% DUAL FORMULATION NOTE:
% The utopia-apocalypse constraint decomposes into multiple structural subclaims: (1) inherent fragility of complex systems (the mathematical property, approaches Mountain), (2) the manufactured cliff-edge through suppression asymmetry (extraction mechanism, approaches Snare), and (3) the ideological framing that couples small perturbations to apocalyptic outcomes (theater, approaches Piton). These are linked through the network: inherent fragility provides the premise for suppression justification; suppression asymmetry creates the bifurcation that makes the cliff-edge real; the ideological framing amplifies perceived risk to justify suppression intensity. Each story in this family has distinct extractiveness and evidence profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(utopia_apocalypse_fragility, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
