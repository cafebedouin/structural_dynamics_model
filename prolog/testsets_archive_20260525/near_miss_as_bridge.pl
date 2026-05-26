% ============================================================================
% CONSTRAINT STORY: near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_near_miss_as_bridge, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: near_miss_as_bridge
 *   human_readable: Near-Miss Integration as Bridge Between Simulation and Real-World Learning
 *   domain: safety_engineering/organizational_learning/high_reliability_operations
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear power, aviation, healthcare) face
 *   a fundamental learning constraint: the rarest, most catastrophic failure
 *   modes are by definition the hardest to study empirically.
 *   Full-catastrophe testing is ethically and economically prohibitive.
 *   Simulators enable safe training but cannot reproduce the full complexity
 *   of real-world operations, leaving a fidelity gap. Near-miss incidents —
 *   failures that were contained or corrected before cascading — occupy a
 *   structural middle ground: they provide real-world complexity without
 *   catastrophic consequences, enabling investigation and feedback into
 *   simulator design. This constraint examines whether near-miss integration
 *   constitutes a sufficient bridge between simulator-based training and
 *   operational competence validation, or whether it represents an extraction
 *   mechanism that burdens operators and investigation capacity while serving
 *   training organization and regulatory interests. The constraint exhibits
 *   tangled-rope structure: genuine coordination function (near-miss analysis
 *   does improve simulator fidelity and operator learning) coupled with
 *   asymmetric extraction (investigation resources redirected toward
 *   simulator updates rather than systemic prevention, operators bear risk
 *   while training organizations capture legitimacy benefits).
 *
 * KEY AGENTS:
 *   - Frontline Operators: Primary victims (powerless/trapped) — participate in both simulator training and real-world deployment; their near-miss experience is systematically extracted for training validation; cannot exit the system without losing occupational identity
 *   - Incident Investigation Capacity: Secondary victim (moderate/constrained) — safety engineers and human factors specialists benefit from constraint (expertise centralized) while bearing extraction costs (investigation redirected toward simulator updates rather than root-cause prevention); face resource constraints and time pressure
 *   - Training Organization and Regulatory Certifier: Primary beneficiaries (institutional/arbitrage) — near-miss integration provides data-driven validation pathway avoiding expensive catastrophe testing; enables rapid simulator updates and regulatory approval; captures legitimacy from evidence-based training claims
 *   - Safety Standards Coalition: Organized agent (organized/constrained) — regulatory bodies, safety advocacy, worker unions; see near-miss integration as temporary bridge with sunset toward prevention-focused design maturity
 *   - Simulator-First Training Orthodoxy: Institutional incumbency (institutional/arbitrage) — traditional view that simulator fidelity is primary validation; maintains itself through budget allocation and training doctrine despite reduced explanatory power; piton classification reflects performative maintenance of doctrine
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent organizational choices (using near-misses as sufficient feedback) as immutable constraints of safety system design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(near_miss_as_bridge, 0.52).
domain_priors:suppression_score(near_miss_as_bridge, 0.48).
domain_priors:theater_ratio(near_miss_as_bridge, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(near_miss_as_bridge, extractiveness, 0.52).
narrative_ontology:constraint_metric(near_miss_as_bridge, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(near_miss_as_bridge, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(near_miss_as_bridge, "Near-Miss Integration as Bridge Between Simulation and Real-World Learning").
narrative_ontology:topic_domain(near_miss_as_bridge, "safety_engineering/organizational_learning/high_reliability_operations").

domain_priors:requires_active_enforcement(near_miss_as_bridge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(near_miss_as_bridge, training_organization).
narrative_ontology:constraint_beneficiary(near_miss_as_bridge, regulatory_approval_pathway).
narrative_ontology:constraint_victim(near_miss_as_bridge, frontline_operators).
narrative_ontology:constraint_victim(near_miss_as_bridge, incident_investigation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Trapped by the constraint that their real-world near-miss experience is systematically underutilized for simulator validation while they remain the primary test subject. Operators bear the risk and cognitive load of incident investigation without proportional authority over how findings are integrated into training. They cannot exit: operating in high-risk domains requires participation in both simulation and real-world deployment. Maximum experienced extraction.
constraint_indexing:constraint_classification(near_miss_as_bridge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INCIDENT INVESTIGATION CAPACITY (TANGLED ROPE) — Safety engineers, human factors specialists, and investigation teams benefit from the constraint (their expertise becomes central to training updates) while bearing extraction costs (systematic pressure to redirect investigation findings toward simulator updates rather than systemic safety improvements, constraint of time and resources away from deeper causal analysis). Mixed position: genuine coordination function (near-miss analysis) coupled with asymmetric extraction (investigation redirected to serve training rather than vice versa).
constraint_indexing:constraint_classification(near_miss_as_bridge, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRAINING ORGANIZATION & CERTIFIER (ROPE) — Benefits from the near-miss integration constraint: it provides data-driven validation pathway that avoids costly full-catastrophe testing while enabling rapid simulator updates and regulatory approval cycles. Near-miss data supplies legitimacy claims (evidence-based training) and reduces liability exposure. The constraint is experienced as pure coordination: integrating real-world learning into simulators solves the genuine problem of simulator-reality fidelity without waiting for disasters.
constraint_indexing:constraint_classification(near_miss_as_bridge, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SAFETY STANDARDS COALITION (SCAFFOLD) — Organized agents (regulatory bodies, safety advocacy groups, worker unions) see near-miss integration as temporary bridge with sunset logic: as organizational learning maturity increases and systemic root-cause analysis matures, the near-miss-as-sufficient pathway should evolve toward prevention-focused design rather than incident-responsive training updates. The sunset: institutionalize proactive hazard identification that makes near-miss investigation reactive rather than foundational. Sunset timeline: 15-20 years as safety culture matures.
constraint_indexing:constraint_classification(near_miss_as_bridge, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SIMULATOR-FIRST ORTHODOXY (PITON) — Traditional view that simulator fidelity is the primary validation mechanism, with real-world feedback relegated to post-hoc refinement. This perspective has largely degraded: simulators cannot reproduce the full complexity of real-world scenarios, yet the institutional commitment to simulator primacy persists through budget allocation, training doctrine, and regulatory expectations. Theater ratio reflects that much simulator development is performative certification of adequacy rather than functional testing. The orthodoxy maintains itself through institutional inertia despite reduced explanatory power.
constraint_indexing:constraint_classification(near_miss_as_bridge, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scope, some learning lag between simulator and reality is inherent to safety system design: no simulator can capture all contingencies, and real-world operations will always discover edge cases the simulator missed. This perspective sees near-miss integration as an immutable consequence of bounded model fidelity — not a choice but a structural necessity. However, the beneficiary declarations (training organization benefits from the current arrangement) suggest this is a false summit: the framing naturalizes institutional choices as physical laws.
constraint_indexing:constraint_classification(near_miss_as_bridge, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(near_miss_as_bridge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(near_miss_as_bridge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(near_miss_as_bridge, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(near_miss_as_bridge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(near_miss_as_bridge, TR),
    TR >= 0.70.

:- end_tests(near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increased from initial near-miss-friendly estimates. The constraint extracts from operators (bears risk of both simulator inadequacy and real-world operations while training organizations capture safety validation benefits) and from investigation capacity (deep causal analysis is systematically deprioritized in favor of simulator-addressable lessons). The extraction is not as severe as pure snares because genuine coordination function exists (near-miss investigation does produce valid learning), but the asymmetry is substantial. The value increased over the 8-year interval, reflecting organizational drift: as simulators become more sophisticated, the pressure to validate them using real-world data intensifies, increasing the extraction intensity on operators and investigators. Suppression (0.48): Moderate-high. Structural barriers include occupational requirement for simulator participation, regulatory mandates for training documentation, career risk of incident involvement (near-miss participation can be career-damaging even when no operator error occurred), and absence of alternative certification pathways. Suppression is not total because operators do have some exit optionality (changing occupations, requesting reassignment) and investigation teams can push back against redirection pressure, but the costs are substantial. Theater ratio (0.61): Moderate-high. Reflects that simulator validation protocols incorporate performative elements: sign-offs on simulator adequacy based on near-miss integration without deep verification that integrated scenarios actually address root causes; certification rituals that attest to fidelity without empirical testing; regulatory approval cycles that treat near-miss-derived updates as sufficient without independent validation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival differentiation across power levels and exit options. The training organization (institutional/arbitrage) experiences rope classification — pure coordination mechanism solving the genuine problem of simulator-reality fidelity. Frontline operators (powerless/trapped) experience snare classification — extraction mechanism with no apparent exit and no corresponding benefit. Investigation teams (moderate/constrained) experience tangled rope — mixed coordination (their expertise is valued) and extraction (investigation is redirected toward training rather than prevention). The organized safety coalition (organized/constrained) experiences scaffold classification — they see the constraint as temporary, with sunset logic based on organizational maturity. The simulator-first orthodoxy (institutional/arbitrage, civilizational) experiences piton classification — its traditional validation mechanism has degraded but persists through institutional inertia. The analytical observer risks mountain classification — naturalizing the learning gap as an inherent property of safety system design. The engine's false-summit detector flags the mountain as constructed rather than natural: the framing that 'near-misses are necessary to validate simulators' is an institutional choice, not a law of physics or organizational necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent and exit structure. Training organizations (beneficiary status + arbitrage exit) derive d ≈ 0.10-0.15, yielding f(d) ≈ -0.01 to 0.02, producing low or negative effective extraction (they benefit, experience the constraint as coordination). Operators (victim status + trapped exit) derive d ≈ 0.92, yielding f(d) ≈ 1.35, producing high experienced extractiveness (they bear costs with minimal escape route). Investigation capacity (mixed beneficiary/victim + constrained exit) derives d ≈ 0.55-0.60, yielding f(d) ≈ 0.75-0.85, producing moderate extractiveness (genuine coordination function coupled with extraction cost). Organized safety coalition (beneficiary with sunset + constrained exit) derives d ≈ 0.35-0.40, yielding f(d) ≈ 0.35-0.45, producing low-moderate extractiveness compatible with scaffold classification. The piton perspective (institutional/arbitrage at civilizational scope) derives d from canonical fallback ≈ 0.00, yielding f(d) ≈ -0.12, but the piton classification overrides based on theater_ratio ≥ 0.70. The mountain perspective (analytical/analytical) derives d ≈ 0.72 from canonical, yielding f(d) ≈ 1.15, suggesting high extractiveness, but the mountain classification gates on ε ≤ 0.25 and suppression ≤ 0.05, both false here — triggering false-summit detection via beneficiary presence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by differentiating organizational learning regimes: different safety domains weight the three sibling readings differently. Aviation has moved toward near-miss-as-bridge (moderate extractiveness accepted for learning benefit). Nuclear has moved toward simulation-as-sufficient (high theater ratio maintained through extensive simulator validation). Healthcare remains oscillating between near-miss-as-bridge and catastrophe-as-necessary (rare catastrophic events periodically trigger regulatory intervention that shifts the balance). The mandatrophy in the competence_retention_exercise kernel may be irresolvable — it may be that organizations fundamentally require all three constraint types simultaneously, and the question is not which reading is 'correct' but rather how they balance across time and incident cycles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_sufficiency_threshold,
    'How many near-miss events and their investigated root causes are required before simulator validation is empirically equivalent to full-catastrophe testing?',
    'Statistical analysis of near-miss findings vs catastrophic failure modes across multiple safety domains; correlation between near-miss investigation coverage and actual failure prevention; comparison of simulator update lag time under near-miss-driven vs catastrophe-driven regimes',
    'If threshold is achievable within organizational learning cycles (e.g., 100-500 incidents): near-miss integration is genuinely sufficient bridge (Rope or Scaffold from beneficiary perspective). If threshold requires orders of magnitude more events: constraint remains extraction mechanism disguised as learning, and Snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_sufficiency_threshold, empirical, 'Minimum near-miss investigation coverage for simulator validation equivalence').

omega_variable(
    investigation_depth_vs_simulator_utility_tradeoff,
    'Does redirecting investigation resources toward simulator integration shallow the root-cause analysis that would enable systemic prevention?',
    'Comparative analysis of investigation reports in near-miss-integration vs systemic-prevention-focused organizations; correlation between investigation depth and effectiveness of preventive design changes; longitudinal tracking of whether simulator updates address root causes or symptoms',
    'If integration redirects investigation toward simulator-addressable failures (symptoms): extraction cost to investigation capacity is real (Tangled Rope confirmed). If integration is compatible with deep root-cause analysis: mixed cost-benefit (Rope from investigator perspective possible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(investigation_depth_vs_simulator_utility_tradeoff, empirical, 'Whether near-miss-to-simulator focus narrows investigation scope').

omega_variable(
    kernel_reading_sufficiency_framing,
    'Is near-miss integration sufficient as a standalone learning mechanism, or does it require coupling with catastrophe-triggered interventions to maintain operational credibility?',
    'Historical case studies: organizations that achieved safety improvement through near-miss integration alone vs those requiring regulatory-triggered catastrophe response; analysis of organizational learning acceptance when based solely on near-miss data vs when validated by rare catastrophic failure.',
    'If sufficient alone: the near-miss-as-bridge reading is structurally independent. If requires catastrophe validation: the reading understates the coupling to catastrophe-as-necessary, and the competence_retention_exercise kernel shows more complex dependencies than the three sibling readings capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sufficiency_framing, empirical, 'Whether near-miss learning is sufficient without catastrophe coupling').

omega_variable(
    kernel_sibling_reading_overlap,
    'How do the three sibling readings (near_miss_as_bridge, simulation_as_sufficient, catastrophe_as_necessary) actually relate structurally? Are they mutually exclusive or layered?',
    'Analyze each reading as a distinct constraint story with its own epsilon and classification. Compare their overlapping beneficiary/victim structures, exit option differentiation, and temporal horizons. Map how organizational learning regimes instantiate different weightings of these three constraints.',
    'If mutually exclusive: exactly one reading applies per domain (Snare/Rope/Mountain depending on empirical case). If layered: all three constraints operate simultaneously, and organizations navigate between them via regulatory cycles and incident-triggered rebalancing. Determines whether mandatrophy in the competence_retention_exercise kernel is resolvable or constitutive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_sibling_reading_overlap, conceptual, 'Structural relationship between the three sibling readings of competence_retention_exercise kernel').

omega_variable(
    operator_identity_lock_vs_structural_trap,
    'Are frontline operators trapped by material barriers (cannot refuse simulator participation or operational deployment) or identity-locked (their professional identity and competence self-concept are constituted through the safety system)?',
    'Ethnographic analysis: do operators conceptualize exit from the system as losing a material livelihood, or as losing their identity as competent practitioners? Post-exit trajectory: do operators retain safety mindset outside the system, or does it dissolve when the constraint structure is removed?',
    'If trapped (material barriers): focus remediation on restructuring exit options. If identity-locked (cognitive frame): constraint persists even after material barriers are removed, and systemic reform requires identity-frame remaking. Affects whether classification should upgrade to constrained or remain trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operator_identity_lock_vs_structural_trap, empirical, 'Whether operator binding is material entrapment or identity fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(near_miss_as_bridge, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nmab_tr_t0, near_miss_as_bridge, theater_ratio, 0, 0.52).
narrative_ontology:measurement(nmab_tr_t4, near_miss_as_bridge, theater_ratio, 4, 0.58).
narrative_ontology:measurement(nmab_tr_t8, near_miss_as_bridge, theater_ratio, 8, 0.61).

% Extraction over time
narrative_ontology:measurement(nmab_be_t0, near_miss_as_bridge, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nmab_be_t4, near_miss_as_bridge, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(nmab_be_t8, near_miss_as_bridge, base_extractiveness, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(near_miss_as_bridge, 0.25).
narrative_ontology:affects_constraint(near_miss_as_bridge, simulation_as_sufficient).
narrative_ontology:affects_constraint(near_miss_as_bridge, catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% The competence_retention_exercise kernel has three structural readings: near_miss_as_bridge (this story), simulation_as_sufficient, and catastrophe_as_necessary. Each reading is a distinct constraint with its own epsilon, beneficiary/victim structure, and perspectival classification profile. They are not competing measurements of one phenomenon but genuinely alternative structural arrangements that organizations employ at different historical moments and in different safety domains. Network linkage captures that the three readings are upstream/downstream of each other: near_miss integration depends on simulation infrastructure (upstream: simulation_as_sufficient) while potentially preventing catastrophe-triggered learning (downstream: catastrophe_as_necessary). The epsilon values differ significantly: near_miss_as_bridge epsilon ≈ 0.52 reflects mixed coordination and extraction; simulation_as_sufficient epsilon ≈ 0.28 (lower extraction, more pure coordination); catastrophe_as_necessary epsilon ≈ 0.75 (higher extraction, catastrophe-driven learning imposes severe costs before generating learning).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(near_miss_as_bridge, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
