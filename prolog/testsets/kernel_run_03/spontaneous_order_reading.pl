% ============================================================================
% CONSTRAINT STORY: spontaneous_order_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spontaneous_order_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: spontaneous_order_reading
 *   human_readable: Spontaneous Order: Markets as Natural Default, State Intervention as Artificial Imposition
 *   domain: political_economy/institutional_design/economic_ideology
 *
 * SUMMARY:
 *   The spontaneous order reading of markets asserts that markets emerge
 *   naturally from voluntary exchange and that state intervention is an
 *   artificial imposition on this natural order. This reading has deep roots
 *   in classical liberal political economy (Adam Smith, Hayek) and remains
 *   influential in contemporary policy and academic economics. As a reading
 *   of the contested kernel 'market as natural default,' it occupies one
 *   position in an ongoing epistemic dispute about whether markets are
 *   natural, inevitable, and beneficent or whether they are contingent
 *   institutional arrangements that require active state design and
 *   maintenance. This story generates a constraint from the spontaneous order
 *   perspective: the state's regulatory apparatus appears as an extraction
 *   mechanism and artificial barrier, while market mechanisms appear as
 *   coordination solutions. The constraint exhibits high theater (academic
 *   teaching of market efficiency despite counterevidence) and moderate
 *   extractiveness (the reading legitimizes certain structural asymmetries
 *   while delegitimizing alternative institutional forms). The expanding
 *   extractiveness over the interval reflects accumulating evidence of market
 *   failures, financial crises, and inequality that the spontaneous order
 *   frame must absorb through increasingly complex theoretical
 *   elaboration—theater rises as empirical support for the base claim
 *   declines.
 *
 * KEY AGENTS:
 *   - Market Beneficiary Institutions: Primary beneficiary (institutional/arbitrage) — firms and capital holders benefit from deregulatory framing; have arbitrage options across jurisdictions and can exit high-regulation regimes
 *   - Dependent Laborers: Primary victim (powerless/trapped) — wage workers constrained by survival necessity in labor markets; frame naturalizes asymmetric bargaining power as market equilibrium
 *   - Regulatory Authorities: Secondary victim (institutional/constrained) — state capacity is delegitimized; constrained exit because dismantling state institutions is politically costly and creates coordination failures
 *   - Small Producers: Secondary beneficiary-victim (moderate/constrained) — experience coordination benefits (price signals) alongside extraction pressure (competition from larger actors); frame legitimizes competitive pressure as natural selection
 *   - Academic Economists: Institutional custodian (institutional/mobile) — maintain and teach the frame as intellectual authority; high theater because persistence relies on pedagogical legitimacy rather than empirical falsification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — can recognize the false summit structure: the reading declares beneficiaries (market institutions), which triggers FSM detection and reveals the constraint as constructed rather than natural
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spontaneous_order_reading, 0.38).
domain_priors:suppression_score(spontaneous_order_reading, 0.62).
domain_priors:theater_ratio(spontaneous_order_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spontaneous_order_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(spontaneous_order_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(spontaneous_order_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spontaneous_order_reading, tangled_rope).
narrative_ontology:human_readable(spontaneous_order_reading, "Spontaneous Order: Markets as Natural Default, State Intervention as Artificial Imposition").
narrative_ontology:topic_domain(spontaneous_order_reading, "political_economy/institutional_design/economic_ideology").

domain_priors:requires_active_enforcement(spontaneous_order_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(spontaneous_order_reading, '40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65').
narrative_ontology:cs_created_at('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', '').
narrative_ontology:cs_kernel_codification('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', fixed_text).
narrative_ontology:cs_authority_grounding('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', lineage).
narrative_ontology:cs_interpretation_layer_present('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65').
narrative_ontology:cs_kernel_id(spontaneous_order_reading, market_as_natural_default).
narrative_ontology:cs_reading_relation('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', engineered_infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', beneficiary_maintenance_reading, influences).
narrative_ontology:cs_axiom('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', foundational, voluntary_exchange_natural_default).
narrative_ontology:cs_axiom_status(voluntary_exchange_natural_default, holdable).
narrative_ontology:cs_axiom_grounding('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', voluntary_exchange_natural_default, conventional).
narrative_ontology:cs_axiom('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', foundational, state_intervention_artificial_imposition).
narrative_ontology:cs_axiom_status(state_intervention_artificial_imposition, holdable).
narrative_ontology:cs_axiom_grounding('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', state_intervention_artificial_imposition, deontological).
narrative_ontology:cs_reference_frame('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', natural_market_equilibrium).
narrative_ontology:cs_drift_state('40b5f37e-74a4-4a86-b1f3-d6ce7b6dcf65', contemporary_post_crisis_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spontaneous_order_reading, market_participants_with_low_regulation_exposure).
narrative_ontology:constraint_victim(spontaneous_order_reading, state_regulatory_authority).
narrative_ontology:constraint_victim(spontaneous_order_reading, agents_unable_to_exit_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT LABORER (SNARE) — Trapped in market wage dynamics with no alternative livelihood option. The spontaneous order reading frames labor market constraints as natural equilibrium rather than structural extraction. No meaningful exit; suppression is experienced as inexorable market logic. Maximum asymmetric extraction from this position.
constraint_indexing:constraint_classification(spontaneous_order_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL PRODUCER (TANGLED ROPE) — Moderate agency within market constraints. Experiences genuine coordination benefits (price signals, voluntary exchange) alongside extraction pressure (larger competitors, asymmetric information). Can exit to subsistence or cooperative structures at significant cost. Mixed experience: some coordination function (market enables specialization) combined with asymmetric extraction (market concentration).
constraint_indexing:constraint_classification(spontaneous_order_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MARKET BENEFICIARY INSTITUTION (ROPE) — Capital-concentrated actor (large firm, financial institution) experiences markets as coordination mechanism with net benefit. Price signals, liquidity, access to resources — all coordination functions that benefit this agent. Can arbitrage across markets or jurisdictions. The spontaneous order frame legitimizes their positional advantage as natural outcome.
constraint_indexing:constraint_classification(spontaneous_order_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — State agency faces institutional constraint: genuine coordination problems require state capacity (labor safety, environmental externalities, consumer protection), yet the spontaneous order reading frames ANY such intervention as artificial imposition. Constrained exit: dismantling regulatory capacity is politically costly and creates coordination failures. Experiences extraction pressure from the spontaneous order framing itself — the reading justifies dismantling the state's authority.
constraint_indexing:constraint_classification(spontaneous_order_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC ECONOMICS ESTABLISHMENT (PITON) — Institutional deployment of spontaneous order framing as intellectual legitimacy rather than functional claim. The reading persists through institutional inertia and theater: teaching market efficiency despite contradictory evidence (behavioral economics, market failures, inequality literature). Theater ratio is high because the professional commitment to the frame persists despite erosion of empirical support. Theater maintains intellectual identity and disciplinary boundary.
constraint_indexing:constraint_classification(spontaneous_order_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT CANDIDATE (MOUNTAIN) — From universal scope, one might argue spontaneous order reflects irreducible properties of human preference aggregation and coordination — a natural law of voluntary exchange. However, the declaration of market beneficiaries and state as victim indicates this is a false summit: identifying who benefits and who bears costs reveals the constraint as a constructed institutional arrangement with identifiable beneficiary interests, not an immutable natural law.
constraint_indexing:constraint_classification(spontaneous_order_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spontaneous_order_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spontaneous_order_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spontaneous_order_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(spontaneous_order_reading, TR),
    TR >= 0.70.

:- end_tests(spontaneous_order_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The spontaneous order reading extracts by legitimizing market asymmetries (wage suppression through supply-demand framing, information asymmetries, collective action traps) while delegitimizing state regulatory alternatives. The extraction is not maximal because genuine coordination functions do exist in markets (price signals, voluntary specialization, efficiency gains from trade). However, extractiveness increases over the interval (0.22 → 0.38) as accumulating evidence of market failures (financial crises, environmental externalities, rising inequality) requires increasingly elaborate theoretical defenses of the base claim, indicating rising theater to maintain the frame. Suppression (0.62): High. The spontaneous order frame suppresses awareness of state capacity to address coordination failures, creates ideological barrier to regulatory experimentation, and naturalizes what are contingent institutional arrangements. Suppression operates through three channels: (1) intellectual hegemony in economics (teaching markets as natural default despite contradictory evidence), (2) rhetorical closure (labeling any state action as artificial imposition, pre-emptively dismissing empirical objections), (3) institutional incentives that reward elaboration of market efficiency theory while funding comparative research on state alternatives remains scarce. Theater ratio (0.68): High and rising. Academic deployment of spontaneous order framing increasingly consists of theoretical sophistication (behavioral economics incorporating irrationality yet preserving market efficiency conclusions, financial economics developing models of efficient markets despite empirical crashes) rather than empirical falsification. Theater rises because the frame persists through pedagogical commitment and disciplinary identity despite erosion of empirical support.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The market beneficiary institution sees pure coordination and net benefit (Rope) — they experience markets as mechanisms that serve their interests and can arbitrage across jurisdictions if dissatisfied. The dependent laborer sees pure extraction with no exit (Snare) — the spontaneous order framing naturalizes their desperation-constrained choices as voluntary exchange. The regulatory authority sees itself as victim of delegitimization (Tangled Rope) — constrained by genuine coordination problems that require state capacity, yet the frame frames their attempts to address these problems as artificial interference. The academic establishment sees the frame as institutional theater (Piton) — it persists through intellectual identity and pedagogy rather than empirical support. The small producer occupies a mixed space (Tangled Rope) — experiencing both coordination benefits and extraction pressure, with ambiguous exits. The analytical observer risks seeing natural law (false summit Mountain) unless they recognize that declaring market beneficiaries triggers FSM detection and reveals the constraint as constructed rather than natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by whether the agent benefits from or bears costs under this constraint. Market beneficiary institutions have d ≈ 0.1 (low): they benefit from the frame, experience low f(d), and thus low experienced extraction — the frame coordinates in their favor. Dependent laborers have d ≈ 0.92 (high): they bear costs (wage suppression, inability to demand regulatory protection), trapped with no exit, and thus experience maximum f(d) and high χ. The regulatory authority has d ≈ 0.65 (moderate-high): constrained to exist within institutional hierarchy while the frame delegitimizes their core function — the constraint extracts from them by denying legitimacy to their labor. Small producers have d ≈ 0.58 (moderate): they benefit from some market functions (price signals) but bear competitive extraction pressure from larger firms, with constrained exit. The academic establishment has d ≈ 0.35 (moderate-low): they benefit from the frame through disciplinary identity and hierarchical position, yet the rising theater indicates internal strain — the institutional identity depends on defending a claim against mounting evidence, extracting time and intellectual energy into theoretical defense.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by showing that tangled_rope classification accommodates both genuine coordination function (price signals enable specialization and exchange) AND asymmetric extraction (the frame legitimizes market power asymmetries, suppresses awareness of state regulatory capacity as alternative coordination mechanism). The mandatrophy—whether state regulation is natural/necessary or artificial/extractive—IS the kernel disagreement. This reading resolves it by fiat: declaring state intervention artificial by definitional assertion rather than structural analysis. The constraint's increasing theater ratio (0.48 → 0.68) indicates the frame is accumulating conceptual strain—it must absorb evidence of market failures while preserving the claim that markets are natural defaults. This strain shows in the piton perspective: academic institutional persistence of the frame increasingly relies on theoretical elaboration and pedagogical commitment rather than empirical falsification of alternatives. The false summit perspective reveals that the frame's persistence depends on not recognizing beneficiaries—once market beneficiary institutions are declared, FSM detection identifies the constraint as constructed rather than natural, undermining the mountain classification and forcing engagement with how the frame benefits specific agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_voluntary,
    'What constitutes genuine voluntariness in market exchange when structural dependency, asymmetric information, and desperation constrain effective choice?',
    'Empirical analysis of exit options for market participants; comparison of choice capacity across income, power, and structural position; counterfactual analysis of transaction patterns under different bargaining conditions',
    'If ''voluntariness'' requires substantive exit options: many market exchanges are coerced by necessity (labor markets under survival constraints), reclassifying extraction patterns from rope to snare. If ''voluntariness'' is purely formal (absent explicit threat): the spontaneous order frame applies even to highly asymmetric exchanges. This is the deepest frame-dependence in the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_of_voluntary, conceptual, 'Definition of voluntariness in market exchange under structural constraints').

omega_variable(
    externality_boundary,
    'Which harms count as market-internal costs (bearable by participants) vs. external costs (justifying state intervention) vs. coordination failures (resolvable by markets themselves)?',
    'Comparative institutional analysis: historical cases where markets internalized externalities without state intervention vs. cases requiring state action; analysis of whether externalities are structurally internal or external to market mechanism',
    'If externalities are internal: state regulation is unnecessary artificial imposition, spontaneous order frame holds. If externalities are inherent to market structure: some regulatory intervention is coordination enabler, not imposition — reclassifies large portion of state action from extraction to rope/tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_boundary, empirical, 'Boundary between market-internal costs and external harms justifying intervention').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint an observation of how markets actually operate (spontaneous order as empirical claim) or a normative prescription about how they should be treated (spontaneous order as political claim)?',
    'Conceptual clarification: examining whether the reading functions as descriptive account (testable against market behavior) or normative frame (resistant to empirical falsification). Historical analysis of how the reading responds to contradictory evidence (market failures, inequality, financial crises).',
    'If empirical: the reading should accommodate robust counterevidence (market failures, coordination breakdowns); persistence despite falsification indicates the reading functions as political ideology masquerading as natural law. If normative: the reading claims markets SHOULD operate freely regardless of empirical outcomes — makes different claim about state legitimacy and deserves different evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether spontaneous order is empirical claim or normative prescription').

omega_variable(
    state_extraction_measurement,
    'How do we measure and compare state regulatory extraction (assuming it IS extraction) against market-native extraction mechanisms (monopoly power, information asymmetry, collective action problems)?',
    'Develop comparative metrics: distributional impact of state intervention vs. absence of intervention; rent-seeking by regulatory capture vs. rent-seeking by market power concentration; transaction costs of state mechanisms vs. market mechanisms for solving the same coordination problem',
    'If state extraction proves lower than market-native extraction for specific coordination problem: state intervention is extraction-reducing, reclassifying from snare to tangled_rope. If market-native extraction is lower: spontaneous order frame is correct for that domain. Likely outcome: heterogeneous findings across domains, requiring domain-specific rather than universal spontaneous order claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_extraction_measurement, empirical, 'Comparative measurement of state vs. market extraction mechanisms').

omega_variable(
    institutional_memory_loss,
    'Does the academic/policy persistence of spontaneous order framing reflect genuine ongoing empirical support, or has the reading become decoupled from empirical scrutiny through institutional inertia (piton mechanism)?',
    'Historical analysis of when the spontaneous order reading entered economics canon and whether subsequent empirical challenge has shifted the reading''s institutional position. Compare citation patterns: theoretical elaboration of spontaneous order vs. empirical testing; measure proportion of economic research devoted to market failure vs. market efficiency topics.',
    'If piton mechanism confirmed: the constraint''s persistence relies on theater and institutional identity rather than empirical vindication. This would reclassify the piton perspective from observational to prescriptive: the institutional preservation is what maintains the reading despite falsifying evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_memory_loss, empirical, 'Whether institutional persistence of spontaneous order frame reflects empirical support or inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spontaneous_order_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spon_tr_t0, spontaneous_order_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(spon_tr_t20, spontaneous_order_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(spon_tr_t40, spontaneous_order_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(spon_be_t0, spontaneous_order_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(spon_be_t20, spontaneous_order_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(spon_be_t40, spontaneous_order_reading, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spontaneous_order_reading, resource_allocation).
narrative_ontology:affects_constraint(spontaneous_order_reading, engineered_infrastructure_reading).
narrative_ontology:affects_constraint(spontaneous_order_reading, beneficiary_maintenance_reading).
narrative_ontology:affects_constraint(spontaneous_order_reading, regulatory_capture_coordination).
narrative_ontology:affects_constraint(spontaneous_order_reading, market_failure_suppression).

% DUAL FORMULATION NOTE:
% This is one reading of the contested kernel market_as_natural_default. The sibling readings (engineered_infrastructure_reading, beneficiary_maintenance_reading) emit different constraints with different ε values and victim sets from the same underlying institutional phenomena. The spontaneous order reading is upstream in legitimacy discourse—it provides the ideological frame that the other readings contest. When this reading's authority erodes (rising theater, accumulating falsification), the downstream readings become more structurally visible as alternatives. Link this story to its siblings via network.affects_constraints to model the kernel dispute structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(spontaneous_order_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
