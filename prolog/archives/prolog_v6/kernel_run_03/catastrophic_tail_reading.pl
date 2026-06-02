% ============================================================================
% CONSTRAINT STORY: catastrophic_tail_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophic_tail_reading, []).

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
 *   constraint_id: catastrophic_tail_reading
 *   human_readable: Catastrophic Tail Risk Reading in Acceptable Risk Governance
 *   domain: energy_policy/risk_assessment/public_safety_governance
 *
 * SUMMARY:
 *   The catastrophic-tail reading of acceptable risk in energy policy
 *   constructs a decision framework where catastrophic outcomes (meltdown,
 *   explosion, uncontrollable wildfire, runaway climate feedback) are
 *   weighted equally with or above expected-value calculations, regardless of
 *   probability. Under this reading, collective risk decisions must prevent
 *   maximum credible catastrophes even when the expected value of prevention
 *   exceeds the expected loss from the catastrophe occurring. This constraint
 *   operates as a kernel reading — one of three competing frameworks
 *   (catastrophic-tail, expected-value, and precautionary principle) for
 *   interpreting what 'acceptable risk' means in governance. The
 *   catastrophic-tail reading structures policy across energy infrastructure
 *   (forcing renewable penetration over cost-optimal baseload), land
 *   management (wildfire prevention mandates), industrial safety (redundancy
 *   requirements), and climate policy (emissions limits that exceed
 *   cost-benefit optima). The constraint exhibits tangled-rope structure: it
 *   coordinates legitimate catastrophe prevention (genuine coordination
 *   function) while simultaneously extracting from cost-bearing
 *   constituencies by forcing expensive alternatives regardless of
 *   expected-value analysis. The theater ratio measures the gap between the
 *   theoretical justification (tail-risk dominance in decision theory) and
 *   practical governance (most policy analysis ultimately trades catastrophe
 *   probability against cost, reweighting rather than eliminating probability
 *   from the calculation). Over 45 years of operation, both theater and
 *   extractiveness have drifted upward: as major predicted catastrophes
 *   failed to materialize, the framework persists through institutional
 *   inertia; simultaneously, the constraint's enforcement requires
 *   increasingly elaborate justifications and engineering requirements,
 *   raising the performance burden on risk agencies.
 *
 * KEY AGENTS:
 *   - Potential Disaster-Affected Populations: Primary victim group (powerless/trapped) — geographic or demographic exposure to catastrophe; cannot exit risk exposure; their protection is the stated constraint justification, but suppression of probability-weighted analysis excludes their empirical risk preferences
 *   - Risk-Conscious Energy Consumers: Secondary victim group (moderate/constrained) — bear transition costs through higher energy prices; partial exit available at high cost (relocation, off-grid conversion)
 *   - Renewable Energy Industry: Primary beneficiary (institutional/arbitrage) — direct winner from policy mandate favoring renewable alternatives; arbitrage exit available through supply chain flexibility; benefits from catastrophic-tail reading regardless of disaster probability
 *   - Baseload Operators (Nuclear/Fossil): Secondary beneficiary/victim (organized/constrained) — sunk capital stranded by regulatory preference for alternatives; provide genuine coordination function (reliable baseload) while experiencing extraction through forced retirement or retooling; constrained by regulatory barriers rather than market forces
 *   - Risk Regulatory Agencies: Institutional maintainers (institutional/arbitrage) — see their own framework as increasingly performative; maintain catastrophic-tail language while conducting hidden expected-value analysis in actual decision-making; theater ratio reflects this gap
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent reading as logical necessity; the precautionary principle argument can appear as natural law but masks the specific institutional commitments embedded in catastrophic-tail framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophic_tail_reading, 0.58).
domain_priors:suppression_score(catastrophic_tail_reading, 0.68).
domain_priors:theater_ratio(catastrophic_tail_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophic_tail_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophic_tail_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(catastrophic_tail_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophic_tail_reading, tangled_rope).
narrative_ontology:human_readable(catastrophic_tail_reading, "Catastrophic Tail Risk Reading in Acceptable Risk Governance").
narrative_ontology:topic_domain(catastrophic_tail_reading, "energy_policy/risk_assessment/public_safety_governance").

domain_priors:requires_active_enforcement(catastrophic_tail_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophic_tail_reading, '2b8f74d6-d5da-4999-b57a-efdc9d074102').
narrative_ontology:cs_created_at('2b8f74d6-d5da-4999-b57a-efdc9d074102', '').
narrative_ontology:cs_kernel_codification('2b8f74d6-d5da-4999-b57a-efdc9d074102', formalized).
narrative_ontology:cs_authority_grounding('2b8f74d6-d5da-4999-b57a-efdc9d074102', extraction).
narrative_ontology:cs_interpretation_layer_present('2b8f74d6-d5da-4999-b57a-efdc9d074102').
narrative_ontology:cs_kernel_id(catastrophic_tail_reading, acceptable_risk_for_energy).
narrative_ontology:cs_reading_relation('2b8f74d6-d5da-4999-b57a-efdc9d074102', expected_value_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b8f74d6-d5da-4999-b57a-efdc9d074102', precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('2b8f74d6-d5da-4999-b57a-efdc9d074102', foundational, catastrophic_outcome_dominance).
narrative_ontology:cs_axiom_status(catastrophic_outcome_dominance, holdable).
narrative_ontology:cs_axiom_grounding('2b8f74d6-d5da-4999-b57a-efdc9d074102', catastrophic_outcome_dominance, deontological).
narrative_ontology:cs_axiom('2b8f74d6-d5da-4999-b57a-efdc9d074102', foundational, irreversibility_precludes_probability_discount).
narrative_ontology:cs_axiom_status(irreversibility_precludes_probability_discount, holdable).
narrative_ontology:cs_axiom_grounding('2b8f74d6-d5da-4999-b57a-efdc9d074102', irreversibility_precludes_probability_discount, deontological).
narrative_ontology:cs_reference_frame('2b8f74d6-d5da-4999-b57a-efdc9d074102', tail_risk_weighted_decision_framework).
narrative_ontology:cs_drift_state('2b8f74d6-d5da-4999-b57a-efdc9d074102', contemporary_post_decades_without_catastrophe, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophic_tail_reading, risk_averse_constituencies).
narrative_ontology:constraint_beneficiary(catastrophic_tail_reading, renewable_energy_advocates).
narrative_ontology:constraint_beneficiary(catastrophic_tail_reading, future_generations).
narrative_ontology:constraint_victim(catastrophic_tail_reading, energy_production_efficiency).
narrative_ontology:constraint_victim(catastrophic_tail_reading, baseload_dependent_infrastructure).
narrative_ontology:constraint_victim(catastrophic_tail_reading, cost_bearing_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISASTER-AFFECTED POPULATIONS (SNARE) — Trapped in exposure to catastrophic outcomes. No exit from geographic or demographic vulnerability. Suppression is maximal: their preferences are excluded from expected-value calculations that discount low-probability disasters. The constraint forces policy to weight their exposure equally regardless of probability, but the underlying catastrophe remains irreversible. Maximum experienced extraction.
constraint_indexing:constraint_classification(catastrophic_tail_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RISK-CONSCIOUS CONSUMERS (TANGLED ROPE) — Constrained by dependence on energy infrastructure and regional energy policy choices. Partial exit through conservation or relocation (high cost). Benefit from catastrophe prevention infrastructure; bear costs through higher energy prices when the constraint forces expensive renewable buildout. Mixed coordination and asymmetric extraction.
constraint_indexing:constraint_classification(catastrophic_tail_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY INDUSTRY (ROPE) — Arbitrage exit available through relocation of manufacturing, supply chain flexibility. Direct beneficiary of catastrophic-tail reading: policy mandates renewable penetration regardless of expected-value optimization. Constraint functions as coordination mechanism allocating investment and R&D resources toward alternatives. Net positive extraction flow toward this agent.
constraint_indexing:constraint_classification(catastrophic_tail_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BASELOAD OPERATORS (TANGLED ROPE) — Organized agents with significant sunk capital in existing infrastructure. Constrained by regulatory barriers to profitable operation (their stranded assets); limited arbitrage through retooling or divestment. Provide genuine coordination function (reliable baseload power) alongside experiencing asymmetric extraction (policy preference for alternatives regardless of cost-benefit). High suppression: their risk characterization (low-probability failures already engineered out) is discounted in favor of catastrophic-scenario weighting.
constraint_indexing:constraint_classification(catastrophic_tail_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RISK REGULATORY AGENCIES (PITON) — Institutional actors maintaining an increasingly performative framework. The catastrophic-tail reading was grounded in empirical risk science (low-probability event analysis); as catastrophic outcomes remain unrealized over decades, the regulatory framework persists through institutional inertia. Theater ratio reflects gap between the theoretical centrality of tail-risk analysis and the practical governance focus (most policy time spent on expected-value tradeoffs disguised in catastrophic-risk language). Sees its own framework as degraded.
constraint_indexing:constraint_classification(catastrophic_tail_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PRECAUTIONARY PRINCIPLE (MOUNTAIN) — From civilizational scale, some catastrophic outcomes (asteroid impact, engineered pandemic, runaway climate feedback) are existential: probability is irrelevant because a single occurrence ends the framework. This perspective sees the catastrophic-tail reading as reflecting irreducible logical constraints on rational decision-making under radical uncertainty. However, the structural data reveals a false summit: the constraint's extractiveness derives from institutional power asymmetries and career incentive structures, not from logical necessity.
constraint_indexing:constraint_classification(catastrophic_tail_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophic_tail_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophic_tail_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophic_tail_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophic_tail_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophic_tail_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophic_tail_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint's extraction mechanism operates through forced choice of expensive alternatives (renewable infrastructure, redundancy systems) without probability discounting. The extraction is not as severe as a pure snare (0.72+) because the constraint does provide genuine catastrophe prevention benefit and some beneficiary groups (disaster-affected populations) experience real protection. However, the extraction is substantial because the mandate to ignore probability asymmetrically advantages renewable advocates and disadvantages cost-optimization arguments. The moderate level reflects that the constraint generates both coordination function and asymmetric cost-bearing. Suppression (0.68): High. The constraint suppresses explicit probability weighting in policy discourse — decision-makers are forbidden from openly using expected-value reasoning even when that reasoning would be the most transparent decision method. This suppression is structural: saying 'the probability is very low, so we tolerate the risk' is rhetorical weakness under catastrophic-tail framing, even if the probability is accurately estimated. Suppression is not total (0.90+) because cost-benefit analysis persists in practice, hidden beneath catastrophic-risk language. Theater ratio (0.52): Moderate. The theoretical framework (tail-risk dominance) is genuine risk science, not pure performance. However, 52% theater reflects that much policy energy goes to justifying why specific alternatives are chosen — energy sources that seem renewable but may carry different catastrophe profiles (dams triggering earthquakes, concentrated solar causing local heating), or energy efficiency mandates that have their own catastrophe risk profile. The theater has grown over the interval as the constraint's age without predicted catastrophes has reduced its face validity.
 *
 * PERSPECTIVAL GAP:
 *   The catastrophic-tail reading produces perspectival divergence across power axes. Powerless agents (disaster-affected populations) are the named beneficiaries but experience maximum suppression — their actual probability-weighted preferences are excluded from decision-making. Moderate agents experience tangled-rope: constrained by energy dependence, benefiting from catastrophe infrastructure, bearing asymmetric transition costs. Organized agents (baseload operators) experience asymmetric extraction: genuine coordination function undervalued relative to catastrophe prevention mandate. Institutional beneficiaries (renewable industry) experience rope: the constraint coordinates investment allocation and provides arbitrage exit. The analytical observer risks seeing precautionary principle as natural law (mountain), but the structural extraction data reveals a contingent institutional reading competing with expected-value and precautionary alternatives. The perspectival gap is not a communication failure — it reflects that this reading of acceptable risk genuinely benefits and harms different constituencies asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is fixed by the agent's relationship to catastrophe prevention and cost-bearing. Disaster-affected populations have d ≈ 0.95 (full target: extraction runs toward them through suppressed voice and exclusion from probability reasoning). Risk-conscious consumers have d ≈ 0.70 (significant target: constrained exit, higher energy costs). Renewable industry has d ≈ 0.10 (near-full beneficiary: mandated demand, arbitrage flexibility). Baseload operators have d ≈ 0.65 (target, though less severely than consumers: stranded capital, maintained coordination function). Risk agencies have d ≈ 0.30 (slight beneficiary: institutional maintenance of framework, though growing theater burden). The analytical observer has canonical d ≈ 0.72 (observing target positions). The chi formula scales these through f(d) and scope modifiers — powerless agents at global scope experience maximum chi; institutional beneficiaries with arbitrage experience negative or near-zero chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that the catastrophic-tail reading is one of three coherent but mutually exclusive readings of acceptable risk governance. The mandatrophy is not 'is catastrophic-tail correct?' but 'which reading of the kernel is operative?'. The constraint's tangled-rope classification reflects that catastrophic-tail reading provides genuine coordination (catastrophe prevention infrastructure) alongside asymmetric extraction (cost-bearing distributed among those least able to absorb it, probability-weighting suppressed to prevent transparent cost-benefit discourse). The piton perspective (from risk agencies) identifies the theatrical dimension: as decades pass without predicted catastrophes, the framework increasingly requires elaborate justification. The mountain perspective (analytical/precautionary) risks naturalizing a contingent reading as logical necessity. The mandatrophy is resolved by showing that all six types are legitimate readings of catastrophic-tail policy depending on the observer's structural position — not by choosing a single correct type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_definition_ambiguity,
    'What constitutes a ''maximum credible catastrophe'' and how is credibility determined?',
    'Historical analysis of risk assessments: which scenarios were deemed ''credible'' that later proved impossible or highly implausible? Comparison of published catastrophe scenarios across decades to measure drift in what counts as credible.',
    'If catastrophe threshold is loose (environmental damage beyond GDP loss): constraint drives renewable mandate even when expected-value cost of prevention exceeds expected loss. If threshold is tight (civilizational extinction-scale only): constraint becomes rope (pure coordination mechanism) rather than tangled rope (asymmetric extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_definition_ambiguity, conceptual, 'Definition of catastrophe and credibility threshold').

omega_variable(
    probability_discount_mechanism,
    'Is the ''prevention regardless of probability'' mandate a coherent decision rule or a rhetorical construction that masks cost-benefit reasoning?',
    'Policy document analysis: track statements claiming tail-risk dominates expected value; cross-reference with actual cost-benefit assessments in policy construction. Test whether low-probability, high-cost prevention options are consistently chosen over high-probability, moderate-cost alternatives.',
    'If truly probability-indifferent: catastrophic-tail reading is foundational axiom of this reading. If probability is covertly reweighted: the constraint is actually expected-value optimization with altered probability weights — a completely different structural type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_discount_mechanism, empirical, 'Whether probability discount is coherent decision rule or rhetorical construction').

omega_variable(
    renewable_cost_distribution_asymmetry,
    'Are the economic costs of renewable transition borne uniformly by energy consumers, or do they concentrate on specific populations (rural, low-income, capital-constrained)?',
    'Distributional analysis of renewable transition costs: electricity price increases by region, relocation costs for communities dependent on fossil-fuel infrastructure, capital availability for distributed solar in low-income areas. Compare actual cost distribution to the constraint''s claim of protecting ''all potential disaster-affected populations equally.''',
    'If costs concentrate asymmetrically: the constraint''s claimed protection of disaster-affected populations is incomplete — it protects from one risk class (catastrophic industrial failure) while creating new extraction (energy poverty from transition costs). Classification may degrade from tangled_rope to snare for low-income groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_cost_distribution_asymmetry, empirical, 'Distribution of renewable transition costs across populations').

omega_variable(
    sibling_reading_kernel_stability,
    'Which reading of the ''acceptable risk'' kernel is empirically dominant in actual policy?',
    'Institutional analysis: review regulatory agencies'' stated decision rules, cost-benefit analyses in policy documents, and testimony in regulatory hearings. Measure what fraction of risk decisions are made via catastrophic-tail reasoning vs expected-value optimization vs precautionary principle.',
    'If catastrophic-tail reading dominates: this constraint''s structural properties (extractiveness, suppression) accurately reflect institutional operation. If expected-value reading dominates: this constraint is a normative framework imposed against institutional resistance — extraction is the enforcement cost. If precautionary reading dominates: catastrophic-tail reading is foreclosed by the actual authority structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_stability, empirical, 'Empirical dominance of risk reading in actual policy').

omega_variable(
    catastrophe_realization_null_hypothesis,
    'The absence of catastrophic outcomes over 50+ years of this reading''s operation — does it falsify the catastrophic-tail model or confirm successful prevention?',
    'Counterfactual analysis: identify near-misses and prevented incidents; assess whether engineering improvements (post-1970s accident prevention in nuclear, emissions controls reducing climate feedback) reduce catastrophe probability or whether catastrophe probability was always lower than modeled. Compare actual failure rates to predicted rates in original risk assessments.',
    'If absence supports successful prevention: piton classification is incorrect — the constraint is actively preventing real catastrophes and should reclassify toward rope. If absence supports lower-than-modeled probability: catastrophic-tail reading''s entire foundation is empirically questioned — may foreclose this reading in favor of expected-value reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_realization_null_hypothesis, empirical, 'Whether absence of catastrophe confirms prevention success or reveals overestimated risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophic_tail_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophic_tail_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cata_tr_t15, catastrophic_tail_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(cata_tr_t30, catastrophic_tail_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(cata_tr_t45, catastrophic_tail_reading, theater_ratio, 45, 0.58).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophic_tail_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t15, catastrophic_tail_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(cata_be_t30, catastrophic_tail_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(cata_be_t45, catastrophic_tail_reading, base_extractiveness, 45, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophic_tail_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophic_tail_reading, renewable_energy_transition_cost).
narrative_ontology:affects_constraint(catastrophic_tail_reading, baseload_stranded_capital).
narrative_ontology:affects_constraint(catastrophic_tail_reading, climate_policy_mitigation_mandate).

% DUAL FORMULATION NOTE:
% The catastrophic-tail reading constrains energy policy but is itself constrained by upstream readings of acceptable risk. The three readings (catastrophic-tail, expected-value, precautionary) form a sibling constraint family linked through the shared kernel (acceptable_risk_for_energy). Separate constraint stories model each reading with distinct ε, beneficiary/victim structures, and perspectival classifications. This story instantiates only the catastrophic-tail reading; expected_value_reading and precautionary_reading are separate constraint_ids with their own base_properties and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
