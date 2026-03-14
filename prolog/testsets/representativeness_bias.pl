% ============================================================================
% CONSTRAINT STORY: representativeness_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_representativeness_bias, []).

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
 *   constraint_id: representativeness_bias
 *   human_readable: Representativeness Bias in Decision-Making and Resource Allocation
 *   domain: cognitive_psychology/institutional_decision_making
 *
 * SUMMARY:
 *   Representativeness bias — the tendency to classify agents, outcomes, or
 *   entities based on similarity to prototypes rather than base-rate
 *   frequencies — creates a structural constraint that operates
 *   simultaneously as a cognitive coordination mechanism, an extraction
 *   system, a degraded ritual, and a problem under solution. The bias reduces
 *   cognitive load (coordination benefit) while systematically misclassifying
 *   atypical category members (extraction cost). The constraint exhibits
 *   perspectival variation: institutions experience it as a rational
 *   heuristic; misclassified individuals experience it as a snare;
 *   category-typical members experience mixed coordination and extraction;
 *   organized debiasing efforts experience it as a temporary problem with a
 *   sunset; folk essentialist narratives perform explanation through high
 *   theater; and the uncritical analytical observer risks naturalizing a
 *   contingent cognitive habit as an immutable law of mind. The measurement
 *   trajectory shows extractiveness and theater ratio rising together over
 *   the observation interval, indicating that as the bias becomes
 *   institutionalized (hardcoded into algorithmic systems, formalized in
 *   policy, naturalized in narratives), both the raw extraction and the
 *   performative justification intensify. Debiasing interventions create a
 *   countervailing pressure, making this a contested constraint with multiple
 *   futures.
 *
 * KEY AGENTS:
 *   - Misclassified Individual: Primary victim (powerless/trapped) — atypical category members who bear extraction cost; cannot exit categorization or refute stereotype through individual data alone
 *   - Category-Typical Member: Secondary actor (moderate/constrained) — stereotyped category members who benefit from heuristic speed but are also constrained by stereotype ceiling; can exit through exceptional performance at high cost
 *   - Decision-Making Institution: Primary beneficiary (institutional/arbitrage) — banks, employers, judges, recommendation systems that reduce cognitive load and cost through category-based heuristics; can arbitrage when accuracy becomes more profitable
 *   - Statistical Literacy Movement: Organized agent (organized/constrained) — educators, regulators, algorithmic auditors building debiasing infrastructure; face institutional resistance but have sunset logic driving change
 *   - Essentialism Narrative: Institutional storyteller (institutional/arbitrage) — folk theories, marketing narratives, cultural essentialist frames that perform explanation for stereotype maintenance; maintain high theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks mistaking a contingent cognitive-institutional habit for a natural law of cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(representativeness_bias, 0.58).
domain_priors:suppression_score(representativeness_bias, 0.65).
domain_priors:theater_ratio(representativeness_bias, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(representativeness_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(representativeness_bias, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(representativeness_bias, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(representativeness_bias, tangled_rope).
narrative_ontology:human_readable(representativeness_bias, "Representativeness Bias in Decision-Making and Resource Allocation").
narrative_ontology:topic_domain(representativeness_bias, "cognitive_psychology/institutional_decision_making").

domain_priors:requires_active_enforcement(representativeness_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(representativeness_bias, pattern_matching_agents).
narrative_ontology:constraint_beneficiary(representativeness_bias, stereotyped_category_beneficiaries).
narrative_ontology:constraint_beneficiary(representativeness_bias, institutional_simplification_adopters).
narrative_ontology:constraint_victim(representativeness_bias, base_rate_accurate_individuals).
narrative_ontology:constraint_victim(representativeness_bias, atypical_category_members).
narrative_ontology:constraint_victim(representativeness_bias, system_accuracy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MISCLASSIFIED INDIVIDUAL (SNARE) — An atypical member of a stereotyped category (e.g., a woman in STEM, a poor person applying for credit, a dark-skinned person in facial recognition) bears the extraction cost of representativeness bias without exit options. Their individual attributes are subordinated to category stereotype. Cannot escape categorization; cannot refute stereotype through individual data alone. Maximum extraction.
constraint_indexing:constraint_classification(representativeness_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CATEGORY-TYPICAL MEMBER (TANGLED ROPE) — A member who does match the stereotype benefits from rapid, low-friction categorization (coordination benefit: faster service, lower verification burden) but is also constrained by the stereotype's ceiling. Genuine coordination (heuristic speeds decisions) but also extraction (stereotyped expectations limit opportunity). Can exit through exceptional performance but at high cost.
constraint_indexing:constraint_classification(representativeness_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DECISION-MAKING INSTITUTION (ROPE) — Banks, employers, judges, diagnostic systems use representativeness heuristic to reduce cognitive load and accelerate decisions. Genuine coordination function: categorization solves the problem of decision-making under bounded rationality. Benefits from the constraint (lower processing cost, faster throughput). Can arbitrage: switch to more accurate models when cost-benefit changes, but currently arbitrage favors heuristic over accuracy.
constraint_indexing:constraint_classification(representativeness_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATISTICAL LITERACY MOVEMENT (SCAFFOLD) — Organized efforts (base-rate training, debiasing interventions, algorithmic transparency mandates) see representativeness bias as a temporary coordination failure resolvable through education and institutional redesign. The sunset logic: as statistical literacy increases and decision systems incorporate base-rate corrections, the heuristic's grip weakens. Suppression (institutional resistance to complexity) declines as automation enables accurate categorization at lower cost.
constraint_indexing:constraint_classification(representativeness_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ESSENTIALISM NARRATIVE (PITON) — Cultural narratives and folk theories that treat stereotype-based categorization as reflecting objective category essences ('this is just how X people are') are largely performative. They persist through institutional inertia and identity fusion despite being empirically falsified. The performance maintains the heuristic: calling stereotypes 'essences' makes them feel inevitable, not contingent. Theater ratio is high because essentialism performs explanation that explains nothing.
constraint_indexing:constraint_classification(representativeness_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, bounded rationality is an immutable feature of cognitive architecture: agents with finite time and computation capacity must rely on heuristics; representativeness bias is inherent to how similarity judgments work. This perspective sees the bias as a natural law of cognition. However, the structural data contradicts the mountain classification — the engine will flag this as a false summit, revealing that naturalization of a contingent cognitive habit as an immutable law obscures the institutional arrangements that maintain it.
constraint_indexing:constraint_classification(representativeness_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(representativeness_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(representativeness_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(representativeness_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(representativeness_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(representativeness_bias, TR),
    TR >= 0.70.

:- end_tests(representativeness_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The bias creates genuine extraction from misclassified individuals while providing genuine coordination benefit to institutions. The net is extractive because the benefits (faster decisions, lower processing cost) flow to institutions while costs (misclassification, lost opportunity) flow to individuals. The initial value (0.42) reflects the bias operating with weaker institutional embedding; the later value (0.58) reflects the bias hardcoded into algorithms and policy. Suppression (0.65): Moderate-high. Significant barriers to exit and resistance to debiasing include: cognitive automaticity (the bias is effortless), institutional path dependence (systems are built around heuristics), financial incentives (accuracy costs money relative to heuristics), identity fusion (essentialism feels like truth to those socialized into it), and feedback loops (misclassified agents often lack data to refute stereotypes). Theater ratio (0.68): Moderate-high. Folk essentialism performs a high-theater function: narrative explanations that feel satisfying ('X people are just like that') do explanatory work that doesn't survive base-rate scrutiny. Institutional policies formalize the heuristic into procedures that perform neutrality and rigor while embedding categorization bias. Algorithmic systems inherit and amplify the theater: 'the algorithm is objective' covers the fact that representativeness bias has been encoded.
 *
 * PERSPECTIVAL GAP:
 *   The representativeness bias exhibits full DR perspectival divergence. An institution classifies it as Rope (coordination mechanism for bounded rationality). A misclassified individual classifies it as Snare (pure extraction with no exit). A category-typical member classifies it as Tangled Rope (mixed coordination and extraction). The debiasing movement classifies it as Scaffold (temporary problem under solution with sunset). The essentialism narrative classifies it as Piton (performative function, low real verification). The uncritical analytical observer classifies it as Mountain (natural law of cognition). These are not disagreements about metrics — they are differences in structural position producing different experienced extractiveness values. The engine's falsity detector identifies the mountain as a false summit: the appeal to bounded rationality naturalizes what is actually an institutional choice to privilege heuristics despite knowing they are less accurate. Debiasing is possible; the constraint is not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position + exit options. Misclassified individuals (powerless, trapped) occupy d ≈ 0.95, experiencing maximum extraction (f(d) ≈ 1.42). Institutions (institutional, arbitrage) occupy d ≈ 0.05, experiencing negative extraction (f(d) ≈ -0.12) — the constraint benefits them. The perspectival gap reveals that the constraint is extractive when observed from the target's position but coordinative when observed from the beneficiary's position. This is the diagnostic signature of a Tangled Rope from the neutral analytical view: genuine coordination function (heuristics do reduce bounded-rationality costs) coupled with asymmetric extraction (benefits accrue to institutions, costs to misclassified individuals).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for representativeness bias is resolved by indexing all six perspectives and showing how each one captures a genuine structural feature: (1) The Snare from the victim's view (pure extraction, no exit) is their lived experience. (2) The Rope from the institution's view (coordination mechanism) is the genuine cognitive function. (3) The Tangled Rope from the analytical view combines both: coordination function + asymmetric extraction. (4) The Scaffold from the debiasing coalition (temporary, sunset logic) reflects real institutional change trajectories. (5) The Piton from the essentialist narrative (performative, degraded) reflects how folk theory maintains the heuristic. (6) The Mountain from the uncritical observer (natural law) is a false summit — the constraint is institutionally contingent, not cognitively inevitable. The mandatrophy is not 'which type is correct?' but 'which perspectives do we activate in policy?' The analytical observer's responsibility is to specify all six and argue for institutional design that reduces the Snare experience (victim's view) without destroying the genuine Rope coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stereotype_accuracy_threshold,
    'At what accuracy level does a stereotype become rational (a reliable base rate) versus extractive (perpetuating false category membership)?',
    'Longitudinal base-rate tracking: measure actual category distributions vs. perceived distributions over time; correlation between stereotype accuracy and decision accuracy across domains',
    'If threshold is high (>85% accuracy required): most operational stereotypes are extractive (Snare classification confirmed). If threshold is low (>60% suffices): stereotypes can be coordination mechanisms (Rope from institutional perspective). Different thresholds imply different institutional responsibilities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stereotype_accuracy_threshold, empirical, 'Accuracy threshold distinguishing rational categorization from extractive stereotyping').

omega_variable(
    debiasing_sustainability,
    'Do debiasing interventions (base-rate training, algorithmic corrections, explicit diversity mandates) produce lasting reduction in representativeness bias or do they revert under cognitive load?',
    'Longitudinal measurement of bias persistence: compare immediate post-intervention accuracy with accuracy under time pressure, resource scarcity, or high-stakes decision contexts; neural imaging of stereotype activation under cognitive load',
    'If debiasing proves sustainable: scaffold perspective is valid (sunset logic works). If debiasing reverts under load: representativeness bias is closer to natural law (mountain perspective gains force). Different outcomes imply different policy trajectories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debiasing_sustainability, empirical, 'Whether debiasing interventions produce durable change or revert under pressure').

omega_variable(
    category_boundary_fuzziness,
    'Are categories perceived as having sharp boundaries (essentialist framing) or fuzzy membership (statistical framing)? Does perceptual framing affect extraction mechanics?',
    'Experimental measurement: category boundary tasks with fuzzy vs sharp exemplars; measurement of stereotype rigidity under fuzzy vs essentialist instructions; longitudinal tracking of category fluidity in populations with different statistical literacy',
    'If boundaries are perceived as sharp: categories support extractive essentialist narratives (Snare/Piton mechanisms strengthen). If fuzzy: categories support statistical revision (Scaffold sunset accelerates). Perceptual framing is partially learnable, suggesting institutional responsibility for framing choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_boundary_fuzziness, conceptual, 'Whether categories are perceived as sharp essences or fuzzy statistical distributions').

omega_variable(
    institutional_incentive_alignment,
    'Do institutions using representativeness bias maintain incentives to keep the bias salient (e.g., marketing, stereotyping as convenience) or do they have competing incentives to reduce it (e.g., accuracy for performance)?',
    'Institutional audit: measure cost savings from heuristic-based decisions vs accuracy losses from bias; track investment in debiasing infrastructure; measure feedback loops (do institutions receive accuracy data that would incentivize correction?)',
    'If institutions profit from bias: extraction will persist despite debiasing efforts (Snare is structural). If institutions have accuracy incentives: sunset logic is viable (Scaffold is achievable). Incentive alignment is partially controllable through regulation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_alignment, preference, 'Whether institutional incentives support or oppose representativeness bias reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(representativeness_bias, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repbias_tr_t0, representativeness_bias, theater_ratio, 0, 0.55).
narrative_ontology:measurement(repbias_tr_t3, representativeness_bias, theater_ratio, 3, 0.62).
narrative_ontology:measurement(repbias_tr_t6, representativeness_bias, theater_ratio, 6, 0.68).
narrative_ontology:measurement(repbias_tr_t9, representativeness_bias, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(repbias_be_t0, representativeness_bias, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(repbias_be_t3, representativeness_bias, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(repbias_be_t6, representativeness_bias, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(repbias_be_t9, representativeness_bias, base_extractiveness, 9, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(representativeness_bias, information_standard).
narrative_ontology:boltzmann_floor_override(representativeness_bias, 0.12).
narrative_ontology:affects_constraint(representativeness_bias, base_rate_neglect).
narrative_ontology:affects_constraint(representativeness_bias, stereotyping_in_criminal_justice).
narrative_ontology:affects_constraint(representativeness_bias, algorithmic_bias_amplification).

% DUAL FORMULATION NOTE:
% Representativeness bias is upstream of specific domain implementations (criminal justice risk assessment, hiring algorithms, medical diagnosis systems, credit scoring). Each domain has its own constraint story with domain-specific ε and beneficiary/victim declarations. This story represents the general cognitive mechanism; downstream stories represent institutional embeddings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(representativeness_bias, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
