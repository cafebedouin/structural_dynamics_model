% ============================================================================
% CONSTRAINT STORY: epistemic_dataset_construction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_dataset_construction, []).

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
 *   constraint_id: epistemic_dataset_construction
 *   human_readable: Epistemic Dataset Construction for Trust Assessment
 *   domain: social_philosophy/trust_theory/relational_ethics
 *
 * SUMMARY:
 *   The epistemic standard that trust should be warranted by behavioral
 *   distribution across varied conditions over time, rather than faith or
 *   single-event heroism, creates a structural tension between epistemic
 *   reliability and temporal accessibility. This constraint exhibits the core
 *   tangled_rope pattern: it solves a genuine coordination problem (reducing
 *   exploitation by trust-violators) while simultaneously creating asymmetric
 *   extraction (advantaging agents with temporal resources to conduct
 *   extended observation). The constraint is downstream of
 *   structural_thinning_convergence (the mathematical limit on how much
 *   behavioral data can be compressed into shorter observation windows) but
 *   represents a distinct social-institutional arrangement. Where
 *   structural_thinning_convergence is a mountain (information-theoretic
 *   limit), epistemic_dataset_construction is a tangled_rope (institutional
 *   norm that could be structured differently). The theater_ratio (0.38)
 *   reflects growing performative elements: agents learn to perform
 *   consistency during observation periods while maintaining different
 *   behavior outside observation, and reputation systems add verification
 *   theater without necessarily reducing gaming. The extractiveness has
 *   increased over the interval (0.38 → 0.48) as economic precarity has
 *   reduced the proportion of agents with sufficient temporal slack to build
 *   trust datasets, concentrating trust-network access among the
 *   already-embedded.
 *
 * KEY AGENTS:
 *   - Agents with Temporal Resources: Primary beneficiary (institutional/arbitrage) — can afford extended observation periods; accumulate trust networks that compound over time
 *   - Agents without Observation Time: Primary victim (powerless/trapped) — economic precarity forces high-stakes trust decisions on insufficient data; excluded from trust networks
 *   - Newcomers to Social Context: Secondary victim (moderate/constrained) — lack shared history but can eventually build datasets at significant cost; experience both coordination and extraction
 *   - Established Relationship Networks: Primary beneficiary (institutional/arbitrage) — accumulated datasets across multiple agents reduce marginal cost of new trust assessments
 *   - Institutional Gatekeepers: Mixed position (powerful/mobile) — use epistemic standard for both legitimate screening and extractive exclusion
 *   - Reputation System Designers: Organized agents (organized/mobile) — building technological substitutes for direct observation with scaffold logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine epistemic function and structural extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_dataset_construction, 0.48).
domain_priors:suppression_score(epistemic_dataset_construction, 0.52).
domain_priors:theater_ratio(epistemic_dataset_construction, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_dataset_construction, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_dataset_construction, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(epistemic_dataset_construction, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(epistemic_dataset_construction, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(epistemic_dataset_construction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_dataset_construction, tangled_rope).
narrative_ontology:human_readable(epistemic_dataset_construction, "Epistemic Dataset Construction for Trust Assessment").
narrative_ontology:topic_domain(epistemic_dataset_construction, "social_philosophy/trust_theory/relational_ethics").

domain_priors:requires_active_enforcement(epistemic_dataset_construction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_dataset_construction, agents_with_temporal_resources).
narrative_ontology:constraint_beneficiary(epistemic_dataset_construction, established_relationship_networks).
narrative_ontology:constraint_beneficiary(epistemic_dataset_construction, institutional_gatekeepers).
narrative_ontology:constraint_victim(epistemic_dataset_construction, agents_without_observation_time).
narrative_ontology:constraint_victim(epistemic_dataset_construction, newcomers_to_social_context).
narrative_ontology:constraint_victim(epistemic_dataset_construction, economically_precarious_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY PRECARIOUS AGENT (SNARE) — Cannot afford the temporal investment required to build trust datasets. Must make high-stakes trust decisions on insufficient data due to time poverty. Faces maximum extraction: the epistemic standard punishes those without observation time while claiming to be a neutral reliability metric.
constraint_indexing:constraint_classification(epistemic_dataset_construction, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: NEWCOMER TO SOCIAL CONTEXT (TANGLED ROPE) — Constrained by lack of shared history with local agents but benefits from the epistemic standard when it prevents exploitation by bad actors. Experiences both coordination (the standard protects against trust-exploiters) and extraction (the standard excludes those without local temporal embeddedness). Can eventually build datasets but at significant cost.
constraint_indexing:constraint_classification(epistemic_dataset_construction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED RELATIONSHIP NETWORK (ROPE) — Benefits from accumulated trust datasets across multiple agents and contexts. The epistemic standard coordinates reliable relationship formation while imposing minimal cost on those already embedded in stable networks. Experiences the constraint as pure coordination: behavioral consistency over time is both observable and rewarded.
constraint_indexing:constraint_classification(epistemic_dataset_construction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REPUTATION SYSTEM DESIGNERS (SCAFFOLD) — Organized agents building technological and institutional mechanisms to compress observation time (reputation systems, credential verification, reference networks, blockchain trust protocols). See the temporal barrier as a coordination problem with a sunset: distributed verification systems can aggregate behavioral data across contexts, reducing individual observation requirements. Estimated sunset: 15-25 years as reputation infrastructure matures.
constraint_indexing:constraint_classification(epistemic_dataset_construction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL GATEKEEPER (TANGLED ROPE) — Has resources to conduct extended observation but also uses the epistemic standard to justify exclusion of outsiders. Benefits from coordination (reliable trust assessment) while extracting rents (temporal barriers create artificial scarcity of trusted relationships, increasing gatekeeper power). Mixed experience: genuine epistemic function entangled with structural advantage.
constraint_indexing:constraint_classification(epistemic_dataset_construction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine epistemic value of behavioral consistency across varied conditions (coordination function: trust warranted by evidence reduces exploitation) and the structural extraction (temporal resource requirements create barriers that concentrate trust-network access among the already-embedded). The constraint solves a real coordination problem while simultaneously advantaging those with observation time and disadvantaging those without.
constraint_indexing:constraint_classification(epistemic_dataset_construction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_dataset_construction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_dataset_construction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_dataset_construction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_dataset_construction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_dataset_construction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The temporal resource requirement creates significant barriers. Agents with stable employment, established networks, and economic security can afford multi-year observation periods and varied-condition testing. Agents in economic precarity, frequent geographic mobility, or social marginalization cannot. The extraction is not total (some agents can meet the standard) but substantial (many cannot, and the barrier is rising). The value reflects that while the epistemic function is genuine, the cost structure concentrates benefits among the already-advantaged. Suppression (0.52): Moderate-high. Alternatives to extended temporal observation are suppressed by both epistemic arguments (compressed signals are unreliable) and institutional arrangements (reputation systems are immature, reference networks favor insiders, credential verification is gameable). The suppression is not absolute (some agents find workarounds through mutual aid networks or high-trust subcultures) but significant. Theater ratio (0.38): Moderate. Some performative elements exist (agents learn to perform consistency during observation windows, reputation systems add verification rituals) but the core epistemic function remains: behavioral consistency across varied conditions does predict future reliability better than single-event assessment. The theater is rising as gaming strategies become more sophisticated.
 *
 * PERSPECTIVAL GAP:
 *   The economically precarious agent sees pure extraction (Snare) — the epistemic standard punishes time poverty while claiming neutrality. The established relationship network sees pure coordination (Rope) — behavioral consistency over time is both observable and rewarded, with minimal cost to those already embedded. The newcomer and gatekeeper see mixed coordination-extraction (Tangled Rope) — the standard both protects against exploitation and excludes outsiders. The reputation system designers see a temporary problem with a sunset (Scaffold) — technological infrastructure can compress observation time. The analytical observer sees the full structural ambiguity (Tangled Rope) — genuine epistemic warrant entangled with resource-based exclusion. The gap reveals that 'trust warranted by evidence' is simultaneously a legitimate epistemic standard and a mechanism that concentrates social capital among those with temporal resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (agents_with_temporal_resources, established_relationship_networks, institutional_gatekeepers) experience low directionality — the constraint runs toward them, not away from them. They capture the coordination benefits (reliable trust assessment) while bearing minimal costs (they already have observation time and embedded networks). Victims (agents_without_observation_time, newcomers_to_social_context, economically_precarious_agents) experience high directionality — the constraint extracts from them by imposing temporal barriers they cannot meet. The newcomer and gatekeeper perspectives show the tangled_rope pattern: both coordination and extraction are present, with the balance depending on the agent's structural position. The analytical observer recognizes both functions without experiencing either as dominant.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the same epistemic standard can be both coordination and extraction depending on the agent's temporal resources. The coordination function is genuine: trust warranted by behavioral consistency across varied conditions does reduce exploitation compared to faith-based or single-event assessment. The extraction is also genuine: the temporal requirement creates barriers that systematically disadvantage agents without observation time. The tangled_rope classification captures this irreducible duality. The constraint is NOT a snare pretending to be coordination (the epistemic function is real) and NOT a rope with incidental costs (the extraction is structural, not accidental). It is precisely what tangled_rope describes: a mechanism that solves a coordination problem while embedding asymmetric extraction in the solution itself. The perspectival gap between the precarious agent (snare) and the established network (rope) is not a measurement error — it is the structural reality of how temporal resources mediate access to epistemic warrant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observation_time_threshold,
    'What minimum observation period across what variety of conditions constitutes sufficient epistemic warrant for trust, and is this threshold uniform across relationship types?',
    'Empirical analysis of trust-violation rates correlated with observation period length and condition variety; comparison across professional, personal, and civic relationship domains',
    'If threshold is low (months) and uniform: extraction is minimal, most agents can meet it. If threshold is high (years) and context-dependent: extraction is severe, creating permanent epistemic underclass.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observation_time_threshold, empirical, 'Minimum observation period and condition variety for warranted trust').

omega_variable(
    powerless_treatment_signal_validity,
    'Is treatment of powerless people actually a compressed signal of character, or does this heuristic systematically misclassify agents who perform deference strategically?',
    'Longitudinal tracking of agents'' behavior toward powerless vs powerful others; correlation with subsequent trust violations; analysis of strategic performance vs stable traits',
    'If valid: the heuristic reduces observation time requirements (coordination). If invalid or gameable: the heuristic creates false confidence while maintaining temporal barriers (extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(powerless_treatment_signal_validity, empirical, 'Validity of powerless-treatment as character signal').

omega_variable(
    reputation_system_substitutability,
    'Can technological reputation systems (distributed ledgers, reference networks, credential verification) actually substitute for direct temporal observation, or do they introduce new gaming vectors that require even longer observation to detect?',
    'Comparison of trust-violation rates in relationships mediated by reputation systems vs direct observation; analysis of reputation-gaming detection timelines; measurement of false-positive/false-negative rates',
    'If substitutable: scaffold perspective confirmed, sunset is real. If not substitutable or introduces new gaming: reputation systems add theater without reducing temporal barriers, increasing total extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reputation_system_substitutability, empirical, 'Whether reputation systems can substitute for direct observation').

omega_variable(
    stress_threshold_universality,
    'Are the stress thresholds that reveal stable behavioral traits universal across cultures and contexts, or are they observer-relative constructs that systematically favor certain social positions?',
    'Cross-cultural analysis of what counts as ''stress'' and ''varied conditions''; examination of whether epistemic standards privilege stressors familiar to dominant groups; analysis of trait-stability measurement across cultural contexts',
    'If universal: the epistemic standard is genuinely coordination. If observer-relative: the standard naturalizes dominant-group norms as universal reliability metrics, increasing extraction on cultural outsiders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stress_threshold_universality, conceptual, 'Universality vs observer-relativity of stress thresholds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_dataset_construction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edc_tr_t0, epistemic_dataset_construction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(edc_tr_t5, epistemic_dataset_construction, theater_ratio, 5, 0.32).
narrative_ontology:measurement(edc_tr_t10, epistemic_dataset_construction, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(edc_be_t0, epistemic_dataset_construction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(edc_be_t5, epistemic_dataset_construction, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(edc_be_t10, epistemic_dataset_construction, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_dataset_construction, identity_coordination).

% DUAL FORMULATION NOTE:
% Epistemic_dataset_construction is downstream of structural_thinning_convergence (the information-theoretic limit on behavioral data compression) but represents a distinct constraint. Structural_thinning_convergence is a mountain (mathematical limit on how much observation time can be compressed). Epistemic_dataset_construction is a tangled_rope (social-institutional norm about what counts as sufficient warrant, which could be structured differently even given the mathematical limit). The upstream mountain sets a floor on observation requirements; the downstream tangled_rope determines how those requirements are institutionalized and who bears the cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
