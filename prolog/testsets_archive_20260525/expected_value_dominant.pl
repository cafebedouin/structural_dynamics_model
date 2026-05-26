% ============================================================================
% CONSTRAINT STORY: expected_value_dominant
% ============================================================================
% Version: 2.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expected_value_dominant, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: expected_value_dominant
 *   human_readable: Expected Value Dominant Risk Framework for Energy Pathways
 *   domain: energy_policy/risk_assessment/decision_theory
 *
 * SUMMARY:
 *   The expected value dominant framework for energy risk assessment operates
 *   by aggregating all harms into a single metric (mortality-per-TWh), in
 *   which expected value is the dominant decision principle: probability ×
 *   harm determines acceptable risk. This reading of the
 *   acceptable_risk_energy kernel produces a tangled rope constraint because
 *   it coordinates genuine risk comparison across energy pathways (legitimate
 *   coordination function) while simultaneously extracting through the choice
 *   of aggregation method itself. The framework weights coal mining deaths
 *   and air pollution deaths equally with probability-discounted nuclear
 *   accident risks and concentrates decision authority in institutions using
 *   expected value logic. Suppression is high (0.68) because the framework
 *   operates through several mechanisms: diffusion of causality (pollution
 *   deaths are distributed and chronic, making individual attribution
 *   difficult), methodological suppression (measurement ambiguity in
 *   attribution is hidden behind quantified metrics), and institutional
 *   suppression (alternative decision frameworks — tail-risk dominance,
 *   option value preservation, robust satisficing — are marginalized in
 *   policy discourse). Theater ratio has risen from 0.38 to 0.55 over the
 *   30-year interval as the methodology has become more sophisticated in
 *   appearance while its foundational assumptions have become more contested.
 *   The extractiveness has accumulated from 0.35 to 0.58, driven by
 *   accumulating evidence that expected value dominance systematically
 *   underweights low-probability high-consequence outcomes and by growing
 *   recognition that the framework locks in infrastructure pathways,
 *   suppressing future energy optionality.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Incumbents: Institutional beneficiary (institutional/arbitrage) — expected value framework legitimizes continued operation by including mining deaths in the same metric, effectively equalizing risk profiles across pathways
 *   - Pollution-Harmed Populations: Primary victim (powerless/trapped) — distributed chronic harm absorbed into aggregate statistics; cannot exit exposure or challenge attribution
 *   - Catastrophic Tail Risk Bearers (nuclear accident victims, climate cascade failure victims): Secondary victim (powerless/trapped) — tail risk discounted by probability in expected value calculus; low-probability high-consequence outcomes systematically underweighted
 *   - Future Generations: Tertiary victim (powerless/trapped at generational timescale) — option value of preserving multiple energy pathways suppressed by lock-in effects of infrastructure investment decisions made under expected value dominance
 *   - Renewable Energy and Nuclear Advocates: Moderate actors (moderate/constrained) — experience both suppression (barriers to deployment) and coordination benefit (expected value framework enables comparative risk communication)
 *   - Policy Institutions: Institutional actors (institutional/arbitrage) — enforce the constraint through regulatory acceptance and funding decisions; benefit from the simplicity and apparent objectivity of expected value logic
 *   - Risk Assessment Methodology Discipline: Institutional actor (institutional/arbitrage) — maintains the framework through peer review, textbooks, and professional standards despite growing contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expected_value_dominant, 0.58).
domain_priors:suppression_score(expected_value_dominant, 0.68).
domain_priors:theater_ratio(expected_value_dominant, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expected_value_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(expected_value_dominant, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(expected_value_dominant, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(expected_value_dominant, "Expected Value Dominant Risk Framework for Energy Pathways").
narrative_ontology:topic_domain(expected_value_dominant, "energy_policy/risk_assessment/decision_theory").

domain_priors:requires_active_enforcement(expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(expected_value_dominant, formalized).
narrative_ontology:cs_authority_grounding(expected_value_dominant, extraction).
narrative_ontology:cs_interpretation_layer_present(expected_value_dominant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expected_value_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(expected_value_dominant, risk_aggregation_methodology).
narrative_ontology:constraint_victim(expected_value_dominant, pollution_harmed_populations).
narrative_ontology:constraint_victim(expected_value_dominant, catastrophic_tail_risk_bearers).
narrative_ontology:constraint_victim(expected_value_dominant, future_option_value_preservation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLLUTION-HARMED POPULATIONS (SNARE) — Trapped in regions with endemic air pollution from fossil fuel combustion. Expected value framework fully weights their harm into aggregate mortality statistics, but suppression operates through diffusion of causality (individual vs. systemic attribution), political powerlessness to demand fuel switching, and resource scarcity that makes alternatives unaffordable. These populations cannot exit the constraint and experience it as pure extraction — their mortality is rationalized as acceptable risk rather than challenged.
constraint_indexing:constraint_classification(expected_value_dominant, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RENEWABLE ENERGY ADVOCATES (TANGLED ROPE) — Constrained by capital costs, grid integration barriers, and political opposition but also benefit from the expected value framework's transparency (it validates their claims that fossil deaths are underestimated in informal risk assessment). They experience both extraction (suppressed by structural barriers to implementation) and coordination (the expected value metric enables comparative risk communication). Moderate power with constrained exit — advocacy is possible but costly.
constraint_indexing:constraint_classification(expected_value_dominant, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FOSSIL FUEL INCUMBENTS (ROPE) — Primary beneficiaries. The expected value framework enables them to participate in risk quantification debates and claim scientific legitimacy for continued operation. Expected value dominance explicitly includes their operational risk (coal mining deaths, oil rig accidents) in the same metric as pollution externalities, which partially equalizes the risk profiles. However, the benefit flows primarily to incumbents through regulatory acceptance and continued operation, while costs are borne by distributed populations. Institutional actors with arbitrage options (portfolio diversification, geographic shift) experience the constraint as coordination: they can negotiate risk thresholds within the expected value framework.
constraint_indexing:constraint_classification(expected_value_dominant, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CLIMATE AND NUCLEAR POLICY COALITIONS (SCAFFOLD) — Organized agents (climate modeling institutes, nuclear safety authorities, international energy bodies) see the expected value framework as a temporary coordination mechanism that will sunset as catastrophic tail risk becomes empirically salient or option value preservation logic displaces expected value dominance in policy discourse. The framework functions as transitional infrastructure: it enables quantitative risk comparison across pathways but is structurally brittle against evidence of black swan events or lock-in effects.
constraint_indexing:constraint_classification(expected_value_dominant, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RISK ASSESSMENT METHODOLOGICAL ESTABLISHMENT (PITON) — The expected value framework persists as the default methodology in regulatory bodies, academic risk journals, and policy institutions through institutional inertia and the difficulty of establishing alternative frameworks rather than through its explanatory power. Theater is high (0.55) because the framework performs legitimacy (quantification, peer review, scientific apparatus) while its core assumptions (expected value suffices for tail-risk comparison, mortality metrics are commensurate across contexts) remain contested. The piton classification reflects that the methodology is maintained not because it solves the underlying problem but because no unified alternative has yet become institutionalized.
constraint_indexing:constraint_classification(expected_value_dominant, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MATHEMATICAL INEVITABILITY VIEW (MOUNTAIN) — From a civilizational perspective, expected value dominance appears as a mathematically necessary principle: rational risk comparison MUST aggregate harms into a single metric, and expected value (probability × harm) is the canonical aggregation function. This perspective sees the constraint as emergent from the logic of rational decision-making itself, not as a contingent institutional arrangement. However, the structural data contradicts this — identifiable beneficiaries (fossil fuel incumbents), active enforcement mechanisms (regulatory acceptance), and suppression all suggest this is a contingent institutional reading, not a natural law. The engine's false summit detector will identify this as a false mountain.
constraint_indexing:constraint_classification(expected_value_dominant, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expected_value_dominant_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expected_value_dominant, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expected_value_dominant, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(expected_value_dominant, TR),
    TR >= 0.70.

:- end_tests(expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The framework coordinates genuine risk comparison across energy pathways (legitimate coordination reduces to perhaps 0.25 base extraction), but the choice of expected value aggregation method itself produces additional extraction through: (1) suppression of alternative decision frameworks (tail-risk dominant reading, option-value preserving reading), (2) discounting of low-probability high-consequence outcomes, (3) lock-in effects that suppress future optionality. The 0.58 value reflects both the coordination benefit and the aggregation-method extraction, measured at the analytical observer level. Suppression (0.68): High. Multiple suppression mechanisms operate: diffusion of causality for pollution deaths (chronic, distributed, multi-causal), epistemically uncertain probability discounting for tail events (Knightian uncertainty), measurement ambiguity hidden behind quantified metrics, and institutional suppression of alternative frameworks in policy discourse. The suppression is not total — expected value logic is transparent and debatable — but significant barriers prevent adoption of alternatives. Theater ratio (0.55): Moderate-high. The methodology performs high legitimacy through quantification, peer review, and scientific apparatus, but foundational assumptions (commensurability of distributed vs concentrated harm, probability discount rates, option value suppression) are increasingly contested. The theater has accumulated as measurement sophistication has increased while debates about assumptions have become more sophisticated without changing institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   The expected value framework generates perspectival gaps across all six classification types. Fossil fuel incumbents see the framework as coordination (Rope) — a scientific method enabling rational debate. Pollution-harmed populations see it as extraction (Snare) — their chronic harm is rationalized as statistically acceptable. Tail-risk-conscious policy coalitions see it as temporary scaffolding (Scaffold) — the framework enables current decisions but will be superseded by more sophisticated approaches. The risk methodology establishment sees it as performing legitimacy without functional updating (Piton) — the framework persists through institutional inertia despite growing contestation. Moderate renewable advocates experience both benefits (comparative risk communication) and costs (infrastructure lock-in), seeing Tangled Rope. The analytical observer risks seeing this as a natural law of rationality (Mountain) — but this is a false summit. The expected value framework is a contingent institutional reading, not a mathematical necessity. Alternative rational decision frameworks (robust satisficing, minimax, option-value preservation, threshold rules) are equally defensible under different axiom sets.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by structural position relative to the constraint. Fossil fuel incumbents (institutional, arbitrage): d ≈ 0.15 (beneficiary with exit options — low experienced extraction). Pollution-harmed populations (powerless, trapped): d ≈ 0.92 (full target with no exit — maximum experienced extraction). Catastrophic tail risk bearers (powerless, trapped): d ≈ 0.88 (target, but risk is probabilistic rather than immediate — high experienced extraction). Future generations (powerless, trapped at generational scale): d ≈ 0.85 (target through lock-in, but extraction mechanism is temporal rather than spatial — high experienced extraction). Renewable advocates (moderate, constrained): d ≈ 0.58 (split between benefiting from comparative risk framework and suffering from infrastructure lock-in). Policy institutions (institutional, arbitrage): d ≈ 0.12 (beneficiary through regulatory authority and simplicity benefit — low experienced extraction). The chi formula scales these d values by the sigmoid f(d) and scope modifier σ(S). The perspectival gaps reflect these d-value differences: beneficiaries (low d) see Rope or Scaffold; victims (high d) see Snare; mixed actors see Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through explicit kernel-reading framing. This constraint instantiates ONE reading (expected_value_dominant) of the contested kernel (acceptable_risk_energy). Other readings would produce different constraints with different ε values and different beneficiary/victim structures. Catastrophic_tail_dominant reading: ε ≈ 0.72, tail risk dominates decision, victims shift to emphasize catastrophic event bearers. Option_value_preserving reading: ε ≈ 0.38, future optionality becomes explicit goal, lock-in effects become primary suppression mechanism. The mandatrophy is not 'which reading is correct?' but 'which reading is instantiated by each constraint story?' By decomposing the kernel into multiple constraint stories, each with its own ε, beneficiary/victim structure, and perspectives, the apparent contradiction between 'expected value is rational' and 'expected value suppresses tail risk' is resolved: both are true within their respective reading frames. The framework itself (the kernel) is stable; the readings are contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'Which reading of the acceptable_risk_energy kernel is instantiated by this constraint?',
    'Explicit declaration: this constraint instantiates the expected_value_dominant reading, where aggregate expected value (probability × harm) is the dominant decision principle. Sibling readings (catastrophic_tail_dominant: tail risk dominates; option_value_preserving: preserving low-probability high-value futures dominates) yield different victim sets, suppression mechanisms, and type classifications.',
    'Each reading produces a different constraint story with different ε, different beneficiary/victim structure, and different perspectives. This file is ONE reading only. Expected value dominant: ε=0.58, fossil deaths weighted equally with all harm categories. Catastrophic tail dominant: ε would be higher (0.72+), nuclear tail risk dominates decision; victim set changes. Option value dominant: ε would be lower (0.38), preserving future energy pathways (nuclear fusion, renewable breakthroughs) becomes explicit goal; suppression mechanisms differ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Kernel reading identity: expected_value_dominant among {expected_value_dominant, catastrophic_tail_dominant, option_value_preserving}').

omega_variable(
    commensurability_assumption,
    'Are deaths from air pollution (distributed, chronic, probabilistic at individual level) genuinely commensurate with deaths from low-probability catastrophic events (concentrated, acute, catastrophic at community level)?',
    'Empirical: comparison of how affected populations and policy-makers respond to equivalent mortality risk presented as distributed vs concentrated; psychological: evidence on whether expected value frames accurately predict risk perception and acceptance across contexts; normative: philosophical debate on whether aggregation across risk architectures is legitimate.',
    'If NOT commensurate: the expected value framework improperly naturalizes a specific risk architecture (distributed harm = acceptable; concentrated harm = catastrophic). Victims shift — those bearing distributed harm become more salient as victims; catastrophic tail risk bearers become less salient. Suppression mechanism changes from ''diffusion of causality'' to ''category error in metric design.'' Type shifts from Tangled Rope (genuine coordination via quantification) toward Snare (extraction via inappropriate aggregation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commensurability_assumption, conceptual, 'Commensurability of distributed vs. catastrophic mortality in expected value aggregation').

omega_variable(
    probability_discounting_legitimacy,
    'Is it legitimate to discount low-probability catastrophic outcomes in energy policy decisions when the discount rate (probability) itself is epistemically uncertain?',
    'Historical case study: compare forecasted vs. observed probabilities for Fukushima, Chernobyl, climate-induced energy infrastructure failures. Empirical: quantify epistemic uncertainty bounds on rare-event probabilities. Methodological: debate on whether Knightian uncertainty (unknown unknowns) should be handled differently from reducible uncertainty.',
    'If probability estimates systematically underestimate rare events: expected value framework systematically underweights catastrophic tail risks, producing a type shift toward Snare (the framework extracts by suppressing tail risk). If epistemic uncertainty is handled explicitly: expected value framework loses simplicity and loses institutional adoption — shifts toward Scaffold (temporary while more robust frameworks develop).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_discounting_legitimacy, empirical, 'Whether probability discounting in expected value is epistemically justified for rare tail events').

omega_variable(
    lock_in_and_path_dependency,
    'Does expected value dominance suppress the option value of preserving future energy pathways (nuclear fusion, advanced renewables, carbon sequestration) by locking in current fossil or current fission infrastructure?',
    'Path dependency analysis: document capital lock-in for fossil vs. nuclear vs. renewable infrastructure; real options valuation: quantify the cost of closing off future technology adoption; counterfactual: model scenarios where option value preservation is the dominant decision principle.',
    'If lock-in is severe: expected value framework suppresses future flexibility and compounds extraction over generational horizons. Victims shift to include future populations with fewer energy pathway options. Type shifts toward Snare at generational/civilizational timescales (short-term rationality generates long-term extraction). If lock-in is limited: option value concerns are secondary and the expected value framework''s legitimacy is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lock_in_and_path_dependency, empirical, 'Path dependency and option value suppression in expected value energy decisions').

omega_variable(
    measurement_and_attribution_ambiguity,
    'Can mortality attribution (coal deaths, pollution deaths, nuclear accident deaths) be measured precisely enough to support expected value comparison, or does the measurement ambiguity itself hide extraction?',
    'Meta-analysis: compare mortality estimates across independent studies for same fuel source. Causality analysis: how much of coal pollution mortality is direct causation vs. contributory factor vs. correlation? Counterfactual: what would have died from without the fuel pathway? Geographic variability: how much does attribution vary by region, climate, industrial density?',
    'If measurement variance is high: the expected value framework suppresses this ambiguity through quantified metrics, hiding the fact that ''exact'' mortality comparisons rest on contested methodological choices. Suppression mechanism changes from ''diffusion of causality'' to ''false precision.'' Beneficiary/victim structure changes — measurement methodology becomes an active enforcer of the extraction, producing a more sophisticated Tangled Rope or possible Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_and_attribution_ambiguity, empirical, 'Measurement precision and attribution ambiguity in mortality-per-TWh metrics').

omega_variable(
    false_summit_natural_law_risk,
    'Does the expected value framework risk naturalizing a contingent institutional reading (dominant decision rule among policy institutions) as a natural law of rational decision-making?',
    'Philosophical: show that non-expected-value decision principles (robust satisficing, minimax, option value, threshold rules) are equally rational under different axiom sets. Historical: document the contingent institutional processes (Von Neumann-Morgenstern axioms, post-WWII American policy rationality, Cold War nuclear strategic thinking) that elevated expected value to dominance. Anthropological: document cultures or institutions that use different risk aggregation principles.',
    'If false summit confirmed: the mountain perspective is a false summit (naturalization of contingent institutional choice). The engine''s FSM detector will identify this. Suppression mechanism includes the suppression of alternative decision frameworks in policy discourse, not just suppression of harm data. The constraint becomes more sophisticated — it extracts not just by weighing harms differently but by making one weighting scheme appear inevitable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Risk of naturalizing expected value dominance as mathematical necessity rather than institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expected_value_dominant, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(expe_tr_t0, expected_value_dominant, theater_ratio, 0, 0.38).
narrative_ontology:measurement(expe_tr_t15, expected_value_dominant, theater_ratio, 15, 0.48).
narrative_ontology:measurement(expe_tr_t30, expected_value_dominant, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(expe_be_t0, expected_value_dominant, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(expe_be_t15, expected_value_dominant, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(expe_be_t30, expected_value_dominant, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(expected_value_dominant, catastrophic_tail_dominant).
narrative_ontology:affects_constraint(expected_value_dominant, option_value_preserving).
narrative_ontology:affects_constraint(expected_value_dominant, fossil_fuel_lock_in).
narrative_ontology:affects_constraint(expected_value_dominant, nuclear_regulatory_capture).

% DUAL FORMULATION NOTE:
% The acceptable_risk_energy kernel decomposes into three structurally distinct constraints corresponding to different decision-dominant readings: expected_value_dominant (this file), catastrophic_tail_dominant, and option_value_preserving. Each reading produces different ε values, different beneficiary/victim structures, and different perspectives. This decomposition follows the ε-invariance principle — if the observable (decision-dominant principle) changes, ε changes, so we have multiple constraints not one constraint with a hidden parameter. All three stories link via network.affects_constraints to show the family structure. Each story's commentary.kernel_context records the kernel identity and sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
