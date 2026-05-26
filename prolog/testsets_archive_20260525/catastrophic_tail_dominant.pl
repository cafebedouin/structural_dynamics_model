% ============================================================================
% CONSTRAINT STORY: catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophic_tail_dominant, []).

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
 *   constraint_id: catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail Risk Dominance in Energy Policy Decision-Making
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   Energy policy increasingly prioritizes avoiding low-probability
 *   catastrophic outcomes (nuclear accidents, supply chain collapse, grid
 *   failure) even at the cost of accepting higher expected aggregate harm
 *   (continued fossil fuel deaths, air pollution, climate impacts). This
 *   constraint represents ONE specific reading of how acceptable risk is
 *   framed in energy decision-making — the catastrophic-tail-dominant
 *   reading, where tail risks receive disproportionate weight in policy
 *   justification. The constraint operates as a hybrid
 *   coordination-extraction mechanism: nuclear advocates frame catastrophic
 *   risk aversion as a coordination mechanism that serves collective
 *   low-carbon transition, but this framing systematically suppresses the
 *   voices of distributed-harm populations (fossil fuel workers,
 *   air-pollution victims, climate-stressed communities) whose interests
 *   conflict with the tail-dominance prioritization. The fossil
 *   fuel-dependent communities that bear transition costs are structurally
 *   trapped — they face concentrated, immediate unemployment and community
 *   collapse, while the catastrophe they are protecting against is diffuse
 *   and future-oriented. The constraint's theater ratio (0.61) reflects that
 *   formal risk assessment apparatus (expected value calculations,
 *   probabilistic analysis, cost-benefit frameworks) performs legitimation
 *   work more than analytical work: the frameworks are deployed to justify
 *   choices made on institutional and political grounds, not to resolve
 *   genuine value conflicts.
 *
 * KEY AGENTS:
 *   - Nuclear Energy and Climate Advocates (Institutional): Primary beneficiaries — secure funding, regulatory support, and institutional legitimacy through catastrophic tail aversion framing
 *   - Fossil Fuel Dependent Communities (Powerless/Trapped): Primary victims — concentrated, immediate transition costs; bear the implementation burden of catastrophe avoidance
 *   - Distributed Harm Populations (Powerless/Trapped): Primary victims — diffuse deaths from air pollution, climate impacts, occupational exposure; systematically underweighted in formal risk frameworks
 *   - Energy Consumers and Grid Operators (Moderate/Constrained): Mixed role — benefit from coordination (energy supply) and suffer from policy constraints (infrastructure lock-in, transition costs)
 *   - Renewable Energy Coalition (Organized/Constrained): Secondary beneficiary — catastrophic tail aversion accelerates decarbonization but also validates nuclear deployment that competes with renewable expansion
 *   - Risk Assessment Apparatus (Institutional/Mobile): Institutional actor that maintains performative frameworks — formal procedures legitimize politically-determined choices while obscuring value conflicts
 *   - Analytical Observer (Analytical): Risks naturalizing a contingent institutional choice (tail weighting) as a law of rational choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophic_tail_dominant, 0.58).
domain_priors:suppression_score(catastrophic_tail_dominant, 0.68).
domain_priors:theater_ratio(catastrophic_tail_dominant, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophic_tail_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophic_tail_dominant, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(catastrophic_tail_dominant, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(catastrophic_tail_dominant, "Catastrophic Tail Risk Dominance in Energy Policy Decision-Making").
narrative_ontology:topic_domain(catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(catastrophic_tail_dominant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophic_tail_dominant, nuclear_energy_advocates).
narrative_ontology:constraint_beneficiary(catastrophic_tail_dominant, low_carbon_institutional_actors).
narrative_ontology:constraint_victim(catastrophic_tail_dominant, fossil_fuel_dependent_communities).
narrative_ontology:constraint_victim(catastrophic_tail_dominant, working_class_energy_consumers).
narrative_ontology:constraint_victim(catastrophic_tail_dominant, distributed_harm_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(catastrophic_tail_dominant, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(catastrophic_tail_dominant, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

constraint_indexing:constraint_classification(catastrophic_tail_dominant, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(catastrophic_tail_dominant, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(catastrophic_tail_dominant, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(catastrophic_tail_dominant, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

constraint_indexing:constraint_classification(catastrophic_tail_dominant, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophic_tail_dominant_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophic_tail_dominant, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophic_tail_dominant, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophic_tail_dominant, TR),
    TR >= 0.70.

:- end_tests(catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The catastrophic tail-dominant framing produces real redistribution: institutional actors (nuclear advocates, climate establishment) capture policy legitimacy and funding, while distributed-harm populations and fossil fuel communities bear implementation and transition costs. The extraction is not total (0.70+) because some coordination benefit exists — tail aversion does reduce catastrophic risk, and institutional actors genuinely experience lower-probability downside. However, the extraction is substantial because the weighting asymmetry is constructed, not natural: distributed harms are systematically discounted not because they are truly less important but because they are diffuse and politically weak. Suppression (0.68): High. Multiple mechanisms suppress alternatives: (1) Mathematical suppression — infinite or near-infinite utility weights on catastrophes make formal optimization intractable, forcing rigid decision heuristics that close off debate; (2) Epistemic suppression — uncertainty quantification is asymmetric (catastrophic risks quantified with high confidence, distributed harms treated as uncertain); (3) Political suppression — distributed-harm populations lack institutional voice; fossil fuel communities face coordination barriers to collective resistance; (4) Institutional suppression — risk assessment apparatus is gatekept by experts aligned with tail-dominance framing. Theater ratio (0.61): Moderate-high. The formal risk assessment apparatus performs significant legitimation work. Formal procedures (probabilistic risk assessment, cost-benefit analysis, expert elicitation) create appearance of objective analysis while actually encoding contingent value choices (how to weight tail risks, how to discount distributed harms, what probability thresholds matter). The apparatus is theater because it obscures rather than resolves the underlying conflict between tail aversion and aggregate harm reduction.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between how institutional beneficiaries experience the constraint (coordination) and how trapped victims experience it (extraction). The nuclear advocate sees tail aversion as solving a collective problem and views themselves as part of a solution coalition. The coal miner sees tail aversion as the policy justification for shutting down their industry without compensation or support. These are not different interpretations of the same constraint — they are genuinely different structural experiences. The magnitude of the perspective gap correlates with the asymmetry in exit options (arbitrage vs trapped) and power levels (institutional vs powerless). The piton perspective (risk assessment apparatus) occupies a distinct position: it performs legitimate the tail-dominance framing while being somewhat detached from its consequences. The apparatus maintains high theater precisely to manage the perspectival gap — formal procedures create appearance of consensus where genuine value conflict exists.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness (0.58), the agent's directionality value (d derived from power, exit options, and beneficiary/victim status), and scope effects. Institutional beneficiary with arbitrage exit: d ≈ 0.15, f(d) ≈ -0.01, χ ≈ -0.006 (net subsidy — constraint benefits this agent). Powerless victim with trapped exit: d ≈ 0.90, f(d) ≈ 1.28, χ ≈ 0.74 (high extraction — maximum experienced harm). Moderate victim with constrained exit: d ≈ 0.65, f(d) ≈ 1.00, χ ≈ 0.58 (moderate extraction — experiences the constraint as constraining but not inescapable). Organized actor with constrained exit: d ≈ 0.45, f(d) ≈ 0.50, χ ≈ 0.29 (low extraction — has agency and sees alternatives). The spread in χ values across perspectives (from -0.006 to 0.74) reveals the constraint's asymmetry: it creates extreme extraction for powerless agents, no extraction for institutional beneficiaries, and variable extraction for moderate and organized agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by differentiating genuine coordination (nuclear advocates do solve a low-carbon transition problem) from constructed extraction (the weighting asymmetry that suppresses distributed-harm voices is not inherent to the problem, but chosen). The tangled-rope classification is correct: the constraint contains both real coordination (tail aversion does reduce catastrophic risk) and real asymmetric extraction (institutional actors benefit while powerless actors bear concentrated costs). The mandatrophy resolution is perspectival: from the beneficiary's view, the constraint is coordination. From the victim's view, it is extraction. The analytical observer's mountain is a false summit — it naturalizes a contingent weighting choice as a law of rational decision theory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_weight_specification,
    'What probability threshold and impact magnitude determine whether a tail risk deserves infinite (or near-infinite) weight in decision-making?',
    'Cross-domain analysis of decision thresholds: climate policy (1 in 1000 year catastrophes weighted heavily), AI safety (1 in 10^6 extinction scenarios weighted infinitely), pandemic preparedness (1 in 100 year events weighted moderately). Consistency test: do the same frameworks apply across domains?',
    'If threshold is objective and universal: catastrophic tail dominance is mountain (natural law of decision theory). If threshold varies by domain, stakeholder power, and institutional context: it is constructed extraction (snare or tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_weight_specification, conceptual, 'Specification of probability-impact threshold for catastrophic risk weighting').

omega_variable(
    distributed_harm_discounting,
    'Why are distributed, diffuse harms (air pollution deaths, climate impacts, occupational exposure) systematically underweighted relative to concentrated, single-event catastrophes in formal risk assessment?',
    'Comparative analysis of utility functions: probability of death from coal air pollution (distributed, ~7000 annual deaths in high-coal regions) vs probability of nuclear accident deaths (concentrated, ~50 year occurrence). Are utility weights equal? If distributed deaths are discounted, what mechanism justifies the discount?',
    'If discounting is justified: tail dominance reflects rational risk preferences. If unjustified: tail dominance is extraction mechanism (victims of distributed harms subsidize catastrophe prevention for beneficiaries of tail-risk-aversion policies). Likely determines if constraint is Rope or Snare from distributed-harm perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributed_harm_discounting, empirical, 'Mechanism and justification for discounting distributed harm in risk assessment').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is ONE reading of the contested kernel acceptable_risk_energy. Which framing dominates decision-making: catastrophic-tail-dominant (this reading), expected-value-dominant (sibling reading), or option-value-preserving (sibling reading)?',
    'Policy analysis: track actual budget allocation, regulatory stringency, and energy deployment decisions over time. Which framework''s predictions match observed outcomes? Interviews with policy-makers: which risk frame do they cite when justifying decisions? If multiple frames are invoked in different contexts, the kernel is distributed rather than dominated by one reading.',
    'If catastrophic-tail-dominant reading dominates: this constraint story is the primary structural description. If expected-value-dominant dominates: expected_value_dominant reading''s extractiveness and suppression are higher than catastrophic_tail_dominant''s. If option-value-preserving dominates: both other readings are downstream coordination mechanisms. The kernel''s reading determines which constraint story is the trunk of the family tree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Dominance of catastrophic-tail vs expected-value vs option-value framings in actual policy').

omega_variable(
    reversibility_asymmetry,
    'Is the asymmetry in how fossil fuel and nuclear risks are treated justified by differences in reversibility: are fossil fuel harms more reversible/correctable than nuclear catastrophes?',
    'Climate science analysis: are changes from 400+ ppm CO2 reversible on human timescales? Radioactive waste: is containment for 10,000 years sustainable? Can distributed harm be remedied faster than catastrophic harm can be mitigated after occurrence?',
    'If nuclear harms are truly less reversible: tail-dominance weighting is justified (mountain-adjacent). If both are comparably irreversible: the asymmetry is constructed and serves extraction logic (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_asymmetry, empirical, 'Comparative reversibility of fossil fuel vs nuclear environmental impacts').

omega_variable(
    infinite_utility_collapse,
    'When catastrophic risks receive infinite or near-infinite utility weights, does the formal decision apparatus become analytically intractable or does it collapse to a fixed heuristic?',
    'Study of risk assessment practice: do researchers actually compute expected values with infinite weights, or do they apply decision rules (e.g., ''nuclear must be < 1 in 10 million annual risk'') that bypass optimization? If rules dominate, the apparatus is theater (piton). If optimization persists, the apparatus is genuinely constraining (mountain).',
    'If apparatus collapses to heuristic: suppression is high (0.68) and theater is high (0.61), confirming piton classification from risk assessment perspective. If genuine optimization persists: the apparatus is architecturally fundamental, supporting mountain view.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infinite_utility_collapse, empirical, 'Whether infinite-weight risk assessment becomes computationally tractable or collapses to heuristic').

omega_variable(
    kernel_sibling_structural_delta,
    'What are the structural differences between this reading (catastrophic-tail-dominant) and the sibling readings (expected-value-dominant and option-value-preserving)?',
    'Constraint family analysis: map the three readings to ε values and victim/beneficiary sets. How does each reading''s extractiveness differ? Which reading''s beneficiaries and victims overlap? Where does the constraint family''s extraction concentration shift as the reading changes?',
    'Expected-value-dominant reading: likely lower suppression (distributed harm counted in expected value), lower theater (formal optimization less theatrical when all harms weighted equally). Option-value-preserving reading: likely rope or scaffold classification (coordination benefit of keeping technological options open exceeds extraction). This omega documents why the catastrophic_tail_dominant reading produces the highest extractiveness and suppression of the three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Structural and metric differences across the acceptable_risk_energy kernel''s three readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophic_tail_dominant, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catast_theater_t0, catastrophic_tail_dominant, theater_ratio, 0, 0.48).
narrative_ontology:measurement(catast_theater_t5, catastrophic_tail_dominant, theater_ratio, 5, 0.55).
narrative_ontology:measurement(catast_theater_t10, catastrophic_tail_dominant, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(catast_extract_t0, catastrophic_tail_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(catast_extract_t5, catastrophic_tail_dominant, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(catast_extract_t10, catastrophic_tail_dominant, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophic_tail_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophic_tail_dominant, 0.18).
narrative_ontology:affects_constraint(catastrophic_tail_dominant, expected_value_dominant).
narrative_ontology:affects_constraint(catastrophic_tail_dominant, option_value_preserving).
narrative_ontology:affects_constraint(catastrophic_tail_dominant, nuclear_waste_intergenerational).
narrative_ontology:affects_constraint(catastrophic_tail_dominant, distributed_harm_underweighting).

% DUAL FORMULATION NOTE:
% This constraint is part of the acceptable_risk_energy constraint family with three structurally distinct readings. The catastrophic-tail-dominant reading (ε=0.58, high suppression, piton theater apparatus) is upstream of nuclear-specific constraints (waste storage, safety certification) because tail aversion justifies nuclear deployment. It is sibling to expected-value-dominant (ε likely lower, more balanced harm accounting) and option-value-preserving (ε likely lower, rope classification emphasizing coordination). All three readings operate on the same underlying institutional domain but decompose the 'acceptable risk' commitment differently. Decomposition necessary because measuring the constraint via different risk framings yields materially different ε values, indicating structurally distinct constraints rather than alternative perspectives on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophic_tail_dominant, institutional, 0.15).
constraint_indexing:directionality_override(catastrophic_tail_dominant, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
