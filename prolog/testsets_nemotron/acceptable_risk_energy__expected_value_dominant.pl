% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable Risk in Energy Pathway Assessment
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   The expected-value-dominant reading of 'acceptable risk' in energy policy
 *   treats mortality-per-TWh as the decisive metric, weighting fossil fuel
 *   air pollution and mining deaths at full expected value while discounting
 *   low-probability nuclear catastrophic outcomes by their probability. This
 *   reading emerged from Cold War-era probabilistic risk assessment (PRA)
 *   institutions and was cemented in integrated assessment models (IAMs) that
 *   guide climate-energy policy. The constraint coordinates energy investment
 *   decisions around a single commensurable metric (coordination function)
 *   but simultaneously extracts by rendering fossil fuel chronic harms
 *   visible while nuclear acute harms are probabilistically attenuated
 *   (extraction function). Fossil fuel interests benefit from the full-weight
 *   accounting of their diffuse harms being methodologically difficult to
 *   attribute and regulate; nuclear proponents benefit from probability
 *   discounting of tail risks; mainstream economists and modelers benefit
 *   from the institutionalization of expected utility as the sole rational
 *   framework. Victims include communities bearing fossil pollution burdens
 *   (whose harms are real but statistically diffuse), climate-vulnerable
 *   populations (whose future harms are discounted), nuclear accident victims
 *   (whose low-probability catastrophic harms are systematically minimized),
 *   and energy justice advocates (excluded from the methodological frame).
 *   Active enforcement occurs through regulatory licensing, IAM gatekeeping,
 *   insurance frameworks, and the exclusion of alternative decision
 *   frameworks from policy discourse.
 *
 * KEY AGENTS:
 *   - fossil_fuel_interests: Primary beneficiary (institutional/arbitrage) — extracts via full-weight accounting of diffuse harms being practically unenforceable
 *   - nuclear_industry_proponents: Primary beneficiary (organized/constrained) — benefits from probability discounting of catastrophic tail risks
 *   - mainstream_energy_economists: Agenda setter / beneficiary (institutional/analytical) — sets the methodological frame, gains professional authority
 *   - integrated_assessment_modelers: Agenda setter (institutional/analytical) — operationalizes the constraint in policy-critical models
 *   - fossil_fuel_affected_communities: Primary victim (powerless/trapped) — bears diffuse chronic harms rendered methodologically invisible
 *   - climate_vulnerable_populations: Primary victim (powerless/identity_locked) — bears discounted future harms with no exit
 *   - future_generations: Victim (excluded/analytical) — structurally excluded from the discounting calculus
 *   - nuclear_accident_victims: Victim (powerless/trapped) — bears catastrophic harms discounted by probability
 *   - energy_justice_advocates: Excluded (moderate/constrained) — challenges the victim set construction but lacks institutional standing
 *   - precautionary_principle_advocates: Excluded (moderate/constrained) — offers alternative decision framework excluded from IAMs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.72).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable Risk in Energy Pathway Assessment").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '3d85cc11-9f41-48e5-8425-6332251d5717').
narrative_ontology:cs_kernel_codification('3d85cc11-9f41-48e5-8425-6332251d5717', formalized).
narrative_ontology:cs_authority_grounding('3d85cc11-9f41-48e5-8425-6332251d5717', extraction).
narrative_ontology:cs_interpretation_layer_present('3d85cc11-9f41-48e5-8425-6332251d5717').
narrative_ontology:cs_reading_relation('3d85cc11-9f41-48e5-8425-6332251d5717', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('3d85cc11-9f41-48e5-8425-6332251d5717', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('3d85cc11-9f41-48e5-8425-6332251d5717', foundational, expected_utility_maximization_as_rationality).
narrative_ontology:cs_axiom_status(expected_utility_maximization_as_rationality, holdable).
narrative_ontology:cs_axiom_grounding('3d85cc11-9f41-48e5-8425-6332251d5717', expected_utility_maximization_as_rationality, conventional).
narrative_ontology:cs_axiom('3d85cc11-9f41-48e5-8425-6332251d5717', foundational, probabilistic_discounting_of_tail_risks).
narrative_ontology:cs_axiom_status(probabilistic_discounting_of_tail_risks, holdable).
narrative_ontology:cs_axiom_grounding('3d85cc11-9f41-48e5-8425-6332251d5717', probabilistic_discounting_of_tail_risks, empirically_contingent).
narrative_ontology:cs_axiom('3d85cc11-9f41-48e5-8425-6332251d5717', secondary, intergenerational_discounting_legitimacy).
narrative_ontology:cs_axiom_status(intergenerational_discounting_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3d85cc11-9f41-48e5-8425-6332251d5717', intergenerational_discounting_legitimacy, conventional).
narrative_ontology:cs_reference_frame('3d85cc11-9f41-48e5-8425-6332251d5717', cold_war_pra_framework).
narrative_ontology:cs_drift_state('3d85cc11-9f41-48e5-8425-6332251d5717', climate_emergency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d85cc11-9f41-48e5-8425-6332251d5717', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, fossil_fuel_interests).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_industry_proponents).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, mainstream_energy_economists).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, integrated_assessment_modelers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_affected_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, nuclear_accident_victims).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, energy_justice_advocates).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, expected_utility_theory).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, cost_benefit_analysis_primacy).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, probabilistic_risk_assessment_validity).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, intergenerational_discounting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major oil, gas, and coal companies and their industry associations. Benefit from the expected-value frame because their diffuse, chronic harms (air pollution, mining deaths, climate contributions) are methodologically difficult to fully internalize at the same standard as nuclear acute risks. They maintain exit options through asset diversification, political influence, and the ability to shift production geographies. The constraint's suppression of alternatives (renewables, efficiency) protects their market position.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_interests, beneficiary,
    institutional, generational, arbitrage, global).

% Nuclear vendors, utilities, and regulatory advocates. Benefit from probability discounting of low-probability high-consequence accidents (Fukushima, Chernobyl-scale events). Their exit is constrained by massive capital commitments, regulatory licensing path dependence, and waste liability structures. They advocate for the expected-value frame as 'rational' while opposing precautionary frameworks that would weight tail risks more heavily.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_industry_proponents, beneficiary,
    organized, generational, constrained, global).

% Academic and policy economists who established expected utility theory as the normative standard for energy risk. They set the methodological agenda through journals, advisory roles, and institutional positions. They benefit professionally from the frame's dominance (grants, citations, policy influence). Their exit is analytical — they could adopt alternative frameworks but face professional costs for doing so.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, mainstream_energy_economists, agenda_setter,
    institutional, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, mainstream_energy_economists, beneficiary).

% Researchers building and running IAMs (e.g., IMAGE, MESSAGE, GCAM, REMIND) that operationalize expected-value risk assessment for climate policy. They control the computational infrastructure that translates the constraint into policy scenarios. Their professional standing depends on the frame's continued centrality in IPCC and national assessments. Exit is analytical but institutionally costly.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, integrated_assessment_modelers, agenda_setter,
    institutional, biographical, analytical, global).

% Communities near extraction sites, refineries, power plants, and transport corridors bearing air pollution, water contamination, and health impacts. Their harms enter the victim set with full weight in principle but are methodologically diffuse and politically invisible. Exit is trapped: geographic immobility, economic dependence, and lack of political voice prevent escape. The constraint's coordination function (standardized risk metrics) does not reach their scale of resolution.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_affected_communities, payer,
    powerless, biographical, trapped, local).

% Populations in low-lying, arid, or otherwise climate-exposed regions bearing escalating harms from fossil-driven warming. Their future harms are discounted by the constraint's intergenerational discount rate. Exit is identity-locked: their cultural, territorial, and existential identity is bound to threatened lands; relocation is not 'exit' but destruction. They are structurally excluded from the discounting calculus that determines their fate.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, climate_vulnerable_populations, payer,
    powerless, generational, identity_locked, global).

% All persons not yet born who will bear the compounded harms of energy pathway decisions made today. They have no voice in the constraint's construction, no standing in cost-benefit analysis, and no exit from the world they will inherit. The expected-value frame's discounting structurally excludes them by design — their infinite future harm is rendered finite by the discount rate.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% Populations exposed to radioactive contamination from nuclear accidents (Chernobyl, Fukushima, Kyshtym, Windscale, and potential future events). Their catastrophic harms are acknowledged but discounted by probability in the expected-value frame. Exit is trapped: contamination renders land uninhabitable for generations, health effects are latent and intergenerational, and compensation regimes are capped and contested.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_accident_victims, payer,
    powerless, biographical, trapped, regional).

% Civil society organizations, legal advocates, and scholars challenging the victim set construction, discounting practices, and exclusion of alternative frameworks. They have analytical access to the constraint's structure but lack institutional standing to change IAM gatekeeping, licensing criteria, or insurance frameworks. Their exit is constrained: they can advocate alternatives but cannot implement them at policy scale.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_justice_advocates, excluded,
    moderate, biographical, constrained, global).

% Scholars, regulators, and advocates for decision frameworks that prioritize avoiding irreversible harm under uncertainty (precautionary principle, robust decision-making, safe minimum standards, info-gap theory). They offer structurally distinct readings of acceptable risk but are excluded from the computational and institutional machinery of energy policy assessment. Their exit is constrained: they operate in parallel discourses (international law, environmental policy) but cannot access the IAM/PRA nexus.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, precautionary_principle_advocates, excluded,
    moderate, biographical, constrained, global).

% The Deferential Realism classification engine's analytical seat — sees the full structural asymmetry: beneficiaries with arbitrage/analytical exit, victims with trapped/identity_locked exit, agenda-setters who administer the frame and benefit from its authority. Computes per-seat effective extraction from the declared structural data.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single commensurable metric (mortality-per-TWh) enabling cross-pathway energy investment and policy decisions under uncertainty. Solves the genuine coordination problem of comparing wildly different risk profiles (chronic diffuse vs. acute catastrophic) on a common scale.
% TRANSFER_FUNCTION: Moves decision-making authority and resource allocation toward pathways whose harms are methodologically favored by the expected-value frame (fossil with diffuse harms, nuclear with discounted tail risks) and away from pathways whose harms are methodologically penalized (renewables with upfront capital costs, efficiency with distributed benefits). Transfers risk burden from incumbent industries to affected communities and future generations.
% ABSENT_VOICES: Future generations (structurally excluded by discounting), communities in sacrifice zones (methodologically invisible at IAM resolution), precautionary principle advocates (institutionally excluded from IAM/PRA machinery), and ecological systems (no standing in anthropocentric mortality metrics). These voices would object to the victim set construction, the discounting of tail risks, and the exclusion of non-utilitarian decision frameworks.
% DISAPPEARANCE_RATIONALE: If the expected-value-dominant constraint vanished overnight, energy policy would lose its central commensuration metric. Fossil and nuclear pathways would lose their methodological subsidy; renewable and efficiency pathways would gain relative standing; alternative decision frameworks (precautionary, robust, option-value) would enter policy discourse. The global energy investment architecture (trillions in capital allocation) would reorganize around a new risk paradigm. The world rearranges because the constraint actively structures trillions in capital flows and institutional authority.
% FOUNDING_PROBLEM: Post-WWII energy expansion required a rational framework for comparing risks across nascent nuclear, expanding fossil, and hypothetical renewable pathways under deep uncertainty about technology evolution, demand growth, and geopolitical stability. The founding problem was: how to make energy infrastructure decisions commensurable and defensible when risks are heterogeneous, uncertain, and politically contested.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear industry historians and PRA pioneers (e.g., Rasmussen Report authors) attest the problem was rational comparison under uncertainty. Climate economists (e.g., Stern, Nordhaus debates) and energy justice scholars attest the problem has shifted: deep uncertainty and irreversible harm now dominate, making expected-value frameworks inadequate. The IPCC's evolving treatment of risk (AR5 to AR6) corroborates the shift from expected-value to risk-management framings. No single party outside the benefiting interests attests the original problem is unchanged.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint's methodological choices systematically advantage incumbent energy pathways: fossil diffuse harms are hard to internalize while nuclear acute harms are attenuated by probability weighting. Suppression (0.72) is high because alternative decision frameworks (precautionary principle, robust decision-making, safe minimum standards, option value approaches) are excluded from the institutional machinery of energy planning — not by argument but by gatekeeping in IAMs, licensing, and insurance. Theater ratio (0.28) is moderate: the PRA/IAM apparatus performs genuine coordination (standardized risk comparison across pathways) but a growing share of its activity defends the methodological frame against challenges from climate justice, tail risk, and deep uncertainty literatures. Accessibility collapse (0.55) reflects that once the expected-value frame is adopted, alternatives appear 'irrational' — but resistance (0.65) is rising as climate damages materialize and the frame's exclusions become visible. The measurement series (1970-2030) show extractiveness and suppression rising together as the frame matured and fossil/nuclear interests consolidated around it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (economists, modelers, industry), the constraint is a genuine coordination achievement: a commensurable metric enabling rational energy choices. From the victim seats (affected communities, climate-vulnerable, future generations), the same structure operates as enforced extraction: their harms are systematically minimized by methodological fiat. The engine computes this per-seat divergence from the structural data; the claimed_type (tangled_rope) reflects my assessment that both coordination and extraction are genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel interests and nuclear proponents are structural beneficiaries (d near 0.0-0.15): they gain from methodological choices that make their harms appear smaller or more manageable. Mainstream economists and modelers are agenda-setters with analytical exit (d ~0.1-0.2): they administer the frame and gain professional rents. Fossil-affected communities and climate-vulnerable populations are structural targets (d near 0.85-0.95): they bear harms the frame renders invisible or discounts, with trapped or identity-locked exit. Nuclear accident victims are targets (d ~0.7-0.8): their harms are acknowledged but discounted by probability. Energy justice and precautionary advocates are excluded (d ~0.6-0.7): they see the structure but cannot change it from within. The directionality derives from the victim/beneficiary declarations and exit options — the engine computes d from these structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — rational comparison of energy risks under uncertainty — remains live, but the specific expected-value frame has outlived its adequacy as climate tail risks and deep uncertainty challenge its epistemic foundations. The constraint persists not because it solves the problem better than alternatives, but because the institutional machinery (IAMs, PRA, licensing, insurance) has co-evolved with fossil and nuclear interests to make the frame self-reinforcing. This is Tangled Rope, not Piton: the coordination function is real and actively maintained, but extraction is asymmetric and enforcement-dependent. The high suppression and rising extractiveness indicate active maintenance, not inertial decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expected_value_kernel_vs_reading,
    'Is the expected-value framing a genuine Mountain of decision theory or a constructed constraint that benefits identifiable agents?',
    'Historical analysis of whether expected utility theory was the only coherent decision framework available when energy risk institutions were designed, or whether alternative frameworks (precautionary principle, robust decision-making, safe minimum standards) were systematically excluded. Compare institutional genealogies across nuclear, fossil, and renewable sectors.',
    'If constructed, the constraint is a false-summit Mountain (or Tangled Rope) benefiting fossil and nuclear interests through methodological capture; if natural, it is a genuine coordination standard for rational choice under uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expected_value_kernel_vs_reading, conceptual, 'Whether expected-value dominance reflects a natural law of rationality or an institutional choice that advantages certain energy pathways').

omega_variable(
    fossil_deaths_victim_set_inclusion,
    'Are fossil fuel air pollution and mining deaths structurally included in the victim set with full weight, or are they rendered invisible through methodological choices?',
    'Trace the accounting boundary in major integrated assessment models (IAMs) and policy cost-benefit analyses: which harms are internalized, which are externalized, and whether the exclusion correlates with institutional power of the emitting industries.',
    'Full inclusion would make fossil pathways the dominant harm source, reversing the typical ranking; partial inclusion makes nuclear appear uniquely dangerous per unit energy. This is the central structural delta between expected_value_dominant and catastrophic_tail_dominant readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fossil_deaths_victim_set_inclusion, empirical, 'Whether the victim set structurally encompasses diffuse chronic harms from fossil pathways at the same evidentiary standard as acute nuclear harms').

omega_variable(
    tail_risk_discounting_mechanism,
    'Does discounting low-probability high-consequence nuclear events by their probability reflect rational expectation-maximization or a structural exclusion of catastrophic tail risk?',
    'Compare the discounting treatment across domains: are nuclear accident probabilities derived from the same epistemic standards as climate tipping point probabilities? Test whether the probability estimates themselves are endogenous to the institutional interests they serve.',
    'If discounting is epistemically symmetric, the reading is internally coherent; if nuclear tails are discounted while climate tails are not (or vice versa), the constraint applies a double standard that extracts from the disfavored pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_discounting_mechanism, conceptual, 'Whether probabilistic discounting of catastrophic outcomes is applied symmetrically across energy pathways').

omega_variable(
    suppression_of_fossil_pathway_alternatives,
    'Is the high suppression of fossil pathway alternatives structural (subsidies, infrastructure lock-in, regulatory capture) or a natural consequence of market optimization?',
    'Analyze the persistence of fossil subsidies, stranded asset risk socialization, and the rate of renewable deployment under counterfactual policy regimes. Test whether the ''market'' for energy pathways is a level playing field or a constructed arena.',
    'If suppression is structural, the constraint actively prevents exit from fossil pathways, making it extractive (Snare/Tangled Rope); if natural, it is a Rope coordinating around real physical limits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_fossil_pathway_alternatives, empirical, 'Whether the constraint''s enforcement machinery actively blocks transition from fossil pathways').

omega_variable(
    commitment_system_reading_relations,
    'What is the structural relationship between this expected-value-dominant reading and its sibling readings of the acceptable_risk_energy kernel?',
    'Analyze whether a single policy framework could simultaneously hold expected-value-dominance, catastrophic-tail-dominance, and option-value-preservation as live commitments, or whether they logically foreclose each other within one institutional framework.',
    'Determines whether the kernel''s readings are in genuine contest (coexists_with) or whether adopting one reading forces rejection of the others (forecloses), which shapes how institutional capture operates across the kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_system_reading_relations, conceptual, 'Structural relations between sibling readings of the acceptable_risk_energy kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 1970, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_tr_t1970, acceptable_risk_energy__expected_value_dominant, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_tr_t1985, acceptable_risk_energy__expected_value_dominant, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_tr_t2000, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_tr_t2015, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_tr_t2030, acceptable_risk_energy__expected_value_dominant, theater_ratio, 2030, 0.28).

% Extraction over time
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_be_t1970, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_be_t1985, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_be_t2000, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_be_t2015, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_be_t2030, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_su_t1970, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_su_t1985, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_su_t2000, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_su_t2015, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_su_t2030, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 2030, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1970, tn=2030
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_01, acceptable_risk_energy__expected_value_dominant, accessibility_collapse(class), 1970, 0.5).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_01, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_02, acceptable_risk_energy__expected_value_dominant, accessibility_collapse(class), 2030, 0.6).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_02, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_03, acceptable_risk_energy__expected_value_dominant, accessibility_collapse(individual), 1970, 0.55).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_03, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_04, acceptable_risk_energy__expected_value_dominant, accessibility_collapse(individual), 2030, 0.65).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_04, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_05, acceptable_risk_energy__expected_value_dominant, accessibility_collapse(organizational), 1970, 0.4).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_05, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_06, acceptable_risk_energy__expected_value_dominant, accessibility_collapse(organizational), 2030, 0.55).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_06, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_07, acceptable_risk_energy__expected_value_dominant, accessibility_collapse(structural), 1970, 0.45).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_07, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_08, acceptable_risk_energy__expected_value_dominant, accessibility_collapse(structural), 2030, 0.65).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_08, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_09, acceptable_risk_energy__expected_value_dominant, resistance(class), 1970, 0.3).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_09, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_10, acceptable_risk_energy__expected_value_dominant, resistance(class), 2030, 0.65).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_10, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_11, acceptable_risk_energy__expected_value_dominant, resistance(individual), 1970, 0.25).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_11, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_12, acceptable_risk_energy__expected_value_dominant, resistance(individual), 2030, 0.7).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_12, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_13, acceptable_risk_energy__expected_value_dominant, resistance(organizational), 1970, 0.35).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_13, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_14, acceptable_risk_energy__expected_value_dominant, resistance(organizational), 2030, 0.6).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_14, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_15, acceptable_risk_energy__expected_value_dominant, resistance(structural), 1970, 0.4).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_15, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_16, acceptable_risk_energy__expected_value_dominant, resistance(structural), 2030, 0.55).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_16, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_17, acceptable_risk_energy__expected_value_dominant, stakes_inflation(class), 1970, 0.4).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_17, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_18, acceptable_risk_energy__expected_value_dominant, stakes_inflation(class), 2030, 0.6).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_18, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_19, acceptable_risk_energy__expected_value_dominant, stakes_inflation(individual), 1970, 0.45).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_19, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_20, acceptable_risk_energy__expected_value_dominant, stakes_inflation(individual), 2030, 0.65).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_20, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_21, acceptable_risk_energy__expected_value_dominant, stakes_inflation(organizational), 1970, 0.3).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_21, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_22, acceptable_risk_energy__expected_value_dominant, stakes_inflation(organizational), 2030, 0.55).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_22, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_23, acceptable_risk_energy__expected_value_dominant, stakes_inflation(structural), 1970, 0.35).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_23, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_24, acceptable_risk_energy__expected_value_dominant, stakes_inflation(structural), 2030, 0.65).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_24, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_25, acceptable_risk_energy__expected_value_dominant, suppression(class), 1970, 0.55).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_25, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_26, acceptable_risk_energy__expected_value_dominant, suppression(class), 2030, 0.75).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_26, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_27, acceptable_risk_energy__expected_value_dominant, suppression(individual), 1970, 0.6).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_27, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_28, acceptable_risk_energy__expected_value_dominant, suppression(individual), 2030, 0.8).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_28, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_29, acceptable_risk_energy__expected_value_dominant, suppression(organizational), 1970, 0.45).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_29, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_30, acceptable_risk_energy__expected_value_dominant, suppression(organizational), 2030, 0.7).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_30, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_31, acceptable_risk_energy__expected_value_dominant, suppression(structural), 1970, 0.5).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_31, observed).
narrative_ontology:measurement(acceptable_risk_energy__expected_value_dominant_grid_32, acceptable_risk_energy__expected_value_dominant, suppression(structural), 2030, 0.75).
narrative_ontology:measurement_basis(acceptable_risk_energy__expected_value_dominant_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__expected_value_dominant, 0.15).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, integrated_assessment_model_gatekeeping).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, probabilistic_risk_assessment_licensing).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, energy_insurance_frameworks).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, climate_damage_discounting).

% DUAL FORMULATION NOTE:
% This constraint is one member of the acceptable_risk_energy constraint family (kernel_id: acceptable_risk_energy). The three readings (expected_value_dominant, catastrophic_tail_dominant, option_value_preserving) share a kernel but instantiate structurally distinct constraints with different ε values, victim sets, and suppression profiles. The expected_value_dominant reading has the highest extractiveness (0.68) and suppression (0.72) because its methodological frame most advantages incumbent fossil and nuclear pathways. The catastrophic_tail_dominant reading would show lower extractiveness for nuclear but higher for fossil (different victim weighting). The option_value_preserving reading would show lowest extractiveness but highest theater (maintaining unused pathways as coordination theater). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, institutional, 0.15).
constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, powerless, 0.9).
constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, moderate, 0.65).
constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, organized, 0.2).
constraint_indexing:directionality_override(acceptable_risk_energy__expected_value_dominant, analytical, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
