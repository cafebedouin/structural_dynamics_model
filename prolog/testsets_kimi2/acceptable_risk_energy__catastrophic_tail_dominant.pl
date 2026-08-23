% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Acceptable Risk Framework in Energy Policy
 *   domain: risk assessment/energy policy/decision theory
 *
 * SUMMARY:
 *   This constraint story captures the catastrophic-tail-dominant reading of
 *   the acceptable_risk_energy kernel in energy policy. Under this reading,
 *   regulatory frameworks and public institutions treat low-probability,
 *   high-consequence events (e.g., nuclear reactor core melts) as lexically
 *   prior to aggregate expected harm, even when empirical mortality metrics
 *   show the suppressed technology is safer than incumbent alternatives on an
 *   expected-value basis. The constraint is actively enforced through nuclear
 *   safety regulation, licensing moratoria, and public-risk discourse that
 *   renders nuclear expansion structurally infeasible. The result is not
 *   merely a preference ordering but a constraint that reallocates energy
 *   investment toward fossil fuel pathways, imposing diffuse aggregate health
 *   and climate costs on the public. This is a kernel reading: the same
 *   natural-language concept of 'acceptable risk' hosts rival readings
 *   (expected-value-dominant, option-value-preserving) with structurally
 *   distinct beneficiary/victim profiles and Îµ values.
 *
 * KEY AGENTS:
 *   - nuclear_regulators (institutional/constrained): agenda-setter â defines and enforces the catastrophic-tail risk framework
 *   - fossil_fuel_incumbents (powerful/mobile): beneficiary â captures market share from nuclear suppression
 *   - nuclear_sector_firms (organized/trapped): payer â bears direct suppression costs and regulatory burden
 *   - general_public_health (powerless/trapped): payer â bears diffuse aggregate harm from retained fossil fuels
 *   - expected_value_analysts (moderate/analytical): excluded â systematically marginalized in policy discourse
 *   - precautionary_advocacy_networks (organized/mobile): beneficiary/agenda-setter â derives influence from maintaining tail-risk salience
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.72).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.8).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Acceptable Risk Framework in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk assessment/energy policy/decision theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, 'c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c').
narrative_ontology:cs_kernel_codification('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', formalized).
narrative_ontology:cs_authority_grounding('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', expertise).
narrative_ontology:cs_interpretation_layer_present('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c').
narrative_ontology:cs_reading_relation('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', acceptable_risk_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', foundational, catastrophic_risk_infinite_weight).
narrative_ontology:cs_axiom_status(catastrophic_risk_infinite_weight, holdable).
narrative_ontology:cs_axiom_grounding('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', catastrophic_risk_infinite_weight, deontological).
narrative_ontology:cs_axiom('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', foundational, expected_harm_not_sovereign).
narrative_ontology:cs_axiom_status(expected_harm_not_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', expected_harm_not_sovereign, deontological).
narrative_ontology:cs_reference_frame('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', catastrophic_aversion_framework).
narrative_ontology:cs_drift_state('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', climate_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c3fe82c5-d534-47d3-bafc-4f8b20bd5a2c', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_advocacy_networks).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_sector_firms).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, general_public_health).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets legally binding safety standards and risk acceptance criteria for nuclear energy. Enforces a framework that treats low-probability catastrophic outcomes as disqualifying regardless of expected mortality comparisons. Derives institutional legitimacy and budget from maintaining this precautionary boundary.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Retain dominant market share in base-load power generation because nuclear expansion is blocked by regulatory frameworks that weight catastrophic tail risk heavily. Collect rents from continued fossil fuel infrastructure utilization and avoided competition from a lower-marginal-cost low-carbon baseload source.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, mobile, global).

% Bear the direct costs of risk frameworks that impose near-infinite safety requirements, lengthy permitting, and public-liability structures that render financing contingent on political risk. Their technology is structurally suppressed despite lower mortality per TWh.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_sector_firms, payer,
    organized, biographical, trapped, national).

% Pays the aggregate health and climate cost of an energy system that retains fossil fuels because the catastrophic-tail framework blocks nuclear scaling. Mortality from particulate pollution and climate tipping points is distributed, temporally deferred, and discounted in the regulatory framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, general_public_health, payer,
    powerless, generational, trapped, global).

% Produce comparative risk assessments showing nuclear mortality per TWh is lower than fossil alternatives. Their framework is structurally excluded from regulatory decision-making because it contradicts the lexical priority of catastrophic-tail avoidance.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, expected_value_analysts, excluded,
    moderate, civilizational, analytical, global).

% Derive influence, funding, and institutional role from maintaining public salience of catastrophic nuclear risk. Their advocacy sets the political boundary conditions within which regulators operate, though they do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_advocacy_networks, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_advocacy_networks, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for evaluating energy technologies when catastrophic failure modes exist, allowing societies to coordinate on safety standards and avoid uninsurable collective losses.
% TRANSFER_FUNCTION: Transfers energy market share and investment flows away from nuclear pathways toward fossil and renewable pathways, while transferring the burden of proof for safety to nuclear operators and the burden of aggregate pollution harm to the general public.
% ABSENT_VOICES: Expected-value energy economists and future climate-vulnerable populations are largely excluded from the regulatory conversation; the former because their framework contradicts the precautionary premise, the latter because they are temporally and geographically diffuse.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant framework vanished overnight, nuclear permitting would accelerate, fossil fuel retirement would likely hasten, and the composition of the global energy mix would shift substantially; the institutional architecture of nuclear regulation would reorganize around expected-value or option-value criteria.
% FOUNDING_PROBLEM: The founding problem was uncoordinated development of energy technologies with potentially catastrophic failure modes (e.g., reactor meltdowns) in the absence of societal consensus on how to value low-probability, high-consequence risks against routine harms.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear engineers and climate scientists outside the fossil-fuel beneficiary set attest that the original catastrophic-risk problem has been partially solved by technological advance (Gen III+ reactors, passive safety), while public-health economists attest that the aggregate harm from fossil fuels now exceeds the catastrophic risk premium; corroboration from these outside seats supports the contested status.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the framework systematically transfers energy market share and public-health costs away from nuclear technology and toward fossil incumbents, despite lower expected mortality from nuclear. Suppression (0.80) is higher because the constraint's persistence depends on active regulatory exclusion of nuclear pathways, public-liability structures that make financing contingent on political risk, and discourse that renders expected-value comparisons unspeakable in policy. Theater ratio (0.45) reflects moderate performativity: genuine safety engineering exists, but an increasing share of regulatory activity performs precautionary due diligence that exceeds marginal risk reduction (e.g., endlessly revising plant designs in response to each hypothetical tail scenario) while ignoring the accumulating aggregate harm of fossil alternatives. Accessibility collapse (0.70) is substantial because once the catastrophic-tail frame dominates institutional discourse, expected-value alternatives become cognitively and politically inaccessible. Resistance (0.55) is moderate: the nuclear sector and some academic economists mount sustained critique, but they are outmatched by the institutional weight of regulators and the cultural power of precautionary advocacy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulators) experiences the constraint as genuine, expertise-based coordination around an authentic societal aversion to catastrophe. The payer seats (nuclear firms and the diffuse public) experience the same structure as extraction: the former because it confiscates their market viability, the latter because it imposes aggregate health costs that the framework structurally discounts. The beneficiary seat (fossil fuel incumbents) experiences it as a favorable market condition rather than an active constraint. The engine computes this divergence from identical structural data; the claim is tangled_rope precisely because the coordination function (catastrophe avoidance) and extraction function (fossil fuel market preservation) are inseparable in the operating arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel incumbents are structural beneficiaries: the constraint subsidizes their market position by suppressing a lower-cost, lower-emission competitor (d near 0.0). Nuclear sector firms are structural targets: the constraint extracts directly from them through denied permits, inflated compliance costs, and exclusion from capital markets (d near 1.0). The general public health payer is also a target, but more diffusely: the extraction is ambient (air pollution, climate deferred costs) rather than direct transfer, so their d is high but their powerlessness and diffuse scope dampen immediate perceived extractiveness. Regulators derive institutional authority, placing them near symmetric or lightly beneficiary (d ~0.3-0.4). Expected-value analysts are excluded entirely; their d is undefined or analytical. No overrides are needed because the structural derivation from role and exit options captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework was built to solve a genuine coordination problemâhow to govern technologies with potentially uninsurable catastrophic tails. That problem is not dead; nuclear accidents remain possible. However, the classification as tangled_rope rather than snare prevents mislabeling the coordination function as pure cover: the safety standards do reduce real risk. The mandatrophy question is whether the coordination function has atrophied into a fossil-fuel market preservation device. The theater ratio (0.45) and rising extractiveness series suggest partial atrophy but not full piton degradation: a substantial fraction of regulatory activity remains functionally oriented to safety rather than performance. If the founding problem is judged dead (reactor designs have solved the founding catastrophic risk), the mismatch flag (dead founding + world_rearranges disappearance) would trigger piton investigation; here the founding problem status is contested, keeping the classification in tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'Would reclassifying this constraint under the expected_value_dominant reading dissolve the asymmetric extraction, or merely redistribute it?',
    'Compare the full three-reading kernel family; if expected_value produces symmetric costs but option_value preserves nuclear, the extraction is reading-specific.',
    'If extraction vanishes under sibling readings, this constraint''s classification as tangled_rope is confirmed as a property of the catastrophic_tail reading, not the kernel itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether the extraction is specific to this kernel reading').

omega_variable(
    fossil_fuel_mortality_discounting,
    'Are fossil-fuel-related deaths truly distributed and reversible, or does the catastrophic-tail framework artificially render them invisible to decision-makers?',
    'Epidemiological attribution of mortality to particulate exposure and climate change, compared against nuclear accident mortality.',
    'If fossil mortality is shown to be comparably irreversible and concentrated on vulnerable populations, the victim set of this constraint expands and the coordination story weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_fuel_mortality_discounting, empirical, 'Whether fossil mortality is structurally discounted').

omega_variable(
    regulatory_authority_legitimacy,
    'Does the nuclear regulator''s authority rest on genuine technical expertise, or on the extraction of institutional budget and autonomy through catastrophic-risk dramatization?',
    'Independent audit of regulatory cost-benefit methodologies against international best practice; assessment of regulatory capture by precautionary advocacy.',
    'If authority is primarily extractive, directionality for the regulator shifts toward beneficiary and the constraint edges toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_authority_legitimacy, conceptual, 'Whether regulatory authority is expertise or extraction-based').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of nuclear pathways driven primarily by structural regulatory barriers or by internalized public fear that persists even when barriers are technically surmountable?',
    'Natural experiments in jurisdictions with regulatory reform versus jurisdictions with similar rules but strong public opposition.',
    'If suppression persists after structural barriers are lowered, effective suppression is higher than structural measures suggest, amplifying extraction for nuclear sector firms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cte_ctd_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cte_ctd_tr_t10, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.35).
narrative_ontology:measurement(cte_ctd_tr_t20, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cte_ctd_tr_t30, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.43).
narrative_ontology:measurement(cte_ctd_tr_t40, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(cte_ctd_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cte_ctd_be_t10, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(cte_ctd_be_t20, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cte_ctd_be_t30, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cte_ctd_be_t40, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cte_ctd_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cte_ctd_su_t10, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(cte_ctd_su_t20, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(cte_ctd_su_t30, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(cte_ctd_su_t40, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the acceptable_risk_energy kernel. The natural-language label 'acceptable risk' in energy policy conflates three structurally distinct decision rules: catastrophic tail dominance, expected value minimization, and option value preservation. Each reading produces a distinct Îµ, beneficiary/victim structure, and constraint type. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
