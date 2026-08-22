% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Expected-Value-Dominant Acceptable Risk Standard for Energy Pathways
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint is the expected-value-dominant reading of the
 *   acceptable-risk-energy kernel: acceptable risk is operationalized as
 *   minimizing aggregate expected harm across energy pathways, measured via
 *   mortality-per-TWh. Under this reading, fossil fuel mortality (ambient air
 *   pollution, mining, occupational exposure) is counted at full statistical
 *   weight because it is chronic and well-sampled, while nuclear accident
 *   mortality is discounted by its low probability of occurrence, producing a
 *   low expected-harm figure for nuclear even though any single realized
 *   accident would be catastrophic and geographically concentrated. This
 *   reading actively suppresses fossil-pathway legitimacy (high suppression
 *   score) by treating the aggregate ranking as dispositive for siting and
 *   licensing questions that specific communities experience very differently
 *   depending on which pathway is sited near them. The sibling readings —
 *   catastrophic_tail_dominant and option_value_preserving — evaluate the
 *   same underlying kernel (what counts as 'acceptable risk' in energy
 *   policy) by different lights and are NOT represented as alternatives
 *   within this constraint; they are separate constraints with their own ε
 *   and stakeholder sets.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.42).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.71).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable Risk Standard for Energy Pathways").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '4cefad53-f616-46cc-bde1-802ffdf90dd4').
narrative_ontology:cs_kernel_codification('4cefad53-f616-46cc-bde1-802ffdf90dd4', formalized).
narrative_ontology:cs_authority_grounding('4cefad53-f616-46cc-bde1-802ffdf90dd4', expertise).
narrative_ontology:cs_interpretation_layer_present('4cefad53-f616-46cc-bde1-802ffdf90dd4').
narrative_ontology:cs_reading_relation('4cefad53-f616-46cc-bde1-802ffdf90dd4', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('4cefad53-f616-46cc-bde1-802ffdf90dd4', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('4cefad53-f616-46cc-bde1-802ffdf90dd4', foundational, expected_value_is_the_correct_aggregation_rule).
narrative_ontology:cs_axiom_status(expected_value_is_the_correct_aggregation_rule, holdable).
narrative_ontology:cs_axiom_grounding('4cefad53-f616-46cc-bde1-802ffdf90dd4', expected_value_is_the_correct_aggregation_rule, instrumental).
narrative_ontology:cs_axiom('4cefad53-f616-46cc-bde1-802ffdf90dd4', foundational, probability_discounting_is_valid_across_consequence_scales).
narrative_ontology:cs_axiom_status(probability_discounting_is_valid_across_consequence_scales, holdable).
narrative_ontology:cs_axiom_grounding('4cefad53-f616-46cc-bde1-802ffdf90dd4', probability_discounting_is_valid_across_consequence_scales, empirically_contingent).
narrative_ontology:cs_reference_frame('4cefad53-f616-46cc-bde1-802ffdf90dd4', probability_weighted_aggregate_harm_minimization).
narrative_ontology:cs_drift_state('4cefad53-f616-46cc-bde1-802ffdf90dd4', contemporary_siting_disputes, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4cefad53-f616-46cc-bde1-802ffdf90dd4', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, grid_planners_and_regulators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, public_health_from_reduced_air_pollution).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, coal_and_gas_frontline_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, artisanal_mining_and_extraction_workers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, populations_near_potential_nuclear_accident_sites).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, mortality_per_twh_as_sufficient_risk_metric).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, expected_value_maximization_as_rational_policy_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate reactors whose mortality-per-TWh figures are low once accident probabilities are multiplied through. The expected-value framing lets them present nuclear as the safest major energy source and secure permitting, insurance caps, and public subsidy on that basis. They face no discounting of the benefit their favored metric grants them.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators, beneficiary,
    organized, generational, arbitrage, national).

% Adopt mortality-per-TWh as the official comparative standard for licensing, siting, and portfolio decisions. They administer the metric, choose which harms are counted (occupational, ambient, accident) and at what discount, and can revise the standard, but doing so would mean re-litigating decades of settled licensing and comparative-risk precedent.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, grid_planners_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Represents the aggregate mortality reduction achieved when fossil generation is displaced by lower-mortality-per-TWh sources under this standard. Not an organized actor, but the outcome the standard is credited with producing.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, public_health_from_reduced_air_pollution, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(acceptable_risk_energy__expected_value_dominant, public_health_from_reduced_air_pollution).

% Bear the chronic, diffuse, statistically routine mortality from ambient air pollution and combustion byproducts that the standard counts in full weight against fossil pathways. Their harm is real and counted, but its diffuseness means no single incident triggers remediation, compensation, or exit; they cannot relocate or change fuel mix on their own.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, coal_and_gas_frontline_communities, payer,
    powerless, biographical, trapped, regional).

% Die in mining accidents and from occupational exposure feeding both fossil and some mineral-intensive low-carbon supply chains. Counted in the fossil-pathway mortality tally when it applies, but their labor market offers no alternative income and no voice in how the standard is set.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, artisanal_mining_and_extraction_workers, payer,
    powerless, biographical, trapped, global).

% Bear the tail risk of low-probability, high-consequence accidents (contamination, displacement, multi-generational exclusion zones). Under this reading their expected harm is discounted by probability and averaged into a per-TWh figure that reads as favorable, even though realized harm at their specific site, if it occurs, is catastrophic and irreversible rather than diffuse.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, populations_near_potential_nuclear_accident_sites, payer,
    powerless, generational, constrained, regional).

% Argue that discounting catastrophic, irreversible, spatially concentrated harms by probability is a category error when consequences are non-ergodic (a community that loses its land once cannot be made whole by another community's averaged safety elsewhere). Their framing loses out in official standard-setting bodies that have adopted the expected-value metric as the comparative baseline.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_tail_dominant_reading_advocates, excluded,
    moderate, generational, constrained, national).

% Argue that deep uncertainty about future accident rates, waste outcomes, and technology change means the portfolio should preserve pathway diversity rather than optimize point-estimate expected harm now. Their preference for hedging against model error is not represented in a single-number mortality-per-TWh ranking.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, option_value_preserving_reading_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides regulators, utilities, and the public with one comparable number across energy sources so that licensing, siting, and portfolio decisions do not have to relitigate incommensurable harm types (chronic pollution vs. acute accident vs. occupational death) from scratch each time.
% TRANSFER_FUNCTION: Moves regulatory and public legitimacy toward nuclear and away from fossil generation by counting fossil's diffuse chronic mortality at full weight while counting nuclear's catastrophic-but-rare mortality at its low probability-weighted average; moves siting risk onto the specific, powerless communities near nuclear facilities who would bear a tail event that the aggregate metric renders statistically invisible at the national level.
% ABSENT_VOICES: Communities living near candidate nuclear sites are represented in the metric only as an averaged, probability-discounted quantity, not as parties whose specific, place-based catastrophic exposure gets weighed on its own terms. Advocates of catastrophic-tail-dominant and option-value-preserving readings are excluded from the standard-setting bodies that have already settled on the expected-value framing as official.
% DISAPPEARANCE_RATIONALE: If mortality-per-TWh expected-value ranking were abandoned as the official comparative standard, licensing and siting decisions would have to justify pathway choices on different grounds (tail-risk avoidance or option preservation), altering permitting timelines, insurance requirements, and the comparative legitimacy currently enjoyed by nuclear operators and grid planners who rely on the metric's ranking.
% FOUNDING_PROBLEM: Energy policy needed some way to compare wildly different harm profiles (chronic low-level pollution deaths vs. rare catastrophic accidents vs. occupational extraction deaths) on a common scale so that decisions were not made purely on availability heuristics or media salience of dramatic accidents.
% FOUNDING_PROBLEM_CORROBORATION: Public health epidemiologists outside the nuclear industry attest that fossil-fuel ambient mortality is real, large, and undercounted in public discourse relative to its statistical weight — corroborating the standard's original diagnostic function. Independent risk theorists and disaster-studies researchers, also outside the beneficiary set, attest that the founding problem has mutated: the standard is now used less to correct availability bias and more to foreclose tail-risk and option-value objections in siting fights, which is a different function than the one it was built for.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than extreme because the standard does perform genuine coordination work — it prevents purely availability-driven or media-driven energy siting decisions and it does correctly count large, real, previously undercounted fossil mortality. But it also has an asymmetric effect: probability discounting is mathematically appropriate for repeated, ergodic events but converts catastrophic, non-repeatable, geographically concentrated harm into a number that understates what a specific affected community would actually experience. Suppression is high (0.71, rising over the interval) because the standard's growing entrenchment in regulatory and licensing bodies increasingly forecloses procedural space for tail-risk or option-value arguments — those framings are treated as already-settled rather than live alternatives in most licensing venues.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators and grid planners derive as low-d beneficiaries: the metric's structure (probability-weighted averaging) mathematically favors low-probability/high-consequence pathways over the fossil chronic-harm profile, and they did not need to lobby for this — it falls out of the metric choice itself. Fossil-adjacent frontline communities and extraction workers derive as high-d targets: their harm is fully counted (so the standard is 'accurate' about them) but that accuracy is instrumentalized to delegitimize their pathway rather than to compensate or protect them, and they have no exit (trapped, powerless). Populations near nuclear sites are a distinct high-d case: their harm is counted honestly in expectation but the expectation itself is the mechanism that discounts their specific exposure — a directionality override is not needed here because the beneficiary/victim declarations already capture this correctly through the probability-discounting structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (comparing incommensurable harm types on a common scale to avoid availability-bias-driven policy) remains partly live — chronic fossil mortality genuinely needs a counterweight to media salience of rare accidents. But the standard's function has partially drifted: it is now also used as a closing move in siting disputes, foreclosing procedural consideration of tail-risk and option-value objections rather than merely correcting for cognitive bias. This is not full mandatrophy (the coordination function has not fully atrophied) but a partial capture — hence tangled_rope rather than snare or piton: there is a real coordination function (commensurable comparison) and a real asymmetric extraction (foreclosure of alternative risk framings that would protect specific frontline and siting-adjacent populations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ergodicity_of_energy_mortality_risk,
    'Is energy-pathway mortality risk ergodic (repeated trials converge to the expected value for any given community) or non-ergodic (a single catastrophic realization is irreversible and cannot be averaged away by other communities'' safety)?',
    'Formal decision-theoretic analysis of whether the relevant population for risk-averaging is the individual site/community (non-ergodic, tail-dominant framing more defensible) or the national/global energy system as a whole (ergodic, expected-value framing more defensible). This is the central technical dispute between this reading and catastrophic_tail_dominant.',
    'If risk is properly non-ergodic at the community level, this reading''s use of aggregate expected-value ranking to justify specific siting decisions is a category error that systematically undercounts what affected communities actually bear, strengthening the case for reclassifying this constraint as more extractive (closer to snare) toward nuclear-adjacent populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergodicity_of_energy_mortality_risk, conceptual, 'Whether the metric''s averaging is philosophically appropriate to the risk structure it is applied to.').

omega_variable(
    kernel_framing_choice_disclosure,
    'Which reading of the acceptable_risk_energy kernel governs official standard-setting, and is that choice made explicitly or does it enter regulatory practice as if it were the only reasonable interpretation of ''acceptable risk''?',
    'Textual and procedural analysis of regulatory rulemaking records: does the record acknowledge catastrophic_tail_dominant and option_value_preserving as rejected alternatives, or does it treat mortality-per-TWh ranking as simply what acceptable risk means, without argued selection among readings?',
    'If the expected-value reading was adopted without explicit argued rejection of the sibling readings, the suppression score authored here (0.71) may understate the degree to which alternative risk framings were foreclosed by default rather than by reasoned contest — this is the committer-structure ambiguity proper to a kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_disclosure, conceptual, 'Whether this reading''s dominance in practice reflects a contested choice or an unexamined default.').

omega_variable(
    fossil_mortality_causal_attribution,
    'How much of the ambient air pollution mortality attributed to the fossil pathway under this metric is causally attributable to combustion emissions specifically, versus co-occurring industrial and transportation sources that would persist even if the energy pathway changed?',
    'Epidemiological source-apportionment studies isolating power-generation combustion from other ambient pollution sources in the same populations.',
    'If a meaningful share of counted fossil-pathway mortality is misattributed, the full-weight counting of fossil harm this reading relies on may be somewhat inflated relative to nuclear''s probability-discounted figure, narrowing the gap the metric produces.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_mortality_causal_attribution, empirical, 'Measurement uncertainty in the mortality-per-TWh inputs for the fossil pathway.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.1).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__expected_value_dominant, theater_ratio, 8, 0.13).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__expected_value_dominant, theater_ratio, 16, 0.16).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__expected_value_dominant, theater_ratio, 24, 0.18).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_energy__expected_value_dominant, theater_ratio, 32, 0.2).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language 'acceptable risk in energy policy' concept per the eps-invariance principle. Each reading of the acceptable_risk_energy kernel is authored as a separate constraint with its own ehsilon, beneficiary/victim structure, and classification: expected_value_dominant (this story, tangled_rope, moderate ehsilon, favors nuclear/grid-planning beneficiaries via probability discounting), catastrophic_tail_dominant (prioritizes avoiding low-probability catastrophic outcomes, likely favors fossil-adjacent tail-risk-averse framings and disfavors nuclear siting), and option_value_preserving (maintains pathway diversity under deep uncertainty, likely favors incumbents across all pathways and disfavors any single-pathway lock-in). This reading's expected-value framing structurally influences the option_value reading by consuming the regulatory legitimacy and resource allocation that a diversity-preserving standard would otherwise claim, without logically foreclosing it. It coexists with (rather than forecloses) the catastrophic_tail_dominant reading because both remain live positions held by different regulatory and advocacy coalitions with no logical resolution between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
