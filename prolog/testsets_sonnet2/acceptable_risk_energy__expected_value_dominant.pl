% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable Risk Standard (Mortality-per-TWh)
 *   domain: risk assessment/energy policy/decision theory
 *
 * SUMMARY:
 *   This story authors ONE reading of a contested kernel — 'acceptable risk
 *   in energy policy' — the expected-value-dominant reading, which holds that
 *   the correct decision rule is minimization of aggregate expected mortality
 *   across all pathways using a commensurable mortality-per-TWh metric. Under
 *   this reading, fossil fuel deaths (air pollution, occupational mining
 *   harm) enter the victim ledger at full statistical weight because they are
 *   frequent and well-documented, while nuclear accident mortality is
 *   discounted by its low probability of occurrence. This produces real
 *   coordination value (a tractable, auditable comparative metric replacing
 *   availability-biased intuition) alongside real extraction (the discounting
 *   procedure structurally favors whichever pathway's harm profile is
 *   rare-and-catastrophic over whichever pathway's harm profile is
 *   common-and-diffuse, independent of whether that ordering tracks what
 *   affected populations would choose to bear). Sibling readings —
 *   catastrophic_tail_dominant and option_value_preserving — are NOT part of
 *   this constraint; they are separate constraint stories with their own ε,
 *   victim sets, and classifications, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - nuclear_industry_operators: institutional beneficiary of the discounting procedure
 *   - expected_value_risk_analysts: agenda-setters who author and maintain the metric
 *   - fossil_fuel_pollution_exposed_communities: full-weight victims under this reading, but geographically trapped and uncompensated
 *   - coal_mining_labor: occupational victims whose harm indicts an industry offering them no funded transition
 *   - populations_near_undisclosed_tail_risk_sites: bear probability-discounted catastrophic exposure with no personal averaging available
 *   - catastrophic_risk_advocates and option_value_preservation_advocates: excluded from the reading's own methodological frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.58).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.71).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable Risk Standard (Mortality-per-TWh)").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk assessment/energy policy/decision theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, '6f12d405-4755-43ae-a1c9-0e2d45ba2769').
narrative_ontology:cs_kernel_codification('6f12d405-4755-43ae-a1c9-0e2d45ba2769', formalized).
narrative_ontology:cs_authority_grounding('6f12d405-4755-43ae-a1c9-0e2d45ba2769', expertise).
narrative_ontology:cs_interpretation_layer_present('6f12d405-4755-43ae-a1c9-0e2d45ba2769').
narrative_ontology:cs_reading_relation('6f12d405-4755-43ae-a1c9-0e2d45ba2769', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('6f12d405-4755-43ae-a1c9-0e2d45ba2769', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('6f12d405-4755-43ae-a1c9-0e2d45ba2769', foundational, aggregate_expected_harm_is_the_correct_optimand).
narrative_ontology:cs_axiom_status(aggregate_expected_harm_is_the_correct_optimand, holdable).
narrative_ontology:cs_axiom_grounding('6f12d405-4755-43ae-a1c9-0e2d45ba2769', aggregate_expected_harm_is_the_correct_optimand, instrumental).
narrative_ontology:cs_axiom('6f12d405-4755-43ae-a1c9-0e2d45ba2769', foundational, probability_weighting_of_catastrophic_outcomes_is_normatively_sufficient).
narrative_ontology:cs_axiom_status(probability_weighting_of_catastrophic_outcomes_is_normatively_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('6f12d405-4755-43ae-a1c9-0e2d45ba2769', probability_weighting_of_catastrophic_outcomes_is_normatively_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('6f12d405-4755-43ae-a1c9-0e2d45ba2769', availability_bias_corrected_comparative_risk_assessment).
narrative_ontology:cs_drift_state('6f12d405-4755-43ae-a1c9-0e2d45ba2769', post_fukushima_post_climate_consensus_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6f12d405-4755-43ae-a1c9-0e2d45ba2769', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, expected_value_risk_analysts).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, grid_planners_seeking_tractable_metrics).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_pollution_exposed_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, coal_mining_labor).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, populations_near_undisclosed_tail_risk_sites).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, aggregate_expected_harm_minimization_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__expected_value_dominant, mortality_per_twh_commensurability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cite mortality-per-TWh comparisons to argue nuclear power is statistically among the safest energy sources once accident probability is weighted against fossil fuel's continuous, dispersed mortality toll. The standard is the primary public-facing defense against siting and licensing opposition, and shapes regulatory review timelines and insurance requirements in the operators' favor.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Author and maintain the mortality-per-TWh methodology used across national energy agencies and international bodies. They select the discounting model for low-probability, high-consequence events and set the aggregation window; their institutional standing and funding depend on the model remaining the dominant comparative framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, expected_value_risk_analysts, agenda_setter,
    institutional, civilizational, analytical, global).

% Bear continuous, diffuse mortality from particulate exposure near coal and gas infrastructure. Under this reading their deaths enter the ledger at full statistical weight, which correctly makes fossil pathways look worse in aggregate expected-harm terms — but the same aggregation obscures that their harm is concentrated, involuntary, and geographically fixed, unlike the diversified exposure implied by a per-TWh average. They cannot relocate the plant or the mine, and the metric that indicts fossil fuel does nothing to compensate or relocate them.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_pollution_exposed_communities, payer,
    powerless, biographical, trapped, regional).

% Occupational mortality (black lung, mining accidents) is counted in the fossil pathway's per-TWh mortality figure, reinforcing the case against continued fossil extraction. Workers have some job mobility but are economically tied to the industry the metric is used to phase out, with no parallel mechanism in the standard itself to fund their transition.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, coal_mining_labor, payer,
    powerless, biographical, constrained, regional).

% Live near facilities where a low-probability, high-consequence nuclear event has been probability-discounted nearly to zero in the aggregate mortality calculus, even though a realized event would concentrate catastrophic, multi-generational harm on them specifically. The expected-value framework treats their tail exposure as adequately priced by its low frequency; they experience it as an undiversifiable, place-bound risk with no averaging available to them personally.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, populations_near_undisclosed_tail_risk_sites, payer,
    powerless, generational, trapped, regional).

% Use the single commensurable mortality-per-TWh number to compare dissimilar energy pathways in procurement and permitting decisions, avoiding harder multi-criteria or distributional analysis. Their planning workflow is built around the metric's tractability, which they benefit from even when they privately recognize its distributional blind spots.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, grid_planners_seeking_tractable_metrics, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, grid_planners_seeking_tractable_metrics, agenda_setter).

% Argue that expected-value aggregation is the wrong decision rule for irreversible, catastrophic, or civilization-scale tail events, and that probability-discounting a Chernobyl- or Fukushima-scale event mischaracterizes what is actually at stake for exposed populations. Their objections are treated as methodologically out of scope by the standard's maintainers rather than engaged on the merits within this reading's framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, catastrophic_risk_advocates, excluded,
    moderate, civilizational, constrained, global).

% Argue that under deep uncertainty about future costs, technology, and climate outcomes, the correct posture is to preserve multiple energy pathways rather than optimize a single expected-value point estimate. Their framing is largely absent from the mortality-per-TWh literature, which treats pathway selection as a solved optimization rather than an open portfolio problem.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, option_value_preservation_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, nuclear_industry_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single commensurable metric that lets regulators, planners, and the public compare mortality risk across structurally dissimilar energy pathways (continuous low-level pollution vs. rare catastrophic accidents) using one number, enabling tractable policy comparison instead of incommensurable qualitative debate.
% TRANSFER_FUNCTION: Moves regulatory and reputational burden onto fossil fuel pathways (by counting their diffuse, continuous mortality at full weight) and moves regulatory and reputational burden away from nuclear pathways (by discounting catastrophic-but-rare mortality by its low probability), shifting siting, permitting, and investment favorability toward nuclear operators and away from communities bearing either pathway's realized harm.
% ABSENT_VOICES: Catastrophic-tail-dominant advocates and option-value-preservation advocates are structurally outside this reading's own methodology — their objections concern the choice of decision rule itself, which this reading treats as settled rather than as a live policy choice requiring their input.
% DISAPPEARANCE_RATIONALE: If the expected-value-dominant standard vanished, nuclear licensing and siting arguments would lose their primary comparative defense, fossil fuel phase-out arguments would lose their strongest single-number indictment, and energy policy debates would likely fragment into pathway-specific, harder-to-compare distributional and precautionary arguments — grid planning and regulatory review processes built around the single metric would need new comparative frameworks.
% FOUNDING_PROBLEM: Energy policy needed a way to compare mortality risk across pathways with radically different harm profiles (chronic diffuse pollution vs. rare catastrophic accidents) so that regulators and investors could make risk-informed comparative decisions instead of relying on availability-bias-driven public perception (nuclear accidents are vivid and rare; fossil pollution deaths are invisible and continuous).
% FOUNDING_PROBLEM_CORROBORATION: Nuclear industry operators and the risk-analyst community that authored the methodology attest the founding problem remains live and the standard correctly corrects for availability bias. Independent voices outside the beneficiary set — public health researchers documenting undercounted fossil externalities, and separately, catastrophic-risk scholars (e.g. in the nuclear safety and existential risk literatures) — attest the standard has calcified into a tool that launders distributional and irreversibility concerns out of the policy conversation, rather than resolving them.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects that the metric does real coordination work (commensurable comparison replacing incoherent debate) while also systematically transferring reputational and regulatory burden along a probability-vs-frequency axis that has no independent normative justification within the reading itself — it is a modeling choice, not a discovered fact, yet it is deployed as if it were the latter. Suppression (0.71) is high because the standard's institutional dominance actively forecloses methodological alternatives (tail-weighted or option-value frameworks) from entering mainstream regulatory comparison; this is suppression of an alternative decision RULE, not merely an alternative energy source. Theater ratio is moderate-low (0.28) and rising: the metric performs increasingly more legitimating than analytical work as its critics accumulate without dislodging its institutional position. accessibility_collapse (0.62) reflects that once a jurisdiction adopts mortality-per-TWh as its comparative standard, switching frameworks requires re-litigating the entire risk-assessment methodology, which raises the practical bar for competing frameworks. resistance (0.6) is substantial and comes specifically from catastrophic-risk and option-value scholarship, not from the general public, who mostly do not see the framework choice as contestable at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators and the analysts who maintain the methodology sit near the beneficiary end: the discounting procedure is the primary defense of their licensing and siting position, and they have institutional and analytical mobility (arbitrage/analytical exit). Fossil-exposed communities and mining labor sit near the target end: they are named as victims under this reading's own accounting (their deaths count fully), yet the metric produces no remedy or transfer to them — it merely indicts the industry they are economically bound to, without funding their exit. Populations near tail-risk nuclear sites are a distinct victim class: their exposure is real but is precisely the category this reading discounts, so they experience the standard's classification of THEM as low-risk even though a realized event would be catastrophic and place-bound for them specifically — this is a directionality inversion worth flagging (they are structurally treated as low-d/beneficiary-adjacent by the model, while their lived exposure argues for high-d/target).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (correcting for availability bias when comparing diffuse chronic harm to rare catastrophic harm) was genuine and arguably remains live in narrow analytical terms. But the standard has been extended past that founding function: it now operates as a closed decision rule that forecloses methodological alternatives from regulatory consideration, rather than as one input among several. This is the seat divergence the mandatrophy check exists to catch — the analysts who authored the tool experience it as a live, functioning correction; the populations whose catastrophic exposure it discounts experience it as a settled verdict against their standing to object.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decision_rule_choice_is_itself_contested,
    'Is the choice of expected-value aggregation as the governing decision rule for energy mortality risk a neutral methodological fact, or itself a contested normative choice that structurally favors certain pathways?',
    'Compare regulatory outcomes and siting decisions across jurisdictions that use expected-value-dominant metrics versus those using explicit tail-risk-weighted or option-value frameworks (e.g., precautionary-principle jurisdictions); document whether pathway rankings diverge.',
    'If rankings diverge substantially by decision-rule choice alone, this reading''s classification as ''coordination'' overstates its neutrality — the rule selection is doing normative work that its own methodology does not surface for public deliberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decision_rule_choice_is_itself_contested, conceptual, 'Whether expected-value dominance is a neutral aggregation choice or a contested normative commitment.').

omega_variable(
    tail_risk_population_consent,
    'Did populations living near nuclear tail-risk sites consent to having their catastrophic exposure probability-discounted in the governing risk standard, or was this decision made by analysts and regulators without their structural input?',
    'Review siting and licensing consultation records for whether affected populations were presented with the discounting methodology and its implications, versus only summary risk figures.',
    'If consent was not meaningfully obtained, the discounting operation looks more like imposed extraction (their catastrophic exposure priced without their input) than like coordinated risk-sharing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_population_consent, empirical, 'Whether tail-risk-exposed populations had structural input into the discounting methodology applied to their exposure.').

omega_variable(
    sibling_reading_resource_competition,
    'Does the institutional dominance of the expected-value-dominant reading actively suppress funding and regulatory attention for catastrophic-tail-dominant and option-value-preserving frameworks, or do all three coexist as live methodological options within current energy policy institutions?',
    'Survey regulatory agency methodology guidance documents and academic funding allocations across the three reading types over the past two decades.',
    'If expected-value-dominant crowds out the alternatives institutionally, the suppression metric authored here understates the reading''s structural effect on the broader kernel contest; if the alternatives are well-funded and actively debated, suppression is more localized to specific regulatory contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_resource_competition, empirical, 'Degree of institutional crowding-out between the three sibling readings of the acceptable-risk-energy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.12).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__expected_value_dominant, theater_ratio, 8, 0.15).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__expected_value_dominant, theater_ratio, 16, 0.18).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__expected_value_dominant, theater_ratio, 24, 0.22).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_energy__expected_value_dominant, theater_ratio, 32, 0.25).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__expected_value_dominant, 0.15).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'acceptable risk in energy policy' per the ε-invariance principle. Each sibling reading (expected_value_dominant, catastrophic_tail_dominant, option_value_preserving) instantiates a structurally distinct decision rule with its own victim set and its own ε — they are not the same constraint measured three ways. This file authors expected_value_dominant only; the committer structure (which reading, what the siblings would change, where the disagreement is located) is routed to the omegas array and commentary.kernel_context rather than folded into this constraint's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
