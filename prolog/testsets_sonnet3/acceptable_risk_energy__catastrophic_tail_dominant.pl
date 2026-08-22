% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Catastrophic-Tail-Dominant Reading of Acceptable Energy Risk
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This story instantiates the catastrophic-tail-dominant reading of the
 *   acceptable-risk-in-energy kernel: the standard that governs energy
 *   infrastructure risk should be judged by its worst plausible single event,
 *   not by its aggregate expected harm across the population it governs.
 *   Under this reading, a low-probability, high-magnitude, irreversible
 *   nuclear accident scenario dominates the risk calculus even when the
 *   fossil-fuel status quo it displaces produces far higher aggregate
 *   mortality through chronic air pollution and climate load. This is
 *   deliberately NOT the expected_value_dominant reading (which would treat
 *   mortality-per-TWh as decisive and reverse most of this story's
 *   beneficiary/victim structure) nor the option_value_preserving reading
 *   (which treats the standard's function as keeping multiple pathways open
 *   rather than definitively suppressing one). Those are separate
 *   constraints, sibling files in the same kernel family, linked here only
 *   through cs_structure.reading_relations and network.affects_constraints —
 *   their ε values and stakeholder sets are not authored in this file.
 *
 * KEY AGENTS:
 *   - incumbent_fossil_fuel_generators: institutional beneficiary of nuclear's regulatory disadvantage
 *   - anti_nuclear_advocacy_organizations: agenda-setters whose institutional relevance depends on sustained catastrophic-risk salience
 *   - populations_exposed_to_fossil_fuel_air_pollution: powerless, trapped payers of the discounted chronic harm
 *   - future_generations_facing_climate_load: powerless, trapped, civilizational-horizon payers with no seat in current standard-setting
 *   - nuclear_regulatory_agencies: institutional agenda-setters administering the tail-avoidance licensing standard
 *   - risk_decision_theorists: analytical observers of the dread-risk psychological asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.62).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Reading of Acceptable Energy Risk").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, '493f97af-8447-46aa-ae2d-124e828c81e4').
narrative_ontology:cs_kernel_codification('493f97af-8447-46aa-ae2d-124e828c81e4', distributed).
narrative_ontology:cs_authority_grounding('493f97af-8447-46aa-ae2d-124e828c81e4', distributed).
narrative_ontology:cs_reading_relation('493f97af-8447-46aa-ae2d-124e828c81e4', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('493f97af-8447-46aa-ae2d-124e828c81e4', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('493f97af-8447-46aa-ae2d-124e828c81e4', foundational, catastrophic_irreversible_harm_has_lexical_priority).
narrative_ontology:cs_axiom_status(catastrophic_irreversible_harm_has_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('493f97af-8447-46aa-ae2d-124e828c81e4', catastrophic_irreversible_harm_has_lexical_priority, deontological).
narrative_ontology:cs_axiom('493f97af-8447-46aa-ae2d-124e828c81e4', secondary, attributable_singular_events_warrant_asymmetric_regulatory_weight).
narrative_ontology:cs_axiom_status(attributable_singular_events_warrant_asymmetric_regulatory_weight, holdable).
narrative_ontology:cs_axiom_grounding('493f97af-8447-46aa-ae2d-124e828c81e4', attributable_singular_events_warrant_asymmetric_regulatory_weight, conventional).
narrative_ontology:cs_reference_frame('493f97af-8447-46aa-ae2d-124e828c81e4', post_severe_accident_precautionary_licensing_regime).
narrative_ontology:cs_drift_state('493f97af-8447-46aa-ae2d-124e828c81e4', contemporary_climate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('493f97af-8447-46aa-ae2d-124e828c81e4', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, incumbent_fossil_fuel_generators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, coal_and_gas_dependent_utility_shareholders).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, populations_exposed_to_fossil_fuel_air_pollution).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_facing_climate_load).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_workers_and_suppliers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, energy_poor_households_paying_higher_prices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Continue operating and building gas and coal capacity while nuclear licensing, siting, and financing face regulatory regimes calibrated to the catastrophic-tail standard. Do not need to defend their own mortality and morbidity footprint in the same terms, since those harms are distributed and chronic rather than acute and singular. Benefit competitively every year a nuclear plant is delayed or cancelled under this standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, incumbent_fossil_fuel_generators, beneficiary,
    institutional, generational, arbitrage, national).

% Set the political and legal agenda that codifies catastrophic-tail dominance into siting law, insurance liability caps, and licensing review timelines. Draw organizational legitimacy, funding, and membership from continued nuclear risk salience; a resolved or de-escalated nuclear risk narrative would reduce their institutional relevance.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, beneficiary).

% Bear ongoing respiratory and cardiovascular mortality from particulate and combustion emissions that a mortality-per-TWh accounting would count as decisive, but which are individually undramatic, distributed across a population, and never trigger the acute regulatory response a nuclear incident would. Cannot relocate away from grid siting decisions and have no seat in the risk-standard-setting process.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, populations_exposed_to_fossil_fuel_air_pollution, payer,
    powerless, biographical, trapped, regional).

% Inherit the cumulative atmospheric loading produced by decades of fossil generation kept online in part because nuclear alternatives were suppressed under the catastrophic-tail standard. Have no representation in current risk-tolerance decisions and cannot exit a climate trajectory already set in motion.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_facing_climate_load, payer,
    powerless, civilizational, trapped, global).

% Face a licensing and financing environment shaped by catastrophic-tail risk aversion that imposes cost and delay disproportionate to the mortality-per-TWh record of modern reactor designs. Career and capital investment depend on a pathway the standard actively narrows; exit means leaving the sector rather than fixing the standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_workers_and_suppliers, payer,
    moderate, biographical, constrained, national).

% Pay higher electricity prices where the catastrophic-tail standard forecloses the cheapest low-carbon baseload option and forces reliance on more expensive or more volatile-priced fossil and intermittent-renewable-plus-storage combinations. Have no capacity to influence the risk standard that shapes their utility bill.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_poor_households_paying_higher_prices, payer,
    powerless, immediate, trapped, regional).

% Administer licensing regimes calibrated to catastrophic, low-probability tail events (core damage frequency thresholds, worst-case release scenarios) rather than to comparative mortality-per-TWh across the whole energy system. Enforce the standard through review timelines, siting vetoes, and liability regimes; could recalibrate to an expected-harm standard but institutional mandate and public accountability structures push toward tail-avoidance.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Produce mortality-per-TWh and disability-adjusted life year comparisons showing fossil generation vastly outweighs nuclear in aggregate harm, but their comparative-mortality framing rarely enters the legal and political processes that set acceptable-risk standards, which remain organized around catastrophic-event salience rather than aggregate epidemiological accounting.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, climate_and_public_health_epidemiologists, excluded,
    organized, generational, analytical, global).

% Study why catastrophic, involuntary, dread-inducing, low-probability risks are weighted far more heavily in public risk perception and policy than chronic, voluntary, familiar risks of equal or greater expected harm, and can trace how this psychological asymmetry becomes institutionalized into formal acceptable-risk standards.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_decision_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legally operationalizable standard for what counts as an acceptable risk from energy infrastructure, allowing regulators, courts, and insurers to coordinate on licensing, siting, and liability without re-litigating risk tolerance case by case. The genuine coordination problem it solves is giving diverse institutional actors a common decision rule for catastrophic, irreversible, or hard-to-reverse harms.
% TRANSFER_FUNCTION: Moves regulatory burden, financing cost, and market share away from nuclear generation and toward fossil generation, while moving the realized health and climate burden away from acute, attributable nuclear-incident harm and onto the diffuse, chronic, harder-to-attribute victims of fossil combustion and long-run climate load.
% ABSENT_VOICES: Public health epidemiologists and comparative-mortality risk analysts who would argue the standard systematically undercounts aggregate harm are rarely direct parties to nuclear siting and licensing proceedings, which are structured around acute-incident risk rather than aggregate epidemiological comparison. Future generations bearing climate load have no seat at all.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant standard were replaced overnight by an expected-aggregate-harm standard, nuclear licensing timelines and siting vetoes calibrated to worst-case release scenarios would loosen substantially, fossil generation would lose its comparative regulatory advantage, and the mortality and price burdens currently borne by fossil-exposed and energy-poor populations would shift toward whatever new distribution the expected-value standard produced.
% FOUNDING_PROBLEM: Built to prevent regulators and courts from licensing infrastructure whose failure mode is a single catastrophic, irreversible, geographically concentrated, and potentially uncontrollable event — a category of harm that ordinary cost-benefit or expected-value accounting was seen as poorly equipped to price, given ambiguity about probability estimation and the moral asymmetry between chronic diffuse harm and acute catastrophic harm.
% FOUNDING_PROBLEM_CORROBORATION: Decision theorists working outside both the nuclear and fossil-fuel-adjacent advocacy communities corroborate that catastrophic, dread, involuntary risks are genuinely perceived and weighted differently by publics and institutions than chronic risks of equal or greater expected harm — the underlying psychological asymmetry is real and independently documented. But public health epidemiologists, also outside the benefiting parties, corroborate that this asymmetry has hardened into a standard that produces higher aggregate harm than the alternative it displaced, meaning the founding problem (irrational panic-driven licensing) has been addressed by machinery that now itself causes the harm it was meant to prevent.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) and suppression (0.78) are authored high but not maximal: the standard has a genuine coordination function (giving regulators a workable, legally defensible decision rule for irreversible catastrophic harm) which keeps it out of pure-snare territory, but the asymmetric weighting of acute nuclear harm against chronic fossil harm produces a real, growing transfer that this reading treats as extractive from the aggregate-harm-bearing populations. Theater ratio (0.40) reflects that a substantial share of the licensing and siting apparatus performs risk-aversion ritual (worst-case scenario modeling, defense-in-depth review layers) beyond what the tail-avoidance function strictly requires, while some of it remains functionally real. Accessibility collapse is moderate (0.5): alternative risk standards are known, documented, and actively argued for by epidemiologists and decision theorists — they have not disappeared from view, they have been institutionally sidelined. Resistance (0.55) is moderate-to-high, reflecting the growing epidemiological and climate-economics literature pushing back against tail-dominance.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent fossil generators and anti-nuclear advocacy organizations sit near the beneficiary end: the former captures market share and avoided regulatory burden, the latter captures institutional relevance and political capital, neither administers the harm they benefit from bearing. Fossil-pollution-exposed populations, future generations, and energy-poor households sit near the full-target end: trapped or near-trapped exit, no voice in standard-setting, and the standard's asymmetric weighting is precisely what discounts their harm relative to the harm it prioritizes avoiding. Nuclear workers and suppliers are moderate targets — constrained rather than trapped, since they can exit the sector, but the standard directly narrows their pathway. Regulatory agencies are agenda-setters whose institutional exit is constrained by their own mandate structure, not by capture in the corrupt sense — they administer a standard shaped by upstream political and legal commitments they did not unilaterally choose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing catastrophic, irreversible, singular-event harm from being licensed under ordinary cost-benefit reasoning that might mis-price low-probability tail risk — was and remains a genuinely live concern in the abstract (decision theorists corroborate the underlying psychological and structural asymmetry between dread risks and chronic risks is real). What has drifted is the standard's calibration: it has hardened into a mechanism that, by this reading's own metrics, now produces higher aggregate harm than the fossil-fuel-dominant status quo it protects, while the institutions most invested in maintaining tail-dominance (anti-nuclear advocacy groups, fossil incumbents) are not the institutions bearing that aggregate harm. This is the tangled_rope signature: real coordination function (a workable catastrophic-risk decision rule) plus asymmetric extraction (chronic-harm populations pay for acute-harm avoidance) sustained by active enforcement (licensing review, siting law, liability caps) rather than by the coordination benefit alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_risk_lexical_priority_vs_aggregate_harm,
    'Is lexical priority for catastrophic, irreversible tail risk over aggregate expected harm a defensible normative commitment (grounded in irreversibility and non-fungibility of catastrophic loss), or is it a cognitive bias (dread risk, availability heuristic) that has been institutionalized into policy without independent normative justification?',
    'Comparative institutional analysis: examine whether other domains with comparable catastrophic-but-rare failure modes (dam failures, pandemic risk, aviation) apply the same lexical tail-priority, and whether outcomes under tail-priority regimes versus expected-value regimes show measurably different long-run aggregate harm trajectories.',
    'If lexical tail-priority is normatively defensible independent of dread-risk psychology, this reading is closer to a genuine (if costly) coordination mechanism (rope/scaffold-leaning). If it is primarily an institutionalized cognitive bias with no independent justification, the coordination story is weaker and the constraint reads closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_lexical_priority_vs_aggregate_harm, conceptual, 'Whether tail-risk lexical priority is normatively grounded or an institutionalized cognitive bias.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice among catastrophic_tail_dominant, expected_value_dominant, and option_value_preserving readings itself a value-laden policy choice with no unique correct answer, or does one reading dominate the others once irreversibility, attribution asymmetry, and distributional equity are all properly weighted?',
    'Formal decision-theoretic reconciliation attempting to nest all three readings under a single social welfare function with explicit risk-aversion and equity parameters; if no such reconciliation converges, the readings are genuinely incommensurable rather than resolvable by better analysis.',
    'If one reading dominates once properly weighted, this story''s ε and beneficiary/victim structure should be understood as provisional pending that resolution. If genuinely incommensurable, all three readings persist as legitimate, structurally distinct constraints indefinitely, each with its own stable ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, preference, 'Whether the three kernel readings are ultimately reconcilable or genuinely incommensurable value choices.').

omega_variable(
    attribution_asymmetry_as_cover_or_reality,
    'Is the discounting of fossil-fuel mortality as ''distributed and reversible'' a legitimate epidemiological distinction (individual deaths are not centrally caused by a single identifiable failure, and public health interventions can partially reverse trend lines) or is it a rhetorical mechanism that launders aggregate harm out of the acceptable-risk calculus precisely because it lacks a single attributable catastrophic event?',
    'Epidemiological and legal-causation analysis of whether chronic pollution mortality is treated with equivalent regulatory seriousness to acute-event mortality in comparable domains (e.g., industrial chemical exposure standards vs. explosion risk standards) to test whether the distributed/reversible framing is applied consistently or selectively to advantage fossil incumbents.',
    'If the distinction is applied inconsistently to advantage fossil incumbents specifically, the beneficiary declaration for incumbent_fossil_fuel_generators strengthens considerably; if applied consistently across domains, the standard''s asymmetry is less obviously extractive and more a genuine, if contestable, feature of how the standard-setting apparatus treats causation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_asymmetry_as_cover_or_reality, empirical, 'Whether distributed/reversible framing of fossil harm is a consistent epidemiological standard or a selective rhetorical discount.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.25).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.28).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.31).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.34).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.37).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 50, 0.39).
narrative_ontology:measurement(acce_tr_t60, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(acce_be_t60, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 50, 0.76).
narrative_ontology:measurement(acce_su_t60, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the acceptable_risk_energy kernel, each authored as a separate constraint with its own stable ε and stakeholder structure per the ε-invariance principle. catastrophic_tail_dominant (this file) authors nuclear accidents as infinite-weight victim-generating events and discounts distributed fossil harm, yielding tangled_rope with fossil incumbents and anti-nuclear advocacy organizations as beneficiaries. expected_value_dominant would author mortality-per-TWh as decisive, largely reversing the beneficiary/victim assignment. option_value_preserving would author the standard's function as flexibility-preservation under deep uncertainty rather than as a decisive verdict against nuclear, producing a different (likely rope-leaning) classification. The three are linked bidirectionally via affects_constraints; no attempt is made here to reconcile or average their ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
