% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative-Risk Standard for Nuclear Acceptability (vs. Fossil Alternatives)
 *   domain: energy policy/risk governance/environmental justice
 *
 * SUMMARY:
 *   This story instantiates the comparative_risk_dominant reading of the
 *   acceptable-risk-for-energy kernel: nuclear risk is treated as acceptable
 *   not against any absolute threshold but relative to the documented harms
 *   of the fossil-fuel alternative (coal mortality, climate catastrophe).
 *   Under this reading, nuclear acceptability is contingent and comparative —
 *   a reactor's residual hazard is licensed and defended by pointing at coal
 *   deaths and climate damage, not by clearing an independent bar. This
 *   produces a genuine coordination function (a workable, communicable
 *   decision rule where none absolute exists) bundled with an asymmetric
 *   transfer: concentrated local and intergenerational costs are weighed
 *   against diffuse global benefits captured by parties who bear none of the
 *   local risk. The sibling readings — catastrophic_tail_dominant (which
 *   would foreground irreversibility and intergenerational burden as trumping
 *   any comparative case) and expected_value_dominant (which would settle
 *   acceptability via probability-weighted annual cost-benefit, indifferent
 *   to the fossil comparison specifically) — are NOT part of this constraint;
 *   they are separate constraints in the same kernel family, linked
 *   structurally but authored independently with their own ε and stakeholder
 *   sets.
 *
 * KEY AGENTS:
 *   - nuclear_utility_operators: institutional beneficiary/agenda_setter — retains license and operating legitimacy via comparative framing
 *   - nuclear_host_communities: powerless, trapped payer — bears concentrated local hazard weighed against diffuse global comparator
 *   - future_generations_waste_stewards: civilizational-horizon payer with no present voice — inherits waste custody generated to resolve today's tradeoff
 *   - climate_vulnerable_populations_excluded_from_siting: excluded — invoked as the moral stakes of the comparison but absent from siting and governance decisions
 *   - nuclear_regulators: institutional agenda_setter — administers the comparative standard and could replace it with an absolute or precautionary one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.42).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.38).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative-Risk Standard for Nuclear Acceptability (vs. Fossil Alternatives)").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "energy policy/risk governance/environmental justice").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, 'ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea').
narrative_ontology:cs_kernel_codification('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', distributed).
narrative_ontology:cs_authority_grounding('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', distributed).
narrative_ontology:cs_reading_relation('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', foundational, acceptability_is_inherently_relative_to_alternatives).
narrative_ontology:cs_axiom_status(acceptability_is_inherently_relative_to_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', acceptability_is_inherently_relative_to_alternatives, instrumental).
narrative_ontology:cs_axiom('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', foundational, climate_urgency_overrides_intergenerational_waste_discounting_concern).
narrative_ontology:cs_axiom_status(climate_urgency_overrides_intergenerational_waste_discounting_concern, holdable).
narrative_ontology:cs_axiom_grounding('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', climate_urgency_overrides_intergenerational_waste_discounting_concern, empirically_contingent).
narrative_ontology:cs_reference_frame('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', post_fukushima_risk_informed_regulation).
narrative_ontology:cs_drift_state('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', contemporary_climate_urgency_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ff506ddc-cbfc-46d6-b4a1-decc17d7f8ea', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_utility_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_policy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, grid_reliability_planners).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_stewards).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations_excluded_from_siting).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, coal_dependent_communities).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__comparative_risk_dominant, net_risk_reduction_justifies_residual_hazard).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__comparative_risk_dominant, climate_catastrophe_as_baseline_comparator).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate and seek license renewal or new-build approval for reactors, using the comparative-risk framing (nuclear vs. coal deaths, nuclear vs. climate damage) to secure regulatory and public acceptance without having to clear an absolute safety threshold. Benefit directly from continued operation and from a standard that never forces them to answer 'is this safe enough on its own terms.'
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_utility_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_utility_operators, agenda_setter).

% Advocate for nuclear retention or expansion as a low-carbon baseload source, deploying the comparative frame to override intergenerational waste and local-hazard objections. Do not bear siting risk personally; benefit from the emissions-avoidance case regardless of where the reactors are sited or who stores the waste.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_policy_advocates, beneficiary,
    organized, generational, mobile, global).

% Need dispatchable, low-carbon capacity to balance intermittent renewables; the comparative standard lets them retain nuclear plants in the resource mix without resolving standalone risk tolerance. Their planning horizon is annual reliability, not multi-generational waste custody.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, grid_reliability_planners, beneficiary,
    institutional, biographical, constrained, national).

% Live within the accident and contamination radius of existing or proposed reactors and near-site waste storage. The comparative standard means their local, concrete, place-bound risk is weighed against a diffuse global counterfactual (coal mortality, climate damage) they did not choose and cannot renegotiate. Relocation is economically and socially costly; regulatory processes rarely let them argue for an absolute local threshold independent of the fossil comparison.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_host_communities, payer,
    powerless, generational, trapped, local).

% Will inherit spent fuel and decommissioning obligations spanning millennia, generated to satisfy today's comparative-risk tradeoff against coal and climate harms that are urgent now but whose beneficiaries cannot be consulted. They have no seat in current licensing proceedings and no mechanism to object to a risk transfer made on their behalf before they exist.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_stewards, payer,
    powerless, civilizational, trapped, national).

% Are invoked as the moral justification for accepting nuclear risk (their exposure to climate catastrophe is the comparator that makes nuclear 'acceptable') but have no role in reactor siting, waste governance, or the terms of the comparison. Low-lying and drought-exposed populations abroad bear the climate risk cited to justify domestic nuclear siting decisions made without their participation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations_excluded_from_siting, excluded,
    powerless, generational, trapped, global).

% Administer licensing standards and formally adopt comparative-risk methodology (risk-informed regulation, societal risk curves benchmarked against other energy sources) rather than an absolute per-reactor threshold. Control whether the standard could be replaced with an absolute-threshold or precautionary framework, and bear reputational but not physical cost if the comparison is later judged to have understated local or intergenerational harm.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Bear the actual coal-emissions mortality and morbidity that the comparative standard uses as its justifying baseline. Benefit if nuclear displacement of coal reduces their pollution burden, but have no direct stake in how nuclear-specific siting or waste risk is distributed elsewhere; their suffering under the status quo is the argument's fuel, not something the standard directly manages.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, coal_dependent_communities, observer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, coal_dependent_communities, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__comparative_risk_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable decision rule for licensing and public communication that avoids paralysis: since zero-risk energy does not exist, regulators and advocates need some basis for saying nuclear risk is 'acceptable,' and comparing it to the well-documented, ongoing harms of fossil generation gives a tractable, communicable standard instead of an undefined absolute bar.
% TRANSFER_FUNCTION: Moves acceptance of concentrated, place-bound, long-duration nuclear hazard (accident risk, waste custody) onto host communities and future generations, in exchange for diffuse, globally-distributed climate and pollution benefit captured by climate advocates, grid planners, and populations far from reactor sites who never had to accept siting risk themselves.
% ABSENT_VOICES: Nuclear host communities rarely get a forum where their objection can be evaluated on its own local terms rather than being weighed against a global counterfactual; future waste stewards have no seat in any proceeding since they do not yet exist; climate-vulnerable populations abroad are cited as the moral stakes of the comparison but are not parties to domestic siting or waste-governance decisions.
% DISAPPEARANCE_RATIONALE: If the comparative standard vanished and regulators were forced to justify nuclear risk against an absolute (not relative) threshold, licensing proceedings, plant retirements, and new-build decisions would change substantially — some jurisdictions would tighten standards past what current reactors meet, others would need new precautionary or absolute-risk legislation, and the current justification for continued operation and expansion would need to be rebuilt from different premises.
% FOUNDING_PROBLEM: Following major reactor accidents and the absence of any defensible zero-risk energy technology, regulators needed a way to communicate that some level of nuclear risk was tolerable without claiming it was risk-free, and comparative framing against known fossil-fuel harms filled that gap.
% FOUNDING_PROBLEM_CORROBORATION: Independent risk-assessment scholars and international nuclear safety bodies (e.g. IAEA safety-goal literature) attest that no absolute-zero standard exists for any energy source, corroborating that a comparative baseline is genuinely necessary rather than a pure invention of industry; however, environmental-justice researchers studying siting patterns near host communities attest that the comparison is applied asymmetrically, weighing diffuse global benefit against concentrated local cost without a parallel mechanism for local veto or compensation calibrated to that asymmetry.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).
:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the standard is not primarily a rent-extraction device but does perform a real transfer — it lets beneficiaries avoid ever resolving standalone nuclear risk tolerance by permanently anchoring acceptability to a moving fossil-fuel comparator. Suppression (0.38) reflects that host communities and future stewards are structurally excluded from renegotiating the comparison's terms, though this operates through procedural framing rather than overt coercion. Theater ratio (0.30) captures that some of the comparative risk communication (annual mortality comparisons, life-years saved) functions as genuine technical analysis and some functions as a rhetorical closing move that forecloses the absolute-threshold question before it is asked. Accessibility collapse (0.45) is moderate — the comparative frame is dominant in policy discourse but has not fully foreclosed precautionary or absolute-threshold arguments, which persist in some jurisdictions and among some regulators. Resistance (0.55) is substantial: host-community opposition, anti-nuclear movements, and intergenerational-ethics scholarship actively contest the comparative framing rather than accepting it as settled.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear utility operators, climate advocates, and grid planners sit near the beneficiary end: the comparative standard removes their burden of clearing an absolute bar and lets them proceed with continued or expanded operation. Host communities and future waste stewards sit near the full-target end: they bear concentrated, place-bound, and multi-generational costs that are weighed against a benefit accruing elsewhere, and their exit options are effectively nonexistent (relocation is costly for communities; nonexistence precludes voice for future stewards). Climate-vulnerable populations abroad are excluded rather than symmetric — they are rhetorically central to the justification but structurally absent from the decision process, which is why they carry the excluded role rather than beneficiary or payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no energy technology is risk-free, so some tractable acceptability standard is needed — remains genuinely live; this is not a pure zombie mandate. But the standard's persistence in comparative-only form, without ever developing a parallel absolute-floor safeguard for concentrated local and intergenerational harms, indicates the coordination function has been retained selectively: it resolves the beneficiaries' problem (how do we justify continued/expanded nuclear use) without correspondingly resolving the payers' problem (how do we get a floor on locally concentrated and intergenerational risk regardless of the fossil comparison). This is the tangled-rope signature: real coordination value bundled with asymmetric extraction that requires active regulatory maintenance (licensing frameworks, risk-communication practice) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comparative_baseline_choice,
    'Is coal (the dirtiest, most readily available fossil comparator) the appropriate baseline for the comparison, or would a cleaner fossil/renewable-storage baseline change the acceptability verdict for the same reactor?',
    'Recompute comparative-risk acceptability using natural gas or grid-storage-backed renewables as the counterfactual instead of coal; observe whether the acceptability verdict for existing and proposed reactors changes materially.',
    'If acceptability is highly sensitive to which fossil technology is chosen as baseline, the standard is less a principled risk framework and more a rhetorical device selecting the most favorable comparator — strengthening the case that this reading functions as extraction dressed as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_baseline_choice, conceptual, 'Sensitivity of the comparative standard to baseline-comparator selection.').

omega_variable(
    intergenerational_discounting_legitimacy,
    'Is it legitimate for a comparative standard optimizing for current-generation climate urgency to discount waste-custody risk borne by unconsulted future generations, or does this constitute an unauthorized transfer that no present party had standing to make?',
    'Comparative institutional analysis of how other long-horizon liability regimes (e.g. nuclear decommissioning trusts, Superfund) handle representation of future affected parties; philosophical literature on intergenerational justice and standing.',
    'If the transfer to future stewards is judged illegitimate absent some representational mechanism, the tangled-rope classification strengthens (asymmetric extraction becomes harder to justify as mere unavoidable byproduct of coordination) and pressure builds toward the catastrophic_tail_dominant sibling reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discounting_legitimacy, preference, 'Whether present-generation urgency can legitimately discount unconsulted future risk-bearers.').

omega_variable(
    reading_selection_as_policy_lever,
    'Given that the same reactor can be found acceptable, unacceptable, or contested depending purely on which kernel reading (comparative, catastrophic-tail, expected-value) is applied, is the choice of reading itself a contestable policy decision that should be made explicit and accountable, or is it treated as a hidden methodological default?',
    'Audit regulatory and legislative documents for explicit acknowledgment of the reading choice versus its silent embedding in methodology (e.g., risk-informed regulation guidance documents); survey whether alternative readings are presented to decision-makers as live options.',
    'If the reading choice is never surfaced as a decision, the comparative_risk_dominant reading may function to foreclose the catastrophic_tail_dominant reading by default rather than by reasoned adjudication, which would be a procedural extraction mechanism independent of the object-level risk merits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_as_policy_lever, conceptual, 'Whether the kernel-reading selection is made accountable or defaults silently to comparative framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 8, 0.22).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 16, 0.25).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 24, 0.27).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 32, 0.29).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__comparative_risk_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the acceptable_risk_for_energy kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. comparative_risk_dominant (this story) ties nuclear acceptability to the fossil-fuel counterfactual and shows moderate tangled-rope extraction (ε=0.42) concentrated on host communities and future waste stewards. catastrophic_tail_dominant would show different victim structure (weighted toward intergenerational/irreversibility harm regardless of the fossil comparison) and likely higher suppression if it forecloses expansion outright. expected_value_dominant would show a different beneficiary/victim structure again, organized around probability-weighted annualized cost rather than a fossil-specific comparator. All three are linked bidirectionally in network.affects_constraints because policy debates routinely shift which reading is invoked depending on which produces the desired outcome for a given proposal — the reading choice is itself a locus of contest documented in the reading_selection_as_policy_lever omega.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
