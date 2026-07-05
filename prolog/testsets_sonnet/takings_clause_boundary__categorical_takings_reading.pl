% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical-Plus-Penn-Central Takings Framework (Loretto/Lucas Per Se Rules with Ad Hoc Balancing for the Middle)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This story instantiates the categorical-takings reading of the Takings
 *   Clause boundary kernel: the doctrinal structure established by Loretto v.
 *   Teleprompter Manhattan CATV Corp. (permanent physical occupation is a per
 *   se taking) and Lucas v. South Carolina Coastal Council (total elimination
 *   of economically beneficial use is a per se taking), with everything else
 *   evaluated under the ad hoc, multi-factor balancing of Penn Central
 *   Transportation Co. v. New York City. This reading is distinct from the
 *   physical_appropriation_reading (which would treat ONLY physical seizure
 *   as compensable and reject the total-value-elimination category and Penn
 *   Central balancing altogether) and the regulatory_takings_reading (which
 *   would extend compensation to any regulation that goes 'too far' in
 *   diminishing value, without the categorical/ad-hoc bifurcation this
 *   reading depends on). The categorical-plus-Penn-Central structure is the
 *   doctrinally dominant, currently operative reading in U.S. constitutional
 *   law — it is a compromise position that neither sibling reading would
 *   produce, because it creates a genuinely two-tiered system: bright-line
 *   certainty at the poles, contextual uncertainty in the middle. This is a
 *   single, ε-stable constraint: I have not folded the sibling readings'
 *   extraction profiles into this measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.42).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.38).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical-Plus-Penn-Central Takings Framework (Loretto/Lucas Per Se Rules with Ad Hoc Balancing for the Middle)").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '3a6b42bb-1f68-413d-85b4-d3eeb8bef80e').
narrative_ontology:cs_kernel_codification('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', formalized).
narrative_ontology:cs_authority_grounding('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', lineage).
narrative_ontology:cs_interpretation_layer_present('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e').
narrative_ontology:cs_reading_relation('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', foundational, bright_line_rules_warranted_only_at_extremes).
narrative_ontology:cs_axiom_status(bright_line_rules_warranted_only_at_extremes, holdable).
narrative_ontology:cs_axiom_grounding('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', bright_line_rules_warranted_only_at_extremes, instrumental).
narrative_ontology:cs_axiom('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', foundational, contextual_balancing_appropriate_for_ordinary_regulation).
narrative_ontology:cs_axiom_status(contextual_balancing_appropriate_for_ordinary_regulation, holdable).
narrative_ontology:cs_axiom_grounding('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', contextual_balancing_appropriate_for_ordinary_regulation, conventional).
narrative_ontology:cs_reference_frame('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', penn_central_ad_hoc_inquiry_as_default).
narrative_ontology:cs_drift_state('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', post_lucas_categorical_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a6b42bb-1f68-413d-85b4-d3eeb8bef80e', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, municipal_and_state_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_at_the_poles).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, land_use_planning_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, mid_range_regulatory_takings_claimants).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, small_property_owners_with_diminished_but_not_eliminated_value).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, environmental_and_land_use_permit_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce land-use, environmental, and zoning regulations knowing the per se rules give them a bright line to design around (avoid permanent physical occupation, avoid total value elimination) while the Penn Central middle ground gives courts broad discretion that in practice defers heavily to the government's stated purpose. They administer the compensation determination process and control which facts enter the record.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, municipal_and_state_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Own property subject to a permanent physical occupation (cable box, easement) or a regulation eliminating all economically beneficial use (Lucas-style). They get near-automatic compensation under the categorical rules without needing to litigate the multi-factor balancing test. Their situation is legally clean and predictable.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_at_the_poles, beneficiary,
    moderate, biographical, mobile, national).

% Own property subject to regulation that diminishes value substantially (50-90%) without eliminating it entirely and without physical occupation. They fall into the Penn Central ad hoc balancing test, which has no fixed formula, weighs investment-backed expectations against character of the government action in ways courts apply inconsistently, and results in compensation far less often than the severity of the loss would suggest. They bear litigation costs for years with uncertain outcomes.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, mid_range_regulatory_takings_claimants, payer,
    moderate, biographical, constrained, national).

% Individual homeowners or small landholders whose property value drops sharply under wetlands designations, historic preservation overlays, or downzoning, but who retain some residual use and therefore cannot invoke the Lucas total-wipeout rule. They lack resources to litigate a multi-year Penn Central case and often simply absorb the loss.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, small_property_owners_with_diminished_but_not_eliminated_value, payer,
    powerless, biographical, trapped, local).

% Developers and landowners seeking permits who are denied or conditioned in ways that reduce value without triggering per se treatment. They face the Penn Central test's unpredictability as a cost of doing business, often settling for reduced development rights rather than litigating an uncertain claim.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, environmental_and_land_use_permit_applicants, payer,
    moderate, biographical, constrained, regional).

% Benefit from the doctrinal structure because it preserves broad regulatory latitude for environmental, zoning, and land-use goals in the vast middle category while conceding only the narrow poles. This lets planning continue largely unconstrained by compensation liability.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, land_use_planning_agencies, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, land_use_planning_agencies, agenda_setter).

% Authored the tripartite structure (Loretto, Lucas, Penn Central) as a doctrinal compromise between protecting property rights and preserving regulatory power. Continues to adjudicate boundary disputes about what counts as 'permanent,' 'total,' and how to weigh Penn Central factors, effectively controlling how porous the categorical/ad-hoc line is.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, supreme_court, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, diffuse).
narrative_ontology:fixing_cost_class(takings_clause_boundary__categorical_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides government regulators, courts, and property owners a shared, administrable framework for deciding when a regulation crosses into compensable taking territory — bright-line rules resolve easy extreme cases without costly litigation, while a flexible standard handles the vast range of intermediate regulatory impacts that a rigid rule could not anticipate.
% TRANSFER_FUNCTION: Moves the burden of uncompensated value loss from government treasuries onto property owners whose losses fall short of total elimination or physical occupation but who nonetheless suffer severe diminution; the framework routes compensation certainty to the poles and litigation risk/likely non-compensation to the middle.
% ABSENT_VOICES: Mid-range claimants and small property owners are structurally present as litigants but functionally underrepresented in the framework's design — the doctrine was built through appellate litigation dominated by well-resourced parties (utilities in Loretto, a developer in Lucas) and academic commentary, not by the diffuse mass of owners whose losses fall in the ad hoc middle and who mostly never litigate at all because Penn Central's unpredictability makes suits not worth the cost.
% DISAPPEARANCE_RATIONALE: If this tripartite structure vanished, courts would need an entirely different mechanism to allocate takings liability; regulators would lose the safe harbor of avoiding the two per se categories, land-use planning would face either far greater compensation exposure (if replaced by a broader standard) or far less owner protection (if replaced by a narrower one), and decades of reliance-based zoning and environmental regulation would face renewed constitutional exposure.
% FOUNDING_PROBLEM: The Takings Clause needed an administrable line between regulation (government's ordinary police power, uncompensated) and taking (requiring 'just compensation') that avoided both making all regulation potentially compensable (crippling government) and allowing all regulation to escape compensation regardless of severity (evacuating the constitutional right).
% FOUNDING_PROBLEM_CORROBORATION: Property rights scholars and organizations (e.g., Pacific Legal Foundation, some law-and-economics academics) attest the founding problem remains live and that the Penn Central middle systematically under-compensates severe regulatory impacts, citing empirical studies of takings claim win rates. Municipal law associations and environmental law scholars attest the framework functions largely as intended, preserving needed regulatory flexibility. Independent legal historians outside both camps note the doctrine has drifted from Justice Holmes's original 'too far' inquiry toward a categorical/ad-hoc bifurcation that neither the founding case (Pennsylvania Coal) nor early commentators anticipated, suggesting mandate drift rather than stable original design.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).
:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects a real but moderate transfer: the categorical rules genuinely protect owners at the poles (low extraction there), but the vast Penn Central middle systematically under-compensates severe-but-not-total diminutions, and that under-compensation has mildly increased over the interval as courts have applied Penn Central with increasing deference to government purpose (Murr v. Wisconsin's parcel-as-a-whole doctrine, for instance, narrowed what counts as 'the property' for denominator purposes, reducing effective compensation rates). Suppression (0.38) is moderate: property owners are not coerced into silence, but the doctrinal unpredictability of the ad hoc middle functions as a structural barrier — most mid-range claimants never litigate because expected recovery does not justify the cost, which is a suppression-by-unpredictability mechanism distinct from formal legal bars. Theater ratio (0.28) captures that a meaningful share of Penn Central litigation activity is now devoted to characterizing facts to fit or avoid the per se categories (redefining occupations as 'temporary,' litigating what counts as 'total' loss) rather than substantively balancing the equities Penn Central was designed to weigh.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/planning-agency seat, this reading looks like principled line-drawing: a doctrine that honors constitutional text at genuine extremes while preserving democratic self-governance over land use. From the mid-range claimant seat, the same doctrine looks like a bait-and-switch: a constitutional right that is real and enforceable exactly at the two poles almost no one's situation actually occupies, and illusory everywhere else. The engine's per-seat computation should register this divergence as the structural signature of a tangled rope — the coordination function (administrable line-drawing) is real, but it rides on an asymmetric extraction (compensation concentrated at rarely-triggered poles, non-compensation concentrated in the common middle).
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners at the poles are near-beneficiaries: the per se rules were built for them and deliver compensation with minimal litigation friction. Regulators and planning agencies are beneficiaries of the overall structure because it concedes the narrow, rare poles while preserving nearly unconstrained authority over the much larger middle category. Mid-range claimants and small owners are the structural targets: they bear the doctrine's central compromise — the price of regulatory flexibility in the middle is largely paid by them, in the form of uncompensated diminution and prohibitive litigation costs relative to expected recovery.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing ordinary regulation from compensable taking — remains genuinely live; this is not an atrophied mandate. But the SPECIFIC categorical/ad-hoc bifurcation is itself a later doctrinal innovation (1978-1992) layered onto a much older, vaguer standard (Pennsylvania Coal Co. v. Mahon, 1922's 'too far' test), and its persistence is defended partly on stare decisis grounds even as scholars document its administrability has not delivered the predictability it promised in the Penn Central middle. This makes it a poor fit for pure mountain or pure rope framing: there is a genuine, still-live coordination problem, but the specific doctrinal architecture chosen to solve it distributes the solution's benefits and costs asymmetrically in a way that persists partly through institutional inertia and stare decisis rather than continued re-derivation from first principles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_ad_hoc_boundary_manipulability,
    'Is the line between ''permanent physical occupation''/''total value elimination'' and the Penn Central middle a stable, principled distinction, or is it manipulable by characterizing facts (e.g., ''temporary'' occupations, retaining nominal residual use to avoid Lucas) such that the categorical protections are narrower in practice than in theory?',
    'Empirical coding of takings litigation outcomes to measure how often courts characterize borderline facts to avoid triggering per se treatment, compared against outcomes under the sibling readings'' alternative boundary rules.',
    'If the boundary is highly manipulable, the categorical protections are more theater than substance and the effective structure is closer to the physical_appropriation_reading (narrow protection) than the doctrine''s text suggests, which would push this constraint''s classification toward more concentrated extraction (snare-adjacent) rather than genuine tangled-rope coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_ad_hoc_boundary_manipulability, empirical, 'Whether the categorical/ad hoc line is a stable boundary or a manipulable pressure point.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the categorical-plus-Penn-Central structure the correct reading of what the Takings Clause requires, or is it a judicially constructed compromise that lacks textual or historical grounding compared to either the narrower physical_appropriation_reading or the broader regulatory_takings_reading?',
    'Originalist historical analysis of eighteenth- and nineteenth-century takings practice, compared against the doctrinal genealogy from Pennsylvania Coal through Penn Central, Loretto, and Lucas.',
    'If the categorical/ad-hoc split is a twentieth-century judicial invention with weak historical grounding, the reading''s legitimacy rests more on stare decisis and administrability than on constitutional fidelity — this does not change ε but bears on how corroborated the founding_problem_status claim should be treated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading''s compromise architecture is textually/historically grounded or a constructed doctrinal artifact.').

omega_variable(
    penn_central_predictability_trend,
    'Has Penn Central balancing become MORE predictable over time as precedent accumulates, or has it remained irreducibly ad hoc, such that the ''uncertainty in the middle'' cost to mid-range claimants is stable, worsening, or improving?',
    'Longitudinal empirical study of Penn Central win/loss rates and doctrinal citation patterns across circuits from 1978 to present.',
    'A finding of increasing predictability would support the coordination framing (the standard is maturing into a workable rule); a finding of persistent or worsening unpredictability would support the extraction framing (the middle remains a zone of arbitrary, largely non-compensable loss).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_predictability_trend, empirical, 'Whether Penn Central''s ad hoc balancing has stabilized into predictability or remains irreducibly uncertain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(taki_tr_t1988, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1988, 0.19).
narrative_ontology:measurement(taki_tr_t1998, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(taki_tr_t2008, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement(taki_tr_t2016, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(taki_be_t1988, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1988, 0.34).
narrative_ontology:measurement(taki_be_t1998, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1998, 0.38).
narrative_ontology:measurement(taki_be_t2008, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(taki_be_t2016, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2016, 0.41).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(taki_su_t1988, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1988, 0.28).
narrative_ontology:measurement(taki_su_t1998, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1998, 0.31).
narrative_ontology:measurement(taki_su_t2008, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2008, 0.34).
narrative_ontology:measurement(taki_su_t2016, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2016, 0.36).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__categorical_takings_reading, 0.1).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the takings_clause_boundary kernel. physical_appropriation_reading is the narrowest sibling (only direct seizure/permanent occupation compensable — lower ε, less regulatory flexibility conceded). regulatory_takings_reading is the broadest sibling (any regulation going 'too far' triggers compensation — higher ε from the regulator's perspective, since compensation exposure is much wider). This reading occupies the doctrinal middle: it concedes compensation at the poles (converging with physical_appropriation_reading there) while preserving broad regulatory latitude in between (converging with a rejection of regulatory_takings_reading's broader standard). Each reading has a distinct victim/beneficiary structure and a distinct ε; they are linked here rather than merged because measuring the constraint through the categorical-plus-balancing observable produces a structurally different claim than measuring it through either sibling's observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
