% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Clause Boundary: Physical Appropriation Reading
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   The Takings Clause ('nor shall private property be taken for public use,
 *   without just compensation') is a single constitutional text that
 *   different courts read as triggering compensation under structurally
 *   different conditions. This story instantiates the physical-appropriation
 *   reading: compensation is owed only when government physically seizes
 *   property or permanently occupies it (Loretto-style physical invasion);
 *   regulations that destroy economic value without physical dispossession —
 *   no matter how severe — trigger no compensation obligation at all. Under
 *   this reading, the government's regulatory power is functionally
 *   unconstrained by the compensation requirement so long as it never crosses
 *   into direct physical appropriation. This is a narrower reading than the
 *   categorical-takings reading (which treats total value elimination as a
 *   per se taking even absent physical occupation) and far narrower than the
 *   regulatory-takings reading (which asks whether a regulation goes 'too
 *   far' in diminishing value). The three readings are not measurement
 *   variations on one constraint — they instantiate different victim sets,
 *   different beneficiary structures, and different ε values, and are
 *   authored as three separate constraint stories linked via
 *   network.affects_constraints, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - municipal_and_state_regulators: agenda_setter (institutional/arbitrage) — draft rules exploiting the physical/non-physical line
 *   - environmental_and_land_use_agencies: beneficiary (institutional/arbitrage) — achieve conservation goals at zero compensation cost
 *   - regulated_property_owners_bearing_uncompensated_value_loss: payer (moderate/trapped) — bear total value loss with no judicial remedy
 *   - landowners_subject_to_severe_use_restrictions_short_of_occupation: payer (powerless/trapped) — most severely exposed, least able to litigate
 *   - reviewing_courts: observer/agenda_setter (institutional/analytical) — choose which reading governs, determinative of outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.58).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.52).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Boundary: Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, 'ffdc170b-bf97-4eeb-942d-f45f8e78cacf').
narrative_ontology:cs_kernel_codification('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', fixed_text).
narrative_ontology:cs_authority_grounding('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', lineage).
narrative_ontology:cs_interpretation_layer_present('ffdc170b-bf97-4eeb-942d-f45f8e78cacf').
narrative_ontology:cs_reading_relation('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', foundational, compensation_triggered_only_by_physical_dispossession).
narrative_ontology:cs_axiom_status(compensation_triggered_only_by_physical_dispossession, holdable).
narrative_ontology:cs_axiom_grounding('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', compensation_triggered_only_by_physical_dispossession, conventional).
narrative_ontology:cs_axiom('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', secondary, police_power_regulation_categorically_uncompensable_absent_occupation).
narrative_ontology:cs_axiom_status(police_power_regulation_categorically_uncompensable_absent_occupation, holdable).
narrative_ontology:cs_axiom_grounding('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', police_power_regulation_categorically_uncompensable_absent_occupation, instrumental).
narrative_ontology:cs_reference_frame('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', police_power_regulatory_primacy).
narrative_ontology:cs_drift_state('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', post_penn_central_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ffdc170b-bf97-4eeb-942d-f45f8e78cacf', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, municipal_and_state_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, environmental_and_land_use_agencies).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, adjacent_landowners_benefiting_from_regulation).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners_bearing_uncompensated_value_loss).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, landowners_subject_to_severe_use_restrictions_short_of_occupation).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, government_regulatory_flexibility_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, police_power_primacy_over_property_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce zoning, environmental, and land-use rules that can eliminate most or all economic use of a parcel without ever taking title or possession. Because the physical-appropriation line means no compensation is owed unless the government physically seizes or permanently occupies the land, regulators can impose severe restrictions confident that courts will not require payment. They set the boundary of what counts as a taking by drafting rules just short of physical occupation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, municipal_and_state_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Impose wetlands, coastal, and habitat protections that can freeze development entirely. Under the physical-appropriation reading, these agencies achieve conservation goals at zero fiscal cost to the public treasury, since no physical taking has occurred and no compensation is triggered no matter how severe the value loss.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, environmental_and_land_use_agencies, beneficiary,
    institutional, generational, arbitrage, regional).

% Own land that a regulation renders unbuildable or unsellable, sometimes reducing its value to near zero, but the government has taken neither title nor possession. Under this reading they have no compensation claim at all, regardless of magnitude of loss, because the constitutional trigger is physical seizure or permanent occupation, not value diminution. Their only recourse is political lobbying for a variance or legislative relief, not a judicial takings claim.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners_bearing_uncompensated_value_loss, payer,
    moderate, biographical, trapped, local).

% Hold marginal or single-parcel property interests — a family farm, a small coastal lot — where a single regulatory decision can destroy the asset's value. They lack the resources to litigate a categorical or regulatory-takings theory in the alternative and are bound by whichever doctrinal reading a given court applies; under the physical-appropriation reading their claim is dismissed at the threshold.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, landowners_subject_to_severe_use_restrictions_short_of_occupation, payer,
    powerless, biographical, trapped, local).

% Gain from restrictions placed on a neighbor's parcel — preserved views, undeveloped buffers, protected watersheds — without themselves bearing any compensation obligation, since the cost of the restriction is absorbed entirely by the regulated owner under this reading.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, adjacent_landowners_benefiting_from_regulation, beneficiary,
    moderate, generational, mobile, local).

% Argue, largely from outside any given court's controlling doctrine, that severe value-destroying regulation is functionally indistinguishable from a physical taking and should trigger compensation. Their position corresponds to the regulatory-takings and categorical-takings readings; under a jurisdiction committed to the physical-appropriation reading, their arguments are heard but structurally foreclosed at the threshold question of what counts as a taking at all.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_rights_litigators_and_advocacy_groups, excluded,
    organized, generational, constrained, national).

% Decide, case by case, which doctrinal reading of the Takings Clause governs a dispute. When a court adopts the physical-appropriation reading, it forecloses regulatory-takings and value-diminution claims as a matter of law before reaching any factual weighing; the choice of reading is itself outcome-determinative and is made by the same institution that must live with the consequences of its own precedent.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, reviewing_courts, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, reviewing_courts, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives government a bright-line, administrable rule for when compensation is constitutionally required, letting regulators plan land-use, environmental, and safety programs without pricing every regulation's compensation exposure in advance.
% TRANSFER_FUNCTION: Moves the cost of achieving public regulatory goals (conservation, zoning, safety) from the general treasury onto the specific property owners whose land is restricted, so long as the restriction stops short of physical seizure or permanent occupation.
% ABSENT_VOICES: Property owners whose land value is destroyed by severe regulation, and the property-rights advocacy bar arguing on their behalf, are structurally present in litigation but excluded from the threshold determination — the physical-appropriation line decides their claim is not even the right kind of claim before any balancing of harms occurs.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned in favor of a regulatory-takings or categorical reading, regulators would face compensation exposure for severe restrictions, land-use and environmental agencies would need to budget for takings liability or narrow their rules, and severely restricted owners would gain a judicial remedy that currently does not exist for them — a substantial reallocation of costs from private owners back toward the public fisc.
% FOUNDING_PROBLEM: Courts needed a workable line distinguishing legitimate exercises of the police power (which historically required no compensation) from the eminent domain power (which has always required compensation), in an era before regulation could destroy property value without ever touching title or possession.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and land-use agencies attest the physical/regulatory distinction remains necessary to preserve governmental capacity to regulate without treasury-breaking liability. Independent legal scholars, property-rights litigation groups, and dissenting judicial opinions attest that the line has become a doctrinal convenience that lets government achieve confiscatory outcomes through regulation that it could not achieve through direct seizure without paying — a critique made by parties outside the regulatory beneficiary class, though it has not displaced the reading in jurisdictions that retain it.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that regulated owners can lose the entire economic value of an asset with zero compensation — a real and substantial transfer from private owner to public purpose, but bounded by the fact that only a subset of regulatory actions (those approaching total elimination of value) produce this maximal effect; most regulation under this reading imposes partial, non-compensable costs that are individually smaller but collectively pervasive. Suppression (0.52) is moderate: the doctrine does not physically coerce anyone, but it forecloses an entire category of judicial claim by definitional fiat, which functions as a structural bar rather than an evidentiary contest. Theater ratio is low (0.22) because the coordination function (workable line-drawing for regulatory planning) is genuinely operative, not merely performed. Accessibility collapse (0.62) is substantial because once a court adopts this reading, the alternative claim (regulatory taking) is not weighed and rejected — it is unavailable in principle. Resistance (0.55) reflects sustained, organized property-rights litigation continually pressing the boundary in courts, even though it has not dislodged the reading where adopted.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator's seat, the physical-appropriation line is principled coordination: it lets government function without an unbounded compensation exposure that would paralyze ordinary land-use and environmental policy. From the severely restricted owner's seat, the identical rule is uncompensated confiscation dressed in doctrinal language — the government achieves through regulation what it could not achieve through direct condemnation without paying. The engine computes these as structurally different per-seat classifications from the same authored data; that divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators and land-use/environmental agencies are the structural beneficiaries: they set the boundary line via rulemaking and litigation posture, and they capture the fiscal benefit of not having to compensate for severe value loss (d near the beneficiary end). Regulated owners — especially those whose land is reduced to near-zero value by a single restriction — are the structural targets: trapped by the situs of their asset (land cannot exit its jurisdiction), they bear the cost with no compensable remedy (d near the full-target end). Adjacent landowners are secondary beneficiaries who receive spillover value (preserved views, protected watersheds) without contributing to the cost. Courts occupy a dual seat: analytically they observe and adjudicate, but doctrinally their choice of reading is itself an exercise of agenda-setting power that is outcome-determinative before any facts are weighed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing police-power regulation from eminent-domain seizure — remains partially live: some regulatory line is still needed to prevent the compensation requirement from swallowing all regulation. But the specific placement of the line at physical appropriation, as opposed to severity of value loss, is contested as having drifted from a workable administrability rule into a mechanism that immunizes maximally severe regulatory takings from any compensation obligation whatsoever. Classifying this as tangled_rope (not snare) preserves the genuine coordination function — courts and regulators need SOME workable line — while registering the asymmetric extraction borne by owners whose property is regulated to near-valuelessness. A snare classification would deny the real coordination problem this line solves; a rope classification would deny the uncompensated victims it produces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    line_placement_naturalness_vs_construction,
    'Is the physical/non-physical line a principled constitutional distinction rooted in the historical meaning of ''taking,'' or a doctrinally convenient administrability rule that happens to minimize government compensation exposure?',
    'Originalist historical analysis of takings jurisprudence at the founding and through 19th-century eminent domain practice, cross-checked against the actual fiscal and regulatory-capacity consequences that would follow from adopting a broader reading.',
    'If the line is a principled reading of original meaning, the tangled_rope classification''s coordination function is more strongly grounded (courts are following the text, not manufacturing a favorable boundary). If it is primarily an administrability convenience that happens to shield government from compensation liability, the extraction component of the classification is stronger than the coordination component suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(line_placement_naturalness_vs_construction, conceptual, 'Whether the physical-appropriation boundary reflects original constitutional meaning or judicial/administrative convenience.').

omega_variable(
    kernel_reading_selection_mechanism,
    'What determines which of the three sibling readings (physical_appropriation, categorical, regulatory) a given court or jurisdiction adopts, and is that selection itself capturable by the regulators who benefit from the narrowest reading?',
    'Comparative analysis of state and federal takings jurisprudence tracking which reading prevails in which jurisdictions, correlated with amicus participation, judicial appointment patterns, and regulatory-agency litigation strategy over time.',
    'If reading-selection correlates with which institutional actors litigate a given case (regulators favor physical-appropriation precedent, property owners favor regulatory-takings precedent), the doctrinal ''choice'' of reading is itself a site of the same extraction dynamic the underlying constraint models — a second-order tangled rope sitting on top of the first.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'Whether the selection among sibling kernel readings is itself an extractable, capturable process.').

omega_variable(
    severity_threshold_ambiguity,
    'At what point does a regulation''s value-destruction become functionally equivalent to physical appropriation, and does the physical-appropriation reading''s bright line simply relocate rather than resolve this question?',
    'Empirical study of cases decided under the physical-appropriation reading where value loss approached total elimination — did courts strain the physical/non-physical distinction, or did the line hold cleanly regardless of severity?',
    'If courts strain the distinction in extreme cases (e.g., finding creative physical-invasion theories to compensate egregious value destruction), the bright line is less stable than authored and the true operative rule is closer to the categorical reading in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(severity_threshold_ambiguity, empirical, 'Whether the physical/non-physical line holds cleanly at the extremes or bends under severe-value-loss pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(taki_tr_t8, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(taki_tr_t16, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(taki_tr_t24, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(taki_tr_t32, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(taki_be_t8, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(taki_be_t16, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(taki_be_t24, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(taki_be_t32, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(taki_su_t8, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(taki_su_t16, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(taki_su_t24, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(taki_su_t32, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__physical_appropriation_reading, 0.1).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, categorical_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the Takings Clause boundary' per the ε-invariance principle. physical_appropriation_reading (this story) has the narrowest victim set and lowest measured ε among the three; categorical_takings_reading extends compensation to total-value-elimination cases; regulatory_takings_reading extends it further to any regulation that goes 'too far.' Each reading is authored as a distinct constraint with its own beneficiary/victim structure and its own stakeholders — they are not the same constraint measured three ways. Courts choosing among these readings are the mechanism by which one reading becomes operative law in a given jurisdiction; that choice is itself flagged as a second-order extraction site in the omega on kernel_reading_selection_mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
