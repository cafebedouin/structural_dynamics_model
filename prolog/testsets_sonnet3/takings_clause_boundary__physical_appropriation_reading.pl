% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Clause Boundary — Physical Appropriation Reading
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint is the physical-appropriation reading of the Takings
 *   Clause boundary: the doctrinal position that only direct physical seizure
 *   or permanent physical occupation of property triggers the Fifth
 *   Amendment's compensation requirement, while regulations that destroy
 *   economic value — however severe — do not, absent a separate categorical
 *   or Penn Central showing under different doctrines. This reading gives
 *   regulators broad room to impose value-destroying restrictions without
 *   budgeting compensation, while property owners bear regulatory losses as
 *   an uncompensated cost of ownership. Two sibling readings exist as
 *   separate constraints: the categorical_takings_reading (extending per se
 *   compensation to total value elimination even without physical occupation)
 *   and the regulatory_takings_reading (compensating regulations that go 'too
 *   far' under a broader ad hoc balancing test). Each sibling has a
 *   materially different victim set and a different ε — this story addresses
 *   only the narrow physical-appropriation position.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.58).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.62).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Boundary — Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, 'a488e5d6-0706-4546-9c1b-b00878909cf1').
narrative_ontology:cs_kernel_codification('a488e5d6-0706-4546-9c1b-b00878909cf1', fixed_text).
narrative_ontology:cs_authority_grounding('a488e5d6-0706-4546-9c1b-b00878909cf1', lineage).
narrative_ontology:cs_interpretation_layer_present('a488e5d6-0706-4546-9c1b-b00878909cf1').
narrative_ontology:cs_reading_relation('a488e5d6-0706-4546-9c1b-b00878909cf1', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('a488e5d6-0706-4546-9c1b-b00878909cf1', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('a488e5d6-0706-4546-9c1b-b00878909cf1', foundational, compensation_requires_physical_invasion_or_occupation).
narrative_ontology:cs_axiom_status(compensation_requires_physical_invasion_or_occupation, holdable).
narrative_ontology:cs_axiom_grounding('a488e5d6-0706-4546-9c1b-b00878909cf1', compensation_requires_physical_invasion_or_occupation, conventional).
narrative_ontology:cs_axiom('a488e5d6-0706-4546-9c1b-b00878909cf1', secondary, regulatory_value_loss_is_uncompensated_background_risk_of_ownership).
narrative_ontology:cs_axiom_status(regulatory_value_loss_is_uncompensated_background_risk_of_ownership, holdable).
narrative_ontology:cs_axiom_grounding('a488e5d6-0706-4546-9c1b-b00878909cf1', regulatory_value_loss_is_uncompensated_background_risk_of_ownership, instrumental).
narrative_ontology:cs_reference_frame('a488e5d6-0706-4546-9c1b-b00878909cf1', police_power_eminent_domain_dichotomy).
narrative_ontology:cs_drift_state('a488e5d6-0706-4546-9c1b-b00878909cf1', post_penn_central_regulatory_state_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a488e5d6-0706-4546-9c1b-b00878909cf1', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, municipal_and_state_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, environmental_and_land_use_agencies).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, utility_and_infrastructure_planners).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners_bearing_value_loss).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, landlords_subject_to_rent_and_use_controls).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, developers_facing_uncompensated_zoning_downzoning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce zoning, land-use, environmental, and rent regulations knowing that unless a regulation results in permanent physical occupation or a literal appropriation of the property, no compensation is owed no matter how much economic value the regulation destroys. This reading is the operational rule they rely on when writing regulations that impose steep value losses without budgeting for compensation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, municipal_and_state_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Impose wetlands restrictions, historic preservation limits, and land-use caps that can eliminate most of a parcel's value. Under this reading, as long as no physical entry or occupation occurs, these restrictions trigger no compensation duty, letting the agencies pursue public-interest goals without a corresponding fiscal liability.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, environmental_and_land_use_agencies, beneficiary,
    institutional, generational, analytical, national).

% Plan easements, height restrictions, and access limitations around infrastructure corridors. When their restrictions stop short of literal seizure or permanent occupation, this reading exempts them from compensation, letting projects proceed without acquiring formal property interests.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, utility_and_infrastructure_planners, beneficiary,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, utility_and_infrastructure_planners, agenda_setter).

% Own land or buildings whose economic value is substantially or entirely destroyed by regulation — a downzoning, a use ban, an environmental restriction — but because no government agent physically enters or occupies the property, they receive no compensation under this reading. Their only recourse is state-level regulatory takings doctrine or political lobbying, both far less certain than the categorical physical-takings rule.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners_bearing_value_loss, payer,
    moderate, biographical, trapped, local).

% Face rent control, tenant-protection, and use-restriction ordinances that materially reduce rental income and resale value. Because these are use regulations rather than physical seizures, this reading treats the losses as uncompensated background risk of property ownership, regardless of magnitude.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, landlords_subject_to_rent_and_use_controls, payer,
    moderate, biographical, constrained, local).

% Purchase land anticipating a permitted use, then face a downzoning or entitlement denial that eliminates the anticipated value. Litigation under this reading fails unless they can show a literal physical taking; they absorb the loss or attempt separate regulatory-takings claims under a different, harder-to-win doctrine.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, developers_facing_uncompensated_zoning_downzoning, payer,
    moderate, biographical, constrained, regional).

% Apply a bright-line rule: physical invasion or permanent occupation triggers per se compensation; everything else, however severe the value loss, is evaluated (if at all) under separate, less protective frameworks or denied a takings remedy entirely. Courts value the rule's administrability and predictability over case-by-case economic-impact balancing.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, courts_applying_the_physical_appropriation_test, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, courts_applying_the_physical_appropriation_test, observer).

% Argue that severe regulatory value destruction is functionally indistinguishable from physical seizure and should trigger compensation. Under this reading their position has no doctrinal purchase in federal takings analysis — they must seek relief through state constitutions, legislation, or advocate for doctrinal change, none of which this reading's framework accommodates.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_rights_advocacy_groups, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__physical_appropriation_reading, diffuse).
narrative_ontology:fixing_cost_class(takings_clause_boundary__physical_appropriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives regulators, courts, and property owners a bright-line, administrable rule for when compensation is constitutionally required — avoiding case-by-case economic-impact litigation for every regulation that touches property value.
% TRANSFER_FUNCTION: Moves the cost of regulatory value destruction from public treasuries to individual property owners, so long as the regulation stops short of physical entry or permanent occupation; the fiscal savings accrue to the regulating government, the loss lands on the owner.
% ABSENT_VOICES: Property owners suffering near-total value loss from regulation (rather than physical seizure) have no seat in the interpretive process that draws the physical/regulatory line; property rights advocacy groups press the point in amicus practice but do not control doctrine under this reading.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned in favor of treating severe value-destroying regulation as compensable, regulators would face vastly larger compensation exposure, land-use and environmental regulation would slow or require budgeted compensation funds, and litigation volume challenging regulations as de facto takings would rise sharply.
% FOUNDING_PROBLEM: Courts needed a workable line between government's police-power regulation of property (traditionally uncompensated) and government's exercise of eminent domain (traditionally compensated), to prevent every regulation from becoming a compensable taking and paralyzing routine governance.
% FOUNDING_PROBLEM_CORROBORATION: Municipal and environmental regulators attest the line remains necessary to prevent unworkable compensation liability for ordinary regulation. Property rights scholars and advocacy groups, along with dissenting judicial opinions in regulatory takings cases, attest that the physical/regulatory distinction has become formalistic — protecting trivial physical intrusions (a cable box, a beach easement) while leaving owners uncompensated for regulations that destroy 100% of value, and argue the line no longer tracks the constitutional purpose of preventing the public from placing burdens on individuals that should be borne by the public as a whole.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate-high (0.58) because the rule systematically shifts the cost of regulatory value destruction onto property owners without any compensation mechanism, and this shift compounds as regulatory ambition (environmental, land-use, rent control) has grown over the interval — hence the rising base_extractiveness series. Suppression (0.62) reflects that the doctrine forecloses a legal remedy outright for a large class of harmed owners; it is not merely inconvenient, it is dispositive. Theater is comparatively low (0.28) because the rule's administrability function is genuinely operative — courts do apply the bright line consistently — though a growing share of enforcement activity increasingly defends the line's formalism (cable-box and beach-easement cases) rather than serving the coordination function it was built for.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators and agencies that write value-destroying regulations are the structural beneficiaries: the reading absorbs the fiscal exposure they would otherwise carry, so their directionality sits near the beneficiary end. Property owners, landlords, and developers whose value is destroyed by regulation (not physical entry) are the targets — trapped or constrained in exit because litigation under this doctrine offers no path to compensation, pushing their directionality toward the full-target end. Courts occupy an agenda-setting, largely analytical position: they administer the line but do not personally collect or pay under it.
 *
 * MANDATROPHY ANALYSIS:
 *   The bright-line rule was built to solve a real coordination problem — distinguishing ordinary police-power regulation from eminent domain so that routine governance would not collapse under case-by-case takings litigation. That founding problem remains partly live (regulators still need administrable rules), which is why this constraint is authored as tangled_rope rather than snare: it retains a genuine coordination function even as it enables asymmetric extraction. Where the classification would tip toward snare is if courts extend the physical/regulatory line so rigidly that even severe, deliberately targeted value destruction escapes all compensation review — at that point the coordination story becomes pure cover for cost-shifting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_regulatory_line_coherence,
    'Is the physical/regulatory distinction a principled constitutional boundary, or an administrable-but-arbitrary line that happens to track which harms are politically salient (visible physical intrusion) rather than which harms are severe (total value destruction)?',
    'Compare outcomes across matched-severity cases: a trivial permanent physical intrusion (a cable box) that receives automatic compensation versus a severe regulatory value-elimination (a 95% value-destroying wetlands restriction) that receives none. If courts and legislatures cannot articulate a principled reason for the differential beyond administrability, the line is better read as a policy convenience than a constitutional discovery.',
    'If the line is arbitrary rather than principled, this reading''s classification shifts further toward extraction (less coordination justification survives); if principled, the coordination function is stronger than the metrics currently reflect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_regulatory_line_coherence, conceptual, 'Whether the physical/non-physical boundary tracks a real constitutional distinction or administrative convenience.').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does the disagreement between this reading and its siblings live — is it about what ''taking'' means as a matter of original meaning, or about how much fiscal exposure courts are willing to impose on modern regulatory government?',
    'Trace the doctrinal history (Mahon, Loretto, Lucas, Penn Central, Cedar Point Nursery) to see whether shifts in the line track new historical evidence about founding-era understanding or track changing judicial tolerance for regulatory-state liability.',
    'If the disagreement is substantially about fiscal tolerance rather than original meaning, this reading''s coexistence with regulatory_takings_reading is better modeled as a policy contest than an interpretive one, which would affect how the sibling constraints'' reading_relations should be understood over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the kernel contest is interpretive (meaning of ''taking'') or consequentialist (fiscal tolerance for regulatory liability).').

omega_variable(
    aggregate_victim_undercount,
    'Does the physical-appropriation reading''s narrow victim set (limited to literal physical dispossession) undercount the true population harmed by uncompensated regulatory value destruction, given that most property value losses in the modern regulatory state come from land-use, environmental, and rent regulation rather than physical seizure?',
    'Empirical survey of takings litigation outcomes and denied claims across state and federal courts, tallying value-loss magnitude in cases where compensation was denied for lack of physical occupation.',
    'A large undercount would support treating the narrow reading as structurally extractive at a much larger scale than the stakeholder list captures here, strengthening the case that ε for the standing arrangement (under this reading''s own operation) is higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_victim_undercount, empirical, 'Whether the narrow victim set understates the scale of uncompensated regulatory value destruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(taki_tr_t10, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(taki_tr_t30, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(taki_tr_t50, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(taki_be_t10, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(taki_be_t30, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(taki_be_t50, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(taki_su_t10, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(taki_su_t30, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(taki_su_t50, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the Takings Clause boundary,' per the ε-invariance principle: the physical_appropriation_reading (this file, narrowest victim set, ε=0.58), the categorical_takings_reading (extends per se compensation to total value elimination without physical occupation, wider victim set, higher expected ε), and the regulatory_takings_reading (broadest compensation trigger via ad hoc balancing, narrowest surviving uncompensated victim set, likely lowest ε among the three because more harms are captured). Each reading has a distinct beneficiary/victim structure and must not be averaged into a single ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
