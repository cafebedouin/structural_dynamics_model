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
 *   human_readable: Takings Clause Boundary: Physical Appropriation Only Reading
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   This story instantiates the physical-appropriation reading of the Takings
 *   Clause boundary kernel: compensation under the Fifth Amendment is owed
 *   only for direct physical seizure or permanent physical occupation of
 *   property, not for regulatory diminution of value however severe. Under
 *   this reading, government retains broad regulatory power at no
 *   compensation cost so long as it stops short of physically taking or
 *   occupying the land. This is a narrow reading among three live doctrinal
 *   positions on the same kernel (the text 'nor shall private property be
 *   taken for public use, without just compensation'); the sibling readings —
 *   categorical (per se rules for occupation and total wipeout) and
 *   regulatory-takings (any severe diminution can trigger compensation) — are
 *   separate constraints with their own ε and are not blended into this one.
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
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Boundary: Physical Appropriation Only Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '7f87695c-c0bc-4f30-b9d5-30c4cb845598').
narrative_ontology:cs_kernel_codification('7f87695c-c0bc-4f30-b9d5-30c4cb845598', fixed_text).
narrative_ontology:cs_authority_grounding('7f87695c-c0bc-4f30-b9d5-30c4cb845598', lineage).
narrative_ontology:cs_interpretation_layer_present('7f87695c-c0bc-4f30-b9d5-30c4cb845598').
narrative_ontology:cs_reading_relation('7f87695c-c0bc-4f30-b9d5-30c4cb845598', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f87695c-c0bc-4f30-b9d5-30c4cb845598', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('7f87695c-c0bc-4f30-b9d5-30c4cb845598', foundational, compensation_triggered_only_by_physical_dispossession).
narrative_ontology:cs_axiom_status(compensation_triggered_only_by_physical_dispossession, holdable).
narrative_ontology:cs_axiom_grounding('7f87695c-c0bc-4f30-b9d5-30c4cb845598', compensation_triggered_only_by_physical_dispossession, conventional).
narrative_ontology:cs_axiom('7f87695c-c0bc-4f30-b9d5-30c4cb845598', secondary, regulatory_value_loss_is_ordinary_ownership_risk).
narrative_ontology:cs_axiom_status(regulatory_value_loss_is_ordinary_ownership_risk, holdable).
narrative_ontology:cs_axiom_grounding('7f87695c-c0bc-4f30-b9d5-30c4cb845598', regulatory_value_loss_is_ordinary_ownership_risk, instrumental).
narrative_ontology:cs_reference_frame('7f87695c-c0bc-4f30-b9d5-30c4cb845598', police_power_regulation_baseline).
narrative_ontology:cs_drift_state('7f87695c-c0bc-4f30-b9d5-30c4cb845598', post_penn_central_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7f87695c-c0bc-4f30-b9d5-30c4cb845598', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, state_and_local_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, environmental_and_land_use_agencies).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, municipalities_seeking_zoning_control).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners_bearing_uncompensated_value_loss).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, landowners_subject_to_severe_use_restrictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, developers_and_speculative_land_investors).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, developers_and_speculative_land_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce zoning, environmental, and land-use rules confident that unless they physically seize or permanently occupy land, no compensation is owed no matter how much economic value the rule destroys. This reading is what lets them regulate aggressively without budgeting for compensation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, state_and_local_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Rely on this reading to impose wetlands protections, historic preservation rules, and development moratoria without triggering the treasury cost of compensating every owner whose land value drops. The narrower the compensation trigger, the more freely they can regulate.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, environmental_and_land_use_agencies, beneficiary,
    institutional, generational, analytical, national).

% Use downzoning and land-use restriction as a low-cost policy tool. Under this reading they can eliminate most or all of a parcel's development value through regulation alone and owe nothing, because no physical seizure or occupation occurred.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, municipalities_seeking_zoning_control, beneficiary,
    organized, generational, analytical, regional).

% Have their land rendered valueless or nearly so by regulation — a mining ban, a wetlands designation, a historic overlay — but because title and physical possession are never taken, they receive no compensation. Their only recourse is expensive litigation arguing an as-applied exception, which rarely succeeds under this reading.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, regulated_property_owners_bearing_uncompensated_value_loss, payer,
    moderate, biographical, constrained, national).

% Smaller landowners without the resources to litigate a takings claim absorb the full economic loss of severe use restrictions as background risk of ownership. They cannot sell at pre-regulation value and cannot force a buyout, because the physical-appropriation line treats their loss as categorically outside the compensation requirement.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, landowners_subject_to_severe_use_restrictions, payer,
    powerless, biographical, trapped, regional).

% Absorb regulatory value destruction on some parcels but can diversify across jurisdictions and price regulatory risk into acquisition costs; unlike individual homeowners they can exit specific losses by reallocating capital, though the doctrine still denies them direct compensation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, developers_and_speculative_land_investors, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, developers_and_speculative_land_investors, beneficiary).

% Argue the physical-appropriation line is a doctrinal artifact that lets government achieve confiscation through regulation while evading the compensation the Constitution requires for outright seizure. They litigate and lobby for the broader regulatory-takings and categorical readings but do not control which reading a given court adopts.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_rights_advocacy_groups, excluded,
    organized, generational, constrained, national).

% Adjudicate which reading of the Takings Clause governs a given case. Under this reading, courts dismiss regulatory-diminution claims that fall short of physical invasion or permanent occupation, channeling nearly all disputes into the no-compensation outcome regardless of severity of economic loss.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, federal_and_state_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives government a bright-line rule distinguishing compensable takings from ordinary regulation, letting legislatures and agencies regulate land use, environmental harms, and public safety without pricing every rule against a compensation budget.
% TRANSFER_FUNCTION: Moves the cost of regulatory value destruction from the public treasury (which would otherwise pay compensation) onto individual property owners whose land value is diminished by regulation but not physically seized.
% ABSENT_VOICES: Property rights advocacy groups and severely restricted landowners argue for the regulatory-takings or categorical readings but do not control doctrine; their objections surface in dissenting opinions and academic critique rather than in controlling precedent under this reading.
% DISAPPEARANCE_RATIONALE: If the physical-appropriation-only line were abandoned, most regulatory diminution claims would become potentially compensable, forcing legislatures and agencies to budget for compensation before enacting land-use and environmental rules — a structural shift toward the regulatory-takings or categorical readings that would slow or reprice much current regulation.
% FOUNDING_PROBLEM: Early takings jurisprudence needed a workable line between government's police power to regulate for public welfare and its power of eminent domain, to prevent courts from second-guessing every regulation as a compensable taking.
% FOUNDING_PROBLEM_CORROBORATION: Government regulators and land-use agencies attest the physical line remains necessary to keep regulation administrable and affordable. Property rights scholars and organizations outside the regulatory-beneficiary set attest the line has drifted into a shield that lets government achieve near-total value confiscation through regulation while categorically avoiding the compensation the takings guarantee was meant to require — this second view corroborates from outside the beneficiary set.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at a moderate-high 0.58 by story end: the reading transfers real, sometimes total, economic loss from regulated owners to the public purse's benefit, but the transfer requires an active line-drawing exercise by courts (hence requires_active_enforcement) rather than automatic operation. Suppression (0.62) reflects that the doctrine forecloses a large class of otherwise plausible claims by definitional fiat — a landowner whose parcel is rendered worthless by regulation simply has no cognizable claim under this line, regardless of severity. Theater ratio is comparatively low (0.28): courts are genuinely adjudicating a real coordination function (workable line-drawing for the police power) even as it also serves an extractive function for regulators. Accessibility collapse (0.5) and resistance (0.6) reflect a genuinely contested doctrine — the alternative readings remain live and litigated, so alternatives have not collapsed the way they would for settled or natural constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators, environmental agencies, and municipalities are declared beneficiaries: the narrow trigger lets them regulate without pricing in compensation, so their directionality sits toward the subsidized end. Regulated property owners and severely restricted landowners are declared victims: they absorb value destruction with no compensation remedy, placing them toward the full-target end, amplified further for landowners with trapped exit options and no litigation capacity. Developers and speculative investors are also payers but with mobile exit — they can diversify across jurisdictions to blunt individual losses even though the doctrine denies them direct compensation, which is why their directionality should sit closer to the middle than the individually trapped landowner class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing every regulation from being second-guessed as a compensable taking — remains partly live (regulation would be unworkable if every diminution triggered compensation) but the corroboration split shows contestation: outside observers (property scholars, advocacy groups) argue the line has drifted from workable-administration rationale into a mechanism that lets government achieve confiscatory outcomes through regulatory means precisely because it avoids the physical-appropriation trigger. This is the seat divergence the tangled_rope classification is meant to capture: regulators experience the constraint as legitimate governance capacity; uncompensated landowners experience the identical structure as extraction dressed as police power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_line_administrability_vs_confiscation_shield,
    'Is the physical-appropriation-only line a genuinely necessary administrability boundary for the police power, or has it become a doctrinal shield letting government achieve de facto confiscation through regulation while evading the compensation the Constitution requires?',
    'Comparative empirical study of regulatory outcomes in this reading''s jurisdictions versus jurisdictions applying the broader regulatory-takings or categorical readings — measuring whether owners under the narrow reading experience systematically greater uncompensated value loss for comparable regulatory objectives.',
    'If the line functions mainly as an administrability boundary, the tangled_rope classification understates its coordination function; if it functions mainly as confiscation-shield, the classification should weight toward snare, since the coordination story becomes cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_line_administrability_vs_confiscation_shield, conceptual, 'Whether the physical-appropriation line is genuine administrability or extraction cover.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Which reading of the takings_clause_boundary kernel controls in a given jurisdiction or case, and what determines that selection — text, precedent, judicial composition, or political economy of the regulating body?',
    'Doctrinal survey tracking which reading (physical-appropriation, categorical, or regulatory-takings) different circuits and state supreme courts apply, cross-referenced with case outcomes and judicial appointment patterns.',
    'If reading selection tracks judicial composition rather than principled doctrinal distinction, all three sibling constraints are better modeled as competing live claims on the same underlying kernel rather than as a settled hierarchy, reinforcing the decomposition into separate stories rather than a single blended constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'What determines which sibling reading of the kernel actually controls in practice.').

omega_variable(
    beneficiary_capture_of_line_drawing,
    'Do regulators and agencies that benefit from the narrow physical-appropriation line exert influence over how courts draw the physical/regulatory boundary in specific cases (e.g., characterizing severe restrictions as ''mere regulation'' rather than de facto occupation)?',
    'Track amicus participation and litigation posture of government agencies in boundary-line cases; assess whether agency characterization arguments succeed at higher rates than would be predicted by case facts alone.',
    'Evidence of systematic characterization influence would support treating the beneficiary declarations as capturing an active extraction dynamic rather than incidental effect of a neutral line.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_line_drawing, empirical, 'Whether beneficiary agencies shape line-drawing outcomes beyond neutral adjudication.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(taki_tr_t8, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(taki_tr_t16, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(taki_tr_t24, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(taki_tr_t32, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(taki_be_t8, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(taki_be_t16, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(taki_be_t24, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(taki_be_t32, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(taki_su_t8, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(taki_su_t16, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(taki_su_t24, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(taki_su_t32, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__physical_appropriation_reading, 0.1).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'Takings Clause boundary' kernel per the ε-invariance principle. physical_appropriation_reading (this file) authors a narrower victim set and lower baseline extraction profile than regulatory_takings_reading, which extends compensation obligation to severe economic diminution absent physical invasion. categorical_takings_reading occupies a middle position, applying per se rules to occupation/total-wipeout cases while remitting other regulations to case-by-case balancing. All three share the same kernel text and founding problem but differ in ε, beneficiary/victim scope, and classification; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
