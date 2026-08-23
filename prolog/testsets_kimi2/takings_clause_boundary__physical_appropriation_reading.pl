% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Takings Clause Physical Appropriation Reading
 *   domain: constitutional/law/property
 *
 * SUMMARY:
 *   This constraint instantiates the physical_appropriation_reading of the
 *   contested takings_clause_boundary kernel. Under this reading, the Fifth
 *   Amendment's Takings Clause requires compensation only for direct physical
 *   seizures or permanent physical occupations of property. All other
 *   regulatory burdens, including total economic wipeouts effected by
 *   regulation, are treated as non-compensable exercises of the police power.
 *   The reading narrows the victim set to victims of physical dispossession
 *   and expands government regulatory freedom. Sibling readings include
 *   regulatory_takings_reading (significant economic diminution is
 *   compensable) and categorical_takings_reading (total wipeouts plus
 *   physical occupations are per se takings).
 *
 * KEY AGENTS:
 *   - state_and_federal_governments (agenda_setter/beneficiary â institutional/constrained)
 *   - property_owners_subject_to_regulation (payer â moderate/constrained)
 *   - regulatory_agencies (beneficiary â institutional/constrained)
 *   - property_rights_expansionists (excluded â organized/constrained)
 *   - constitutional_scholars (observer â analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.65).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.68).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional/law/property").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, 'a9997eab-dc0b-4d9e-a575-40fc4d8d9b53').
narrative_ontology:cs_kernel_codification('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', fixed_text).
narrative_ontology:cs_authority_grounding('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', lineage).
narrative_ontology:cs_interpretation_layer_present('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53').
narrative_ontology:cs_reading_relation('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', takings_clause_boundary__categorical_takings_reading, influences).
narrative_ontology:cs_axiom('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', foundational, physical_seizure_compensation_threshold).
narrative_ontology:cs_axiom_status(physical_seizure_compensation_threshold, holdable).
narrative_ontology:cs_axiom_grounding('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', physical_seizure_compensation_threshold, conventional).
narrative_ontology:cs_axiom('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', foundational, police_power_non_compensable).
narrative_ontology:cs_axiom_status(police_power_non_compensable, holdable).
narrative_ontology:cs_axiom_grounding('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', police_power_non_compensable, conventional).
narrative_ontology:cs_reference_frame('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', physical_appropriation_baseline).
narrative_ontology:cs_drift_state('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', post_cedar_point_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a9997eab-dc0b-4d9e-a575-40fc4d8d9b53', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, state_and_federal_governments).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_subject_to_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and perpetuate the constitutional interpretation through the federal judiciary and, indirectly, state courts bound by Supreme Court precedent. Benefit from broad regulatory authority without fiscal liability for non-physical regulatory diminutions.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, state_and_federal_governments, agenda_setter,
    institutional, generational, constrained, national).

% Bear uncompensated economic losses from zoning, environmental, and other non-physical land-use regulations. Their takings claims are dismissed unless they demonstrate a direct physical appropriation or permanent occupation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_subject_to_regulation, payer,
    moderate, biographical, constrained, national).

% Exercise land-use, environmental, and economic regulatory power without compensation liability for regulatory diminution, provided no physical invasion occurs.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).

% Advocate for compensation when regulation destroys economic value, regardless of physical invasion. Structurally excluded from prevailing doctrine by the physical-appropriation threshold rule.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_rights_expansionists, excluded,
    organized, biographical, constrained, national).

% Analyze the doctrinal coherence of the physical appropriation test, its historical provenance, and its distributional consequences across constitutional and property law.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line rule for courts to distinguish compensable takings from non-compensable exercises of the police power, reducing judicial discretion and administrative cost in regulatory and eminent domain disputes.
% TRANSFER_FUNCTION: Shifts the cost of non-physical regulatory diminution from the public fisc to affected property owners; transfers legal protection from broad economic rights to narrow physical possession rights.
% ABSENT_VOICES: Property owners facing severe economic diminution without physical invasion, and regulatory takings advocates who would argue for compensation under balancing tests; their claims are foreclosed by the physical appropriation threshold.
% DISAPPEARANCE_RATIONALE: If the physical appropriation limit vanished, courts would revert to balancing tests or broader categorical rules, rearranging fiscal burdens, regulatory incentives, and the viability of zoning and environmental programs.
% FOUNDING_PROBLEM: The need to prevent government evasion of the just compensation requirement through regulatory means while preserving a robust police power for health, safety, and welfare regulation.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and some constitutional historians attest the founding understanding targeted physical appropriation; progressive legal scholars and property rights economists outside the benefiting government parties contest this as anachronistic or underinclusive.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is substantial because the constraint permits government regulations that diminish property value up to one hundred percent without compensation, so long as no physical invasion occurs. Suppression (0.68) is high because the constraint forecloses entire categories of regulatory takings claims at the pleading stage. Theater_ratio (0.40) reflects the increasing performative invocation of originalism and formalism to justify what functions as a cost-shifting rule. Accessibility_collapse (0.60) is moderate-to-high: alternative theories remain doctrinally available but are practically disfavored for non-physical intrusions. Resistance (0.55) reflects sustained scholarly and advocacy criticism plus intermittent doctrinal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the government and regulatory agency seats, the constraint is a necessary limitation on compensation liability that preserves the police power and prevents regulatory paralysis. From the property owner seat, it is an arbitrary threshold that permits uncompensated wealth destruction. The engine computes this divergence from the same structural data: identical scope and power atoms produce opposite effective extraction depending on beneficiary versus payer role.
 *
 * DIRECTIONALITY LOGIC:
 *   state_and_federal_governments and regulatory_agencies are structural beneficiaries: the constraint subsidizes their regulatory activity by eliminating compensation liability for non-physical regulations (low d, low or negative effective extraction). property_owners_subject_to_regulation are the targets: they bear the cost of regulatory diminution without recourse (high d, high effective extraction). property_rights_expansionists are excluded from the beneficiary structure entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction or pure coordination. It genuinely coordinates constitutional adjudication by supplying a bright-line rule that reduces litigation uncertainty and judicial discretion. However, it asymmetrically allocates the cost of regulation to property owners while shielding the public fisc. Without the coordination function, it would be a snare; without the asymmetric cost-shifting, it would be a rope. The founding problem â distinguishing takings from legitimate regulation â remains contested and live, so the constraint is not yet a piton, though the rising theater_ratio indicates increasing performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_contingency,
    'Does the historical record of the Fifth Amendment''s ratification support a physical-appropriation-only reading, or is this a doctrinal construction of the late twentieth century?',
    'Archival historical analysis of takings discourse at the Founding; comparison with state constitutional provisions and early eminent domain practice.',
    'If the original meaning is indeterminate or broader, the physical appropriation reading loses its conventional grounding and computes as more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_contingency, empirical, 'Historical grounding of the physical appropriation test').

omega_variable(
    regulatory_loss_as_background_risk,
    'Are non-physical regulatory losses a legitimate background risk of property ownership, or a concealed wealth transfer to the public?',
    'Comparative institutional analysis of property value fluctuations under regulatory regimes versus physical dispossession.',
    'If background risk, the extraction metric overstates cost-shifting; if wealth transfer, the coordination story is cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_loss_as_background_risk, conceptual, 'Nature of uncompensated regulatory losses').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of regulatory takings claims structural (judicial dismissal) or internalized (property owners'' acceptance of police power norms)?',
    'Post-doctrinal-shift litigation rates: if claim filing rates are low despite high regulatory burden, suppression is partly internalized.',
    'Internalized suppression raises effective extraction above the structural measure; the constraint operates on identity as well as courts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of takings claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcpa_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tcpa_tr_t20, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(tcpa_tr_t40, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(tcpa_tr_t60, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(tcpa_tr_t80, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(tcpa_tr_t100, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(tcpa_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(tcpa_be_t20, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(tcpa_be_t40, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(tcpa_be_t60, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(tcpa_be_t80, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(tcpa_be_t100, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 100, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(takings_clause_boundary__physical_appropriation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
