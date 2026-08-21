% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint represents the 'physical appropriation' reading of the
 *   Fifth Amendment's Takings Clause, which holds that only direct physical
 *   seizures or permanent physical occupations of private property by the
 *   government trigger the requirement for 'just compensation.' Under this
 *   reading, regulations that merely diminish property value, without
 *   physical invasion, do not constitute a taking. This reading is one of
 *   several competing interpretations of the Takings Clause, each with
 *   different implications for government power and property rights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.25).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.15).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Boundary: Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, 'f1cb27c2-4300-4e31-b967-f8f02298e85a').
narrative_ontology:cs_kernel_codification('f1cb27c2-4300-4e31-b967-f8f02298e85a', fixed_text).
narrative_ontology:cs_authority_grounding('f1cb27c2-4300-4e31-b967-f8f02298e85a', lineage).
narrative_ontology:cs_interpretation_layer_present('f1cb27c2-4300-4e31-b967-f8f02298e85a').
narrative_ontology:cs_reading_relation('f1cb27c2-4300-4e31-b967-f8f02298e85a', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1cb27c2-4300-4e31-b967-f8f02298e85a', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('f1cb27c2-4300-4e31-b967-f8f02298e85a', foundational, physical_invasion_is_the_sine_qua_non_of_a_taking).
narrative_ontology:cs_axiom_status(physical_invasion_is_the_sine_qua_non_of_a_taking, holdable).
narrative_ontology:cs_axiom_grounding('f1cb27c2-4300-4e31-b967-f8f02298e85a', physical_invasion_is_the_sine_qua_non_of_a_taking, conventional).
narrative_ontology:cs_axiom('f1cb27c2-4300-4e31-b967-f8f02298e85a', secondary, economic_loss_from_regulation_is_background_risk).
narrative_ontology:cs_axiom_status(economic_loss_from_regulation_is_background_risk, holdable).
narrative_ontology:cs_axiom_grounding('f1cb27c2-4300-4e31-b967-f8f02298e85a', economic_loss_from_regulation_is_background_risk, conventional).
narrative_ontology:cs_reference_frame('f1cb27c2-4300-4e31-b967-f8f02298e85a', original_intent_physical_invasion).
narrative_ontology:cs_drift_state('f1cb27c2-4300-4e31-b967-f8f02298e85a', contemporary_judicial_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f1cb27c2-4300-4e31-b967-f8f02298e85a', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, government_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_physically_dispossessed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_economically_impacted).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% This reading allows government to implement broad regulatory schemes (environmental protection, zoning, public health) without triggering compensation, as long as they do not involve direct physical seizure or permanent occupation. They benefit from reduced fiscal burden and increased policy flexibility.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, government_regulators, agenda_setter,
    institutional, generational, constrained, national).

% These are the only property owners who receive compensation under this reading, specifically when their property is directly seized or permanently occupied. While compensated for the physical taking, they bear the burden of proving such a taking occurred and may face significant legal costs.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_physically_dispossessed, payer,
    moderate, biographical, constrained, local).

% These property owners experience significant economic loss due to regulation (e.g., zoning changes, environmental restrictions) but are not physically dispossessed. Under this reading, they receive no compensation and bear these losses as a background risk of property ownership. Their only recourse is political action or challenging the regulation's legality, not a takings claim.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_economically_impacted, payer,
    organized, biographical, constrained, national).

% Advocate for regulations that serve broader public goods (environmental protection, historic preservation, affordable housing). This reading supports their policy goals by limiting the government's compensation obligations, making such regulations more feasible to implement.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates, beneficiary,
    organized, generational, mobile, national).

% Interpret and apply the Takings Clause, defining the boundary between compensable takings and non-compensable regulation. This reading provides a clear, bright-line rule for their adjudication, reducing the complexity of takings claims compared to more expansive readings.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between legitimate government regulation and compensable property appropriation, providing a clear rule for when compensation is due. This clarity allows both government and property owners to plan with greater certainty regarding regulatory impacts.
% TRANSFER_FUNCTION: Transfers the cost of regulatory burdens from the government (and taxpayers) to property owners, except in cases of direct physical seizure or permanent occupation, where compensation flows from the government to the dispossessed owner.
% ABSENT_VOICES: Property rights advocates who argue for a broader interpretation of 'taking' to include significant economic diminution without physical appropriation. Their arguments for compensation in regulatory contexts are largely excluded by this reading's narrow scope.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, the legal landscape for property rights and government regulation would be fundamentally altered. Without a clear physical appropriation standard, courts would likely adopt a more expansive (and potentially inconsistent) approach to regulatory takings, leading to increased litigation, greater fiscal burdens on government, and reduced predictability for both regulators and property owners.
% FOUNDING_PROBLEM: The Fifth Amendment's Takings Clause was established to prevent the government from forcing individuals 'alone to bear public burdens which, in all fairness and justice, should be borne by the public as a whole.' The problem was how to define the boundary of this protection.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historical analyses corroborate the founding problem of balancing public good with private property rights. The ongoing debate among different readings of the Takings Clause confirms the problem remains live, with different factions emphasizing different aspects of 'fairness and justice'.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).
:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because compensation is provided for the narrow class of physical takings, and the reading itself is relatively stable and predictable. Suppression is low (0.15) because property owners retain legal avenues to challenge physical appropriations, and the government's power to regulate is generally accepted. Theater ratio is low (0.05) as the distinction between physical and regulatory impacts is generally clear, and enforcement is straightforward. Accessibility collapse is moderate (0.7) because while physical takings are compensated, the vast majority of regulatory impacts on property value are not, effectively collapsing the 'compensation' alternative for most property owners. Resistance is moderate (0.3) from property rights groups who advocate for broader takings protections.
 *
 * PERSPECTIVAL GAP:
 *   Government entities and public interest groups perceive this as a reasonable and necessary balance, allowing for effective governance. Property owners, particularly those whose property value is significantly diminished by regulation without physical taking, perceive it as an unfair burden, arguing that their property has been 'taken' in an economic sense without compensation. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Government regulators and public interest advocates are beneficiaries (d near 0.0) as this reading grants them broad regulatory power without significant compensation burdens. Property owners who are physically dispossessed are payers (d near 1.0) as they bear the direct cost of the taking, though they receive compensation. Property owners economically impacted by regulation but not physically dispossessed are also payers (d near 1.0) as they bear uncompensated losses. Courts act as agenda-setters, interpreting and enforcing this boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling legitimate government regulation as extraction by setting a high bar for compensation. It ensures that the government can pursue public welfare goals without being financially paralyzed by every regulation that impacts property value. However, critics argue it risks mislabeling significant economic extraction as mere coordination, particularly for property owners who bear substantial uncompensated regulatory burdens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_regulatory_distinction_ambiguity,
    'Is the distinction between a ''direct physical seizure/occupation'' and a ''regulation that goes too far'' always clear, or does it involve subjective judicial interpretation?',
    'Analysis of judicial decisions in borderline cases: if outcomes are highly inconsistent or depend on specific judicial philosophies, the distinction is ambiguous.',
    'If ambiguous, the effective suppression and extractiveness for property owners could be higher than measured, as the ''bright-line'' rule becomes less predictable, increasing litigation risk and uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_vs_regulatory_distinction_ambiguity, conceptual, 'Ambiguity in distinguishing physical takings from regulatory takings.').

omega_variable(
    economic_impact_threshold_justification,
    'Is there a principled justification for compensating only physical takings, while allowing significant economic diminution from regulation to go uncompensated?',
    'Philosophical and economic analysis of property rights theory, comparing ''bundle of sticks'' conceptions of property with ''physical integrity'' conceptions. Examination of comparative legal systems'' approaches to regulatory compensation.',
    'If no principled justification exists, this reading could be reclassified as a Snare for economically impacted property owners, as the coordination story (regulatory flexibility) would be seen as cover for uncompensated extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_impact_threshold_justification, preference, 'Justification for the compensation threshold based on physical appropriation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(taki_tr_t10, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(taki_tr_t30, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(taki_tr_t50, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(taki_be_t10, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(taki_be_t30, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(taki_be_t50, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(taki_su_t10, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(taki_su_t30, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(taki_su_t50, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the Takings Clause boundary. Each reading defines the scope of government power and property rights differently, leading to distinct beneficiary/victim structures and classifications. This reading (physical appropriation) influences the others by setting a narrow baseline for compensable takings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
