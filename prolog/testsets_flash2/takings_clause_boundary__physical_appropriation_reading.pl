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
 *   physical invasion, do not constitute a taking. This interpretation grants
 *   significant latitude to government regulators but places the burden of
 *   regulatory losses on property owners. This is one reading of the broader
 *   'takings_clause_boundary' kernel.
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
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '110d534d-5b7e-4252-baeb-7ed6e5b043fd').
narrative_ontology:cs_kernel_codification('110d534d-5b7e-4252-baeb-7ed6e5b043fd', fixed_text).
narrative_ontology:cs_authority_grounding('110d534d-5b7e-4252-baeb-7ed6e5b043fd', lineage).
narrative_ontology:cs_interpretation_layer_present('110d534d-5b7e-4252-baeb-7ed6e5b043fd').
narrative_ontology:cs_reading_relation('110d534d-5b7e-4252-baeb-7ed6e5b043fd', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('110d534d-5b7e-4252-baeb-7ed6e5b043fd', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('110d534d-5b7e-4252-baeb-7ed6e5b043fd', foundational, physical_invasion_is_sine_qua_non_of_taking).
narrative_ontology:cs_axiom_status(physical_invasion_is_sine_qua_non_of_taking, holdable).
narrative_ontology:cs_axiom_grounding('110d534d-5b7e-4252-baeb-7ed6e5b043fd', physical_invasion_is_sine_qua_non_of_taking, conventional).
narrative_ontology:cs_axiom('110d534d-5b7e-4252-baeb-7ed6e5b043fd', secondary, government_has_broad_police_power_uncompensated).
narrative_ontology:cs_axiom_status(government_has_broad_police_power_uncompensated, holdable).
narrative_ontology:cs_axiom_grounding('110d534d-5b7e-4252-baeb-7ed6e5b043fd', government_has_broad_police_power_uncompensated, conventional).
narrative_ontology:cs_reference_frame('110d534d-5b7e-4252-baeb-7ed6e5b043fd', original_textual_meaning_of_taking).
narrative_ontology:cs_drift_state('110d534d-5b7e-4252-baeb-7ed6e5b043fd', post_penn_central_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('110d534d-5b7e-4252-baeb-7ed6e5b043fd', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, government_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, physically_dispossessed_property_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement and enforce regulations that may diminish property value without triggering compensation, as long as they do not involve direct physical seizure or permanent occupation. They benefit from the flexibility to pursue public policy goals without high fiscal costs.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, government_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Support broad government power to regulate for environmental protection, public health, and safety without compensation, viewing property rights as subservient to collective welfare. This reading aligns with their policy goals.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates, beneficiary,
    organized, generational, mobile, national).

% Are the direct victims of physical appropriation, receiving compensation for their loss. However, their numbers are small under this narrow reading, and they have no recourse if their property is merely devalued by regulation without physical taking.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, physically_dispossessed_property_owners, payer,
    powerless, immediate, trapped, local).

% Bear significant economic losses due to regulations that do not involve physical occupation. Under this reading, they are not entitled to compensation and must absorb the loss, or attempt to influence the political process to change regulations. They are excluded from the compensation mechanism.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_facing_regulatory_losses, excluded,
    moderate, biographical, constrained, regional).

% The ultimate arbiter of the Takings Clause, whose interpretations define the boundary. This reading reflects a period of judicial deference to legislative power in property regulation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between private property rights and public regulatory power, providing a clear (though narrow) rule for when government action requires compensation, thereby enabling broad regulatory action without constant litigation over economic impacts.
% TRANSFER_FUNCTION: Transfers the cost of regulatory burdens from the government (and taxpayers) to individual property owners, except in cases of direct physical seizure, where compensation is transferred from the government to the dispossessed owner.
% ABSENT_VOICES: Property owners who suffer significant economic diminution from regulation without physical appropriation are effectively absent from the compensation conversation; their claims are dismissed as non-takings. Advocates for broader property rights would argue for compensation in these cases.
% DISAPPEARANCE_RATIONALE: If this reading of the Takings Clause vanished, the government's ability to regulate property without compensation would be severely curtailed. Every regulation impacting property value would potentially trigger a compensation claim, leading to massive litigation, increased public spending, or a significant reduction in regulatory activity. The balance of power between the state and private property would fundamentally shift.
% FOUNDING_PROBLEM: The Fifth Amendment's Takings Clause was established to prevent the government from forcing individuals to bear public burdens alone, ensuring 'just compensation' for private property taken for public use.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historical analyses corroborate the founding problem of preventing uncompensated government appropriation. However, the scope of 'taking' and 'public use' remains highly contested, leading to different readings of the clause's application.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.25) because compensation is provided for the narrow class of physical takings, and the constraint primarily defines the boundary of non-compensation. Suppression is low (0.15) because the constraint is a legal interpretation, not an active coercive force, though it suppresses claims for regulatory takings. Theater ratio is very low (0.05) as the legal interpretation is directly applied, with little performative maintenance. The claimed type is 'rope' because it provides a clear rule for coordination between government and property owners, even if the terms are unfavorable to some. The metrics reflect the period where this reading was dominant, prior to the expansion of regulatory takings jurisprudence.
 *
 * PERSPECTIVAL GAP:
 *   Government regulators perceive this as a necessary coordination mechanism for effective governance, while property owners experiencing regulatory devaluation see it as an unfair imposition of public burdens without compensation. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Government regulators and public interest advocates are beneficiaries, as this reading allows for broad regulatory action without high compensation costs. Physically dispossessed property owners are victims, as they are directly impacted by the taking, though they receive compensation. Property owners facing regulatory losses are excluded from compensation under this reading, bearing the costs without recourse. The Supreme Court acts as an agenda-setter, defining and enforcing this boundary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_regulatory_distinction,
    'Is the distinction between physical appropriation and regulatory diminution a principled, natural boundary, or a policy choice reflecting judicial deference to legislative power?',
    'Comparative legal analysis across jurisdictions with different takings jurisprudence, and historical analysis of the Supreme Court''s evolving interpretations of property rights.',
    'If a policy choice, the constraint''s ''naturalness'' is undermined, suggesting it could be reclassified as a more extractive type (e.g., Tangled Rope) from the perspective of property owners bearing regulatory losses. If a natural boundary, its Rope-like coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_vs_regulatory_distinction, conceptual, 'Ambiguity regarding the fundamental nature of the physical/regulatory distinction in takings law.').

omega_variable(
    scope_of_public_use,
    'How broadly should ''public use'' be interpreted in the context of physical takings, and does this reading allow for private-to-private transfers under the guise of public benefit?',
    'Judicial review of eminent domain cases where property is transferred to private entities for economic development, and legislative clarification of ''public use'' definitions.',
    'A broad interpretation of ''public use'' could increase the number of physically dispossessed property owners, potentially increasing the constraint''s effective extraction, even with compensation. A narrow interpretation would limit government power to take property.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_public_use, preference, 'Uncertainty regarding the permissible scope of government''s ''public use'' power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 1978, 1987).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1978, 0.05).
narrative_ontology:measurement(taki_tr_t1982, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1982, 0.05).
narrative_ontology:measurement(taki_tr_t1987, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1987, 0.05).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1978, 0.2).
narrative_ontology:measurement(taki_be_t1982, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1982, 0.22).
narrative_ontology:measurement(taki_be_t1987, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1987, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1978, 0.1).
narrative_ontology:measurement(taki_su_t1982, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1982, 0.12).
narrative_ontology:measurement(taki_su_t1987, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1987, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'takings_clause_boundary' kernel. This 'physical appropriation' reading narrowly defines takings, influencing the scope and application of the 'regulatory takings' and 'categorical takings' readings by setting a high bar for compensation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
