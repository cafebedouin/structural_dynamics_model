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
 *   human_readable: Takings Clause: Physical Appropriation Only Reading
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint represents a narrow reading of the Fifth Amendment's
 *   Takings Clause, asserting that only direct physical seizures or permanent
 *   physical occupations of private property by the government trigger a
 *   requirement for 'just compensation.' Under this interpretation,
 *   regulations that diminish property value without physical appropriation
 *   are generally not considered takings. This reading prioritizes
 *   government's police power over property owners' economic interests in
 *   non-physical regulatory contexts. The claimed type 'rope' reflects the
 *   government's perspective that this reading coordinates its regulatory
 *   authority, while the high extractiveness and suppression metrics reflect
 *   the burden placed on property owners.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.7).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.8).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause: Physical Appropriation Only Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, 'e02fa8b7-6b7d-4026-acd2-8f385cd9e513').
narrative_ontology:cs_kernel_codification('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', fixed_text).
narrative_ontology:cs_authority_grounding('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', lineage).
narrative_ontology:cs_interpretation_layer_present('e02fa8b7-6b7d-4026-acd2-8f385cd9e513').
narrative_ontology:cs_reading_relation('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', takings_clause_boundary__regulatory_takings_reading, forecloses).
narrative_ontology:cs_reading_relation('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', takings_clause_boundary__categorical_takings_reading, forecloses).
narrative_ontology:cs_axiom('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', foundational, property_is_physical_possession).
narrative_ontology:cs_axiom_status(property_is_physical_possession, holdable).
narrative_ontology:cs_axiom_grounding('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', property_is_physical_possession, deontological).
narrative_ontology:cs_axiom('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', foundational, police_power_is_uncompensated_for_non_physical_impacts).
narrative_ontology:cs_axiom_status(police_power_is_uncompensated_for_non_physical_impacts, holdable).
narrative_ontology:cs_axiom_grounding('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', police_power_is_uncompensated_for_non_physical_impacts, conventional).
narrative_ontology:cs_reference_frame('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', original_intent_fifth_amendment).
narrative_ontology:cs_drift_state('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', contemporary_jurisprudence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e02fa8b7-6b7d-4026-acd2-8f385cd9e513', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, government_entities).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_affected_by_regulation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, developers_and_businesses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities (federal, state, local governments) exercise police power to regulate land use, environmental protection, and public health. Under this reading, they are largely free to impose regulations that diminish property value without incurring compensation obligations, as long as there is no direct physical seizure or permanent occupation. This provides significant flexibility and reduces public expenditure.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, government_entities, agenda_setter,
    institutional, generational, arbitrage, national).

% These individuals or corporations bear the economic losses from regulations that restrict the use or value of their property, without receiving compensation, unless the regulation amounts to a direct physical taking. Their options are to comply, sell at a reduced value, or engage in costly litigation with a high bar for success.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_affected_by_regulation, payer,
    powerless, biographical, constrained, local).

% The judiciary interprets and enforces the Takings Clause, defining the boundary between legitimate police power and compensable takings. This reading requires them to strictly adhere to a physical appropriation test, limiting the scope of judicial review for regulatory impacts.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% These groups advocate for either broader government regulatory power (often aligning with this reading's outcome) or stronger property rights (opposing this reading). They influence public opinion and legislative efforts, but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates, observer,
    organized, generational, constrained, national).

% These entities face regulatory costs and restrictions on their property. While they may have more resources to navigate or challenge regulations than individual owners, this reading still places the burden of non-physical regulatory losses squarely on them, influencing investment decisions and development patterns.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, developers_and_businesses, payer,
    powerful, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit narrow, rule for when government must compensate property owners for actions affecting their property, thereby coordinating government planning and private expectations regarding regulatory burdens.
% TRANSFER_FUNCTION: Transfers the economic cost of most regulatory burdens from government budgets to individual property owners, by limiting compensation to direct physical appropriations.
% ABSENT_VOICES: Property owners whose land value is severely diminished by regulations (e.g., zoning changes, environmental restrictions) but who retain physical possession; they would argue for compensation for their economic losses.
% DISAPPEARANCE_RATIONALE: If this boundary vanished, governments would face immense uncertainty and potential liability for every regulation impacting property value, likely leading to paralysis in public policy or massive increases in public spending for compensation. Property owners would gain significant leverage, fundamentally altering the balance of power.
% FOUNDING_PROBLEM: To balance the government's inherent 'police power' to regulate for public welfare against the individual's right to private property, ensuring that private property is not taken for public use without just compensation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historical analyses of the Fifth Amendment's drafting, and dissenting judicial opinions consistently highlight the ongoing tension and debate over the proper scope of the Takings Clause, indicating the founding problem remains contested.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high because property owners bear significant uncompensated losses from regulations that do not involve physical appropriation. Suppression is high because property owners have very limited legal recourse for these losses under this narrow interpretation, effectively trapping them into bearing the costs. The theater ratio is low as the legal standard, while contested, is clearly articulated and enforced when applied. Accessibility collapse is high for property owners seeking compensation for non-physical regulatory burdens, as this reading largely forecloses that avenue. Resistance is moderate, reflecting ongoing legal and political challenges to this narrow interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the government's perspective, this reading is a necessary 'rope' that coordinates its ability to govern effectively for the public good, preventing excessive compensation claims. From the perspective of property owners, it functions as a 'snare,' allowing the government to extract economic value through regulation without fair compensation, effectively socializing the costs of public policy onto private individuals.
 *
 * DIRECTIONALITY LOGIC:
 *   Government entities are clear beneficiaries (low d) as they can regulate broadly without compensation. Property owners affected by regulation are clear targets (high d) as they bear the costs. Courts, while setting the boundary, are structurally aligned with the government's interest in maintaining regulatory flexibility under this reading. Developers and businesses, while powerful, are still payers under this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Does the original intent of the Fifth Amendment''s Takings Clause strictly limit ''taking'' to physical appropriation, or did it encompass broader forms of government interference with property rights?',
    'Further historical and legal scholarship examining founding-era legal concepts of property and government power, and analysis of early judicial interpretations.',
    'If original intent is found to be broader, this reading''s legitimacy as a ''rope'' (coordinating government power) would be undermined, potentially shifting it towards a ''snare'' (extracting from property owners under false pretenses of originalism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, conceptual, 'Ambiguity regarding the historical scope of ''taking'' in the Fifth Amendment.').

omega_variable(
    physical_vs_regulatory_distinction_clarity,
    'Is the distinction between ''physical appropriation'' and ''regulatory impact'' always clear, or are there hybrid cases that blur the boundary?',
    'Analysis of judicial decisions in edge cases (e.g., mandated access, temporary occupations, severe use restrictions) to identify consistent application or persistent ambiguity.',
    'If the distinction is consistently blurred, the clarity and predictability claimed by this reading would be undermined, increasing litigation costs and potentially leading to arbitrary outcomes for property owners.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_vs_regulatory_distinction_clarity, empirical, 'Clarity of the physical vs. regulatory distinction in practice.').

omega_variable(
    reading_as_political_preference,
    'Is this narrow reading primarily a legal interpretation, or does it reflect a political preference for maximizing government regulatory flexibility and minimizing public compensation costs?',
    'Analysis of judicial voting patterns, legislative advocacy, and public discourse surrounding takings cases, particularly when economic conditions or political ideologies shift.',
    'If primarily a political preference, the ''rope'' framing (coordination) would be seen as a cover for ''snare'' (extraction), as the legal justification would be secondary to policy outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_political_preference, preference, 'Whether the reading is driven by legal principle or political preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(taki_be_t1900, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(taki_be_t1930, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1930, 0.65).
narrative_ontology:measurement(taki_be_t1960, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1960, 0.68).
narrative_ontology:measurement(taki_be_t1990, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1900, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(taki_su_t1930, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1930, 0.75).
narrative_ontology:measurement(taki_su_t1960, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1960, 0.78).
narrative_ontology:measurement(taki_su_t1990, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
