% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Takings Clause Boundary: Categorical Takings Reading
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   This constraint represents the 'categorical takings' reading of the Fifth
 *   Amendment's Takings Clause, as established by cases like Loretto v.
 *   Teleprompter Manhattan CATV Corp. (1982) and Lucas v. South Carolina
 *   Coastal Council (1992), alongside the Penn Central Transportation Co. v.
 *   City of New York (1978) balancing test for all other regulations. It
 *   creates bright-line rules for extreme government actions (permanent
 *   physical occupations, total value elimination) that are 'per se' takings,
 *   while leaving a large middle ground to a more flexible, fact-intensive
 *   inquiry. This reading attempts to provide predictability for property
 *   owners at the poles while preserving regulatory flexibility, but
 *   introduces significant uncertainty for regulations that fall between the
 *   extremes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.45).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.3).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Takings Clause Boundary: Categorical Takings Reading").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '202b209b-322c-4a05-a55a-060d57a26519').
narrative_ontology:cs_kernel_codification('202b209b-322c-4a05-a55a-060d57a26519', fixed_text).
narrative_ontology:cs_authority_grounding('202b209b-322c-4a05-a55a-060d57a26519', lineage).
narrative_ontology:cs_interpretation_layer_present('202b209b-322c-4a05-a55a-060d57a26519').
narrative_ontology:cs_reading_relation('202b209b-322c-4a05-a55a-060d57a26519', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('202b209b-322c-4a05-a55a-060d57a26519', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('202b209b-322c-4a05-a55a-060d57a26519', foundational, private_property_is_a_fundamental_right).
narrative_ontology:cs_axiom_status(private_property_is_a_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('202b209b-322c-4a05-a55a-060d57a26519', private_property_is_a_fundamental_right, deontological).
narrative_ontology:cs_axiom('202b209b-322c-4a05-a55a-060d57a26519', foundational, government_police_power_is_essential_for_public_welfare).
narrative_ontology:cs_axiom_status(government_police_power_is_essential_for_public_welfare, holdable).
narrative_ontology:cs_axiom_grounding('202b209b-322c-4a05-a55a-060d57a26519', government_police_power_is_essential_for_public_welfare, instrumental).
narrative_ontology:cs_reference_frame('202b209b-322c-4a05-a55a-060d57a26519', penn_central_balancing_framework).
narrative_ontology:cs_drift_state('202b209b-322c-4a05-a55a-060d57a26519', post_lucas_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('202b209b-322c-4a05-a55a-060d57a26519', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_with_extreme_losses).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners_with_moderate_losses).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, local_governments_facing_litigation).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, private_property_rights_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, police_power_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These owners benefit from clear compensation rules when their property is permanently occupied or totally devalued. They gain predictability and a strong legal claim, but still face the burden of litigation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_with_extreme_losses, beneficiary,
    powerful, biographical, constrained, local).

% These owners bear the cost of regulatory burdens that do not meet the high bar for categorical takings. They face uncertainty and the high cost of litigation under the Penn Central balancing test, often leading to uncompensated losses.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_with_moderate_losses, payer,
    moderate, biographical, constrained, local).

% Regulators benefit from the flexibility to enact most land-use and environmental regulations without triggering automatic compensation. They face clear limits at the extremes but have broad discretion in the middle ground, balancing public welfare against private property rights.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, government_regulators, agenda_setter,
    institutional, generational, constrained, national).

% These governments bear the financial and administrative costs of defending regulations against takings claims, particularly under the fact-intensive Penn Central test. They face unpredictable outcomes and potential large compensation awards.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, local_governments_facing_litigation, payer,
    organized, immediate, constrained, local).

% The ultimate arbiter of takings jurisprudence, setting the legal framework that defines the boundary between legitimate regulation and compensable taking. Its decisions shape the incentives and risks for all other stakeholders.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between legitimate government regulation (police power) and private property rights, providing some clear rules for extreme cases while allowing for flexible balancing in others.
% TRANSFER_FUNCTION: Transfers the cost of certain regulations (permanent physical occupations, total value elimination) from property owners to the government, while transferring the cost of other regulations (those evaluated by Penn Central) from the government to property owners.
% ABSENT_VOICES: Advocates for a broader interpretation of regulatory takings (e.g., 'every diminution in value is a taking') are largely excluded from the current legal framework, as are those who argue for near-absolute government police power without compensation.
% DISAPPEARANCE_RATIONALE: If this reading of the Takings Clause vanished, the legal landscape for property rights and government regulation would be thrown into chaos. Either all regulations would require compensation (crippling government) or no regulations would (eroding property rights), leading to a fundamental reorganization of economic and social structures.
% FOUNDING_PROBLEM: The Fifth Amendment's Takings Clause was established to prevent the government from forcing some people alone to bear public burdens which, in all fairness and justice, should be borne by the public as a whole.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, property rights advocates, and government attorneys generally agree that the core problem of balancing individual property rights against public welfare remains live, though they dispute the appropriate legal tests. Historical records and ongoing litigation corroborate the persistence of this tension.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) is moderate because while some property owners benefit from clear compensation rules, many others bear uncompensated regulatory burdens. Suppression (0.3) is moderate; property owners can resist through litigation, but the legal tests are complex and costly. The theater ratio (0.1) is low as the legal framework is genuinely applied, though its outcomes are often contested. The temporal measurements reflect shifts in judicial interpretation and the volume of takings litigation over time, with Lucas (1992) increasing extractiveness and suppression, followed by some moderation.
 *
 * PERSPECTIVAL GAP:
 *   Property owners with extreme losses perceive this as a just and clear protection of their rights, while those with moderate losses see it as an arbitrary line that leaves them vulnerable to uncompensated burdens. Regulators appreciate the flexibility, but local governments often view the Penn Central test as a costly and unpredictable litigation trap. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners experiencing permanent physical occupations or total value elimination are beneficiaries (low d) as they receive clear compensation. Property owners with moderate regulatory burdens are payers (high d) as they often bear uncompensated costs. Government regulators are agenda-setters (low d) as they retain broad power, but local governments facing litigation are payers (high d) due to unpredictable legal costs. The Supreme Court is an analytical observer/agenda-setter, shaping the rules.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    penn_central_predictability_ambiguity,
    'Does the Penn Central balancing test provide sufficient predictability for property owners and regulators, or does its ad hoc nature create undue uncertainty and litigation costs?',
    'Empirical study of litigation rates, settlement patterns, and judicial outcomes under Penn Central over time, compared to jurisdictions with more categorical regulatory takings tests.',
    'If Penn Central is found to be highly unpredictable, it would suggest higher effective extraction from property owners and higher costs for local governments than currently measured, potentially shifting the constraint towards a Snare for the middle ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_predictability_ambiguity, empirical, 'Uncertainty and litigation costs associated with the Penn Central balancing test.').

omega_variable(
    categorical_vs_balancing_framing,
    'Is the distinction between ''categorical'' and ''balancing'' takings a coherent legal framework, or does it represent an unstable compromise between competing theories of property rights?',
    'Conceptual analysis of judicial opinions and legal scholarship, examining internal consistency and the persistence of theoretical disputes. Resolution would depend on which underlying theory of property rights is adopted.',
    'If incoherent, the constraint''s stability is lower than perceived, and its persistence relies more on judicial precedent than on a robust theoretical foundation. This would increase the ''theater_ratio'' and ''suppression'' metrics over time as the framework is defended against internal inconsistencies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_balancing_framing, conceptual, 'Coherence of the categorical vs. balancing takings distinction.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the Takings Clause kernel, or merely a subset of the ''regulatory_takings_reading''?',
    'Analysis of the core axioms and their logical independence. If the bright-line rules for physical occupations and total value elimination are truly foundational and not merely extreme examples of regulatory takings, it is distinct.',
    'If not distinct, this reading collapses into the broader ''regulatory_takings_reading'', suggesting a single, more complex constraint rather than a family of related ones. This would alter the network structure and potentially the overall extractiveness of the combined constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinction between categorical takings and broader regulatory takings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.4).
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1992, 0.48).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, environmental_protection_regulations).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, zoning_ordinances).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'takings_clause_boundary' kernel. It defines specific bright-line rules for 'per se' takings (physical occupations, total value elimination) while applying the Penn Central balancing test to all other regulations. It coexists with and influences other readings of the Takings Clause, which emphasize either physical appropriation or broader regulatory impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
