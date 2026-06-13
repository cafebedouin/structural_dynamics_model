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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Takings Clause Boundary: Categorical Takings Reading
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint defines the boundary of what constitutes a 'taking' under
 *   the Fifth Amendment, requiring 'just compensation.' It establishes two
 *   categorical rules (permanent physical occupations and total value
 *   elimination are per se takings) and a balancing test (Penn Central
 *   factors) for all other regulatory actions. This reading attempts to
 *   provide bright-line rules for extreme cases while preserving regulatory
 *   flexibility for the vast majority of government actions. It is one
 *   reading of the broader 'takings_clause_boundary' kernel.
 *
 * KEY AGENTS:
 *   - property_owners_at_extremes: Primary beneficiary (powerful/mobile) — gain predictable compensation.
 *   - property_owners_in_middle_ground: Primary payer (moderate/constrained) — bear uncertainty and litigation costs.
 *   - government_regulators: Beneficiary (institutional/constrained) — gain regulatory flexibility.
 *   - local_governments_facing_litigation: Payer (organized/constrained) — bear legal and compensation costs.
 *   - supreme_court: Agenda setter (institutional/analytical) — defines and refines the legal framework.
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
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Takings Clause Boundary: Categorical Takings Reading").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, 'f0ec30d8-e9e5-4026-bb1a-01d96f533d9a').
narrative_ontology:cs_kernel_codification('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', fixed_text).
narrative_ontology:cs_authority_grounding('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', lineage).
narrative_ontology:cs_interpretation_layer_present('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a').
narrative_ontology:cs_reading_relation('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', foundational, per_se_takings_for_extreme_deprivations).
narrative_ontology:cs_axiom_status(per_se_takings_for_extreme_deprivations, holdable).
narrative_ontology:cs_axiom_grounding('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', per_se_takings_for_extreme_deprivations, conventional).
narrative_ontology:cs_axiom('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', foundational, penn_central_balancing_for_non_categorical_regulations).
narrative_ontology:cs_axiom_status(penn_central_balancing_for_non_categorical_regulations, holdable).
narrative_ontology:cs_axiom_grounding('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', penn_central_balancing_for_non_categorical_regulations, conventional).
narrative_ontology:cs_reference_frame('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', post_penn_central_framework).
narrative_ontology:cs_drift_state('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f0ec30d8-e9e5-4026-bb1a-01d96f533d9a', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_at_extremes).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners_in_middle_ground).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, local_governments_facing_litigation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from clear compensation rules when their property is permanently occupied or totally devalued, providing strong protection against the most severe government actions. They have predictable outcomes in these extreme cases.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_at_extremes, beneficiary,
    powerful, generational, mobile, national).

% Face uncertainty and high litigation costs when their property is regulated but not totally devalued or physically occupied. Their claims are subject to the multi-factor Penn Central balancing test, making outcomes less predictable and compensation harder to secure.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_in_middle_ground, payer,
    moderate, biographical, constrained, local).

% Benefit from the flexibility to enact a wide range of regulations without triggering automatic compensation, as long as they avoid permanent physical occupations or total value elimination. The Penn Central test provides a buffer against takings claims for most regulations.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, government_regulators, beneficiary,
    institutional, generational, constrained, national).

% Bear the financial and administrative burden of defending regulations against takings claims, especially those falling under the Penn Central test. They face unpredictable legal costs and potential compensation payouts, which can strain local budgets.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, local_governments_facing_litigation, payer,
    organized, immediate, constrained, local).

% Establishes and refines the legal framework for takings jurisprudence, including the categorical rules and the Penn Central balancing test. Its decisions shape the incentives and risks for both property owners and regulators.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for balancing private property rights with the government's power to regulate for public welfare, offering some predictability for extreme cases while allowing flexibility for most regulations.
% TRANSFER_FUNCTION: Transfers the cost of regulation from the general public (via compensation) to specific property owners (via uncompensated value diminution) depending on the severity and type of government action.
% ABSENT_VOICES: Property owners who believe any significant diminution of value should be compensated, regardless of physical occupation or total loss, are often marginalized by the current framework's emphasis on the Penn Central test for most regulatory actions.
% DISAPPEARANCE_RATIONALE: If this reading of the Takings Clause vanished, the legal landscape for property rights and government regulation would become highly unstable. Either all regulations would be compensable (paralyzing government) or none would be (eroding property rights), leading to a fundamental reorganization of economic and political power.
% FOUNDING_PROBLEM: The need to define the boundary between legitimate government regulation (police power) and unconstitutional taking of private property, ensuring fairness to property owners while allowing for public welfare initiatives.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, constitutional historians, and ongoing litigation all corroborate that the tension between private property rights and public regulation remains a live and contested issue, requiring continuous judicial interpretation. The problem is attested by sources outside the direct beneficiaries of the current framework.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).

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
 *   Extractiveness (0.45) is moderate because while some property owners receive full compensation, many others bear significant uncompensated losses under the Penn Central test. Suppression (0.30) is low-moderate, reflecting the legal and financial barriers to challenging regulations, but not outright coercion. Theater ratio (0.10) is low, as the legal framework is actively applied and contested, not merely performative. The trend shows a slight increase in extractiveness and suppression over time, indicating a gradual shift in the balance towards regulatory power.
 *
 * PERSPECTIVAL GAP:
 *   Property owners at the extremes experience this as a protective 'rope' or even a 'mountain' of constitutional right, guaranteeing compensation. Property owners in the middle ground, however, experience it as a 'tangled rope' or even a 'snare,' where the balancing test makes compensation uncertain and costly to pursue, effectively extracting value. Government regulators perceive it as a 'rope' that enables public welfare initiatives while respecting core property rights.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court, as the agenda setter, defines the constraint (d=0.5). Property owners at the extremes are beneficiaries (d=0.1) due to clear compensation. Government regulators are also beneficiaries (d=0.2) due to regulatory flexibility. Property owners in the middle ground are targets (d=0.8) due to uncompensated losses and litigation costs. Local governments are also targets (d=0.7) due to the burden of defending regulations.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the entire Takings Clause as a 'snare' (ignoring the categorical protections) or a 'rope' (ignoring the extraction from middle-ground property owners). The 'tangled rope' classification accurately captures the dual function of coordination (providing a framework for regulation) and asymmetric extraction (shifting costs to some property owners). The founding problem remains live, but its resolution is continuously contested, preventing a 'piton' classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    penn_central_predictability,
    'How predictable are the outcomes of the Penn Central balancing test for property owners and regulators?',
    'Empirical analysis of takings litigation outcomes, focusing on the consistency of judicial application of Penn Central factors across different jurisdictions and case types.',
    'If highly unpredictable, the ''tangled rope'' aspect for middle-ground property owners is more severe, approaching a ''snare.'' If more predictable, it leans closer to a ''rope'' with higher coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_predictability, empirical, 'Uncertainty in Penn Central application affects effective extraction.').

omega_variable(
    categorical_vs_balancing_tension,
    'Is the distinction between categorical takings and Penn Central balancing a stable conceptual boundary, or is it subject to continuous reinterpretation and erosion?',
    'Analysis of Supreme Court and appellate court decisions over time, looking for shifts in how ''total value elimination'' or ''permanent physical occupation'' are defined, or how the Penn Central factors are weighted.',
    'If the boundary erodes, the constraint becomes more uniformly extractive (snare-like) or more uniformly protective (rope-like), depending on the direction of erosion. If stable, the ''tangled rope'' classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_balancing_tension, conceptual, 'Stability of the categorical/balancing distinction.').

omega_variable(
    reading_legitimacy_source,
    'Is this categorical takings reading grounded primarily in textualism, originalism, or a pragmatic balancing of interests?',
    'Analysis of judicial opinions and legal scholarship that articulate the theoretical underpinnings of this reading, identifying the dominant interpretive methodology.',
    'If primarily originalist, its persistence is tied to historical interpretation; if pragmatic, its evolution is tied to policy outcomes. This affects its resilience to challenge and potential for future drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_source, conceptual, 'Interpretive grounding of the categorical takings reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.08).
narrative_ontology:measurement(taki_tr_t1990, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(taki_tr_t2000, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(taki_tr_t2010, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.4).
narrative_ontology:measurement(taki_be_t1990, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(taki_be_t2000, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(taki_be_t2010, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(taki_su_t1990, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1990, 0.27).
narrative_ontology:measurement(taki_su_t2000, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(taki_su_t2010, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2010, 0.29).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, property_rights_enforcement).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, environmental_regulation_limits).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, zoning_ordinance_validity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'takings_clause_boundary' kernel. Each reading defines the boundary of a 'taking' differently, leading to distinct beneficiary/victim structures and classifications. This reading (categorical takings) provides bright-line rules for extreme cases and a balancing test for others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
