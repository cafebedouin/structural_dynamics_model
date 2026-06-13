% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause: Originalist Narrow Reading
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'originalist narrow reading' of the U.S.
 *   Constitution's Commerce Clause, which limits federal power to regulating
 *   trade that physically crosses state borders and the instrumentalities of
 *   such trade. It explicitly excludes purely intrastate economic activity,
 *   even if it has an aggregate effect on interstate commerce. This reading
 *   is presented as a 'mountain' by its proponents, reflecting a belief in
 *   its inherent constitutional truth and immutability, despite its contested
 *   status in legal scholarship and jurisprudence. The metrics reflect its
 *   relatively low direct extraction and suppression, as its primary function
 *   is to limit federal power rather than actively extract from or suppress
 *   specific economic actors, though it does impose costs on those seeking
 *   national uniformity or federal solutions to externalities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.3).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.2).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, mountain).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause: Originalist Narrow Reading").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:emerges_naturally(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52').
narrative_ontology:cs_kernel_codification('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', fixed_text).
narrative_ontology:cs_authority_grounding('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', lineage).
narrative_ontology:cs_interpretation_layer_present('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52').
narrative_ontology:cs_reading_relation('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', commerce_clause_text__substantial_effects_limited_reading, forecloses).
narrative_ontology:cs_axiom('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', foundational, original_public_meaning_supremacy).
narrative_ontology:cs_axiom_status(original_public_meaning_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', original_public_meaning_supremacy, deontological).
narrative_ontology:cs_axiom('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', foundational, enumerated_powers_strict_construction).
narrative_ontology:cs_axiom_status(enumerated_powers_strict_construction, holdable).
narrative_ontology:cs_axiom_grounding('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', enumerated_powers_strict_construction, deontological).
narrative_ontology:cs_reference_frame('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', founding_era_constitutional_structure).
narrative_ontology:cs_drift_state('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3ef13e5e-e900-4de7-8f28-b2ffa3c9fc52', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, uniform_national_standards).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, externality_management).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, federal_legislators_seeking_broad_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and interpret the Commerce Clause according to its original public meaning, limiting federal power to direct interstate trade and its instrumentalities. Their careers and intellectual identity are tied to this interpretive method.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, originalist_legal_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from the preservation of state police powers over intrastate economic activity, free from federal preemption. This reading maximizes their regulatory autonomy.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Support this reading as a bulwark against the expansion of federal power and a defense of decentralized governance. They gain ideological and political leverage from its adoption.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, generational, mobile, national).

% The concept of uniform national standards for economic activity is a victim, as this reading prevents federal legislation from creating such standards for purely intrastate matters, even if they have aggregate interstate effects.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, uniform_national_standards, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_text__originalist_narrow_reading, uniform_national_standards).

% The ability to manage interstate externalities (e.g., environmental pollution, labor standards) through federal legislation is constrained, as this reading limits the scope of federal intervention to direct border-crossing trade.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, externality_management, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_text__originalist_narrow_reading, externality_management).

% Find their legislative authority constrained by this narrow reading, limiting their ability to enact national policies addressing issues with indirect interstate effects. They must find alternative constitutional bases or accept the limits.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_legislators_seeking_broad_powers, payer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of regulatory authority between federal and state governments, providing a clear (though narrow) boundary for federal intervention in commerce.
% TRANSFER_FUNCTION: Transfers regulatory power over intrastate economic activity from the federal government to state governments, and limits the scope of federal legislative action.
% ABSENT_VOICES: Advocates for a robust federal role in addressing national economic problems and interstate externalities are effectively marginalized by this reading, as their preferred legislative solutions would be deemed unconstitutional. They would argue for a more flexible interpretation of federal power.
% DISAPPEARANCE_RATIONALE: If this narrow reading vanished, the constitutional landscape of federalism would fundamentally shift. Federal power over commerce would likely expand significantly, leading to a reorganization of regulatory authority, new national legislation, and a diminished role for state-level economic regulation.
% FOUNDING_PROBLEM: The U.S. Constitution was designed to create a federal system with enumerated powers, preventing an overly powerful central government while allowing for national coordination where necessary.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political scientists, including those who disagree with the originalist interpretation, generally corroborate that the founding problem of balancing federal and state power remains live. The specific solution offered by this reading, however, is highly contested.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, ExtMetricName, E),
    domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(commerce_clause_text__originalist_narrow_reading),
    narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.3) because this reading primarily defines a boundary of federal power, rather than creating a mechanism for direct extraction. Its 'victims' are abstract concepts (uniform national standards, externality management) and federal legislators, who bear the cost of limited authority. Suppression is also low (0.2) as it relies on judicial interpretation rather than active coercive enforcement against economic actors. Theater ratio is low (0.1) because the interpretive method is largely consistent with its stated goals. The temporal measurements show a dip in extractiveness and suppression during periods of expansive federal power (e.g., New Deal era) when this reading was less dominant, and a resurgence with more recent originalist jurisprudence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist scholars and states' rights advocates, this reading is a faithful interpretation of the Constitution, a 'mountain' that correctly delineates federal power. From the perspective of those advocating for a more robust federal role, it is a 'snare' or 'tangled rope' that artificially constrains necessary national action, creating costs for the broader public good. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist legal scholars and state governments are beneficiaries (d near 0.0) as this reading aligns with their interpretive commitments and maximizes state autonomy. Anti-federal consolidation advocates also benefit ideologically. Uniform national standards, externality management, and federal legislators seeking broad powers are victims/targets (d near 1.0) as their objectives are directly constrained by this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_ambiguity,
    'Is the ''original public meaning'' of ''commerce among the several states'' truly limited to direct border-crossing trade, or did it encompass a broader understanding of economic intercourse in the founding era?',
    'Further historical and linguistic analysis of 18th-century usage of ''commerce'' and ''among the several states'' by non-originalist scholars, or a shift in judicial interpretive methodology.',
    'If the original meaning is found to be broader, the ''mountain'' claim of this reading would be undermined, reclassifying it as a ''tangled rope'' or ''snare'' sustained by a contested interpretive method rather than natural law. If the narrow meaning is definitively corroborated, its ''mountain'' status would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_ambiguity, empirical, 'Ambiguity in the historical interpretation of ''original public meaning''.').

omega_variable(
    federalism_vs_national_problems,
    'Does this narrow reading of the Commerce Clause adequately allow for federal solutions to genuinely national economic problems and interstate externalities, or does it create a regulatory gap?',
    'Empirical studies on the effectiveness of state-level regulation in addressing issues with interstate scope (e.g., environmental pollution, financial crises) in the absence of federal intervention, or a shift in political consensus regarding the appropriate balance of federal and state power.',
    'If it demonstrably creates unmanageable regulatory gaps, the ''beneficiary'' status of state governments would be re-evaluated, and the constraint might be reclassified as a ''snare'' on national problem-solving. If state-level solutions are found sufficient, its current classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_vs_national_problems, preference, 'Whether the federalism balance struck by this reading is functionally adequate for contemporary national challenges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1789, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(comm_tr_t1850, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(comm_tr_t1900, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.02).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1789, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(comm_be_t1850, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1850, 0.28).
narrative_ontology:measurement(comm_be_t1900, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.2).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.25).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1789, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(comm_su_t1850, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1850, 0.18).
narrative_ontology:measurement(comm_su_t1900, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.1).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1995, 0.15).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Commerce Clause text kernel, each representing a distinct interpretation of federal power over commerce. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
