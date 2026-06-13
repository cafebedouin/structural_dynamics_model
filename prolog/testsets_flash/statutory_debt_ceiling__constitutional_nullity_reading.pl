% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling (Constitutional Nullity Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This constraint story models the statutory debt ceiling from the
 *   perspective that it is constitutionally void, superseded by the 14th
 *   Amendment Section 4, which states that 'The validity of the public debt
 *   of the United States, authorized by law, shall not be questioned.' Under
 *   this reading, the debt ceiling is legally inoperative; the Treasury
 *   Department is constitutionally obligated to pay debts incurred by
 *   congressional appropriations, regardless of the statutory limit. Any
 *   legislative action around the debt ceiling is therefore performative, not
 *   genuinely constraining.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.05).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.02).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.95).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling (Constitutional Nullity Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'b0e7158e-922d-4b4a-917c-a6ed32e3d6d8').
narrative_ontology:cs_kernel_codification('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', fixed_text).
narrative_ontology:cs_authority_grounding('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', lineage).
narrative_ontology:cs_interpretation_layer_present('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8').
narrative_ontology:cs_reading_relation('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', foundational, public_debt_validity_unquestionable).
narrative_ontology:cs_axiom_status(public_debt_validity_unquestionable, holdable).
narrative_ontology:cs_axiom_grounding('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', public_debt_validity_unquestionable, deontological).
narrative_ontology:cs_axiom('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', foundational, statutory_law_subordinate_to_constitution).
narrative_ontology:cs_axiom_status(statutory_law_subordinate_to_constitution, holdable).
narrative_ontology:cs_axiom_grounding('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', statutory_law_subordinate_to_constitution, deontological).
narrative_ontology:cs_reference_frame('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', constitutional_supremacy_framework).
narrative_ontology:cs_drift_state('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', contemporary_political_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0e7158e-922d-4b4a-917c-a6ed32e3d6d8', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, federal_government_creditors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, congressional_majority).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, congressional_minority).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, fourteenth_amendment_section_four).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the executive branch entity responsible for managing federal debt, the Treasury is constitutionally obligated to pay debts incurred by appropriations. Under this reading, the debt ceiling is legally irrelevant to its operations, allowing it to execute borrowing as needed.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, beneficiary,
    institutional, generational, analytical, national).

% Holders of U.S. government debt are protected by the 14th Amendment, ensuring their investments will be repaid regardless of statutory limits. This provides stability and confidence in U.S. sovereign debt.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_government_creditors, beneficiary,
    organized, generational, mobile, global).

% While constitutionally obligated to fund appropriations, the majority party in Congress may engage in performative debates around the debt ceiling, which are legally meaningless under this reading. Their political capital is expended on a non-binding constraint.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congressional_majority, payer,
    institutional, biographical, constrained, national).

% Attempts by a minority party to use the debt ceiling as leverage for policy concessions are rendered moot by its constitutional nullity. Their political threats are theatrical, lacking actual legal force to compel default.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congressional_minority, payer,
    powerful, biographical, constrained, national).

% Analyze the legal and historical arguments for the debt ceiling's unconstitutionality, providing the intellectual framework for this reading. They observe the political theater without being bound by it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading, the debt ceiling does not solve a genuine coordination problem; rather, the 14th Amendment Section 4 provides the coordination for federal debt obligations, ensuring stability regardless of statutory limits.
% TRANSFER_FUNCTION: No legitimate transfer of resources occurs via the debt ceiling under this reading, as it is legally void. Any perceived transfers are a result of political maneuvering, not the constraint itself.
% ABSENT_VOICES: Those who believe the debt ceiling is a legitimate fiscal control mechanism, or a necessary tool for legislative leverage, are effectively absent from this constitutional interpretation. They would argue for its operational validity and the importance of statutory limits.
% DISAPPEARANCE_RATIONALE: If the statutory debt ceiling 'disappeared' (i.e., was formally recognized as unconstitutional), the actual operation of the federal government's fiscal policy would remain unchanged, as the Treasury would continue to pay debts as constitutionally mandated. The political theater would cease, but the underlying financial mechanisms would persist.
% FOUNDING_PROBLEM: The 14th Amendment Section 4 was enacted to ensure the validity of the public debt, particularly after the Civil War, preventing future attempts to repudiate federal obligations.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and legal experts widely corroborate the historical context and ongoing relevance of the 14th Amendment Section 4 in safeguarding the public debt. The Treasury Department's consistent position on its obligation to pay debts also supports this, independent of political rhetoric.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near zero (0.05) because, from this reading, the constraint has no legal force and thus cannot extract. Suppression is also near zero (0.02) as there is no legitimate mechanism to suppress Treasury's constitutional obligation. The theater ratio is extremely high (0.95) because all activity surrounding the debt ceiling (debates, votes, threats of default) is considered performative, lacking actual legal effect. Accessibility collapse is high (0.9) because the constitutional mandate leaves no legitimate alternative for the Treasury. Resistance is low (0.05) because the constitutional principle is largely settled, even if politically contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the debt ceiling is a Mountain (constitutional law) that renders the statutory limit inert. Other readings (e.g., 'coordination_scaffold_reading' or 'extraction_snare_reading') would experience the debt ceiling as a live, operative constraint with significant extractiveness and suppression. The divergence is between a legal-constitutional interpretation and political-operational interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   The Treasury Department and federal government creditors are beneficiaries (d near 0.0) because the 14th Amendment protects them from the debt ceiling's potential effects, ensuring debt is paid. Congress, particularly those who attempt to use the debt ceiling for political leverage, are targets (d near 1.0) as their actions are rendered legally meaningless by the constitutional nullity. The analytical observer (d near 0.5) sees the full structural picture.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_statutory_authority,
    'Is the statutory debt ceiling a constitutionally valid constraint, or is it rendered void by the 14th Amendment Section 4?',
    'Supreme Court ruling on the constitutionality of the debt ceiling, or a clear legislative repeal.',
    'If ruled unconstitutional, this reading is affirmed, and the debt ceiling is a pure Piton. If ruled constitutional, the constraint shifts to a Snare or Tangled Rope, depending on its operational dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_statutory_authority, conceptual, 'Ambiguity regarding the legal force of the debt ceiling relative to constitutional provisions.').

omega_variable(
    reading_of_statutory_debt_ceiling,
    'This constraint is the ''constitutional_nullity_reading'' of the ''statutory_debt_ceiling'' kernel. What would change if a ''coordination_scaffold_reading'' or ''extraction_snare_reading'' were adopted?',
    'A shift in judicial or executive branch interpretation, or a legislative act explicitly affirming one of the other readings'' premises.',
    'If the ''coordination_scaffold_reading'' were adopted, the constraint would be reclassified as a Scaffold, with a genuine coordination function. If the ''extraction_snare_reading'' were adopted, it would be reclassified as a Snare, with high extractiveness and identifiable victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_statutory_debt_ceiling, conceptual, 'Impact of alternative readings of the statutory debt ceiling kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0, 0.9).
narrative_ontology:measurement(stat_tr_t10, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 10, 0.92).
narrative_ontology:measurement(stat_tr_t20, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 20, 0.94).
narrative_ontology:measurement(stat_tr_t30, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 30, 0.95).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(stat_be_t10, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement(stat_be_t20, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(stat_be_t30, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(stat_su_t10, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 10, 0.02).
narrative_ontology:measurement(stat_su_t20, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 20, 0.02).
narrative_ontology:measurement(stat_su_t30, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 30, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__extraction_snare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'statutory_debt_ceiling' kernel, each representing a distinct structural interpretation of its operation and legal force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
