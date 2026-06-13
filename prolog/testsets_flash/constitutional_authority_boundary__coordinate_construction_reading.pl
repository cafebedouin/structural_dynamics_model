% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__coordinate_construction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction of Constitutional Authority
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'coordinate construction' reading of
 *   constitutional authority, where three co-equal branches (legislative,
 *   executive, judicial) each interpret the Constitution within their own
 *   sphere, with no single branch holding final, unchallengeable interpretive
 *   authority. This reading emphasizes checks and balances and inter-branch
 *   negotiation over a hierarchical interpretive structure. The constraint is
 *   claimed as a Rope because it facilitates coordination among branches, but
 *   its moderate extractiveness reflects the inherent friction and potential
 *   for conflict in such a distributed system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.4).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.2).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '3ebccf5e-15d3-41f4-8312-2fa8d34df611').
narrative_ontology:cs_kernel_codification('3ebccf5e-15d3-41f4-8312-2fa8d34df611', fixed_text).
narrative_ontology:cs_authority_grounding('3ebccf5e-15d3-41f4-8312-2fa8d34df611', lineage).
narrative_ontology:cs_interpretation_layer_present('3ebccf5e-15d3-41f4-8312-2fa8d34df611').
narrative_ontology:cs_reading_relation('3ebccf5e-15d3-41f4-8312-2fa8d34df611', constitutional_authority_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ebccf5e-15d3-41f4-8312-2fa8d34df611', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('3ebccf5e-15d3-41f4-8312-2fa8d34df611', foundational, no_single_final_arbiter).
narrative_ontology:cs_axiom_status(no_single_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('3ebccf5e-15d3-41f4-8312-2fa8d34df611', no_single_final_arbiter, deontological).
narrative_ontology:cs_axiom('3ebccf5e-15d3-41f4-8312-2fa8d34df611', foundational, inter_branch_dialogue_is_constitutional).
narrative_ontology:cs_axiom_status(inter_branch_dialogue_is_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('3ebccf5e-15d3-41f4-8312-2fa8d34df611', inter_branch_dialogue_is_constitutional, conventional).
narrative_ontology:cs_reference_frame('3ebccf5e-15d3-41f4-8312-2fa8d34df611', founding_era_distributed_authority).
narrative_ontology:cs_drift_state('3ebccf5e-15d3-41f4-8312-2fa8d34df611', contemporary_era_judicial_review_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3ebccf5e-15d3-41f4-8312-2fa8d34df611', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, citizenry).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, checks_and_balances_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution in its lawmaking and oversight functions, asserting its own sphere of authority. Benefits from not being unilaterally overridden by other branches, but must negotiate and compromise.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the Constitution in its enforcement and foreign policy roles, asserting its own sphere of authority. Benefits from not being unilaterally overridden, but must respect legislative and judicial limits.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the Constitution in its adjudication of cases, asserting its own sphere of authority. Benefits from not being unilaterally overridden, but its interpretations are subject to legislative and executive responses (e.g., statutory changes, non-acquiescence).
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the stability and accountability provided by distributed authority, preventing any single branch from accumulating unchecked power. Bears the costs of occasional inter-branch conflict and slower policy implementation.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, citizenry, beneficiary,
    organized, generational, constrained, national).

% Analyze the interplay of the branches' interpretive claims, documenting instances of cooperation, conflict, and resolution. Their work informs public discourse and legal arguments but does not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of governmental power by distributing interpretive authority across three co-equal branches, preventing any single branch from becoming the sole arbiter of constitutional meaning and ensuring checks and balances.
% TRANSFER_FUNCTION: Transfers interpretive power from a single, centralized authority to multiple, co-equal institutional actors, requiring negotiation and mutual respect for constitutional meaning.
% ABSENT_VOICES: Advocates for judicial supremacy or parliamentary primacy are present in the discourse but are structurally excluded from the 'final arbiter' role within this reading; they would argue for a single, ultimate interpretive authority.
% DISAPPEARANCE_RATIONALE: If this principle vanished, one branch would inevitably assert ultimate interpretive authority, leading to a fundamental restructuring of governmental power, potentially toward a unitary or parliamentary system. The current balance of power would collapse.
% FOUNDING_PROBLEM: The problem of preventing tyranny and ensuring accountable governance by avoiding the concentration of absolute power in any single governmental entity, while still allowing for effective governance.
% FOUNDING_PROBLEM_CORROBORATION: Historians, political scientists, and legal scholars widely corroborate that the founding problem of preventing concentrated power remains live, citing ongoing debates about executive overreach, judicial activism, and legislative gridlock. This corroboration comes from outside the direct beneficiaries of any single branch's power.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).
:- end_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.4) due to the transaction costs and occasional impasses inherent in a system of shared, rather than hierarchical, interpretive authority. Each branch must expend resources to assert and defend its constitutional interpretations against the others. Suppression is low (0.2) because no single branch can unilaterally suppress the interpretive claims of another; rather, they must engage in a continuous process of negotiation and accommodation. Theater ratio is low (0.1) as the system is genuinely functional, though sometimes inefficient. Accessibility collapse is low (0.3) as alternative interpretations and assertions of authority are always possible, leading to resistance (0.45) from branches whose interpretations are challenged.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of any single branch, the constraint might feel like a limitation on its power, but from the system-level, it's a coordination mechanism. The engine's per-seat classification should reflect this: each branch, while an agenda-setter, also experiences the constraint as a check on its own ambitions, leading to a slightly higher d than a pure beneficiary.
 *
 * DIRECTIONALITY LOGIC:
 *   All three branches (legislative, executive, judicial) are structural beneficiaries (d near 0.0-0.2) as they each retain significant interpretive power and are protected from unilateral override. The citizenry is also a beneficiary, gaining from the stability and prevention of tyranny. There are no direct 'victims' in this reading, as the system is designed to prevent concentrated extraction, though the costs of inter-branch conflict are borne diffusely.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_finality_ambiguity,
    'Does the ''coordinate construction'' reading truly preclude any de facto final arbiter, or does one branch (e.g., the judiciary through judicial review) tend to exert de facto finality in practice?',
    'Empirical analysis of historical inter-branch conflicts and their resolutions: if one branch''s interpretations consistently prevail without effective counter-measures, the de facto finality claim strengthens.',
    'If de facto finality is established, the constraint''s extractiveness and suppression would be higher for the other branches, potentially shifting its classification towards a Tangled Rope for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_finality_ambiguity, empirical, 'Ambiguity regarding de facto interpretive finality in a coordinate construction system.').

omega_variable(
    inter_branch_conflict_cost,
    'Are the transaction costs and policy delays inherent in coordinate construction a necessary price for liberty, or an inefficient extraction from effective governance?',
    'Comparative institutional analysis with systems of judicial supremacy or parliamentary primacy, evaluating governance efficiency and democratic accountability trade-offs.',
    'If deemed an inefficient extraction, the base extractiveness of this constraint would be re-evaluated upward, potentially pushing it towards a Tangled Rope for the citizenry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_branch_conflict_cost, preference, 'Whether inter-branch conflict costs are a feature or a bug of coordinate construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cons_tr_t100, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(cons_tr_t150, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 150, 0.1).
narrative_ontology:measurement(cons_tr_t200, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(cons_tr_t250, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 250, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(cons_be_t100, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(cons_be_t150, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 150, 0.42).
narrative_ontology:measurement(cons_be_t200, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(cons_be_t250, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 250, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(cons_su_t100, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement(cons_su_t150, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 150, 0.2).
narrative_ontology:measurement(cons_su_t200, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 200, 0.2).
narrative_ontology:measurement(cons_su_t250, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 250, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_authority_boundary' kernel. It describes a system of coordinate construction, where interpretive authority is distributed among co-equal branches, in contrast to judicial supremacy or parliamentary primacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
