% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope (Progressive Textualist Reading)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint is the 'progressive textualist' reading of the
 *   'equality_clause_scope' kernel. It asserts that the constitutional
 *   equality principle expands its application scope primarily through the
 *   democratic amendment process, rather than through judicial
 *   reinterpretation. This reading balances the original text's limits with
 *   the capacity for democratic revision, distinguishing it from both
 *   restrictive originalism and expansive universalism. The constraint
 *   functions as a Tangled Rope: it provides a coordination function (a
 *   stable, text-based understanding of equality and a legitimate path for
 *   change) but also extracts from those whose claims are not yet within its
 *   democratically amended scope, requiring active enforcement to maintain
 *   the current scope and the amendment process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.6).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.7).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.6).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope (Progressive Textualist Reading)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '5802ee10-ba34-4f16-be84-7a183610f9f1').
narrative_ontology:cs_kernel_codification('5802ee10-ba34-4f16-be84-7a183610f9f1', fixed_text).
narrative_ontology:cs_authority_grounding('5802ee10-ba34-4f16-be84-7a183610f9f1', lineage).
narrative_ontology:cs_interpretation_layer_present('5802ee10-ba34-4f16-be84-7a183610f9f1').
narrative_ontology:cs_reading_relation('5802ee10-ba34-4f16-be84-7a183610f9f1', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('5802ee10-ba34-4f16-be84-7a183610f9f1', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('5802ee10-ba34-4f16-be84-7a183610f9f1', foundational, democratic_sovereignty_in_constitutional_change).
narrative_ontology:cs_axiom_status(democratic_sovereignty_in_constitutional_change, holdable).
narrative_ontology:cs_axiom_grounding('5802ee10-ba34-4f16-be84-7a183610f9f1', democratic_sovereignty_in_constitutional_change, conventional).
narrative_ontology:cs_axiom('5802ee10-ba34-4f16-be84-7a183610f9f1', foundational, textual_fidelity_with_amendment_capacity).
narrative_ontology:cs_axiom_status(textual_fidelity_with_amendment_capacity, holdable).
narrative_ontology:cs_axiom_grounding('5802ee10-ba34-4f16-be84-7a183610f9f1', textual_fidelity_with_amendment_capacity, conventional).
narrative_ontology:cs_reference_frame('5802ee10-ba34-4f16-be84-7a183610f9f1', amendment_driven_constitutional_evolution).
narrative_ontology:cs_drift_state('5802ee10-ba34-4f16-be84-7a183610f9f1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5802ee10-ba34-4f16-be84-7a183610f9f1', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, current_beneficiaries_of_equality).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, democratic_process_advocates).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, progressive_textualist_scholars).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, excluded_groups_seeking_equality).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, judicial_activism_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively defend the constitutional amendment process as the legitimate and primary mechanism for expanding the scope of equality, ensuring broad public consensus for fundamental changes. They benefit from the stability and perceived legitimacy of this process.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, democratic_process_advocates, agenda_setter,
    organized, generational, constrained, national).

% Are groups whose claims to equality are already recognized and protected under the existing constitutional text, as interpreted through this reading. They benefit from the stability of their recognized rights.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, current_beneficiaries_of_equality, beneficiary,
    powerful, biographical, mobile, national).

% Are groups whose claims to equality are not yet explicitly covered by the constitutional text or its amendments. They bear the cost of the high bar for constitutional change, requiring sustained political mobilization to achieve their rights.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, excluded_groups_seeking_equality, payer,
    powerless, generational, trapped, national).

% Advocate for courts to interpret the equality clause more broadly and dynamically, expanding its scope without requiring formal amendments. Their preferred mechanism for change is structurally excluded by this reading's emphasis on democratic process.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judicial_activism_advocates, excluded,
    moderate, biographical, constrained, national).

% Articulate and defend this specific reading of the equality clause, providing intellectual justification for the balance between textual fidelity and democratic evolution. They shape the discourse around constitutional interpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, progressive_textualist_scholars, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, progressive_textualist_scholars, agenda_setter).

% Are the primary agents of constitutional amendment, holding the power to propose and ratify changes that expand the scope of equality. Their collective will is the ultimate arbiter of constitutional evolution under this reading.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, legislators_and_voters, agenda_setter,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, text-based framework for the principle of equality, ensuring that its expansion is a deliberate, supermajority democratic act, preventing arbitrary judicial reinterpretation and coordinating societal consensus around fundamental rights.
% TRANSFER_FUNCTION: Transfers the authority for defining the scope of equality from judicial bodies to the democratic amendment process, and transfers the burden of achieving broader equality to those seeking it through that process, requiring significant political capital and mobilization.
% ABSENT_VOICES: Expansive universalists and judicial activists, who would argue for a more immediate and judicially-driven application of equality, are structurally excluded from the primary mechanism of scope expansion under this reading. They would advocate for a lower bar for constitutional change.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the scope of equality would either become entirely static (if a restrictive originalist reading became dominant) or subject to rapid, potentially inconsistent, judicial reinterpretation (if an expansive universalist reading prevailed), fundamentally altering the legal and political landscape of civil rights and the balance of power between branches of government.
% FOUNDING_PROBLEM: To establish a foundational principle of equality within a constitutional framework, while ensuring that its evolving application reflects broad democratic consensus and legitimate constitutional change, rather than narrow judicial interpretation or static adherence to original intent.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, political scientists, and historical records attest to the ongoing tension between judicial interpretation and democratic will in constitutional evolution, supporting the idea that this reading addresses a persistent problem of legitimate constitutional change. Public opinion polls also frequently show a desire for both stability and progress in rights, reflecting the problem's live status.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) reflects the ongoing burden on groups seeking to expand equality through the difficult amendment process. Suppression (0.70) is high due to the supermajority requirements for constitutional amendments, which effectively suppress rapid or judicially-driven expansion. The theater ratio (0.15) is low because the amendment process, while difficult, is a genuine and functional mechanism for constitutional change, not mere performance. Accessibility collapse is moderate (0.50) as alternatives like judicial fiat are rejected, but the amendment path remains open. Resistance (0.45) is moderate, coming from those who advocate for faster, broader expansion of equality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of excluded groups, the constraint is highly extractive and suppressive, as it imposes a significant burden on their path to equality. From the perspective of democratic process advocates, it is a legitimate and necessary coordination mechanism for constitutional stability and popular sovereignty. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Democratic process advocates and current beneficiaries of equality are primary beneficiaries, as this reading legitimizes their preferred method of change and protects their existing rights. Excluded groups seeking equality and judicial activism advocates are targets, bearing the costs of the high bar for change and the rejection of their preferred mechanisms. Progressive textualist scholars and legislators/voters are agenda-setters, actively shaping and defending this interpretation and its mechanisms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_minority_rights,
    'Does the democratic amendment process, as the primary mechanism for expanding equality, adequately protect minority rights, or does it inherently impose an unjust burden on those seeking inclusion?',
    'Comparative analysis of civil rights outcomes in systems with different constitutional amendment/interpretation mechanisms, particularly focusing on the speed and equity of rights expansion for historically marginalized groups.',
    'If the process is found to systematically disadvantage minorities, the constraint''s effective extractiveness and suppression for ''excluded_groups_seeking_equality'' would be higher, potentially shifting the overall classification towards a Snare for that seat. If it''s found to be a robust, albeit slow, mechanism, the Tangled Rope classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_minority_rights, conceptual, 'The tension between democratic majoritarianism and the protection of minority rights in constitutional evolution.').

omega_variable(
    practical_burden_of_amendment,
    'What is the actual, practical burden (time, resources, political capital) required for ''excluded_groups_seeking_equality'' to achieve constitutional amendment, and how does this compare to the burden of judicial advocacy?',
    'Empirical study of historical amendment campaigns versus landmark judicial cases, quantifying the resources, timeframes, and success rates for different paths to rights expansion.',
    'If the practical burden of amendment is found to be prohibitively high, the ''trapped'' exit option for ''excluded_groups_seeking_equality'' would be reinforced, increasing their effective extraction. If it''s found to be comparable or more effective in the long run, their exit options might be re-evaluated as ''constrained'' rather than ''trapped''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practical_burden_of_amendment, empirical, 'Quantifying the practical difficulty of constitutional amendment as a path to rights expansion.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''progressive textualist'' reading of the equality clause, or does it lean more towards a ''restrictive originalist'' outcome in practice due to the high bar for amendment?',
    'Analysis of legislative history, judicial opinions, and public discourse over time to determine if the intent and effect of the amendment process align with progressive expansion or de facto stasis. Comparison of actual amendment outcomes versus proposed but failed amendments.',
    'If the practical outcome consistently favors stasis or minimal expansion, the classification might shift towards a Snare for ''excluded_groups_seeking_equality'', as the ''progressive'' aspect becomes theatrical cover for extraction. If genuine, albeit slow, expansion is observed, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Verifying the ''progressive'' aspect of this textualist reading against actual outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1900, equality_clause_scope__progressive_textualist, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(equa_tr_t1940, equality_clause_scope__progressive_textualist, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(equa_tr_t1960, equality_clause_scope__progressive_textualist, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(equa_tr_t1980, equality_clause_scope__progressive_textualist, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(equa_tr_t2000, equality_clause_scope__progressive_textualist, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(equa_tr_t2020, equality_clause_scope__progressive_textualist, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1900, equality_clause_scope__progressive_textualist, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.57).
narrative_ontology:measurement(equa_be_t1940, equality_clause_scope__progressive_textualist, base_extractiveness, 1940, 0.58).
narrative_ontology:measurement(equa_be_t1960, equality_clause_scope__progressive_textualist, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(equa_be_t1980, equality_clause_scope__progressive_textualist, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement(equa_be_t2000, equality_clause_scope__progressive_textualist, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(equa_be_t2020, equality_clause_scope__progressive_textualist, base_extractiveness, 2020, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1900, equality_clause_scope__progressive_textualist, suppression_requirement, 1900, 0.68).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.69).
narrative_ontology:measurement(equa_su_t1940, equality_clause_scope__progressive_textualist, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(equa_su_t1960, equality_clause_scope__progressive_textualist, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(equa_su_t1980, equality_clause_scope__progressive_textualist, suppression_requirement, 1980, 0.71).
narrative_ontology:measurement(equa_su_t2000, equality_clause_scope__progressive_textualist, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(equa_su_t2020, equality_clause_scope__progressive_textualist, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'equality_clause_scope' kernel. It focuses on democratic amendment as the primary mechanism for scope expansion, distinguishing it from 'restrictive_originalist' (static scope) and 'expansive_universalist' (judicial expansion) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
