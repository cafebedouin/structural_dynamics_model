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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope (Progressive Textualist Reading)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'progressive textualist' reading of the
 *   equality clause's scope, which holds that while the Constitution contains
 *   an equality principle, its application scope expands primarily through
 *   the democratic amendment process, not through judicial reinterpretation.
 *   This reading emphasizes popular sovereignty and the high bar for
 *   constitutional change, balancing original limits with the capacity for
 *   democratic revision. It is one reading of the 'equality_clause_scope'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.3).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.4).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.3).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope (Progressive Textualist Reading)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '32feb359-7b5f-4372-ba3c-4d1cf5e8c460').
narrative_ontology:cs_kernel_codification('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', fixed_text).
narrative_ontology:cs_authority_grounding('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', lineage).
narrative_ontology:cs_interpretation_layer_present('32feb359-7b5f-4372-ba3c-4d1cf5e8c460').
narrative_ontology:cs_reading_relation('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', foundational, amendment_as_sole_legitimate_expansion_mechanism).
narrative_ontology:cs_axiom_status(amendment_as_sole_legitimate_expansion_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', amendment_as_sole_legitimate_expansion_mechanism, conventional).
narrative_ontology:cs_axiom('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', foundational, popular_sovereignty_over_judicial_fiat).
narrative_ontology:cs_axiom_status(popular_sovereignty_over_judicial_fiat, holdable).
narrative_ontology:cs_axiom_grounding('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', popular_sovereignty_over_judicial_fiat, deontological).
narrative_ontology:cs_reference_frame('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', constitutional_amendment_supremacy).
narrative_ontology:cs_drift_state('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('32feb359-7b5f-4372-ba3c-4d1cf5e8c460', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, democratic_majority).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, amendment_proponents).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, judicial_activists).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, minority_groups_seeking_immediate_rights).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, popular_sovereignty).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, constitutional_amendment_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the principle that fundamental rights expansion requires broad democratic consensus, expressed through the amendment process, reinforcing popular sovereignty.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, democratic_majority, beneficiary,
    institutional, generational, mobile, national).

% Advocates for expanding equality through constitutional amendments, accepting the high bar for change as legitimate. They are the primary agents of change within this framework.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, amendment_proponents, agenda_setter,
    organized, generational, constrained, national).

% Their preferred method of rights expansion (judicial reinterpretation) is constrained by this reading, which insists on democratic amendment. They bear the cost of a slower, more difficult path to universal equality.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judicial_activists, payer,
    powerful, biographical, constrained, national).

% Experience delays in the recognition of their rights, as the amendment process is slow and requires supermajority consensus. They bear the cost of waiting for democratic will to align with their claims.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, minority_groups_seeking_immediate_rights, payer,
    powerless, immediate, trapped, national).

% Analyze the historical development and philosophical underpinnings of constitutional equality, evaluating the consistency and implications of this reading against others.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expansion of fundamental rights by channeling it through a supermajority democratic process (constitutional amendment), ensuring broad societal consensus and legitimacy for changes to the foundational text.
% TRANSFER_FUNCTION: Transfers the authority for defining the scope of equality from unelected judicial bodies to the democratically elected representatives and the people, via the amendment process. It also transfers the burden of achieving consensus to proponents of expanded rights.
% ABSENT_VOICES: Those who believe in a 'living constitution' or a purely natural law basis for universal equality, independent of textual or democratic consent, are structurally marginalized. They would argue for a more immediate and expansive judicial role.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the process of rights expansion would likely shift dramatically, with courts potentially taking a much more active role in reinterpreting the equality clause, leading to different outcomes and a different balance of power between branches of government.
% FOUNDING_PROBLEM: The problem of how to reconcile the original, limited scope of constitutional equality with evolving societal norms and demands for broader inclusion, while maintaining the legitimacy of the constitutional framework.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political historians corroborate that this tension has been a persistent feature of American constitutionalism since its inception, with ongoing debates about the proper role of courts versus democratic processes in rights expansion.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).
:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is moderate, reflecting the 'cost' of a slower, more difficult path to rights expansion for some groups, but also the 'benefit' of broad democratic legitimacy. Suppression (0.4) is moderate, as it actively constrains judicial activism in this domain. Theater ratio (0.1) is low, as the amendment process is a genuine, functional mechanism for change, not merely performative. The values reflect a system that is functional but imposes real costs on those seeking rapid change.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the democratic majority, this constraint is a legitimate Rope, ensuring broad consensus for fundamental changes. From the perspective of judicial activists or minority groups, it can feel like a Snare, delaying or denying rights based on a procedural hurdle, even if the underlying principle of equality is acknowledged.
 *
 * DIRECTIONALITY LOGIC:
 *   The democratic majority and proponents of amendments are beneficiaries (d near 0.0) as their preferred method of change is privileged. Judicial activists and minority groups seeking immediate rights are payers (d near 1.0) as their avenues for change are constrained or delayed. Constitutional scholars are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling democratic constitutional change as either pure extraction (by acknowledging the coordination function of the amendment process) or as a purely natural law (by recognizing the need for active democratic consent to expand scope). It highlights the ongoing tension between stability and adaptation in constitutional interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_minority_rights,
    'Does the emphasis on democratic amendment for rights expansion adequately protect minority rights, or does it risk perpetuating historical injustices by requiring supermajority consent for fundamental equality?',
    'Comparative analysis of rights outcomes in systems with different amendment/judicial review balances, particularly for historically marginalized groups. Examination of the speed and efficacy of amendment processes in addressing new equality claims.',
    'If it risks perpetuating injustice, the ''extractiveness'' for minority groups is higher than measured, and the constraint leans more towards a Snare for those seats. If it proves robust, the ''Rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_minority_rights, conceptual, 'The tension between democratic legitimacy and the protection of minority rights in constitutional evolution.').

omega_variable(
    amendment_process_accessibility,
    'Is the constitutional amendment process genuinely accessible to popular movements, or has it become so difficult as to be practically inert, effectively freezing the scope of equality?',
    'Empirical study of successful and failed amendment campaigns, analyzing resource requirements, political hurdles, and public engagement. Comparison with other democratic mechanisms for change.',
    'If the process is inert, the ''suppression'' and ''extractiveness'' for proponents of change are higher, and the constraint functions more like a ''Piton'' or ''Snare'' by denying effective avenues for democratic expansion. If accessible, the ''Rope'' classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_process_accessibility, empirical, 'The practical accessibility and efficacy of the constitutional amendment process for expanding equality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__progressive_textualist, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__progressive_textualist, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.09).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__progressive_textualist, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__progressive_textualist, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__progressive_textualist, base_extractiveness, 1787, 0.2).
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__progressive_textualist, base_extractiveness, 1868, 0.25).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.28).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__progressive_textualist, base_extractiveness, 1965, 0.29).
narrative_ontology:measurement(equa_be_t2024, equality_clause_scope__progressive_textualist, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__progressive_textualist, suppression_requirement, 1787, 0.3).
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__progressive_textualist, suppression_requirement, 1868, 0.35).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.38).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__progressive_textualist, suppression_requirement, 1965, 0.39).
narrative_ontology:measurement(equa_su_t2024, equality_clause_scope__progressive_textualist, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, judicial_review_scope).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, civil_rights_legislation_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equality_clause_scope' kernel, alongside 'restrictive_originalist' and 'expansive_universalist'. Each reading defines a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
