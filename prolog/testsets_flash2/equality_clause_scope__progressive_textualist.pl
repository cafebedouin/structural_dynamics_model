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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope (Progressive Textualist Reading)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'progressive textualist' reading of a
 *   constitutional equality clause, where the principle of equality is
 *   present in the text but its application scope expands primarily through
 *   democratic amendment, not judicial reinterpretation. This reading seeks a
 *   balance between constitutional stability and adaptability, emphasizing
 *   popular sovereignty in defining fundamental rights. It is one reading of
 *   the 'equality_clause_scope' kernel, distinct from more restrictive or
 *   expansive interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.35).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.45).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.35).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.45).
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
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, 'bbfcacd5-518e-4f03-ad5e-504630f993cd').
narrative_ontology:cs_kernel_codification('bbfcacd5-518e-4f03-ad5e-504630f993cd', fixed_text).
narrative_ontology:cs_authority_grounding('bbfcacd5-518e-4f03-ad5e-504630f993cd', lineage).
narrative_ontology:cs_interpretation_layer_present('bbfcacd5-518e-4f03-ad5e-504630f993cd').
narrative_ontology:cs_reading_relation('bbfcacd5-518e-4f03-ad5e-504630f993cd', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('bbfcacd5-518e-4f03-ad5e-504630f993cd', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('bbfcacd5-518e-4f03-ad5e-504630f993cd', foundational, democratic_amendment_is_primary_legitimate_change_mechanism).
narrative_ontology:cs_axiom_status(democratic_amendment_is_primary_legitimate_change_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('bbfcacd5-518e-4f03-ad5e-504630f993cd', democratic_amendment_is_primary_legitimate_change_mechanism, conventional).
narrative_ontology:cs_axiom('bbfcacd5-518e-4f03-ad5e-504630f993cd', secondary, judicial_reinterpretation_is_secondary_to_democratic_will).
narrative_ontology:cs_axiom_status(judicial_reinterpretation_is_secondary_to_democratic_will, holdable).
narrative_ontology:cs_axiom_grounding('bbfcacd5-518e-4f03-ad5e-504630f993cd', judicial_reinterpretation_is_secondary_to_democratic_will, conventional).
narrative_ontology:cs_reference_frame('bbfcacd5-518e-4f03-ad5e-504630f993cd', constitutional_text_plus_amendment_tradition).
narrative_ontology:cs_drift_state('bbfcacd5-518e-4f03-ad5e-504630f993cd', contemporary_rights_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bbfcacd5-518e-4f03-ad5e-504630f993cd', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, democratic_majority).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, amendment_process_advocates).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, minority_groups_awaiting_amendment).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, judicial_activism_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the principle that fundamental changes to equality's scope require broad democratic consensus, typically expressed through the amendment process. This ensures their will, when sufficiently broad and sustained, can shape constitutional meaning.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, democratic_majority, beneficiary,
    institutional, generational, mobile, national).

% Bears the cost of delayed recognition of equality rights, as their inclusion within the clause's scope depends on the slow and difficult amendment process rather than swifter judicial interpretation. Their rights are recognized only when a supermajority agrees.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, minority_groups_awaiting_amendment, payer,
    powerless, generational, constrained, national).

% Actively champion the amendment process as the legitimate means for constitutional evolution, particularly regarding civil rights. They invest in building broad coalitions and public support for formal constitutional change.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, amendment_process_advocates, agenda_setter,
    organized, generational, mobile, national).

% Are structurally excluded from their preferred mechanism of rights expansion (judicial reinterpretation). They argue that the amendment process is too slow and often fails to protect vulnerable minorities, but this reading denies the legitimacy of their preferred path.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judicial_activism_advocates, excluded,
    moderate, biographical, constrained, national).

% Analyze the historical development and theoretical underpinnings of constitutional interpretation, observing how different readings of the equality clause shape legal and political outcomes. They assess the coherence and consequences of this reading.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates constitutional change by channeling fundamental expansions of rights through a supermajoritarian democratic process (amendment), ensuring broad societal consensus and legitimacy for evolving constitutional meaning.
% TRANSFER_FUNCTION: Transfers the authority for defining the scope of equality from unelected judicial bodies to the elected representatives and the people, requiring a higher bar for change but ensuring democratic buy-in.
% ABSENT_VOICES: Advocates for immediate judicial recognition of evolving equality rights are present in public discourse but are structurally sidelined by this reading's insistence on democratic amendment. They would argue for a more responsive, rights-protective judiciary.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the authority for constitutional change would immediately shift. Judicial interpretation would likely become the primary mechanism for expanding equality rights, leading to different legal outcomes and political contestation over judicial legitimacy. The balance of power between branches of government would fundamentally alter.
% FOUNDING_PROBLEM: The problem of how a foundational text, written in a specific historical context, can remain relevant and legitimate across generations as societal values and understandings of justice evolve, particularly regarding equality.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political scientists, from outside the immediate beneficiaries, corroborate that the tension between constitutional stability and adaptability, and the role of democratic versus judicial processes in resolving it, remains a central and live problem in constitutional theory and practice.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.35) is moderate, reflecting the cost to minority groups of waiting for democratic consensus, but not pure extraction as the mechanism is legitimate and open to all. Suppression (0.45) is moderate, as it actively suppresses alternative (judicial) paths to rights expansion. Theater ratio (0.1) is low, as the amendment process is a genuine, if difficult, mechanism for change. The claimed type is 'rope' because it genuinely coordinates constitutional evolution through a legitimate, albeit slow, process, with identifiable beneficiaries (democratic majority) and payers (minority groups awaiting amendment).
 *
 * PERSPECTIVAL GAP:
 *   The democratic majority perceives this as a legitimate and fair process for constitutional evolution, ensuring broad societal buy-in. Minority groups, however, experience it as a constraint that delays justice and imposes significant costs, highlighting the 'rope' aspect for some and 'tangled rope' or 'snare' for others, depending on their position and the urgency of their claims. The engine will compute these per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The democratic majority and amendment process advocates are beneficiaries (d near 0.0) as this reading empowers their preferred method of constitutional change. Minority groups awaiting amendment and judicial activism advocates are payers/targets (d near 1.0) as they bear the costs of delayed rights recognition or the suppression of their preferred interpretive method. Constitutional scholars are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling democratic constitutional evolution as pure extraction by emphasizing the genuine coordination function of the amendment process. While it imposes costs on some, it is not a snare because the mechanism for change is open and legitimate, even if difficult. It avoids the piton classification because the process is actively defended and utilized, not merely maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_process_efficacy,
    'Is the amendment process, in practice, a sufficiently responsive mechanism for expanding equality rights, or does its difficulty render it effectively inaccessible for marginalized groups?',
    'Empirical analysis of amendment success rates for civil rights issues over time, compared to judicial interventions. Case studies of specific amendment campaigns and their outcomes.',
    'If the process is found to be effectively inaccessible, the ''extractiveness'' and ''suppression'' metrics for minority groups would be higher, potentially shifting the classification towards a ''tangled_rope'' or ''snare'' from their seat. If it is responsive, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_efficacy, empirical, 'Assesses the practical accessibility and responsiveness of the amendment process for rights expansion.').

omega_variable(
    legitimacy_of_judicial_review,
    'What is the proper balance of authority between democratic processes and judicial review in defining constitutional equality? Is judicial reinterpretation a legitimate, or even necessary, mechanism for rights expansion?',
    'Conceptual analysis of constitutional theory, political philosophy, and historical precedent regarding the role of courts in a democracy. This is a normative debate without a purely empirical resolution.',
    'If judicial review is deemed a necessary check on democratic majorities for rights protection, this reading''s ''suppression'' of judicial activism would be viewed more negatively, potentially weakening its ''rope'' classification. If democratic amendment is prioritized, the ''rope'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_judicial_review, conceptual, 'Examines the normative justification for democratic vs. judicial roles in constitutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__progressive_textualist, theater_ratio, 0, 0.08).
narrative_ontology:measurement(equa_tr_t25, equality_clause_scope__progressive_textualist, theater_ratio, 25, 0.09).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__progressive_textualist, theater_ratio, 50, 0.1).
narrative_ontology:measurement(equa_tr_t75, equality_clause_scope__progressive_textualist, theater_ratio, 75, 0.1).
narrative_ontology:measurement(equa_tr_t100, equality_clause_scope__progressive_textualist, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__progressive_textualist, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(equa_be_t25, equality_clause_scope__progressive_textualist, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__progressive_textualist, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(equa_be_t75, equality_clause_scope__progressive_textualist, base_extractiveness, 75, 0.34).
narrative_ontology:measurement(equa_be_t100, equality_clause_scope__progressive_textualist, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__progressive_textualist, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(equa_su_t25, equality_clause_scope__progressive_textualist, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__progressive_textualist, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(equa_su_t75, equality_clause_scope__progressive_textualist, suppression_requirement, 75, 0.43).
narrative_ontology:measurement(equa_su_t100, equality_clause_scope__progressive_textualist, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equality_clause_scope' kernel. Each reading offers a distinct mechanism for constitutional evolution and defines the legitimate scope of the equality principle differently. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
