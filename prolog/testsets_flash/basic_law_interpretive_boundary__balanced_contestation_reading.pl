% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary: Balanced Contestation Reading
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint describes the 'balanced contestation' reading of the
 *   interpretive boundary for Basic Laws in a constitutional system where
 *   both the judiciary and the legislature hold legitimate but bounded
 *   authority. Courts interpret within their jurisdictional domain, while the
 *   legislature retains ultimate sovereign power, constrained by
 *   international obligations and norms of judicial independence. This
 *   reading emphasizes institutional dialogue and negotiation, where neither
 *   institution is fully dominant, and the effective extraction varies by
 *   policy domain. This is one reading of the
 *   'basic_law_interpretive_boundary' kernel, distinct from
 *   'judicial_supremacy_reading' and 'parliamentary_sovereignty_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.45).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.3).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary: Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, 'a9464063-fbd4-4358-93ba-84a4771c8ed8').
narrative_ontology:cs_kernel_codification('a9464063-fbd4-4358-93ba-84a4771c8ed8', fixed_text).
narrative_ontology:cs_authority_grounding('a9464063-fbd4-4358-93ba-84a4771c8ed8', lineage).
narrative_ontology:cs_interpretation_layer_present('a9464063-fbd4-4358-93ba-84a4771c8ed8').
narrative_ontology:cs_reading_relation('a9464063-fbd4-4358-93ba-84a4771c8ed8', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9464063-fbd4-4358-93ba-84a4771c8ed8', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('a9464063-fbd4-4358-93ba-84a4771c8ed8', foundational, institutional_dialogue_is_constitutional_norm).
narrative_ontology:cs_axiom_status(institutional_dialogue_is_constitutional_norm, holdable).
narrative_ontology:cs_axiom_grounding('a9464063-fbd4-4358-93ba-84a4771c8ed8', institutional_dialogue_is_constitutional_norm, conventional).
narrative_ontology:cs_axiom('a9464063-fbd4-4358-93ba-84a4771c8ed8', foundational, neither_branch_holds_absolute_interpretive_finality).
narrative_ontology:cs_axiom_status(neither_branch_holds_absolute_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('a9464063-fbd4-4358-93ba-84a4771c8ed8', neither_branch_holds_absolute_interpretive_finality, deontological).
narrative_ontology:cs_reference_frame('a9464063-fbd4-4358-93ba-84a4771c8ed8', post_founding_constitutional_consensus).
narrative_ontology:cs_drift_state('a9464063-fbd4-4358-93ba-84a4771c8ed8', contemporary_political_polarization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a9464063-fbd4-4358-93ba-84a4771c8ed8', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_stability).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, public_trust_in_institutions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_efficiency).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_finality).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, public_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts laws and holds ultimate sovereign power, but its legislative output is subject to judicial review. Engages in dialogue with the judiciary over constitutional interpretation and can initiate amendments to Basic Laws.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% Interprets Basic Laws and reviews legislation for constitutionality. Its rulings are authoritative within its jurisdictional domain but can be challenged by legislative responses or constitutional amendments.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Rely on both institutions for stable governance and constitutional protection. Bear the costs of institutional conflict (e.g., policy uncertainty, delays) but also benefit from checks and balances.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, public_citizens, payer,
    organized, biographical, constrained, national).

% Analyze the dynamics of constitutional interpretation and the balance of power between institutions. Their work informs public discourse and legal arguments but does not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% Influence the domestic constitutional framework through international obligations and norms of judicial independence. While not directly part of the domestic interpretive process, their views constrain the legislature's sovereign power and support judicial independence.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_legal_bodies, excluded,
    institutional, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of constitutional authority between the legislative and judicial branches, ensuring neither branch unilaterally defines the constitutional order and maintaining a system of checks and balances.
% TRANSFER_FUNCTION: Transfers interpretive authority and policy finality between the legislature and judiciary, depending on the specific issue and institutional dynamics. It also transfers the burden of constitutional justification between them.
% ABSENT_VOICES: While international legal bodies influence the framework, they are not direct participants in the domestic interpretive dialogue. Their direct participation might shift the balance, particularly regarding human rights and judicial independence.
% DISAPPEARANCE_RATIONALE: If this 'balanced contestation' disappeared, the system would likely collapse into either judicial supremacy or parliamentary sovereignty, fundamentally altering the constitutional structure, the nature of governance, and the rights of citizens. The current institutional dialogue would cease, leading to a new, likely more extractive, constitutional order.
% FOUNDING_PROBLEM: The founding problem was to establish a stable constitutional order that balanced democratic accountability (legislature) with the protection of fundamental rights and the rule of law (judiciary), preventing the concentration of absolute power in any single branch.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, legal practitioners, and public discourse consistently attest that the problem of balancing democratic will with constitutional limits remains live. While the specific manifestations evolve, the fundamental tension persists, corroborated by ongoing debates and institutional conflicts, not just by the benefiting institutions themselves.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).
:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the exercise of power between two powerful institutions, preventing either from unilaterally dominating the constitutional interpretation. However, it also involves asymmetric extraction: the ongoing negotiation and potential for judicial review impose costs on legislative efficiency, while legislative responses and potential overrides impose costs on judicial finality. Active enforcement is required to maintain this balance, as both institutions constantly test the boundaries. The relatively low extractiveness (0.45) and suppression (0.30) reflect the 'balanced' nature of the contestation, where neither side can fully suppress the other, leading to a dynamic equilibrium rather than outright capture. Theater ratio is low (0.10) as the contestation is genuine, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature, the constraint imposes a cost on its ability to enact policy without judicial interference, leading to a higher effective extraction. From the judiciary's perspective, the constraint provides a legitimate basis for review but also limits its finality, as legislative responses or constitutional amendments are possible. Both institutions experience a degree of extraction, but the balance prevents one from becoming a pure victim or beneficiary, consistent with a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'constitutional_stability' and 'public_trust_in_institutions' are beneficiaries, as the constraint's operation prevents constitutional crises. 'Legislative_efficiency' and 'judicial_finality' are victims, as their respective goals are constrained by the ongoing institutional dialogue. The legislature and judiciary, as institutional actors, have 'constrained' exit options; they cannot simply opt out of the constitutional framework. Their directionality is thus modulated by their specific roles as both beneficiaries of constitutional order and targets of the interpretive contestation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the ongoing institutional dialogue as either pure coordination (Rope) or pure extraction (Snare). If it were a Rope, it would imply a frictionless, mutually beneficial arrangement, which ignores the real costs and power struggles. If it were a Snare, it would imply one institution is purely extracting from the other, which contradicts the 'balanced contestation' premise. The Tangled Rope accurately captures the hybrid nature of genuine coordination with inherent, asymmetric costs that require active maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''balanced contestation'' reading of the Basic Law interpretive boundary, or does it lean towards judicial supremacy or parliamentary sovereignty in practice?',
    'Empirical analysis of legislative responses to judicial review, judicial deference rates, and public opinion on institutional legitimacy over time. If one institution consistently overrides the other, reclassify.',
    'If it leans towards judicial supremacy, the constraint becomes more extractive for the legislature; if towards parliamentary sovereignty, it becomes more extractive for the judiciary and potentially citizens seeking constitutional protection. This reading''s classification as a Tangled Rope depends on the balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Ambiguity in the practical balance of power between institutions in interpreting Basic Laws.').

omega_variable(
    policy_domain_variability,
    'Does the ''balanced contestation'' hold across all policy domains, or does one institution exert more influence in specific areas (e.g., security vs. economic policy)?',
    'Comparative case studies of judicial review outcomes and legislative responses across different policy sectors. If significant domain-specific asymmetry is found, decompose into multiple constraints.',
    'If the balance varies significantly by domain, the overall extractiveness and suppression metrics for this single constraint are an average that obscures localized snares or ropes. Decomposition would yield more precise classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_domain_variability, empirical, 'Variability of institutional balance across different policy domains.').

omega_variable(
    sibling_reading_impact_judicial_supremacy,
    'How would the constraint''s structure change if the ''judicial_supremacy_reading'' were adopted?',
    'Conceptual analysis of legal precedent and constitutional theory. The judicial_supremacy_reading would shift the constraint''s extractiveness and suppression significantly towards the legislature, making it a Snare for legislative power.',
    'The constraint would become a Snare for the legislature, with higher extractiveness and suppression, and the judiciary would become a clear beneficiary with near-arbitrage exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_judicial_supremacy, conceptual, 'Impact of adopting the judicial supremacy reading.').

omega_variable(
    sibling_reading_impact_parliamentary_sovereignty,
    'How would the constraint''s structure change if the ''parliamentary_sovereignty_reading'' were adopted?',
    'Conceptual analysis of legal precedent and constitutional theory. The parliamentary_sovereignty_reading would shift the constraint''s extractiveness and suppression significantly towards the judiciary, making it a Snare for judicial independence.',
    'The constraint would become a Snare for the judiciary, with higher extractiveness and suppression, and the legislature would become a clear beneficiary with near-arbitrage exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_parliamentary_sovereignty, conceptual, 'Impact of adopting the parliamentary sovereignty reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'basic_law_interpretive_boundary' kernel. Each reading represents a distinct structural claim about the balance of power in constitutional interpretation, with different extractiveness profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
