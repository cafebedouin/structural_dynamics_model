% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Partition (Partition Reading)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'partition reading' of territorial
 *   legitimacy in the Israeli-Palestinian conflict, grounded in UN Resolution
 *   181 and subsequent international legal frameworks. It posits that both
 *   Israeli and Palestinian states derive legitimacy from international
 *   recognition within defined borders (e.g., 1948/1967 lines), making
 *   settlements beyond these lines illegitimate and a two-state solution
 *   structurally possible. The constraint is claimed as a Tangled Rope
 *   because it offers a coordination function (recognized statehood) but
 *   involves significant, actively enforced extraction from those whose
 *   claims are marginalized by this framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.6).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.7).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Partition (Partition Reading)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, 'c071c2a1-2d10-4b54-938b-99d705295557').
narrative_ontology:cs_kernel_codification('c071c2a1-2d10-4b54-938b-99d705295557', formalized).
narrative_ontology:cs_authority_grounding('c071c2a1-2d10-4b54-938b-99d705295557', lineage).
narrative_ontology:cs_interpretation_layer_present('c071c2a1-2d10-4b54-938b-99d705295557').
narrative_ontology:cs_reading_relation('c071c2a1-2d10-4b54-938b-99d705295557', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c071c2a1-2d10-4b54-938b-99d705295557', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('c071c2a1-2d10-4b54-938b-99d705295557', foundational, international_law_as_basis_for_sovereignty).
narrative_ontology:cs_axiom_status(international_law_as_basis_for_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c071c2a1-2d10-4b54-938b-99d705295557', international_law_as_basis_for_sovereignty, conventional).
narrative_ontology:cs_axiom('c071c2a1-2d10-4b54-938b-99d705295557', foundational, two_state_solution_as_just_outcome).
narrative_ontology:cs_axiom_status(two_state_solution_as_just_outcome, holdable).
narrative_ontology:cs_axiom_grounding('c071c2a1-2d10-4b54-938b-99d705295557', two_state_solution_as_just_outcome, deontological).
narrative_ontology:cs_reference_frame('c071c2a1-2d10-4b54-938b-99d705295557', un_resolution_181_framework).
narrative_ontology:cs_drift_state('c071c2a1-2d10-4b54-938b-99d705295557', contemporary_political_stalemate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c071c2a1-2d10-4b54-938b-99d705295557', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israeli_state_actors).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_authority_actors).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, un_member_states).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settlers_beyond_67_lines).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, two_state_solution_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, international_law_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from international recognition of its sovereignty within 1948/1967 borders, but faces pressure regarding settlements beyond these lines. Actively enforces its interpretation of security needs and territorial control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_state_actors, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israeli_state_actors, agenda_setter).

% Seeks full state recognition and sovereignty based on the partition plan and 1967 borders. Benefits from international support for a two-state solution, but struggles with effective territorial control and internal divisions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_authority_actors, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, palestinian_authority_actors, agenda_setter).

% Uphold the principle of international law and UN resolutions as the basis for territorial legitimacy. They provide diplomatic and financial support, and exert pressure for a two-state solution, but face challenges in enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, un_member_states, agenda_setter,
    institutional, civilizational, analytical, global).

% Bear the cost of displacement and loss of property, with their right of return often unaddressed by the partition framework. Their claims are recognized by international law but remain largely unrealized.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Their presence beyond the 1967 lines is considered illegitimate by this reading, creating legal and political vulnerability. They are often ideologically committed to their settlements, making exit difficult.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israeli_settlers_beyond_67_lines, payer,
    moderate, biographical, identity_locked, local).

% Analyze and interpret international law, UN resolutions, and historical documents to assess the legitimacy of territorial claims. Their work informs policy debates but does not directly enforce outcomes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for international recognition of two sovereign states, aiming to resolve territorial disputes through a legally defined partition and mutual recognition.
% TRANSFER_FUNCTION: Transfers claims of legitimate sovereignty and territorial control to both Israeli and Palestinian entities within internationally recognized borders, while imposing costs on those whose claims fall outside this framework.
% ABSENT_VOICES: Those who reject the legitimacy of the partition framework entirely, such as proponents of a single, binational state, or those who assert exclusive historical claims to the entire territory. Their perspectives are marginalized by the dominant international legal discourse.
% DISAPPEARANCE_RATIONALE: If the international legal framework for partition and state recognition vanished, the basis for diplomatic engagement, aid, and sanctions would collapse. The existing states' legitimacy would be fundamentally challenged, leading to a chaotic reorganization of territorial claims and power dynamics.
% FOUNDING_PROBLEM: The problem of competing national aspirations and territorial claims in Mandate Palestine, requiring a framework for self-determination and peaceful coexistence for both Jewish and Arab populations.
% FOUNDING_PROBLEM_CORROBORATION: UN resolutions, international diplomatic efforts, and the ongoing conflict itself corroborate that the problem of competing claims and the need for a resolution framework remain live. While the specific terms are contested, the underlying need for a legitimate basis for territorial division persists, attested by international bodies and numerous peace initiatives.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the costs borne by Palestinian refugees and Israeli settlers beyond the 1967 lines, whose claims are not fully accommodated by this framework. Suppression (0.7) is high due to the active diplomatic and military enforcement required to maintain the existing territorial arrangements and marginalize alternative claims. The theater ratio (0.4) indicates that while genuine diplomatic efforts occur, a significant portion of international discourse and enforcement is performative, maintaining the 'possibility' of a two-state solution without fully resolving the underlying power asymmetries. The metrics show fluctuations reflecting periods of intense conflict and peace negotiations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of UN member states, this framework is a necessary (if imperfect) Rope for international order and conflict resolution. For Palestinian refugees, it operates as a Snare, legitimizing their displacement while offering no concrete path to return. For Israeli settlers beyond the 1967 lines, it is also a Snare, delegitimizing their presence and threatening their identity-locked claims. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli and Palestinian Authority actors are beneficiaries as they gain international recognition and a basis for statehood, though they also bear costs in terms of territorial concessions or internal political challenges. UN member states act as agenda-setters, promoting and enforcing this framework. Palestinian refugees and Israeli settlers beyond the 1967 lines are payers, bearing the direct costs of displacement or legal vulnerability. International legal scholars act as observers, analyzing the framework's application.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_effectiveness_ambiguity,
    'Is the international legal framework for partition genuinely enforceable, or is its persistence primarily due to the balance of power among states?',
    'Analysis of cases where international legal consensus has been successfully enforced against a powerful state''s objections, or conversely, where it has consistently failed.',
    'If enforcement is primarily power-based, the constraint''s ''requires_active_enforcement'' is a misnomer, and its classification shifts closer to a Snare, as the coordination function is merely a cover for power projection. If genuinely enforceable, it retains its Tangled Rope character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_ambiguity, empirical, 'Ambiguity regarding the true source of the constraint''s persistence: legal authority vs. power dynamics.').

omega_variable(
    two_state_solution_viability,
    'Is the two-state solution, as envisioned by this partition reading, still a viable and implementable outcome given current demographic and political realities?',
    'Empirical assessment of territorial contiguity, demographic trends, and political will for implementation, as evaluated by independent geopolitical analysts.',
    'If no longer viable, the ''coordination function'' of this reading becomes purely theatrical, increasing the ''theater_ratio'' and shifting the constraint towards a Piton or Snare, as its stated purpose is unachievable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_solution_viability, empirical, 'Uncertainty about the practical feasibility of the partition reading''s core outcome.').

omega_variable(
    refugee_right_of_return_status,
    'How does the partition reading reconcile the right of return for Palestinian refugees (recognized by international law) with the demographic implications for a two-state solution?',
    'Legal and political analysis of proposed mechanisms for implementing the right of return (e.g., compensation, phased return, return to a Palestinian state) and their acceptance by all parties.',
    'If the right of return is structurally foreclosed by the partition reading''s implementation, the extraction from Palestinian refugees is higher and more permanent, pushing the constraint further towards a Snare. If a viable reconciliation exists, the extractiveness is mitigated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(refugee_right_of_return_status, conceptual, 'Conceptual tension between the partition framework and the right of return.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__partition_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy__partition_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__partition_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__partition_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy__partition_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__partition_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__partition_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy__partition_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__partition_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy' kernel. Each reading offers a distinct basis for territorial claims and statehood, leading to different classifications and stakeholder impacts. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
