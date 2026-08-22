% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Endogenous Reinterpretation of Marriage Commitment (Woodruff's Vision)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the reinterpretation of a divinely commanded
 *   practice (plural marriage) through an internal divine revelation
 *   (Woodruff's 1890 Manifesto) that effectively reversed the practice while
 *   preserving institutional legitimacy. The constraint is framed as a
 *   'tangled rope' because it genuinely coordinates the community's response
 *   to external pressure, but does so by extracting a cost from theological
 *   consistency and from dissenting members who must reconcile the apparent
 *   shift in divine will. The revelation narrative obscures the underlying
 *   doctrine-practice gap, allowing the institution to adapt without
 *   admitting a change in fundamental principles.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.6).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Endogenous Reinterpretation of Marriage Commitment (Woodruff's Vision)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, '8f148de2-c7aa-487a-a8f9-5a058738131a').
narrative_ontology:cs_kernel_codification('8f148de2-c7aa-487a-a8f9-5a058738131a', formalized).
narrative_ontology:cs_authority_grounding('8f148de2-c7aa-487a-a8f9-5a058738131a', lineage).
narrative_ontology:cs_interpretation_layer_present('8f148de2-c7aa-487a-a8f9-5a058738131a').
narrative_ontology:cs_reading_relation('8f148de2-c7aa-487a-a8f9-5a058738131a', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('8f148de2-c7aa-487a-a8f9-5a058738131a', marriage_commitment_reversal__practice_doctrine_gap, forecloses).
narrative_ontology:cs_axiom('8f148de2-c7aa-487a-a8f9-5a058738131a', foundational, divine_will_is_adaptable).
narrative_ontology:cs_axiom_status(divine_will_is_adaptable, holdable).
narrative_ontology:cs_axiom_grounding('8f148de2-c7aa-487a-a8f9-5a058738131a', divine_will_is_adaptable, theological).
narrative_ontology:cs_axiom('8f148de2-c7aa-487a-a8f9-5a058738131a', foundational, prophetic_revelation_is_binding).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('8f148de2-c7aa-487a-a8f9-5a058738131a', prophetic_revelation_is_binding, theological).
narrative_ontology:cs_reference_frame('8f148de2-c7aa-487a-a8f9-5a058738131a', continuous_revelation_paradigm).
narrative_ontology:cs_drift_state('8f148de2-c7aa-487a-a8f9-5a058738131a', post_manifesto_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8f148de2-c7aa-487a-a8f9-5a058738131a', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members_seeking_legitimacy).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissenting_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The prophet and senior leadership who received and promulgated the revelation. They maintain interpretive authority, preserve institutional legitimacy, and navigate the theological shift while maintaining member cohesion. Their identity is fused with the institution's divine mandate.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% The internal coherence and immutability of divine will. It 'pays' by being reinterpreted or seemingly contradicted, creating a challenge for those who seek a stable, unchanging divine law. Its 'costs' are borne by those who must reconcile the apparent shift in God's command.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).

% Members who struggle to reconcile the new revelation with prior doctrine or who feel their prior commitments are invalidated. They face social pressure to conform, potential excommunication, or the difficult choice of leaving the community. Their costs are social and spiritual.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissenting_members, payer,
    moderate, biographical, constrained, local).

% Members who accept the new revelation as a legitimate reinterpretation of divine will, allowing them to maintain their faith while complying with external pressures. They benefit from continued institutional belonging and the perceived divine sanction of their actions.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members_seeking_legitimacy, beneficiary,
    organized, biographical, identity_locked, global).

% Government and legal bodies that imposed pressure leading to the change in practice. They observe the internal religious response, assessing its compliance with secular law, but do not directly participate in the theological reinterpretation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, external_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective action of the religious community by providing a unified, divinely sanctioned path forward in response to changed circumstances, maintaining institutional cohesion and member loyalty.
% TRANSFER_FUNCTION: Transfers interpretive authority and institutional legitimacy to the current leadership, while transferring the burden of theological reconciliation and potential cognitive dissonance to the membership, particularly those who dissent.
% ABSENT_VOICES: Historical figures or prior doctrinal interpretations that would directly contradict the new revelation are effectively silenced or reinterpreted to fit the new narrative. Their 'objection' is absorbed into the ongoing theological discourse.
% DISAPPEARANCE_RATIONALE: If the endogenous reinterpretation vanished, the institutional leadership would face a severe legitimacy crisis, the community would fracture over the unresolved doctrinal conflict, and the prior practice would either reassert itself or be seen as having been abandoned without divine sanction, leading to widespread disaffiliation.
% FOUNDING_PROBLEM: The problem of reconciling a divinely commanded practice with overwhelming external legal and social pressure that threatened the institution's survival, without appearing to abandon God's will.
% FOUNDING_PROBLEM_CORROBORATION: The institutional leadership attests the problem is live, as the need to adapt divine commands to changing contexts is an ongoing challenge. External historians and sociologists corroborate the historical pressure and the institutional imperative to survive, supporting the 'live' status of the underlying problem, even if they dispute the divine nature of the solution.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the reinterpretation preserves the institution's core, but at the cost of theological consistency and the cognitive burden on members. Suppression is high (0.6) due to the strong social and spiritual pressure to accept the prophet's revelation as binding, limiting exit options for dissenting members. Theater ratio is low (0.2) because the revelation is presented as a genuine divine communication, not merely a performative act, though it serves a clear institutional function. The metrics reflect the internal dynamics of a commitment system adapting to external pressure through a divinely sanctioned reinterpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the institutional leadership, this is a divinely guided adaptation (rope-like coordination). From the perspective of theological consistency or dissenting members, it is an extraction of prior truth or personal conviction (snare-like extraction). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership benefits from maintaining interpretive authority and institutional survival (low d). Theological consistency (an abstract entity) and dissenting members bear the costs of reinterpretation and conformity (high d). Church members seeking legitimacy benefit from the clear path to compliance and continued belonging (low d). External authorities are observers, their d is analytical.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_immutability,
    'Is divine will immutable, or can it be reinterpreted/adapted by contemporary revelation in response to changing circumstances?',
    'Theological consensus across diverse faith traditions, or a meta-revelation clarifying the nature of divine communication.',
    'If immutable, the reinterpretation is a human-driven adaptation, not a divine one, increasing extractiveness from theological consistency. If adaptable, the reinterpretation is a legitimate coordination, reducing perceived extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_will_immutability, conceptual, 'Ambiguity regarding the nature of divine will and its susceptibility to reinterpretation.').

omega_variable(
    revelation_authenticity,
    'Was the Woodruff Manifesto a genuine divine revelation, or an institutional response to overwhelming external pressure framed as revelation?',
    'Historical analysis of internal church records, personal testimonies, and external political pressures leading up to the Manifesto, seeking to disentangle divine inspiration from institutional pragmatism.',
    'If primarily an institutional response, the theater_ratio would be higher, and the extractiveness from theological consistency more pronounced. If genuine, the coordination function is stronger, and extraction lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_authenticity, empirical, 'The authenticity of the divine revelation as distinct from institutional pragmatism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(marr_tr_t1892, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1892, 0.17).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1894, 0.18).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1896, 0.19).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1898, 0.2).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.4).
narrative_ontology:measurement(marr_be_t1892, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1892, 0.42).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1894, 0.43).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1896, 0.44).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1898, 0.45).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.55).
narrative_ontology:measurement(marr_su_t1892, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1892, 0.57).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1894, 0.58).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1896, 0.59).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1898, 0.6).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_commitment_reversal' kernel. It focuses on the internal theological reinterpretation as the mechanism for change, influencing how the external coercion and the doctrine-practice gap are understood.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
