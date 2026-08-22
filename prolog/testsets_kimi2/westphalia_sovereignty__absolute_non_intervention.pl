% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Sovereignty as Categorical Territorial Inviolability
 *   domain: political/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the absolute_non_intervention reading of the
 *   westphalia_sovereignty kernel. It encodes territorial inviolability as a
 *   categorical legal norm, treating external interference in domestic
 *   affairs as per se illegitimate regardless of internal conduct. The norm
 *   is formalized in the UN Charter and sustained by an active enforcement
 *   architecture of diplomatic recognition, legal sanctions for violators,
 *   and great-power deterrence. State elites are the primary beneficiaries,
 *   gaining territorial monopoly and impunity for internal repression, while
 *   populations under authoritarian control constitute the victim set,
 *   structurally denied external protection even in the face of mass
 *   atrocities. The constraint is claimed as tangled_rope because it retains
 *   a genuine coordination functionâpreventing universal interstate
 *   intervention and stabilizing territorial boundariesâwhile
 *   simultaneously extracting from the most vulnerable through asymmetric
 *   shielding of abusive regimes.
 *
 * KEY AGENTS:
 *   - state_elites: Primary agenda-setter and beneficiary (institutional/arbitrage) â enforce the norm through treaty architecture and diplomatic practice, collecting territorial monopoly and impunity.
 *   - populations_under_authoritarian_control: Primary payer (powerless/trapped) â bear the cost of shielded internal repression with no external recourse.
 *   - great_powers: Secondary beneficiary (powerful/arbitrage) â benefit from systemic stability and selective enforcement leverage.
 *   - humanitarian_organizations: Excluded voice (moderate/constrained) â structurally sidelined by the categorical norm's state-centric subject doctrine.
 *   - international_legal_scholars: Analytical observer (analytical/analytical) â map the norm's operation and contestation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.72).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.68).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Sovereignty as Categorical Territorial Inviolability").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "political/international_law").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '12022980-4af7-41c0-9e68-0d9115632c77').
narrative_ontology:cs_kernel_codification('12022980-4af7-41c0-9e68-0d9115632c77', formalized).
narrative_ontology:cs_authority_grounding('12022980-4af7-41c0-9e68-0d9115632c77', lineage).
narrative_ontology:cs_interpretation_layer_present('12022980-4af7-41c0-9e68-0d9115632c77').
narrative_ontology:cs_reading_relation('12022980-4af7-41c0-9e68-0d9115632c77', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_reading_relation('12022980-4af7-41c0-9e68-0d9115632c77', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('12022980-4af7-41c0-9e68-0d9115632c77', foundational, territorial_inviolability_per_se).
narrative_ontology:cs_axiom_status(territorial_inviolability_per_se, holdable).
narrative_ontology:cs_axiom_grounding('12022980-4af7-41c0-9e68-0d9115632c77', territorial_inviolability_per_se, conventional).
narrative_ontology:cs_reference_frame('12022980-4af7-41c0-9e68-0d9115632c77', classical_westphalian_sovereignty).
narrative_ontology:cs_drift_state('12022980-4af7-41c0-9e68-0d9115632c77', post_r2p_consensus_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('12022980-4af7-41c0-9e68-0d9115632c77', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, great_powers).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise territorial monopoly and control the normative architecture of international recognition. They draft treaties, invoke UN Charter Article 2(7), and deploy diplomatic and legal coercion to shield internal conduct from external scrutiny. They can selectively violate the norm when advantageous while insisting on its absoluteness for others.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, state_elites, beneficiary).

% Bear the cost of internal repression that is structurally shielded from external intervention. They are denied legal standing in the interstate system and cannot trigger enforcement mechanisms without state consent. Exit is geographically and politically blocked by the very territorial monopoly the norm guarantees.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, payer,
    powerless, immediate, trapped, national).

% Benefit from systemic stability and the reduced threat of constant interstate warfare. They retain leverage to intervene selectively when their interests align, while invoking the norm to constrain rival interventions. The categorical rule preserves a favorable balance of power and limits challengers.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, great_powers, beneficiary,
    powerful, generational, arbitrage, global).

% Would advocate for conditional or humanitarian intervention but are structurally sidelined by the state-centric architecture. Their access to atrocity zones depends on state consent, and their legal arguments for protection are overridden by per se inviolability claims in UN forums.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, humanitarian_organizations, excluded,
    moderate, biographical, constrained, global).

% Map the contest between absolute, conditional, and graded readings of sovereignty. They document the norm's historical drift from a war-prevention mechanism to an impunity shield, and assess the coherence of rival legal interpretations without institutional power to enforce either.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents universal interstate intervention by establishing reciprocal territorial standstill, solving the collective-action problem of constant cross-border warfare and dynastic/ideological expansion.
% TRANSFER_FUNCTION: Transfers impunity and territorial monopoly from the international community to state elites, while transferring the cost of internal repression from the interstate system to populations trapped within borders.
% ABSENT_VOICES: Populations under authoritarian control are excluded from the forums where sovereignty norms are articulated; their absence is structurally guaranteed by the legal doctrine that only states are subjects of international law.
% DISAPPEARANCE_RATIONALE: If categorical territorial inviolability vanished overnight, the UN Charter's Article 2(7) architecture would collapse, humanitarian intervention frameworks would activate, state elites would face external accountability for internal conduct, and the interstate system would reorganize around conditional or graded sovereignty.
% FOUNDING_PROBLEM: The Thirty Years' War and subsequent European conflicts demonstrated that religious and dynastic intervention into domestic affairs produced chronic, catastrophic warfare; the arrangement was built to suppress universalist intervention and stabilize territorial boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Peace of Westphalia attest the founding problem from outside the beneficiary set. Contemporary human rights scholars and some Global South theorists contest that the seventeenth-century problem justifies twenty-first-century shielding of atrocities; the International Commission on Intervention and State Sovereignty documented the shift in problem structure from interstate war to internal atrocity.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the norm systematically transfers the cost of internal atrocities from the international community to trapped populations, while the coordination benefit accrues to state elites and great powers. Suppression (0.68) reflects active legal and diplomatic enforcement that excludes rival frameworks like R2P. Theater_ratio (0.50 at interval end) captures the increasing ritualistic invocation of sovereignty at the UN as the functional coordination rationale (preventing interstate war) weakens relative to the protective function for abusive regimes. Accessibility_collapse (0.65) indicates that while humanitarian intervention alternatives exist in theory, they are legally and politically collapsed in practice for most victim populations. Resistance (0.55) registers sustained human rights advocacy and liberal state coalitions pushing conditional-responsibility alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the state-elite seat, the constraint appears as the indispensable coordination mechanism of interstate order; from the subjugated-population seat, it appears as an actively enforced shield for their oppressors. The engine computes this divergence from the structural data: identical constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and great powers are declared beneficiaries with arbitrage-grade exit (they can violate the norm when convenient, e.g., Kosovo 1999, Iraq 2003), so their derived directionality sits near the beneficiary end, damping effective extraction into subsidy or low cost. Populations_under_authoritarian_control are declared victims with trapped exit and powerless status, placing their directionality near the full-target end and amplifying effective extraction. Humanitarian organizations are excluded rather than targeted, so they fall outside the beneficiary/victim derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâchronic interstate warfare driven by universalist interventionâwas substantially solved by the post-1945 order. However, the arrangement persists and has been repurposed to shield internal atrocities. The R5 genealogy flags a mismatch: founding_problem_status is contested while disappearance_verdict is world_rearranges, indicating the constraint may be drifting toward piton or snare. The theater_ratio measurements show this drift: performative maintenance rises as the live coordination rationale fades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_function_atrophy,
    'Has the coordination function of preventing interstate war atrophied relative to the extraction function of shielding internal atrocities?',
    'Comparative historical analysis of intervention frequency and humanitarian outcomes pre- and post-UN Charter; measurement of atrocity prevalence in sovereign-shielded states.',
    'If coordination remains dominant, the constraint reads as rope; if extraction dominates, classification shifts toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_atrophy, empirical, 'Whether the non-intervention norm still solves a live coordination problem or primarily shields extraction.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the absolute_non_intervention reading foreclose the conditional_responsibility reading within a single legal framework, or do they coexist in tension?',
    'Jurisprudential analysis of whether Article 2(7) and Chapter VII can be coherently reconciled with R2P doctrine in one institutional framework.',
    'If foreclosed, the kernel is logically fractured and authority_grounding may shift to distributed; if coexistent, the framework accommodates rival readings as live coalitional positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between absolute and conditional sovereignty readings.').

omega_variable(
    victim_exclusion_mechanism,
    'Are subjugated populations structurally excluded from the international legal subject position, or merely disadvantaged within it?',
    'Analysis of international legal personality doctrine; whether non-state actors can ever trigger enforcement without state consent.',
    'If structurally excluded, the constraint''s victim set is inherent to the architecture; if merely disadvantaged, reform could shift the classification toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_exclusion_mechanism, conceptual, 'Whether victimhood is built into the legal architecture or contingent on practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalia_abs_tr_t0, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.25).
narrative_ontology:measurement(westphalia_abs_tr_t15, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 15, 0.3).
narrative_ontology:measurement(westphalia_abs_tr_t30, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 30, 0.35).
narrative_ontology:measurement(westphalia_abs_tr_t45, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 45, 0.42).
narrative_ontology:measurement(westphalia_abs_tr_t60, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 60, 0.48).
narrative_ontology:measurement(westphalia_abs_tr_t75, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 75, 0.5).

% Extraction over time
narrative_ontology:measurement(westphalia_abs_be_t0, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(westphalia_abs_be_t15, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(westphalia_abs_be_t30, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(westphalia_abs_be_t45, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(westphalia_abs_be_t60, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(westphalia_abs_be_t75, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(westphalia_abs_su_t0, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(westphalia_abs_su_t15, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(westphalia_abs_su_t30, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(westphalia_abs_su_t45, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 45, 0.62).
narrative_ontology:measurement(westphalia_abs_su_t60, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(westphalia_abs_su_t75, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the westphalia_sovereignty kernel. The absolute_non_intervention reading has high epsilon due to its shielding effect; the conditional_responsibility reading lowers epsilon for protected populations but raises it for state elites; the graded_sovereignty reading distributes epsilon variably across the capacity spectrum. Each reading requires its own constraint story per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
