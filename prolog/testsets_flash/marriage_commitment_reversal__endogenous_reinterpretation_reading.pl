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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Endogenous Reinterpretation of Marriage Commitment (Woodruff's Vision)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the institutional shift in the Church of Jesus
 *   Christ of Latter-day Saints regarding plural marriage, specifically
 *   through the lens of President Wilford Woodruff's 1890 Manifesto (Official
 *   Declaration 1) and subsequent clarifications, framed as an internal
 *   divine revelation. This reading emphasizes the reinterpretation of God's
 *   will under changed circumstances, allowing the institution to adapt while
 *   maintaining the prophet's interpretive authority. It is one reading of
 *   the broader 'marriage_commitment_reversal' kernel, which also includes
 *   interpretations focusing on external coercion or a doctrine-practice gap.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Agenda setter (institutional/arbitrage) — maintains interpretive authority.
 *   - theological_consistency: Victim (powerless/civilizational) — bears the cost of doctrinal shift.
 *   - church_members_seeking_social_acceptance: Beneficiary (organized/constrained) — benefits from reduced social stigma.
 *   - members_committed_to_prior_doctrine: Payer (moderate/identity_locked) — bears the cost of abandoning deeply held beliefs.
 *   - federal_government: Observer (institutional/analytical) — external pressure driving the change.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.6).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Endogenous Reinterpretation of Marriage Commitment (Woodruff's Vision)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, '0ee09839-39cb-47eb-9212-fed0f1239573').
narrative_ontology:cs_kernel_codification('0ee09839-39cb-47eb-9212-fed0f1239573', formalized).
narrative_ontology:cs_authority_grounding('0ee09839-39cb-47eb-9212-fed0f1239573', lineage).
narrative_ontology:cs_interpretation_layer_present('0ee09839-39cb-47eb-9212-fed0f1239573').
narrative_ontology:cs_reading_relation('0ee09839-39cb-47eb-9212-fed0f1239573', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('0ee09839-39cb-47eb-9212-fed0f1239573', marriage_commitment_reversal__practice_doctrine_gap, forecloses).
narrative_ontology:cs_axiom('0ee09839-39cb-47eb-9212-fed0f1239573', foundational, divine_will_is_dynamic).
narrative_ontology:cs_axiom_status(divine_will_is_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('0ee09839-39cb-47eb-9212-fed0f1239573', divine_will_is_dynamic, theological).
narrative_ontology:cs_axiom('0ee09839-39cb-47eb-9212-fed0f1239573', foundational, prophetic_revelation_is_supreme).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('0ee09839-39cb-47eb-9212-fed0f1239573', prophetic_revelation_is_supreme, theological).
narrative_ontology:cs_reference_frame('0ee09839-39cb-47eb-9212-fed0f1239573', prophetic_guidance_adaptive_to_circumstance).
narrative_ontology:cs_drift_state('0ee09839-39cb-47eb-9212-fed0f1239573', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0ee09839-39cb-47eb-9212-fed0f1239573', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members_seeking_social_acceptance).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, members_committed_to_prior_doctrine).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (aligning church practice with societal norms to ensure institutional survival) but involves asymmetric extraction. Extraction is moderate (0.45) as it extracts from theological consistency and from members deeply committed to the prior doctrine, who must reconcile the shift. Suppression is moderate (0.6) as internal dissent was managed through the authority of revelation. Theater ratio is high (0.7) because the 'revelation' narrative serves to obscure the underlying strategic adaptation to external pressure, maintaining a performative adherence to divine guidance while fundamentally altering practice. The temporal measurements show increasing extractiveness and theater as the reinterpretation solidifies and the institutional narrative becomes more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional leadership, the constraint is a necessary, divinely guided adaptation (closer to a Rope). From the perspective of theological consistency and members committed to prior doctrine, it represents a significant, costly shift that strains belief (closer to a Snare). The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is a beneficiary (d=0.0-0.1) as the constraint preserves their authority and the institution's viability. Theological consistency is a victim (d=0.9-1.0) as its integrity is compromised. Church members seeking social acceptance are beneficiaries (d=0.1-0.2) as they gain relief from persecution. Members committed to prior doctrine are payers (d=0.7-0.8) due to the identity-locked nature of their commitment and the cost of reconciling the doctrinal shift. The federal government is an observer (d=0.5) as its pressure is external to the internal reinterpretation mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling a strategic adaptation as a pure Mountain (unchanging divine will) or a pure Snare (simple coercion). By framing the change as revelation, the institution avoids the appearance of pure coercion, but the underlying adaptive function and the costs borne by theological consistency and some members reveal its Tangled Rope nature. The high theater ratio indicates that the 'revelation' narrative is a significant part of its maintenance, obscuring the strategic elements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_ambiguity,
    'Was Woodruff''s September 23 vision a genuine divine revelation, or a strategic reinterpretation framed as such to preserve institutional legitimacy amidst external pressure?',
    'Historical analysis of internal church records, personal diaries, and contemporary accounts for evidence of pre-vision internal theological debate or external political negotiation that shaped the ''revelation''s'' content.',
    'If a strategic reinterpretation, the constraint''s extractiveness from theological consistency is higher, and its suppression of internal dissent is more pronounced, shifting its classification closer to a Snare. If genuine, it reinforces the Mountain-like aspect of divine will, but still functions as a Tangled Rope due to the beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_authenticity_ambiguity, empirical, 'Ambiguity regarding the authenticity of the divine revelation vs. strategic reinterpretation.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint best understood as an endogenous reinterpretation of God''s will, or as an exogenous override of practice, or as a persistent gap between doctrine and practice?',
    'Analysis of the primary source texts and institutional pronouncements: does the text explicitly claim divine intervention (endogenous), or merely suspend practice due to external threat (exogenous), or maintain the doctrine while suspending practice without explicit justification (gap)?',
    'If exogenous, the constraint''s extractiveness from members is higher due to coercion. If a practice-doctrine gap, the constraint''s theater_ratio is higher as the institution maintains a performative adherence to a non-practiced doctrine. This reading (endogenous) preserves institutional legitimacy by framing the change as divine will.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''marriage_commitment_reversal'' kernel, specifically the ''endogenous_reinterpretation_reading''. Sibling readings include ''exogenous_override_reading'' and ''practice_doctrine_gap''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1905).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 10, 0.65).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 15, 0.7).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 15, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel. Each reading offers a distinct structural interpretation of the same historical event, with different beneficiaries, victims, and classifications. They are linked to capture the contested nature of the institutional shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
