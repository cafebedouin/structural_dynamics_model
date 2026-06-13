% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Lapsed Alternatives Reading)
 *   domain: technology_history/industrial_standards
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   as a 'Rope' where its value comes from coordination, and alternatives
 *   have lapsed due to a failure to reach critical mass, rather than active
 *   suppression. It is one reading of the 'qwerty_persistence' kernel. The
 *   constraint is not actively enforced by a central authority, but by the
 *   self-reinforcing network effects of adoption. The
 *   'lapsed_alternatives_reading' emphasizes the coordination value and the
 *   high switching costs for individuals and manufacturers, leading to a low
 *   extractiveness and suppression profile.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.15).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Keyboard Layout Persistence (Lapsed Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '40d8174a-f667-4b69-b3f5-87c47b505ba5').
narrative_ontology:cs_kernel_codification('40d8174a-f667-4b69-b3f5-87c47b505ba5', implicit).
narrative_ontology:cs_authority_grounding('40d8174a-f667-4b69-b3f5-87c47b505ba5', practice).
narrative_ontology:cs_reading_relation('40d8174a-f667-4b69-b3f5-87c47b505ba5', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('40d8174a-f667-4b69-b3f5-87c47b505ba5', foundational, standard_value_from_network_effects).
narrative_ontology:cs_axiom_status(standard_value_from_network_effects, holdable).
narrative_ontology:cs_axiom_grounding('40d8174a-f667-4b69-b3f5-87c47b505ba5', standard_value_from_network_effects, empirically_contingent).
narrative_ontology:cs_axiom('40d8174a-f667-4b69-b3f5-87c47b505ba5', foundational, alternatives_fail_due_to_critical_mass).
narrative_ontology:cs_axiom_status(alternatives_fail_due_to_critical_mass, holdable).
narrative_ontology:cs_axiom_grounding('40d8174a-f667-4b69-b3f5-87c47b505ba5', alternatives_fail_due_to_critical_mass, empirically_contingent).
narrative_ontology:cs_reference_frame('40d8174a-f667-4b69-b3f5-87c47b505ba5', self_organizing_coordination).
narrative_ontology:cs_drift_state('40d8174a-f667-4b69-b3f5-87c47b505ba5', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('40d8174a-f667-4b69-b3f5-87c47b505ba5', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, typing_tutors_and_educators).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, computer_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce keyboards following the QWERTY layout due to established tooling, supply chains, and consumer demand. While they could theoretically switch to alternative layouts, the cost of retooling and market education is prohibitive without a coordinated industry-wide shift.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, agenda_setter,
    organized, generational, constrained, global).

% Are accustomed to the QWERTY layout through decades of use and learning. Switching to an alternative layout would require significant relearning, which is a personal cost. They benefit from the ubiquity of QWERTY, allowing them to use any keyboard, but are 'locked in' by their learned skill.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, computer_users, payer,
    moderate, biographical, identity_locked, global).

% Promote more ergonomically efficient keyboard layouts (e.g., Dvorak, Colemak). They face immense barriers to adoption due to the entrenched QWERTY standard, lacking the critical mass needed to make their alternatives viable for mass production or widespread learning.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates, excluded,
    powerless, generational, trapped, global).

% Benefit from a stable, widely adopted standard for teaching typing. While they could teach alternative layouts, the demand is overwhelmingly for QWERTY, making it the de facto curriculum. They are constrained by market demand but benefit from its predictability.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typing_tutors_and_educators, beneficiary,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that any user can sit down at any keyboard and type, and that manufacturers can produce keyboards knowing there is a user base. It coordinates the physical interface between human and machine.
% TRANSFER_FUNCTION: Primarily transfers the cost of learning and adaptation from the collective (everyone learning a new layout) to the individual (everyone learning QWERTY once). It also transfers the cost of retooling from manufacturers to the maintenance of the existing standard.
% ABSENT_VOICES: Advocates for more efficient alternative layouts are effectively absent from the mainstream conversation, their arguments for superior ergonomics failing to overcome the inertia of the installed base and learned behavior. They would argue for a coordinated shift to a better standard.
% DISAPPEARANCE_RATIONALE: If the QWERTY standard vanished overnight, there would be immediate chaos in human-computer interaction. Manufacturers would have no default to produce, users would be unable to type, and a new standard would eventually emerge, but only after a period of significant disruption and re-coordination.
% FOUNDING_PROBLEM: The original problem was to create a functional, mass-producible typewriter layout that prevented typebars from jamming, given the mechanical limitations of 19th-century technology.
% FOUNDING_PROBLEM_CORROBORATION: The original mechanical problem is long dead due to advances in technology. Ergonomic studies and alternative layout designs, corroborated by independent research institutions and academic studies, demonstrate that QWERTY is no longer optimal for typing speed or comfort. The persistence is now due to coordination inertia, not the original technical constraint.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the 'cost' is primarily the switching cost for individuals and manufacturers, which is diffuse and not captured by a specific beneficiary. Suppression is negligible (0.05) as there is no active enforcement mechanism preventing the adoption of alternatives; rather, alternatives simply fail to gain traction. Theater ratio is 0.0 as there's no performative maintenance. Accessibility collapse is high (0.8) because, while alternatives exist, their practical accessibility collapses due to the lack of widespread support and the high individual cost of switching. Resistance is low (0.1) because most users accept the standard as a given, and organized resistance is minimal.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of most users and manufacturers, QWERTY is simply 'the way things are' – a stable, beneficial standard. From the perspective of alternative layout advocates, it's a suboptimal standard that persists due to inertia, preventing the adoption of superior alternatives. This reading emphasizes the coordination function and the absence of active extraction, contrasting with the 'incumbent_preservation_reading' which would highlight active defense by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers and typing educators are beneficiaries in that they operate within a stable, predictable standard, reducing their coordination costs. Computer users are payers in that they bear the individual switching cost of learning QWERTY, but also benefit from its ubiquity. Alternative layout advocates are excluded, as the system's inertia prevents their participation. No single entity captures significant extraction; the 'cost' is a diffuse coordination overhead.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_ambiguity,
    'Is the persistence of QWERTY primarily a coordination problem with diffuse costs, or is there an identifiable beneficiary actively defending the standard to extract rents?',
    'Analysis of lobbying efforts, patent defense, and investment patterns by major keyboard manufacturers and software companies. If active defense is found, re-evaluate extractiveness and suppression.',
    'If active defense by beneficiaries is the primary driver, the constraint would shift towards a Tangled Rope or Snare, with higher extractiveness and suppression, as described by the ''incumbent_preservation_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ambiguity, conceptual, 'Distinguishing between coordination-driven persistence and active rent-seeking.').

omega_variable(
    identity_lock_strength,
    'To what extent is the ''identity_locked'' exit option for computer_users a result of genuine skill acquisition versus a lack of perceived alternatives?',
    'Studies on the ease of learning alternative layouts when presented with clear incentives and accessible training, independent of QWERTY''s ubiquity. If learning is easier than assumed, the ''identity_locked'' status might be overstated.',
    'If the identity lock is weaker, users'' exit options are less constrained, potentially lowering the effective extractiveness of the constraint by reducing the ''cost'' of switching.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Assessing the true cost and psychological barrier of switching keyboard layouts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 1878, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1878, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1878, 0.0).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1920, 0.0).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1878, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1878, 0.05).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1920, 0.1).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1878, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1878, 0.0).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1920, 0.0).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1960, 0.0).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2000, 0.0).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2024, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the 'qwerty_persistence' kernel. This 'lapsed_alternatives_reading' emphasizes coordination value and switching costs, while the 'incumbent_preservation_reading' focuses on active defense by beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
