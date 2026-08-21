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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Persistence (Lapsed Alternatives Reading)
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This constraint story analyzes the persistence of the QWERTY keyboard
 *   layout from the perspective that its dominance is primarily due to its
 *   coordination value and the natural lapsing of alternatives that fail to
 *   achieve critical mass. It is a reading of the 'qwerty_persistence'
 *   kernel, distinct from the 'incumbent_preservation_reading' which focuses
 *   on active defense by beneficiaries. This reading posits QWERTY as a Rope,
 *   where the costs are symmetric switching costs rather than asymmetric
 *   extraction, and alternatives are suppressed by network effects rather
 *   than active coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.1).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Persistence (Lapsed Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, 'ecd673de-485a-428f-b4c6-c353d0fecbf3').
narrative_ontology:cs_kernel_codification('ecd673de-485a-428f-b4c6-c353d0fecbf3', fixed_text).
narrative_ontology:cs_authority_grounding('ecd673de-485a-428f-b4c6-c353d0fecbf3', practice).
narrative_ontology:cs_reading_relation('ecd673de-485a-428f-b4c6-c353d0fecbf3', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('ecd673de-485a-428f-b4c6-c353d0fecbf3', foundational, coordination_value_drives_adoption).
narrative_ontology:cs_axiom_status(coordination_value_drives_adoption, holdable).
narrative_ontology:cs_axiom_grounding('ecd673de-485a-428f-b4c6-c353d0fecbf3', coordination_value_drives_adoption, empirically_contingent).
narrative_ontology:cs_axiom('ecd673de-485a-428f-b4c6-c353d0fecbf3', foundational, alternatives_lapse_without_critical_mass).
narrative_ontology:cs_axiom_status(alternatives_lapse_without_critical_mass, holdable).
narrative_ontology:cs_axiom_grounding('ecd673de-485a-428f-b4c6-c353d0fecbf3', alternatives_lapse_without_critical_mass, empirically_contingent).
narrative_ontology:cs_reference_frame('ecd673de-485a-428f-b4c6-c353d0fecbf3', network_effect_equilibrium).
narrative_ontology:cs_drift_state('ecd673de-485a-428f-b4c6-c353d0fecbf3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ecd673de-485a-428f-b4c6-c353d0fecbf3', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, all_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, typists).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the universal compatibility and reduced learning curve of a single dominant keyboard layout. They bear the symmetric, diffuse costs of switching to an alternative, but these are outweighed by coordination benefits.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, all_users, beneficiary,
    organized, biographical, constrained, global).

% Produce keyboards primarily in the QWERTY layout, benefiting from a stable, universally accepted standard that reduces design and marketing complexity. They perpetuate the standard through their production choices.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary).

% Are trained on and accustomed to the QWERTY layout. They benefit from its ubiquity but bear the personal switching costs (time, effort) if they were to adopt an alternative layout. Their muscle memory reinforces the standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typists, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__lapsed_alternatives_reading, typists, beneficiary).

% Promote alternative keyboard layouts (e.g., Dvorak, Colemak) that claim ergonomic or efficiency advantages. Their efforts are largely unsuccessful due to the overwhelming network effects and switching costs associated with QWERTY, leading their alternatives to lapse without reaching critical mass.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates, excluded,
    powerless, generational, trapped, global).

% Analyze the historical, sociological, and economic factors contributing to QWERTY's persistence, focusing on network effects, coordination benefits, and the natural failure of alternatives to gain adoption.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, historical_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, stable standard for keyboard layouts, enabling seamless interaction across different devices, users, and software without requiring constant re-learning or adaptation.
% TRANSFER_FUNCTION: Primarily transfers coordination benefits (reduced learning curves, universal compatibility, lower production costs for manufacturers) to all participants, with symmetric, diffuse switching costs borne by individual users and manufacturers.
% ABSENT_VOICES: Advocates for ergonomically or efficiently superior alternative layouts (e.g., Dvorak, Colemak) are effectively excluded from widespread adoption due to the overwhelming network effects of QWERTY. They would argue for a more rational or user-centric standard.
% DISAPPEARANCE_RATIONALE: If the QWERTY layout vanished overnight, the entire global ecosystem of typing, from hardware manufacturing to user training and software design, would be thrown into chaos. A new standard would eventually emerge, but the transition would be massively disruptive, demonstrating the deep dependence on this coordination mechanism.
% FOUNDING_PROBLEM: The original problem was to design a mechanical typewriter keyboard layout that prevented typebars from jamming by separating frequently used letter pairs, and to provide a consistent standard for typists.
% FOUNDING_PROBLEM_CORROBORATION: Historical engineering documents and accounts from the late 19th and early 20th centuries corroborate the original mechanical jamming problem. Modern ergonomic and computer science analyses, from outside the benefiting parties, confirm that the jamming problem is entirely obsolete for electronic keyboards, yet the layout persists.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.10) reflects that the 'cost' of QWERTY is primarily the symmetric switching cost for any individual or manufacturer to adopt an alternative, not a rent collected by a specific party. Suppression (0.05) is minimal and structural, arising from the sheer difficulty of coordinating a shift away from a deeply entrenched standard, rather than active enforcement. Theater ratio (0.02) is negligible, as the layout remains functionally effective for its primary purpose (typing). Accessibility collapse (0.80) is high for alternatives because, despite potential technical merits, they cannot overcome the network effects to achieve widespread adoption.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_users' and 'keyboard_manufacturers', QWERTY is a highly beneficial coordination mechanism. From the 'alternative_layout_advocates' perspective, it's a barrier to innovation, but one that persists due to collective action problems and path dependence, not active malice. The engine's classification as a Rope reflects this reading's emphasis on coordination and symmetric costs.
 *
 * DIRECTIONALITY LOGIC:
 *   All users are diffuse beneficiaries of the coordination, experiencing low directionality. Keyboard manufacturers, as agenda-setters, also benefit from market stability. Typists bear the immediate switching costs but are also beneficiaries of compatibility. Alternative layout advocates are targets of the constraint's persistence, as their innovations are effectively excluded by the network effect. The costs are largely symmetric coordination costs, not concentrated extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_of_persistence_ambiguity,
    'Is QWERTY''s persistence primarily due to its coordination value and the natural lapsing of alternatives (this reading), or due to active defense by incumbents protecting capital investments (incumbent_preservation_reading)?',
    'Detailed historical and economic analysis of lobbying efforts, patent defense, and market strategies by keyboard manufacturers versus the observed organic adoption patterns and switching costs over time.',
    'If active defense is dominant, the constraint would reclassify towards a Snare or Tangled Rope with higher extraction and identifiable beneficiaries. If coordination value is dominant, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_of_persistence_ambiguity, empirical, 'Distinguishing between coordination-driven persistence and incumbent-driven preservation.').

omega_variable(
    symmetric_vs_asymmetric_costs,
    'Are the costs associated with QWERTY truly symmetric (borne equally by all participants as switching costs), or are there hidden asymmetric costs or benefits that constitute extraction?',
    'Economic modeling of the total cost of ownership for QWERTY versus alternative layouts, including training, error rates, and health impacts, disaggregated by user groups and manufacturers.',
    'If significant asymmetric costs are found, the constraint''s extractiveness would increase, potentially shifting its classification towards a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetric_vs_asymmetric_costs, empirical, 'Assessing the symmetry of costs and benefits in QWERTY''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1920, 0.02).
narrative_ontology:measurement(qwer_tr_t1970, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1970, 0.03).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2024, 0.02).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1873, 0.1).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1920, 0.09).
narrative_ontology:measurement(qwer_be_t1970, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1970, 0.11).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1873, 0.05).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1920, 0.04).
narrative_ontology:measurement(qwer_su_t1970, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1970, 0.06).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the 'qwerty_persistence' kernel. This reading emphasizes coordination value and the natural failure of alternatives; the 'incumbent_preservation_reading' emphasizes active defense by beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
