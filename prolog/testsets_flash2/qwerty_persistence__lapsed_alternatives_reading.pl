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
    narrative_ontology:epsilon_provenance/5,
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
 *   The QWERTY keyboard layout persists as the dominant standard, not due to
 *   its inherent efficiency, but because the coordination value of a
 *   universal layout outweighs the individual benefits of switching to a more
 *   optimal, but non-standard, alternative. This reading emphasizes the
 *   collective action problem: no single actor benefits enough to
 *   unilaterally switch, and alternatives fail to reach critical mass,
 *   leading to their effective 'lapsing' from the market. This is one reading
 *   of the 'qwerty_persistence' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.15).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.2).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Keyboard Layout Persistence (Lapsed Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, 'c46e39c6-ee65-411f-b030-760d05a79aee').
narrative_ontology:cs_kernel_codification('c46e39c6-ee65-411f-b030-760d05a79aee', implicit).
narrative_ontology:cs_authority_grounding('c46e39c6-ee65-411f-b030-760d05a79aee', practice).
narrative_ontology:cs_reading_relation('c46e39c6-ee65-411f-b030-760d05a79aee', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('c46e39c6-ee65-411f-b030-760d05a79aee', foundational, coordination_value_outweighs_individual_efficiency).
narrative_ontology:cs_axiom_status(coordination_value_outweighs_individual_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('c46e39c6-ee65-411f-b030-760d05a79aee', coordination_value_outweighs_individual_efficiency, conventional).
narrative_ontology:cs_axiom('c46e39c6-ee65-411f-b030-760d05a79aee', foundational, alternatives_fail_to_reach_critical_mass).
narrative_ontology:cs_axiom_status(alternatives_fail_to_reach_critical_mass, holdable).
narrative_ontology:cs_axiom_grounding('c46e39c6-ee65-411f-b030-760d05a79aee', alternatives_fail_to_reach_critical_mass, empirically_contingent).
narrative_ontology:cs_reference_frame('c46e39c6-ee65-411f-b030-760d05a79aee', universal_keyboard_standard_by_coordination).
narrative_ontology:cs_drift_state('c46e39c6-ee65-411f-b030-760d05a79aee', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c46e39c6-ee65-411f-b030-760d05a79aee', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, software_developers).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must continue to produce QWERTY keyboards due to user demand, incurring retooling costs if they were to switch to an alternative layout. They bear the cost of maintaining the standard but also benefit from a stable market.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, payer,
    organized, biographical, constrained, global).

% Are accustomed to QWERTY and face significant retraining costs and productivity loss if they were to switch to a more 'efficient' layout. Their skill is an investment in the existing standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typists, payer,
    moderate, biographical, identity_locked, global).

% Have designed more efficient keyboard layouts (e.g., Dvorak, Colemak) but cannot achieve critical mass for adoption due to the entrenched QWERTY standard. They are excluded from market relevance by the coordination failure.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_designers, excluded,
    powerless, generational, trapped, global).

% Benefit from a single, universal keyboard standard, simplifying software development and localization. They would face increased complexity if multiple layouts were equally prevalent.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, software_developers, beneficiary,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a universal standard for keyboard input, allowing users to easily switch between devices and software to be developed for a consistent input method.
% TRANSFER_FUNCTION: Distributes the costs of maintaining the QWERTY standard (e.g., retraining, suboptimal efficiency) symmetrically across manufacturers and users, while providing the benefit of universal compatibility.
% ABSENT_VOICES: Designers and advocates of alternative, potentially more efficient, keyboard layouts are absent from the decision-making process, as their innovations fail to gain traction against the entrenched standard.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, there would be a chaotic period of re-standardization, with significant costs for retraining, retooling, and software adaptation, eventually leading to a new dominant layout or a fragmented market.
% FOUNDING_PROBLEM: The need for a standardized, reliable input method for typewriters to prevent jamming and facilitate widespread adoption.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and ergonomic researchers corroborate that the original jamming problem is long solved by modern technology, and the current persistence is due to path dependence and switching costs, not functional necessity. Keyboard manufacturers and typists, while bearing costs, also benefit from the stability of the existing standard.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) as the costs are primarily switching costs and suboptimal efficiency, distributed broadly. Suppression is also low (0.2) as there's no active enforcement, but rather a passive suppression of alternatives due to network effects and coordination failure. Theater ratio is negligible (0.05) as the constraint is genuinely functional in its coordination role, even if suboptimal. Accessibility collapse is high (0.7) because viable alternatives exist but are practically inaccessible due to lack of adoption.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual typists or manufacturers, the constraint is a 'given' that imposes costs but also provides the benefit of interoperability. From the perspective of alternative layout designers, it's a barrier to innovation. The engine's classification will reflect this symmetric cost/benefit distribution for most participants, leading to a Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers and typists are 'payers' in that they bear the costs of the suboptimal layout and switching costs, but they also benefit from the coordination. Software developers are beneficiaries, as they gain from a universal standard. Alternative layout designers are 'excluded' as they cannot overcome the network effects. The lack of a concentrated beneficiary or victim group is key to this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the QWERTY layout as a Snare or Tangled Rope. While it imposes costs, these are primarily coordination costs and switching barriers, not actively extracted rents by a specific beneficiary. The 'dead' founding problem status, combined with a 'world_rearranges' disappearance verdict, indicates a constraint that has outlived its original justification but persists due to path dependence, characteristic of a Rope or Piton. The low extractiveness and theater ratio lean towards Rope in this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_defense_vs_passive_persistence,
    'To what extent is QWERTY''s persistence due to passive coordination failure and switching costs (this reading), versus active defense by incumbents protecting sunk capital investments (incumbent_preservation_reading)?',
    'Detailed historical analysis of lobbying efforts, patent defense, and marketing strategies by major keyboard manufacturers, alongside economic modeling of switching costs versus active rent-seeking.',
    'If active defense is significant, the constraint would shift towards a Tangled Rope or Snare, with identifiable beneficiaries actively extracting value. If passive persistence dominates, the Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_defense_vs_passive_persistence, empirical, 'Distinguishes between coordination failure and active rent-seeking as drivers of persistence.').

omega_variable(
    efficiency_gain_threshold,
    'What is the actual efficiency gain offered by alternative layouts, and at what threshold would this gain overcome switching costs to trigger a market transition?',
    'Large-scale ergonomic studies and economic modeling of user adoption curves for alternative layouts, potentially with government or industry subsidies to reduce initial switching costs.',
    'A sufficiently high, demonstrable efficiency gain could challenge the ''lapsed alternatives'' premise, suggesting that the constraint is more fragile than currently perceived, or that the ''suppression'' of alternatives is more active than passive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_gain_threshold, empirical, 'Quantifies the potential for alternatives to disrupt the standard.').


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
narrative_ontology:measurement(qwer_tr_t1970, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1878, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1878, 0.05).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1920, 0.1).
narrative_ontology:measurement(qwer_be_t1970, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1878, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1878, 0.05).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1920, 0.1).
narrative_ontology:measurement(qwer_su_t1970, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'lapsed_alternatives_reading' of the 'qwerty_persistence' kernel, focusing on coordination value and network effects. It is linked to the 'incumbent_preservation_reading', which emphasizes active defense by beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
