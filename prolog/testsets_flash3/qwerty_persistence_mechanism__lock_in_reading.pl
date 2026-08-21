% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Lock-in Reading)
 *   domain: economic_history/technology_studies
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   through a 'lock-in' mechanism, where the collective benefit of a shared
 *   standard outweighs the individual incentive to switch to a technically
 *   superior alternative, even though no single actor actively profits from
 *   its suboptimality. This is one reading of the 'QWERTY persistence'
 *   kernel, focusing on the coordination failure and path dependence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.45).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.6).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout Persistence (Lock-in Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, 'f4cf9c58-d105-428f-b65a-c226392f62d7').
narrative_ontology:cs_kernel_codification('f4cf9c58-d105-428f-b65a-c226392f62d7', implicit).
narrative_ontology:cs_authority_grounding('f4cf9c58-d105-428f-b65a-c226392f62d7', practice).
narrative_ontology:cs_interpretation_layer_present('f4cf9c58-d105-428f-b65a-c226392f62d7').
narrative_ontology:cs_reading_relation('f4cf9c58-d105-428f-b65a-c226392f62d7', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4cf9c58-d105-428f-b65a-c226392f62d7', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_axiom('f4cf9c58-d105-428f-b65a-c226392f62d7', foundational, social_cost_benefit_divergence).
narrative_ontology:cs_axiom_status(social_cost_benefit_divergence, holdable).
narrative_ontology:cs_axiom_grounding('f4cf9c58-d105-428f-b65a-c226392f62d7', social_cost_benefit_divergence, empirically_contingent).
narrative_ontology:cs_axiom('f4cf9c58-d105-428f-b65a-c226392f62d7', foundational, no_single_actor_benefits_from_suboptimality).
narrative_ontology:cs_axiom_status(no_single_actor_benefits_from_suboptimality, holdable).
narrative_ontology:cs_axiom_grounding('f4cf9c58-d105-428f-b65a-c226392f62d7', no_single_actor_benefits_from_suboptimality, empirically_contingent).
narrative_ontology:cs_reference_frame('f4cf9c58-d105-428f-b65a-c226392f62d7', efficient_market_adoption).
narrative_ontology:cs_drift_state('f4cf9c58-d105-428f-b65a-c226392f62d7', post_david_critique_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f4cf9c58-d105-428f-b65a-c226392f62d7', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, existing_qwerty_users).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, new_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ubiquity of QWERTY, which reduces coordination costs when using shared equipment or teaching new users. However, they are 'locked in' by their own learned skill and the cost of retraining, making exit to superior layouts personally prohibitive.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, existing_qwerty_users, beneficiary,
    organized, biographical, identity_locked, global).

% Bear the cost of learning a suboptimal layout, often unaware of alternatives. Their choice is constrained by the overwhelming prevalence of QWERTY keyboards and training materials.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, new_typists, payer,
    powerless, immediate, constrained, global).

% Benefit from the stable demand for QWERTY layouts, avoiding the costs of retooling or marketing alternative designs. While they could produce alternatives, the market's inertia makes it unprofitable.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers, beneficiary,
    powerful, generational, mobile, global).

% Invest in developing technically superior keyboard layouts (e.g., Dvorak, Colemak) but face immense barriers to adoption due to the installed base and network effects of QWERTY. They bear the cost of market resistance.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_developers, payer,
    moderate, generational, constrained, global).

% Perpetuate QWERTY by teaching it as the standard, reinforcing its dominance. While some may acknowledge alternatives, the practical demands of teaching a widely used layout outweigh the theoretical benefits of a superior one.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typing_tutors_and_educators, agenda_setter,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a universal standard for keyboard layouts, allowing users to easily switch between different computers and share typing skills without re-learning.
% TRANSFER_FUNCTION: Transfers the cost of learning and using a suboptimal layout from the collective (existing users, manufacturers) to new typists and developers of alternative layouts, in exchange for coordination benefits.
% ABSENT_VOICES: Future generations of typists who might benefit from a more efficient layout, and those who would advocate for a more dynamic, merit-based standard for technology adoption, are not part of the decision-making process.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, there would be immense short-term chaos as billions of users would be unable to type. However, over time, a more efficient, potentially diverse set of layouts would emerge, leading to a long-term increase in typing efficiency and user satisfaction, fundamentally reorganizing the human-computer interface.
% FOUNDING_PROBLEM: To create a standardized, robust mechanical keyboard layout that prevented typebar jamming in early typewriters and facilitated rapid, two-handed typing.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts and engineering analyses confirm QWERTY's original purpose. However, ergonomic studies and computer science research from independent academic institutions widely corroborate that the jamming problem is obsolete for modern digital keyboards, and QWERTY's layout is no longer optimal for typing speed or comfort, rendering its founding problem 'dead'.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because the 'cost' is diffuse and borne by the collective in terms of lost efficiency, rather than concentrated extraction by a single agent. Suppression (0.6) is high due to the strong network effects and the high cost of individual exit (retraining, lack of compatible equipment). Theater ratio (0.1) is low because there's little active 'performance' to maintain QWERTY; its persistence is largely inertial. The claimed type is Tangled Rope because it provides a genuine coordination function (universal standard) but also imposes asymmetric costs (suboptimal efficiency, barriers to alternatives) that require active, albeit diffuse, enforcement (social norms, educational systems).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of existing QWERTY users and manufacturers, the layout is a functional standard that provides coordination benefits, making it appear more like a Rope. From the perspective of new typists and alternative developers, it's a barrier to efficiency and innovation, making it appear more extractive. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing QWERTY users and keyboard manufacturers are beneficiaries, as they avoid the costs of switching or retooling. New typists and alternative layout developers are payers, bearing the costs of learning a suboptimal system or facing market resistance. Typing tutors act as agenda-setters, reinforcing the standard. The 'lock-in' mechanism means that while no single actor extracts rents, the collective system is suboptimal, creating a social cost-benefit divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing typebar jamming) is dead, yet the constraint persists. This indicates a form of mandatrophy where the original justification has atrophied, but the constraint remains due to path dependence and coordination failure, not active rent-seeking by a single party. The classification as Tangled Rope, rather than Snare, prevents mislabeling a collective suboptimality as pure extraction, while still highlighting the asymmetric costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_mechanism_ambiguity,
    'Is QWERTY''s persistence primarily due to ''lock-in'' (coordination failure) or ''beneficiary extraction'' (active maintenance by incumbents)?',
    'Historical analysis of manufacturer lobbying efforts against alternative layouts, and economic studies quantifying the direct profits derived from QWERTY''s dominance versus the costs of switching for users.',
    'If beneficiary extraction is dominant, the constraint would be reclassified closer to a Snare, with higher extractiveness and identifiable beneficiaries actively profiting. If lock-in is dominant, the Tangled Rope classification holds, emphasizing collective suboptimality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_mechanism_ambiguity, empirical, 'Distinguishing between coordination failure and active rent-seeking as the primary driver of QWERTY''s persistence.').

omega_variable(
    technical_superiority_contestation,
    'Are alternative keyboard layouts (e.g., Dvorak) genuinely and significantly superior to QWERTY, or is their claimed superiority overstated?',
    'Large-scale, independent, double-blind studies comparing typing speed, error rates, and ergonomic comfort across various layouts, controlling for learning effects and user bias.',
    'If alternatives are not significantly superior, the ''cost'' of QWERTY''s persistence is lower, reducing its extractiveness and potentially shifting it closer to a Rope or even a Piton (if the coordination function is still primary). If superiority is confirmed, the extractiveness of QWERTY''s persistence is higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technical_superiority_contestation, empirical, 'Empirical validation of the technical inferiority claim underpinning the lock-in narrative.').

omega_variable(
    kernel_framing_choice,
    'Is the ''lock-in'' reading the most appropriate framing for QWERTY''s persistence, or should it be framed as ''naturalization'' or ''beneficiary extraction''?',
    'Analysis of the dominant narratives in economic history and technology studies, and the specific policy interventions (or lack thereof) that each framing would imply. This is a conceptual choice about which structural mechanism is most salient.',
    'Choosing the ''naturalization'' reading would imply lower extractiveness and suppression, potentially classifying it as a Mountain or Rope. Choosing the ''beneficiary extraction'' reading would imply higher extractiveness and suppression, classifying it closer to a Snare. This choice fundamentally alters the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'The choice of which structural mechanism (lock-in, naturalization, or extraction) best explains QWERTY''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(qwer_tr_t90, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 90, 0.1).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 120, 0.1).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 150, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(qwer_be_t90, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 90, 0.45).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 120, 0.45).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 150, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qwer_su_t30, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(qwer_su_t90, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 90, 0.6).
narrative_ontology:measurement(qwer_su_t120, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 120, 0.6).
narrative_ontology:measurement(qwer_su_t150, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 150, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
