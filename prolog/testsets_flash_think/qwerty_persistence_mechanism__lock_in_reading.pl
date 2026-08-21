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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Keyboard Layout Lock-in (Path Dependence Reading)
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   This constraint is the 'lock_in_reading' of the
 *   'qwerty_persistence_mechanism' kernel, emphasizing path-dependent
 *   coordination failure and collective suboptimality without active
 *   individual extraction. It describes how the QWERTY keyboard layout
 *   persists despite its technical inferiority for modern digital typing,
 *   primarily due to network effects, high switching costs, and institutional
 *   inertia. Sibling readings include 'naturalization_reading' (QWERTY is
 *   adequate) and 'beneficiary_extraction_reading' (manufacturers actively
 *   maintain QWERTY for profit).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.35).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.8).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, piton).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout Lock-in (Path Dependence Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic/technological/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '043160af-e28f-4f03-bdf5-9b7f6f1c6da1').
narrative_ontology:cs_kernel_codification('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', implicit).
narrative_ontology:cs_authority_grounding('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', practice).
narrative_ontology:cs_reading_relation('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_axiom('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', foundational, technological_suboptimality_persists).
narrative_ontology:cs_axiom_status(technological_suboptimality_persists, holdable).
narrative_ontology:cs_axiom_grounding('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', technological_suboptimality_persists, empirically_contingent).
narrative_ontology:cs_axiom('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', foundational, network_effects_create_lock_in).
narrative_ontology:cs_axiom_status(network_effects_create_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', network_effects_create_lock_in, empirically_contingent).
narrative_ontology:cs_reference_frame('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', initial_typewriter_standard).
narrative_ontology:cs_drift_state('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', contemporary_digital_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('043160af-e28f-4f03-bdf5-9b7f6f1c6da1', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, typing_tutors_and_educators).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, current_qwerty_users).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, potential_dvorak_users).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, new_typists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, current_qwerty_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Continue to produce QWERTY keyboards due to established tooling, supply chains, and market demand. They benefit by avoiding the prohibitive retooling costs and market disruption of switching to an alternative layout, effectively administering the standard through inertia.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers, agenda_setter,
    powerful, generational, constrained, global).

% Teach QWERTY as the default standard, perpetuating its use across generations of typists. They benefit from an established curriculum and widespread familiarity, avoiding the costs and friction of introducing alternative layouts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typing_tutors_and_educators, agenda_setter,
    organized, biographical, constrained, national).

% Are locked into the QWERTY layout due to learned muscle memory and the ubiquity of QWERTY devices. They bear the diffuse cost of suboptimal typing efficiency but benefit from a universally compatible standard. Switching to a more efficient layout would require significant retraining and a period of reduced productivity.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, current_qwerty_users, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, current_qwerty_users, beneficiary).

% Would benefit from a more efficient keyboard layout (e.g., Dvorak) but are effectively excluded from widespread adoption due to the overwhelming network effects and lock-in of QWERTY. They bear the opportunity cost of foregone efficiency.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, potential_dvorak_users, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, potential_dvorak_users, excluded).

% Are forced to learn the QWERTY layout as the default, inheriting its inefficiencies and the costs of its suboptimality without having made an active choice. Their exit options are minimal as they enter a QWERTY-dominated world.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, new_typists, payer,
    powerless, immediate, trapped, global).

% Analyze the historical development and persistence of QWERTY, documenting its path-dependent nature and the mechanisms of lock-in. They provide an analytical perspective on the collective suboptimality.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, interoperable keyboard standard for communication and data entry, ensuring compatibility across devices and users.
% TRANSFER_FUNCTION: Transfers the diffuse cost of suboptimal typing efficiency and the burden of learning a less efficient layout from manufacturers and educators (who avoid retooling/curriculum change) to individual users and the economy as a whole (lost productivity).
% ABSENT_VOICES: Advocates for technically superior layouts (e.g., Dvorak, Colemak) are marginalized by the installed base and network effects; they would argue for a collective switch to a more efficient standard but lack the power to initiate such a change.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the global typing infrastructure would collapse, requiring a massive, costly, and disruptive re-coordination around a new standard. All digital communication and data entry would be severely hampered until a new standard emerged and was adopted.
% FOUNDING_PROBLEM: The original problem was to prevent typewriter keys from jamming by separating frequently used letter pairs, and to establish a standardized layout for efficient typing.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and human-computer interaction researchers, through empirical studies and historical analysis, corroborate that the original problem of key jamming is obsolete in modern digital contexts. They attest that QWERTY's persistence is now primarily due to path dependence and lock-in, not its inherent optimality for contemporary use, supporting the 'dead' status of the founding problem.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Piton because its original function (preventing typewriter key jamming) is obsolete, and its current persistence is driven by inertia (established training, manufacturing, and user habits) rather than active, concentrated benefit. The 'extraction' is diffuse, representing the collective cost of suboptimal efficiency. Extractiveness is low (0.35) because no single party captures significant rents from QWERTY's suboptimality; rather, it's a collective burden. Suppression is high (0.80) due to the prohibitive costs of individual or collective switching (retraining, retooling, coordination). Theater ratio is moderate (0.45) as the continued production and teaching of QWERTY, while functional, also serves to maintain a suboptimal status quo that is no longer justified by its original design principles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of manufacturers and educators, QWERTY represents a stable, low-friction status quo. From the perspective of users, it's an unavoidable, slightly inefficient standard that requires significant personal investment to overcome. Economic historians view it as a classic case of market failure and path dependence.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers and typing educators act as agenda-setters and beneficiaries by avoiding the costs of retooling and curriculum changes, respectively. Current and new typists are payers, bearing the diffuse costs of suboptimal efficiency and the burden of learning an inefficient layout. Potential Dvorak users are also payers, bearing the opportunity cost of a superior, but inaccessible, alternative. The lock-in mechanism itself, rather than active enforcement, drives these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate for QWERTY (preventing key jamming) is entirely dead in the digital age. The constraint persists due to lock-in and inertia, not because it solves a live problem optimally. This aligns with the Piton classification, indicating a constraint whose function has atrophied but remains due to institutional momentum, with diffuse costs and no clear, concentrated beneficiary actively maintaining it for profit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lock_in_vs_active_extraction,
    'Is QWERTY''s persistence primarily due to passive lock-in and network effects, or is it actively maintained by manufacturers and educators to protect their investments and market positions (beneficiary extraction)?',
    'Detailed economic analysis of manufacturer profit margins and retooling costs, and sociological studies of educational institutions'' resistance to curriculum change, compared against the collective switching costs for users.',
    'If active extraction is the dominant mechanism, the constraint would reclassify towards a Snare or Tangled Rope, with higher extractiveness. If passive lock-in is dominant, the Piton classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_vs_active_extraction, empirical, 'Distinguishing between market failure (lock-in) and active rent-seeking as the primary driver of QWERTY''s persistence.').

omega_variable(
    qwerty_adequacy_vs_suboptimality,
    'Is QWERTY genuinely suboptimal for modern digital typing, or has its adequacy been naturalized over time, making alternatives appear unnecessary (naturalization reading)?',
    'Further human-computer interaction studies comparing typing speed, error rates, and ergonomic stress across QWERTY and alternative layouts, controlling for training effects and user bias.',
    'If QWERTY is found to be genuinely adequate or superior for a significant portion of users, the constraint would shift towards a Rope or even Mountain (if truly optimal), with lower extractiveness. If suboptimality is confirmed, the Piton classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qwerty_adequacy_vs_suboptimality, empirical, 'Assessing the true performance of QWERTY relative to alternatives in modern contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1970, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1980, 0.41).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1990, 0.42).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2000, 0.43).
narrative_ontology:measurement(qwer_tr_t2010, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(qwer_tr_t2020, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1970, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1980, 0.31).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(qwer_be_t2010, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(qwer_be_t2020, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2020, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1970, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1970, 0.8).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(qwer_su_t1990, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(qwer_su_t2010, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(qwer_su_t2020, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
