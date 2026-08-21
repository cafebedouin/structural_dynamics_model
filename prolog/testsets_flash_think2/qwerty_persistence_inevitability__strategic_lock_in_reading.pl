% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Keyboard Layout: Strategic Lock-in
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story analyzes the persistence of the QWERTY keyboard
 *   layout as a case of manufacturer-engineered lock-in, rather than purely
 *   accidental path dependency. The QWERTY layout, initially designed to
 *   prevent mechanical jamming in early typewriters, became entrenched
 *   through strategic standardization efforts by manufacturers and training
 *   institutions. This reading highlights the active role of beneficiaries in
 *   maintaining a suboptimal standard to extract rents and impose costs on
 *   users and potential competitors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.85).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.9).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Keyboard Layout: Strategic Lock-in").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, 'ecc50962-f3ff-4753-9c6a-45efd04e4b6a').
narrative_ontology:cs_kernel_codification('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', formalized).
narrative_ontology:cs_authority_grounding('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', extraction).
narrative_ontology:cs_interpretation_layer_present('ecc50962-f3ff-4753-9c6a-45efd04e4b6a').
narrative_ontology:cs_reading_relation('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', qwerty_persistence_inevitability__path_dependency_reading, forecloses).
narrative_ontology:cs_axiom('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', foundational, qwerty_design_was_suboptimal_but_profitable).
narrative_ontology:cs_axiom_status(qwerty_design_was_suboptimal_but_profitable, holdable).
narrative_ontology:cs_axiom_grounding('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', qwerty_design_was_suboptimal_but_profitable, empirically_contingent).
narrative_ontology:cs_axiom('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', foundational, standardization_was_engineered_for_rent_extraction).
narrative_ontology:cs_axiom_status(standardization_was_engineered_for_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', standardization_was_engineered_for_rent_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', engineered_standardization_for_profit).
narrative_ontology:cs_drift_state('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', contemporary_digital_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('ecc50962-f3ff-4753-9c6a-45efd04e4b6a', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, original_qwerty_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_training_schools).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original manufacturers who, through cartel agreements and strategic partnerships with training institutions, engineered the QWERTY standard to prevent jamming on early typewriters and later to maintain market dominance and extract rents by locking in users.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, original_qwerty_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Institutions that benefited from a standardized curriculum and the high demand for QWERTY-trained typists. They actively propagated the standard and resisted the adoption of alternative layouts, reinforcing the lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_training_schools, beneficiary,
    organized, biographical, constrained, national).

% Individuals who learn and use the QWERTY layout. They bear the ergonomic costs associated with its inefficient design and face significant retraining barriers and social costs if they attempt to switch to more efficient alternative layouts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typists, payer,
    powerless, biographical, identity_locked, global).

% Companies that have developed and attempted to market more ergonomically efficient or faster keyboard layouts (e.g., Dvorak). They are largely excluded from the market due to the entrenched QWERTY standard and the high switching costs for typists.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_manufacturers, excluded,
    powerful, biographical, trapped, global).

% Academics and professionals who study human-computer interaction and workplace ergonomics. They have consistently documented the inefficiencies and health costs associated with the QWERTY layout but have limited power to change the entrenched standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, ergonomics_researchers, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, original_qwerty_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, standardized keyboard layout that allows for widespread typing skill transfer, mass production of keyboards, and simplified training across different users and devices.
% TRANSFER_FUNCTION: Transfers ergonomic costs and retraining burdens from manufacturers to typists, and economic rents from typists and potential alternative keyboard manufacturers to the original QWERTY manufacturers and their successors.
% ABSENT_VOICES: Typists suffering from ergonomic issues and manufacturers of more efficient layouts were not part of the original standardization decisions or subsequent reinforcement mechanisms. Their concerns are largely unaddressed by the dominant standard.
% DISAPPEARANCE_RATIONALE: If the QWERTY standard and its enforcement mechanisms vanished overnight, the entire typing ecosystem would need to re-standardize. This would lead to initial chaos but would likely result in the widespread adoption of more efficient and ergonomic layouts, reorganizing the market for keyboards and typing education.
% FOUNDING_PROBLEM: Early mechanical typewriters suffered from key jamming when frequently used keys were placed too close together. The QWERTY layout was designed to slow typists down and separate common key pairs to mitigate this mechanical problem.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts and engineering analyses confirm the original jamming problem. Ergonomics researchers and alternative keyboard advocates corroborate that the mechanical problem is long dead in modern digital keyboards, but the standard persists due to institutional inertia and strategic lock-in, not functional necessity.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the ongoing ergonomic costs borne by typists and the suppressed innovation in keyboard design, which benefits the entrenched manufacturers. Suppression (0.90) is severe due to the pervasive nature of the standard, reinforced by training, muscle memory, and the lack of viable alternatives in the mainstream market. The low theater ratio (0.15) indicates that the constraint is functionally effective at maintaining the lock-in and rent extraction, rather than being merely performative. The metrics show a clear trend of increasing extractiveness and suppression as the lock-in mechanism solidified over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the original manufacturers, the QWERTY layout was a necessary coordination mechanism that solved a critical technical problem and provided a stable market. From the perspective of typists and alternative manufacturers, it is an extractive standard that imposes unnecessary costs and stifles innovation. The engine's classification will highlight this divergence by computing a 'tangled_rope' type from the structural data, contrasting with any 'rope' or 'mountain' claims of naturalness or pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The original QWERTY manufacturers and associated training schools are clear beneficiaries, actively shaping and enforcing the standard to their advantage. Typists are the primary targets, bearing the ergonomic and retraining costs. Alternative keyboard manufacturers are excluded, unable to compete with the entrenched standard. Ergonomics researchers act as analytical observers, documenting the costs without direct influence on the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_mechanism_ambiguity,
    'Is QWERTY''s persistence primarily due to accidental path dependency (unforeseen consequences of early choices) or strategic lock-in (deliberate engineering by beneficiaries)?',
    'Further historical research into manufacturer archives, cartel agreements, and lobbying efforts to determine the extent of intentional design in QWERTY''s entrenchment.',
    'If resolved towards accidental path dependency, the constraint would be reclassified closer to a ''piton'' or ''rope'' with lower extraction and suppression. If resolved towards strategic lock-in, the ''tangled_rope'' classification is reinforced, potentially shifting towards ''snare'' if the coordination function is found to be entirely a cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_mechanism_ambiguity, empirical, 'Distinguishing between accidental and strategic causes of QWERTY''s persistence.').

omega_variable(
    ergonomic_cost_quantification,
    'What is the precise economic and health cost borne by typists due to the QWERTY layout''s inefficiencies and ergonomic shortcomings?',
    'Large-scale epidemiological studies on typist health, economic modeling of productivity losses, and comparative studies with alternative keyboard layouts.',
    'A higher quantified cost would increase the measured extractiveness and strengthen the ''tangled_rope'' or ''snare'' classification, highlighting the significant burden on victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergonomic_cost_quantification, empirical, 'Quantifying the ergonomic and health costs imposed by the QWERTY layout.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qwer_tr_t10, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(qwer_be_t10, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(qwer_su_t10, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(qwer_su_t30, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
