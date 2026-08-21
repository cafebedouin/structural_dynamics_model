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
 *   human_readable: QWERTY Persistence: Strategic Lock-in Reading
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story presents the 'strategic lock-in' reading of QWERTY
 *   keyboard persistence. It argues that QWERTY's dominance is not merely an
 *   accidental outcome of path dependency, but a result of deliberate
 *   strategic actions by early typewriter manufacturers to create and
 *   maintain market lock-in through standardization, training partnerships,
 *   and suppression of alternatives. This reading highlights the extractive
 *   nature of this manufactured inevitability, where typists bear ergonomic
 *   costs and retraining barriers, while manufacturers and training
 *   institutions benefit from the entrenched standard.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.78).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.85).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence: Strategic Lock-in Reading").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '217766b1-c6ba-4b23-8db9-c849543d51f5').
narrative_ontology:cs_kernel_codification('217766b1-c6ba-4b23-8db9-c849543d51f5', formalized).
narrative_ontology:cs_authority_grounding('217766b1-c6ba-4b23-8db9-c849543d51f5', extraction).
narrative_ontology:cs_interpretation_layer_present('217766b1-c6ba-4b23-8db9-c849543d51f5').
narrative_ontology:cs_reading_relation('217766b1-c6ba-4b23-8db9-c849543d51f5', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('217766b1-c6ba-4b23-8db9-c849543d51f5', foundational, standardization_as_strategic_tool).
narrative_ontology:cs_axiom_status(standardization_as_strategic_tool, holdable).
narrative_ontology:cs_axiom_grounding('217766b1-c6ba-4b23-8db9-c849543d51f5', standardization_as_strategic_tool, empirically_contingent).
narrative_ontology:cs_axiom('217766b1-c6ba-4b23-8db9-c849543d51f5', foundational, active_suppression_of_alternatives).
narrative_ontology:cs_axiom_status(active_suppression_of_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('217766b1-c6ba-4b23-8db9-c849543d51f5', active_suppression_of_alternatives, empirically_contingent).
narrative_ontology:cs_reference_frame('217766b1-c6ba-4b23-8db9-c849543d51f5', manufacturer_engineered_dominance).
narrative_ontology:cs_drift_state('217766b1-c6ba-4b23-8db9-c849543d51f5', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('217766b1-c6ba-4b23-8db9-c849543d51f5', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_operators).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_designers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, contemporary_keyboard_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original cartel of typewriter manufacturers (Remington, Densmore, Caligraph, Yost) that standardized QWERTY and established training partnerships to ensure its dominance. They actively suppressed alternatives and benefited from the lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel, agenda_setter,
    institutional, generational, arbitrage, national).

% Profited from the standardized QWERTY curriculum, as their training became essential for employment. They had a vested interest in maintaining QWERTY's dominance and resisted alternative layouts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_operators, beneficiary,
    organized, biographical, constrained, local).

% Bear the ergonomic costs and inefficiencies of the QWERTY layout. They are locked in by the ubiquity of QWERTY keyboards and the high retraining costs for alternatives, making exit difficult due to professional identity and market expectations.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typists, payer,
    powerless, biographical, identity_locked, global).

% Developed more efficient or ergonomic keyboard layouts (e.g., Dvorak) but faced insurmountable barriers to market entry due to QWERTY's entrenched standardization and the active suppression by manufacturers and training institutions.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_designers, excluded,
    moderate, generational, trapped, global).

% Continue to benefit from the established QWERTY standard, as it simplifies manufacturing and ensures a ready market. While not part of the original cartel, they inherit the benefits of the lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, contemporary_keyboard_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a universal standard for typewriter keyboards, allowing typists to move between machines and facilitating mass production and training.
% TRANSFER_FUNCTION: Transferred ergonomic costs and retraining burdens to typists, while transferring market dominance and sustained revenue to QWERTY manufacturers and associated training institutions.
% ABSENT_VOICES: Alternative keyboard designers and ergonomists, whose superior designs were actively suppressed, would advocate for a more efficient and health-conscious standard. Typists, as a collective, would demand layouts optimized for human physiology rather than mechanical constraints.
% DISAPPEARANCE_RATIONALE: If QWERTY's dominance vanished overnight, the market would rapidly shift towards more ergonomic and efficient layouts, driven by user demand and technological advancements. Keyboard manufacturing, typing education, and even office ergonomics would undergo a significant reorganization.
% FOUNDING_PROBLEM: The original problem was to prevent typewriter keys from jamming by separating commonly used letter pairs, and to establish a standard for mass production and training.
% FOUNDING_PROBLEM_CORROBORATION: Historical analysis and ergonomic studies from independent researchers and academic institutions corroborate that the original mechanical problem is long solved, and the layout's persistence is now driven by institutional inertia and lock-in, not functional necessity. The manufacturers' claims of continued functional necessity are not widely corroborated outside their immediate sphere of influence.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the QWERTY layout imposes significant ergonomic and efficiency costs on typists, which are not offset by superior functionality. Suppression is very high due to the ubiquity of QWERTY keyboards, the high cost of retraining, and the historical active suppression of alternative layouts by the original cartel. The theater ratio is moderate, reflecting that while QWERTY still 'functions' as a keyboard, its continued justification often relies on appeals to 'inevitability' rather than genuine functional superiority, masking the underlying lock-in mechanisms. The metrics show a clear trend of increasing extractiveness and suppression as the lock-in deepened over time.
 *
 * PERSPECTIVAL GAP:
 *   The 'strategic lock-in' reading fundamentally diverges from the 'path dependency' reading. While the latter sees QWERTY as an accidental outcome, this reading identifies clear beneficiaries and active enforcement mechanisms, leading to a classification as a Tangled Rope rather than a Piton or Rope. The engine's per-seat classification would reflect this: manufacturers as beneficiaries of an extractive system, typists as victims of a manufactured constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The original typewriter manufacturers and typing school operators are clear beneficiaries, having actively engineered and profited from the QWERTY standard. Typists are the primary victims, bearing the costs of an inefficient and potentially harmful layout, with limited exit options due to identity-locked professional norms and market ubiquity. Alternative keyboard designers are excluded, their innovations suppressed by the entrenched standard. Contemporary manufacturers continue to benefit from the inherited standard.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_role_of_cartel_actions,
    'To what extent were the 1893 cartel''s actions (training partnerships, suppression of alternatives) causally determinative of QWERTY''s long-term dominance, versus merely reinforcing an already established path?',
    'Counterfactual historical analysis: detailed examination of market dynamics and adoption rates in scenarios where cartel actions were absent or different, using historical economic modeling.',
    'If cartel actions were highly determinative, it strengthens the ''strategic lock-in'' reading and its Tangled Rope classification. If they were merely reinforcing, it lends more weight to the ''path dependency'' reading, potentially shifting classification towards a Piton or Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_role_of_cartel_actions, empirical, 'Assessing the causal weight of strategic actions in QWERTY''s entrenchment.').

omega_variable(
    identity_lock_in_vs_structural_barriers,
    'For typists, what proportion of their ''identity_locked'' exit option is due to internalized professional identity (e.g., ''a typist uses QWERTY'') versus external structural barriers (e.g., lack of alternative keyboards, employer mandates)?',
    'Sociological studies of typist communities and surveys on perceived barriers to switching, combined with market analysis of alternative keyboard availability and cost.',
    'If internalized identity lock-in is dominant, the suppression is more deeply embedded and harder to dislodge. If structural barriers are primary, policy interventions (e.g., mandating alternative keyboard availability) could be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_vs_structural_barriers, empirical, 'Distinguishing internalized vs. structural components of typist lock-in.').

omega_variable(
    reading_framing_ambiguity,
    'Is the QWERTY persistence phenomenon best framed as ''strategic lock-in'' (with active beneficiaries and enforcement) or ''path dependency'' (an accidental, self-reinforcing standard)?',
    'Further historical and economic research focusing on the intent and impact of early manufacturer actions, and the counterfactuals of alternative keyboard adoption. The choice of framing depends on the weight given to agency versus emergent system properties.',
    'Adopting the ''path dependency'' reading would likely shift the classification towards a Piton (atrophied function, inertial persistence) or even a Rope (if the coordination benefits are emphasized without active extraction), as it downplays active extraction and enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Conceptual ambiguity between strategic lock-in and path dependency as explanations for QWERTY''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1873, 0.1).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1873, 0.4).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1873, 0.5).
narrative_ontology:measurement(qwer_su_t1900, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'QWERTY persistence inevitability' kernel. It focuses on strategic lock-in, while 'qwerty_persistence_inevitability__path_dependency_reading' focuses on accidental path dependency. Both are distinct constraints arising from the same underlying phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
