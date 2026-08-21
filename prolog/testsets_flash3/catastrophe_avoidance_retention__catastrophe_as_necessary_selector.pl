% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Selector for Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint represents the reading that only actual catastrophes
 *   provide the necessary selection pressure (chaos, mortality salience,
 *   organizational trauma) to maintain competence in high-reliability
 *   systems. It implies that long peacetime periods inevitably lead to
 *   competence decay, that simulation creates false confidence, and that
 *   industries are vulnerable to black swan re-emergence. This is one reading
 *   of the 'catastrophe_avoidance_retention' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.65).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.7).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, snare).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Selector for Competence").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '58af1959-b0ba-40a4-85e7-7beaa02cebbb').
narrative_ontology:cs_kernel_codification('58af1959-b0ba-40a4-85e7-7beaa02cebbb', implicit).
narrative_ontology:cs_authority_grounding('58af1959-b0ba-40a4-85e7-7beaa02cebbb', practice).
narrative_ontology:cs_reading_relation('58af1959-b0ba-40a4-85e7-7beaa02cebbb', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('58af1959-b0ba-40a4-85e7-7beaa02cebbb', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('58af1959-b0ba-40a4-85e7-7beaa02cebbb', foundational, only_real_chaos_selects_competence).
narrative_ontology:cs_axiom_status(only_real_chaos_selects_competence, holdable).
narrative_ontology:cs_axiom_grounding('58af1959-b0ba-40a4-85e7-7beaa02cebbb', only_real_chaos_selects_competence, empirically_contingent).
narrative_ontology:cs_axiom('58af1959-b0ba-40a4-85e7-7beaa02cebbb', secondary, simulated_trauma_creates_false_confidence).
narrative_ontology:cs_axiom_status(simulated_trauma_creates_false_confidence, holdable).
narrative_ontology:cs_axiom_grounding('58af1959-b0ba-40a4-85e7-7beaa02cebbb', simulated_trauma_creates_false_confidence, empirically_contingent).
narrative_ontology:cs_reference_frame('58af1959-b0ba-40a4-85e7-7beaa02cebbb', post_catastrophe_learning_cycle).
narrative_ontology:cs_drift_state('58af1959-b0ba-40a4-85e7-7beaa02cebbb', long_peacetime_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('58af1959-b0ba-40a4-85e7-7beaa02cebbb', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention_doctrine_proponents).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, high_reliability_organizations).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_engineers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the proponents of the doctrine that only real catastrophes provide sufficient learning. They benefit from the intellectual inertia and the perceived 'realism' of their position, which often dismisses alternative learning methods as insufficient. Their professional identity is tied to this view.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention_doctrine_proponents, beneficiary,
    institutional, generational, identity_locked, global).

% Organizations operating in high-risk environments (e.g., nuclear power, aviation) that strive for zero accidents. They are victims of this constraint because it implies their continuous efforts in safety and simulation are inherently insufficient, leading to a constant, unresolvable pressure to 'do more' or accept inevitable failure, despite significant investment in safety systems.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, high_reliability_organizations, payer,
    organized, biographical, constrained, national).

% Professionals dedicated to designing and implementing safety systems. They are victims because their work is implicitly devalued by the doctrine, suggesting that their preventative measures are ultimately futile without the 'real' test of catastrophe. Their professional identity is often tied to preventing, not just reacting to, failure.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_engineers, payer,
    moderate, biographical, identity_locked, global).

% Individuals directly involved in operating high-risk systems. They bear the ultimate cost of this constraint, as it implies they are perpetually at risk of catastrophic failure, and that their daily competence is only truly validated by surviving an actual disaster. Their options are limited by their employment and the inherent risks of their roles.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators, payer,
    powerless, immediate, trapped, local).

% Experts in creating high-fidelity training simulations. They are excluded from the core conversation about 'true' competence maintenance, as their methods are deemed insufficient by the catastrophe-as-selector doctrine. They would argue for the efficacy of their tools in building resilience.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_designers, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It implicitly coordinates the focus of safety efforts towards post-catastrophe analysis and resilience, rather than pre-catastrophe prevention, by asserting that true learning only occurs after failure.
% TRANSFER_FUNCTION: Transfers a sense of inevitability and fatalism regarding safety from the proponents of the doctrine to organizations and individuals striving for proactive safety, effectively devaluing continuous improvement efforts in peacetime.
% ABSENT_VOICES: Proponents of proactive safety, simulation-based learning, and near-miss analysis are implicitly dismissed or excluded, as their methods are considered inadequate by this doctrine. They would argue for the efficacy of continuous, non-catastrophic learning.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, organizations would be freed from the fatalistic view that only catastrophe teaches. Investment in proactive safety, simulation, and near-miss learning would be re-legitimized and potentially increase, leading to a shift in safety culture and resource allocation.
% FOUNDING_PROBLEM: The problem of maintaining high levels of organizational competence and vigilance during long periods of operational success, where complacency can set in.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the doctrine attest that the problem of complacency in peacetime is still live, citing historical examples of organizations failing after long periods of success. Critics acknowledge the problem of complacency but dispute that catastrophe is the only or best solution, pointing to alternative learning methods.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the psychological and organizational cost of maintaining a fatalistic view that devalues proactive safety. Suppression (0.7) arises from the difficulty of challenging a deeply ingrained, 'realistic' perspective that often dismisses alternatives as naive. The theater ratio (0.2) is low because the doctrine is genuinely held, but it can lead to performative 'lessons learned' after minor incidents that don't fundamentally challenge the core belief.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the doctrine's proponents, this is a 'mountain' of organizational reality – an unchangeable truth about human nature and learning. From the perspective of safety engineers and organizations striving for proactive safety, it is a 'snare' that traps them in a cycle of inevitable failure and devalues their efforts.
 *
 * DIRECTIONALITY LOGIC:
 *   Proponents of the doctrine benefit from its intellectual dominance and the validation it provides for their worldview (identity_locked beneficiary). High-reliability organizations, safety engineers, and frontline operators are victims, bearing the psychological and practical costs of this fatalistic view (payers with constrained or trapped exit options). Simulation designers are excluded, as their methods are deemed insufficient.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_evidence_for_catastrophe_necessity,
    'Is there robust empirical evidence that only actual catastrophes provide the necessary selection pressure for competence, or are alternative learning mechanisms equally effective?',
    'Longitudinal studies comparing safety outcomes in organizations using different learning strategies (catastrophe-driven vs. simulation/near-miss driven) in similar high-risk domains.',
    'If alternatives are proven effective, the ''catastrophe as necessary selector'' doctrine would be reclassified as a snare or piton, as its core premise would be empirically refuted. If it holds, it would move closer to a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_evidence_for_catastrophe_necessity, empirical, 'Tests the core empirical claim of the doctrine.').

omega_variable(
    definition_of_competence_maintenance,
    'Is ''competence maintenance'' defined solely by survival of extreme events, or does it include proactive prevention and adaptive capacity in non-catastrophic conditions?',
    'Conceptual analysis and expert consensus on the scope and definition of ''organizational competence'' in high-reliability systems.',
    'A broader definition of competence would weaken the ''catastrophe as necessary selector'' claim, as it would acknowledge other forms of learning. A narrow definition would reinforce it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_competence_maintenance, conceptual, 'Examines the definitional boundaries of ''competence'' in this context.').

omega_variable(
    psychological_cost_of_fatalism,
    'What is the measurable psychological and motivational cost of this fatalistic doctrine on safety professionals and frontline operators?',
    'Psychometric studies and qualitative interviews with affected personnel, measuring burnout, morale, and perceived efficacy of safety interventions.',
    'High psychological costs would increase the measured extractiveness and suppression, reinforcing the snare classification, as the doctrine imposes a significant burden beyond its stated function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_cost_of_fatalism, empirical, 'Quantifies the human cost of the doctrine''s underlying fatalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1980, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(cata_tr_t1990, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cata_tr_t2010, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(cata_tr_t2024, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t1980, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(cata_be_t1990, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(cata_be_t2000, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(cata_be_t2010, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(cata_be_t2024, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1980, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(cata_su_t1990, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(cata_su_t2000, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(cata_su_t2010, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(cata_su_t2024, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention__hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_avoidance_retention' kernel. It asserts that only actual catastrophes provide the necessary selection pressure for competence. Sibling readings offer alternative mechanisms for competence maintenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
