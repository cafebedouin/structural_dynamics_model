% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Dueling as Cognitively Unthinkable (Contraction Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint describes the historical process by which dueling, once a
 *   legitimate means of honor satisfaction, became not merely illegal or
 *   socially frowned upon, but cognitively unthinkable – a category-level
 *   impossibility in Western societies. This 'contraction reading' argues
 *   that the practice was evacuated from the possibility space of social
 *   action, rather than merely suppressed or declining. The constraint is
 *   claimed as a Mountain because its persistence is due to a fundamental
 *   shift in social cognition and normative structure, making it appear as an
 *   irreducible limit on behavior, rather than an actively enforced rule.
 *   Beneficiaries are the state and the bourgeois order, which benefit from
 *   this shift, but do not actively 'extract' from its operation once the
 *   cognitive shift is complete.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.05).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.95).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Dueling as Cognitively Unthinkable (Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '17188139-fada-417d-97ec-03559b6bbca8').
narrative_ontology:cs_kernel_codification('17188139-fada-417d-97ec-03559b6bbca8', implicit).
narrative_ontology:cs_authority_grounding('17188139-fada-417d-97ec-03559b6bbca8', practice).
narrative_ontology:cs_interpretation_layer_present('17188139-fada-417d-97ec-03559b6bbca8').
narrative_ontology:cs_reading_relation('17188139-fada-417d-97ec-03559b6bbca8', honor_satisfaction_mechanism__decline_reading, influences).
narrative_ontology:cs_reading_relation('17188139-fada-417d-97ec-03559b6bbca8', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('17188139-fada-417d-97ec-03559b6bbca8', foundational, dueling_is_cognitively_impossible).
narrative_ontology:cs_axiom_status(dueling_is_cognitively_impossible, holdable).
narrative_ontology:cs_axiom_grounding('17188139-fada-417d-97ec-03559b6bbca8', dueling_is_cognitively_impossible, conventional).
narrative_ontology:cs_reference_frame('17188139-fada-417d-97ec-03559b6bbca8', post_dueling_unthinkability).
narrative_ontology:cs_drift_state('17188139-fada-417d-97ec-03559b6bbca8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('17188139-fada-417d-97ec-03559b6bbca8', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, state_legal_system).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, bourgeois_social_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, former_dueling_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of dueling, as it consolidates its monopoly on violence and legal dispute resolution. No longer needs to actively suppress dueling, as the practice has become unthinkable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_legal_system, beneficiary,
    institutional, generational, analytical, national).

% Benefits from the shift away from honor-based violence towards more 'rational' and legalistic forms of conflict resolution, aligning with its values of order and commerce. The cognitive shift reinforces its normative framework.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, bourgeois_social_order, beneficiary,
    organized, generational, mobile, national).

% Individuals who, in a prior era, would have participated in dueling for honor. They are now 'payers' in the sense that their prior means of honor satisfaction is no longer available or even conceivable, forcing adaptation to new social norms. Their identity is locked into a social order that no longer validates dueling.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, former_dueling_participants, payer,
    powerless, biographical, identity_locked, local).

% Study the historical processes by which dueling became unthinkable, analyzing the mechanisms of normative change and cognitive shifts in social practices. They are outside the direct operation of the constraint but observe its effects.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social behavior by establishing a shared understanding that dueling is not a legitimate or even possible means of resolving disputes or satisfying honor, thereby channeling conflict into other, state-sanctioned mechanisms.
% TRANSFER_FUNCTION: Transfers the 'right' to violence and dispute resolution from individuals (and their honor codes) to the state and its legal apparatus. It also transfers social capital from those who might have gained honor through dueling to those who succeed within the new, non-violent social order.
% ABSENT_VOICES: The 'honor culture' that once legitimized dueling is absent, not merely suppressed but conceptually evacuated. Its proponents, if they could speak from the past, would find the contemporary social order incomprehensible in its rejection of dueling as a valid practice.
% DISAPPEARANCE_RATIONALE: If the cognitive impossibility of dueling 'disappeared' overnight, it would mean a return to a prior social state where dueling was conceivable. This would represent a fundamental rearrangement of social norms and the state's monopoly on violence, but the constraint itself (the 'unthinkability') is a description of a stable, achieved state, not an active mechanism that could vanish. Its 'disappearance' would be a reversal of history, not a removal of an active force.
% FOUNDING_PROBLEM: The problem of uncontrolled private violence and challenges to state authority posed by the honor culture and the practice of dueling.
% FOUNDING_PROBLEM_CORROBORATION: Legal codes and historical records from the period of dueling's decline, as well as sociological analyses of state formation and the rise of bourgeois norms, corroborate that dueling was seen as a problem. The 'dead' status is attested by the complete absence of dueling as a social practice and its conceptual impossibility in modern legal and social thought, corroborated by historical consensus.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that once dueling becomes unthinkable, no party is actively 'paying' for its non-existence; it's a settled state. Suppression is very high (0.95) because the constraint is maintained by a deep, internalized normative structure that makes alternatives (dueling) almost impossible to conceive. Accessibility collapse (0.98) is near total, as the very idea of dueling as a viable option has vanished. Resistance is negligible (0.01) because there is no active constituency for dueling. The theater ratio is 0.0 because there is no performative maintenance; the constraint operates at a deep, cognitive level. The claimed type is Mountain because this reading posits a fundamental, almost natural-law-like shift in social reality, where dueling is no longer a 'thing' that can be chosen.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and the bourgeois order, the 'unthinkability' of dueling is a natural, beneficial evolution. From the perspective of the historical 'honor culture,' this shift would represent a profound loss and an incomprehensible reordering of social values. The constraint's Mountain classification reflects the contemporary, post-shift perspective where the absence of dueling is taken as a given.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal system and the bourgeois social order are beneficiaries because the absence of dueling consolidates their power and normative framework. Former dueling participants are 'payers' in a historical sense, as their traditional means of honor satisfaction was removed from the social lexicon, forcing a re-evaluation of identity and social interaction. Analytical historians are observers, studying the phenomenon without being subject to its direct operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' was to eliminate dueling, which it achieved by making it unthinkable. The constraint itself is the achieved state of 'unthinkability.' The classification as Mountain prevents mislabeling this deep cognitive shift as a mere Snare or Tangled Rope, which would imply ongoing active extraction or coordination, rather than a fundamental reordering of social possibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the ''unthinkability'' of dueling a genuine natural law of social evolution, or a deeply embedded social construct that could, in principle, be reversed?',
    'Comparative historical sociology examining other societies where similar practices were re-legitimized, or a counterfactual analysis of conditions under which dueling might become conceivable again.',
    'If a social construct, the Mountain classification is a ''false summit,'' and the constraint might be reclassified as a deeply internalized Snare or Tangled Rope, implying a hidden, persistent extractive mechanism that benefits the current social order. If a natural law, the Mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between a fundamental social shift and a deeply internalized, but reversible, construct.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''honor_satisfaction_mechanism'' kernel. Does this ''contraction_reading'' accurately capture the structural disappearance of dueling, or do sibling readings (decline_reading, composite_reading) offer a more complete or accurate account?',
    'Further historical and sociological research, particularly focusing on the cognitive and normative shifts rather than just frequency of practice or multiplicity of causes. The engine''s cross-reading analysis will compare the structural properties of this reading against its siblings.',
    'If sibling readings are more accurate, this constraint''s classification might be superseded or integrated into a broader ''composite'' constraint, altering the understanding of dueling''s disappearance from a ''Mountain'' of cognitive impossibility to a ''Tangled Rope'' of multiple, interacting suppressive forces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity regarding the most accurate reading of the honor satisfaction mechanism kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1800, 0.02).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1850, 0.01).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1900, 0.0).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1800, 0.07).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1850, 0.06).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1750, 0.8).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1800, 0.9).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1850, 0.93).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1900, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
