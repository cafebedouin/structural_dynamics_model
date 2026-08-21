% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Code as Normative Substrate (Practice Decline Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story, 'Honor Code as Normative Substrate (Practice
 *   Decline Reading)', examines the persistence of the honor code as a
 *   normative framework despite the decline of dueling as a practice. It
 *   argues that dueling declined primarily due to exogenous enforcement
 *   mechanisms (legal prohibitions, institutional barriers, and changing
 *   opportunity costs), rather than a fundamental transformation or
 *   delegitimation of the honor code itself. The honor code, in this reading,
 *   continues to function as a 'rope' by coordinating social behavior around
 *   reputation, but its traditional 'satisfaction' mechanism (dueling) is
 *   suppressed. This is one reading of the 'honor_satisfaction_substrate'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.25).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Code as Normative Substrate (Practice Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '4671c0c0-c2f1-412d-be0a-18f35d10e57f').
narrative_ontology:cs_kernel_codification('4671c0c0-c2f1-412d-be0a-18f35d10e57f', implicit).
narrative_ontology:cs_authority_grounding('4671c0c0-c2f1-412d-be0a-18f35d10e57f', practice).
narrative_ontology:cs_interpretation_layer_present('4671c0c0-c2f1-412d-be0a-18f35d10e57f').
narrative_ontology:cs_reading_relation('4671c0c0-c2f1-412d-be0a-18f35d10e57f', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4671c0c0-c2f1-412d-be0a-18f35d10e57f', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('4671c0c0-c2f1-412d-be0a-18f35d10e57f', foundational, honor_code_normative_persistence).
narrative_ontology:cs_axiom_status(honor_code_normative_persistence, holdable).
narrative_ontology:cs_axiom_grounding('4671c0c0-c2f1-412d-be0a-18f35d10e57f', honor_code_normative_persistence, conventional).
narrative_ontology:cs_axiom('4671c0c0-c2f1-412d-be0a-18f35d10e57f', foundational, exogenous_enforcement_primary_driver).
narrative_ontology:cs_axiom_status(exogenous_enforcement_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('4671c0c0-c2f1-412d-be0a-18f35d10e57f', exogenous_enforcement_primary_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('4671c0c0-c2f1-412d-be0a-18f35d10e57f', honor_code_with_dueling_as_option).
narrative_ontology:cs_drift_state('4671c0c0-c2f1-412d-be0a-18f35d10e57f', post_legal_prohibition_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4671c0c0-c2f1-412d-be0a-18f35d10e57f', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_bound_individuals).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, social_order_maintainers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, would_be_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who still feel the pull of honor, but are deterred from dueling by legal and social consequences. They benefit from the clarity of the honor code but are constrained by the practical impossibility of its traditional satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_bound_individuals, beneficiary,
    moderate, biographical, constrained, local).

% Legal and institutional authorities (police, courts, military command) who actively enforce anti-dueling laws and norms. They benefit from the reduction of violence and maintenance of public order, and their enforcement is the primary mechanism of the constraint's persistence.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, social_order_maintainers, agenda_setter,
    institutional, generational, analytical, national).

% Individuals who, in the absence of legal prohibition, might resort to dueling to satisfy honor. They bear the cost of suppressed action, facing severe legal penalties and social ostracization if they attempt to duel.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, would_be_duelists, payer,
    powerless, immediate, trapped, local).

% Academics who study the evolution of honor codes and dueling practices. They analyze the interplay of normative structures and exogenous enforcement, providing an analytical perspective on the constraint's dynamics.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code provides a framework for resolving perceived slights and maintaining social standing, coordinating individual behavior around a shared understanding of reputation and respect. The legal prohibition coordinates public safety by preventing violent resolution of disputes.
% TRANSFER_FUNCTION: The constraint transfers the right to violent self-redress from individuals to the state, and transfers the cost of maintaining public order (through enforcement) to the state and its citizens. It also transfers the burden of finding alternative dispute resolution mechanisms to individuals.
% ABSENT_VOICES: Historical proponents of dueling as a necessary mechanism for maintaining honor and social hierarchy are absent from contemporary discourse. They would argue that the current system fails to adequately address certain forms of insult or challenge to reputation.
% DISAPPEARANCE_RATIONALE: If the legal and institutional barriers to dueling vanished overnight, while the honor code remained as a normative substrate, there would likely be a resurgence of dueling or similar forms of violent redress, particularly in subcultures where honor remains salient. Social order would be disrupted, and new forms of conflict resolution would emerge or old ones would be re-established.
% FOUNDING_PROBLEM: The problem of unchecked violence arising from personal disputes and challenges to honor, which dueling was originally intended to regulate but often exacerbated.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and sociologists corroborate that the problem of personal violence and the need for regulated dispute resolution remains live, even if the specific form of dueling has declined. They point to ongoing issues of interpersonal violence and the state's role in maintaining peace, corroborating the continued relevance of the underlying problem from an institutional perspective.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is relatively low (0.25) because the honor code itself, while demanding, is not inherently extractive in this reading; the 'extraction' comes from the state's suppression of dueling, which prevents individuals from pursuing a traditional form of honor satisfaction. Suppression is high (0.70) due to active legal prohibition and institutional disincentives against dueling. Theater ratio is low (0.10) as the enforcement against dueling is genuine and effective, not merely performative. The decline in extractiveness over time reflects the increasing effectiveness of exogenous suppression, making the 'cost' of dueling so high that it becomes unthinkable for most, thus reducing the 'extraction' of the choice itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'honor_bound_individuals', the constraint is a 'tangled rope' or even a 'snare' because it coordinates their behavior around honor while simultaneously denying them the traditional means of satisfying it, forcing them into constrained alternatives. From the 'social_order_maintainers' perspective, it's a 'rope' or even a 'mountain' (social order) because it effectively suppresses violence and maintains public peace.
 *
 * DIRECTIONALITY LOGIC:
 *   Social order maintainers are beneficiaries (d near 0.0) as they achieve public order. Honor-bound individuals are beneficiaries of the code's clarity but targets of the dueling prohibition (d near 0.5, leaning slightly target). Would-be duelists are clear targets (d near 1.0) as they are directly suppressed. Cultural historians are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the decline of dueling as a 'mountain' (natural cultural evolution) or a 'piton' (atrophied practice) by highlighting the active, exogenous enforcement that sustains the constraint. It shows that the 'mandate' to prevent dueling is still very much 'live' and actively enforced, even if the honor code itself has attenuated. The constraint is a 'rope' that coordinates away from violence, but it requires active suppression to hold, indicating it's not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_code_internal_transformation,
    'To what extent did the honor code itself undergo an internal transformation (e.g., shift from ''culture of honor'' to ''culture of dignity'') that contributed to the decline of dueling, independent of exogenous enforcement?',
    'Detailed historical and anthropological analysis of primary sources (diaries, literature, legal records) to identify changes in the *meaning* and *social function* of honor over time, and the emergence of alternative, non-violent forms of status competition.',
    'If significant internal transformation is found, it would support the ''cultural_contraction_reading'' or ''composite_overdetermined_reading'', suggesting the constraint is less a ''rope'' sustained by external force and more a ''mountain'' (cultural evolution) or ''tangled rope'' (hybrid of internal and external factors). This would shift the balance of causality from external suppression to internal cultural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_code_internal_transformation, empirical, 'Assessing the role of endogenous cultural shifts versus exogenous enforcement in dueling''s decline.').

omega_variable(
    dueling_thinkability_threshold,
    'At what point did dueling become ''unthinkable'' rather than merely ''impractical'' for the majority of honor-bound individuals, and what was the primary driver of this shift?',
    'Qualitative historical analysis of public discourse, legal records, and personal accounts to identify the tipping point where dueling transitioned from a risky but viable option to a socially unacceptable and morally repugnant act. This would involve examining the relative weight of legal penalties versus changing moral sentiments.',
    'If dueling became unthinkable primarily due to a shift in moral sentiment (internal delegitimation), it would strengthen the ''cultural_contraction_reading''. If it remained thinkable but was simply too costly or risky due to enforcement, it supports this ''practice_decline_reading''. This impacts whether the constraint is a ''rope'' (coordination under pressure) or closer to a ''mountain'' (internalized norm).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dueling_thinkability_threshold, conceptual, 'Distinguishing between impracticality and unthinkability in the decline of dueling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1850, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement(hono_tr_t1890, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1890, 0.12).
narrative_ontology:measurement(hono_tr_t1910, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1910, 0.1).
narrative_ontology:measurement(hono_tr_t1930, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1870, 0.3).
narrative_ontology:measurement(hono_be_t1890, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1890, 0.28).
narrative_ontology:measurement(hono_be_t1910, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1910, 0.26).
narrative_ontology:measurement(hono_be_t1930, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1930, 0.25).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1950, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1850, 0.5).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1870, 0.6).
narrative_ontology:measurement(hono_su_t1890, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(hono_su_t1910, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1910, 0.68).
narrative_ontology:measurement(hono_su_t1930, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(hono_su_t1950, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_satisfaction_substrate' kernel, focusing on the role of exogenous enforcement in the decline of dueling. Sibling readings explore internal cultural transformation and overdetermined causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
