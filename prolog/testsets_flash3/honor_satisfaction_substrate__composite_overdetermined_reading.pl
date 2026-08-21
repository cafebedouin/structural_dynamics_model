% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Dueling's Decline: Composite Overdetermined Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This reading posits that the decline of dueling was not due to a single
 *   cause but was 'overdetermined' by the simultaneous and entangled
 *   operation of exogenous legal/institutional suppression and endogenous
 *   cultural delegitimation. It views dueling as a Tangled Rope: a
 *   coordination mechanism for honor that also extracted costs (violence,
 *   legal risk) and was actively enforced by social norms. Its decline
 *   involved both the breaking of this 'rope' by external force and the
 *   erosion of the 'mountain' (the honor substrate) that gave it meaning. The
 *   claimed type is Tangled Rope because, even in decline, the remnants of
 *   the honor code and the state's active suppression maintained a coercive
 *   structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Dueling's Decline: Composite Overdetermined Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, 'cf8aa9f0-d825-4068-b023-25b809eb3a96').
narrative_ontology:cs_kernel_codification('cf8aa9f0-d825-4068-b023-25b809eb3a96', implicit).
narrative_ontology:cs_authority_grounding('cf8aa9f0-d825-4068-b023-25b809eb3a96', practice).
narrative_ontology:cs_interpretation_layer_present('cf8aa9f0-d825-4068-b023-25b809eb3a96').
narrative_ontology:cs_reading_relation('cf8aa9f0-d825-4068-b023-25b809eb3a96', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf8aa9f0-d825-4068-b023-25b809eb3a96', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('cf8aa9f0-d825-4068-b023-25b809eb3a96', foundational, decline_is_overdetermined_by_multiple_causes).
narrative_ontology:cs_axiom_status(decline_is_overdetermined_by_multiple_causes, holdable).
narrative_ontology:cs_axiom_grounding('cf8aa9f0-d825-4068-b023-25b809eb3a96', decline_is_overdetermined_by_multiple_causes, empirically_contingent).
narrative_ontology:cs_axiom('cf8aa9f0-d825-4068-b023-25b809eb3a96', foundational, exogenous_and_endogenous_factors_are_causally_entangled).
narrative_ontology:cs_axiom_status(exogenous_and_endogenous_factors_are_causally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('cf8aa9f0-d825-4068-b023-25b809eb3a96', exogenous_and_endogenous_factors_are_causally_entangled, empirically_contingent).
narrative_ontology:cs_reference_frame('cf8aa9f0-d825-4068-b023-25b809eb3a96', dueling_as_legitimate_honor_satisfaction).
narrative_ontology:cs_drift_state('cf8aa9f0-d825-4068-b023-25b809eb3a96', late_19th_century_decline, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cf8aa9f0-d825-4068-b023-25b809eb3a96', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, emerging_bourgeoisie).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_seekers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively enforced laws against dueling, viewing it as a challenge to state monopoly on violence and a barbaric practice. Benefited from increased social order and legitimacy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Benefited from the decline of dueling as it removed a violent, aristocratic practice that was incompatible with their values of civility, commerce, and rational dispute resolution. Their cultural norms gained ascendancy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, emerging_bourgeoisie, beneficiary,
    organized, generational, mobile, national).

% Were culturally bound to the honor code that necessitated dueling for satisfaction. Faced legal penalties and social delegitimization for upholding their traditional practices. Their identity was deeply intertwined with the practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_seekers, payer,
    powerful, biographical, identity_locked, regional).

% Individuals who engaged in duels, facing direct legal consequences and increasing social stigma. Their options were to abandon the practice, face legal repercussions, or find alternative, less formal means of 'satisfaction'.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_participants, payer,
    moderate, immediate, constrained, local).

% Analyze the historical forces leading to dueling's decline, examining both legal changes and shifts in cultural norms. Their work seeks to understand the complex interplay of these factors.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, if violent, mechanism for resolving disputes and maintaining social status within aristocratic circles, ensuring that challenges to honor were met with a prescribed response.
% TRANSFER_FUNCTION: Transferred social legitimacy and the right to violence from individuals (aristocrats) to the state, while simultaneously shifting cultural capital from traditional honor-based systems to emerging dignity-based norms.
% ABSENT_VOICES: The voices of those who might have sought to reform dueling from within the honor code, rather than abolish it, were increasingly marginalized by both state suppression and cultural shifts. Their attempts to maintain a 'gentlemanly' form of dueling were ultimately unsuccessful.
% DISAPPEARANCE_RATIONALE: If the composite forces leading to dueling's decline had not operated, the social and legal landscape would be significantly different. Aristocratic honor systems would likely have persisted longer, and the state's monopoly on violence might have been challenged more frequently. The cultural shift towards 'dignity' would have been slower or taken a different form.
% FOUNDING_PROBLEM: Dueling emerged as a means to resolve disputes and uphold personal and family honor in societies where state legal systems were either weak or not seen as adequate for matters of personal affront.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and cultural anthropologists corroborate that the state's legal authority and the broader cultural shift towards dignity have largely rendered the original 'problem' of honor satisfaction obsolete in its dueling form. While honor remains, its satisfaction mechanisms have transformed.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).
:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.4) reflects the inherent costs of dueling (risk of death/injury, legal penalties) and the social pressure to participate. Suppression (0.7) is high due to both legal prohibitions and the increasing social stigma. Theater ratio (0.1) is low because the practice, while declining, remained a serious affair for those who engaged in it, not merely a performance. Accessibility collapse (0.75) is high as both legal and cultural avenues for dueling closed off. Resistance (0.2) is low, reflecting the diminishing number of participants and the growing societal consensus against the practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and the bourgeoisie, the decline of dueling was a positive development, a move towards a more civilized society. From the perspective of the aristocratic honor seekers, it was a loss of a fundamental mechanism for maintaining their social standing and personal integrity. This reading acknowledges both perspectives as contributing to the overdetermined decline.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal apparatus and the emerging bourgeoisie are beneficiaries, gaining social order and cultural ascendancy. Aristocratic honor seekers and dueling participants are payers, bearing the costs of legal penalties and social delegitimization. The 'identity_locked' exit option for aristocrats highlights their deep cultural entanglement with the honor code, making exit from the practice a profound challenge to their self-conception.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (resolving honor disputes) became 'dead' as the cultural substrate shifted and legal alternatives emerged. However, the persistence of some honor-based conflicts, even without dueling, suggests a transformation rather than a complete disappearance of the underlying social dynamic. The classification as Tangled Rope captures the hybrid nature of its decline, where both coordination failure and extractive enforcement played roles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_entanglement,
    'To what extent were the legal/institutional suppression and the honor code transformation causally independent, versus mutually reinforcing?',
    'Detailed historical case studies comparing regions with differing legal enforcement timelines and cultural shifts, using counterfactual analysis to isolate effects.',
    'If pathways were largely independent, the decline is a simpler additive effect. If highly entangled, it reinforces the ''overdetermined'' nature and the difficulty of isolating single causes, supporting the composite reading''s complexity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_entanglement, empirical, 'Degree of causal entanglement between legal suppression and cultural change.').

omega_variable(
    honor_code_persistence_form,
    'Did the honor code truly ''transform'' or merely ''retreat'' into less visible forms, maintaining its normative force for some groups?',
    'Sociological analysis of contemporary ''cultures of honor'' in specific sub-groups, examining their dispute resolution mechanisms and attitudes towards violence.',
    'If the honor code merely retreated, the ''mountain erosion'' aspect of this reading is weaker, and the ''practice_decline_reading'' (emphasizing exogenous enforcement) gains strength. If it genuinely transformed, this reading''s emphasis on endogenous delegitimation is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_code_persistence_form, conceptual, 'Nature of honor code''s persistence post-dueling decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1850, 0.09).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1700, 0.3).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.35).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1850, 0.38).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1850, 0.65).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_satisfaction_substrate' kernel. It emphasizes the composite, overdetermined nature of dueling's decline, combining legal suppression and cultural transformation. It is linked to 'practice_decline_reading' and 'cultural_contraction_reading' as sibling interpretations of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
