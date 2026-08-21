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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: The Social Obligation to Duel for Honor (Composite Overdetermined Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story, 'The Social Obligation to Duel for Honor', is
 *   presented through the 'Composite Overdetermined Reading' of the
 *   'honor_satisfaction_substrate' kernel. It describes the decline of
 *   dueling as a social practice, arguing that its disappearance was not due
 *   to a single cause but was overdetermined by the simultaneous and causally
 *   entangled operation of exogenous legal/institutional suppression and
 *   endogenous delegitimation through the transformation of the honor code
 *   itself. The constraint is claimed as a Tangled Rope, reflecting its dual
 *   function of coordinating honor disputes while extracting high costs, and
 *   the metrics track its decline over the 18th and 19th centuries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.55).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.75).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "The Social Obligation to Duel for Honor (Composite Overdetermined Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, 'ecb6d1a0-631a-489a-b680-e6959ea82a3a').
narrative_ontology:cs_kernel_codification('ecb6d1a0-631a-489a-b680-e6959ea82a3a', implicit).
narrative_ontology:cs_authority_grounding('ecb6d1a0-631a-489a-b680-e6959ea82a3a', practice).
narrative_ontology:cs_interpretation_layer_present('ecb6d1a0-631a-489a-b680-e6959ea82a3a').
narrative_ontology:cs_reading_relation('ecb6d1a0-631a-489a-b680-e6959ea82a3a', honor_satisfaction_substrate__practice_decline_reading, influences).
narrative_ontology:cs_reading_relation('ecb6d1a0-631a-489a-b680-e6959ea82a3a', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('ecb6d1a0-631a-489a-b680-e6959ea82a3a', foundational, decline_is_overdetermined).
narrative_ontology:cs_axiom_status(decline_is_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('ecb6d1a0-631a-489a-b680-e6959ea82a3a', decline_is_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('ecb6d1a0-631a-489a-b680-e6959ea82a3a', secondary, legal_and_cultural_causal_entanglement).
narrative_ontology:cs_axiom_status(legal_and_cultural_causal_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('ecb6d1a0-631a-489a-b680-e6959ea82a3a', legal_and_cultural_causal_entanglement, empirically_contingent).
narrative_ontology:cs_reference_frame('ecb6d1a0-631a-489a-b680-e6959ea82a3a', honor_code_supremacy).
narrative_ontology:cs_drift_state('ecb6d1a0-631a-489a-b680-e6959ea82a3a', post_enlightenment_legal_reform_and_cultural_shift, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('ecb6d1a0-631a-489a-b680-e6959ea82a3a', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, honor_seekers).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, social_elites).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, honor_seekers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, duel_victims_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who felt compelled by social norms to defend their honor through dueling, risking injury or death. They gained social standing and avoided ostracism, but paid a high personal cost. Their identity was deeply tied to the honor code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_seekers, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, honor_seekers, beneficiary).

% The aristocratic and gentry classes who established and maintained the honor code, initially benefiting from its role in social ordering and status differentiation. Over time, a segment of this group began to delegitimize dueling, shifting social norms.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, social_elites, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, social_elites, beneficiary).

% State and judicial bodies that increasingly criminalized dueling, imposing legal penalties and institutional barriers. Their actions constituted the exogenous suppression that contributed to dueling's decline.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Families of those killed or injured in duels, bearing the ultimate cost of the honor system without any direct benefit or means of redress within the dueling framework. They were often vocal opponents of the practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, duel_victims_families, payer,
    powerless, immediate, trapped, local).

% Scholars who analyze the historical context, social function, and eventual decline of dueling, examining both legal and cultural factors. They provide the analytical framework for understanding the constraint's overdetermined decline.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, if violent, mechanism for resolving disputes over honor and maintaining social hierarchy among elites, preventing potentially endless feuds or less structured violence.
% TRANSFER_FUNCTION: Transferred social status and reputation to participants who adhered to its rules, while extracting the risk of injury, death, and legal penalties from those compelled to duel.
% ABSENT_VOICES: Women, lower social classes, and religious moralists were largely excluded from the formal honor code and dueling practice, though they bore its social consequences. They would have argued for alternative, non-violent means of dispute resolution and a redefinition of honor.
% DISAPPEARANCE_RATIONALE: The decline of dueling was part of a broader societal shift away from 'cultures of honor' towards 'cultures of dignity'. Its disappearance fundamentally altered how personal grievances were addressed, how social status was maintained, and the role of violence in elite society. The legal and cultural landscape rearranged significantly.
% FOUNDING_PROBLEM: To provide a clear, ritualized means for gentlemen to defend their honor and resolve grievances without resorting to uncontrolled violence or endless feuds, thereby maintaining social order among elites.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal records, sociological analyses of social change, and contemporary philosophical critiques of honor culture corroborate that the original problem of uncontrolled elite violence was largely superseded by legal systems and evolving social norms, rendering dueling obsolete. This is attested by legal historians and cultural anthropologists, not just descendants of the dueling class.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.55 at interval end) reflects the diminishing, but still present, social pressure and risk associated with dueling as it became less common. Suppression (0.75) is high due to the increasing legal prohibitions and institutional barriers. Theater ratio (0.15) remains low throughout the decline, as dueling, even when rare, retained its serious, often fatal, character. Accessibility collapse (0.45) indicates that while legal alternatives existed, social pressure still made them difficult for some to accept for honor disputes. Resistance (0.55) reflects the ongoing, though diminishing, adherence to the honor code by some segments of society.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'honor_seekers' in the early part of the interval, the constraint was a necessary, if dangerous, means to maintain social standing. By the end, many saw it as an anachronism. 'Legal_authorities' consistently viewed it as a criminal act, while 'social_elites' experienced a shift in their own norms, moving from upholding to delegitimizing the practice. The engine's per-seat classification would capture these evolving and divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'honor_seekers' are both beneficiaries (gaining status) and victims (risking life), placing their directionality near symmetric but leaning towards target as costs mounted. 'Social_elites' were initially primary beneficiaries and agenda-setters, but their role shifted as the honor code transformed. 'Legal_authorities' acted as agenda-setters enforcing suppression. 'Duel_victims_families' were clear targets, bearing the ultimate costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to manage elite violence and honor disputes. This reading argues that the mandate became 'dead' not just because external forces suppressed it, but also because the underlying cultural substrate that gave it meaning transformed. The persistence of dueling in some pockets, despite legal prohibition, highlights the inertial aspect, but the overall decline indicates a resolution of its original function through both external and internal pressures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_dominance_ambiguity,
    'What was the relative causal weight of exogenous legal suppression versus endogenous cultural delegitimation in the decline of dueling?',
    'Comparative historical analysis across jurisdictions with differing legal enforcement timelines and cultural contexts, or counterfactual modeling of historical trajectories.',
    'If legal suppression was dominant, the constraint''s decline is primarily a ''rope-breaking'' event. If cultural delegitimation was dominant, it''s primarily ''mountain erosion''. This reading asserts entanglement, but the precise balance remains an open empirical question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_dominance_ambiguity, empirical, 'Relative impact of legal vs. cultural factors in dueling''s decline.').

omega_variable(
    interdependence_of_mechanisms,
    'How did legal suppression and cultural transformation causally influence each other during the decline of dueling?',
    'Detailed micro-historical studies examining how legal rulings were perceived and internalized by elites, and how changing social norms influenced the political will for legal enforcement.',
    'Understanding the feedback loops would refine the ''non-independent causal pathways'' claim, potentially revealing that one mechanism amplified or attenuated the other, leading to a more nuanced understanding of the constraint''s dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interdependence_of_mechanisms, empirical, 'Causal entanglement between legal and cultural factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1740, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1740, 0.12).
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1780, 0.14).
narrative_ontology:measurement(hono_tr_t1820, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1820, 0.15).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1860, 0.15).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1700, 0.8).
narrative_ontology:measurement(hono_be_t1740, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1740, 0.75).
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1780, 0.68).
narrative_ontology:measurement(hono_be_t1820, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1820, 0.62).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1860, 0.58).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(hono_su_t1740, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1740, 0.45).
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1780, 0.6).
narrative_ontology:measurement(hono_su_t1820, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1820, 0.7).
narrative_ontology:measurement(hono_su_t1860, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1860, 0.73).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_satisfaction_substrate' kernel. It integrates the insights of the 'practice_decline_reading' (exogenous enforcement) and the 'cultural_contraction_reading' (endogenous cultural shift), arguing for their simultaneous and entangled causal roles in dueling's decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
