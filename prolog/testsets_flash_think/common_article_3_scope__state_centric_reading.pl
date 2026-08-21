% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope (State-Centric Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This constraint represents the 'state-centric reading' of Common Article
 *   3 of the Geneva Conventions, which holds that CA3 applies only when a
 *   conflict meets specific intensity and organization thresholds, thereby
 *   excluding low-level violence and domestic law enforcement operations from
 *   IHL's scope. This interpretation is often advanced by states to preserve
 *   their operational discretion and sovereignty in internal security
 *   matters. The constraint is claimed as a Tangled Rope because it purports
 *   to coordinate by providing legal clarity for states, but it
 *   simultaneously extracts by denying IHL protections to certain groups,
 *   requiring active enforcement of its narrow scope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.75).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.85).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '9a24bfb0-bbf6-47a9-9a65-047dcd6b4563').
narrative_ontology:cs_kernel_codification('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', fixed_text).
narrative_ontology:cs_authority_grounding('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', lineage).
narrative_ontology:cs_interpretation_layer_present('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563').
narrative_ontology:cs_reading_relation('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', foundational, state_sovereignty_primary_over_ihl_scope).
narrative_ontology:cs_axiom_status(state_sovereignty_primary_over_ihl_scope, holdable).
narrative_ontology:cs_axiom_grounding('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', state_sovereignty_primary_over_ihl_scope, deontological).
narrative_ontology:cs_axiom('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', foundational, ihl_applies_only_to_interstate_or_equivalent_conflict).
narrative_ontology:cs_axiom_status(ihl_applies_only_to_interstate_or_equivalent_conflict, holdable).
narrative_ontology:cs_axiom_grounding('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', ihl_applies_only_to_interstate_or_equivalent_conflict, conventional).
narrative_ontology:cs_reference_frame('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', post_cold_war_internal_conflicts_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9a24bfb0-bbf6-47a9-9a65-047dcd6b4563', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, states_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, military_commanders).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As primary interpreters and enforcers of IHL, states benefit from a narrow application of Common Article 3, retaining maximum discretion over internal security operations and avoiding international scrutiny for low-level violence. They actively advocate for and apply these thresholds.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, states_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain operational flexibility and reduced legal constraints when IHL, specifically Common Article 3, is not deemed applicable to conflicts below certain intensity and organization thresholds. This allows them to use tactics and rules of engagement that might be prohibited in a full armed conflict.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, military_commanders, beneficiary,
    powerful, biographical, mobile, global).

% Are denied the minimum humanitarian protections afforded by Common Article 3, such as humane treatment and judicial guarantees, if their conflict does not meet the state-centric thresholds for intensity and organization. They are treated as criminals under domestic law, often without IHL safeguards.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, local).

% Are exposed to greater risk and lack specific IHL protections when the violence they experience is classified below the Common Article 3 thresholds. Their suffering is framed as a domestic law enforcement issue, potentially leading to less international attention and protection.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_conflict, payer,
    powerless, immediate, trapped, local).

% Consistently argue for a broader application of Common Article 3 to ensure minimum humanitarian standards in all situations of organized violence. They are excluded from the state-centric interpretation process but exert pressure through advocacy, reporting, and legal challenges.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_advocates, excluded,
    organized, biographical, constrained, global).

% Are tasked with interpreting and applying IHL, including Common Article 3. While they consider state practice, their interpretations can sometimes challenge or reinforce the state-centric view, influencing the constraint's future application.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_courts, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to distinguish between situations requiring the full application of International Humanitarian Law (armed conflict) and those governed by domestic law enforcement, thereby coordinating state action and preserving national sovereignty.
% TRANSFER_FUNCTION: Transfers legal discretion and reduced accountability to states and their military forces in situations of internal violence, at the cost of denying minimum humanitarian protections to irregular combatants and civilians caught in conflicts deemed below the IHL threshold.
% ABSENT_VOICES: Irregular combatants and civilians directly affected by violence that falls below the state-centric thresholds for Common Article 3 application. They would advocate for universal minimum humanitarian standards regardless of conflict classification, but their voices are not directly represented in the legal interpretation process.
% DISAPPEARANCE_RATIONALE: If this state-centric interpretation vanished, states would face immediate and significant pressure to apply IHL more broadly to all forms of organized violence, fundamentally altering their legal and operational frameworks for internal security, potentially increasing international oversight and accountability.
% FOUNDING_PROBLEM: To provide a clear legal distinction between armed conflicts (governed by IHL) and internal disturbances or tensions (governed by domestic law), preventing the over-extension of IHL into purely internal affairs and preserving state sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: States and their legal advisors consistently assert the ongoing necessity of this distinction to maintain the integrity of domestic legal systems and prevent IHL from becoming an all-encompassing framework. Some traditional IHL scholars also corroborate the historical intent to limit IHL's scope to genuine armed conflicts.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the severe cost borne by irregular combatants and civilians who are denied IHL protections under this narrow interpretation. The high suppression (0.85) indicates the active legal and political efforts by states to maintain and defend these thresholds against broader interpretations. The low theater ratio (0.10) signifies that this is a genuine legal interpretation with real-world consequences, not a performative facade. Extractiveness and suppression have shown a slight increase over the interval, reflecting the ongoing contestation and states' continued efforts to limit IHL's reach in an era of prevalent internal conflicts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states and military commanders, this interpretation provides necessary clarity and preserves sovereignty, functioning as a coordination mechanism. From the perspective of excluded combatants and civilians, it is a mechanism of extraction that denies fundamental protections. The engine's per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   States and military commanders are beneficiaries (low directionality) as they gain discretion and operational flexibility. Irregular combatants and civilians in low-intensity conflicts are targets (high directionality) as they bear the cost of denied protections. Human rights advocates are excluded, actively challenging the interpretation but not participating in its formulation. International courts act as observers, influencing the interpretation through jurisprudence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_motivation_ambiguity,
    'Is the state-centric interpretation of Common Article 3''s scope a genuine effort to clarify IHL''s boundaries, or a strategic move by states to avoid accountability for actions in internal conflicts?',
    'Analysis of state practice in situations where IHL application would be inconvenient, coupled with judicial review of proportionality and necessity in such contexts. If states consistently apply IHL broadly when it benefits them but narrowly when it imposes costs, it suggests strategic avoidance.',
    'If primarily strategic, the constraint''s effective extractiveness is higher, and its coordination function is more theatrical, pushing it closer to a Snare. If genuinely clarifying, it retains its Tangled Rope classification with a stronger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_motivation_ambiguity, conceptual, 'Ambiguity regarding the underlying motivation for the narrow interpretation of CA3 scope.').

omega_variable(
    civilian_protection_impact,
    'What is the empirically measurable impact on civilian protection and human rights outcomes when Common Article 3 is not applied to situations of organized violence below the state-centric thresholds?',
    'Comparative empirical studies of civilian casualties, detention conditions, and judicial guarantees in conflicts classified below CA3 thresholds versus those where CA3 is applied, controlling for other conflict variables.',
    'If the data show significantly worse outcomes for civilians and combatants when CA3 is not applied, it would strengthen the argument for the ''expansive_human_rights_reading'' and highlight the severe extraction of the state-centric view. If outcomes are comparable, it would weaken the extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_protection_impact, empirical, 'Empirical impact of CA3 non-application on civilian protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__state_centric_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(comm_tr_t1996, common_article_3_scope__state_centric_reading, theater_ratio, 1996, 0.1).
narrative_ontology:measurement(comm_tr_t2002, common_article_3_scope__state_centric_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(comm_tr_t2008, common_article_3_scope__state_centric_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(comm_tr_t2014, common_article_3_scope__state_centric_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(comm_tr_t2020, common_article_3_scope__state_centric_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__state_centric_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(comm_be_t1996, common_article_3_scope__state_centric_reading, base_extractiveness, 1996, 0.68).
narrative_ontology:measurement(comm_be_t2002, common_article_3_scope__state_centric_reading, base_extractiveness, 2002, 0.7).
narrative_ontology:measurement(comm_be_t2008, common_article_3_scope__state_centric_reading, base_extractiveness, 2008, 0.72).
narrative_ontology:measurement(comm_be_t2014, common_article_3_scope__state_centric_reading, base_extractiveness, 2014, 0.74).
narrative_ontology:measurement(comm_be_t2020, common_article_3_scope__state_centric_reading, base_extractiveness, 2020, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__state_centric_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(comm_su_t1996, common_article_3_scope__state_centric_reading, suppression_requirement, 1996, 0.78).
narrative_ontology:measurement(comm_su_t2002, common_article_3_scope__state_centric_reading, suppression_requirement, 2002, 0.8).
narrative_ontology:measurement(comm_su_t2008, common_article_3_scope__state_centric_reading, suppression_requirement, 2008, 0.82).
narrative_ontology:measurement(comm_su_t2014, common_article_3_scope__state_centric_reading, suppression_requirement, 2014, 0.84).
narrative_ontology:measurement(comm_su_t2020, common_article_3_scope__state_centric_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_article_3_scope' kernel. It defines the scope of IHL application based on strict intensity and organization thresholds, contrasting with more expansive or evolving interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
