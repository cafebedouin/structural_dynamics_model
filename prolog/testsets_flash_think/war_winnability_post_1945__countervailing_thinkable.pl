% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Nuclear War Winnability (Countervailing Strategy Reading)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint describes the strategic doctrine that nuclear weapons,
 *   while imposing immense costs, do not render great-power total war
 *   categorically unwinnable, particularly through concepts like counterforce
 *   targeting. This reading emerged during the Cold War and persists in
 *   various forms, justifying continued strategic planning for victory
 *   scenarios. It is presented as a 'Tangled Rope' because it attempts to
 *   coordinate strategic stability while simultaneously extracting resources
 *   and undermining arms control efforts through its underlying premise.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.65).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.75).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Nuclear War Winnability (Countervailing Strategy Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, 'b0fd3505-e92e-4eee-bba7-bb2f17779379').
narrative_ontology:cs_kernel_codification('b0fd3505-e92e-4eee-bba7-bb2f17779379', formalized).
narrative_ontology:cs_authority_grounding('b0fd3505-e92e-4eee-bba7-bb2f17779379', lineage).
narrative_ontology:cs_interpretation_layer_present('b0fd3505-e92e-4eee-bba7-bb2f17779379').
narrative_ontology:cs_reading_relation('b0fd3505-e92e-4eee-bba7-bb2f17779379', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('b0fd3505-e92e-4eee-bba7-bb2f17779379', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('b0fd3505-e92e-4eee-bba7-bb2f17779379', foundational, nuclear_war_is_thinkable).
narrative_ontology:cs_axiom_status(nuclear_war_is_thinkable, holdable).
narrative_ontology:cs_axiom_grounding('b0fd3505-e92e-4eee-bba7-bb2f17779379', nuclear_war_is_thinkable, conventional).
narrative_ontology:cs_axiom('b0fd3505-e92e-4eee-bba7-bb2f17779379', secondary, counterforce_targeting_is_feasible).
narrative_ontology:cs_axiom_status(counterforce_targeting_is_feasible, holdable).
narrative_ontology:cs_axiom_grounding('b0fd3505-e92e-4eee-bba7-bb2f17779379', counterforce_targeting_is_feasible, empirically_contingent).
narrative_ontology:cs_reference_frame('b0fd3505-e92e-4eee-bba7-bb2f17779379', cold_war_strategic_stability).
narrative_ontology:cs_drift_state('b0fd3505-e92e-4eee-bba7-bb2f17779379', post_cold_war_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b0fd3505-e92e-4eee-bba7-bb2f17779379', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, nuclear_powers).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_advocates).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, peace_movements).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the continuous need for force modernization and strategic planning implied by a 'winnable' nuclear war doctrine, ensuring mission continuity and funding.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, agenda_setter,
    institutional, generational, arbitrage, global).

% Develop and refine doctrines for limited nuclear war, counterforce targeting, and escalation control, maintaining a sense of purpose and relevance for their profession.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_planners, agenda_setter,
    institutional, biographical, constrained, global).

% Maintain strategic options and deterrence credibility by asserting the possibility of limited victory, even if at high cost, against potential adversaries.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, nuclear_powers, beneficiary,
    institutional, generational, constrained, global).

% Bear the cost of undermined efforts to reduce nuclear arsenals and prevent proliferation, as the doctrine of winnability provides a rationale for continued arms races.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_advocates, payer,
    organized, generational, constrained, global).

% Bear the psychological and political cost of a world where nuclear war is considered a viable, albeit costly, strategic option, perpetuating existential threat.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, peace_movements, payer,
    organized, biographical, constrained, global).

% Indirectly fund the extensive military budgets and nuclear modernization programs required to support the doctrine of limited nuclear victory.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, taxpayers, payer,
    powerless, immediate, trapped, national).

% Their arguments that nuclear war is categorically unwinnable are excluded from the operational planning and doctrinal development that this reading enables, despite their intellectual contributions.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, deterrence_theorists_unthinkable_reading, excluded,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for strategic stability and escalation control by defining limits and conditions for nuclear use, preventing immediate total war through a credible threat of limited, but decisive, retaliation.
% TRANSFER_FUNCTION: Transfers significant resources (funding, intellectual capital, scientific talent) to strategic planning, force modernization, and intelligence gathering, from other societal priorities. It also transfers the risk of limited nuclear war to populations, from the pursuit of absolute peace or disarmament.
% ABSENT_VOICES: Proponents of 'existential deterrence' (the 'deterrence_unthinkable' reading) who argue that any nuclear war is unwinnable and planning for it is dangerous. Also, global civil society advocating for nuclear abolition, whose calls for disarmament are sidelined by this doctrine.
% DISAPPEARANCE_RATIONALE: If the idea of limited nuclear victory vanished, strategic planning would fundamentally shift towards pure deterrence or disarmament, military budgets would be reallocated, and the international security architecture would be profoundly altered. The nuclear arsenals might remain, but their purpose and deployment would change dramatically.
% FOUNDING_PROBLEM: How to manage the existence of nuclear weapons without either succumbing to total war (through lack of credible deterrence) or abandoning strategic options entirely (through disarmament or a belief in absolute unwinnability); how to maintain deterrence credibility while avoiding uncontrolled escalation.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and defense ministries of nuclear powers corroborate this, citing ongoing threats and the need for credible deterrence. Arms control experts and historians, while disagreeing with the *solution* (i.e., winnability), corroborate the existence of the underlying problem of nuclear management and the historical context that led to this doctrine.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderately high (0.65) due to the diversion of resources to nuclear arsenals and the perpetuation of a high-risk international environment. Suppression is high (0.75) as this doctrine actively suppresses alternative strategic paradigms (e.g., pure deterrence, nuclear abolition) that would challenge its premises. Theater ratio is moderate (0.40); while there is genuine strategic planning, some aspects of 'winnability' may serve to justify military budgets rather than reflect realistic outcomes. Resistance is high (0.70) from arms control advocates and peace movements who contest the very premise of winnable nuclear war.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers and strategic planners, this doctrine provides a necessary framework for deterrence and national security in a nuclear age. From the perspective of arms control advocates and peace movements, it is a dangerous rationalization that perpetuates the threat of nuclear war and diverts resources from human needs. The engine will compute these divergent classifications based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The military-industrial complex, strategic planners, and nuclear powers are beneficiaries, gaining mission continuity, professional relevance, and strategic flexibility. Arms control advocates, peace movements, and taxpayers are victims, bearing the costs of increased military spending, heightened risk, and frustrated disarmament efforts. The 'deterrence_theorists_unthinkable_reading' are excluded, as their perspective is not integrated into the operational logic of this doctrine.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_necessity_vs_rationalization,
    'Is the doctrine of limited nuclear victory a genuine strategic necessity for maintaining deterrence credibility, or primarily a rationalization for continued military-industrial complex funding and institutional relevance?',
    'Analysis of declassified strategic documents, independent economic audits of defense spending, and historical case studies of nuclear crises where ''limited victory'' doctrines were (or were not) operationally viable.',
    'If primarily a rationalization, the constraint''s effective extraction is higher, and its coordination function is more theatrical, pushing it closer to a Snare. If a genuine necessity, its coordination function is stronger, supporting its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_necessity_vs_rationalization, empirical, 'Ambiguity between strategic necessity and institutional rationalization.').

omega_variable(
    escalation_control_feasibility,
    'Is limited nuclear war truly controllable, or does any use inevitably escalate to total war, rendering ''limited victory'' an incoherent concept?',
    'Further theoretical modeling of escalation dynamics, historical analysis of near-miss incidents, and expert consensus from interdisciplinary panels on nuclear risk. (Direct empirical resolution is impossible without catastrophic events).',
    'If escalation is inevitable, the ''winnability'' premise collapses, making the constraint''s coordination function largely theatrical and its extraction purely coercive, pushing it towards a Snare. If control is feasible, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_control_feasibility, conceptual, 'Feasibility of escalation control in a nuclear exchange.').

omega_variable(
    winnability_definition_ambiguity,
    'What constitutes ''victory'' in a nuclear exchange, and is such a victory achievable without unacceptable costs that negate the very concept of ''winning''?',
    'Philosophical and ethical analysis of ''victory'' in a post-nuclear context, combined with detailed scenario planning that accounts for long-term societal and environmental consequences beyond immediate military objectives.',
    'If ''victory'' is redefined to include post-conflict societal collapse, the premise of winnability is undermined, increasing the perceived extraction and theatricality of the doctrine. If a meaningful ''victory'' can be defined, the doctrine''s internal coherence is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(winnability_definition_ambiguity, conceptual, 'Conceptual ambiguity of ''victory'' in nuclear war.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1960, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(war__tr_t1970, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1970, 0.32).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(war__tr_t1990, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1990, 0.37).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(war__tr_t2010, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(war__tr_t2020, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(war__be_t1970, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement(war__be_t1990, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(war__be_t2010, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(war__be_t2020, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(war__su_t1970, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1980, 0.71).
narrative_ontology:measurement(war__su_t1990, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement(war__su_t2010, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(war__su_t2020, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, arms_control_treaties).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, nuclear_modernization_programs).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'war_winnability_post_1945' kernel, focusing on the continued possibility of limited victory through counterforce targeting, in contrast to readings emphasizing categorical unwinnability or rhetorical contraction. It directly influences arms control efforts and nuclear modernization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
