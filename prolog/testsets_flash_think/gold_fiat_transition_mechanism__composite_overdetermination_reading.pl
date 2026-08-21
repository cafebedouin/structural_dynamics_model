% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Composite Overdetermination Reading of Gold-Fiat Transition
 *   domain: Monetary Economics / Political Economy / History of Economic Thought
 *
 * SUMMARY:
 *   This constraint represents the 'composite overdetermination' reading of
 *   the gold-fiat transition, arguing that it was a convergence of multiple
 *   independent structural changes (telecommunications, Bretton Woods
 *   collapse, labor shifts, legal tender maturation) rather than a single
 *   causal event like the Nixon Shock. This reading challenges simpler, more
 *   singular narratives, coordinating a more nuanced historical understanding
 *   while potentially displacing alternative interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.55).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.65).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Composite Overdetermination Reading of Gold-Fiat Transition").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Monetary Economics / Political Economy / History of Economic Thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'f00f90ce-5070-468e-bb6f-3048432bac2a').
narrative_ontology:cs_kernel_codification('f00f90ce-5070-468e-bb6f-3048432bac2a', distributed).
narrative_ontology:cs_authority_grounding('f00f90ce-5070-468e-bb6f-3048432bac2a', expertise).
narrative_ontology:cs_interpretation_layer_present('f00f90ce-5070-468e-bb6f-3048432bac2a').
narrative_ontology:cs_reading_relation('f00f90ce-5070-468e-bb6f-3048432bac2a', gold_fiat_transition_mechanism__automatic_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('f00f90ce-5070-468e-bb6f-3048432bac2a', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_axiom('f00f90ce-5070-468e-bb6f-3048432bac2a', foundational, monetary_transition_is_multi_causal).
narrative_ontology:cs_axiom_status(monetary_transition_is_multi_causal, holdable).
narrative_ontology:cs_axiom_grounding('f00f90ce-5070-468e-bb6f-3048432bac2a', monetary_transition_is_multi_causal, empirically_contingent).
narrative_ontology:cs_axiom('f00f90ce-5070-468e-bb6f-3048432bac2a', secondary, nixon_shock_symbolic_not_causal).
narrative_ontology:cs_axiom_status(nixon_shock_symbolic_not_causal, holdable).
narrative_ontology:cs_axiom_grounding('f00f90ce-5070-468e-bb6f-3048432bac2a', nixon_shock_symbolic_not_causal, empirically_contingent).
narrative_ontology:cs_reference_frame('f00f90ce-5070-468e-bb6f-3048432bac2a', complex_historical_causality_framework).
narrative_ontology:cs_drift_state('f00f90ce-5070-468e-bb6f-3048432bac2a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f00f90ce-5070-468e-bb6f-3048432bac2a', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, academic_historians).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, political_economists).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, proponents_of_simpler_narratives).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, single_cause_theorists).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and refine the composite overdetermination reading, benefiting from its acceptance as it validates their methodological approach to complex historical events. They actively shape the narrative through research and publication.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, academic_historians, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, academic_historians, beneficiary).

% Benefit from the acceptance of this reading as it aligns with their interdisciplinary approach, emphasizing the interplay of economic, political, and technological factors. They use this framework in their analyses.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, political_economists, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of having their more singular, often ideologically-driven, explanations for the gold-fiat transition challenged and potentially displaced in academic and public discourse. Their intellectual capital is devalued.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, proponents_of_simpler_narratives, payer,
    moderate, biographical, constrained, global).

% Experience intellectual and professional costs as their theories, which attribute the transition to a single dominant factor (e.g., Nixon's decision, a specific technological shift), are undermined by the composite overdetermination argument.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, single_cause_theorists, payer,
    moderate, biographical, constrained, global).

% Bears the cognitive cost of engaging with a more complex and less easily digestible historical narrative, potentially leading to confusion or disengagement compared to simpler, more definitive explanations. They are passive recipients of academic debates.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, general_public, payer,
    powerless, immediate, trapped, national).

% Operate within the fiat monetary system and observe historical interpretations of its origins. While not directly affected by the academic debate, their understanding of the system's foundations can be influenced by prevailing historical narratives.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_bankers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates understanding among scholars and the public regarding the complex, multi-causal nature of the gold-fiat transition, moving away from singular or simplistic explanations.
% TRANSFER_FUNCTION: Transfers intellectual authority and narrative dominance from proponents of simpler, single-cause explanations to those advocating for a composite, overdetermined account of the transition.
% ABSENT_VOICES: Those who prefer a clear, singular villain or hero in historical narratives, or those whose ideological frameworks depend on a specific, simplified origin story for fiat money, are implicitly excluded from the nuanced academic discourse.
% DISAPPEARANCE_RATIONALE: If the composite overdetermination reading vanished, the understanding of a pivotal historical event would revert to simpler, potentially less accurate, and more contested narratives. The intellectual landscape of monetary history would reorganize around these less nuanced explanations.
% FOUNDING_PROBLEM: The problem of accurately explaining a complex historical shift (the gold-fiat transition) that was widely attributed to singular events or causes, leading to incomplete or misleading understandings.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians and economists, particularly those specializing in economic history and political economy, corroborate that the challenge of accurately representing complex historical causality remains a live problem, and that singular narratives often persist despite evidence of overdetermination.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is 'tangled_rope' because this reading serves a genuine coordination function by providing a more comprehensive historical account, but it also involves asymmetric extraction. Proponents of simpler narratives 'pay' by having their views challenged and potentially marginalized in academic discourse. The 'active enforcement' comes from the peer review process, academic publishing standards, and the intellectual competition to establish dominant historical interpretations. Extractiveness and suppression are moderate-to-high, reflecting the intellectual contestation and the effort required to establish and defend a complex narrative against simpler ones.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of academic historians, this reading is a robust, evidence-based coordination of understanding. From the perspective of those holding simpler narratives, it can feel like an extractive imposition that undermines their established views. The engine's classification as 'tangled_rope' captures this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic historians and political economists are beneficiaries and agenda-setters, as this reading validates their methodological approaches and enhances their intellectual authority. Proponents of simpler narratives and single-cause theorists are targets, as their intellectual positions are challenged. The general public is a payer, bearing the cognitive load of a more complex narrative, even if it is more accurate. Central bankers are observers, as the debate is about the historical origins of the system they operate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''gold_fiat_transition_mechanism'' kernel, or merely a more detailed version of another reading?',
    'Analysis of core axiomatic differences and their logical implications for other readings. If its foundational axioms directly contradict those of other readings, it is distinct.',
    'If not distinct, it should be merged or reclassified as a sub-reading, reducing the complexity of the kernel family. If distinct, its unique contribution to understanding the transition is affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this reading''s unique identity within the kernel family.').

omega_variable(
    empirical_corroboration_strength,
    'How strongly is the ''composite overdetermination'' empirically supported against simpler, single-cause narratives?',
    'Further historical and econometric research, including counterfactual analysis, to quantify the relative causal weight of each identified structural change and compare it against alternative models.',
    'Stronger empirical support would increase the reading''s coordination function and reduce the perceived extraction from proponents of simpler views. Weaker support would reduce its legitimacy and increase its ''theater_ratio'' if maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_corroboration_strength, empirical, 'Strength of evidence for multi-causal explanation.').

omega_variable(
    narrative_dominance_and_suppression,
    'To what extent does this complex reading actually displace simpler, more singular narratives in public discourse, beyond academic circles?',
    'Content analysis of popular media, educational curricula, and public opinion surveys over time to track the prevalence and acceptance of multi-causal vs. singular explanations.',
    'If the complex reading fails to gain traction outside academia, its effective suppression of simpler narratives is lower, and its coordination function is limited to a specialist audience. If it achieves broader dominance, its ''suppression'' metric is more fully realized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(narrative_dominance_and_suppression, empirical, 'Public acceptance and displacement of simpler narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(gold_tr_t1990, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(gold_tr_t2010, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(gold_tr_t2020, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(gold_be_t1990, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(gold_be_t2010, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(gold_be_t2020, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 2020, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(gold_su_t1990, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(gold_su_t2010, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(gold_su_t2020, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gold_fiat_transition_mechanism' kernel. It focuses on the multi-causal, overdetermined nature of the transition, contrasting with readings that emphasize single causal factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
