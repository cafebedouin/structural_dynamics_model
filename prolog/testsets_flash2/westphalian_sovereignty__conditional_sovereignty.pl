% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty (Responsibility to Protect)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'conditional sovereignty' reading of
 *   Westphalian sovereignty, where state sovereignty is not absolute but
 *   contingent on a state's fulfillment of its responsibility to protect its
 *   own population from mass atrocities. Systematic human rights violations
 *   are seen as triggering a legitimate right, and sometimes duty, for
 *   external intervention. This reading emerged in response to failures to
 *   prevent genocide and mass killings, notably formalized as the
 *   Responsibility to Protect (R2P) doctrine. It is claimed as a Snare
 *   because it extracts autonomy from states that fail their duties, and its
 *   application is often selective and enforced by powerful actors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.4).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.65).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty (Responsibility to Protect)").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, 'd7357d35-48db-400a-89ab-6ed246e09413').
narrative_ontology:cs_kernel_codification('d7357d35-48db-400a-89ab-6ed246e09413', formalized).
narrative_ontology:cs_authority_grounding('d7357d35-48db-400a-89ab-6ed246e09413', lineage).
narrative_ontology:cs_interpretation_layer_present('d7357d35-48db-400a-89ab-6ed246e09413').
narrative_ontology:cs_reading_relation('d7357d35-48db-400a-89ab-6ed246e09413', westphalian_sovereignty__absolute_sovereignty, influences).
narrative_ontology:cs_reading_relation('d7357d35-48db-400a-89ab-6ed246e09413', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('d7357d35-48db-400a-89ab-6ed246e09413', foundational, sovereignty_is_conditional).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('d7357d35-48db-400a-89ab-6ed246e09413', sovereignty_is_conditional, deontological).
narrative_ontology:cs_axiom('d7357d35-48db-400a-89ab-6ed246e09413', foundational, responsibility_to_protect_populations).
narrative_ontology:cs_axiom_status(responsibility_to_protect_populations, holdable).
narrative_ontology:cs_axiom_grounding('d7357d35-48db-400a-89ab-6ed246e09413', responsibility_to_protect_populations, deontological).
narrative_ontology:cs_reference_frame('d7357d35-48db-400a-89ab-6ed246e09413', post_cold_war_humanitarian_consensus).
narrative_ontology:cs_drift_state('d7357d35-48db-400a-89ab-6ed246e09413', contemporary_geopolitical_fragmentation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d7357d35-48db-400a-89ab-6ed246e09413', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_human_rights_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervening_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, states_committing_violations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, populations_under_intervention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, populations_under_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states bear the direct cost of intervention, losing autonomy and potentially facing regime change. Their 'exit' is to cease violations, which may conflict with their internal political logic.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, states_committing_violations, payer,
    institutional, immediate, trapped, national).

% Benefit from the legitimation of intervention to protect human rights, advancing their normative agenda and gaining a mechanism to address severe abuses. They advocate for the application of this principle.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% These states gain moral and sometimes strategic justification for intervention, potentially expanding their influence. They bear the costs of military action but also accrue political capital. Their 'exit' is to not intervene, which may incur reputational costs.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervening_states, agenda_setter,
    institutional, biographical, constrained, global).

% While theoretically beneficiaries of protection, they often bear the immediate costs of conflict, displacement, and instability during intervention. Their situation is highly dependent on the nature and success of the intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, populations_under_intervention, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, populations_under_intervention, beneficiary).

% The primary body for authorizing legitimate intervention under international law. Its permanent members hold veto power, making authorization subject to geopolitical interests. It acts as a gatekeeper for the principle's application.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% These states observe the application of the principle, weighing its implications for their own sovereignty and potential future interventions. They may support or oppose specific interventions based on their interests and interpretation of the norm.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, non_intervening_sovereign_states, observer,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international actors to coordinate responses to mass atrocity crimes, aiming to prevent or halt systematic human rights violations when states fail to protect their own populations.
% TRANSFER_FUNCTION: Transfers the right to exclusive domestic jurisdiction from a sovereign state to the international community (or a subset of states) when that state fails its responsibility to protect its population, enabling external intervention.
% ABSENT_VOICES: Populations in states that are not deemed 'strategic' enough for intervention, or those whose suffering is not politically convenient for powerful states, remain unheard. Their plight, though severe, does not trigger the mechanism.
% DISAPPEARANCE_RATIONALE: If the principle of conditional sovereignty vanished, the international legal landscape would revert to a more absolute interpretation of state sovereignty, making legitimate external intervention for human rights violations much harder to justify, and potentially increasing impunity for perpetrators of mass atrocities.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to mass atrocities (e.g., Rwanda, Srebrenica) in the late 20th century, due to the strict interpretation of non-interference in domestic affairs.
% FOUNDING_PROBLEM_CORROBORATION: International commissions (e.g., ICISS), human rights organizations, and many UN member states (excluding some powerful states and those fearing intervention) corroborate that the problem of mass atrocities and state failure to protect remains live, necessitating this principle.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.40) is moderate because it imposes a significant, but conditional, limit on state autonomy. Suppression (0.65) is high because it requires active enforcement, often military, to overcome state resistance to intervention. Theater ratio (0.20) is present but not dominant; while there is performative rhetoric around 'humanitarian intervention,' the core function of legitimizing intervention for atrocities is real. The application of this principle is highly contested and often selective, leading to resistance from states that prioritize absolute sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intervening states and human rights advocates, this is a necessary evolution of international law to prevent atrocities. From the perspective of states targeted for intervention, or those wary of external interference, it is a violation of fundamental sovereignty and a potential pretext for geopolitical power projection. The engine's classification as a Snare reflects the coercive, extractive nature of its enforcement, even if its stated goal is protective.
 *
 * DIRECTIONALITY LOGIC:
 *   International human rights advocates and intervening states are beneficiaries, gaining a legitimate framework for action. States committing violations are clear targets/payers, losing autonomy and facing intervention. Populations under intervention are complex: theoretically beneficiaries of protection, but often immediate payers of the costs of conflict. The UN Security Council acts as an agenda-setter, controlling the formal authorization of interventions, but its actions are subject to the geopolitical interests of its permanent members.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_of_intervention,
    'Is the application of conditional sovereignty genuinely universal, or is it selectively applied based on geopolitical interests and resource availability?',
    'Empirical analysis of all cases of mass atrocities vs. actual interventions, controlling for severity and capacity. If intervention correlates strongly with strategic interest rather than atrocity severity, selectivity is confirmed.',
    'If selective, the constraint''s effective extractiveness is higher for targeted states (as it''s applied arbitrarily) and its coordination function is weaker (as it fails to coordinate universal response). This would push it closer to a pure Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selectivity_of_intervention, empirical, 'Whether intervention is applied universally or selectively.').

omega_variable(
    definition_of_systematic_violations,
    'What constitutes ''systematic human rights violations'' that trigger intervention, and who legitimately defines this threshold?',
    'Development of clear, universally accepted, and independently verifiable criteria for ''systematic violations'' by a neutral international body, with a transparent process for assessment.',
    'Ambiguity in definition allows powerful states to define the threshold opportunistically, increasing the constraint''s extractiveness and suppression for targeted states. Clearer definitions would reduce this arbitrary power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_systematic_violations, conceptual, 'Ambiguity in the threshold for intervention.').

omega_variable(
    intervention_effectiveness,
    'Do external interventions consistently achieve their stated goal of protecting populations and improving human rights, or do they often lead to unintended negative consequences?',
    'Longitudinal studies and meta-analyses of post-intervention outcomes, assessing human rights, stability, and state capacity in intervened states compared to non-intervened control groups.',
    'If interventions are often ineffective or counterproductive, the ''beneficiary'' status of populations under intervention is undermined, and the overall legitimacy of the constraint as a coordination mechanism is weakened, pushing it closer to a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_effectiveness, empirical, 'Effectiveness of interventions in achieving stated goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t2001, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(west_tr_t2007, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(west_tr_t2013, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2013, 0.25).
narrative_ontology:measurement(west_tr_t2018, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t2001, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2001, 0.3).
narrative_ontology:measurement(west_be_t2007, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2007, 0.35).
narrative_ontology:measurement(west_be_t2013, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement(west_be_t2018, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t2001, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(west_su_t2007, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2007, 0.6).
narrative_ontology:measurement(west_su_t2013, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2013, 0.68).
narrative_ontology:measurement(west_su_t2018, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
