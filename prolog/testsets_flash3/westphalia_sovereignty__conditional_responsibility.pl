% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Sovereignty as Conditional Responsibility (R2P)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint, known as the Responsibility to Protect (R2P), posits
 *   that state sovereignty is not absolute but conditional on a state's
 *   responsibility to protect its own population from mass atrocities. If a
 *   state fails in this duty, the international community has a
 *   responsibility to intervene. This reading of sovereignty lowers the
 *   threshold for external interference, granting adjudicative authority to
 *   international bodies and legitimizing humanitarian intervention. It is a
 *   contested framework within international law and political theory.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.65).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.7).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Sovereignty as Conditional Responsibility (R2P)").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, 'bb858996-effd-4528-ae4e-8229b1426599').
narrative_ontology:cs_kernel_codification('bb858996-effd-4528-ae4e-8229b1426599', formalized).
narrative_ontology:cs_authority_grounding('bb858996-effd-4528-ae4e-8229b1426599', lineage).
narrative_ontology:cs_interpretation_layer_present('bb858996-effd-4528-ae4e-8229b1426599').
narrative_ontology:cs_reading_relation('bb858996-effd-4528-ae4e-8229b1426599', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('bb858996-effd-4528-ae4e-8229b1426599', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('bb858996-effd-4528-ae4e-8229b1426599', foundational, sovereignty_is_conditional_on_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional_on_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('bb858996-effd-4528-ae4e-8229b1426599', sovereignty_is_conditional_on_responsibility, deontological).
narrative_ontology:cs_axiom('bb858996-effd-4528-ae4e-8229b1426599', foundational, international_community_has_residual_responsibility).
narrative_ontology:cs_axiom_status(international_community_has_residual_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('bb858996-effd-4528-ae4e-8229b1426599', international_community_has_residual_responsibility, deontological).
narrative_ontology:cs_reference_frame('bb858996-effd-4528-ae4e-8229b1426599', post_rwanda_srebrenica_consensus).
narrative_ontology:cs_drift_state('bb858996-effd-4528-ae4e-8229b1426599', contemporary_geopolitical_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bb858996-effd-4528-ae4e-8229b1426599', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_threat).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, states_failing_to_protect_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, non_compliant_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states face potential external intervention and loss of territorial control when they fail to protect their own populations from mass atrocities. Their sovereignty is conditional on internal conduct, and they bear the costs of intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, states_failing_to_protect_populations, payer,
    powerful, immediate, trapped, national).

% These coalitions, often led by powerful states or regional organizations, gain legitimacy and a mandate to intervene in the internal affairs of other states under the R2P doctrine. They define the conditions for intervention and execute it.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, agenda_setter,
    institutional, biographical, mobile, global).

% Institutions like the UN Security Council gain enhanced authority to adjudicate state conduct and authorize interventions, strengthening their role in international affairs. They benefit from the expanded scope of legitimate action.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, generational, constrained, global).

% These populations are the primary intended beneficiaries, receiving protection from mass atrocities when their own state fails to provide it. Their lives and safety are theoretically secured by the conditional nature of sovereignty.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_threat, beneficiary,
    powerless, immediate, trapped, local).

% States that resist the R2P framework or challenge the legitimacy of interventions, even if not directly committing atrocities, bear the cost of diplomatic isolation, sanctions, or even military action. Their traditional claims to absolute sovereignty are undermined.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, non_compliant_states, payer,
    moderate, biographical, constrained, national).

% These actors, often states or scholars, argue for a strict interpretation of non-intervention and view R2P as a dangerous erosion of state sovereignty. They are often marginalized in the discourse when atrocities are ongoing, but their arguments persist.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, traditional_sovereignty_advocates, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international action to prevent and respond to mass atrocities by establishing a shared understanding of when state sovereignty becomes conditional, thereby legitimizing external intervention.
% TRANSFER_FUNCTION: Transfers adjudicative authority over internal state conduct from individual states to the international community, and potentially transfers territorial control or military resources from intervening powers to protect populations.
% ABSENT_VOICES: States and scholars advocating for absolute non-intervention are often sidelined in the face of humanitarian crises, arguing that R2P is a pretext for intervention based on geopolitical interests rather than genuine humanitarian concern.
% DISAPPEARANCE_RATIONALE: If the R2P doctrine vanished, the international community would lose a key legitimizing framework for intervention, likely leading to increased inaction in the face of atrocities, or interventions based purely on national interest without a humanitarian veneer. The global response to mass atrocities would fundamentally reorganize.
% FOUNDING_PROBLEM: The international community's failure to prevent or respond effectively to mass atrocities (e.g., Rwanda, Srebrenica) due to strict interpretations of state sovereignty and non-intervention.
% FOUNDING_PROBLEM_CORROBORATION: The UN Secretary-General's reports, independent human rights organizations, and academic analyses consistently corroborate the ongoing challenge of mass atrocities and the need for effective international response, supporting the continued relevance of R2P's founding problem.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because it imposes a significant cost on states that fail to protect their populations, including potential loss of territorial inviolability. Suppression (0.7) is also high, as it requires active enforcement through diplomatic pressure, sanctions, or military intervention to compel compliance or punish non-compliance. Theater ratio (0.4) reflects that while the humanitarian justification is real, interventions are often influenced by geopolitical interests, leading to selective application. The slight dip in extractiveness and suppression at the end of the interval reflects a period of reduced intervention and increased debate over R2P's application.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intervening powers and global governance institutions, R2P is a necessary evolution of international law to prevent atrocities. From the perspective of states targeted for intervention or those advocating for absolute sovereignty, it is an extractive mechanism that undermines national self-determination and can be abused for geopolitical gain. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   States failing to protect their populations are direct targets (high d), facing the most severe consequences. Humanitarian intervention coalitions and global governance institutions are beneficiaries (low d), gaining legitimacy and authority. Populations under atrocity threat are also beneficiaries, as the constraint aims to protect them. Non-compliant states, even if not directly committing atrocities, bear costs for resisting the framework. Traditional sovereignty advocates are excluded, as their views are often marginalized in the R2P discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_selectivity_bias,
    'Is the application of R2P genuinely universal, or is it selectively applied based on geopolitical interests and power dynamics?',
    'Empirical analysis of all mass atrocity cases over a decade, comparing intervention rates and outcomes against a baseline of geopolitical interest and power projection capabilities.',
    'If selective application is confirmed, the effective extractiveness and suppression for less powerful states would be higher than measured, as the constraint would function as a tool of power rather than a universal norm. This would shift the classification closer to a Snare for those states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_selectivity_bias, empirical, 'Examines whether R2P is applied consistently or with bias.').

omega_variable(
    legitimacy_vs_effectiveness_tradeoff,
    'Does the emphasis on international legitimacy for intervention (e.g., UNSC authorization) hinder timely and effective responses to atrocities?',
    'Comparative case studies of interventions with and without explicit UNSC authorization, assessing speed, scale, and humanitarian outcomes, alongside diplomatic costs.',
    'If legitimacy requirements consistently delay or prevent effective action, the coordination function of R2P is undermined, and its ''beneficiary'' status for populations under threat becomes more theatrical. This could increase the theater_ratio and shift the classification towards a Piton or Snare for the populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_effectiveness_tradeoff, conceptual, 'Assesses the practical impact of legitimacy requirements on R2P''s effectiveness.').

omega_variable(
    sovereignty_definition_ambiguity,
    'Is ''sovereignty'' fundamentally about territorial control or about the protection of populations?',
    'Conceptual clarification through international legal scholarship and state practice, seeking convergence on a primary definition or explicit acknowledgment of a dual nature.',
    'If sovereignty is primarily defined by territorial control, R2P is a direct challenge and highly extractive. If it is primarily about population protection, R2P is a coordination mechanism. This conceptual choice influences the perceived extractiveness and legitimacy of interventions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_definition_ambiguity, conceptual, 'Ambiguity in the core definition of sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.3).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__conditional_responsibility, theater_ratio, 5, 0.35).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__conditional_responsibility, theater_ratio, 10, 0.4).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__conditional_responsibility, theater_ratio, 15, 0.45).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__conditional_responsibility, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalia_sovereignty' kernel, focusing on conditional responsibility. It directly challenges the 'absolute_non_intervention' reading and offers a different basis for intervention than 'graded_sovereignty'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
