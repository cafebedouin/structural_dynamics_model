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
 *   This constraint represents the 'Responsibility to Protect' (R2P)
 *   doctrine, which posits that state sovereignty is not absolute but
 *   conditional on a state's responsibility to protect its own population
 *   from mass atrocities. If a state fails in this duty, the international
 *   community has a responsibility to intervene. This is one reading of the
 *   broader 'Westphalian Sovereignty' kernel, which is highly contested. The
 *   constraint is claimed as a Tangled Rope because it genuinely aims to
 *   coordinate humanitarian action but also involves significant extraction
 *   of traditional state prerogatives and requires active enforcement against
 *   non-compliant states.
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
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, 'f75a42ca-ca4d-4310-abb2-8973d30c1145').
narrative_ontology:cs_kernel_codification('f75a42ca-ca4d-4310-abb2-8973d30c1145', formalized).
narrative_ontology:cs_authority_grounding('f75a42ca-ca4d-4310-abb2-8973d30c1145', lineage).
narrative_ontology:cs_interpretation_layer_present('f75a42ca-ca4d-4310-abb2-8973d30c1145').
narrative_ontology:cs_reading_relation('f75a42ca-ca4d-4310-abb2-8973d30c1145', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('f75a42ca-ca4d-4310-abb2-8973d30c1145', westphalia_sovereignty__graded_sovereignty, influences).
narrative_ontology:cs_axiom('f75a42ca-ca4d-4310-abb2-8973d30c1145', foundational, sovereignty_is_conditional_on_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional_on_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('f75a42ca-ca4d-4310-abb2-8973d30c1145', sovereignty_is_conditional_on_responsibility, deontological).
narrative_ontology:cs_axiom('f75a42ca-ca4d-4310-abb2-8973d30c1145', secondary, international_community_has_right_to_intervene).
narrative_ontology:cs_axiom_status(international_community_has_right_to_intervene, holdable).
narrative_ontology:cs_axiom_grounding('f75a42ca-ca4d-4310-abb2-8973d30c1145', international_community_has_right_to_intervene, conventional).
narrative_ontology:cs_reference_frame('f75a42ca-ca4d-4310-abb2-8973d30c1145', post_cold_war_humanitarian_imperative).
narrative_ontology:cs_drift_state('f75a42ca-ca4d-4310-abb2-8973d30c1145', contemporary_great_power_rivalry, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f75a42ca-ca4d-4310-abb2-8973d30c1145', '').
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

% These states face potential external intervention and loss of territorial control when they fail to protect their own populations from mass atrocities. Their sovereignty is conditional on internal conduct, making them targets of this constraint.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, states_failing_to_protect_populations, payer,
    powerful, immediate, trapped, national).

% These coalitions, often led by powerful states or regional organizations, gain legitimacy and a mandate to act under this reading of sovereignty. They are the primary enforcers and beneficiaries of the expanded scope for intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, agenda_setter,
    institutional, biographical, mobile, global).

% Organizations like the UN Security Council gain increased authority to adjudicate state conduct and authorize interventions. This reading strengthens their role in international affairs and global security.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, generational, constrained, global).

% These populations are the intended ultimate beneficiaries, as the constraint aims to provide them with protection from mass atrocities when their own state fails. They gain a theoretical right to external protection.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_threat, beneficiary,
    powerless, immediate, trapped, local).

% States that resist the principle of conditional sovereignty, even if not actively committing atrocities, bear the cost of increased scrutiny and potential loss of international standing. They are pressured to conform to new norms.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, non_compliant_states, payer,
    moderate, biographical, constrained, national).

% These actors, often states prioritizing traditional Westphalian principles, are structurally excluded from the legitimizing discourse of conditional sovereignty. Their arguments for absolute non-interference are sidelined in favor of interventionist justifications.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, absolute_non_intervention_advocates, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international action to prevent and respond to mass atrocities by establishing a shared understanding of when state sovereignty can be overridden for humanitarian purposes.
% TRANSFER_FUNCTION: Transfers the absolute right to territorial inviolability from states to the international community, conditional on states' performance of their responsibility to protect populations. This enables the transfer of military and political resources for intervention.
% ABSENT_VOICES: States and political theorists who adhere to an absolute interpretation of non-intervention are often marginalized in discussions about R2P, arguing that it undermines the foundational principles of international law and can be abused for geopolitical interests.
% DISAPPEARANCE_RATIONALE: If conditional sovereignty vanished, the international community would revert to a stricter non-intervention norm, making humanitarian interventions much harder to justify legally and politically. States would regain absolute territorial inviolability, potentially at the cost of populations facing atrocities, and global governance institutions would lose a key adjudicative power.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to mass atrocities (e.g., Rwanda, Srebrenica) in the late 20th century, leading to calls for a re-evaluation of sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The UN Secretary-General's reports, independent commissions on intervention and state sovereignty, and numerous academic analyses from international law and political science scholars corroborate the problem's existence and its ongoing relevance, citing continued atrocities and debates over intervention.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because it fundamentally alters the traditional understanding of state sovereignty, imposing external conditions and potentially leading to military intervention. Suppression (0.7) is also high, as it requires overcoming significant resistance from states that prioritize non-intervention and involves the threat or use of force. The theater ratio (0.4) reflects that while the principle is often invoked, actual interventions are selective and sometimes driven by geopolitical interests, leading to a gap between stated humanitarian goals and practical application. The slight dip in extractiveness and suppression towards the end of the interval reflects increased contestation and a more cautious approach to intervention post-Libya.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states facing intervention, this is a Snare, an imposition of external power. From the perspective of humanitarian coalitions, it is a Rope, a necessary coordination mechanism for global security. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   States failing to protect their populations are clear targets (high d), as they face the direct consequences of intervention. Humanitarian intervention coalitions and global governance institutions are beneficiaries (low d), gaining legitimacy and authority. Populations under threat are also beneficiaries, as the constraint aims to protect them. Non-compliant states, even if not committing atrocities, bear costs through increased scrutiny and pressure. Advocates of absolute non-intervention are excluded, as their core premise is directly challenged by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling R2P as pure extraction by acknowledging its genuine coordination function (protecting populations). However, the high extractiveness and suppression, coupled with a non-negligible theater ratio, indicate that it operates as a Tangled Rope, where the coordination function is intertwined with significant power transfer and enforcement, rather than a pure Rope. The 'contested' status of the founding problem further highlights the ongoing debate about its true function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_selectivity_bias,
    'Is the application of conditional sovereignty (R2P) genuinely universal, or is it selectively applied based on geopolitical interests and power dynamics?',
    'Systematic analysis of all mass atrocity cases over a decade, comparing intervention outcomes against a neutral set of criteria (e.g., severity of atrocities, state capacity, regional stability) to detect patterns of bias.',
    'If selective, the effective extractiveness and suppression for weaker states would be higher than measured, as the constraint would function as a tool of power projection rather than a universal norm. This would push the classification closer to a Snare for targeted states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_selectivity_bias, empirical, 'Assesses whether R2P is applied consistently or with bias.').

omega_variable(
    legitimacy_of_intervention_authority,
    'Does the international community, particularly the UN Security Council, possess sufficient moral and legal legitimacy to unilaterally adjudicate and enforce conditional sovereignty?',
    'Analysis of global public opinion, state compliance patterns, and the frequency of vetoes/abstentions in the UNSC on R2P-related resolutions. A decline in perceived legitimacy would manifest as increased resistance and non-compliance.',
    'If legitimacy is low, the constraint''s persistence relies more heavily on raw power and less on normative acceptance, increasing its effective suppression and extractiveness for all states, pushing it towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_intervention_authority, conceptual, 'Examines the normative grounding of R2P''s enforcement mechanism.').

omega_variable(
    sovereignty_definition_ambiguity,
    'Is ''sovereignty'' fundamentally an absolute, indivisible concept, or can it be legitimately reinterpreted as conditional without undermining the entire international legal order?',
    'Conceptual analysis of international legal theory and state practice over time. This is a deep philosophical and legal debate with no definitive empirical resolution.',
    'If sovereignty is fundamentally absolute, this reading is a conceptual Snare, as its core premise is built on a contradiction. If it is legitimately conditional, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_definition_ambiguity, conceptual, 'Fundamental conceptual debate over the nature of state sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t2001, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2001, 0.3).
narrative_ontology:measurement(west_tr_t2007, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2007, 0.35).
narrative_ontology:measurement(west_tr_t2013, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2013, 0.4).
narrative_ontology:measurement(west_tr_t2018, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2018, 0.45).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t2001, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(west_be_t2007, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement(west_be_t2013, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement(west_be_t2018, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t2001, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(west_su_t2007, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2007, 0.65).
narrative_ontology:measurement(west_su_t2013, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2013, 0.7).
narrative_ontology:measurement(west_su_t2018, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is the 'conditional_responsibility' reading of the 'westphalia_sovereignty' kernel. It directly challenges the 'absolute_non_intervention' reading and influences the 'graded_sovereignty' reading by establishing a baseline for intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
