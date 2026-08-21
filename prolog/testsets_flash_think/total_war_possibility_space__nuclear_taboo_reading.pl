% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Total War Taboo (Nuclear Reading)
 *   domain: international_relations_theory/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the normative prohibition against total war,
 *   particularly in the nuclear age, as a constructed taboo. It posits that
 *   while the material capability for total war persists, its normative
 *   legitimacy has been largely removed through international consensus,
 *   diplomatic practice, and the development of non-proliferation regimes.
 *   This reading emphasizes the role of shared norms and active enforcement
 *   in preventing catastrophic conflict, distinct from purely material
 *   deterrence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.65).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.75).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Total War Taboo (Nuclear Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations_theory/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '445ed606-4a9d-451c-a62e-70ca89cdc1bd').
narrative_ontology:cs_kernel_codification('445ed606-4a9d-451c-a62e-70ca89cdc1bd', implicit).
narrative_ontology:cs_authority_grounding('445ed606-4a9d-451c-a62e-70ca89cdc1bd', practice).
narrative_ontology:cs_interpretation_layer_present('445ed606-4a9d-451c-a62e-70ca89cdc1bd').
narrative_ontology:cs_reading_relation('445ed606-4a9d-451c-a62e-70ca89cdc1bd', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('445ed606-4a9d-451c-a62e-70ca89cdc1bd', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('445ed606-4a9d-451c-a62e-70ca89cdc1bd', foundational, total_war_is_normatively_unacceptable).
narrative_ontology:cs_axiom_status(total_war_is_normatively_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('445ed606-4a9d-451c-a62e-70ca89cdc1bd', total_war_is_normatively_unacceptable, deontological).
narrative_ontology:cs_axiom('445ed606-4a9d-451c-a62e-70ca89cdc1bd', secondary, nuclear_weapons_enabled_taboo_formation).
narrative_ontology:cs_axiom_status(nuclear_weapons_enabled_taboo_formation, holdable).
narrative_ontology:cs_axiom_grounding('445ed606-4a9d-451c-a62e-70ca89cdc1bd', nuclear_weapons_enabled_taboo_formation, empirically_contingent).
narrative_ontology:cs_reference_frame('445ed606-4a9d-451c-a62e-70ca89cdc1bd', post_hiroshima_normative_shift).
narrative_ontology:cs_drift_state('445ed606-4a9d-451c-a62e-70ca89cdc1bd', contemporary_geopolitical_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('445ed606-4a9d-451c-a62e-70ca89cdc1bd', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, global_community).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, revisionist_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, rogue_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, nuclear_non_proliferation_regime).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, international_normative_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of total war, avoiding catastrophic destruction and societal collapse. Bears diffuse costs of maintaining the international order.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, global_community, beneficiary,
    powerless, generational, constrained, global).

% Actively promote and enforce the taboo, as it underpins their own security and the stability of the international system. They benefit from avoiding direct confrontation and the costs of total war, but also bear the responsibility of maintaining the taboo.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nuclear_powers, beneficiary).

% Benefit from the stability and reduced risk of global conflict. However, they are also constrained by the non-proliferation regime, which is a key enforcement mechanism of the taboo, limiting their sovereign choices regarding defense.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states, payer).

% Bear the cost of being denied the option of total war as a means to achieve their geopolitical objectives. They often challenge the legitimacy of the taboo and the international order that enforces it, but face severe international condemnation and potential sanctions if they overtly pursue total war.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, revisionist_states, payer,
    powerful, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, revisionist_states, excluded).

% Are explicitly targeted by the enforcement mechanisms of the taboo, facing isolation and intervention if they attempt to acquire total war capabilities or threaten to use them. Their options are severely limited by the international consensus.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, rogue_actors, payer,
    moderate, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, rogue_actors, excluded).

% Serve as key institutional actors in codifying, promoting, and enforcing the total war taboo through treaties, resolutions, and diplomatic pressure. They provide a forum for collective action and normative reinforcement.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, international_organizations, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, international_organizations, observer).

% Are individuals, NGOs, or states that actively champion and reinforce the total war taboo, working to embed it deeper into international law and discourse. Their efforts are crucial for the taboo's persistence and evolution.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, agenda_setter,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent global catastrophic conflict by establishing a shared normative prohibition against total war, thereby coordinating state behavior away from escalation and existential threats.
% TRANSFER_FUNCTION: Transfers the perceived legitimacy of total war as a strategic option from individual states to a collective, normatively enforced prohibition. It transfers security benefits (avoidance of global war) to all, while extracting the freedom to pursue total war from any state.
% ABSENT_VOICES: States and non-state actors who believe that total war, or the threat of it, is a necessary or legitimate tool for their survival, expansion, or ideological goals. They are excluded from the normative consensus and often face international isolation or intervention.
% DISAPPEARANCE_RATIONALE: If the total war taboo vanished overnight, the international system would fundamentally reorganize. States would re-evaluate their security doctrines, arms races would intensify, and the risk of large-scale, potentially nuclear, conflict would dramatically increase, leading to a more unstable and dangerous world.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons after World War II, which made the traditional concept of total war (mobilizing all societal resources for victory) an unacceptable path due to the risk of mutual annihilation.
% FOUNDING_PROBLEM_CORROBORATION: Academic consensus in International Relations theory, historical records of Cold War diplomacy, ongoing non-proliferation efforts, and the continued existence of nuclear arsenals all corroborate that the founding problem (existential threat) remains live, even if the specific mechanisms of restraint are debated.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) reflects the high value of the option of total war for states seeking to fundamentally alter the international order, which is extracted by the taboo. Suppression (0.75) is high due to the active and continuous diplomatic, legal, and military efforts to enforce non-proliferation and condemn threats of total war. The theater ratio (0.25) is moderate, acknowledging that while the taboo is genuinely held, some diplomatic rhetoric and actions may be performative, especially when states seek to justify their own military actions while condemning others. Accessibility collapse (0.60) is moderate because total war remains materially possible, but normatively and politically extremely difficult. Resistance (0.55) is present from revisionist states and rogue actors who challenge the existing international order.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers and the global community largely perceive the taboo as a beneficial coordination mechanism. However, revisionist states and rogue actors experience it as a highly extractive and suppressive constraint, limiting their sovereign options and perceived security needs. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The global community, nuclear powers, and non-nuclear states are beneficiaries, as they gain from the stability and reduced risk of global conflict. Nuclear powers also act as agenda-setters, actively shaping and enforcing the taboo. Revisionist states and rogue actors are payers/victims, as the taboo extracts their perceived right or ability to wage total war. Non-nuclear states are also payers to the extent they are constrained by non-proliferation regimes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_vs_deterrence_causality,
    'Is the absence of total war primarily due to the normative taboo, or to the material deterrence provided by nuclear weapons?',
    'Counterfactual analysis of historical crises, or empirical study of state behavior in situations where normative and material pressures diverge.',
    'If deterrence is primary, the constraint''s extractiveness and suppression might be lower, as the ''choice'' to avoid total war is less free. If the taboo is primary, the constraint''s normative force is stronger, and its persistence depends more on norm maintenance than on military balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causality, empirical, 'Distinguishing the causal weight of normative prohibition versus material deterrence in preventing total war.').

omega_variable(
    taboo_universality_and_equity,
    'Does the total war taboo apply equally to nuclear and non-nuclear states, or is it experienced differently by those with and without nuclear weapons?',
    'Analysis of international responses to threats from nuclear vs. non-nuclear states, and the perceived legitimacy of nuclear deterrence doctrines.',
    'If the taboo is not universal, its coordination function is weaker, and its extraction from non-nuclear states (who are denied nuclear weapons) is more asymmetric, potentially reclassifying it closer to a Snare for those actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_universality_and_equity, conceptual, 'Assessing the equitable application and experience of the total war taboo across different state capabilities.').

omega_variable(
    norm_entrepreneur_exit_impact,
    'How resilient is the total war taboo to the exit or repudiation by key norm entrepreneurs (e.g., major powers or influential international organizations)?',
    'Observing the impact of major powers withdrawing from arms control treaties or explicitly questioning the taboo''s legitimacy on the behavior of other states and the broader international normative framework.',
    'If the taboo significantly weakens or collapses upon the exit of key actors, it suggests a higher degree of dependence on active enforcement and less internalized normative force, potentially increasing its theater ratio and extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_entrepreneur_exit_impact, empirical, 'The impact of key actors abandoning the total war taboo on its persistence and effectiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1965, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(tota_tr_t1985, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(tota_tr_t2005, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(tota_be_t1965, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(tota_be_t1985, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(tota_be_t2005, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(tota_su_t1965, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(tota_su_t1985, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1985, 0.78).
narrative_ontology:measurement(tota_su_t2005, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, nuclear_non_proliferation_treaty).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, arms_control_regimes).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel, focusing on the normative prohibition. It is linked to 'deterrence_equilibrium_reading' and 'space_contraction_reading' as alternative explanations for the absence of total war.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
