% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Normative Illegitimacy of Total War (Post-1945)
 *   domain: international_relations/law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'normative_reading_drop' of the
 *   'total_war_winnability_post1945' kernel. It posits that total war, while
 *   physically possible, became normatively illegitimate after 1945 due to
 *   the development of international law, particularly Article 2(4) of the UN
 *   Charter and subsequent humanitarian law. This reading emphasizes the role
 *   of formal legal instruments and shared international norms in
 *   constraining state behavior, rather than purely material or ideational
 *   shifts. The constraint is classified as a Rope because it solves a
 *   genuine collective action problem (preventing catastrophic war) with
 *   broad benefits, despite imposing costs on states that might otherwise
 *   pursue unlimited objectives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.25).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.3).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.25).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Normative Illegitimacy of Total War (Post-1945)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/law").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '97c5cadc-2334-4c01-88c5-dbbfd41fb03d').
narrative_ontology:cs_kernel_codification('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', formalized).
narrative_ontology:cs_authority_grounding('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', lineage).
narrative_ontology:cs_interpretation_layer_present('97c5cadc-2334-4c01-88c5-dbbfd41fb03d').
narrative_ontology:cs_reading_relation('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', foundational, prohibition_on_aggression).
narrative_ontology:cs_axiom_status(prohibition_on_aggression, holdable).
narrative_ontology:cs_axiom_grounding('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', prohibition_on_aggression, deontological).
narrative_ontology:cs_axiom('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', foundational, civilian_immunity_principle).
narrative_ontology:cs_axiom_status(civilian_immunity_principle, holdable).
narrative_ontology:cs_axiom_grounding('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', civilian_immunity_principle, deontological).
narrative_ontology:cs_reference_frame('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', post_un_charter_order).
narrative_ontology:cs_drift_state('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', contemporary_geopolitical_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97c5cadc-2334-4c01-88c5-dbbfd41fb03d', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, un_member_states).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from the normative constraint against total war, as it reduces the likelihood of widespread destruction, atrocities, and existential threats to their lives and livelihoods. They have no direct means to enforce the norm but are its primary protected class.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, generational, trapped, universal).

% As signatories to the UN Charter and other international humanitarian law, they collectively uphold and enforce the norm against total war. While they benefit from global stability, they are also constrained in their use of force, particularly against civilian populations. Their exit options are constrained by the reputational and legal costs of violating these norms.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, un_member_states, agenda_setter,
    institutional, generational, constrained, global).

% Bear the costs of this constraint by having their options for achieving political objectives through unlimited warfare curtailed. They face international condemnation, sanctions, and potential military intervention if they violate the core tenets of the norm. Their ability to exit is limited by the collective power of the international system.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, global).

% These bodies (e.g., ICJ, ICC, UN Security Council) interpret, apply, and enforce international law, including Article 2(4) and humanitarian law. They provide the formal framework for the normative constraint, though their effectiveness depends on the political will of member states.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Monitor compliance with international humanitarian law, document violations, and advocate for the protection of civilians. They act as a moral and informational check on state behavior, reinforcing the normative constraint through public pressure and reporting.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, humanitarian_organizations, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior to prevent the escalation of conflicts into total war, establishing shared boundaries on the legitimate use of force and protecting non-combatants, thereby maintaining a degree of international order and stability.
% TRANSFER_FUNCTION: Transfers the absolute right to wage war without limits from individual states to a collective security framework governed by international law. It transfers security and protection benefits to civilian populations and non-belligerent states.
% ABSENT_VOICES: Historical proponents of total war as a legitimate and necessary instrument of state policy (e.g., Clausewitzian interpretations of absolute war) are absent from contemporary international legal discourse. Future revisionist actors who might seek to dismantle the post-1945 normative order are also structurally excluded from its foundational principles.
% DISAPPEARANCE_RATIONALE: If the normative illegitimacy of total war vanished overnight, the international system would revert to a state where states could legitimately target civilian populations and infrastructure without restraint. This would lead to catastrophic conflicts, widespread human suffering, and a breakdown of the current international order, fundamentally reorganizing global security dynamics.
% FOUNDING_PROBLEM: The catastrophic human cost, widespread destruction, and existential threat posed by two World Wars, particularly with the advent of nuclear weapons, which demonstrated the unacceptability of total war as a policy instrument.
% FOUNDING_PROBLEM_CORROBORATION: Historians, international relations scholars, international legal experts, and the ongoing efforts of the United Nations and humanitarian organizations consistently corroborate the historical context and the continued relevance of preventing total war. The persistence of armed conflicts and the need for humanitarian intervention underscore the live nature of the problem, even if the specific form of 'total war' has evolved.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is low (0.25) because the primary function is coordination for collective security, with costs borne by revisionist powers being a necessary consequence of upholding the norm. Suppression (0.30) is also relatively low, reflecting that compliance is largely voluntary, driven by shared norms and reputational costs, rather than overt physical coercion. However, it requires active enforcement through international institutions and collective action. Theater ratio (0.15) is low, indicating that the commitment to these norms is largely genuine, though some states may engage in performative compliance while seeking loopholes. The slight increase in extractiveness and suppression over time reflects the ongoing challenges to the post-1945 order and the need for continuous reinforcement of these norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of global civilian populations, this constraint is a vital protection. From the perspective of UN member states, it is a necessary framework for collective security. From the perspective of revisionist powers, it is an imposed limitation on their sovereignty and strategic options. The engine's per-seat classification will reflect these divergent experiences, with beneficiaries seeing a Rope and payers potentially experiencing a Tangled Rope or even a Snare if their objectives are fundamentally incompatible with the norm.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations are clear beneficiaries (d=0.0) as the norm directly protects them. UN member states and international legal institutions act as agenda-setters and beneficiaries (d near 0.2-0.3), gaining from a more stable international system while also being constrained. Revisionist powers are the primary payers/targets (d near 0.7-0.8), as their freedom of action to wage unlimited war is curtailed. Humanitarian organizations act as observers, reinforcing the norm without direct enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing total war) remains highly live, preventing mandatrophy. The ongoing relevance of the founding problem (catastrophic war) means the constraint's function has not atrophied. The classification as a Rope, despite some extraction from revisionist powers, prevents mislabeling genuine collective security as pure extraction, acknowledging the broad benefits of preventing total war.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_structural_causation,
    'To what extent is the ''drop'' in total war attributable to normative illegitimacy (this reading) versus the structural impossibility imposed by nuclear weapons (''structural_contraction_reading'')?',
    'Counterfactual historical analysis: examining periods where nuclear deterrence was weak or absent, or where non-nuclear powers engaged in conflict, to assess the independent effect of normative constraints. Empirical studies on state decision-making processes regarding the use of force.',
    'If structural factors are dominant, this constraint''s ''rope'' classification might be overstated, as the coordination function would be less critical than physical limits. If normative factors are primary, the ''rope'' classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_vs_structural_causation, empirical, 'Distinguishing the causal weight of normative vs. structural factors in preventing total war.').

omega_variable(
    normative_vs_ideational_drift,
    'Is the normative illegitimacy primarily a result of formal legal development (this reading) or a broader, informal ideational shift in strategic culture (''strategic_culture_drift'')?',
    'Content analysis of diplomatic communications, military doctrines, and public discourse over time, alongside legal scholarship tracing the evolution of international law. Interviews with policymakers and military leaders.',
    'If ideational drift is primary, the ''requires_active_enforcement'' metric might be lower, as compliance would be more internalized. If formal legal development is primary, the ''rope'' classification is more strongly tied to institutional mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_vs_ideational_drift, conceptual, 'Distinguishing formal legal development from informal ideational shifts in constraining total war.').

omega_variable(
    norm_effectiveness_contestability,
    'How robust is the normative constraint against total war in the face of rising geopolitical tensions and revisionist challenges?',
    'Ongoing observation of state behavior in armed conflicts, analysis of violations of international humanitarian law, and the effectiveness of international responses to such violations. Expert surveys on the perceived strength of the norm.',
    'If the norm''s effectiveness is significantly eroding, the ''base_extractiveness'' and ''suppression'' metrics might need upward revision, and the ''claimed_type'' could drift towards a ''tangled_rope'' or ''snare'' if enforcement becomes purely coercive or performative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_effectiveness_contestability, empirical, 'Assessing the current and future robustness of the normative constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1965, 0.15).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2025, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel. This reading focuses on the normative and legal mechanisms, while 'structural_contraction_reading' emphasizes physical impossibility due to nuclear weapons, and 'strategic_culture_drift' focuses on ideational shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
