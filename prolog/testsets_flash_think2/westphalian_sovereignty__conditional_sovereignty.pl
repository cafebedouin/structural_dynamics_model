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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty Doctrine
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'conditional sovereignty' reading of
 *   Westphalian sovereignty, asserting that state sovereignty is not absolute
 *   but entails a responsibility to protect human rights. Systematic
 *   violations of these rights can legitimately trigger external
 *   intervention. While framed as a coordination mechanism for global human
 *   rights protection, its application often involves asymmetric power
 *   dynamics, leading to its classification as a snare. The claimed type
 *   (snare) reflects the underlying extractive reality, despite the stated
 *   humanitarian goals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.4).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.7).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '2db2ba79-9abb-4dae-8575-49c1312fd477').
narrative_ontology:cs_kernel_codification('2db2ba79-9abb-4dae-8575-49c1312fd477', formalized).
narrative_ontology:cs_authority_grounding('2db2ba79-9abb-4dae-8575-49c1312fd477', practice).
narrative_ontology:cs_interpretation_layer_present('2db2ba79-9abb-4dae-8575-49c1312fd477').
narrative_ontology:cs_reading_relation('2db2ba79-9abb-4dae-8575-49c1312fd477', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('2db2ba79-9abb-4dae-8575-49c1312fd477', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('2db2ba79-9abb-4dae-8575-49c1312fd477', foundational, sovereignty_is_conditional).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('2db2ba79-9abb-4dae-8575-49c1312fd477', sovereignty_is_conditional, deontological).
narrative_ontology:cs_axiom('2db2ba79-9abb-4dae-8575-49c1312fd477', foundational, human_rights_are_universal).
narrative_ontology:cs_axiom_status(human_rights_are_universal, holdable).
narrative_ontology:cs_axiom_grounding('2db2ba79-9abb-4dae-8575-49c1312fd477', human_rights_are_universal, deontological).
narrative_ontology:cs_reference_frame('2db2ba79-9abb-4dae-8575-49c1312fd477', post_cold_war_humanitarian_consensus).
narrative_ontology:cs_drift_state('2db2ba79-9abb-4dae-8575-49c1312fd477', contemporary_geopolitical_realignment, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2db2ba79-9abb-4dae-8575-49c1312fd477', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_human_rights_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, vulnerable_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, sovereign_states_violating_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legitimization of external action against human rights abuses, providing a framework for their advocacy and potential enforcement. They push for consistent application of the norm.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_human_rights_advocates, beneficiary,
    organized, generational, analytical, global).

% Are the intended beneficiaries of protection from systematic human rights violations, potentially receiving external aid or intervention when their own state fails to protect them. Their situation is often dire, with few internal options.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Bear the cost of lost autonomy and potential external intervention when accused of systematic human rights violations. They face diplomatic pressure, sanctions, or military action, limiting their sovereign control over domestic affairs.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, sovereign_states_violating_rights, payer,
    institutional, biographical, constrained, national).

% Are powerful states that assert the right to intervene under the doctrine of conditional sovereignty. They define the thresholds for intervention and mobilize resources, often balancing humanitarian concerns with geopolitical interests.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervening_states, agenda_setter,
    institutional, biographical, mobile, global).

% Are states that acknowledge the norm but may choose not to participate in interventions due to national interests, capacity limitations, or differing interpretations of the doctrine. They contribute to the broader discourse but are not direct enforcers.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, non_intervening_states, observer,
    institutional, biographical, mobile, global).

% Advocate for the traditional, unconditional view of state sovereignty, viewing any external intervention as illegitimate interference. They are often marginalized in forums where conditional sovereignty is debated as a legitimate framework for action.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, absolute_sovereignty_proponents, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The claimed coordination function is to establish a global norm where states collectively agree to uphold human rights and prevent mass atrocities, thereby fostering a more stable and just international order. However, this function often serves as a cover for the selective application of power.
% TRANSFER_FUNCTION: Transfers a portion of state autonomy and the traditional guarantee of non-interference from states (especially weaker ones) to the international community, particularly powerful states, in exchange for a claimed global security and human rights protection.
% ABSENT_VOICES: Proponents of absolute sovereignty are often excluded from the core decision-making processes regarding intervention, as their fundamental premise directly contradicts the conditional sovereignty doctrine. They would argue that the doctrine is a dangerous precedent for great power overreach.
% DISAPPEARANCE_RATIONALE: If the conditional sovereignty doctrine vanished, states would face fewer external constraints on domestic human rights abuses, potentially leading to more widespread atrocities and a return to a purely Westphalian system where internal affairs are sacrosanct, regardless of their severity. The international community would lose a key (albeit contested) justification for intervention.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to mass atrocities within sovereign states (e.g., Rwanda, Srebrenica) under the strict interpretation of absolute sovereignty, leading to calls for a redefinition of sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: UN reports, human rights organizations (e.g., Amnesty International, Human Rights Watch), international legal scholars, and historical records from the post-Cold War era corroborate the problem of mass atrocities and the limitations of absolute sovereignty in addressing them. This corroboration comes from sources outside the direct beneficiaries of intervention.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.40) is moderate, reflecting the loss of state autonomy and the imposition of external will, but not necessarily direct financial extraction. Suppression (0.70) is high due to the potential for military intervention and diplomatic pressure. Theater ratio (0.10) is low, as interventions are serious, high-stakes actions, not mere performance. Resistance (0.80) is high, as states fiercely defend their sovereignty against external interference. Accessibility collapse (0.50) is moderate; states can still violate rights, but the risk of intervention limits their options.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable populations and human rights advocates, this doctrine is a necessary safeguard and a legitimate evolution of international law. From the perspective of states targeted for intervention, or proponents of absolute sovereignty, it is an illegitimate infringement on national self-determination and a tool for powerful states to pursue their interests.
 *
 * DIRECTIONALITY LOGIC:
 *   International human rights advocates and vulnerable populations are structural beneficiaries, as the doctrine aims to protect rights and provide a framework for action. Sovereign states violating rights are the primary targets/payers, as they lose autonomy and face intervention. Intervening states act as agenda-setters, defining and enforcing the norm, often benefiting from increased influence. Proponents of absolute sovereignty are excluded, as their core premise is incompatible with this doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's mandate (preventing mass atrocities) remains live, but its application is highly contested. The 'snare' classification highlights that while the founding problem is real, the mechanism designed to address it has become a vehicle for asymmetric extraction of state autonomy, often selectively applied. This prevents mislabeling it as a pure 'rope' or 'scaffold' based solely on its humanitarian rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_legitimacy_threshold,
    'What constitutes ''systematic human rights violations'' and ''legitimate external intervention'' in practice, and are these thresholds consistently applied?',
    'Analysis of UN Security Council resolutions, ICJ rulings, and state practice over time to identify consistent patterns or divergences in application. Expert consensus reports on specific cases.',
    'If thresholds are vague or inconsistently applied, the doctrine''s extractive nature (loss of autonomy) is amplified for targeted states, supporting the ''snare'' classification. Clear, consistent application would lend more credence to its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_legitimacy_threshold, conceptual, 'Ambiguity in defining triggers for intervention and consistency of application.').

omega_variable(
    selectivity_bias_in_intervention,
    'Is the doctrine of conditional sovereignty applied universally, or is intervention selectively pursued based on geopolitical interests and power dynamics?',
    'Empirical study comparing instances of systematic human rights violations with actual interventions, controlling for geopolitical factors, resource interests, and power asymmetries of potential intervening states.',
    'Evidence of significant selectivity would strongly reinforce the ''snare'' classification, indicating that the coordination story is largely cover for strategic extraction of influence or resources. Universal application would shift it closer to a ''tangled_rope'' or even ''rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selectivity_bias_in_intervention, empirical, 'Whether interventions are driven by humanitarian principles or geopolitical interests.').

omega_variable(
    effectiveness_of_intervention,
    'Does external intervention, under the conditional sovereignty doctrine, reliably improve human rights outcomes and state stability, or does it often exacerbate conflict and instability?',
    'Longitudinal case studies and comparative analysis of states where intervention occurred versus similar cases where it did not, assessing post-intervention human rights records, state capacity, and regional stability.',
    'If interventions consistently fail to improve outcomes or worsen them, the ''snare'' classification is strengthened, as the claimed benefit (human rights protection) is not realized, making the extraction of sovereignty less justifiable. Consistent positive outcomes would challenge the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_intervention, empirical, 'The actual impact of interventions on human rights and stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(west_tr_t1995, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(west_tr_t2000, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(west_tr_t2010, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(west_tr_t2015, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(west_be_t1995, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(west_be_t2000, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(west_be_t2010, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(west_be_t2015, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2020, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(west_su_t1995, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(west_su_t2000, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(west_su_t2010, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(west_su_t2015, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, un_security_council_veto_power).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('conditional_sovereignty') of the 'westphalian_sovereignty' kernel, alongside 'absolute_sovereignty' and 'graduated_sovereignty'. Each reading represents a distinct structural claim about state sovereignty and intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
