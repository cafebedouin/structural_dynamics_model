% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total War Exits Strategic Possibility Space (Space Contraction Reading)
 *   domain: international_relations_theory/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint represents the 'space contraction' reading of the impact
 *   of nuclear weapons on international relations: that they fundamentally
 *   altered the strategic possibility space, making total war between major
 *   powers literally unthinkable rather than merely undesirable or deterred.
 *   This reading posits a structural, almost natural-law-like, limit on
 *   strategic action. The constraint is claimed as a Mountain due to its
 *   assertion of an irreducible limit on strategic thought, with very low
 *   extraction, suppression, and theater, and high accessibility collapse.
 *   The presence of beneficiaries (global_stability, major_powers) on a
 *   Mountain triggers False Summit Mountain (FSM) detection, requiring omegas
 *   to address the ambiguity between a genuine natural law and a constructed
 *   constraint benefiting identifiable agents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.05).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.1).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Exits Strategic Possibility Space (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations_theory/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '9004ce53-a23b-4142-8c18-2eb71920318a').
narrative_ontology:cs_kernel_codification('9004ce53-a23b-4142-8c18-2eb71920318a', implicit).
narrative_ontology:cs_authority_grounding('9004ce53-a23b-4142-8c18-2eb71920318a', expertise).
narrative_ontology:cs_interpretation_layer_present('9004ce53-a23b-4142-8c18-2eb71920318a').
narrative_ontology:cs_reading_relation('9004ce53-a23b-4142-8c18-2eb71920318a', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('9004ce53-a23b-4142-8c18-2eb71920318a', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('9004ce53-a23b-4142-8c18-2eb71920318a', foundational, total_war_is_strategically_impossible).
narrative_ontology:cs_axiom_status(total_war_is_strategically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('9004ce53-a23b-4142-8c18-2eb71920318a', total_war_is_strategically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('9004ce53-a23b-4142-8c18-2eb71920318a', secondary, nuclear_weapons_fundamentally_altered_strategic_logic).
narrative_ontology:cs_axiom_status(nuclear_weapons_fundamentally_altered_strategic_logic, holdable).
narrative_ontology:cs_axiom_grounding('9004ce53-a23b-4142-8c18-2eb71920318a', nuclear_weapons_fundamentally_altered_strategic_logic, empirically_contingent).
narrative_ontology:cs_reference_frame('9004ce53-a23b-4142-8c18-2eb71920318a', pre_nuclear_total_war_doctrine).
narrative_ontology:cs_drift_state('9004ce53-a23b-4142-8c18-2eb71920318a', contemporary_strategic_thought, gap(stable, severe, true)).
narrative_ontology:cs_created_at('9004ce53-a23b-4142-8c18-2eb71920318a', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, global_stability).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, major_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, military_planners).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, nuclear_revolution_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, long_peace_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shapes the discourse and analytical frameworks for international security. This community largely accepts the premise that total war between major powers is no longer a viable strategic option, shifting research to sub-nuclear conflict and deterrence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_community, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from the reduced existential threat, allowing them to focus on conventional and limited warfare scenarios. They adapt doctrines and force structures to a world where total war is not a planning contingency, implicitly accepting the constraint.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, military_planners, beneficiary,
    institutional, biographical, constrained, national).

% Are the primary beneficiaries of total war being removed from the strategic possibility space, as it eliminates the existential threat of nuclear annihilation. Their strategic choices are fundamentally altered by this perceived impossibility.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, major_powers, beneficiary,
    institutional, civilizational, trapped, global).

% An abstract good that benefits from the absence of total war. Its persistence is directly tied to the constraint's effect on strategic thought.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, global_stability, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(total_war_possibility_space__space_contraction_reading, global_stability).

% Those who might still advocate for total war as a strategic option are largely marginalized or considered irrational within mainstream strategic thought, their arguments foreclosed by the perceived impossibility.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, total_war_advocates, excluded,
    powerless, immediate, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding among major powers and strategic thinkers that total war is no longer a viable or thinkable strategic option, thereby implicitly coordinating their strategic planning away from such scenarios.
% TRANSFER_FUNCTION: Transfers the concept of total war from the realm of strategic possibility to that of historical anomaly or theoretical impossibility, effectively removing it from the active strategic toolkit of states.
% ABSENT_VOICES: Those who believe total war remains a strategically viable, albeit costly, option, or those who attribute its absence to other factors (like a constructed taboo or a fragile deterrence equilibrium) rather than a fundamental contraction of possibility space. Their perspectives are often dismissed as unrealistic or dangerous within the dominant strategic discourse.
% DISAPPEARANCE_RATIONALE: If total war were to re-enter the realm of strategic possibility, the entire global security architecture, military doctrines, and international relations would fundamentally rearrange. The 'long peace' would end, and the existential threat would return to the forefront of statecraft.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons, which made traditional concepts of total war suicidal for all parties involved.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of nuclear arsenals and the absence of great power total war since 1945 corroborate the problem's live status. The shift in military planning away from total war scenarios and the focus of strategic studies on limited conflict further support this, attested by military historians and international security analysts outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that this is a structural limit on action, not a mechanism for rent collection. Low suppression (0.1) indicates it's not actively enforced but rather a new reality. Low theater (0.05) means it's not performative. High accessibility collapse (0.9) signifies that the alternative (thinking about total war as a viable option) has largely vanished from strategic discourse. Low resistance (0.05) indicates broad acceptance of this new strategic reality among relevant actors. The measurements show a stable, low profile across the nuclear age, consistent with a fundamental, unchanging shift in possibility.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a fundamental, almost natural, limit. Other readings (deterrence equilibrium, nuclear taboo) would view it differently, seeing it as a product of active deterrence or normative construction, respectively. The engine will compute these divergences from the structural data and the declared kernel relations.
 *
 * DIRECTIONALITY LOGIC:
 *   Major powers and global stability are beneficiaries, as the constraint removes an existential threat. The strategic studies community and military planners are also beneficiaries, as their work is reoriented away from suicidal scenarios. There are no direct 'victims' in the traditional sense, as the constraint removes an option that would be mutually destructive. 'Total war advocates' are excluded, as their position is rendered untenable by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_impossibility_vs_high_cost,
    'Is total war truly a categorical impossibility in strategic thought, or merely an option with prohibitively high costs that is therefore avoided?',
    'Analysis of strategic planning documents and military exercises for any residual ''total war'' contingencies, or a major power initiating a conflict that escalates beyond sub-nuclear thresholds.',
    'If merely high-cost, the constraint''s ''mountain'' classification would be challenged, potentially reclassifying it as a ''rope'' (deterrence) or ''tangled_rope'' (if deterrence involves extraction). If truly impossible, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_impossibility_vs_high_cost, conceptual, 'Distinguishing between strategic impossibility and extreme cost.').

omega_variable(
    material_vs_normative_constraint,
    'Is the contraction of possibility space primarily a material consequence of nuclear weapons, or is it mediated and sustained by a constructed normative taboo?',
    'Comparative analysis of strategic behavior in states with and without nuclear weapons, or historical analysis of how the ''unthinkability'' evolved alongside normative shifts.',
    'If primarily normative, the ''nuclear_taboo_reading'' gains strength, and this ''space_contraction_reading'' might be seen as a consequence or an incomplete explanation. If purely material, it reinforces the structural nature of this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_vs_normative_constraint, empirical, 'The underlying mechanism of the possibility space contraction: material or normative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1965, total_war_possibility_space__space_contraction_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(tota_tr_t1985, total_war_possibility_space__space_contraction_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(tota_tr_t2005, total_war_possibility_space__space_contraction_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(tota_be_t1965, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1965, 0.05).
narrative_ontology:measurement(tota_be_t1985, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1985, 0.05).
narrative_ontology:measurement(tota_be_t2005, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2005, 0.05).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(tota_su_t1965, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1965, 0.1).
narrative_ontology:measurement(tota_su_t1985, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1985, 0.1).
narrative_ontology:measurement(tota_su_t2005, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2005, 0.1).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2025, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
