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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Total War Possibility Space Contraction (Space Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'space contraction' reading of the impact
 *   of nuclear weapons on international relations. It posits that nuclear
 *   weapons fundamentally altered the strategic possibility space, making
 *   total war between great powers not merely undesirable or costly, but
 *   strategically unthinkable and practically impossible. This reading
 *   emphasizes the institutional atrophy of total-war planning apparatuses
 *   and a shift in strategic studies away from such scenarios. It is one
 *   reading of the 'total_war_possibility_space' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.15).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Possibility Space Contraction (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '0f2c6b74-afc7-4c85-b102-8a587f8bb1bb').
narrative_ontology:cs_kernel_codification('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', implicit).
narrative_ontology:cs_authority_grounding('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', self_enforcing).
narrative_ontology:cs_reading_relation('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', total_war_possibility_space__deterrence_equilibrium_reading, influences).
narrative_ontology:cs_reading_relation('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', foundational, total_war_is_strategically_impossible).
narrative_ontology:cs_axiom_status(total_war_is_strategically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', total_war_is_strategically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', secondary, great_power_conflict_is_sub_nuclear_by_necessity).
narrative_ontology:cs_axiom_status(great_power_conflict_is_sub_nuclear_by_necessity, holdable).
narrative_ontology:cs_axiom_grounding('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', great_power_conflict_is_sub_nuclear_by_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', post_nuclear_strategic_reality).
narrative_ontology:cs_drift_state('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0f2c6b74-afc7-4c85-b102-8a587f8bb1bb', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, great_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, global_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, strategic_planners).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, military_industrial_complex).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the removal of total war as a viable strategic option, avoiding existential destruction. Their strategic planning is fundamentally reoriented away from great-power conventional conflict. They are identity-locked into this new strategic reality by the very nature of their nuclear arsenals.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_powers, beneficiary,
    institutional, generational, identity_locked, global).

% Benefits from the existential threat of total war being removed from the realm of the thinkable, ensuring continued human civilization. They are trapped in this reality, unable to opt out of the consequences of nuclear weapons but also benefiting from the resulting strategic stability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, global_population, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear the cost of having to fundamentally rethink and abandon traditional total war doctrines and planning. Their professional identity and career paths are constrained by the new strategic landscape, forcing a shift to sub-nuclear conflict scenarios. They cannot 'exit' the reality of nuclear weapons.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_planners, payer,
    organized, biographical, constrained, national).

% Faces a contraction in the scope of conventional arms development and procurement for great-power conflict, as total war becomes unthinkable. While still profiting from other forms of conflict, the highest-stakes, highest-budget scenarios are removed. Their business model is constrained by the new strategic reality.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Analyze and debate the implications of nuclear weapons on the possibility of total war. Their work involves understanding the structural changes to the international system and how these weapons have reshaped strategic thought. They are not directly affected by the constraint but observe its effects.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, international_relations_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a new, implicit coordination among nuclear powers to avoid total war, not through explicit agreement but through the shared, unthinkable nature of the outcome. This reorients strategic planning towards limited conflicts.
% TRANSFER_FUNCTION: Transfers the possibility of total war from the realm of strategic options to the realm of the impossible, effectively 'costing' states the option of existential conflict in exchange for continued existence.
% ABSENT_VOICES: Historical military strategists and theorists who operated in a pre-nuclear world, for whom total war was a thinkable, if costly, option. Their doctrines and assumptions are now rendered obsolete by this new reality.
% DISAPPEARANCE_RATIONALE: If nuclear weapons (and thus the impossibility of total war) vanished overnight, the entire global strategic landscape would rearrange. Great powers would immediately re-evaluate conventional military capabilities, mobilization doctrines, and alliances, potentially leading to a return of great-power conventional conflict as a thinkable option.
% FOUNDING_PROBLEM: The inherent human capacity for self-destruction through large-scale conventional warfare, which threatened to escalate to global catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of nuclear arsenals and the absence of great-power total war planning corroborate that the problem of preventing existential conflict remains live, and that nuclear weapons are seen as the solution by many strategic thinkers and policymakers, even outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is claimed as a Mountain because, from this reading's perspective, the impossibility of total war is an irreducible structural feature of the nuclear age, akin to a natural law. Extractiveness is low (0.15) because the 'cost' is the loss of a strategic option that would lead to self-destruction, a net benefit. Suppression is very high (0.95) because the constraint is enforced by the existential threat of nuclear retaliation, making any deviation from this 'unthinkable' status immediately catastrophic. Accessibility collapse is near total (0.98) as the option of total war is simply removed. Resistance is minimal (0.02) because no rational actor actively resists the impossibility of self-destruction. Theater ratio is low (0.05) as there is little performative maintenance; the constraint's effect is direct and structural.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a pre-nuclear strategist, the idea of total war being 'unthinkable' would be alien, perhaps even a form of strategic surrender. From the perspective of this reading, that pre-nuclear mindset is simply obsolete, a relic of a different possibility space.
 *
 * DIRECTIONALITY LOGIC:
 *   Great powers and the global population are beneficiaries, as they avoid existential catastrophe. Strategic planners and the military-industrial complex are 'payers' in the sense that their traditional roles and business models are constrained by the removal of total war as a viable option, forcing adaptation. Their 'payment' is the reorientation of their purpose and the atrophy of certain capabilities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' is the ongoing prevention of existential catastrophe, which remains a live problem. The constraint's function is to maintain the 'unthinkable' status of total war, a function that persists as long as nuclear weapons exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a true ''space contraction'' where total war is unthinkable, or is it merely deterred (deterrence_equilibrium_reading) or normatively prohibited (nuclear_taboo_reading)?',
    'Empirical analysis of great-power military doctrine, strategic planning documents, and war-gaming exercises over time. Evidence of complete atrophy of total-war planning would support this reading; continued planning (even if for deterrence) would support alternatives.',
    'If this is a true space contraction, the constraint is a Mountain. If it''s deterrence, it''s a Tangled Rope (coordination with extraction of risk). If it''s a taboo, it''s a Rope (normative coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing between different readings of nuclear weapons'' impact on total war.').

omega_variable(
    institutional_atrophy_evidence,
    'To what extent has the institutional capacity for planning and executing total war truly atrophied, rather than merely being re-prioritized or hidden?',
    'Declassified historical archives, interviews with former strategic planners, and comparative analysis of military budgets and training exercises across nuclear and non-nuclear eras.',
    'Strong evidence of atrophy supports the ''space contraction'' reading and its Mountain classification. Evidence of hidden or re-prioritized capacity would weaken this reading, suggesting total war remains a ''thinkable'' but ''undesirable'' option, pushing towards a deterrence or taboo reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_atrophy_evidence, empirical, 'Empirical evidence for the atrophy of total war planning capabilities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__space_contraction_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__space_contraction_reading, theater_ratio, 1980, 0.06).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__space_contraction_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__space_contraction_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1960, 0.93).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1980, 0.95).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel. This 'space_contraction_reading' posits total war is unthinkable, influencing (but not foreclosing) the 'deterrence_equilibrium_reading' (total war is deterred) and 'nuclear_taboo_reading' (total war is normatively prohibited).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
