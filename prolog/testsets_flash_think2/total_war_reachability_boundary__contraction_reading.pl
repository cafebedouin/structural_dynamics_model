% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'contraction reading' of the total war
 *   reachability boundary, asserting that the advent of nuclear weapons,
 *   particularly the condition of Mutually Assured Destruction (MAD), has
 *   fundamentally and permanently removed 'winnable total war' from the realm
 *   of feasible strategic options. It is presented as a structural feature of
 *   the international system, akin to a natural law, rather than a policy
 *   choice. The metrics reflect this: very low extractiveness (as no one
 *   'benefits' in a rent-seeking way, but all bear the risk), high
 *   suppression (of the total war option), and low theater (as its reality is
 *   stark).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, 'fd9e0513-49a7-42ce-82c4-79def92c7265').
narrative_ontology:cs_kernel_codification('fd9e0513-49a7-42ce-82c4-79def92c7265', implicit).
narrative_ontology:cs_authority_grounding('fd9e0513-49a7-42ce-82c4-79def92c7265', self_enforcing).
narrative_ontology:cs_reading_relation('fd9e0513-49a7-42ce-82c4-79def92c7265', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('fd9e0513-49a7-42ce-82c4-79def92c7265', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_axiom('fd9e0513-49a7-42ce-82c4-79def92c7265', foundational, mutual_assured_destruction_is_absolute).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('fd9e0513-49a7-42ce-82c4-79def92c7265', mutual_assured_destruction_is_absolute, empirically_contingent).
narrative_ontology:cs_axiom('fd9e0513-49a7-42ce-82c4-79def92c7265', foundational, winnable_total_war_is_impossible).
narrative_ontology:cs_axiom_status(winnable_total_war_is_impossible, holdable).
narrative_ontology:cs_axiom_grounding('fd9e0513-49a7-42ce-82c4-79def92c7265', winnable_total_war_is_impossible, empirically_contingent).
narrative_ontology:cs_reference_frame('fd9e0513-49a7-42ce-82c4-79def92c7265', mad_strategic_reality).
narrative_ontology:cs_drift_state('fd9e0513-49a7-42ce-82c4-79def92c7265', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fd9e0513-49a7-42ce-82c4-79def92c7265', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, human_species).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the universal existential risk of nuclear war, even if the option of 'winnable' total war is foreclosed. This risk is an inherent cost of the nuclear age, from which there is no escape.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, human_species, payer,
    powerless, civilizational, trapped, universal).

% Maintain the nuclear arsenals that define the boundary of total war reachability. While they cannot 'win' a total war, their actions and doctrines shape the stability of the deterrence regime and the perceived impossibility of such a conflict.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the implications of nuclear weapons for international relations, articulating the concept of Mutually Assured Destruction (MAD) and the resulting contraction of strategic options. They observe the constraint's operation without directly benefiting or paying.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_theorists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents global nuclear war by establishing a shared understanding among nuclear-armed states that such a conflict is unwinnable and would result in mutual annihilation, thereby coordinating their strategic restraint.
% TRANSFER_FUNCTION: Transfers the option of 'winnable total war' from the feasible strategic set to the realm of impossibility, imposing a universal existential risk on the human species.
% ABSENT_VOICES: Future generations and non-human species, who would object to the perpetual existential risk but have no direct voice in the strategic doctrines that maintain this boundary.
% DISAPPEARANCE_RATIONALE: If the boundary of total war impossibility vanished overnight (e.g., through a technological breakthrough rendering MAD obsolete), the entire global security architecture would collapse. States would re-evaluate conventional military strategies, potentially leading to large-scale conflicts previously unthinkable, fundamentally reorganizing international relations.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons after their invention, necessitating a new strategic framework to prevent their use and manage the unprecedented destructive power.
% FOUNDING_PROBLEM_CORROBORATION: The problem of nuclear war prevention remains live, corroborated by ongoing nuclear proliferation concerns, international treaties (e.g., NPT), scientific consensus on nuclear winter, and the continued development of strategic doctrines by nuclear powers. This corroboration comes from international bodies, scientific communities, and non-nuclear states, not solely from the nuclear powers themselves.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that while nuclear weapons impose a universal existential risk, no single actor or group captures 'rents' from the impossibility of total war. The high suppression (0.95) signifies the near-complete elimination of total war as a rational strategic option. The low theater ratio (0.05) indicates that the constraint's effect is a genuine, non-performative strategic reality. The high accessibility collapse (0.98) means alternatives to this strategic reality are virtually non-existent. Resistance is low (0.05) because the reality of MAD is widely accepted, even if some theorists explore ways to escape it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers, this constraint is a grim but stable reality that prevents their own destruction. From the perspective of the human species, it is a perpetual Sword of Damocles. The engine's classification will reflect the Mountain-like nature of the constraint, while the FSM trigger (due to victims on a Mountain) will flag the unique cost structure for review.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'human_species' is declared as a victim because it universally bears the existential risk, even if no specific actor extracts from it. Nuclear powers, while 'agenda_setters' of the nuclear order, are also constrained by this boundary. Strategic theorists are analytical observers. No actor is a 'beneficiary' in the sense of collecting rents from this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''contraction_reading'' of the ''total_war_reachability_boundary'' kernel?',
    'Comparison with historical strategic doctrines and contemporary analyses that explicitly assert the permanent impossibility of winnable total war due to MAD.',
    'If the reading is mischaracterized, the classification of the underlying strategic reality would shift, potentially aligning with a ''rope'' (deterrence as coordination) or ''piton'' (contingent reachability) classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific interpretation of the total war boundary.').

omega_variable(
    mountain_with_victims_ambiguity,
    'Is the ''human_species'' truly a ''victim'' of a Mountain constraint, or is ''existential risk'' a different category than ''extraction'' in the sense of rent-seeking?',
    'Refinement of the framework''s definition of ''victim'' to distinguish between direct extraction/coercion and universal, unavoidable existential costs imposed by a natural-law-like constraint. The FSM trigger on this Mountain with victims flags this for review.',
    'If existential risk is reclassified as distinct from ''victimhood'' for Mountains, the FSM would not fire, and the constraint would be a pure Mountain. If it remains a victim, the FSM correctly flags a ''false summit'' where a natural law imposes costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_with_victims_ambiguity, conceptual, 'Ambiguity of victim status for a Mountain constraint imposing universal existential risk.').

omega_variable(
    reachability_vs_impossibility_ambiguity,
    'Is winnable total war truly an ''impossibility'' (as this reading claims), or merely ''less probable'' (dropping_reading) or ''contingently reversible'' (contingent_reachability_reading)?',
    'Ongoing technological developments (e.g., missile defense, space-based weapons) and shifts in strategic doctrine. If new technologies or doctrines credibly re-introduce the possibility of a ''first strike'' or ''limited nuclear war'' without assured retaliation, the ''impossibility'' claim would be challenged.',
    'If the ''impossibility'' claim is refuted, this constraint would likely reclassify from a Mountain to a Rope (deterrence as coordination) or a Piton (atrophied capability that could be revived), with significant implications for global security.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reachability_vs_impossibility_ambiguity, empirical, 'Core disagreement on the fixed vs. mutable nature of the total war boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1965, total_war_reachability_boundary__contraction_reading, theater_ratio, 1965, 0.07).
narrative_ontology:measurement(tota_tr_t1985, total_war_reachability_boundary__contraction_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(tota_tr_t2005, total_war_reachability_boundary__contraction_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__contraction_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.03).
narrative_ontology:measurement(tota_be_t1965, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1965, 0.04).
narrative_ontology:measurement(tota_be_t1985, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1985, 0.05).
narrative_ontology:measurement(tota_be_t2005, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2005, 0.05).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2025, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1945, 0.85).
narrative_ontology:measurement(tota_su_t1965, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1965, 0.92).
narrative_ontology:measurement(tota_su_t1985, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1985, 0.95).
narrative_ontology:measurement(tota_su_t2005, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2005, 0.95).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2025, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
