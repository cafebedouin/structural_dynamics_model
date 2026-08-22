% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Total War Removed from Strategic Possibility Space (Space Contraction Reading)
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint instantiates the space-contraction reading of the
 *   contested total_war_possibility_space kernel. The reading's core claim is
 *   that nuclear weapons have removed total war from the strategically
 *   thinkable—war planning space has contracted, not merely shifted
 *   incentives. This is structurally distinct from the deterrence_equilibrium
 *   reading (total war remains thinkable, merely deterred by mutual
 *   vulnerability) and the nuclear_taboo reading (total war remains
 *   thinkable, merely prohibited by constructed norm). This reading asserts
 *   that the very cognitive category of total war, as a strategic option, has
 *   been foreclosed by the material reality of mutual nuclear annihilation.
 *   The constraint is authored as a mountain (natural law: physics forecloses
 *   the possibility) with very low extractiveness and suppression—it emerges
 *   from the structure of mutual vulnerability, not from any actor's coercive
 *   choice. The beneficiary is not an agent but a proposition: the
 *   continuation of civilization. The measurement series shows near-zero
 *   extractiveness rising very slightly over 81 years as institutional
 *   codification of the constraint accumulates—the constraint becomes more
 *   explicit in doctrine and teaching, but the underlying foreclosure remains
 *   constant.
 *
 * KEY AGENTS:
 *   - Great power strategic planners (general staffs, defense ministries): operate under cognitive regime where total war is not in their planning repertoire
 *   - Nuclear-armed states: institutional actors whose strategic thought is bounded by the cognitive removal of total war from thinkable options
 *   - Strategic studies disciplines: organized knowledge community that inherited post-nuclear cognitive framework and reproduces it in theory, education, and policy analysis
 *   - Post-war civilizational continuity: the beneficiary proposition—not an agent, but what persists when total war is structurally foreclosed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.12).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.08).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Removed from Strategic Possibility Space (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '01cc8171-2a8d-45a8-ae8d-5845c2abfebe').
narrative_ontology:cs_kernel_codification('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', distributed).
narrative_ontology:cs_authority_grounding('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', distributed).
narrative_ontology:cs_reading_relation('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', foundational, total_war_cognitively_foreclosed_not_merely_costly).
narrative_ontology:cs_axiom_status(total_war_cognitively_foreclosed_not_merely_costly, holdable).
narrative_ontology:cs_axiom_grounding('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', total_war_cognitively_foreclosed_not_merely_costly, empirically_contingent).
narrative_ontology:cs_axiom('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', secondary, strategic_possibility_space_is_materially_contracted).
narrative_ontology:cs_axiom_status(strategic_possibility_space_is_materially_contracted, holdable).
narrative_ontology:cs_axiom_grounding('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', strategic_possibility_space_is_materially_contracted, empirically_contingent).
narrative_ontology:cs_reference_frame('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', pre_nuclear_total_war_availability).
narrative_ontology:cs_drift_state('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', contemporary_strategic_thought, gap(stable, severe, true)).
narrative_ontology:cs_created_at('01cc8171-2a8d-45a8-ae8d-5845c2abfebe', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, post_war_civilizational_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military general staffs, defense ministries, and strategic theory communities that once war-gamed total mobilization scenarios now operate in a cognitive framework where those scenarios are not merely costly but cognitively unavailable. They plan deterrence, containment, conventional conflict, proxy war—but not total war. The possibility has exited planning space.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_strategic_planners, observer,
    institutional, generational, analytical, global).

% States with nuclear arsenals operate under a constraint whose mechanism is not economic incentive or normative prohibition, but cognitive closure: total war against another nuclear power is not in their planning repertoire because it is structurally removed from what they can think as strategy. The constraint persists independently of policy preferences or cost calculations.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_armed_states, observer,
    institutional, civilizational, analytical, global).

% The abstract continuity of industrial civilization—continued existence of supply chains, research capacity, institutional memory, knowledge systems—benefits from the cognitive removal of total war scenarios. This is not an actor that collects rents; it is a vindicated proposition about what persists when certain strategic futures are structurally foreclosed.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, post_war_civilizational_continuity, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(total_war_possibility_space__space_contraction_reading, post_war_civilizational_continuity).

% The academic and professional field of strategic studies operates under a cognitive regime shift: the research agenda, war-gaming exercises, policy recommendations, and theoretical frameworks all presume nuclear deterrence, escalation control, and sub-nuclear competition. Total war planning disappeared from the discipline's menu not because it was forbidden but because it stopped being thinkable within the framework the discipline inherited.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_disciplines, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None—this is not a coordination constraint. The constraint removes a strategic possibility from the available thought-space.
% TRANSFER_FUNCTION: None. No agent transfers to another.
% ABSENT_VOICES: Strategists from the pre-nuclear era who conducted total war planning would dissent: they would argue that war is always thinkable as an option, merely constrained by cost. This reading's claim is that they could not hold that position within the post-nuclear framework—the framework has changed what can be thought, not merely what is chosen.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if total war re-entered the strategically thinkable for nuclear-armed powers—the entire apparatus of deterrence theory, strategic stability analysis, escalation ladder models, and military doctrine would require reconstruction. The world would not revert to pre-nuclear planning; it would face a cognitive regime collapse. General staffs would face a radically expanded possibility space; strategic theory would require new frameworks.
% FOUNDING_PROBLEM: The availability of total war as a strategic option—the capacity to mobilize entire societies for annihilation—became physically incompatible with the existence of nuclear weapons, which made mutual total annihilation instant and certain. The problem this constraint solves is: how can strategic thought proceed when the traditional endpoint of conflict escalation (total mobilization, decisive victory, enemy annihilation) leads to mutual destruction?
% FOUNDING_PROBLEM_CORROBORATION: Strategic theorists outside the benefiting parties (academic strategists, military historians, policy analysts studying escalation dynamics) attest that this founding problem is live: mutual nuclear vulnerability means total war remains physically reachable, yet strategists do not include it in their planning frameworks. The constraint persists because total war has been removed from what can be thought as rational strategy, not because states prefer lower-cost options.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is near-zero (0.12 at interval end) because the constraint does not transfer resources from any agent to another; it removes a strategic possibility from the available thought-space. Suppression is also minimal (0.08) because the constraint does not require active coercion—it emerges from the material reality of mutual nuclear annihilation. Theater ratio is negligible (0.02) because there is no performative pretense required; the constraint is what it appears to be. Accessibility collapse is very high (0.92) because alternatives to the constraint (total war as a thinkable strategy) have been structurally removed; military planners cannot meaningfully entertain total war as a strategic option in a nuclear-armed context. Resistance is minimal (0.04) because no strategic actor is actively resisting the constraint—it is accepted as a fact of the strategic environment. The measurement series shows extractiveness rising slightly from 0 in 1945 (immediately after nuclear weapons, when total war planning still existed in institutional memory) to 0.12 by 2026 (as the constraint becomes deeply embedded in doctrine and institutional practice), but the rise is slow because the underlying foreclosure is constant. The constraint's institutional codification increases, but its fundamental character remains unchanged.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap in this reading because the constraint is not experienced differently by different agents—it is a shared cognitive boundary that all strategic actors operate within. The space-contraction reading, unlike the deterrence_equilibrium reading, does not create divergent interests or require different institutional actors to play asymmetric roles. All strategic planners, from all nuclear-armed states, inhabit the same contracted possibility space. The constraint is not extractive from one seat and coordinating from another; it is a shared cognitive regime that all occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to this constraint in the traditional sense because there are no extractors and targets. The constraint is a natural law (in this reading's frame): it emerges from the structure of mutual nuclear vulnerability and applies equally to all strategic actors. Great powers, small powers with nuclear weapons, and non-nuclear states all operate within the same contracted possibility space—total war is not an option for any of them. The constraint benefits the continuation of civilization (a non-agent proposition), but that benefit does not flow from any actor's loss; it is the consequence of a foreclosed strategic option.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not present in this reading. The constraint's founding problem (how to conduct strategy when total mobilization leads to mutual destruction) remains live, and the constraint continues to serve its function of keeping total war out of the thinkable. The constraint has not outlived its mandate because the mandate is permanent: as long as nuclear weapons exist and mutual vulnerability persists, the strategic necessity to remove total war from the possibility space persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_cognitive_regime,
    'Is the removal of total war from strategic thought a natural consequence of nuclear physics (the constraint is a mountain: material reality forecloses certain strategies), or a constructed cognitive regime (the constraint is a socially maintained interpretation that could, in principle, be otherwise)?',
    'Comparative cognitive history: examine whether military planners in different strategic contexts (different time periods, different threat environments, different theoretical schools) maintain or breach the boundary of total war thinkability. If breach attempts fail universally despite incentive structures that would favor them, the constraint is closer to natural law; if breaches occur selectively or are thinkable-but-avoided, it is more constructed.',
    'If natural law, the constraint is a genuine mountain with negligible extraction—the physics of mutual annihilation structurally eliminates total war from rational strategy. If constructed, it is a maintained cognitive order (piton or rope) that could degrade or be deliberately inverted; the extraction of continuity is then contingent on institutions continuously enforcing the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_cognitive_regime, empirical, 'Whether the constraint is a material necessity or an institutionally maintained interpretation.').

omega_variable(
    kernel_contest_possibility,
    'Can the three readings of the total_war_possibility_space kernel coexist in a single framework, or does this reading''s core claim (total war is cognitively removed, not merely costly or prohibited) logically foreclose the deterrence_equilibrium_reading?',
    'Examine the structure of strategic thought in a state or theoretical school that endorses both readings: does it maintain that total war is simultaneously (a) strategically thinkable as an option that would be chosen if deterrence failed, AND (b) cognitively unavailable as a planning object? If both are affirmed, they coexist; if the second denies the first, the readings foreclose each other.',
    'If the readings foreclose each other, this reading''s classification changes—one reading''s adoption dissolves others—and network structure between sibling constraints becomes a causal entanglement rather than independent alternatives. If they coexist, the kernel is genuinely contested and all three constraints are live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_possibility, conceptual, 'Structural relationship between this reading and its siblings in the total_war_possibility_space kernel.').

omega_variable(
    institutional_atrophy_vs_dormancy,
    'Is the observed disappearance of total-war mobilization planning from military doctrine and general staff war games evidence of institutional cognitive atrophy (the capacity to plan total war has been lost, could not be rapidly recovered), or dormancy (the capacity is latent but suppressed, could be reactivated)?',
    'Test case: introduce a major power transition (e.g., a great-power coalition dissolution, a fundamental shift in strategic alignment) that removes nuclear deterrence stability. Do general staffs rapidly reconstitute total-war planning frameworks, or do they struggle to generate them, suggesting atrophy?',
    'Atrophy would confirm the constraint is a mountain: the cognitive possibility space has genuinely contracted and cannot easily be expanded. Dormancy would suggest the constraint is maintained by active institutional suppression (rope or snare), not cognitive foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_atrophy_vs_dormancy, empirical, 'Whether institutional capacity for total-war planning has atrophied or is merely suppressed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__space_contraction_reading, theater_ratio, 1962, 0.01).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.01).
narrative_ontology:measurement_basis(tota_tr_t1975, observed).
narrative_ontology:measurement(tota_tr_t1990, total_war_possibility_space__space_contraction_reading, theater_ratio, 1990, 0.02).
narrative_ontology:measurement_basis(tota_tr_t1990, observed).
narrative_ontology:measurement(tota_tr_t2010, total_war_possibility_space__space_contraction_reading, theater_ratio, 2010, 0.02).
narrative_ontology:measurement_basis(tota_tr_t2010, observed).
narrative_ontology:measurement(tota_tr_t2026, total_war_possibility_space__space_contraction_reading, theater_ratio, 2026, 0.02).
narrative_ontology:measurement_basis(tota_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1962, 0.08).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.1).
narrative_ontology:measurement_basis(tota_be_t1975, observed).
narrative_ontology:measurement(tota_be_t1990, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1990, 0.11).
narrative_ontology:measurement_basis(tota_be_t1990, observed).
narrative_ontology:measurement(tota_be_t2010, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement_basis(tota_be_t2010, observed).
narrative_ontology:measurement(tota_be_t2026, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2026, 0.12).
narrative_ontology:measurement_basis(tota_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.0).
narrative_ontology:measurement_basis(tota_su_t1945, observed).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1962, 0.04).
narrative_ontology:measurement_basis(tota_su_t1962, observed).
narrative_ontology:measurement(tota_su_t1975, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1975, 0.06).
narrative_ontology:measurement_basis(tota_su_t1975, observed).
narrative_ontology:measurement(tota_su_t1990, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1990, 0.07).
narrative_ontology:measurement_basis(tota_su_t1990, observed).
narrative_ontology:measurement(tota_su_t2010, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2010, 0.08).
narrative_ontology:measurement_basis(tota_su_t2010, observed).
narrative_ontology:measurement(tota_su_t2026, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2026, 0.08).
narrative_ontology:measurement_basis(tota_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.08).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the total_war_possibility_space constraint family. The family decomposes the contested kernel 'nuclear weapons changed total war' into three structurally distinct readings, each with different mechanisms, different ε values, and different classifications. space_contraction_reading (this story) claims total war exited the possibility space entirely (mountain). deterrence_equilibrium_reading claims total war remains strategically reachable but deterred (rope or tangled_rope). nuclear_taboo_reading claims total war remains thinkable but became normatively prohibited (snare or piton). All three constraints are linked because their framings contest a single kernel, but each story must be evaluated independently by its own structural evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
