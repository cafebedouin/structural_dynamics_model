% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Deterrence as Rational Dropout Constraint
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the RATIONAL_DROPOUT reading of the
 *   contested nuclear impossibility kernel. Under this reading, nuclear
 *   weapons impose a rational-choice limit on war: victory remains
 *   structurally reachable (no physical law prevents nuclear weapons' use),
 *   but the expected cost of any nuclear exchange rationally exceeds any
 *   conceivable benefit. States therefore 'drop out' of nuclear war as a
 *   rational policy option, even though the option remains technically
 *   available. This reading differs from the structural_contraction reading
 *   (which claims nuclear war is physically impossible) and the
 *   credibility_paradox reading (which claims the deterrent threat is
 *   inherently incredible). The rational_dropout reading accepts that nuclear
 *   war is both technically possible and that a credible threat to wage it
 *   could be constructed; the limit is purely economic: the math of
 *   cost-benefit makes it irrational to wage, not that it cannot be waged.
 *
 * KEY AGENTS:
 *   - Nuclear weapons states — institutional agenda setters, trapped by reciprocal vulnerability; claim the constraint emerges from physics and math, administer it through doctrine
 *   - Military strategists and planners — observers and secondary beneficiaries; arrive at the cost-benefit conclusion independently through scenario analysis
 *   - Policy makers and political leaders — powerful agenda setters; operate within the constraint's rational bounds, make decisions about conventional conflict on the premise nuclear escalation is inaccessible
 *   - Non-nuclear states — moderate-power beneficiaries; live in the security order the constraint shapes but are not trapped by it
 *   - Civilian populations in nuclear states — powerless beneficiaries trapped by the same constraint their governments invoke
 *   - Disarmament advocates — excluded observers; contest whether the constraint is natural or contingent on arsenal maintenance
 *   - Theoretical challenge communities — analytical observers; identify edge cases where rational-dropout becomes contested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.68).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.72).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Deterrence as Rational Dropout Constraint").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'dd696889-4439-4d47-b61b-f4d395653021').
narrative_ontology:cs_kernel_codification('dd696889-4439-4d47-b61b-f4d395653021', fixed_text).
narrative_ontology:cs_authority_grounding('dd696889-4439-4d47-b61b-f4d395653021', extraction).
narrative_ontology:cs_interpretation_layer_present('dd696889-4439-4d47-b61b-f4d395653021').
narrative_ontology:cs_reading_relation('dd696889-4439-4d47-b61b-f4d395653021', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd696889-4439-4d47-b61b-f4d395653021', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('dd696889-4439-4d47-b61b-f4d395653021', foundational, costs_exceed_benefits_in_any_exchange).
narrative_ontology:cs_axiom_status(costs_exceed_benefits_in_any_exchange, holdable).
narrative_ontology:cs_axiom_grounding('dd696889-4439-4d47-b61b-f4d395653021', costs_exceed_benefits_in_any_exchange, empirically_contingent).
narrative_ontology:cs_axiom('dd696889-4439-4d47-b61b-f4d395653021', foundational, rational_actors_will_not_choose_net_negative_outcomes).
narrative_ontology:cs_axiom_status(rational_actors_will_not_choose_net_negative_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('dd696889-4439-4d47-b61b-f4d395653021', rational_actors_will_not_choose_net_negative_outcomes, conventional).
narrative_ontology:cs_reference_frame('dd696889-4439-4d47-b61b-f4d395653021', rational_choice_framework_for_strategic_conflict).
narrative_ontology:cs_drift_state('dd696889-4439-4d47-b61b-f4d395653021', contemporary_era_with_asymmetric_threats, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dd696889-4439-4d47-b61b-f4d395653021', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, human_civilization).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, military_strategists_and_planners).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_in_nuclear_states).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_cost_benefit_thesis).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, mutual_vulnerability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and maintain strategic doctrine justifying deterrence through mutual vulnerability. They administrate the constraint through declaratory policy, force posture, and strategic signaling. Claim the constraint emerges from the physics and mathematics of nuclear exchange. Cannot unilaterally disarm without unilateral vulnerability; trapped by reciprocal vulnerability.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapons_states, agenda_setter,
    institutional, civilizational, trapped, global).

% Reason through war-scenario calculations and arrive at the conclusion that nuclear war's costs are rationally prohibitive despite technical victory remaining possible. They experience the constraint as a genuine logical limit, not an imposed rule. Their strategic doctrine rests on the constraint being real.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, military_strategists_and_planners, observer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, military_strategists_and_planners, beneficiary).

% Operate within the constraint as a structural fact of the security environment. They make decisions about conventional conflict, alliance formation, and crisis management on the premise that nuclear escalation is rationally inaccessible despite technically available. Some invoke the constraint to justify inaction; others treat it as a limit to work around through non-nuclear means.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, policy_makers_and_political_leaders, agenda_setter,
    powerful, biographical, constrained, national).

% Benefit from living in a world where nuclear war is treated as rationally impossible. They lack nuclear arsenals but exist in the security order the constraint shapes. They can choose alliances, proliferation paths, or non-aligned status; the constraint does not trap them.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states, beneficiary,
    moderate, generational, mobile, global).

% Depend on the constraint holding to avoid annihilation. They cannot exit, cannot opt out, cannot negotiate the constraint's terms. They are held by the same rational-dropout logic their own governments invoke.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_in_nuclear_states, beneficiary,
    powerless, biographical, trapped, global).

% Argue the constraint is not a natural law but a contingent choice to maintain arsenals; that rational actors could choose disarmament instead. They contest whether the constraint is truly structural or merely the preferred interpretation of powerful actors. Excluded from the decision-making that treats the constraint as settled.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, disarmament_advocates, excluded,
    organized, generational, mobile, global).

% Scrutinize the claim that costs exceed benefits under all scenarios. They identify edge cases (limited exchanges, escalation control, first-strike advantage scenarios) where the rational-dropout reading becomes contested or breaks down. They do not decide doctrine but challenge the claim's universality.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, theoretical_challenge_communities, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, diffuse).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding among nuclear-armed states that mutual destruction is rationally prohibitive, enabling crisis management and conventional conflict without thermonuclear escalation.
% TRANSFER_FUNCTION: Transfers decision-making authority from military commanders to civilian leaders constrained by rational-choice calculation. Victory as a policy goal becomes inaccessible; survival and maintenance of the deterrent becomes the only rational option.
% ABSENT_VOICES: Non-nuclear states unable to participate in strategic doctrine formulation. Disarmament communities arguing the constraint is contingent on arsenal maintenance, not physics. Future generations who cannot contest the terms under which they live in nuclear-armed world. Proliferating states whose rational calculations about nuclear war may differ from established doctrine.
% DISAPPEARANCE_RATIONALE: If the constraint vanished (actors ceased to treat nuclear war as rationally impossible), strategic postures would shift, crisis escalation control would collapse, and the likelihood of nuclear use would rise substantially. Yet some argue the constraint never truly disappeared but was always contingent on actors' choices — that disarmament would make the constraint moot, not restore it. The verdict divides between those who see it as structural (world rearranges without it) and those who see it as chosen (world could easily be different if actors decided differently).
% FOUNDING_PROBLEM: After 1945, nuclear weapons made total victory structurally impossible: any war between nuclear-armed states that escalates to general exchange results in mutual annihilation of the antagonists' societies and potentially civilization. This created a novel strategic problem: how do states manage competition and conflict when the only available escalation ladder leads to mutual destruction?
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and security analysts across competing powers (RAND Corporation, Soviet/Russian military academy publications, NATO strategic doctrine) attest the founding problem remains live — nuclear exchange would be catastrophic for all parties. Independent scientific analysis of nuclear effects confirms the casualty and environmental projections. Disarmament advocates contest that the problem is 'live' in the sense of genuinely insoluble — they argue it is sustained by political unwillingness to disarm, not by physics alone.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, contested).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__rational_dropout_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is authored as a mountain (emerges_naturally=true) because the reading treats the rational-choice limit as a structural feature of the security environment, not a human construction. Once nuclear weapons exist and both sides possess them, the mathematics of mutual vulnerability creates an objective constraint: any rational state will decline to wage nuclear war. The extractiveness score (0.68) reflects that the constraint operates with substantial force — it eliminates an entire class of policy options and transfers decision-making authority from military planners to civilian leaders bound by cost-benefit calculation. Suppression is high (0.72) because maintaining the constraint requires active enforcement: strategic signaling, force posture, doctrine statements, and crisis management that prevent escalation. Theater is substantial (0.58) because much strategic activity is performative — declaratory policy, military exercises, and public statements that demonstrate commitment to the doctrine without actually testing it. Accessibility collapse is high (0.74) because once the rational-dropout constraint is understood, alternative framings (war is winnable, limited nuclear exchange is feasible, first-strike advantage exists) become rationally inaccessible to decision-makers. Resistance is moderate (0.38) because while strategic theorists identify edge cases and disarmament advocates contest the constraint, the constraint itself experiences relatively little active resistance — states have largely internalized the rational-dropout logic. The measurement series show extractiveness and theater rising modestly over the interval (Cold War through contemporary period) as strategic thought elaborates the constraint's boundaries and doctrine becomes more sophisticated, then plateau as the constraint stabilizes in institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear weapons states and strategic planners experience the constraint as natural and settled. Disarmament advocates experience it as contingent and contestable. The engine's per-seat computation will show this divergence: an agenda_setter seat will compute mountain-hood from the authored natural-law metrics; an excluded seat holding a different reading (structural_contraction or credibility_paradox) would compute differently if measured on a shared rubric.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary group 'human civilization' derives from the constraint's function: it prevents nuclear war, which benefits all humanity. However, this is a non-actor entity (agent=false in the stakeholder), so it does not generate directionality in the classical sense. The real directionality flows through the stakeholders: nuclear weapons states are both beneficiaries (the constraint prevents their destruction) and payers (they must maintain costly deterrent postures, train personnel, conduct exercises). Military strategists are observers who also benefit (the constraint validates their rational-actor models). Policy makers are agenda setters who benefit (the constraint simplifies their decision-making by removing nuclear war from the active choice set). Non-nuclear states are beneficiaries (they avoid nuclear war without bearing deterrent costs). Civilians in nuclear states are trapped beneficiaries (they benefit from the constraint but cannot exit). Disarmament advocates are excluded from the agenda-setting process but contest the reading. The overall directionality for nuclear weapons states tends toward d=0.5 (symmetric: they benefit from the constraint but bear the suppression costs of maintaining it). Non-nuclear states tend toward d≈0.2 (beneficiaries with mobile exit). Civilians tend toward d≈0.7 (trapped beneficiaries — they benefit but cannot leave).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the classical sense: the founding problem (how to manage conflict when nuclear escalation is mutually catastrophic) remains live, and the arrangement (treating nuclear war as rationally inaccessible) continues to solve that problem. However, there is a latent mandatrophy risk: if strategic doctrine shifts (e.g., if theoretical frameworks develop that rationalize limited nuclear war, or if strategic asymmetries create scenarios where a state believes it can 'win'), the constraint could atrophy despite remaining institutionally maintained. The theater_ratio rising over time (0.42 to 0.58) suggests some theatrical maintenance is accumulating — the constraint is being defended partly through declaratory policy and strategic performance rather than purely through rational cost-benefit logic. This is not yet true mandatrophy but a warning sign: the constraint's authority is shifting from rational inevitability to performed maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_contingent_choice,
    'Is the rational-dropout constraint a genuine natural law (costs ALWAYS exceed benefits in any nuclear exchange) or a contingent choice to maintain arsenals in a particular strategic configuration?',
    'Test via counterfactual: if a nuclear-armed state unilaterally disarmed, would the constraint persist in the thinking of its adversaries? If the constraint survives (adversary still treats nuclear war as rationally prohibitive), it is more natural. If the constraint dissolves (adversary becomes willing to wage nuclear war), it is contingent on mutual vulnerability.',
    'If natural, the constraint is a mountain and will persist regardless of policy choice. If contingent, it is closer to a negotiated arrangement that could be unmade. The reading would need to shift toward rope or scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_choice, conceptual, 'Whether the rational-dropout constraint is structural or contingent on arsenal maintenance.').

omega_variable(
    edge_case_scenario_collapse,
    'Do strategically plausible edge cases (limited nuclear exchange, first-strike advantage from technical asymmetry, escalation control through tactical targeting) collapse under scrutiny, or do they represent genuine exceptions to the rational-dropout constraint?',
    'Rigorous game-theoretic analysis of scenarios where costs might not exceed benefits: limited strikes, counterforce-only strategies, escalation dominance through precision. Expert consensus from independent analysts outside beneficiary nuclei.',
    'If edge cases collapse, the constraint is robust and nearly universal. If edge cases are plausible, the constraint is conditional and vulnerable to technological or strategic shifts that create asymmetries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(edge_case_scenario_collapse, empirical, 'Whether plausible scenarios exist where nuclear war becomes rationally accessible.').

omega_variable(
    disarmament_counterfactual_reachability,
    'If all nuclear weapons were eliminated today, would the rational-dropout constraint remain as a constraint on re-arming, or would it disappear (making rapid re-arming rational)?',
    'Policy scenario analysis: model the strategic environment post-disarmament and ask whether the cost-benefit calculus for re-arming under mutual vulnerability assumptions would still yield ''do not wage nuclear war'' as the rational choice.',
    'If the constraint survives disarmament, it is more fundamental to nuclear physics/strategy than to the current arsenal configuration. If it depends on current arsenals, disarmament could eliminate it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disarmament_counterfactual_reachability, conceptual, 'Whether the rational-dropout constraint is independent of the current arsenal configuration.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.72) primarily structural (enforced through deterrent forces, strategic signaling, alliance commitments) or primarily internalized (decision-makers have internalized the rational-dropout logic so thoroughly that suppression costs are minimal)?',
    'Historical counterfactual: if all deterrent forces were removed but decision-makers'' beliefs about nuclear war''s costs remained intact, would states wage nuclear war? If not, suppression is highly internalized; if yes, suppression is structural.',
    'If internalized, the constraint is brittle — it could collapse if decision-makers'' beliefs shift. If structural, the constraint is robust to belief change because it is maintained by material incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of nuclear war is structural or internalized in decision-makers'' rational calculations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(nucl_tr_t0, observed).
narrative_ontology:measurement(nucl_tr_t10, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(nucl_tr_t10, observed).
narrative_ontology:measurement(nucl_tr_t20, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(nucl_tr_t20, observed).
narrative_ontology:measurement(nucl_tr_t40, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 40, 0.56).
narrative_ontology:measurement_basis(nucl_tr_t40, observed).
narrative_ontology:measurement(nucl_tr_t60, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement_basis(nucl_tr_t60, observed).
narrative_ontology:measurement(nucl_tr_t80, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 80, 0.58).
narrative_ontology:measurement_basis(nucl_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(nucl_be_t0, observed).
narrative_ontology:measurement(nucl_be_t10, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(nucl_be_t10, observed).
narrative_ontology:measurement(nucl_be_t20, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(nucl_be_t20, observed).
narrative_ontology:measurement(nucl_be_t40, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(nucl_be_t40, observed).
narrative_ontology:measurement(nucl_be_t60, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(nucl_be_t60, observed).
narrative_ontology:measurement(nucl_be_t80, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(nucl_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t0, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(nucl_su_t0, observed).
narrative_ontology:measurement(nucl_su_t10, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(nucl_su_t10, observed).
narrative_ontology:measurement(nucl_su_t20, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(nucl_su_t20, observed).
narrative_ontology:measurement(nucl_su_t40, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(nucl_su_t40, observed).
narrative_ontology:measurement(nucl_su_t60, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(nucl_su_t60, observed).
narrative_ontology:measurement(nucl_su_t80, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement_basis(nucl_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__rational_dropout_reading, 0.18).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% The nuclear_impossibility_kernel is decomposed into three reading-specific constraints, each instantiating a different interpretation of the standing arrangement of nuclear deterrence. The RATIONAL_DROPOUT reading (this constraint) treats nuclear war as rationally inaccessible despite being technically possible. The STRUCTURAL_CONTRACTION reading treats nuclear war as physically impossible. The CREDIBILITY_PARADOX reading treats the deterrent threat as inherently incredible. Each reading has different epsilon values, different beneficiary/victim structures, and different classification outcomes. They coexist as live strategic doctrines held by different communities. Linkage via affects_constraints models that a shift in one reading's acceptability would create pressure on the others' validity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, institutional, 0.52).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, powerless, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
