% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Total War Winnability Structural Contraction (Post-1945 Nuclear Reading)
 *   domain: international_relations/strategic_studies/commitment_systems
 *
 * SUMMARY:
 *   This reading asserts that nuclear weapons created a structural constraint
 *   on total war: the physical logic of nuclear exchange (mutual
 *   annihilation, radioactive contamination, collapse of critical
 *   infrastructure) makes the traditional definition of victory—the
 *   vanquished population under victor's control, resources extractable,
 *   territory administrable—impossible to achieve. Pre-1945, total war was
 *   costly but winnability remained: a state could defeat an adversary,
 *   occupy territory, and govern it. Post-1945 with nuclear-armed opponents,
 *   victory becomes unachievable because the victor's own territory and
 *   people suffer destruction that erases the gains. This is not a normative
 *   claim (that total war is illegal or immoral) but a structural claim (that
 *   the geometry of nuclear exchange removes victory from the reachable
 *   space). The reading does not assert that decision-makers have
 *   internalized this constraint or abandoned total war as a preference; it
 *   asserts that the constraint is THERE regardless of preference—a mountain
 *   rather than a rope or a socially-enforced rule.
 *
 * KEY AGENTS:
 *   - nuclear_physicists_and_strategists: Understand the structural constraint; communicate fallout, mutual destruction scenarios, unwinnable endgame.
 *   - state_decision_makers: Hold strategic preferences (some may prefer total war if it were winnability-reachable); the constraint removes it from the action set regardless.
 *   - international_legal_regime: Frames abandonment as normative (Article 2(4), humanitarian law); operates independently of structural constraint.
 *   - analytical_observer: Distinguishes the constraint (physics) from the regime (normativity); the reading sits here.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.05).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.0).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Total War Winnability Structural Contraction (Post-1945 Nuclear Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies/commitment_systems").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, '897a7674-412a-46be-8ca0-4e3c135a76e1').
narrative_ontology:cs_kernel_codification('897a7674-412a-46be-8ca0-4e3c135a76e1', distributed).
narrative_ontology:cs_authority_grounding('897a7674-412a-46be-8ca0-4e3c135a76e1', expertise).
narrative_ontology:cs_interpretation_layer_present('897a7674-412a-46be-8ca0-4e3c135a76e1').
narrative_ontology:cs_reading_relation('897a7674-412a-46be-8ca0-4e3c135a76e1', total_war_winnability_post1945__normative_reading_drop, influences).
narrative_ontology:cs_reading_relation('897a7674-412a-46be-8ca0-4e3c135a76e1', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('897a7674-412a-46be-8ca0-4e3c135a76e1', foundational, nuclear_mutual_destruction_unavoidable).
narrative_ontology:cs_axiom_status(nuclear_mutual_destruction_unavoidable, holdable).
narrative_ontology:cs_axiom_grounding('897a7674-412a-46be-8ca0-4e3c135a76e1', nuclear_mutual_destruction_unavoidable, empirically_contingent).
narrative_ontology:cs_axiom('897a7674-412a-46be-8ca0-4e3c135a76e1', foundational, victory_definition_requires_inhabitable_territory).
narrative_ontology:cs_axiom_status(victory_definition_requires_inhabitable_territory, holdable).
narrative_ontology:cs_axiom_grounding('897a7674-412a-46be-8ca0-4e3c135a76e1', victory_definition_requires_inhabitable_territory, deontological).
narrative_ontology:cs_reference_frame('897a7674-412a-46be-8ca0-4e3c135a76e1', winnability_pre1945).
narrative_ontology:cs_drift_state('897a7674-412a-46be-8ca0-4e3c135a76e1', post1945_nuclear_emergence, gap(stable, severe, true)).
narrative_ontology:cs_created_at('897a7674-412a-46be-8ca0-4e3c135a76e1', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals. The structural constraint applies to them equally: victory in total war is unachievable for either. They cannot exit the constraint (disarmament would be chosen, not enforced, and is not observable). They understand the constraint through strategic doctrine and game theory; it shapes their force posture and deterrence calculations. The constraint does not extract from them; it removes an option from their choice set.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, nuclear_armed_superpowers, observer,
    institutional, civilizational, trapped, global).

% Lack nuclear weapons but operate within the system the constraint creates. Their security strategies (alliance, conventional deterrence, regional power balance) are shaped by the knowledge that total war between nuclear powers is not winnability-reachable. The constraint does not extract from them directly; it structures the international environment they inhabit.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_aligned_states, observer,
    powerful, generational, constrained, global).

% Experience the constraint as a feature of the international order. Regional conflicts remain possible; the constraint operates at the total-war scale and does not prevent conventional wars. They have some agency in choosing alignment and conflict strategies, but the superpower-level constraint (winnability removed) is exogenous to their choice set.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, non_aligned_and_developing_states, observer,
    moderate, generational, mobile, global).

% Study and debate the constraint. This reading asserts the constraint is structural (physics); other readings assert it is normative or cultural. Analysts do not enforce or maintain the constraint; they interpret it. Their role is epistemic—to understand why total war has been abandoned.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_analysts_and_theorists, observer,
    analytical, generational, analytical, global).

% Not present in the actual world; mentioned as the would-be victims in a counterfactual total war scenario. They are excluded from the constraint story because the constraint is about what is POSSIBLE (and not possible), not about actual ongoing victimization. Mentioned for completeness in clarifying the constraint's referent.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, hypothetical_populations_in_counterfactual_exchange, excluded,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(total_war_winnability_post1945__structural_contraction_reading, hypothetical_populations_in_counterfactual_exchange).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading does not describe a coordination mechanism or shared problem-solving arrangement. It describes a structural limit on an option (total war winnability). There is no coordination problem being solved by nuclear weapons; the weapons are a consequence of military-industrial and scientific development, not a designed coordination solution.
% TRANSFER_FUNCTION: None. The constraint does not move resources, attention, or status from one party to another. It removes an option from all parties equally. If there were a transfer, the constraint would not be a mountain but an extraction mechanism (snare, rope, etc.). The zero-transfer state is what makes it a mountain candidate.
% ABSENT_VOICES: Total war planners and strategists who believed winnability remained possible post-1945. This reading excludes them implicitly by asserting the constraint is structural (physics, not preference). If they could be heard, they would argue winnability is still reachable via disarming strikes, limited escalation, or asymmetric conflict—that the constraint is weaker than this reading claims. Their absence from the constraint narrative reflects the reading's claim that structure, not consensus, is the operative fact.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared—i.e., if nuclear weapons suddenly ceased to function or did not exist—the world would need to REARRANGE in one sense (military strategies would shift, deterrence doctrines would collapse, total war would become winnability-reachable again). But the physical substrate (the planet, the laws of physics) would not change; what would change is the ACTION SET available to states. The verdict is 'world_unchanged' in the sense that the constraint is not an active arrangement requiring maintenance—it is a brute fact of physics that would need to be actively unraveled (through disarmament, technological breakthrough, or physical change) to alter. The constraint does not constitute an arrangement anyone decided to maintain; it exists regardless of anyone's preference.
% FOUNDING_PROBLEM: The threat of total war: before 1945, states could pursue total war (genocidal conquest, annihilative conflict) with some expectation of winning—of gaining territory, resources, and political control over the defeated population. Post-1945, nuclear weapons made this objective impossible: any victor in nuclear exchange would suffer unacceptable damage to its own population and territory, erasing the gains of victory.
% FOUNDING_PROBLEM_CORROBORATION: Strategic theorists (Brodie, Schelling, Jervis) and physicists who understood nuclear fallout and mutual assured destruction confirmed in the 1940s-1960s that total war winnability had been structurally removed. Military doctrines shifted from conquest (WWII model) to deterrence (Cold War model) reflecting this understanding. Declassified strategic assessments from both US and Soviet governments show decision-makers understood they could not win a total war. Independent corroboration comes from non-benefiting sources: neutral nations, non-aligned movements, and international legal bodies recognized the structural change even when it served no party's immediate interests to do so. The problem (winnability of total war) is dead as a structural possibility, though it persists in hypothetical scenarios and disarmament discourse.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is negligible (0.05) because the constraint does not extract from any agent—it is not maintained by one party collecting from another. It is a constraint ON all parties equally: no one can win a total war with nuclear weapons; no one extracts rents from this fact. Suppression is zero: the constraint is not enforced by coercion or active prevention; it is logically intrinsic. Accessibility collapse is very high (0.95): once the physics is understood, the alternative—winnable total war between nuclear powers—is not accessible; it is foreclosed by the math. Resistance is minimal (0.02): because the constraint does not demand anything of anyone, resistance is very low (apart from a few strategic planners who deny mutual vulnerability or game low-probability scenarios of disarming strikes—marginal resistance to the constraint itself, not widespread). Theater ratio is zero: there is no performative maintenance of this constraint; it sustains itself through material fact. The measurement series holds these values constant over the interval because nuclear physics does not change; the constraint's strength does not drift.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap on this reading: all seats (nuclear powers, non-nuclear states, strategists, civilians) experience the same structural constraint. The reading is not observer-relative; the physics is the same from every chair. Where disagreement enters is on the sibling readings: some seats (strategic culture believers, normative-regime builders) attribute the abandonment of total war to factors other than nuclear structure. But THIS reading does not feature a perspectival gap—it is not a competitive constraint story where different parties have different calculations.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading has NO beneficiaries or victims in the structural sense because it is a mountain. There is no party that benefits from the constraint (no one collects rents), and there are no victim populations being extracted from (the hypothetical victims are populations in a counterfactual WWIII that the constraint prevents, not present populations bearing costs from the constraint's existence). This is the clean case for a mountain: the constraint is a brute structural fact without an asymmetric beneficiary-payer relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this reading. The constraint has no mandate or founding problem—it is a structural fact of physics, not a social arrangement built to solve a problem. The question of whether a founding problem (the threat of total war) has been solved is not what this constraint addresses; the constraint is the REASON the problem cannot be solved the traditional way (by winning).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_structural_vs_normative,
    'Is total war winnability constrained by nuclear physics (structural impossibility) or by international normative consensus and legal regimes (social abandonment)? Are these readings of the same kernel or distinct constraints?',
    'Counterfactual analysis: if nuclear weapons had never been invented but Article 2(4) and humanitarian law still evolved identically, would total war still be normatively illegal? If YES, the readings are structurally distinct constraints (different ε referents). If NO, normative constraints rides on structural substrate.',
    'If structurally distinct: this reading''s mountain classification stands independent of sibling readings'' classifications. If substrate-coupled: the mountain''s authority depends on nuclear physics persisting; a technological displacement (fusion catalysis, antimatter, etc.) that re-enabled large-scale destructive war without fallout would undermine the physical constraint while leaving normative regimes in place.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_structural_vs_normative, conceptual, 'Whether the structural constraint is independent of or substrate-dependent on the normative reading.').

omega_variable(
    nuclear_proliferation_winnability_recovery,
    'Could proliferation of nuclear weapons to actors with different escalation calculus (non-state, authoritarian, ideologically-committed regime) create scenarios where total war becomes winnability-reachable despite mutual assured destruction doctrine between superpowers?',
    'Strategic analysis of asymmetric escalation pathways: if a regional nuclear-armed state attacks a non-nuclear neighbor with conventional forces, then neighbor acquires or uses radiological weapons, does winnability return? Historical case: India-Pakistan nuclear arsenals with asymmetric conventional superiority. Theoretical case: terrorist actor with radiological dispersal device.',
    'If winnability recovery is possible via asymmetry: the structural constraint is more fragile than claimed — winnability is contracted not eliminated, merely pushed from superpower-scale to regional/asymmetric domains. The mountain should be downgraded to snare or scaffolded rope (temporarily impossible; re-enablement risk). If asymmetric escalation still preserves the winner''s unwinnable condition: the constraint is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_proliferation_winnability_recovery, empirical, 'Whether proliferation and asymmetry can restore winnability despite the fundamental logic of mutual nuclear exchange.').

omega_variable(
    reading_boundary_sibling_coexistence,
    'Can a single policy framework simultaneously hold (a) that total war is physically impossible due to nuclear physics AND (b) that total war is normatively abandoned through legal/cultural evolution? Or do these readings constitute incommensurable epistemic positions?',
    'Policy document analysis: examine NATO doctrine, UN materials, strategic studies canon for statements asserting BOTH the physical constraint AND the normative regime as coordinate grounds. If both appear together as complementary (not contradictory) framings, coexistence holds. If policy sources consistently divorce them (nuclear deterrence theory stands on physics alone; humanitarian law stands on normativity alone; neither cites the other), they are genuinely coexisting readings of a contested kernel.',
    'Determines cs_structure.reading_relations assignment: if coexistence is empirically observable in policy and academic literature, declare coexists_with for all siblings. If readings are systematically invoked separately (the physics community invokes physical constraints; the legal community invokes normative constraints; they do not converge), the readings might forceclose or influence rather than coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_sibling_coexistence, empirical, 'Whether the structural and normative readings occupy a single coherent policy framework or remain epistemic alternatives.').

omega_variable(
    counterfactual_total_war_cost_under_nuclear,
    'What was the empirical cost structure of ''victory'' in WWII-scale total war? If nuclear weapons had existed in 1939, would the same military objectives (territorial conquest, regime change, resource extraction) still be achievable with unacceptable cost to the victor?',
    'Quantitative strategic analysis: model nuclear exchange scenarios under WWII objective sets (occupy continental Europe, secure raw materials in Asia). Compute victor''s population loss, infrastructure damage, long-term habitability. Compare to WWII actual costs. If nuclear scenario yields victor''s cost >> WWII cost, the constraint is structural. If nuclear cost <= WWII cost for same objectives, winnability persists and the constraint is narrower.',
    'If nuclear exchange cost is genuinely unacceptable even to a victor: the constraint is robust — winnability is removed. If costs are merely higher than WWII but still politically imposable (on a conquered population or a domestic audience willing to bear it), winnability returns and the constraint collapses to a mere escalation-cost mechanism (rope, not mountain).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_total_war_cost_under_nuclear, empirical, 'Whether nuclear weapons make total war''s costs structurally prohibitive or merely politically expensive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1980, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1980, observed).
narrative_ontology:measurement(tota_tr_t2000, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(tota_tr_t2000, observed).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2025, 0.0).
narrative_ontology:measurement_basis(tota_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1962, 0.04).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1980, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1980, 0.04).
narrative_ontology:measurement_basis(tota_be_t1980, observed).
narrative_ontology:measurement(tota_be_t2000, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement_basis(tota_be_t2000, observed).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2025, 0.05).
narrative_ontology:measurement_basis(tota_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__structural_contraction_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__structural_contraction_reading, 0.0).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading kernel family (total_war_winnability_post1945). Each reading has a distinct ε referent: (1) the structural reading (this file) evaluates the standing arrangement under the structural constraint (nuclear physics makes winnability impossible); (2) the normative reading evaluates the standing arrangement under legal/normative constraints (Article 2(4), humanitarian law); (3) the strategic-culture reading evaluates the standing arrangement under ideational constraints (elite discourse abandonment). The three readings are structurally distinct constraints because their ε values differ, their beneficiary/victim structures differ, and their persistence mechanisms differ. They are linked via network.affects_constraints to enable contamination analysis: if the structural constraint were relaxed (a disarmament or technological shift), what would happen to the other readings' authority?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
