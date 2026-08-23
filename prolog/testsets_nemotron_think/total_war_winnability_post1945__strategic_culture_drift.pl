% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Ideational Exclusion of Total War from Elite Strategic Discourse
 *   domain: international_relations/strategic_studies/commitment_system
 *
 * SUMMARY:
 *   After 1945, total war remained physically reachable — nuclear arsenals
 *   grew, conventional forces modernized, and no physical law prevented major
 *   powers from mobilizing for total war. Yet total war vanished from elite
 *   strategic discourse. This constraint story captures the *strategic
 *   culture drift* reading: an ideational shift, not a normative prohibition
 *   or physical impossibility, excluded total war from the Overton window of
 *   legitimate strategy. The constraint is the shared taboo among defense
 *   intellectuals, planners, and policymakers that treats total war as
 *   conceptually inadmissible. It operated as genuine coordination during the
 *   Cold War (solving the 'how to avoid nuclear exchange' problem) but
 *   atrophied into a piton — the coordination function degraded after 1991,
 *   yet the taboo persists through institutional inertia, career incentives,
 *   and theoretical path dependence. The beneficiaries are the defense
 *   intellectual establishment invested in limited war paradigms; the victims
 *   are strategic planners and political leaders who lose flexibility. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as piton
 *   (atrophied coordination) while metrics show moderate extraction and
 *   rising theater — the engine measures this divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.45).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.35).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.45).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Ideational Exclusion of Total War from Elite Strategic Discourse").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'c194d0d0-5ada-4730-b3f8-ab0e09f071fc').
narrative_ontology:cs_kernel_codification('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', distributed).
narrative_ontology:cs_authority_grounding('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', practice).
narrative_ontology:cs_interpretation_layer_present('c194d0d0-5ada-4730-b3f8-ab0e09f071fc').
narrative_ontology:cs_reading_relation('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', total_war_winnability_post1945__structural_contraction_reading, influences).
narrative_ontology:cs_axiom('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', foundational, total_war_excluded_by_ideational_drift_not_physical_impossibility).
narrative_ontology:cs_axiom_status(total_war_excluded_by_ideational_drift_not_physical_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', total_war_excluded_by_ideational_drift_not_physical_impossibility, empirically_contingent).
narrative_ontology:cs_axiom('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', secondary, limited_war_consensus_atrophied_post_cold_war).
narrative_ontology:cs_axiom_status(limited_war_consensus_atrophied_post_cold_war, holdable).
narrative_ontology:cs_axiom_grounding('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', limited_war_consensus_atrophied_post_cold_war, empirically_contingent).
narrative_ontology:cs_reference_frame('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', cold_war_limited_war_consensus).
narrative_ontology:cs_drift_state('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', post_cold_war_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c194d0d0-5ada-4730-b3f8-ab0e09f071fc', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war_frameworks).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_planners).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, political_decision_makers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, nuclear_deterrence_establishment).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, limited_war_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, nuclear_deterrence_stability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic and policy elites whose careers, funding, and intellectual capital are invested in limited war frameworks (counterinsurgency, hybrid warfare, gray zone). They control the journals, curricula, and advisory channels that define legitimate strategic discourse. The exclusion of total war validates their paradigm and protects their institutional position.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war_frameworks, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war_frameworks, agenda_setter).

% Military planners and staff officers who lose the conceptual tools and planning capacity for total war scenarios. They are trained in limited war paradigms and face career penalties for raising total war contingencies. Their exit is constrained by institutional doctrine and promotion pathways.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_planners, payer,
    organized, biographical, constrained, national).

% Heads of government and national security councils who face a narrowed option space when crises escalate beyond limited war thresholds. They rely on advisors steeped in the limited war consensus and lack independent strategic imagination for total war contingencies. Their exit is constrained by the advisory ecosystem they inherit.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, political_decision_makers, payer,
    powerful, biographical, constrained, national).

% The nuclear command, control, and doctrine community whose institutional rationale depends on total war being 'unthinkable' rather than planned for. The ideational exclusion reinforces deterrence stability narratives and protects budgetary and bureaucratic equities.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, nuclear_deterrence_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% International lawyers and NGO advocates who would argue that the exclusion of total war from discourse creates dangerous gaps in humanitarian protection planning. They are structurally excluded from the elite strategic culture where deterrence and warfighting doctrines are set.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, humanitarian_law_advocates, excluded,
    moderate, generational, mobile, global).

% Strategists who argue for maintaining escalation dominance and counterforce capabilities up to and including total war thresholds. They are marginalized within the current strategic culture as 'destabilizing' or 'anachronistic' despite nuclear modernization programs quietly expanding such capabilities.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, counterforce_escalation_advocates, excluded,
    moderate, biographical, constrained, national).

% External analyst of strategic culture dynamics who sees the full structure: the ideational shift, its beneficiaries, its costs to strategic flexibility, and the kernel contest with sibling readings. Neither collects nor pays within the constraint.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates elite consensus around the 'unthinkability' of total war, preventing paralyzing debate about nuclear catastrophe and enabling stable deterrence signaling during the Cold War and after. The shared taboo functions as a coordination mechanism that substitutes for explicit arms control in some domains.
% TRANSFER_FUNCTION: Transfers strategic flexibility (conceptual tools, planning capacity, optionality for high-end conflict) from military planners and political decision-makers to the stability of the limited war consensus. The gains accrue as institutional coherence for defense intellectuals and deterrence establishments; the costs are borne as narrowed strategic imagination.
% ABSENT_VOICES: Counterforce escalation advocates and humanitarian law advocates are both excluded — the former because they challenge the 'unthinkability' taboo from a warfighting perspective, the latter because they challenge it from a protection perspective. Both would object to the ideational closure but sit outside the elite strategic culture where discourse boundaries are policed.
% DISAPPEARANCE_RATIONALE: If the ideational exclusion vanished overnight, total war would re-enter doctrinal planning, wargaming, and political discourse. Nuclear modernization programs would require explicit total war justification. Crisis management would expand to include deliberate escalation ladders. The strategic culture would reorganize around acknowledged rather than suppressed catastrophe planning.
% FOUNDING_PROBLEM: The founding problem was managing the existential danger of US-Soviet nuclear confrontation: how to fight limited wars and conduct coercive diplomacy without triggering uncontrolled escalation to total nuclear war. The limited war framework (Kahn, Kissinger, Schelling) provided the conceptual architecture for this coordination.
% FOUNDING_PROBLEM_CORROBORATION: Cold War historians (Gaddis, Suri) corroborate the founding problem was live and the framework solved a real coordination challenge. Contemporary strategists (Lieber, Press, Kroenig) argue the problem has mutated — nuclear multipolarity and conventional-nuclear integration make the original limited war consensus inadequate. The defense intellectual establishment asserts the problem remains live in adapted form; this claim is self-interested.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).
:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the ongoing cost of lost strategic options — planners cannot conceptualize, war-game, or prepare for total war contingencies, creating blind spots in crisis management. Suppression (0.35) is moderate: the constraint operates through socialization and career incentives, not active coercion; dissenters are marginalized, not imprisoned. Theater ratio (0.65) is high: wargames, doctrines, and journals perform 'serious strategy' while systematically excluding the highest-stakes contingency. Accessibility collapse (0.55) is partial: total war thinking is recoverable (counterforce advocates maintain quiet capabilities) but requires breaking strong professional taboos. Resistance (0.20) is low: few inside the establishment challenge the taboo; resistance comes from excluded voices outside. The temporal series shows extraction and theater rising as the Cold War coordination function atrophied, while suppression declined — the constraint shifted from actively enforced consensus to self-sustaining cultural inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the constraint appears as successful coordination — the taboo prevented nuclear war and enables stable deterrence. From the payer seats, it appears as dangerous blindness — the inability to plan for the worst case creates precisely the instability deterrence seeks to avoid. The engine computes this divergence from the structural data; the authored claim (piton) captures the analyst's view that the coordination function has atrophied while the constraint persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense intellectuals and the deterrence establishment are structural beneficiaries (d near 0.0): they collect intellectual rents, institutional coherence, and budgetary protection from the taboo. Strategic planners and political decision-makers are targets (d near 0.8-0.9): they pay in lost options and narrowed imagination, with constrained exit (doctrine, promotion, advisory capture). Excluded voices (humanitarian advocates, counterforce proponents) are structurally outside — their exclusion IS the constraint's boundary maintenance. The analytical observer sees the full structure but bears no cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing US-Soviet nuclear escalation) is contested — the original Cold War coordination challenge has mutated into a multipolar, cross-domain escalation problem. The limited war consensus persists as a piton: its primary function degraded but the institutional structure (journals, curricula, career paths, doctrinal templates) maintains it theatrically. No single actor benefits enough to maintain it actively, yet no coalition forms to dismantle it because the cost of rebuilding total war planning capacity is prohibitive relative to perceived benefit — until a crisis reveals the gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the exclusion of total war from discourse best explained by ideational drift (this reading), normative prohibition (sibling), or physical impossibility (sibling)?',
    'Counterfactual historical analysis: if nuclear weapons had not been invented, would total war still have dropped from discourse? If Article 2(4) had not been adopted, would the taboo still exist? Comparative study of non-nuclear strategic cultures.',
    'If structural contraction reading is correct, this constraint is a mountain (physical law) not a piton. If normative reading is correct, it is a scaffold (transitional normative order). This reading''s piton classification depends on the ideational drift being the primary causal mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Primary causal mechanism for total war''s discursive exclusion — ideational vs normative vs structural.').

omega_variable(
    piton_vs_mountain_ambiguity,
    'Does the strategic culture taboo reflect a genuine physical constraint (nuclear weapons make total war unwinnable) that has been culturally internalized, or is it a purely social construction?',
    'Technical assessment of whether any plausible total war scenario remains ''winnable'' in a meaningful sense — comparative analysis of nuclear winter modeling, counterforce exchange outcomes, and conventional escalation ladders.',
    'If total war is physically unwinnable, the constraint is a mountain (natural law) and the piton classification is a false summit. If winnable but excluded, piton stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(piton_vs_mountain_ambiguity, empirical, 'Whether the piton classification mistakes a physical impossibility for institutional atrophy.').

omega_variable(
    suppression_mechanism_ideational,
    'Is the suppression of total war discourse structural (institutional gatekeeping, funding, promotion) or internalized (strategists genuinely believe total war is unthinkable)?',
    'Survey and interview data from defense intellectuals and planners: would they raise total war contingencies if institutional incentives changed? Post-exit trajectory of dissenters who left the establishment.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the taboo travels with the agent. If structural, exit options improve for those who leave the institutional ecosystem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ideational, empirical, 'Structural vs internalized suppression mechanism in the strategic culture taboo.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tota_tr_t15, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 15, 0.25).
narrative_ontology:measurement(tota_tr_t30, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 30, 0.4).
narrative_ontology:measurement(tota_tr_t45, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 45, 0.55).
narrative_ontology:measurement(tota_tr_t60, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 60, 0.62).
narrative_ontology:measurement(tota_tr_t80, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 80, 0.65).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tota_be_t15, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(tota_be_t30, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(tota_be_t45, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 45, 0.4).
narrative_ontology:measurement(tota_be_t60, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(tota_be_t80, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 80, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tota_su_t15, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(tota_su_t30, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(tota_su_t45, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 45, 0.35).
narrative_ontology:measurement(tota_su_t60, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(tota_su_t80, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 80, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__strategic_culture_drift, 0.08).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, nuclear_deterrence_stability_post1945).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, limited_war_doctrine_evolution).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'total_war_winnability_post1945'. The epsilon-invariance principle requires separate stories because each reading attributes the discursive exclusion to a different causal mechanism (ideational, normative, structural) with different extraction profiles. This reading (strategic_culture_drift) authors moderate extraction and high theater — the piton signature. The normative reading would author lower extraction (coordination via law). The structural reading would author near-zero extraction (mountain). They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__strategic_culture_drift, institutional, 0.1).
constraint_indexing:directionality_override(total_war_winnability_post1945__strategic_culture_drift, organized, 0.85).
constraint_indexing:directionality_override(total_war_winnability_post1945__strategic_culture_drift, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
