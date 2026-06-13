% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Contraction (Nuclear Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Nuclear weapons eliminated total war from the feasible set of great-power
 *   strategy. Once both sides possessed arsenals capable of assured mutual
 *   destruction, winning a total war became logically impossible — escalation
 *   beyond a threshold guarantees mutual annihilation, which is not victory.
 *   This reading claims the constraint is a natural law (mountain): the
 *   reachability boundary is contracted by the laws of physics, not by
 *   agreement or enforcement. No actor benefits from the constraint; all bear
 *   the cost of living under existential threat. The constraint's victim set
 *   is universal (all humanity). This is one of three contested readings of
 *   the 'total_war_reachability_boundary' kernel. The contraction_reading
 *   argues the contraction is irreversible physics. The
 *   contingent_reachability_reading argues it is a reversible technological
 *   state (a piton that could flip if ABM or counterforce technologies
 *   advance). The dropping_reading argues total war remains reachable but is
 *   merely suppressed in probability by deterrence coordination. This story
 *   instantiates ONLY the contraction reading; the other readings are
 *   separate constraint stories linked by the kernel.
 *
 * KEY AGENTS:
 *   - Nuclear weapons states: institutional actors with arsenals; they set strategic doctrine but cannot escape the constraint's logic — escalation is suicidal for them too.
 *   - Non-nuclear states: moderate power, trapped by dependence on nuclear patrons' rationality; they have no hand on the lever but are bound by the outcome.
 *   - Humanity as species: the ultimate victim set; not an agent but a collective outcome at risk if the constraint fails.
 *   - Strategists and theorists: analytical observers measuring reachability, debating stability, contesting whether MAD is fragile or robust.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.12).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Contraction (Nuclear Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, 'ac9c8813-b987-4b71-9a94-722bb16769a5').
narrative_ontology:cs_kernel_codification('ac9c8813-b987-4b71-9a94-722bb16769a5', formalized).
narrative_ontology:cs_authority_grounding('ac9c8813-b987-4b71-9a94-722bb16769a5', expertise).
narrative_ontology:cs_interpretation_layer_present('ac9c8813-b987-4b71-9a94-722bb16769a5').
narrative_ontology:cs_reading_relation('ac9c8813-b987-4b71-9a94-722bb16769a5', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_reading_relation('ac9c8813-b987-4b71-9a94-722bb16769a5', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_axiom('ac9c8813-b987-4b71-9a94-722bb16769a5', foundational, thermonuclear_weapons_eliminate_winnability).
narrative_ontology:cs_axiom_status(thermonuclear_weapons_eliminate_winnability, holdable).
narrative_ontology:cs_axiom_grounding('ac9c8813-b987-4b71-9a94-722bb16769a5', thermonuclear_weapons_eliminate_winnability, empirically_contingent).
narrative_ontology:cs_axiom('ac9c8813-b987-4b71-9a94-722bb16769a5', foundational, reachability_contraction_is_irreversible).
narrative_ontology:cs_axiom_status(reachability_contraction_is_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('ac9c8813-b987-4b71-9a94-722bb16769a5', reachability_contraction_is_irreversible, deontological).
narrative_ontology:cs_reference_frame('ac9c8813-b987-4b71-9a94-722bb16769a5', physical_impossibility_of_total_war_under_mad).
narrative_ontology:cs_drift_state('ac9c8813-b987-4b71-9a94-722bb16769a5', contemporary_strategic_practice_2026, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ac9c8813-b987-4b71-9a94-722bb16769a5', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, all_human_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

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
 *   Extractiveness is negligible (0.05) because no actor or coalition captures the constraint's operation for gain — the constraint is symmetrical suffering (all parties face mutual extinction if escalation occurs). Suppression is low (0.12 at end) because the constraint is self-enforcing through physics, not through active coercion — once the logic is understood, rational actors naturally refrain from escalation. Early suppression (0.25 in 1945) was higher because the implication that total war was no longer winnable had not yet propagated through doctrine, military planning, and political consciousness; as the Eisenhower era progressed and MAD became accepted doctrine, the need for active enforcement (political discourse to suppress interest in nuclear war planning, military doctrine to codify 'war is unwinnable') declined. Theater ratio is similarly low and declining: the constraint's function is not performed; it enforces itself. The measurements trace the constraint's maturation: from novel and contested (1945) to widely accepted (2000–2026) as the decades of strategic practice vindicated the logic repeatedly (Cuban Missile Crisis chose conventional confrontation despite nuclear risk; Kargil War stopped before escalation; multiple near-misses resolved without crossing the threshold).
 *
 * PERSPECTIVAL GAP:
 *   All seats experience this constraint identically: none can escalate to total war without mutual annihilation. There is no perspectival gap because the constraint is physically symmetric. Nuclear weapons states might claim strategic advantage through force posture, but the advantage cannot extend to winning total war — the upper bound is sealed. Non-nuclear states suffer the constraint's burden (vulnerability to any nuclear power) without any asymmetric advantage. The analytical observer seat sees the constraint's logical structure most clearly but has no stake in its outcome. The symmetry of extinction risk is the constraint's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply meaningfully to a mountain. All actors are full targets with respect to the species-extinction risk (d ~1.0 on the destruction axis), but mountains do not produce 'directed' extraction — they forbid action universally. The constraint is not extractive in the directional sense (no actor benefits at the expense of another); it is prohibitive. The universal victim set reflects that burden, not an asymmetry of directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no sign of mandatrophy. Its founding function (eliminate total war from the feasible set of great-power strategy) remains its current function. The constraint continues to bind policy, force structure, and escalation avoidance across 81 years of strategic practice. There is no atrophy of function, no survival by inertia — the constraint is continuously vindicated by the behavior of nuclear-armed states who, despite multiple incentives and opportunities to escalate, consistently choose alternatives to nuclear war. This is a living mountain, not a degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mad_logical_stability,
    'Is Mutually Assured Destruction a logically stable equilibrium, or does it rest on brittle assumptions about rationality, command-and-control reliability, and absence of surprise attack vectors?',
    'Game-theoretic analysis of incomplete information (incomplete detection, incomplete rationality, incomplete commitment); historical near-miss incidents analyzed for counterfactual branching; technical studies of command degradation under attack; empirical analysis of whether leaders have consistently acted as the MAD logic predicts.',
    'If MAD is demonstrably fragile (e.g., surprise attack is possible, irrationality is systematic, command systems fail under stress), the constraint becomes a contingent rope, not a mountain — reachability is merely suppressed, not eliminated. If MAD is robust, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mad_logical_stability, empirical, 'Whether the constraint''s logical basis is ironclad or contingent on maintained rationality and technical assumptions.').

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is the contraction of total war a natural consequence of physics (thermonuclear weapons make certain escalation unwinnable by physical law), or is it a constructed constraint that depends on the existence, maintenance, and deployment posture of nuclear arsenals and could be undone by technological change?',
    'If reachability is technology-dependent — if emerging technologies (perfect ABM, counterforce precision, AI-enabled autonomous targeting) could restore first-strike credibility or shield-feasibility — then the constraint is not natural, only currently binding. If reachability remains physics-bound (no technology can eliminate mutual destruction at scale), the mountain stands.',
    'This omega decides the kernel contest between this reading (contraction as mountain) and the contingent_reachability_reading (contraction as piton subject to technological reversal). The axis of disagreement is precisely the naturalness question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, conceptual, 'Whether total war''s elimination from the feasible set is a natural law or a technological contingency.').

omega_variable(
    beneficiary_absence_verification,
    'Does the constraint truly have no beneficiary, or are there hidden beneficiaries — military-industrial actors, nuclear strategists, deterrence institutions whose budgets and authority depend on the constraint''s persistence?',
    'Institutional analysis: who gains budget, status, and power from the continuation of nuclear deterrence doctrine and force maintenance? If actors can be identified who benefit structurally from the constraint''s persistence and would actively defend it against change, then beneficiaries exist (even if masked by the physical inevitability narrative).',
    'If beneficiaries exist, the constraint is not a mountain (natural law) but a tangled_rope (coordination + extraction), with the extraction riding on the manufactured necessity of deterrence. If no actor genuinely benefits (they all suffer the cost of permanent existential threat), the mountain classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_absence_verification, conceptual, 'Whether the constraint''s persistence is defended only by physics or also by organized actors with institutional stakes.').

omega_variable(
    reading_contest_kernel,
    'This story instantiates ONE reading of the contested kernel ''total_war_reachability_boundary''. Is the contraction of total war an irreversible physical fact (this reading''s core claim), a contingent technological state subject to reversal (contingent_reachability_reading), or merely a probabilistic shift in a reachable option (dropping_reading)?',
    'The three readings differ structurally: this reading asserts the constraint is a mountain (total war eliminated from the feasible set by MAD physics). The contingent reading asserts it is a piton (currently atrophied but reversible with technology). The dropping reading asserts total war remains reachable but less likely (coordination as a rope-equilibrium). Empirical resolution depends on what actually happens in future nuclear crises, on whether technologies emerge that restore first-strike credibility, and on whether strategists and decision-makers continue to behave as the constraint''s logic predicts.',
    'If future crises reveal that escalation is reachable and rational actors sometimes cross thresholds, the dropping_reading''s claim is supported. If technology (ABM, counterforce, autonomous targeting) makes first strikes credible again, the contingent_reachability_reading is supported. If escalation remains universally suicidal regardless of technology, this reading (contraction as mountain) stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_kernel, empirical, 'Which reading of the total_war_reachability_boundary kernel is structurally correct — contraction as mountain, piton, or probabilistic rope?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contraction_reading, theater_ratio, 1962, 0.12).
narrative_ontology:measurement(tota_tr_t1982, total_war_reachability_boundary__contraction_reading, theater_ratio, 1982, 0.08).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contraction_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(tota_tr_t2015, total_war_reachability_boundary__contraction_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(tota_tr_t2026, total_war_reachability_boundary__contraction_reading, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1962, 0.05).
narrative_ontology:measurement(tota_be_t1982, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1982, 0.04).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2000, 0.04).
narrative_ontology:measurement(tota_be_t2015, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2015, 0.05).
narrative_ontology:measurement(tota_be_t2026, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2026, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1945, 0.25).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1962, 0.15).
narrative_ontology:measurement(tota_su_t1982, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1982, 0.12).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(tota_su_t2015, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2015, 0.11).
narrative_ontology:measurement(tota_su_t2026, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contraction_reading, 0.05).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'total_war_reachability_boundary'. The contraction_reading claims total war is eliminated from the feasible set by MAD physics (mountain). The contingent_reachability_reading claims it is a reversible technological state (piton). The dropping_reading claims it is reachable but deterred probabilistically (rope). All three readings share the same empirical domain (post-1945 strategic dynamics) but differ fundamentally on reachability and reversibility. The ε values differ: contraction reading has near-zero extractiveness (no beneficiary, universal victim); contingent reading has small extractiveness (institutional actors benefit from deterrence infrastructure); dropping reading has moderate extractiveness (deterrence coordination with asymmetric burden). The three stories are linked as a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
