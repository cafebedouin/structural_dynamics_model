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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Boundary â Contraction Reading
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This is the contraction_reading of the total_war_reachability_boundary
 *   kernel. Under this reading, the accumulation of thermonuclear arsenals
 *   and the logic of mutual assured destruction (MAD) have removed winnable
 *   total war from the feasible set of strategic action entirely. The
 *   boundary is treated as a physical-strategic factâa mountainârather
 *   than as a maintained coordination equilibrium or a reversible
 *   institutional arrangement. No actor can win a total nuclear war;
 *   therefore no actor benefits from the boundary as a distributive
 *   arrangement. The universal victim set reflects the species-level
 *   extinction risk inherent in the arsenals that produce the boundary, not
 *   extraction by the boundary itself.
 *
 * KEY AGENTS:
 *   - global_human_population: Universal payer â bears the species-level extinction risk that underwrites the boundary; no exit from the planetary condition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.02).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Boundary â Contraction Reading").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '764fa4b4-9ed1-465f-9ab7-bfaa253a185c').
narrative_ontology:cs_kernel_codification('764fa4b4-9ed1-465f-9ab7-bfaa253a185c', implicit).
narrative_ontology:cs_authority_grounding('764fa4b4-9ed1-465f-9ab7-bfaa253a185c', distributed).
narrative_ontology:cs_reading_relation('764fa4b4-9ed1-465f-9ab7-bfaa253a185c', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('764fa4b4-9ed1-465f-9ab7-bfaa253a185c', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_axiom('764fa4b4-9ed1-465f-9ab7-bfaa253a185c', foundational, winnable_total_war_physically_precluded).
narrative_ontology:cs_axiom_status(winnable_total_war_physically_precluded, holdable).
narrative_ontology:cs_axiom_grounding('764fa4b4-9ed1-465f-9ab7-bfaa253a185c', winnable_total_war_physically_precluded, empirically_contingent).
narrative_ontology:cs_reference_frame('764fa4b4-9ed1-465f-9ab7-bfaa253a185c', mad_as_physical_strategic_boundary).
narrative_ontology:cs_drift_state('764fa4b4-9ed1-465f-9ab7-bfaa253a185c', contemporary_strategic_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('764fa4b4-9ed1-465f-9ab7-bfaa253a185c', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, global_human_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the symmetric, species-level risk of extinction from nuclear exchange that underwrites the boundary. There is no exit from this condition: the arsenals exist, the physics of thermonuclear destruction and climatic aftermath are understood, and no geographic or institutional mobility removes the risk. The population does not pay a transfer to any specific party, but carries the catastrophic downside that makes total war self-defeating.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, global_human_population, payer,
    powerless, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Physically precludes the possibility of winnable total war among nuclear-armed states, removing the option from the strategic menu by making the cost universally catastrophic regardless of operational coordination.
% TRANSFER_FUNCTION: No asymmetric transfer. The constraint is a physical limit, not a distributive arrangement. The species-level extinction risk is symmetrically distributed and arises from the mechanism that creates the boundary, not from extraction by the boundary itself.
% ABSENT_VOICES: Future generations who would bear the full cost of nuclear winter and genetic damage have no representation in strategic discourse; non-human life and ecosystems are entirely outside the conversation. Advocates of total nuclear disarmament, who would remove both the boundary and the extinction risk, are marginalized in state-centric strategic forums.
% DISAPPEARANCE_RATIONALE: If the boundary vanishedâif winnable total war re-entered the feasible setâthe entire architecture of nuclear deterrence, great-power competition, and international order would reorganize around the restored possibility of decisive strategic victory. States would resume mobilization for total war, alliance structures would shift, and the strategic doctrines of the last eight decades would collapse.
% FOUNDING_PROBLEM: The problem of total war among industrialized great powers, which produced catastrophic casualties in two world wars and threatened civilization, escalating to potentially species-ending proportions with the advent of nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: Strategic historians and nuclear theorists (e.g., Brodie, Schelling) attest that the thermonuclear revolution transformed war's feasibility. Contemporary physicists and climate scientists corroborate that full-scale nuclear exchange would produce nuclear winter, confirming the species-level stakes. No benefiting parties exist to contest this account; corroboration comes entirely from analytical seats outside any extraction structure.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.02, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is negligible (0.02) because the boundary itself does not move value between parties; it is a physical limit. Suppression is negligible (0.05) because the boundary persists without enforcementâit is sustained by the material logic of arsenals and physics, not by coercion. Theater ratio is minimal (0.05) because the boundary requires no performative maintenance; nuclear posturing concerns arsenals and doctrine, not the underlying impossibility of victory. Accessibility collapse is near-total (0.95): once the physics of MAD is understood, the alternative of winnable total war collapses as a live strategic option. Resistance is negligible (0.02): no serious strategic actor presently contests the proposition that total nuclear war is unwinnable. The flat measurement series reflects the stability of the boundary across the nuclear age.
 *
 * PERSPECTIVAL GAP:
 *   From the species-level seat, the boundary appears simultaneously as protection (total war is impossible) and as existential peril (the mechanism that creates the boundary can extinguish the species). The engine computes per-seat classification from structural data. The payer role reflects the extinction-risk cost carried by global_human_population, while the near-zero epsilon ensures that effective extraction chi remains negligible and the mountain classification holds from every seat. There is no beneficiary seat to create divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared because no actor captures value from the boundary. Global_human_population is the sole declared seat, carrying high directionality as the universal victim of extinction risk, with trapped exit and civilizational time horizon. Because epsilon is authored at 0.02, the structural derivation produces near-zero effective extraction despite the high directional weighting. The constraint is a mountain from every index.
 *
 * MANDATROPHY ANALYSIS:
 *   The contraction reading prevents mandatrophy mislabeling by asserting that the boundary is not a maintained institution with an atrophying mandate, but a physical-strategic fact. There is no mandate to atrophy because no authority enforces the boundary; it persists by the material logic of mutual arsenals and the physical impossibility of survival after full-scale exchange. The boundary cannot decay into a piton because its persistence is independent of human maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_by_technology,
    'Could strategic-technological innovation (e.g., perfect missile defense, quantum sensing, limited nuclear winter models) reverse the contraction and restore winnable total war to the feasible set?',
    'Empirical tracking of strategic-technology trajectories and war-game outcomes; if credible pathways to victory-with-acceptable-cost emerge, the boundary is contingent rather than a mountain.',
    'If reversible, the constraint reclassifies from mountain to piton or tangled_rope, and the universal victim set acquires active extraction dynamics as states compete to restore reachability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_by_technology, empirical, 'Whether the MAD boundary is technologically permanent.').

omega_variable(
    victimhood_without_extraction,
    'Does species-level extinction risk constitute victimhood within a zero-extraction mountain constraint, or is it a side-effect of the mechanism that produces the mountain?',
    'Analytical framing audit: if the risk is not a transfer imposed by the boundary but an externality of the arsenals, the victim declaration is metaphorical rather than structural, and the constraint may be a pure mountain without parties.',
    'If the risk is not structurally extractive, the constraint is a pure mountain without parties; if it is, the mountain classification may require FSM re-evaluation despite the absence of beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victimhood_without_extraction, conceptual, 'Whether extinction risk is structural extraction or mechanism externality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_reachability_boundary__contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tota_tr_t16, total_war_reachability_boundary__contraction_reading, theater_ratio, 16, 0.05).
narrative_ontology:measurement(tota_tr_t32, total_war_reachability_boundary__contraction_reading, theater_ratio, 32, 0.05).
narrative_ontology:measurement(tota_tr_t48, total_war_reachability_boundary__contraction_reading, theater_ratio, 48, 0.05).
narrative_ontology:measurement(tota_tr_t64, total_war_reachability_boundary__contraction_reading, theater_ratio, 64, 0.05).
narrative_ontology:measurement(tota_tr_t80, total_war_reachability_boundary__contraction_reading, theater_ratio, 80, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_reachability_boundary__contraction_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(tota_be_t16, total_war_reachability_boundary__contraction_reading, base_extractiveness, 16, 0.02).
narrative_ontology:measurement(tota_be_t32, total_war_reachability_boundary__contraction_reading, base_extractiveness, 32, 0.02).
narrative_ontology:measurement(tota_be_t48, total_war_reachability_boundary__contraction_reading, base_extractiveness, 48, 0.02).
narrative_ontology:measurement(tota_be_t64, total_war_reachability_boundary__contraction_reading, base_extractiveness, 64, 0.02).
narrative_ontology:measurement(tota_be_t80, total_war_reachability_boundary__contraction_reading, base_extractiveness, 80, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_reachability_boundary__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel decomposes into three structurally distinct constraints. The contraction_reading (this file) treats the boundary as a mountain with negligible extraction; the dropping_reading treats it as a rope (coordination equilibrium); and the contingent_reachability_reading treats it as a piton (reversible atrophy). Each carries a different epsilon, different stakeholder structures, and different type classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
