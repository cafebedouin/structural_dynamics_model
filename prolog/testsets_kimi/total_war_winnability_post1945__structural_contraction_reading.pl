% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Nuclear Weapons Physical Impossibility of Total War (Structural Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates the structural contraction reading of the
 *   total_war_winnability_post1945 kernel. It holds that nuclear weapons did
 *   not merely make total war undesirable or illegitimate; they rendered it
 *   structurally impossible by assuring that any total war between
 *   nuclear-armed powers would produce mutual annihilation, eliminating
 *   victory conditions. The constraint is a physical boundary on the action
 *   space of great powers, akin to a natural law within the strategic
 *   environment. It persists without enforcement, without beneficiaries, and
 *   without active suppression of alternatives â the alternatives simply do
 *   not exist as coherent strategies. The kernel is contested: the normative
 *   reading treats the same absence as legal prohibition, and the
 *   strategic-culture reading treats it as ideational unthinkability. Per the
 *   Îµ-invariance principle, these are distinct constraints and are authored
 *   as separate files.
 *
 * KEY AGENTS:
 *   - structural_realist_strategists: Analytical observer â argues nuclear arsenals physically foreclose total war regardless of norms or institutions
 *   - international_lawyers_normative: Analytical observer â holds the sibling normative reading that total war remains physically possible but was outlawed
 *   - constructivist_scholars: Analytical observer â holds the sibling strategic-culture reading that total war became ideationally unthinkable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.02).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.05).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.96).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Nuclear Weapons Physical Impossibility of Total War (Structural Contraction Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, '94df7331-6efa-45a3-8681-81e3caf072ab').
narrative_ontology:cs_kernel_codification('94df7331-6efa-45a3-8681-81e3caf072ab', distributed).
narrative_ontology:cs_authority_grounding('94df7331-6efa-45a3-8681-81e3caf072ab', diffuse_epistemic).
narrative_ontology:cs_reading_relation('94df7331-6efa-45a3-8681-81e3caf072ab', total_war_winnability_post1945__normative_reading_drop, forecloses).
narrative_ontology:cs_reading_relation('94df7331-6efa-45a3-8681-81e3caf072ab', total_war_winnability_post1945__strategic_culture_drift, forecloses).
narrative_ontology:cs_axiom('94df7331-6efa-45a3-8681-81e3caf072ab', foundational, total_war_physically_unreachable_post_1945).
narrative_ontology:cs_axiom_status(total_war_physically_unreachable_post_1945, holdable).
narrative_ontology:cs_axiom_grounding('94df7331-6efa-45a3-8681-81e3caf072ab', total_war_physically_unreachable_post_1945, empirically_contingent).
narrative_ontology:cs_axiom('94df7331-6efa-45a3-8681-81e3caf072ab', foundational, deterrence_operates_by_physical_not_social_means).
narrative_ontology:cs_axiom_status(deterrence_operates_by_physical_not_social_means, holdable).
narrative_ontology:cs_axiom_grounding('94df7331-6efa-45a3-8681-81e3caf072ab', deterrence_operates_by_physical_not_social_means, empirically_contingent).
narrative_ontology:cs_created_at('94df7331-6efa-45a3-8681-81e3caf072ab', '').
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

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents catastrophic great-power total war by making the expected cost of unlimited warfare exceed any conceivable political gain through the physical logic of assured destruction.
% TRANSFER_FUNCTION: No transfer; the constraint imposes a physical boundary on the action space rather than moving resources, status, or labor between agents.
% ABSENT_VOICES: Advocates of complete nuclear abolition who argue the constraint is reversible and temporary; non-state actors excluded from deterrence stability frameworks.
% DISAPPEARANCE_RATIONALE: If nuclear weapons ceased to make total war structurally impossible â for example through perfect strategic defense or total disarmament â great powers would reorganize for total war planning as in the pre-1945 era, alliance structures would shift from deterrence to warfighting postures, and the international system would rearrange fundamentally.
% FOUNDING_PROBLEM: Recurrent catastrophic total war between industrialized great powers, producing the world wars of the first half of the twentieth century.
% FOUNDING_PROBLEM_CORROBORATION: Structural realist theorists in strategic studies (e.g., Waltz, Mearsheimer) attest from an analytical seat outside any beneficiary structure that nuclear weapons have physically foreclosed great-power total war; there is no beneficiary party to self-assert this claim.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.02, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.02 because the constraint extracts nothing from any agent; it is a physical boundary that governs outcomes without transferring resources or rents. Suppression is 0.05 because no coercive apparatus is required to maintain it â states comply with the constraint the way they comply with gravity. Accessibility collapse is 0.96 because, once the physical logic of assured destruction is understood, the alternative (fighting and winning a total nuclear war) collapses as a viable concept. Resistance is 0.03 because there is essentially no active resistance to a physical law; even nuclear-war-fighting doctrines were operational adaptations to the constraint, not rejections of it. The flat measurement series reflect the stability of the physical condition from 1945 to the present.
 *
 * PERSPECTIVAL GAP:
 *   Within this reading there is no perspectival gap in directionality because the constraint has no parties â it acts uniformly on all states as a physical limit. Across the kernel, however, the three readings produce divergent computed types: this structural reading computes as mountain; the normative reading (if instantiated as a separate constraint story) would declare beneficiaries in international law institutions and victims in constrained states; the strategic-culture reading would likely compute as rope or identity_coordination depending on enforcement mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the constraint has no extraction structure. All nuclear-armed states sit symmetrically with respect to the physical boundary; non-nuclear states are also bounded by the spillover risk of nuclear exchange. There is no directional transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this reading because the constraint is not an institution with a mandate. It is a physical condition produced by the distribution and character of nuclear arsenals. The risk of institutional mandate outliving function applies to the normative and cultural siblings (arms-control treaties, strategic discourse), but the structural reading exists independently of any institutional maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_normative_impossibility,
    'Is the post-1945 absence of total war a physically necessary consequence of nuclear arsenals, or a socially constructed normative prohibition that could persist even if the physical balance shifted?',
    'A technological or strategic breakthrough that breaks assured destruction (e.g. perfect missile defense, decapitation capability) would test the structural reading: if total war planning resumes, the constraint was physical; if it remains taboo, the normative reading gains support.',
    'If the constraint is normative rather than physical, it acquires beneficiaries (institutions of arms control, humanitarian law) and victims (states constrained by prohibition), shifting classification away from mountain toward rope or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_normative_impossibility, conceptual, 'Whether total war absence is physical or normative kernel contest').

omega_variable(
    counterfactual_victim_scope,
    'Do hypothetical populations in a counterfactual nuclear exchange constitute a victim set for structural classification purposes?',
    'Exclusion from classification: victimhood in this framework requires actual agents currently shaped by the constraint, not counterfactual casualties. The schema''s stakeholder-coverage rule applies to real parties only.',
    'Clarifies why no beneficiaries or victims are declared on this reading; prevents spurious FSM or extraction detection from hypothetical harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_victim_scope, conceptual, 'Status of counterfactual populations as classification-relevant victims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tota_tr_t20, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(tota_tr_t40, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(tota_tr_t60, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(tota_tr_t80, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 80, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(tota_be_t20, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 20, 0.02).
narrative_ontology:measurement(tota_be_t40, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 40, 0.02).
narrative_ontology:measurement(tota_be_t60, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 60, 0.02).
narrative_ontology:measurement(tota_be_t80, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 80, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% The total_war_winnability_post1945 kernel decomposes into three structurally distinct constraints per the Îµ-invariance principle: physical impossibility (this file), normative prohibition, and ideational unthinkability. Each has a different Îµ, different stakeholder structure, and different classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
