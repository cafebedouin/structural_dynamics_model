% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Post-1945 Total War Winnability (Strategic Culture Drift Reading)
 *   domain: international_relations_theory/strategic_studies/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint describes the ideational shift in strategic culture
 *   post-1945, where the concept of 'total war winnability' dropped from
 *   elite discourse, not due to structural impossibility or normative
 *   illegitimacy, but due to a change in how strategic elites thought about
 *   conflict. It is a reading of the 'total_war_winnability_post1945' kernel,
 *   focusing on the internal, ideational dynamics within strategic
 *   communities. The constraint is claimed as a Piton because its primary
 *   function (preventing total war) has atrophied, but the discursive absence
 *   persists due to institutional inertia and the self-interest of those
 *   invested in limited war frameworks. The metrics reflect a low but
 *   accumulating extractiveness (from strategic flexibility) and a rising
 *   theater ratio, as the 'unthinkability' of total war becomes more
 *   performative than genuinely believed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.25).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.4).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.25).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Post-1945 Total War Winnability (Strategic Culture Drift Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations_theory/strategic_studies/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'afce554b-9486-429c-86c1-6acdb437d4eb').
narrative_ontology:cs_kernel_codification('afce554b-9486-429c-86c1-6acdb437d4eb', implicit).
narrative_ontology:cs_authority_grounding('afce554b-9486-429c-86c1-6acdb437d4eb', practice).
narrative_ontology:cs_interpretation_layer_present('afce554b-9486-429c-86c1-6acdb437d4eb').
narrative_ontology:cs_reading_relation('afce554b-9486-429c-86c1-6acdb437d4eb', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('afce554b-9486-429c-86c1-6acdb437d4eb', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('afce554b-9486-429c-86c1-6acdb437d4eb', foundational, strategic_culture_shapes_possibility).
narrative_ontology:cs_axiom_status(strategic_culture_shapes_possibility, holdable).
narrative_ontology:cs_axiom_grounding('afce554b-9486-429c-86c1-6acdb437d4eb', strategic_culture_shapes_possibility, empirically_contingent).
narrative_ontology:cs_axiom('afce554b-9486-429c-86c1-6acdb437d4eb', foundational, ideational_shifts_are_contingent).
narrative_ontology:cs_axiom_status(ideational_shifts_are_contingent, holdable).
narrative_ontology:cs_axiom_grounding('afce554b-9486-429c-86c1-6acdb437d4eb', ideational_shifts_are_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('afce554b-9486-429c-86c1-6acdb437d4eb', post_wwii_strategic_consensus).
narrative_ontology:cs_drift_state('afce554b-9486-429c-86c1-6acdb437d4eb', contemporary_geopolitical_flux, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('afce554b-9486-429c-86c1-6acdb437d4eb', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, national_security_establishments).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_planners).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, military_doctrine_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These academics and policy analysts benefit from the intellectual dominance of limited war frameworks, which their careers are built upon. The idea of total war winnability challenges their established paradigms, making its discursive absence beneficial to their professional standing.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war, beneficiary,
    organized, biographical, identity_locked, global).

% These institutions administer strategic planning and doctrine. While they benefit from the stability of limited war discourse, they also bear the diffuse cost of atrophied capacity for full-spectrum conflict planning. They maintain the discursive constraint through funding and publication choices.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, national_security_establishments, agenda_setter,
    institutional, generational, constrained, global).

% Military and civilian planners who must prepare for all contingencies. They are victims of the constraint because the ideational shift limits the scope of acceptable strategic thought, potentially leaving them unprepared for scenarios outside the limited war paradigm. Their 'exit' is to challenge the dominant discourse, which carries career risk.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_planners, payer,
    moderate, immediate, constrained, national).

% Those responsible for developing military doctrine find their conceptual toolkit constrained by the prevailing strategic culture. They pay in reduced strategic flexibility and the intellectual cost of working within an incomplete framework. Their professional identity is tied to the accepted strategic culture.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_doctrine_developers, payer,
    moderate, biographical, identity_locked, national).

% Academics and historians who analyze the evolution of strategic thought and challenge dominant narratives. They observe the constraint's operation and its effects on strategic culture, often highlighting the historical contingency of current assumptions.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, historical_revisionists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates strategic discourse around a shared understanding of acceptable conflict intensity and objectives, facilitating inter-state communication and crisis management within a limited war paradigm.
% TRANSFER_FUNCTION: Transfers intellectual and institutional resources away from total war planning and towards limited war scenarios, from those who might consider full-spectrum conflict to those invested in its discursive absence.
% ABSENT_VOICES: Advocates for a more comprehensive, 'worst-case' strategic planning, including the possibility of total war winnability, are marginalized or dismissed as anachronistic. Their absence reinforces the prevailing limited war discourse.
% DISAPPEARANCE_RATIONALE: If the ideational constraint on total war winnability vanished, strategic planning would immediately broaden, defense budgets might reallocate, and the intellectual landscape of strategic studies would undergo a significant reorientation, impacting doctrine, training, and international relations.
% FOUNDING_PROBLEM: The post-WWII era sought to prevent a recurrence of devastating global conflict, leading to a strategic culture that emphasized limited objectives and the unthinkability of total war.
% FOUNDING_PROBLEM_CORROBORATION: National security establishments and defense intellectuals attest that preventing global conflict remains a live problem. Historical revisionists corroborate the shift in strategic culture but contest whether the problem of total war winnability was ever truly 'solved' or merely suppressed from discourse.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.25) because it's primarily an ideational constraint, extracting 'strategic flexibility' rather than material resources. Suppression (0.4) is moderate, reflecting the soft power of academic and institutional consensus rather than overt coercion. The high and rising theater ratio (0.65) is key to its Piton classification: the performance of 'total war is unwinnable' has become more significant than its actual strategic utility, as the capacity for such conflict remains. The ideational shift means that while total war is still physically possible, the intellectual tools and frameworks for conceptualizing its 'winnability' have atrophied, creating a gap between capability and discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of defense intellectuals, the constraint is a beneficial coordination mechanism that maintains intellectual order. From the perspective of strategic planners, it's a subtle but real limitation on their professional capacity. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense intellectuals invested in limited war frameworks are beneficiaries, as their intellectual capital is preserved. National security establishments, while benefiting from discursive stability, also bear the diffuse cost of reduced strategic flexibility, making them both agenda-setters and diffuse payers. Strategic planners and military doctrine developers are payers, as their professional scope is constrained. The constraint's ideational nature means 'identity_locked' is a common exit option for those whose careers and self-concept are tied to the dominant strategic culture.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideational_vs_material_causation,
    'To what extent is the absence of total war winnability discourse due to genuine ideational shifts versus underlying material or structural changes (e.g., nuclear deterrence)?',
    'Comparative historical analysis of strategic cultures in different nuclear/non-nuclear contexts, or counterfactual analysis of how discourse might have evolved without specific material conditions.',
    'If material factors are dominant, this constraint''s extractiveness and suppression might be lower, as the ideational constraint is merely a reflection. If ideational factors are primary, the Piton classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideational_vs_material_causation, empirical, 'Distinguishing ideational from material drivers of strategic culture.').

omega_variable(
    mandatrophy_of_strategic_flexibility,
    'Has the ''mandate'' to consider total war winnability truly atrophied, or is it merely latent, capable of rapid revival under different geopolitical conditions?',
    'Observation of strategic discourse and planning in response to major geopolitical shocks or shifts in power balances. A rapid re-emergence would suggest latency, not atrophy.',
    'If latent, the Piton classification might be too strong, suggesting a more resilient, albeit suppressed, capacity. If truly atrophied, the Piton classification is accurate, highlighting a genuine loss of strategic capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_strategic_flexibility, empirical, 'Assessing the true state of strategic capacity for total war.').

omega_variable(
    discursive_suppression_mechanism,
    'Is the suppression of total war winnability discourse primarily structural (institutional funding, publication biases) or internalized (self-censorship by strategic thinkers)?',
    'Qualitative interviews with strategic planners and defense intellectuals, combined with analysis of funding patterns and publication gatekeeping.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as the constraint operates even without overt enforcement. If structural, external interventions (e.g., funding for alternative research) would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discursive_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for strategic discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1985, 0.5).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2005, 0.6).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.15).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2005, 0.23).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
