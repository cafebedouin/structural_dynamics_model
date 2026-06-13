% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Sovereignty Instantiation
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   The UN Security Council's Article 27 permanent-member veto is one reading
 *   of a contested kernel: the principle that binding international law
 *   requires the consent of the bound. Under the sovereignty reading
 *   instantiated here, the veto is not a privilege captured by great powers
 *   or a mechanism to prevent war—it is the structural instantiation of
 *   Westphalian sovereignty applied to actors with unmatched enforcement
 *   capacity. No global institution can compel a state with enforcement
 *   capacity it cannot match. The veto is the institutional mechanism that
 *   enforces this principle, not a choice the institution made. This reading
 *   produces a Mountain classification (ε ≈ 0.08, near the Boltzmann floor
 *   for enforcement mechanisms): the constraint emerges from the logical
 *   structure of binding authority, not from institutional design choices.
 *   Extractiveness is near-zero because no party collects from the veto's
 *   operation; suppression is near-zero because the veto exerts no coercive
 *   force—it simply marks the boundary of institutional authority. Theater is
 *   low but non-zero: the institution performs commitment to universal
 *   law-making, but that performance is constrained at the veto threshold;
 *   the theater increases modestly over the interval as the institution's
 *   rhetoric grows disconnected from its actual reach (T17 theater-ratio
 *   drift).
 *
 * KEY AGENTS:
 *   - Great power state with nuclear capacity (institutional seat, analytical perspective): holds the veto, not as a collected privilege but as an instantiation of the principle that no state consents to binding authority exceeding its enforcement capacity.
 *   - Smaller state or coalition (organized seat, analytical perspective): seeks collective action, blocked by the veto, but recognizes the veto as derived from the same principle that would protect itself if it possessed unmatched enforcement capacity.
 *   - International legal framework (non-agent, analytical perspective): the UN Charter as a codified commitment system grounding binding authority in consent; the veto as the mechanism that enforces consent as a prerequisite for binding.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.08).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.02).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Sovereignty Instantiation").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '95b83b53-cfee-443a-857c-a53b94b7fd16').
narrative_ontology:cs_kernel_codification('95b83b53-cfee-443a-857c-a53b94b7fd16', formalized).
narrative_ontology:cs_authority_grounding('95b83b53-cfee-443a-857c-a53b94b7fd16', extraction).
narrative_ontology:cs_interpretation_layer_present('95b83b53-cfee-443a-857c-a53b94b7fd16').
narrative_ontology:cs_reading_relation('95b83b53-cfee-443a-857c-a53b94b7fd16', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('95b83b53-cfee-443a-857c-a53b94b7fd16', article_27_veto_power__oligopoly_reading, influences).
narrative_ontology:cs_axiom('95b83b53-cfee-443a-857c-a53b94b7fd16', foundational, no_state_binds_without_consent).
narrative_ontology:cs_axiom_status(no_state_binds_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('95b83b53-cfee-443a-857c-a53b94b7fd16', no_state_binds_without_consent, deontological).
narrative_ontology:cs_axiom('95b83b53-cfee-443a-857c-a53b94b7fd16', foundational, enforcement_asymmetry_structural).
narrative_ontology:cs_axiom_status(enforcement_asymmetry_structural, holdable).
narrative_ontology:cs_axiom_grounding('95b83b53-cfee-443a-857c-a53b94b7fd16', enforcement_asymmetry_structural, empirically_contingent).
narrative_ontology:cs_axiom('95b83b53-cfee-443a-857c-a53b94b7fd16', secondary, veto_enforces_consent_principle).
narrative_ontology:cs_axiom_status(veto_enforces_consent_principle, holdable).
narrative_ontology:cs_axiom_grounding('95b83b53-cfee-443a-857c-a53b94b7fd16', veto_enforces_consent_principle, instrumental).
narrative_ontology:cs_reference_frame('95b83b53-cfee-443a-857c-a53b94b7fd16', westphalian_consent_principle).
narrative_ontology:cs_drift_state('95b83b53-cfee-443a-857c-a53b94b7fd16', contemporary_institutional_contestation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('95b83b53-cfee-443a-857c-a53b94b7fd16', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as 0.08 (near the enforcement-mechanism Boltzmann floor of 0.10), not zero, for two reasons: (1) the measurement captures the minimal institutional overhead required to maintain the veto mechanism itself—rule enforcement, interpretation, procedural administration—and (2) slight ambiguity in whether the veto's passive role (permitting non-binding resolutions, channeling action toward General Assembly) counts as a micro-extraction or is purely structural. Under the sovereignty reading, the veto extracts nothing from actors because it transfers nothing. Suppression is 0.02 (near floor) because the veto exerts zero coercive force—it is a jurisdictional boundary, not an enforced obligation. Theater rises from 0.08 to 0.12 over the interval (T17 trigger: mountain_extraction_accumulation at warning level) because the institution's diplomatic and symbolic commitment to universal law-making grows while its actual binding reach shrinks—the gap between what the institution claims to do and what the veto permits it to do widens, increasing performative activity to bridge the gap. Accessibility_collapse is very high (0.92) because once the principle (no binding without consent from actors with unmatched enforcement capacity) is understood, no alternative to the veto exists—the constraint collapses all other options logically. Resistance is near-zero (0.04) because the constraint is not defended or attacked; it is recognized as inevitable by rational actors. The measurement grid is shared across all three metrics at five time points (0, 19, 38, 57, 76 years) representing institutional evolution from founding (1945) through Cold War, post-Cold War expansion, and contemporary contestation, with a contemporary endpoint at 2021.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty reading produces zero perspectival gap: all rational seats (great power, smaller state, legal framework) see the same constraint—a structural inevitability derived from the principle of consent. The coordination reading and oligopoly reading, by contrast, are situated readings that diverge sharply by perspective: the great power seat in the coordination reading collects security benefits (war prevention); the smaller-state seat in the oligopoly reading pays through blocked collective action. Those readings are NOT this constraint. This reading is designed to be seat-independent—a Mountain that emerges the same from every perspective because it derives from logical necessity, not from distributed costs and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares NO beneficiaries and NO victims because the constraint does not transfer benefits or extract from identifiable parties. The veto governs the boundary of institutional authority, not the allocation of rents. A great power holding the veto is not a 'beneficiary'—it avoids subjection to compulsion, but that avoidance is the constraint's effect on its directionality, not its rent. Under the sovereignty reading, every rational actor (at any power level) would object to being bound without consent; the veto simply ensures that objection is structural rather than contestable. Directionality overrides are not needed because stakeholders in this constraint are analytical observers of a principle, not situated actors collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply under this reading. The founding problem (how to maintain a universal institution when some subjects are more powerful than the institution itself) remains live, and the veto continues to solve it. The constraint is not a zombie institutional form persisting after its function died—it is a structural solution to a problem that persists as long as states retain enforcement capacity they do not surrender. The modest rise in theater_ratio (0.08 to 0.12) reflects drift in institutional rhetoric, not mandatrophy: the institution performs commitment to universal binding authority while remaining structurally constrained by the veto; this is Goodhart drift (the institution optimizes visible commitment while the veto constrains actual commitment), not loss of founding function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_instrumental_framing,
    'Is the veto a structural consequence of Westphalian sovereignty (no binding without consent), or an instrumental device chosen to solve specific coordination problems (war prevention, oligopoly protection)?',
    'Comparative institutional analysis: if alternative institutional designs (e.g., graduated voting, supermajority instead of veto, enforcement delegation to a separate body) could solve the same coordination functions without violating consent, then the veto is instrumental, not structural. If every alternative that preserves the consent principle requires some form of veto-equivalent boundary, then the veto is structural.',
    'If structural, the veto is inevitable (Mountain); if instrumental, it is contestable (Tangled Rope or Snare, depending on distribution of costs/benefits). This is the core distinction between the sovereignty reading and its siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_instrumental_framing, conceptual, 'Whether the veto follows necessarily from sovereignty principle or is one contingent institutional choice among alternatives.').

omega_variable(
    enforcement_capacity_asymmetry_persistence,
    'Do states actually retain enforcement capacity the international institution cannot match? Or has the institution (through UN peacekeeping, integrated enforcement, diplomatic pressure) acquired sufficient enforcement capacity to make the veto''s justification obsolete?',
    'Historical record of Security Council enforcement: can the institution compel a great power against its will? The empirical answer is consistently no (USSR/Russia vetoes, US vetoes over Israel, China vetoes over Taiwan). The question is whether this is a structural fact about international institutions or a temporary empirical feature that could change with institutional reform.',
    'If enforcement capacity asymmetry persists structurally, the veto remains necessary (sovereignty reading: Mountain). If the asymmetry could be eliminated through institutional reform and enforcement delegation, the veto becomes contingent (oligopoly reading: policy choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry_persistence, empirical, 'Whether great-power enforcement-capacity advantage is permanent or could be reformed away.').

omega_variable(
    sibling_reading_empirical_overlap,
    'The three readings (sovereignty, coordination, oligopoly) make different claims about why the veto persists. How can the same empirical institution exemplify all three readings simultaneously?',
    'Kernel decomposition: the three readings are not alternative measurements of the same constraint; they are three different constraints instantiated by the same textual commitment (Article 27). Each reading has a different ε, beneficiary structure, and type. The question is whether this is a valid kernel decomposition or a confusion between readings and measurements.',
    'If valid decomposition, the three stories should exist as separate constraint files linked via network.affects_constraints, each with its own metrics and type. If confusion, the prompt is asking for three incompatible framings of a single constraint, which violates ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_overlap, conceptual, 'Whether the three readings are legitimate kernel decomposition or confused conflation of readings with measurements.').

omega_variable(
    mountain_theater_drift_ambiguity,
    'Theater ratio drifts from 0.08 to 0.12 over the interval. Does this indicate the constraint is acquiring extractive overlay (institutional degradation) or merely increasing diplomatic performance around an unchanged structural boundary?',
    'Measurement of gap between institutional rhetoric (commitment to universal binding authority) and institutional practice (veto-blocked enforcement). If the gap widens and the institution performs more elaborate justifications for the veto, theater drifts upward due to Goodhart dynamics. If the institution''s actual coordination function remains stable and theater rise reflects only increased ceremonialism, it is performance drift without extraction drift (consistent with Mountain classification).',
    'If extractive overlay is present, the constraint may degrade from Mountain toward Piton (institutionalized performance masking atrophied function). Under the sovereignty reading, extractive overlay would be incoherent—the veto has no function except to enforce the consent principle; if that function persists, theater drift is performance without extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_theater_drift_ambiguity, empirical, 'Whether theater drift indicates degradation or performance overlay on stable structural function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a27_veto_sov_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(a27_veto_sov_tr_t19, article_27_veto_power__sovereignty_reading, theater_ratio, 19, 0.1).
narrative_ontology:measurement(a27_veto_sov_tr_t38, article_27_veto_power__sovereignty_reading, theater_ratio, 38, 0.12).
narrative_ontology:measurement(a27_veto_sov_tr_t57, article_27_veto_power__sovereignty_reading, theater_ratio, 57, 0.12).
narrative_ontology:measurement(a27_veto_sov_tr_t76, article_27_veto_power__sovereignty_reading, theater_ratio, 76, 0.12).

% Extraction over time
narrative_ontology:measurement(a27_veto_sov_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(a27_veto_sov_be_t19, article_27_veto_power__sovereignty_reading, base_extractiveness, 19, 0.08).
narrative_ontology:measurement(a27_veto_sov_be_t38, article_27_veto_power__sovereignty_reading, base_extractiveness, 38, 0.08).
narrative_ontology:measurement(a27_veto_sov_be_t57, article_27_veto_power__sovereignty_reading, base_extractiveness, 57, 0.08).
narrative_ontology:measurement(a27_veto_sov_be_t76, article_27_veto_power__sovereignty_reading, base_extractiveness, 76, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(a27_veto_sov_su_t0, article_27_veto_power__sovereignty_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(a27_veto_sov_su_t19, article_27_veto_power__sovereignty_reading, suppression_requirement, 19, 0.02).
narrative_ontology:measurement(a27_veto_sov_su_t38, article_27_veto_power__sovereignty_reading, suppression_requirement, 38, 0.02).
narrative_ontology:measurement(a27_veto_sov_su_t57, article_27_veto_power__sovereignty_reading, suppression_requirement, 57, 0.02).
narrative_ontology:measurement(a27_veto_sov_su_t76, article_27_veto_power__sovereignty_reading, suppression_requirement, 76, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (article_27_veto_power). The sovereignty reading instantiates the veto as a structural consequence of Westphalian consent-principle applied to asymmetric enforcement capacity. The coordination reading (sibling constraint) instantiates the veto as a war-prevention mechanism. The oligopoly reading (sibling constraint) instantiates the veto as extractive entrenchment. All three readings describe the same textual commitment (UN Charter Article 27) but have different ε values, beneficiary/victim structures, and types. The sovereignty reading produces a Mountain classification (ε ≈ 0.08, no beneficiaries/victims, constraint is structural inevitability). The coordination and oligarchy readings produce Tangled Rope and Snare classifications respectively (ε substantially higher, identifiable beneficiary/victim distributions, constraint is contingent institutional choice). These are not the same constraint measured differently; they are three different constraints instantiated by the same kernel. Each should be authored as a separate constraint story; this file encodes the sovereignty reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
