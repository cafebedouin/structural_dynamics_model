% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism: Strict Neutrality Reading
 *   domain: constitutional_law/religious_governance
 *
 * SUMMARY:
 *   Constitutional secularism establishes that the state maintains equal
 *   distance from all religions—no preferential treatment, no state
 *   intervention in religious affairs. This is ONE reading of a contested
 *   kernel. The strict neutrality reading treats state silence toward
 *   internal religious practices as non-interference (even when those
 *   practices are hierarchical or oppressive), and state refusal to intervene
 *   in religious reform as equal distance (even when it leaves marginalized
 *   persons within communities without recourse). This reading is opposed by
 *   principled-intervention advocates (who argue for limited state
 *   intervention to protect basic rights) and reformist advocates (who argue
 *   the state has affirmative duty to eliminate oppressive religious
 *   practices). The constraint story models the strict neutrality reading as
 *   internally coherent but extractive toward powerless persons locked into
 *   community identity.
 *
 * KEY AGENTS:
 *   - State Judiciary: Guardian of the strict neutrality reading; interprets and enforces equal distance.
 *   - Religious Minorities: Beneficiaries of protection from majoritarian dominance; protected by the constraint's guarantee of non-preferential treatment.
 *   - Majority Religious Community: Payer; cannot use state machinery for religious amplification or establishment.
 *   - Marginalized Groups Within Communities: Victims; locked into identity within communities; cannot petition state for intervention against internal oppression.
 *   - Reform-Oriented Religious Segments: Victims; cannot invoke state support for internal reform agendas.
 *   - Constitutional Text Guardians: Beneficiaries of the reading's stability and rule-bound coherence.
 *   - Principled-Intervention Advocates: Excluded from the decision-making frame; their intervention-justifying claims are treated as outside constitutional bounds.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.62).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.41).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism: Strict Neutrality Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '491a91d9-3af5-41e6-9c2d-496f78038732').
narrative_ontology:cs_kernel_codification('491a91d9-3af5-41e6-9c2d-496f78038732', formalized).
narrative_ontology:cs_authority_grounding('491a91d9-3af5-41e6-9c2d-496f78038732', lineage).
narrative_ontology:cs_interpretation_layer_present('491a91d9-3af5-41e6-9c2d-496f78038732').
narrative_ontology:cs_reading_relation('491a91d9-3af5-41e6-9c2d-496f78038732', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('491a91d9-3af5-41e6-9c2d-496f78038732', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('491a91d9-3af5-41e6-9c2d-496f78038732', foundational, equal_distance_state_neutrality).
narrative_ontology:cs_axiom_status(equal_distance_state_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('491a91d9-3af5-41e6-9c2d-496f78038732', equal_distance_state_neutrality, deontological).
narrative_ontology:cs_axiom('491a91d9-3af5-41e6-9c2d-496f78038732', foundational, religious_autonomy_inviolability).
narrative_ontology:cs_axiom_status(religious_autonomy_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('491a91d9-3af5-41e6-9c2d-496f78038732', religious_autonomy_inviolability, conventional).
narrative_ontology:cs_reference_frame('491a91d9-3af5-41e6-9c2d-496f78038732', secular_equal_distance_framework).
narrative_ontology:cs_drift_state('491a91d9-3af5-41e6-9c2d-496f78038732', contemporary_rights_advocacy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('491a91d9-3af5-41e6-9c2d-496f78038732', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, constitutional_order_stability).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, marginalized_groups_within_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, reform_oriented_segments).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint shows moderate-to-high extractiveness (0.62) because it imposes real costs on marginalized persons within religious communities and reform advocates (who cannot invoke state protection) in order to preserve the equal-distance framework. Suppression is moderate (0.41) because the reading is enforced through judicial interpretation and constitutional doctrine, not through coercive force on the majority religious community—the suppression is primarily cognitive/interpretive (constraining what counts as legitimate state action). Theater ratio is low-moderate (0.28) because the reading's core function (preventing majoritarian religious dominance through equal distance) is real and durable, but some enforcement activity goes into defending religious autonomy even when internal structures are oppressive (theater masking the extraction cost). Accessibility collapse is moderately high (0.71) because once the strict neutrality reading is accepted as constitutional law, alternatives (principled intervention, reformist intervention) are structurally foreclosed from the state's decision space—but the collapse is incomplete because courts can and do revisit the reading through dissents and neighboring constitutional rights (e.g., equal protection of marginalized persons). Resistance is moderate-to-high (0.58) because marginalized persons petition for intervention, reform advocates challenge the reading, and competing institutional actors (human-rights commissions, legislative bodies in some polities) propose alternatives. The measurement series shows extractiveness rising through t=38 as awareness of internal-community oppression increases, then plateauing—the constraint's cost becomes stable once the reading is entrenched.
 *
 * PERSPECTIVAL GAP:
 *   The state judiciary and religious minorities perceive the constraint as genuine coordination solving majoritarian dominance. Marginalized persons within communities and reform advocates perceive it as extraction—a constraint that treats religious autonomy as inviolable at the cost of their protection. The constraint achieves one kind of equality (equal distance from religions) by sacrificing another (equal protection of vulnerable persons within religions). This perspectival divergence is structural: the constraint cannot simultaneously maximize external religious equality and internal minority protection—the reading chosen prioritizes the former. The engine should compute institutional and moderate-power seats as perceiving rope/coordination, while powerless and identity-locked seats perceive snare/extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious minorities are beneficiaries (d near 0.1-0.2): they receive protection from majoritarian dominance; the constraint subsidizes their structural position. Marginalized persons within communities are victims (d near 0.85-0.95): they bear the full cost of the constraint's neutrality—they cannot invoke state protection against internal oppression; they are identity-locked, with no exit except community abandonment. The majority religious community is a complex payer: they pay through inability to use state machinery for religious amplification (d near 0.65-0.75), but they also receive a stabilized constitutional order that prevents their own internal divisions from triggering state intervention (d modulation downward to 0.55-0.65). Reform-oriented segments within religions are victims (d near 0.70-0.80): they cannot invoke state support for their reform agendas; they bear the cost of the constraint's non-intervention stance. The state judiciary is the agenda-setter (d near 0.4-0.5, moderate): they maintain and enforce the reading but are themselves constrained by constitutional text and cannot simply change the rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict neutrality reading faces a potential mandatrophy condition: if the founding problem (sectarian violence from state-backed religious preference) is substantially solved, the constraint's core function is satisfied, but the extraction costs toward marginalized persons within communities remain. The measurement series shows extractiveness plateauing at t=38+, suggesting the reading has settled into an equilibrium where awareness of internal-community oppression has stabilized the tension but not resolved it. The theater ratio (rising through t=38, then stable) suggests that as awareness increases, enforcement activity must increasingly defend religious autonomy as a principle rather than as a practical necessity—the constraint is becoming more theatrical as its founding coordination problem recedes. The constraint is not yet mandatrophic (the reading remains live and contested, not purely performative), but the trajectory suggests that if the principled-intervention or reformist readings gain institutional purchase, the strict neutrality reading would be reclassified as inertial piton—a formerly useful doctrine now defended mostly through constitutional tradition rather than live functional need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_as_protection_vs_abandonment,
    'Is state equal distance from religions a form of protection for minorities (negative liberty: freedom from majoritarian state power) or a form of abandonment (inability to invoke state remedies against internal oppression)?',
    'Empirical: track outcomes for marginalized persons within religious communities in strict-neutrality vs. principled-intervention jurisdictions. If marginalized persons are more protected (education, safety, exit options) in intervention jurisdictions, the neutrality reading abandons them. If outcomes are identical, the reading succeeds as protection.',
    'If neutrality is abandonment, the reading should be reclassified as tangled rope or snare (coordination for religious minorities, extraction for internal powerless). If neutrality is genuine protection, the classification holds as rope with distributed costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_as_protection_vs_abandonment, empirical, 'Whether equal-distance neutrality protects or abandons marginalized persons within religions.').

omega_variable(
    internal_hierarchy_compatibility,
    'Can a religious community maintain internal hierarchy (gender exclusion, caste discrimination, authority concentration) indefinitely while the constraint remains stable, or does internal pressure from marginalized segments eventually force legal change?',
    'Historical and longitudinal: observe whether internal-hierarchy religions evolve toward egalitarianism under the constraint or whether entrenchment increases. If entrenchment increases, the neutrality reading may be inadvertently selecting for authoritarian religious structures.',
    'If entrenchment increases, the constraint is extractive toward reform-minded believers and marginalized segments. If evolution toward egalitarianism occurs, the constraint is neutral in outcome even if neutral in intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_hierarchy_compatibility, empirical, 'Whether strict neutrality inadvertently selects for authoritarian internal religious structures.').

omega_variable(
    founder_problem_resolution_status,
    'Has the founding problem (sectarian violence from state-backed religious preference) been solved by constitutional secularism, or has it transformed into a different problem (internal oppression under the guise of religious autonomy)?',
    'Comparative: measure sectarian violence in strict-neutrality jurisdictions vs. principled-intervention jurisdictions. Measure internal violence (oppression, forced exit, honor killings) in each regime type. If sectarian violence is down but internal violence is up, the problem has transformed rather than solved.',
    'If the problem is solved, the constraint is successfully functional as rope. If the problem has transformed, the constraint may be mandatrophic (defending against a past threat while enabling a new one).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_problem_resolution_status, empirical, 'Whether the founding problem (sectarian violence) is solved or transformed.').

omega_variable(
    reading_contestation_and_institutional_instability,
    'Does the coexistence of the strict neutrality, principled-intervention, and reformist readings create institutional instability in constitutional interpretation, or can courts hold one reading stable across time?',
    'Institutional analysis: track whether courts maintain consistent neutrality doctrine or whether dissents and neighboring rights claims erode the reading''s boundaries. If doctrine is stable, the reading is institutionally robust. If boundaries blur, the reading is contested.',
    'If institutional instability is high, the constraint itself becomes less predictable and more subject to reinterpretation—extractiveness may shift as the reading''s hold weakens. This would suggest the constraint is moving toward piton (theatrically maintained but increasingly unstable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contestation_and_institutional_instability, empirical, 'Whether the strict neutrality reading can be institutionally maintained against competing readings.').

omega_variable(
    committer_alternative_framings,
    'Are there coherent framing alternatives to the strict neutrality reading that would classify the same constitutional commitment differently?',
    'Conceptual: the principled-intervention and reformist readings instantiate alternative framings. If a court or constitutional tradition shifted from strict neutrality to principled intervention, would the same textual commitment be reread, or would it require constitutional amendment?',
    'If rereading is possible without amendment, the constraint is subject to reading-drift and may be reclassified by the engine as unstable. If amendment is required, the reading is more deeply entrenched.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_alternative_framings, conceptual, 'Whether the strict neutrality reading is the only defensible reading of the secularism commitment or whether alternative readings are structurally coherent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t12, constitutional_secularism__strict_neutrality_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__strict_neutrality_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement(cons_tr_t38, constitutional_secularism__strict_neutrality_reading, theater_ratio, 38, 0.26).
narrative_ontology:measurement(cons_tr_t50, constitutional_secularism__strict_neutrality_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(cons_tr_t62, constitutional_secularism__strict_neutrality_reading, theater_ratio, 62, 0.28).
narrative_ontology:measurement(cons_tr_t75, constitutional_secularism__strict_neutrality_reading, theater_ratio, 75, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cons_be_t12, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(cons_be_t38, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 38, 0.62).
narrative_ontology:measurement(cons_be_t50, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(cons_be_t62, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 62, 0.62).
narrative_ontology:measurement(cons_be_t75, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t12, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 25, 0.39).
narrative_ontology:measurement(cons_su_t38, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 38, 0.41).
narrative_ontology:measurement(cons_su_t50, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(cons_su_t62, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 62, 0.41).
narrative_ontology:measurement(cons_su_t75, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 75, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__strict_neutrality_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% The strict neutrality reading is one of three structurally distinct constraints sharing the kernel: constitutional commitment to state secularism. The three readings produce different ε values, different beneficiary/victim structures, and different temporal trajectories. The strict neutrality reading (this file) emphasizes protection of religious minorities through equal-distance non-interference; the principled-intervention reading activates when state intervention is justified by basic-rights protection; the reformist reading treats state-backed elimination of oppressive religious practices as mandatory. All three readings are live in contemporary constitutional practice across different jurisdictions and within the same jurisdictions (through dissenting opinions). The three constraints are linked through network.affects_constraints because they are alternative instantiations of the same constitutional kernel, and a shift from one reading to another reshapes the entire constraint landscape. The upstream dependency runs from strict_neutrality → principled_intervention → reformist in terms of empirical contestation (the strict neutrality reading is the most established; the reformist reading is the most contested and newly emerging).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
