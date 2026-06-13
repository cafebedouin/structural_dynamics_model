% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary — Categorical Impermissibility of Medical Coercion
 *   domain: medical_ethics/constitutional_law/public_health_policy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'coercion_legitimacy_boundary.' The reading asserts that medical
 *   intervention without consent is categorically impermissible regardless of
 *   collective benefit. This is a deontological boundary claim: bodily
 *   integrity is foundational and non-tradable, not a variable in a
 *   benefit-cost calculus. The kernel contest pits this reading against two
 *   siblings: a 'public_health_primary' reading that permits medical coercion
 *   when collective harm-prevention outweighs individual autonomy, and a
 *   'proportionality_reading' that scales coercion legitimacy with disease
 *   severity (measles justifies mandates, influenza does not). These three
 *   readings are structurally incommensurable — they make contradictory
 *   claims about the same domain using mutually incompatible frameworks. This
 *   constraint describes only the bodily_autonomy_primary reading.
 *
 * KEY AGENTS:
 *   - bodily_autonomy_framework_adherents: Beneficiaries. Hold a constitutional framework where bodily autonomy is foundational and non-negotiable. The constraint's classification as natural law shields their reading from having to justify itself via collective-benefit arguments.
 *   - immunocompromised_exposed_population: Victims. Medically unable to self-vaccinate; bear the cost of non-enforcement in the form of exposure to diseases they cannot protect against. Their exit is trapped (cannot leave the biological exposure).
 *   - mandate_enforcers: Institutional agenda-setters and structural beneficiaries. Positioned as beneficiaries because the constraint's categorical impermissibility removes coercive tools from their authority set, de-politicizing the choice not to mandate ('We cannot' vs. 'We choose not to because costs exceed benefits').
 *   - unvaccinated_choice_exercisers: Beneficiaries. Directly protected by the constraint's operation; their autonomy choice receives categorical protection from collective-benefit override.
 *   - proportionality_reading_jurisdictions: Excluded. Reject this reading and hold structurally incompatible frameworks. Their exclusion is built into the kernel contest.
 *   - international_human_rights_bodies: Observers. Track divergence between readings but cannot resolve it from within any single framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.41).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.28).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.41).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, mountain).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy Primary — Categorical Impermissibility of Medical Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "medical_ethics/constitutional_law/public_health_policy").

domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '53efedfe-6583-4831-93e3-99cae815e9a1').
narrative_ontology:cs_kernel_codification('53efedfe-6583-4831-93e3-99cae815e9a1', fixed_text).
narrative_ontology:cs_authority_grounding('53efedfe-6583-4831-93e3-99cae815e9a1', lineage).
narrative_ontology:cs_interpretation_layer_present('53efedfe-6583-4831-93e3-99cae815e9a1').
narrative_ontology:cs_reading_relation('53efedfe-6583-4831-93e3-99cae815e9a1', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('53efedfe-6583-4831-93e3-99cae815e9a1', coercion_legitimacy_boundary__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('53efedfe-6583-4831-93e3-99cae815e9a1', foundational, bodily_integrity_non_tradable).
narrative_ontology:cs_axiom_status(bodily_integrity_non_tradable, holdable).
narrative_ontology:cs_axiom_grounding('53efedfe-6583-4831-93e3-99cae815e9a1', bodily_integrity_non_tradable, deontological).
narrative_ontology:cs_axiom('53efedfe-6583-4831-93e3-99cae815e9a1', foundational, consent_categorically_required).
narrative_ontology:cs_axiom_status(consent_categorically_required, holdable).
narrative_ontology:cs_axiom_grounding('53efedfe-6583-4831-93e3-99cae815e9a1', consent_categorically_required, deontological).
narrative_ontology:cs_reference_frame('53efedfe-6583-4831-93e3-99cae815e9a1', foundational_bodily_integrity).
narrative_ontology:cs_drift_state('53efedfe-6583-4831-93e3-99cae815e9a1', contemporary_pandemic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('53efedfe-6583-4831-93e3-99cae815e9a1', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, autonomy_framework_inheritors).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_exposed_population).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, ExtMetricName, E),
    domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a MOUNTAIN (natural law) because the reading asserts bodily integrity as a foundational feature of human persons, not a constructed policy choice. However, beneficiaries are present: the autonomy_framework_adherents and mandate_enforcers demonstrably benefit from the constraint's classification as natural law. This triggers False Summit Mountain (FSM) evaluation. The extractiveness is MODERATE (0.41) rather than near-zero because: (1) the constraint's operation de-politicizes a structurally political choice — the choice not to mandate — by framing it as natural law ('We cannot'), and (2) the immunocompromised population bears a cost (exposure to diseases) that would not exist under public_health_primary or proportionality readings. Suppression is LOW (0.28) because the constraint does not require coercive enforcement; autonomy advocates WANT this reading and resistance from public_health advocates is high but does not translate to active suppression. Accessibility_collapse is HIGH (0.72) because once the bodily_autonomy_primary reading is accepted, alternative framings (proportionality, collective-benefit override) collapse as incoherent — the reading achieves dominance through logical closure, not through suppressive mechanism. Resistance is also HIGH (0.68) because public_health advocates and proportionality advocates mount active, organized resistance and offer competing frameworks. Theater_ratio is LOW-MODERATE (0.22) because the constraint's main activity is legal/constitutional adjudication and advocacy, not performative maintenance. The measurement series is nearly flat (slow rise then plateau) because this reading's structural position — as a constitutional/human-rights claim — is stable across the interval; unlike extractive constraints that intensify over time, a boundary claim tends to equilibrate once established.
 *
 * PERSPECTIVAL GAP:
 *   The bodily_autonomy_framework_adherents and the immunocompromised population experience this constraint completely differently. From the framework adherents' seat, the constraint is protection: it preserves the foundational right against violation. From the immunocompromised seat, the constraint is abandonment: it denies them collective protection others would provide. The mandate_enforcers' position is complex: they are institutional actors with both agenda-setter (they enforce the constraint) and beneficiary (the constraint removes difficult political-cost decisions) dimensions. The constraint forces them to rely on voluntary compliance, which they frame as more legitimate ('respecting autonomy') but which actually de-politicizes a political choice ('We cannot compel because bodily integrity is foundational' rather than 'We are choosing not to compel because the democratic cost exceeds the epidemiological benefit'). From the public_health_primary reading's seat, this beneficiary positioning is itself extractive — the constraint extracts political cover from the apparatus at the cost of the immunocompromised. The engine's per-seat computation should show: framework_adherents near full beneficiary (d ~0.1), immunocompromised near full target (d ~0.9), mandate_enforcers near beneficiary despite agenda-setter role (d ~0.2-0.3) because the constraint shields them from political cost, unvaccinated beneficiaries near beneficiary (d ~0.2).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary identification: bodily_autonomy_framework_adherents benefit directly — their reading is protected from having to justify itself via competing frameworks. Mandate_enforcers are structurally beneficiaries because the constraint removes coercive tools and thus removes the political cost of choosing not to use them; their authority is constrained but their political exposure is reduced. Victim identification: immunocompromised_exposed_population bears the cost of non-enforcement in the form of disease exposure. This is not a rhetorical cost (stigma, social pressure) but a biological cost (disease risk without access to collective protection that would exist under public_health_primary readings). Exit options: the immunocompromised are trapped (biological exposure cannot be exited); bodily_autonomy_framework_adherents have arbitrage options (move to proportionality jurisdictions if autonomy_primary becomes untenable); unvaccinated choice_exercisers have constrained exit (geography bounds them). Power differentials: mandate_enforcers are institutional (centralized authority); framework_adherents are organized (legal and advocacy networks); immunocompromised are powerless (dispersed, medical dependents, lower political voice); unvaccinated choice_exercisers are moderate (can organize resistance but lack institutional authority). This structural asymmetry should produce high directionality for the immunocompromised (target end, ~0.85-0.95) and low directionality for framework_adherents (beneficiary end, ~0.05-0.15), with mandate_enforcers displaced toward beneficiary despite their agenda_setter role because the constraint shields them politically.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT classified as a mandatrophy (the founding problem is NOT dead while the constraint persists). Instead, the founding problem is CONTESTED, which is the appropriate terminal state for a kernel reading. The founding problem was: preventing medical coercion and protecting bodily integrity (historical evidence: eugenic forced sterilization, Tuskegee experiments, Nazi medical atrocities). This problem is LIVE in the sense that coercive medicine remains a persistent risk and medical ethics continues to enforce consent requirements. However, a COMPETING founding problem exists within the public_health_primary reading: preventing mass disease and disability through collective protection when individual choice fails to achieve herd immunity. This competing problem is also LIVE (COVID-19, measles resurgence in low-vaccination communities). The constraint does NOT represent mandatrophy because the reading itself is contestable at the foundational level — the classification should show a Tangled Rope or Snare from the public_health_primary or proportionality seats, while remaining a Mountain from the bodily_autonomy_primary seat. The divergence is the signal, not an error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the categorical impermissibility of medical coercion a feature of natural law / foundational human rights, or is it a constructed interpretive reading chosen for its protective utility?',
    'Comparative legal-historical analysis: If the categorical impermissibility claim emerges from specific historical moments (post-Nazi atrocities, post-Tuskegee, disability rights movements) rather than from timeless philosophical argument, and if it is actively defended against competing readings (proportionality, public_health_primary) rather than self-evident, it is likely constructed. If the same principle emerges independently across cultures and legal traditions without explicit diffusion, it is more plausibly natural-law-like.',
    'If constructed rather than natural, the constraint reclassifies from Mountain to Tangled Rope (coordination function: preventing medical atrocities; extraction: de-politicizing the choice not to mandate). The beneficiary set remains the same, but the mechanism becomes enforcement of an interpretive reading rather than natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether bodily autonomy is foundational or an interpretive choice.').

omega_variable(
    immunocompromised_coercion_framing,
    'Is the immunocompromised population''s exposure to unvaccinated disease a form of COERCION (denied collective protection, forced to accept elevated risk) or an unavoidable externality of respecting autonomy?',
    'Framing analysis: If public_health authorities actively structure non-mandate policies in ways that maximize the immunocompromised''s exposure relative to what baseline protection would exist, it is coercive. If the non-mandate is passive (absence of mandate) rather than active (prevention of protective barriers), it is framed as externality. The empirical test: do non-mandate jurisdictions take affirmative steps to prevent immunocompromised from accessing collective protection (e.g., refusing to fund vaccine distribution, legally blocking vaccine mandates for healthcare workers), or do they simply not require vaccination?',
    'If the immunocompromised''s situation is reframed as coercion (denial of protection), the victim set expands and the constraint''s extractiveness increases (it extracts compliance with autonomy_primary reading at the cost of the immunocompromised''s access to collective protection). If externality, the constraint''s extractiveness remains moderate and framed as tragic-but-necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_coercion_framing, conceptual, 'Whether non-mandate policies toward the immunocompromised are coercive or externality.').

omega_variable(
    mandate_enforcer_benefit_visibility,
    'Do mandate_enforcers (public health authorities, clinicians) explicitly recognize that the bodily_autonomy_primary reading benefits them by de-politicizing the choice not to mandate, or is this benefit structural but unacknowledged?',
    'Discourse analysis: examine whether public health authorities in autonomy_primary jurisdictions explicitly frame non-mandates as ''respecting autonomy'' (acknowledging the benefit of political cover) or whether they frame it as legal necessity (''we cannot mandate''). Interviews with mandate-capable officials in jurisdictions that chose not to mandate despite disease threat could reveal whether the autonomy framing was chosen for its political utility or adopted as necessary constraint.',
    'If acknowledged, the beneficiary relationship is more transparent and the constraint''s extractiveness slightly increases (knowing extraction). If unacknowledged, the de-politicization is more complete but the structural benefit remains. No reclassification occurs; the impact is diagnostic rather than categorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_enforcer_benefit_visibility, empirical, 'Whether public health authorities recognize the political benefit they derive from autonomy_primary framing.').

omega_variable(
    proportion_of_unvaccinated_choice_vs_access_barriers,
    'In autonomy_primary jurisdictions with low vaccination coverage, what proportion of non-vaccination is choice-based (individuals exercising autonomy preferences) versus access-barrier-based (individuals without access to vaccines due to cost, geography, or lack of infrastructure)?',
    'Population surveys distinguishing refusal from access barriers. If high access-barrier proportion, the unvaccinated population is not genuinely ''choosing'' non-vaccination; the autonomy_primary framing protects choice for some while denying access to others. If high choice-based proportion, the autonomy protection is more genuine.',
    'If significant access-barrier component, the constraint''s true operation is: protecting choice for the able-to-choose while perpetuating barriers for the unable-to-access. This would split the ''unvaccinated'' population into beneficiaries (choice-exercisers) and victims (access-barred). The extractiveness would increase because the constraint protects a narrower set than its framing suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportion_of_unvaccinated_choice_vs_access_barriers, empirical, 'The composition of non-vaccination: genuine choice vs. access barriers.').

omega_variable(
    kernel_reading_commensuability,
    'Can the three kernel readings (bodily_autonomy_primary, proportionality, public_health_primary) be nested into a single meta-framework, or are they genuinely incommensurable?',
    'Philosophical analysis: attempt to construct a framework that holds all three readings as sub-cases. For example: ''Bodily autonomy is foundational UNLESS disease severity and transmission risk exceed a threshold, at which point proportionality applies, and public_health_primary applies when collective harm-prevention is demonstrable.'' This would nest bodily_autonomy_primary as the default case within a proportionality framework. If no coherent nesting exists (e.g., bodily_autonomy_primary''s claim that autonomy is NON-TRADABLE is genuinely inconsistent with proportionality''s claim that autonomy SCALES with disease severity), the readings are incommensurable.',
    'If commensurable, the three constraints form a single constraint family with ordered hierarchy (one reading is the default case, others are exceptions). If incommensurable, the three constraints are genuinely rival — they cannot coexist in a single framework and represent competing constitutional orders. This affects whether the engine should treat them as a constraint family with shared directionality derivation or as separate kernel families with fundamentally different authority structures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_commensuability, conceptual, 'Whether kernel readings are nestable into a single meta-framework or genuinely incommensurable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 5, 0.19).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 10, 0.2).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 15, 0.21).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.22).
narrative_ontology:measurement(coer_tr_t25, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 25, 0.22).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 30, 0.22).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 5, 0.39).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(coer_be_t25, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 40, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 5, 0.26).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(coer_su_t25, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 25, 0.28).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'coercion_legitimacy_boundary.' The kernel contains three structurally incommensurable readings: bodily_autonomy_primary (this constraint), public_health_primary (coercion legitimate when collective harm-prevention outweighs individual autonomy), and proportionality_reading (coercion scales with disease severity). Each reading instantiates a different constraint with different ε values, beneficiary/victim sets, and computed types. The three constraints form a kernel family linked by network.affects_constraints edges. Bodily_autonomy_primary appears as a Mountain from its own seat; public_health_primary appears as a Snare or Tangled_Rope from bodily_autonomy_primary's seat (and vice versa). The divergence is the signal — it indicates genuine kernel contest, not measurement error.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__bodily_autonomy_primary, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
