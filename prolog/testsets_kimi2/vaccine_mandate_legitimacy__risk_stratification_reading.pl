% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Vaccine Mandate Legitimacy â Risk Stratification Reading
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story models the risk-stratification reading of vaccine
 *   mandate legitimacy: a legal-constitutional doctrine holding that state
 *   coercion to vaccinate is legitimate only when targeted by actuarial risk
 *   threshold, with blanket mandates failing proportionality. It functions as
 *   a mediating framework between absolute bodily autonomy and unconstrained
 *   public health power, coordinating legitimacy for targeted interventions
 *   while asymmetrically extracting bodily autonomy from high-risk
 *   populations. The reading is one of three sibling readings of the
 *   vaccine_mandate_legitimacy kernel.
 *
 * KEY AGENTS:
 *   - high_risk_population: Primary target (powerless/constrained) â bears the targeted coercion legitimized by the actuarial threshold.
 *   - low_risk_population: Primary beneficiary (organized/mobile) â shielded from blanket mandates by the proportionality requirement.
 *   - public_health_authority: Agenda-setter (institutional/analytical) â defines thresholds and enforces targeted mandates, gaining legitimacy.
 *   - constitutional_courts: Analytical observer (institutional/analytical) â adjudicates proportionality and administers the doctrinal boundary.
 *   - public_health_primacy_advocates: Excluded voice (organized/constrained) â argues for broader mandates, structurally sidelined by the median framework.
 *   - bodily_autonomy_advocates: Excluded voice (organized/constrained) â rejects all coercion, sidelined because targeted mandates are permitted.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.55).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy â Risk Stratification Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, 'af806231-3061-486c-8019-52bb22fb2503').
narrative_ontology:cs_kernel_codification('af806231-3061-486c-8019-52bb22fb2503', formalized).
narrative_ontology:cs_authority_grounding('af806231-3061-486c-8019-52bb22fb2503', lineage).
narrative_ontology:cs_interpretation_layer_present('af806231-3061-486c-8019-52bb22fb2503').
narrative_ontology:cs_reading_relation('af806231-3061-486c-8019-52bb22fb2503', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('af806231-3061-486c-8019-52bb22fb2503', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('af806231-3061-486c-8019-52bb22fb2503', foundational, coercion_proportionate_to_actuarial_risk).
narrative_ontology:cs_axiom_status(coercion_proportionate_to_actuarial_risk, holdable).
narrative_ontology:cs_axiom_grounding('af806231-3061-486c-8019-52bb22fb2503', coercion_proportionate_to_actuarial_risk, deontological).
narrative_ontology:cs_axiom('af806231-3061-486c-8019-52bb22fb2503', foundational, high_risk_targeting_satisfies_narrow_tailoring).
narrative_ontology:cs_axiom_status(high_risk_targeting_satisfies_narrow_tailoring, holdable).
narrative_ontology:cs_axiom_grounding('af806231-3061-486c-8019-52bb22fb2503', high_risk_targeting_satisfies_narrow_tailoring, deontological).
narrative_ontology:cs_reference_frame('af806231-3061-486c-8019-52bb22fb2503', constitutional_proportionality_framework).
narrative_ontology:cs_drift_state('af806231-3061-486c-8019-52bb22fb2503', post_pandemic_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('af806231-3061-486c-8019-52bb22fb2503', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_population).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose actuarial risk profile places them above the judicially recognized threshold; they are subject to targeted vaccination mandates and bear the direct cost of bodily coercion, side-effect risk, and compliance burden. Their exit is limited to leaving the jurisdiction or accepting penalties.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_population, payer,
    powerless, biographical, constrained, national).

% Individuals below the actuarial risk threshold who are shielded from blanket mandates by the proportionality requirement. They benefit from the constraint because it removes the threat of coercion that would apply under a public-health-primacy regime.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_population, beneficiary,
    organized, biographical, mobile, national).

% Sets actuarial thresholds, designs targeted mandate regimes, and enforces compliance among high-risk groups. Gains institutional legitimacy from the proportionality framework but is constrained from issuing blanket mandates by judicial review.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate whether mandates meet the proportionality and necessity prongs of constitutional review; they strike down blanket mandates and uphold narrowly tailored ones, administering the risk-stratification doctrine.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Argue that collective harm prevention justifies blanket mandates regardless of individual risk. They are structurally excluded from the doctrinal center because the risk-stratification framework treats their preferred policy as disproportionate per se.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_primacy_advocates, excluded,
    organized, generational, constrained, national).

% Hold that all medical coercion is categorically impermissible. They are excluded because the framework permits targeted mandates, rejecting the absolutist premise while borrowing their critique of blanket overreach.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, bodily_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mediates between the extreme of absolute bodily autonomy and the extreme of unconstrained public health power by providing a proportionality test that permits state coercion only when actuarially targeted to high-risk individuals.
% TRANSFER_FUNCTION: Transfers the burden of vaccination coercion from the general population (as would occur under a blanket mandate) to a defined high-risk subpopulation, while transferring political and legal legitimacy to the state for those narrower interventions.
% ABSENT_VOICES: Public health primacy advocates, who would accept no risk-based ceiling on state coercion, and bodily autonomy advocates, who would reject any state coercion regardless of risk calculus; both are excluded from the median doctrinal position.
% DISAPPEARANCE_RATIONALE: If the risk-stratification framework vanished, blanket mandates would become legally permissible in many jurisdictions, the protected status of low-risk individuals would dissolve, and the standing compromise between public health and autonomy would collapse toward one of the two extremes.
% FOUNDING_PROBLEM: How to legitimate state coercion for vaccination during a public health emergency without violating constitutional proportionality or individual rights protections.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars and civil liberties organizations outside the direct beneficiary set attest that unbounded emergency police power poses a recurring legitimacy problem; judicial review precedents from multiple jurisdictions corroborate proportionality as the doctrinal answer, though the specific threshold remains disputed.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).
:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate because the constraint authorizes coercion only against a targeted subset rather than the entire population. Suppression (0.55) reflects active enforcement of targeted mandates alongside the suppression of blanket mandates. Theater ratio (0.28) captures modest performative actuarialism in threshold selection, but the proportionality analysis remains largely functional. Accessibility collapse (0.65) is significant because once courts adopt this framework, alternative legal framings struggle to gain traction within the jurisdiction. Resistance (0.45) is moderate: the constraint faces opposition from both public-health maximalists and autonomy absolutists. The measurement series shows gradual intensification as courts refined and enforced the threshold logic over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the low-risk seat, the constraint computes as rope or weak tangled rope â it protects against overreach. From the high-risk seat, it computes as tangled rope or snare â they are the ones who pay the cost of the compromise. From the state's seat, it is coordination with manageable enforcement costs. The engine derives this divergence from the same structural facts: beneficiary declarations for low-risk, payer declarations for high-risk, and divergent exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The low_risk_population is the structural beneficiary (d near 0.0) because the proportionality requirement shields them from coercion that a blanket mandate would impose. The high_risk_population is the structural target (d near 1.0) because the same doctrine that protects the low-risk group legitimizes extraction of compliance from them. The public_health_authority and constitutional_courts sit near the symmetric-to-beneficiary range: they gain policy efficacy and institutional legitimacy but do not personally capture the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the proportionality threshold, a blanket mandate would enroll the entire population as victims and would likely compute as a snare (pure extraction under public-health cover). Without any mandate authority, the constraint would dissolve into the bodily-autonomy reading. The risk-stratification reading prevents mandatrophy by ensuring the founding problem â how to legitimate emergency coercion â is answered with narrow tailoring rather than permanent emergency. It is not a piton because the agenda-setter actively benefits from the legitimacy it provides, and it is not a scaffold because it carries no sunset clause.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_threshold_contestability,
    'Does the actuarial risk threshold have an objective epidemiological foundation, or is it constructed by political and judicial choice such that the victim set size is arbitrary?',
    'Cross-jurisdictional comparison of adopted thresholds against independent epidemiological modeling; convergence toward a biological floor supports objectivity, wide political variance supports constructedness.',
    'If the threshold is constructed, the constraint''s extraction is politically determined and the classification edges toward snare; if objective, the constraint tracks a natural risk gradient and edges toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actuarial_threshold_contestability, empirical, 'Whether the risk threshold is epistemically fixed or politically variable.').

omega_variable(
    reading_stability_vs_collapse,
    'Can the risk-stratification reading persist as a stable median position, or will it collapse into public-health primacy or bodily-autonomy absolutism under institutional pressure?',
    'Track judicial precedent across multiple epidemic or emergency cycles; if courts consistently split the difference, the reading is stable; if they revert to blanket permissiveness or categorical bans, it collapses.',
    'If unstable, this constraint is a transient scaffold; if stable, it is a persistent tangled rope mediating between two poles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stability_vs_collapse, conceptual, 'Stability of the median doctrinal position between two extremes.').

omega_variable(
    kernel_reading_identity,
    'Is the risk-stratification constraint a natural implication of proportionality doctrine, or a constructed compromise that benefits electoral majorities who are predominantly low-risk?',
    'Demographic and political-economy analysis of mandate support correlated with risk profile; if low-risk majorities systematically favor the doctrine while high-risk minorities oppose it, the compromise reading is shown to serve a beneficiary structure.',
    'If it serves a low-risk majority beneficiary structure, FSM-style evaluation applies even though the constraint is not claimed as a mountain; the coordination function may be cover for majoritarian extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the median reading is structurally independent or majoritarian cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 6, 0.37).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 36, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vaccine_mandate_legitimacy__risk_stratification_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, bodily_autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the vaccine_mandate_legitimacy kernel, which decomposes into risk_stratification_reading, public_health_primacy_reading, and bodily_autonomy_primacy_reading. Each reading has a distinct epsilon and stakeholder structure; they are linked by the shared kernel but are structurally independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
