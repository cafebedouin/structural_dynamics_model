% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality Doctrine for Legitimate Health Intervention
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   This story instantiates the proportionality reading of the
 *   legitimate_health_intervention kernel: legitimacy is a function of the
 *   fit between intervention severity and disease-specific threat
 *   characteristics (transmissibility, case-fatality rate), rather than being
 *   anchored solely in population-level outcomes (public_health_primary) or
 *   solely in individual consent (bodily_autonomy_primary). The doctrine's
 *   victim set is conditional on disease severity by design — a measles
 *   outbreak generates a different proportionality calculus, and a different
 *   victim set, than a seasonal flu season. ε is authored for the standing
 *   arrangement as it operates: a real coordination function (preventing both
 *   regulatory neglect and overreach) riding alongside a real extraction
 *   function (authorities capture substantial discretion in setting the
 *   severity thresholds that determine who bears the restriction cost).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.42).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.48).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality Doctrine for Legitimate Health Intervention").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '784adae8-ebc0-4acb-8359-d8b2123d0dce').
narrative_ontology:cs_kernel_codification('784adae8-ebc0-4acb-8359-d8b2123d0dce', distributed).
narrative_ontology:cs_authority_grounding('784adae8-ebc0-4acb-8359-d8b2123d0dce', distributed).
narrative_ontology:cs_reading_relation('784adae8-ebc0-4acb-8359-d8b2123d0dce', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('784adae8-ebc0-4acb-8359-d8b2123d0dce', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('784adae8-ebc0-4acb-8359-d8b2123d0dce', foundational, severity_weighted_dual_value_balancing).
narrative_ontology:cs_axiom_status(severity_weighted_dual_value_balancing, holdable).
narrative_ontology:cs_axiom_grounding('784adae8-ebc0-4acb-8359-d8b2123d0dce', severity_weighted_dual_value_balancing, instrumental).
narrative_ontology:cs_axiom('784adae8-ebc0-4acb-8359-d8b2123d0dce', secondary, disease_characteristics_determine_intervention_ceiling).
narrative_ontology:cs_axiom_status(disease_characteristics_determine_intervention_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('784adae8-ebc0-4acb-8359-d8b2123d0dce', disease_characteristics_determine_intervention_ceiling, empirically_contingent).
narrative_ontology:cs_reference_frame('784adae8-ebc0-4acb-8359-d8b2123d0dce', graduated_proportionality_review_standard).
narrative_ontology:cs_drift_state('784adae8-ebc0-4acb-8359-d8b2123d0dce', post_covid19_mandate_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('784adae8-ebc0-4acb-8359-d8b2123d0dce', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, high_risk_population_cohorts).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, low_severity_disease_refusers).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, civil_liberties_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, moderate_risk_general_public).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, moderate_risk_general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and calibrate the proportionality test itself — deciding what counts as sufficient transmissibility and case-fatality rate to justify a given intervention severity. They administer quarantine, mandate, and closure powers under this doctrine and are the ones whose authority the doctrine legitimizes when courts apply it deferentially.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, public_health_authorities, beneficiary).

% Elderly, immunocompromised, and other high-severity-exposure groups whose morbidity/mortality risk justifies stronger intervention under the disease-weighting logic. They receive protective benefit from interventions calibrated upward for high-transmissibility, high-fatality diseases like measles, without bearing the intervention's restrictive costs directly.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, high_risk_population_cohorts, beneficiary,
    moderate, immediate, constrained, regional).

% Individuals who decline an intervention for a low-severity disease (e.g. seasonal flu) and are nonetheless swept into interventions calibrated for higher-severity conditions, or who face intervention regimes justified by aggregate risk categories that do not match their individual exposure. The proportionality test's disease-level weighting can override their individualized risk profile.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, low_severity_disease_refusers, payer,
    moderate, biographical, constrained, regional).

% Advocacy organizations and individual plaintiffs who challenge specific interventions as disproportionate to actual threat level. They must litigate case-by-case against a doctrine that grants substantial deference to public health authorities' own severity assessments, bearing legal costs and delay while the intervention remains in force pending review.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, civil_liberties_litigants, payer,
    organized, biographical, constrained, national).

% Apply the proportionality test to specific interventions, weighing transmissibility, case-fatality rate, and severity of restriction against alternatives. Their rulings determine, case by case, whether the doctrine's balance has been honestly struck or has drifted toward rubber-stamping authority claims.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, courts_and_reviewing_bodies, observer,
    institutional, generational, analytical, national).

% The bulk of the population subject to whatever intervention the proportionality calculus produces for a given outbreak. They benefit from reduced transmission when the calibration is accurate, and bear restriction costs (closures, mandates, mobility limits) scaled to the authorities' severity assessment, which they have limited capacity to independently verify in real time.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, moderate_risk_general_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, moderate_risk_general_public, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared decision rule for calibrating intervention severity to disease characteristics, so that measles-level threats and flu-level threats are not treated identically — coordinating population protection with individual liberty by making the tradeoff itself contestable and reviewable rather than absolute in either direction.
% TRANSFER_FUNCTION: Moves discretionary authority to public health bodies to set severity thresholds, and moves the burden of contesting a specific application onto the individuals or groups who believe the calibration is wrong for their case — shifting the cost of proving disproportion onto the party restricted.
% ABSENT_VOICES: Individuals whose personal risk profile diverges sharply from their disease-category's aggregate statistics (e.g., a low-risk individual within a high-severity outbreak, or a high-risk individual within a low-severity one) have no seat in a doctrine that weights by disease characteristics rather than individualized risk; they are folded into whichever cohort the disease-level analysis assigns them to.
% DISAPPEARANCE_RATIONALE: Public health authorities and reviewing courts would say a great deal rearranges: without a proportionality anchor, intervention severity would either default to maximal population-protective measures (public_health_primary reading) or collapse toward near-total deference to individual refusal (bodily_autonomy_primary reading), with no doctrinal mechanism to prevent either extreme. Civil liberties litigants might say the doctrine mainly formalizes a deference structure that would persist under another name even if this specific test vanished.
% FOUNDING_PROBLEM: Courts and legislatures needed a workable standard to prevent both underreaction to genuinely dangerous outbreaks and overreaction that treats every disease as though it were a mass-casualty threat, following historical episodes of both regulatory neglect and disproportionate quarantine/mandate overreach.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and public health law historians outside any single health authority attest the proportionality problem remains live — cited in judicial opinions evaluating COVID-era mandates against SARS-era and pre-vaccine measles-era interventions. Civil liberties organizations corroborate that the underlying tension (aggregate risk vs. individual autonomy) has not been resolved, though they dispute whether the doctrine as applied actually achieves proportionality or merely provides cover for authority deference.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, contested).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).
:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rises during high-severity outbreak periods (measles-type events push suppression_requirement to 0.6 at t=12) then partially recedes as outbreak conditions ease — this is not a monotonic drift but reflects the doctrine's conditional structure: its extractive bite scales with the disease characteristics it is applied to, exactly as the kernel reading specifies. Theater ratio stays comparatively low (0.22) because the proportionality test does correspond to real epidemiological variables being weighed, not pure performance, though the discretion in applying weights allows some performative overreach during high-visibility outbreaks. Suppression tracks the same conditional curve as extractiveness because the same disease-severity variable drives both.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities set and administer the proportionality test and are structurally closest to the beneficiary end — the doctrine legitimizes and protects their discretionary calibration. High-risk cohorts benefit from correctly calibrated interventions without bearing their cost. Low-severity disease refusers and civil liberties litigants sit near the target end: the disease-weighting logic can subordinate their individualized risk profile to an aggregate disease-category judgment they did not choose and have limited power to contest in real time. The moderate_risk_general_public is genuinely mixed — real coordination benefit from proportionate response, real cost from restrictions scaled to a severity assessment they cannot independently verify.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine is not obsolete in the R5 sense — the founding problem (preventing both neglect and overreach) remains live and is corroborated by scholars outside the benefiting institutions. But mandatrophy risk exists at the margins: once a severity threshold is set for a disease category, the discretion to revisit that threshold as real-world transmissibility or fatality data update can lag, so an outbreak initially calibrated as high-severity can retain intervention intensity after the empirical picture moderates — this is exactly the metric substitution the theater_ratio and suppression_requirement series are tracking around t=12 to t=16.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_calibration_authorship,
    'Who actually sets the severity thresholds inside the proportionality test — an independent epidemiological process, or the same public health authorities whose interventions the test is meant to constrain?',
    'Institutional analysis of the threshold-setting process: is transmissibility/case-fatality-rate weighting produced by a body structurally separate from the enforcing authority, with adversarial input from civil liberties representation, or is it set unilaterally by the same agency administering the intervention?',
    'If the same authority sets both the threshold and the intervention, the proportionality test functions partly as self-certification, pushing the doctrine toward tangled_rope or even snare at the administrative seat; if threshold-setting is genuinely independent and contestable, the doctrine functions closer to a rope with real coordination benefit and minimal capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calibration_authorship, empirical, 'Whether the severity-weighting mechanism is independently set or self-certified by the enforcing authority.').

omega_variable(
    kernel_reading_divergence_location,
    'Where exactly does the proportionality reading''s victim set diverge from the public_health_primary and bodily_autonomy_primary readings'' victim sets, and which disease-severity band produces the sharpest divergence?',
    'Compare victim sets across the three sibling constraints for a matched set of historical outbreaks (e.g. measles resurgence, seasonal flu, a moderate-transmissibility/moderate-fatality pathogen) to identify the severity band where the three readings prescribe materially different interventions and different victims.',
    'Establishes the committer-frame structural fact that the proportionality reading is not merely a compromise position but instantiates a genuinely conditional constraint structure — ε and victim set are functions of disease characteristics, which is exactly the structural delta distinguishing this reading from its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_location, conceptual, 'Locating the disease-severity band where the three kernel readings diverge most sharply in prescribed victims.').

omega_variable(
    individualized_vs_aggregate_risk_mismatch,
    'How large is the gap between disease-level aggregate risk weighting and individual-level actual risk, and does that gap constitute a distinct suppressed voice or merely acceptable approximation error?',
    'Epidemiological analysis of within-cohort risk variance for diseases the doctrine treats as uniform severity classes; compare against the doctrine''s stated individual-autonomy weighting commitment.',
    'A large mismatch would suggest the doctrine''s claimed weighting of individual autonomy is substantially theatrical relative to disease-level aggregation, supporting a higher theater_ratio and a partial reclassification toward extraction at the individual-refuser seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individualized_vs_aggregate_risk_mismatch, empirical, 'Whether disease-level severity weighting adequately approximates individual risk or systematically misclassifies atypical individuals within a cohort.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__proportionality_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__proportionality_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__proportionality_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__proportionality_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__proportionality_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__proportionality_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__proportionality_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__proportionality_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__proportionality_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__proportionality_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__proportionality_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__proportionality_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__proportionality_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__proportionality_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__proportionality_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__proportionality_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__proportionality_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__proportionality_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimate_health_intervention kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. public_health_primary anchors legitimacy in measurable population-level outcome reduction alone; bodily_autonomy_primary anchors it in informed consent regardless of public benefit; this proportionality_reading makes legitimacy conditional on the fit between intervention severity and disease-specific threat characteristics, producing a victim set and ε that vary by disease transmissibility/case-fatality rate rather than being fixed. The three stories are linked bidirectionally in network.affects_constraints as members of one constraint family; each carries its own claimed_type, metrics, and stakeholder set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
