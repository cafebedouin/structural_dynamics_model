% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionality Standard for Disease-Specific Coercive Public Health Mandates
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This story instantiates the proportionality reading of the
 *   coercion-legitimacy kernel: the claim that the state's authority to
 *   compel medical intervention scales with a pathogen's severity and
 *   transmission dynamics, so that measles-tier diseases justify mandates
 *   while flu-tier diseases generally do not. This is a distinct constraint
 *   from the public-health-primary reading (which would authorize coercion
 *   whenever collective benefit outweighs individual cost, without a severity
 *   floor) and the bodily-autonomy-primary reading (which forecloses coercion
 *   regardless of collective benefit). Under this reading, the victim set is
 *   pathogen-contingent: whoever objects to a mandate for a disease that
 *   clears the severity/transmission threshold bears the coercion; whoever
 *   objects to a mandate for a disease that does not clear it is not coerced
 *   at all. This produces a moderate, case-by-case ε rather than the high ε
 *   of unconditional public-health-primary coercion or the near-zero ε of the
 *   autonomy-primary alternative (which would authorize essentially no
 *   coercive mandate).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.5).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionality Standard for Disease-Specific Coercive Public Health Mandates").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '1304d98f-9d5e-4f51-b4b0-caf97db36a7d').
narrative_ontology:cs_kernel_codification('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', distributed).
narrative_ontology:cs_authority_grounding('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', expertise).
narrative_ontology:cs_interpretation_layer_present('1304d98f-9d5e-4f51-b4b0-caf97db36a7d').
narrative_ontology:cs_reading_relation('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', coercion_legitimacy_boundary__public_health_primary, influences).
narrative_ontology:cs_reading_relation('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', coercion_legitimacy_boundary__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', foundational, severity_transmission_threshold_governs_coercion).
narrative_ontology:cs_axiom_status(severity_transmission_threshold_governs_coercion, holdable).
narrative_ontology:cs_axiom_grounding('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', severity_transmission_threshold_governs_coercion, empirically_contingent).
narrative_ontology:cs_axiom('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', secondary, categorical_rules_produce_mismatched_coercion).
narrative_ontology:cs_axiom_status(categorical_rules_produce_mismatched_coercion, holdable).
narrative_ontology:cs_axiom_grounding('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', categorical_rules_produce_mismatched_coercion, instrumental).
narrative_ontology:cs_reference_frame('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', jacobson_graduated_police_power_standard).
narrative_ontology:cs_drift_state('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', post_covid19_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1304d98f-9d5e-4f51-b4b0-caf97db36a7d', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, school_age_children).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_departments).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, vaccine_refusing_parents).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, religious_exemption_seekers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, low_severity_disease_mandate_targets).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, graduated_coercion_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, epidemiological_proportionality_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces mandate thresholds by adjudicating R0, case fatality rate, and outbreak trajectory on a per-pathogen basis. Administers school exclusion, quarantine, and licensing consequences for noncompliance. Bears legal and political risk if the line is drawn wrong in either direction — too strict invites litigation over autonomy, too lax invites blame for outbreaks.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_departments, agenda_setter,
    institutional, generational, analytical, national).

% Attend schools where measles-level herd immunity thresholds are enforced through exclusion policies; benefit from reduced circulation of high-R0 pathogens they cannot personally consent to be protected from or exposed to. Have no voice in the threshold-setting process.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, school_age_children, beneficiary,
    powerless, biographical, trapped, regional).

% Rely on population-level immunity thresholds against high-severity, high-transmission diseases for protection they cannot get any other way. The proportionality line drawn for measles-tier pathogens is the margin between exposure risk and safety for this group; a flu-tier threshold would leave them undefended for diseases the standard treats as non-mandate-worthy.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Face exclusion from public school, employment consequences, or fines specifically because their objection falls on the high-severity side of the proportionality line (measles, not flu). Their exit options are homeschooling, private unvaccinated-tolerant enclaves, or relocation to jurisdictions with looser thresholds — none of which are available to everyone equally.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vaccine_refusing_parents, payer,
    moderate, biographical, constrained, regional).

% Hold sincere objections that the proportionality standard treats as outweighed once a pathogen crosses the severity/transmission threshold. Exemption is narrowed or denied specifically as disease severity rises, meaning the same objection is honored for low-severity mandates and overridden for high-severity ones — the coercion they experience is a direct function of which side of the line their pathogen falls on.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, religious_exemption_seekers, payer,
    powerless, biographical, constrained, regional).

% In jurisdictions or periods where the proportionality line is drawn more aggressively (e.g., seasonal flu mandates for healthcare workers), bear coercive consequences the standard's own logic says should not apply outside high-severity pathogens — a boundary-drawing error that falls on them specifically because the line is contested and shifts.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, low_severity_disease_mandate_targets, payer,
    powerless, biographical, mobile, regional).

% Adjudicate where specific pathogens fall on the severity/transmission scale, producing the R0 and case-fatality data and legal tests (Jacobson-derived proportionality tests) that the standard depends on. Their disagreements over borderline pathogens (e.g., pertussis, seasonal influenza variants) are where the standard's case-by-case character becomes visible.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, epidemiologists_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated, pathogen-specific test for when the collective harm-prevention interest is strong enough to override individual bodily autonomy — allowing coercion for measles-tier pathogens while withholding it for flu-tier pathogens, rather than adopting a uniform rule in either direction.
% TRANSFER_FUNCTION: Moves the burden of a categorical yes/no decision on coercion into a case-by-case administrative and judicial determination; shifts autonomy costs onto whichever population happens to be objecting to whichever pathogen currently sits above the threshold, and shifts protection benefits to those who cannot protect themselves (the immunocompromised, infants, the medically exempt) when the threshold is crossed.
% ABSENT_VOICES: Individuals with genuine but hard-to-verify medical contraindications sit awkwardly inside this standard — the proportionality test focuses on the pathogen's severity, not the individual's, and their voice is largely absorbed into the generic exemption process rather than heard on its own terms. Future affected populations (for a not-yet-emerged high-R0 pathogen) have no voice in how the threshold will be applied to them.
% DISAPPEARANCE_RATIONALE: Public health departments and immunocompromised advocates would say the world rearranges badly — coercion would either vanish entirely (autonomy-primary outcome, endangering vulnerable populations) or apply uniformly regardless of severity (public-health-primary outcome, over-coercing for low-stakes pathogens). Autonomy advocates would say the world becomes more consistent and predictable rather than rearranging destructively, since the discretion itself is what they object to.
% FOUNDING_PROBLEM: Categorical rules (mandate everything communicable, or mandate nothing without full individual consent) both produced clearly wrong outcomes: the former justified coercion for low-stakes conditions, the latter left high-mortality, high-transmission outbreaks unchecked. The proportionality standard was built to solve the mismatch between disease severity and the intensity of state response.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and constitutional scholars outside the public health administrative apparatus (e.g., Jacobson v. Massachusetts commentary, subsequent case law on Zucht v. King and Prince v. Massachusetts) attest that courts have continued to require severity-and-transmission-specific justification rather than either categorical extreme, indicating the underlying line-drawing problem remains unresolved and actively litigated rather than settled by fiat.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, contested).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).
:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because coercion is bounded by disease severity rather than applied universally — the standard's own logic exempts a large class of objectors (flu-tier). Suppression (0.5) reflects real enforcement machinery (school exclusion, occupational mandates, fines) that becomes active whenever a pathogen crosses the threshold, but is inactive otherwise, so the average suppressive force across the pathogen space is middling rather than severe. Theater ratio is comparatively low (0.28) because the severity/transmission adjudication is substantively evidentiary (R0, case-fatality data, outbreak modeling) rather than performative, though it rises slightly over the measured interval as borderline-pathogen litigation accumulates procedural overhead without resolving the underlying line.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat, the standard looks like principled restraint — a rule that specifically refuses to coerce over low-stakes disease. From the payer seats whose particular pathogen crosses the threshold, the same standard looks indistinguishable in its coercive effect from unconditional public-health-primary coercion; proportionality is invisible to the person currently excluded from school for refusing a measles vaccine, even though it is the operative fact from the administrator's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health departments occupy the agenda-setting seat: they administer and could redraw the threshold, and bear institutional risk from getting it wrong in either direction. Immunocompromised populations and school-age children are beneficiaries with no exit and no voice — they receive the protective effect of a mandate they did not ask for and cannot decline on the other side. Vaccine-refusing parents, religious exemption seekers, and low-severity mandate targets are victims precisely because their objection falls on the coerced side of a threshold set by someone else's epidemiological judgment; their directionality is high not because the standard is universally coercive but because THEIR specific pathogen classification puts them inside the mandate zone.
 *
 * MANDATROPHY ANALYSIS:
 *   The standard resists mandatrophy in one direction (it does not let low-stakes disease mandates persist merely because coercive infrastructure already exists — flu policy is not treated the same as measles policy) but is vulnerable to it in another: if a pathogen's severity declines over time (e.g., through prior immunization campaigns) while mandate infrastructure and enforcement habits remain, the proportionality standard requires an active downward recalibration that institutions often resist making, since removing a mandate is politically costlier than adding one. This is why suppression_requirement rises modestly over the interval even as no new high-severity threats are declared: enforcement infrastructure ratchets upward via accumulated caution rather than declining alongside receding threat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_placement_authority,
    'Who has legitimate authority to set the severity/transmission threshold that separates mandate-justifying pathogens from non-mandate-justifying ones, and by what evidentiary standard?',
    'Comparative analysis of jurisdictions'' threshold-setting processes (legislative statute vs. agency rulemaking vs. judicial common-law development) and whether outcomes converge across independent processes.',
    'If threshold placement is substantially discretionary and outcome-variable across similarly-situated jurisdictions, the proportionality reading functions less as a principled standard and more as a discretionary coercion license dressed in epidemiological language, pushing the classification toward tangled_rope or snare depending on how discretion is exercised. If thresholds converge tightly on independent epidemiological grounds, the reading functions closer to a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_placement_authority, conceptual, 'Whether threshold-setting is principled adjudication or disguised discretion.').

omega_variable(
    borderline_pathogen_classification_stability,
    'For pathogens that sit near the severity/transmission threshold (e.g., pertussis, seasonal high-virulence influenza strains, novel respiratory viruses early in an outbreak), does the standard produce stable, predictable classifications or does it oscillate with political and media salience rather than epidemiological data?',
    'Track classification decisions and their stated justifications across multiple outbreak events for the same or similar pathogens over time; measure correlation with case-fatality/R0 data versus correlation with media attention or political cycle.',
    'If classification is data-stable, the proportionality reading is descriptively accurate to its own claim. If classification tracks salience rather than data, the reading''s moderate ε understates actual extraction during high-salience periods when coercion is applied to pathogens that would not clear the threshold under calmer analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(borderline_pathogen_classification_stability, empirical, 'Whether pathogen classification tracks epidemiology or political salience.').

omega_variable(
    kernel_framing_under_determination,
    'Is the proportionality reading a genuinely distinct third position, or is it a compromise formulation that collapses into public_health_primary in practice whenever public health authorities control threshold-setting (since they can simply declare more pathogens ''severe enough'')?',
    'Examine whether the proportionality standard has ever produced a documented case where a pathogen initially classified as mandate-justifying was later reclassified downward and mandates removed, versus only ever expanding the mandate-justifying set.',
    'If the classification only ratchets toward more mandates over time, the proportionality reading is not structurally distinct from public_health_primary — it merely delays and legitimizes the same expansion. If genuine downward reclassifications occur, the reading is a real third position with its own distinct ε and victim set, as authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the proportionality reading is structurally distinct from public_health_primary or collapses into it under administrative control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(coer_tr_t32, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(coer_be_t32, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(coer_su_t32, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 32, 0.49).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the coercion_legitimacy_boundary kernel. public_health_primary authorizes coercion whenever collective benefit outweighs individual cost, with no severity floor (higher ε, broader victim set). bodily_autonomy_primary forecloses coercion categorically regardless of collective benefit (ε near zero, no victims by its own lights). This proportionality_reading occupies the middle: moderate ε, pathogen-contingent victim set, and a case-by-case adjudicative structure that the other two readings treat as either insufficiently protective (public_health_primary) or already illegitimate in its coercive cases (bodily_autonomy_primary).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
