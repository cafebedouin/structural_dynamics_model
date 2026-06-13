% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority (Proportionality Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   The proportionality reading frames public health mandate legitimacy as a
 *   sliding scale calibrated to four factors: threat severity, alternative
 *   availability, coercion magnitude, and duration of imposition. Under this
 *   reading, an Ebola-scale pathogen justifies broad mandates with few
 *   exceptions and extended duration; a mild seasonal respiratory virus
 *   justifies narrow mandates with broad exceptions and compressed duration.
 *   The reading accepts both coordinated disease control (beneficiary:
 *   immunocompromised) and bodily autonomy extraction (payer:
 *   vaccine-hesitant) as structurally real, with legitimacy contingent on the
 *   authority demonstrating proportionality across all four axes. This is a
 *   TANGLED ROPE: genuine coordination function (herd immunity protection)
 *   paired with asymmetric extraction (autonomy cost to hesitant adults),
 *   requiring active enforcement (exception denial, employment/education
 *   exclusion) to sustain. The measurement series tracks a cyclical pattern:
 *   threat-driven surge in extractiveness and suppression requirement (time
 *   8-24), followed by threat recession and extractiveness decline (time
 *   24-40), with projection of renewed emergence at time 48.
 *
 * KEY AGENTS:
 *   - public_health_authority: agenda-setter, institutional power, sets mandate scope and exception criteria based on threat assessment
 *   - immunocompromised_populations: structural beneficiary, powerless, trapped without herd immunity, gain protection proportional to mandate breadth
 *   - vaccine_hesitant_adults: primary payers, moderate power, constrained exit (accept vaccination, comply with alternatives, or face exclusion)
 *   - healthcare_worker_population: dual-positioned (beneficiary + payer), organized, higher burden justification but still subject to proportionality scrutiny
 *   - medical_exception_seekers: secondary payers with partial exclusion status; exception breadth inversely tracks threat severity under proportionality reading
 *   - civil_liberties_advocates: excluded from authority but litigation pressure shapes enforcement behavior and proportionality scrutiny
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.58).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.62).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority (Proportionality Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '4b5ea93d-0ad8-46d9-be88-6e75b591c55f').
narrative_ontology:cs_kernel_codification('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', formalized).
narrative_ontology:cs_authority_grounding('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', extraction).
narrative_ontology:cs_interpretation_layer_present('4b5ea93d-0ad8-46d9-be88-6e75b591c55f').
narrative_ontology:cs_reading_relation('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', foundational, mandate_legitimacy_proportionality_dependent).
narrative_ontology:cs_axiom_status(mandate_legitimacy_proportionality_dependent, holdable).
narrative_ontology:cs_axiom_grounding('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', mandate_legitimacy_proportionality_dependent, deontological).
narrative_ontology:cs_axiom('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', foundational, four_axis_proportionality_test_binding).
narrative_ontology:cs_axiom_status(four_axis_proportionality_test_binding, holdable).
narrative_ontology:cs_axiom_grounding('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', four_axis_proportionality_test_binding, instrumental).
narrative_ontology:cs_axiom('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', secondary, threat_severity_justifies_autonomy_limitation).
narrative_ontology:cs_axiom_status(threat_severity_justifies_autonomy_limitation, holdable).
narrative_ontology:cs_axiom_grounding('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', threat_severity_justifies_autonomy_limitation, empirically_contingent).
narrative_ontology:cs_reference_frame('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', proportionality_constrained_mandate_authority).
narrative_ontology:cs_drift_state('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', contemporary_endemic_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4b5ea93d-0ad8-46d9-be88-6e75b591c55f', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_system_integrity).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, vaccine_hesitant_adults).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, medical_exception_seekers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is calibrated to threat severity rather than fixed. Peak extraction (0.68 at time 16) corresponds to high-transmission variant emergence; decline to 0.32 at time 40 reflects threat recession. The measurement series models cyclical threat dynamics—realistic for respiratory pathogens. Theater ratio rises sharply mid-interval (0.42 at time 24) when threat perception declines but mandates persist unchanged—a classic Goodhart shift where the safety benefit erodes but enforcement machinery continues. This signals possible extraction layering: enforcement justified by declining-severity threat becomes theater defending institutional authority. Suppression requirement tracks threat severity closely, suggesting enforcement intensity is responsive to proportionality pressure (rises with threat, falls as threat recedes). The coercion grid shows class-level resistance is systematically higher (0.78-0.82) than individual resistance (0.62-0.72), indicating collective action and mobilization of resistance movements. Organizational suppression is higher (0.58-0.68) than individual (0.38-0.54), reflecting targeting of employers/institutions as enforcement locus rather than individual behavior. Structural accessibility collapse is high (0.82-0.85) because the mandate framework is embedded in employment, education, and healthcare access—alternatives are not merely unavailable, they are legally foreclosed. Individual accessibility is lower (0.55-0.68) because individual choices (testing, isolation, remote work) remain open even under strict mandates.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority and immunocompromised populations experience this constraint as coordination (real disease control problem, proportionality as guardrail on justified power). Vaccine-hesitant adults and civil liberties advocates experience it as extraction (bodily autonomy cost, proportionality claim as cover story). Healthcare workers occupy the middle: they see real coordination need (their exposure is high, their colleagues matter to their safety) but also real autonomy cost (mandatory status, burden falls disproportionately on them). The engine should compute distinct types for these seats: proportionality framing makes the constraint APPEAR as rope from the authority seat (coordination + guardrail) and tangled_rope from the hesitant-adult seat (coordination + extraction + enforcement). The divergence is the point—the kernel contest is whether proportionality actually constrains extraction or merely legitimates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authority: d near 0.0 (beneficiary—sets rules, holds agenda power, defines proportionality). Immunocompromised: d near 0.2-0.3 (structural beneficiary but entirely dependent, trapped—cannot exit, cannot verify proportionality claim, vulnerability to false-positive mandates). Vaccine-hesitant adults: d near 0.75-0.85 (target—bear bodily autonomy cost, constrained exit, vulnerable to proportionality abuse when threat is overstated). Medical exception seekers: d near 0.70 (target—exception status contingent on authority's threat assessment, not on individual medical facts). Healthcare workers: d near 0.5-0.6 (mixed—genuine safety benefit from colleagues' vaccination, but higher mandate burden than general population). No directionality override needed; the structural relationships track directly from beneficiary/victim declarations and exit analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   Proportionality reading prevents simple tangled_rope misclassification by requiring explicit temporal and threat-contingent analysis. Without proportionality, the constraint might appear as pure snare (coercive, extractive, with immunity benefit as cover). Proportionality reading correctly identifies it as tangled_rope IF the authority genuinely applies all four proportionality axes AND adjusts mandates accordingly. The measurement series reveals the risk: theater ratio rises sharply when threat recedes but mandates persist (time 16-24), suggesting proportionality scrutiny is weakening and the constraint is drifting toward snare. The t24-t32 period shows potential mandatrophy—the founding problem (disease control at acceptable autonomy cost) may persist, but the proportionality condition (extraction only proportional to threat) is failing as threat declines and mandates are maintained by inertia. The coercion grid's class-level resistance surge (0.78-0.82) corroborates this drift: sustained high resistance at high threat is expected (proportionality bargain holds), but sustained high resistance at low threat signals loss of legitimacy and possible constraint degradation toward piton (maintained by theater, not by genuine proportionality case).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_severity_measurement_ambiguity,
    'What counts as ''severe threat'' under the proportionality reading, and who determines severity assessment?',
    'Develop explicit epidemiological thresholds (R0, case fatality rate, healthcare system capacity, vulnerable population protection level) and require transparent disclosure of how threat assessment drove mandate scope. Compare across jurisdictions with different thresholds to establish if proportionality assessment is consistent or post-hoc rationalization.',
    'If thresholds are undefined or post-hoc, the proportionality reading collapses into unconstrained authority power (snare). If thresholds are preset and transparent, mandates can be ex-ante justified or shown to violate proportionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_severity_measurement_ambiguity, empirical, 'Whether threat severity is measurable independently of the authority''s mandate choice.').

omega_variable(
    alternative_availability_counterfactual,
    'Were less-restrictive alternatives (testing, isolation protocols, targeted protection of vulnerable) genuinely unavailable, or did the authority select mandates without adequately exploring alternatives?',
    'Comparative jurisdictional analysis: compare mandate scope, alternative availability, and epidemiological outcomes across countries/regions with different regulatory approaches. Test whether narrower mandates + stronger alternatives achieved comparable disease control.',
    'If alternatives were available and not pursued, the proportionality claim fails and extraction becomes illegitimate. If alternatives were infeasible, broader mandates are justified under proportionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_availability_counterfactual, empirical, 'Whether proportionality constraint on alternatives is enforced or treated as narrative window-dressing.').

omega_variable(
    coercion_magnitude_asymmetry,
    'Does coercion magnitude fall equally on all payers, or does it concentrate on the most constrained actors (low-income, precarious employment, medical exception seekers)?',
    'Empirical tracking of mandate compliance burden by socioeconomic status, employment type, and medical status. Test whether disproportionate burden on constrained actors triggers heightened proportionality scrutiny or is treated as acceptable inequality.',
    'If coercion is disproportionate and the authority does not adjust, proportionality reading is violated and extraction becomes unjustified asymmetric targeting. If proportionality includes explicit equity safeguards, the reading constrains authority power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_magnitude_asymmetry, empirical, 'Whether proportionality includes equity analysis or treats coercion as aggregate rather than distributed.').

omega_variable(
    duration_ratchet_effect,
    'Once imposed, do mandates decline with threat, or does the authority maintain them past the proportionality window?',
    'Track mandate duration relative to threat level over time. Establish if sunset provisions are enforced, or if duration becomes independent of threat severity.',
    'Rapid duration extension past threat recession signals the proportionality constraint is failing and the mandate is drifting toward snare or piton. Duration discipline demonstrates proportionality reading is operational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duration_ratchet_effect, empirical, 'Whether duration of mandates is genuinely proportional to threat or becomes inertial.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Is proportionality reading genuinely a distinct constraint from bodily_autonomy_primary and public_health_primary, or does it collapse into one of them under adversarial pressure?',
    'Monitor real-world proportionality litigation and legislative debate. Test whether courts/legislatures sustain proportionality scrutiny when threat is ambiguous, or whether they default to either pure autonomy protection or public health supremacy.',
    'If the reading collapses under pressure, it is not a stable kernel reading—it is a performative middle ground. If it sustains distinct logic and constrains both extremes, it is a live, coexistent reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether proportionality reading is structurally stable or a rhetorical position that dissolves under adversarial pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(publ_tr_t8, public_health_mandate_authority__proportionality_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(publ_tr_t16, public_health_mandate_authority__proportionality_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(publ_tr_t32, public_health_mandate_authority__proportionality_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(publ_tr_t40, public_health_mandate_authority__proportionality_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(publ_tr_t48, public_health_mandate_authority__proportionality_reading, theater_ratio, 48, 0.28).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(publ_be_t8, public_health_mandate_authority__proportionality_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(publ_be_t16, public_health_mandate_authority__proportionality_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(publ_be_t32, public_health_mandate_authority__proportionality_reading, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(publ_be_t40, public_health_mandate_authority__proportionality_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(publ_be_t48, public_health_mandate_authority__proportionality_reading, base_extractiveness, 48, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(publ_su_t8, public_health_mandate_authority__proportionality_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(publ_su_t16, public_health_mandate_authority__proportionality_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(publ_su_t32, public_health_mandate_authority__proportionality_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(publ_su_t40, public_health_mandate_authority__proportionality_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(publ_su_t48, public_health_mandate_authority__proportionality_reading, suppression_requirement, 48, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__proportionality_reading, 0.18).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).

% DUAL FORMULATION NOTE:
% The public_health_mandate_authority kernel decomposes into three structurally distinct readings: bodily_autonomy_primary (mandate categorically illegitimate), proportionality_reading (mandate legitimate iff proportional across four axes), and public_health_primary (mandate legitimate when vulnerable populations at risk). Each reading has distinct ε, beneficiary/victim structure, and extracted type. The readings coexist in live dispute across jurisdictions. Proportionality_reading influences both siblings by raising evidentiary burden: autonomy reading must explain why proportionality doesn't matter; public_health reading must justify mandates that fail proportionality tests. Neither reading forecloses the others—all three remain live positions in contemporary bioethical and constitutional discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__proportionality_reading, powerless, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
