% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Legitimate Health Intervention: Proportionality Reading
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The proportionality reading of legitimate health intervention grounds
 *   public authority to mandate medical interventions (vaccination,
 *   quarantine, treatment) in a framework that weighs both population-level
 *   harm and individual autonomy, with weight determined by disease
 *   characteristics (transmissibility, severity, case-fatality rate). This
 *   reading contests two alternative kernels: the bodily_autonomy_primary
 *   reading, which treats informed consent as inviolable regardless of public
 *   benefit, and the public_health_primary reading, which derives legitimacy
 *   solely from measurable reduction in population-level morbidity and
 *   mortality. The proportionality reading is instantiated in constitutional
 *   frameworks (EU Charter Article 52, Canadian Charter Section 1, many state
 *   public health statutes) and serves as the dominant judicial language in
 *   democracies for health intervention justification. However,
 *   proportionality is structurally distinct from both autonomy and public
 *   health frameworks: it creates a tangled_rope constraint because it
 *   requires both genuine coordination (balancing competing legitimate
 *   interests) AND asymmetric extraction (the burden of justifying
 *   intervention intensity falls disproportionately on the target population,
 *   not the authority). The victim set varies by disease severity: for
 *   endemic low-severity diseases, the autonomy-bearing population
 *   experiences high suppression (trapped by mandatory vaccination). For
 *   acute high-severity diseases, the autonomy-bearing population experiences
 *   lower relative suppression (proportionality allows exemptions when threat
 *   is low). The constraint's theater increases across the measurement
 *   interval as public confidence in threat assessment declines and
 *   proportionality language is invoked to legitimize increasingly severe
 *   interventions.
 *
 * KEY AGENTS:
 *   - Unvaccinated Refuser: Primary victim (powerless/trapped) — faces mandatory vaccination or exclusion from public life; bears full cost of non-compliance with zero exit option at immediate horizon
 *   - Vaccine Hesitant: Secondary victim (powerless/constrained) — faces employment and school exclusion (high-cost but surmountable barriers); experiences both coordination benefit (protected if vaccinated) and extraction (suppression of choice)
 *   - Hesitant Parent: Tertiary actor (moderate/mobile) — can relocate to exemption jurisdictions, delay vaccination, choose alternative schooling; experiences proportionality as genuine coordination at generational scale
 *   - Public Health Authority: Primary beneficiary (institutional/arbitrage) — mandates proportionality framework; benefits from legitimate language that grounds intervention authority; experiences the constraint as pure coordination
 *   - Medical Professional: Secondary beneficiary (powerful/constrained) — gains authority from proportionality framework but constrained by liability burden of proportionality assessment
 *   - Analytical Observer: Meta-observer (analytical/analytical) — risks naturalizing proportionality as immutable justice principle rather than recognizing it as one contested reading among three
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.38).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.48).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Legitimate Health Intervention: Proportionality Reading").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, 'b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f').
narrative_ontology:cs_kernel_codification('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', formalized).
narrative_ontology:cs_authority_grounding('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', expertise).
narrative_ontology:cs_interpretation_layer_present('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f').
narrative_ontology:cs_reading_relation('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', foundational, intervention_intensity_must_scale_with_threat_severity).
narrative_ontology:cs_axiom_status(intervention_intensity_must_scale_with_threat_severity, holdable).
narrative_ontology:cs_axiom_grounding('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', intervention_intensity_must_scale_with_threat_severity, deontological).
narrative_ontology:cs_axiom('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', foundational, autonomy_weight_non_zero_and_disease_dependent).
narrative_ontology:cs_axiom_status(autonomy_weight_non_zero_and_disease_dependent, holdable).
narrative_ontology:cs_axiom_grounding('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', autonomy_weight_non_zero_and_disease_dependent, deontological).
narrative_ontology:cs_reference_frame('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', proportionate_intervention_balance).
narrative_ontology:cs_drift_state('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', contemporary_pandemic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b1f99ef4-df90-4a3b-b6e7-b9ebd9be1e9f', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, susceptible_population).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, healthcare_system).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, vaccine_refusers).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, autonomy_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED REFUSER (SNARE) — Immediate horizon, no exit option. Faces mandatory vaccination or exclusion from public spaces, employment, education. The refuser perceives extraction with minimal coordination benefit — the intervention is imposed regardless of consent. High suppression (legal mandate, social exclusion) creates trapped exit status. Classifies as Snare from this perspective: χ driven by high f(d) from victim status + trapped exit.
constraint_indexing:constraint_classification(legitimate_health_intervention__proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VACCINE HESITANT (TANGLED ROPE) — Biographical horizon, constrained exit. Faces employment loss, school exclusion, social stigma — high-cost but surmountable barriers. At biographical scale, the proportionality reading creates genuine coordination: the hesitant agent benefits from the population protection if they choose vaccination, and the constraint coordinates between collective safety and individual choice. However, asymmetric extraction persists: the cost of non-compliance (job loss, exclusion) falls disproportionately on the hesitant. Both coordination function (safety) and asymmetric extraction (enforcement) are present. Classifies as Tangled Rope.
constraint_indexing:constraint_classification(legitimate_health_intervention__proportionality_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HESITANT PARENT (ROPE) — Generational horizon, mobile exit (can relocate to jurisdiction with exemptions, delay vaccination, choose alternative schools). The proportionality constraint is experienced as coordination: the parent benefits from the collective protection while retaining meaningful choice across a generational timeline. The constraint coordinates between safety and autonomy without heavy suppression at this horizon and exit level. Classifies as Rope: coordination dominates, extraction is minimal because exit options are real.
constraint_indexing:constraint_classification(legitimate_health_intervention__proportionality_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY (ROPE) — Institutional power, immediate horizon, arbitrage exit (can shift vaccination policy, adjust exemption rates, reallocate enforcement). Experiences the proportionality constraint as pure coordination: the authority's mandate is to balance population safety and autonomy, which is exactly what proportionality operationalizes. No meaningful extraction experienced by the authority — it is the beneficiary of the coordination mechanism. Classifies as Rope: the constraint solves the authority's core coordination problem.
constraint_indexing:constraint_classification(legitimate_health_intervention__proportionality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL PROFESSIONAL (TANGLED ROPE) — Powerful institutional actor (physician, bioethicist), biographical horizon, constrained exit (career risk of violating licensing standards, liability exposure). The proportionality reading creates coordination (the professional benefits from a legitimate framework that grounds their authority to recommend interventions), but also enforces asymmetric extraction: the professional must navigate the constraint's demand for proportionality assessment, which increases liability burden if assessment is deemed inadequate. The professional's power is constrained by the proportionality requirement itself — genuine coordination with embedded extraction. Classifies as Tangled Rope.
constraint_indexing:constraint_classification(legitimate_health_intervention__proportionality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — Civilizational horizon, universal scope. From a natural law perspective, proportionality is an inherent requirement of legitimate coercion: any intervention that exceeds the minimum necessary to address the threat is, by definition, illegitimate. This perspective treats proportionality as an immutable principle of justice itself. However, the structural data reveals this as a false summit: proportionality is contested by the other readings (bodily_autonomy_primary forecloses proportionality by denying any coercion is legitimate; public_health_primary forecloses proportionality by making population benefit the sole legitimacy criterion). The 'natural law' reading naturalizes one contestable framework among three.
constraint_indexing:constraint_classification(legitimate_health_intervention__proportionality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimate_health_intervention__proportionality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimate_health_intervention__proportionality_reading, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The proportionality reading generates extraction through asymmetric burden: the target population (refusers, hesitant) must justify non-compliance against the authority's (minimal) burden to justify intervention intensity. At low-threat scenarios (endemic flu), this asymmetry is visible and constraints are light (ε ≈ 0.22). At high-threat scenarios (pandemic with novel lethal variant), the proportionality framework allows suppression to rise dramatically (ε ≈ 0.38), because the threat justifies correspondingly severe intervention. The baseline value of 0.38 reflects a moderate-threat scenario (measles-level disease: R₀ ≈ 12, CFR ≈ 0.2%). Suppression (0.48): Moderate-high. Proportionality creates genuine suppression — refusal carries material consequences (job loss, school exclusion, social stigma). However, suppression is lower than in pure public_health_primary frameworks because proportionality allows exemptions for low-threat scenarios and permits conscientious objection when threat is demonstrably low. Theater (0.35): Low-moderate. The proportionality framework itself is relatively transparent — it explicitly states both the population harm and autonomy dimensions and invokes measurable disease characteristics (R₀, CFR) to justify weight assignments. Theater increases only when proportionality language is invoked to justify suppression that the underlying disease characteristics do not support (e.g., severe vaccine mandates for endemic low-severity disease). The measurement trajectory shows theater rising (0.30 → 0.40) as the public loses confidence that threat assessments are genuine rather than pretextual.
 *
 * PERSPECTIVAL GAP:
 *   The strongest perspectival gap appears between the trapped refuser (Snare) and the institutional authority (Rope). The refuser experiences pure extraction with no coordination benefit — they must comply or face exclusion, regardless of the threat level. The authority experiences pure coordination — the proportionality framework solves their problem of legitimate intervention justification. Both perspectives read the same structural constraint, but ε appears to differ dramatically (χ high for refuser, χ low for authority) because their exit options and power positions are radically asymmetric. The generational mobile parent (Rope) perceives genuine coordination because they can choose between jurisdictions and delay across time. The biographical constrained hesitant (Tangled Rope) perceives both: the constraint coordinates safety and choice at a scale where both are salient, but enforcement is asymmetric. The analytical observer (Mountain) risks naturalizing proportionality as an immutable principle, obscuring that it is one contestable reading among three — a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to the constraint. The unvaccinated refuser occupies the victim position (high d ≈ 0.92) — the constraint's primary function is extracting compliance from them. The public health authority occupies the beneficiary position (low d ≈ 0.08) — the constraint enables their mandate and provides legitimate language for their directives. The proportionality reading creates a moderate directionality because it operationalizes genuine coordination (both autonomy and population health matter), not pure extraction. However, the weight function is asymmetric: the authority must justify why intervention intensity exceeds proportionality bounds, but the target must justify why they deserve exemption — the burden allocation itself is the extraction mechanism. The moderate power agents (hesitant parent, medical professional) occupy intermediate positions (d ≈ 0.50-0.65) because they experience both coordination and extraction depending on the specific disease scenario and their resources for exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the proportionality reading instantiates a genuine hybrid constraint that combines coordination and extraction. The coordination function is real: the framework does balance population harm and autonomy in principle. The asymmetric extraction is also real: the burden of justification falls on the target population. The constraint is not 'mislabeled extraction passing as coordination' (which would be a snare), nor is it 'pure coordination with no extraction' (which would be a rope). It is a tangled_rope because both functions are structurally present and both are necessary to the constraint's operation. The proportionality language creates a moderate amount of theater (35%) because it performs legitimacy while allowing discretionary weight assignments that can shift the balance toward suppression. If proportionality language were invoked to justify suppression that disease characteristics manifestly do not support, theater would rise and ε would rise, and the constraint would shift toward snare. The current classification reflects the proportionality reading as practiced in democratic jurisdictions with genuine constraint on authority discretion (e.g., courts reviewing proportionality claims). In jurisdictions where proportionality is invoked but not constrained by review, the constraint would reclassify as snare with high theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_severity_threshold,
    'What threshold of transmissibility + case-fatality rate triggers the shift from constrained-exit (proportionality allows exemptions) to trapped-exit (proportionality requires universal vaccination)?',
    'Comparative case studies (measles R₀≈12-18, CFR≈0.2% vs seasonal influenza R₀≈1.3, CFR≈0.1%); empirical measurement of when mandatory vaccination policies are instituted vs optional',
    'If threshold is high (only immediately lethal pathogens trigger mandatory policies): proportionality reading allows broad exemptions, victim set shrinks, ε drops toward Rope. If threshold is low (even seasonal flu can trigger mandates): victim set expands, ε rises toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disease_severity_threshold, empirical, 'Disease severity threshold determining when intervention severity justifies trapped-exit suppression').

omega_variable(
    autonomy_weighting_mechanism,
    'How does the proportionality reading operationalize the claim that ''both population harm and individual autonomy matter but are weighted by disease characteristics''? What is the actual weight function?',
    'Analysis of judicial decisions invoking proportionality (EU Charter Article 52, Canadian Constitution Section 1, state public health law); extraction of explicit or implicit weight assignments; comparison across jurisdictions',
    'If autonomy weighting is fixed (e.g., always 30% of legitimacy calculation): constraint is transparent and operationalizable. If weighting is discretionary (varies by context): constraint becomes a vessel for suppression, ε rises, classification shifts toward Snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_weighting_mechanism, conceptual, 'How autonomy is weighted relative to population harm in legitimacy assessment').

omega_variable(
    foreclosure_vs_coexistence,
    'Do the three kernel readings (bodily_autonomy_primary, proportionality_reading, public_health_primary) logically foreclose each other, or can a single legal framework legitimately hold all three as live positions?',
    'Textual analysis of constitutional provisions (e.g., EU Charter Section 52, Canadian Constitution, US state constitutions) invoking both autonomy and public health; case law testing whether courts treat these as mutually exclusive or as weighting principles within a single framework',
    'If readings foreclose each other: one must be chosen; the proportionality reading is one competing claim. If readings coexist: proportionality is the attempt to operationalize coexistence; the other readings are alternative coexistence mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence, conceptual, 'Whether kernel readings logically foreclose each other or coexist as live positions').

omega_variable(
    proportionality_as_cover_story,
    'Does the proportionality reading mask extraction that would be visible under the bodily_autonomy_primary or public_health_primary readings? Does proportionality language allow suppression that would be legally transparent if autonomy or public health were explicitly foregrounded?',
    'Comparative legal analysis: cases decided under proportionality language vs cases decided under explicit autonomy or explicit public health frameworks; measurement of suppression levels and victim narratives across frameworks',
    'If proportionality is transparent: it is a genuine coordination mechanism that limits extraction compared to pure autonomy or pure public health. If proportionality masks extraction: ε should be revised upward, and theater_ratio should increase (the proportionality language performs legitimacy while extraction proceeds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_cover_story, empirical, 'Whether proportionality language enables suppression that would be transparent under alternative frameworks').

omega_variable(
    conditional_vs_absolute_constraint,
    'Is this a single constraint with variable ε (scales with disease severity), or multiple distinct constraints (one for measles-level severity, one for flu-level severity)?',
    'Structural analysis: if disease-severity-dependent variation changes the victim set, who benefits, and exit options for the same legal framework, then ε-invariance principle requires separate constraint stories. If the same framework applies to all diseases with proportionality doing the scaling, then single constraint with measurement trajectory showing ε varying across disease scenarios.',
    'If multiple constraints: write separate stories for high-severity (measles, smallpox) and low-severity (seasonal flu, endemic colds) disease intervention constraints; link with network.affects_constraints. If single constraint: current approach is correct; measurement section should show ε trajectories across disease severity spectrum.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditional_vs_absolute_constraint, conceptual, 'Whether disease-severity dependence creates multiple distinct constraints or single variable constraint').

omega_variable(
    reading_committer_ambiguity,
    'Which reading (bodily_autonomy_primary, proportionality_reading, or public_health_primary) is instantiated by the actual legal frameworks and policy decisions observed in practice? Is the proportionality reading a genuine structural feature of policy, or a judicial narrative imposed on frameworks that actually implement either pure autonomy or pure public health?',
    'Comparative institutional analysis: examine constitutional texts, statutory language, regulatory guidance, and court decisions; classify each as implementing autonomy-primary, proportionality, or public-health-primary; measure frequency and stability of each framework across jurisdictions and time periods',
    'If proportionality is genuinely instantiated: this reading has real structural force. If proportionality is judicial narrative laid over autonomy or public health frameworks: the reading is aspirational/performative, theater_ratio should increase, and ε should rise (the proportionality language is extraction cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, empirical, 'Whether proportionality reading is genuinely instantiated or is judicial narrative covering autonomy-primary or public-health-primary implementation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_low_threat, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(theater_moderate_threat, legitimate_health_intervention__proportionality_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(theater_high_threat, legitimate_health_intervention__proportionality_reading, theater_ratio, 6, 0.4).

% Extraction over time
narrative_ontology:measurement(low_threat_baseline, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(moderate_threat_measles, legitimate_health_intervention__proportionality_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(high_threat_critical, legitimate_health_intervention__proportionality_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(suppression_low_threat, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(suppression_moderate_threat, legitimate_health_intervention__proportionality_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(suppression_high_threat, legitimate_health_intervention__proportionality_reading, suppression_requirement, 6, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).

% DUAL FORMULATION NOTE:
% The 'legitimate health intervention' kernel has three structurally distinct readings instantiated in different policies and jurisdictions. This story (proportionality_reading) should be linked to the autonomy_primary and public_health_primary readings via network.affects_constraints. The three readings have different ε values because they have different victim sets and coordination functions. Proportionality (ε ≈ 0.38) is intermediate between pure autonomy (ε ≈ 0.15, pure coordination, minimal extraction) and pure public health (ε ≈ 0.68, high extraction justified by population benefit). The readings coexist across jurisdictions — no single framework has empirically eliminated the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
